;;; Guile-Avahi --- Guile bindings for Avahi.
;;; Copyright (C) 2007  Ludovic Courtès <ludo@gnu.org>
;;;
;;; This file is part of Guile-Avahi.
;;;
;;; Guile-Avahi is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU Lesser General Public License as published by
;;; the Free Software Foundation; either version 3 of the License, or (at
;;; your option) any later version.
;;;
;;; Guile-Avahi is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser
;;; General Public License for more details.
;;;
;;; You should have received a copy of the GNU Lesser General Public License
;;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;;
;;; Excercise the guile-poll and client APIs, assuming the Avahi daemon is
;;; reachable.  This implementation uses an imperative style (full of
;;; `set!'s), which is greatly encourage by Avahi's API, unfortunately.
;;;

(use-modules (avahi)
             (avahi client)
             (avahi client lookup)
             (avahi client publish)
             (avahi test)
             (srfi srfi-1)
             (srfi srfi-11)
             (srfi srfi-19))


;;;
;;; Debugging.
;;;

(define debug? #f)

(define (debug . args)
  (if debug?
      (apply format #t args)))



;;;
;;; Timeout helper functions.
;;;

(define (insert-timeout-sorted lst timeout expiry)
  ;; Return a list containing the elements of LST plus a TIMEOUT-EXPIRY pair
  ;; in such a way that the list remains sorted.

  (define timeout-pair
    (cons timeout expiry))

  (debug "inserting timeout ~a (~a)~%"
         timeout (date->string (time-utc->date expiry)))

  (let loop ((remaining lst)
             (previous  '()))
    (if (null? remaining)
        (reverse! (cons timeout-pair previous))
        (let ((current (car remaining)))
          (if (time<? expiry (cdr current))
              (append (reverse! previous)
                      (cons timeout-pair remaining))
              (loop (cdr remaining)
                    (cons current previous)))))))

(define (time-before-next-deadline timeouts)
  ;; Return a `time-duration' object indicating the remaining time before the
  ;; next timeout listed in TIMEOUTS.  If TIMEOUTS is null, `#f' is returned.
  (if (null? timeouts)
      #f
      (let ((now (current-time time-utc))
            (next (cdr (car timeouts))))
        (debug "now: ~a~%" now)
        (if (time<? next now)
            (make-time time-duration 0 0)
            (time-difference next now)))))

(define (time-min t1 t2)
  (if (time<? t1 t2)
      t1
      t2))


;;;
;;; Event loop.
;;;

(define (make-avahi-client-event-loop client-flags client-callback)
  ;; Return two values: a client and a procedure that iterates on the
  ;; client's input file descriptors.

  (define watch-table (make-hash-table))
  (define read-fds '())
  (define write-fds '())
  (define except-fds '())
  (define timeouts '())

  (define (run-timeouts!)
    ;; Invoke the timeouts from TIMEOUTS (a list of timeout-time pairs) that
    ;; need to be invoked.
    (define now
      (current-time time-utc))

    (define (expired? timeout+time)
      (let ((deadline (cdr timeout+time)))
        (time<=? deadline now)))

    (debug "running timeouts: ~a~%" (length timeouts))

    (let-values (((to-run remaining)
                  (span expired? timeouts)))

      (debug "to-run=~a remaining=~a~%"
             to-run remaining)

      ;; First update the timeout list.
      (set! timeouts remaining)

      ;; Then run the timeouts.  They might actually add new timeouts through
      ;; `update-timeout!' or `new-timeout', hence the need to do things in this
      ;; order.
      (for-each (lambda (timeout+time)
                  (debug "invoking ~a (~a)~%"
                         (car timeout+time)
                         (date->string
                          (time-utc->date (cdr timeout+time))))
                  (invoke-timeout (car timeout+time)))
                to-run)))


  (define (run-watches fd-list event)
    (debug "running watches: ~a ~a~%"
           event fd-list)
    (for-each (lambda (fd)
                (let ((watch (hash-ref watch-table fd)))
                  (invoke-watch watch (list event))))
              fd-list))

  (define (new-watch watch fd events)
    (debug "new-watch: ~a ~a ~a~%" watch fd events)
    (hash-set! watch-table fd watch)
    (for-each (lambda (e)
                (cond ((eq? e watch-event/in)
                       (set! read-fds (cons fd read-fds)))
                      ((eq? e watch-event/out)
                       (set! write-fds (cons fd write-fds)))
                      (else
                       (set! except-fds (cons fd except-fds)))))
              events))

  (define (update-watch! watch new-events)
    (debug "update-watch! ~a ~a~%" watch new-events)
    (let ((fd (watch-fd watch))
          (old-events (watch-events watch)))
      (for-each (lambda (e)
                  (cond ((eq? e watch-event/in)
                         (set! read-fds (delete fd read-fds)))
                        ((eq? e watch-event/out)
                         (set! write-fds (delete fd write-fds)))
                        (else
                         (set! except-fds (delete fd except-fds)))))
                old-events)
      (new-watch watch fd new-events)))

  (define (free-watch watch)
    (debug "free-watch ~a~%" watch)
    (update-watch! watch '())
    (hash-remove! watch-table (watch-fd watch)))

  (define (new-timeout timeout sec nsec)
    (debug "new-timeout: ~a ~a ~a~%"
           timeout
           (cons sec nsec)
           (and sec nsec
                (date->string
                 (time-utc->date (make-time time-utc nsec sec)))))
    (if (and sec nsec)
        (set! timeouts
              (insert-timeout-sorted timeouts timeout
                                     (make-time time-utc nsec sec)))))

  (define (update-timeout! timeout sec nsec)
    (debug "update-timeout: ~a ~a ~a~%"
           timeout
           (cons sec nsec)
           (and sec nsec
                (date->string
                 (time-utc->date (make-time time-utc nsec sec)))))
    (set! timeouts
          (let ((timeouts (alist-delete! timeout timeouts)))
            (if (and sec nsec)
                (insert-timeout-sorted timeouts
                                       timeout
                                       (make-time time-utc nsec sec))
                timeouts))))

  (define (free-timeout timeout)
    (debug "free-timeout: ~a~%" timeout)
    (set! timeouts (alist-delete! timeout timeouts)))

  (let* ((poll (make-guile-poll new-watch update-watch! free-watch
                                new-timeout update-timeout!
                                free-timeout))
         (client (make-client (guile-poll poll) client-flags
                              client-callback)))

    (values

     client

     (lambda (exit-delay-sec)
       ;; Exit at most after EXIT-DELAY-SEC (if specified) or loop endlessly.

       (define exit-delay
         (and (number? exit-delay-sec)
              (make-time time-duration 0 exit-delay-sec)))
       (define end-time
         (and (number? exit-delay-sec)
              (add-duration (current-time time-utc)
                            (make-time time-duration 0 exit-delay-sec))))

       (let loop ()
         (run-timeouts!)
         (if (every null? (list read-fds write-fds except-fds))
             #f
             (let* ((next     (time-before-next-deadline timeouts))
                    (delay    (if (and next exit-delay)
                                  (time-min next exit-delay)
                                  (if exit-delay
                                      exit-delay
                                      next))))
               (debug "next-deadline: ~a ~a~%"
                      (and delay (time-second delay))
                      (and delay (time-nanosecond delay)))
               (debug "fds: ~a ~a ~a~%"
                      (length read-fds) (length write-fds)
                      (length except-fds))
               (debug "timeouts: ~a~%" (length timeouts))

               (let* ((selected (select read-fds write-fds except-fds
                                        (and delay (time-second delay))
                                        (and delay
                                             (quotient (time-nanosecond delay)
                                                       1000))))
                      (reads    (car selected))
                      (writes   (cadr selected))
                      (excepts  (caddr selected)))

                 (if (and end-time
                          (time>? (current-time time-utc) end-time))
                     #t
                     (begin
                       (run-watches reads watch-event/in)
                       (run-watches writes watch-event/out)
                       (run-watches excepts watch-event/err)
                       (loop)))))))))))



;;;
;;; Test.
;;;

(dynamic-wind
    (lambda ()
      #t)

    (lambda ()
      (define %client-flags
        (list client-flag/ignore-user-config))

      ;; Prevent them from being prematurely GC'd.
      (define %group #f)
      (define %browser #f)

      (define seen-service? #f)

      (define (make-service-browser-callback expected-service-name)
        (lambda (browser interface protocol event
                 service-name service-type domain flags)
          (debug "service-browser: ~a ~a ~a ~a ~a~%"
                 event service-name service-type domain flags)
          (set! seen-service?
                (or seen-service?
                    (and (eq? event browser-event/new)
                         (equal? expected-service-name service-name))))))

      (define (make-group-callback service-name)
        (lambda (group state)
          (debug "group-state: ~a~%" state)
          (if (eq? state entry-group-state/established)
              (let ((callback (make-service-browser-callback service-name)))
                (set! %browser
                      (make-service-browser (entry-group-client group)
                                            interface/unspecified
                                            protocol/unspecified
                                            %service-type #f '()
                                            callback))))))

      (define (client-callback client state)
        (debug "client-callback: ~a ~a~%" client state)
        (if (not (and (client? client)
                      (string? (client-state->string state))))
            (throw 'failure)
            (let* ((service-name (make-service-name))
                   (callback     (make-group-callback service-name))
                   (group        (make-entry-group client callback)))
              (set! %group group)
              (add-entry-group-service! group interface/unspecified
                                        protocol/unspecified '()
                                        service-name %service-type #f #f
                                        1234 "scheme=yes" "java=no")
              (commit-entry-group group))))

      (setvbuf (current-output-port) _IONBF)

      (exit (let-values (((client iterate)
                          (make-avahi-client-event-loop %client-flags
                                                        client-callback)))
              (let loop ((seconds 0))
                (if (> seconds %timeout-sec)
                    #f
                    (and (iterate 1)
                         (string? (client-server-version client))
                         (eq? (client-state client)
                              client-state/s-running)
                         (or seen-service? (loop (+ seconds 1)))))))))

    (lambda ()
      ;; failure.
      (exit 1)))

;;; arch-tag: b7dc1451-607d-44ac-8d95-153eaef6a6a9
