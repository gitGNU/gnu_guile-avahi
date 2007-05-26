;;; Guile-Avahi --- Guile bindings for Avahi.
;;; Copyright (C) 2007  Ludovic Courtès <ludovic.courtes@laas.fr>
;;;
;;; Guile-Avahi is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU Lesser General Public
;;; License as published by the Free Software Foundation; either
;;; version 2.1 of the License, or (at your option) any later version.
;;;
;;; Guile-Avahi is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; Lesser General Public License for more details.
;;;
;;; You should have received a copy of the GNU Lesser General Public
;;; License along with Guile-Avahi; if not, write to the Free Software
;;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA

;;;
;;; Excercise the lookup API, using the publication API to have something to
;;; browse.
;;;

(use-modules (avahi)
             (avahi client)
             (avahi client publish)
             (avahi client lookup)
             (srfi srfi-1))

(define %service-type
  "_guile-avahi._tcp")

(define %service-name
  "guile-avahi-service")

(define (make-name-constructor prefix)
  (lambda ()
    (string-append "guile-avahi-" prefix "-"
                   (number->string (car (gettimeofday)) 16))))

(define make-service-name
  (make-name-constructor "service"))

(define make-host-name
  (make-name-constructor "host"))


(dynamic-wind
    (lambda ()
      #t)

    (lambda ()
      (define domain-browser #f)
      (define service-type-browser #f)
      (define service-browser #f)

      (define seen-service-type? #f)
      (define seen-service? #f)

      (define (exit-if-done)
        ;; Exit if everything we wanted to encounter while browsing has been
        ;; discovered.
        (if (and seen-service-type? seen-service?)
            (exit #t)))


      (define (domain-browser-callback browser interface protocol event
                                       domain flags)
        ;;(format #t "domain-browser: ~a ~a ~a~%"
        ;;        event domain flags)
        #t)

      (define (service-type-browser-callback browser interface protocol event
                                             service-type domain flags)
        ;;(format #t "service-type: ~a ~a ~a ~a~%"
        ;;        event service-type domain flags)
        (set! seen-service-type?
              (or seen-service-type?
                  (equal? %service-type service-type)))
        (exit-if-done))

      (define (service-browser-callback browser interface protocol event
                                        service-name service-type
                                        domain flags)
        ;;(format #t "service: ~a ~a ~a ~a ~a~%"
        ;;        event service-name service-type domain flags)
        (set! seen-service?
              (or seen-service?
                  (equal? %service-name service-name)))
        (exit-if-done))

      (define (make-group-callback client)
        (lambda (group state)
          ;;(format #t "group-state: ~a~%" state)
          (if (eq? state entry-group-state/established)
              (begin
                ;; Everything was published, so let's browse!
                (set! domain-browser
                      (make-domain-browser client interface/unspecified
                                           protocol/unspecified
                                           #f
                                           domain-browser-type/browse
                                           '()
                                           domain-browser-callback))
                (set! service-type-browser
                      (make-service-type-browser client
                                                 interface/unspecified
                                                 protocol/unspecified
                                                 #f '()
                                                 service-type-browser-callback))
                (set! service-browser
                      (make-service-browser client
                                            interface/unspecified
                                            protocol/unspecified
                                            %service-type #f '()
                                            service-browser-callback))))))

      (define client-callback
        (let ((group #f))
          (lambda (client state)
            ;;(format #t "client: ~a~%" state)
            (if (eq? state client-state/s-running)
                (begin
                  ;; The client is ready so start publishing a service.
                  (set! group (make-entry-group client
                                                (make-group-callback client)))
                  (add-entry-group-service! group interface/unspecified
                                            protocol/unspecified '()
                                            %service-name
                                            %service-type #f #f
                                            1234 "scheme=yes" "java=no")
                  (commit-entry-group group))))))


      (exit (let* ((poll (make-simple-poll))
                   (client (make-client (simple-poll poll)
                                        (list
                                         client-flag/ignore-user-config)
                                        client-callback)))
              (and (client? client)
                   (let ((start (gettimeofday)))
                     (let loop ((now (gettimeofday)))
                       (cond ((and seen-service-type? seen-service?)
                              #t)
                             ((> (- (car now) (car start)) 5)
                              (format #t "timeout~%")
                              #f)
                             (else
                              (begin
                                ;; FIXME: `iterate-simple-poll' doesn't seem
                                ;; to work here: only `client-callback' gets
                                ;; called, then nothing more.

                                ;;(iterate-simple-poll poll)
                                (run-simple-poll poll)
                                (loop (gettimeofday)))))))))))

    (lambda ()
      ;; failure.
      (exit 1)))


;;; arch-tag: f173da7a-afde-4510-bf8b-f2d60148d1fd
