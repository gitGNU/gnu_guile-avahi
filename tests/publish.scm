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
;;; Excercise the publication API.
;;;

(use-modules (avahi)
             (avahi client)
             (avahi client publish))

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
      (define (group-callback group state)
        ;;(format #t "group-state: ~a~%" state)
        (if (eq? state entry-group-state/established)
            (exit #t)))

      (define client-callback
        (let ((group #f))
          (lambda (client state)
            (if (eq? state client-state/s-running)
                (begin
                  (set! group (make-entry-group client group-callback))
                  (add-entry-group-service! group interface/unspecified
                                            protocol/unspecified '()
                                            (make-service-name)
                                            "_guile-avahi._tcp" #f #f
                                            1234 "scheme=yes" "java=no")
                  (add-entry-group-address! group interface/unspecified
                                            protocol/unspecified '()
                                            (string-append (make-host-name)
                                                           ".local")
                                            protocol/inet INADDR_LOOPBACK)
                  (commit-entry-group group))))))


      (exit (let* ((poll (make-simple-poll))
                   (client (make-client (simple-poll poll)
                                        (list
                                         client-flag/ignore-user-config)
                                        client-callback)))
              (and (client? client)
                   (begin
                     (iterate-simple-poll poll 500)
                     (eq? (client-state client)
                          client-state/s-running))
                   (begin
                     (iterate-simple-poll poll 3000)
                     (run-simple-poll poll)
                     #t)))))

    (lambda ()
      ;; failure.
      (exit 1)))


;;; arch-tag: a6b7b096-b827-4b7b-b81d-9faef52b09b5
