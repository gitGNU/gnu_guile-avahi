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
;;; Test the error/exception mechanism.
;;;

(use-modules (avahi)
             (avahi client)
             (avahi client publish)
             (avahi test))

(dynamic-wind
    (lambda ()
      #t)

    (lambda ()
      (let* ((poll   (make-simple-poll))
             (ready? #f)
             (client
              (make-client (simple-poll poll) '()
                           (lambda (client state)
                             (set! ready?
                                   (or ready?
                                       (eq? state
                                            client-state/s-running)))))))
        (catch 'avahi-error
          (lambda ()
            (iterate-simple-poll-until-true poll (lambda () ready?))
            (let ((group (make-entry-group client (lambda args #f))))
              (free-entry-group! group)

              ;; this one raises an exception
              (entry-group-state group)

              ;; not reached (normally)
              (exit #f)))
          (lambda (key err function . currently-unused)
            ;;(format #t "~a ~a ~a ~a~%"
            ;;        key err function currently-unused)
            (exit (and (eq? key 'avahi-error)
                       (eq? err error/invalid-object)
                       (string? (error->string err))
                       (eq? function 'entry-group-state)))))))

    (lambda ()
      ;; failure
      (exit 1)))

;;; arch-tag: f5b90299-0cd8-4fae-8baf-27b456ceabcf
