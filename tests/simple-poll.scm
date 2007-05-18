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
;;; Excercise the simple-poll and client APIs, assuming the Avahi daemon is
;;; reachable.
;;;

(use-modules (avahi)
             (avahi client)
             (srfi srfi-1))

(dynamic-wind
    (lambda ()
      #t)

    (lambda ()
      (define (client-callback client state)
        (if (not (and (client? client)
                      (string? (client-state->string state))))
            (throw 'failure)))

      (exit (and (simple-poll? (make-simple-poll))
                 (every poll?
                        (let loop ((polls '())
                                   (count 123))
                          (if (= count 0)
                              polls
                              (begin
                                (gc)
                                (loop (cons (simple-poll (make-simple-poll))
                                            polls)
                                      (- count 1))))))
                 (let* ((poll (make-simple-poll))
                        (client (make-client (simple-poll poll)
                                             (list
                                              client-flag/ignore-user-config)
                                             client-callback)))
                   (and (client? client)
                        (begin
                          (iterate-simple-poll poll 700)
                          #t)
                        (every string?
                               (map apply
                                    (list client-server-version
                                          client-host-name
                                          client-host-fqdn)
                                    (make-list 3 (list client))))
                        (eq? (client-state client)
                             client-state/s-running))))))

    (lambda ()
      ;; failure.
      (exit 1)))


;;; arch-tag: b866efb9-235f-4005-b9ca-8b246e0c8f80
