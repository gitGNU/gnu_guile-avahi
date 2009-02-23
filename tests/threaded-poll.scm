;;; Guile-Avahi --- Guile bindings for Avahi.
;;; Copyright (C) 2007, 2009  Ludovic Courtès <ludo@gnu.org>
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
;;; Excercise the threaded-poll and client APIs, assuming the Avahi daemon is
;;; reachable.
;;;

(use-modules (avahi)
             (avahi client)
             (avahi test)
             (srfi srfi-19))

(dynamic-wind
    (lambda ()
      #t)

    (lambda ()
      (define (client-callback client state)
        ;; In practice, this callback is first called from within
        ;; `make-client', i.e., in the main thread.  Thus, we can't really
        ;; test the threaded poll.
        (if (not (and (client? client)
                      (string? (client-state->string state))))
            (throw 'failure)))

      (exit (let* ((poll   (make-threaded-poll))
                   (client (make-client (threaded-poll poll)
                                        (list
                                         client-flag/ignore-user-config)
                                        client-callback)))
              (and (client? client)
                   (eq? (client-state client)
                        client-state/s-running)))))

    (lambda ()
      ;; failure.
      (exit 1)))


