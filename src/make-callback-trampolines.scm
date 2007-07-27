;;; Guile-Avahi --- Guile bindings for Avahi.
;;; Copyright (C) 2007  Ludovic Courtès <ludo@gnu.org>
;;;
;;; This program is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by the
;;; Free Software Foundation; either version 3 of the License, or (at your
;;; option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General
;;; Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License along
;;; with this program.  If not, see <http://www.gnu.org/licenses/>.

(use-modules (avahi build callbacks))


;;;
;;; The program.
;;;

(define (main . args)
  (define %callbacks
    `((common   . ,%avahi-common-callbacks)
      (client   . ,%avahi-client-callbacks)
      (publish  . ,%avahi-publish-callbacks)
      (lookup   . ,%avahi-lookup-callbacks)))

  (define %module
    (string->symbol (car args)))

  (let ((port (current-output-port))
        (callbacks (assq-ref %callbacks %module)))
    (format port "/* Automatically generated, do not edit.  */~%~%")

    (for-each (lambda (callback)
                (output-callback-trampoline callback port))
              callbacks)))

(apply main (cdr (command-line)))

;;; arch-tag: 77a172bd-448f-47d2-a59f-01f62cb05808
