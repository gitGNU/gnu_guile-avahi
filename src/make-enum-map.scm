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

(use-modules (avahi build enums))


;;;
;;; The program.
;;;

(define (main . args)
  (define %enums
    `((common   . ,%avahi-common-enums)
      (client   . ,%avahi-client-enums)
      (publish  . ,%avahi-publish-enums)
      (lookup   . ,%avahi-lookup-enums)))

  (define %module
    (string->symbol (car args)))

  (let ((port (current-output-port))
        (enums (assq-ref %enums %module)))
    (for-each (lambda (enum)
                (output-enum-smob-definitions enum port))
              enums)
    (output-enum-definition-function enums port)))

(apply main (cdr (command-line)))

;;; arch-tag: 3deb7d3a-005d-4f83-a72a-7382ef1e74a0
