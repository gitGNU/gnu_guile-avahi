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

(use-modules (avahi build smobs))


;;;
;;; The program.
;;;

(define (main . args)
  (define %smobs
    `((common   . ,%avahi-common-smobs)
      (client   . ,%avahi-client-smobs)
      (publish  . ,%avahi-publish-smobs)
      (lookup   . ,%avahi-lookup-smobs)))

  (define %module
    (string->symbol (car args)))

  (let ((port (current-output-port))
        (smobs (assoc-ref %smobs %module)))
    (for-each (lambda (type)
                (output-smob-type-definition type port)
                (output-smob-type-predicate type port))
              smobs)))

(apply main (cdr (command-line)))

;;; arch-tag: 364811a0-6d0a-431a-ae50-d2f7dc529903
