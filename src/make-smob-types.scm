;;; Help produce Guile wrappers for Avahi types.
;;;
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

(use-modules (avahi build smobs))


;;;
;;; The program.
;;;

(define (main . args)
  (define %smobs
    `((common   . ,%avahi-common-smobs)
      (client   . ,%avahi-client-smobs)
      (publish  . ,%avahi-publish-smobs)))

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
