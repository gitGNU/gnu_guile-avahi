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

(define-module (avahi build utils)
  :use-module (srfi srfi-13)
  :export (scheme-symbol->c-name))

;;;
;;; Common utilities for the binding generation code.
;;;


;;;
;;; Utilities.
;;;

(define (scheme-symbol->c-name sym)
  ;; Turn SYM, a symbol denoting a Scheme name, into a string denoting a C
  ;; name.
  (string-map (lambda (chr)
                (if (eq? chr #\-) #\_ chr))
              (symbol->string sym)))


;;; arch-tag: 56919ee1-7cce-46b9-b90f-ae6fbcfe4159
