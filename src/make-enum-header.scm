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

(use-modules (avahi build enums))


;;;
;;; The program.
;;;

(define (main . args)
  (define %enums
    `((common . ,%avahi-common-enums)
      (client . ,%avahi-client-enums)))

  (define %module
    (string->symbol (car args)))

  (let ((port (current-output-port))
        (enums (assq-ref %enums %module)))
    (format port "/* Automatically generated, do not edit.  */~%~%")
    (format port "#ifndef GUILE_AVAHI_~a_ENUMS_H~%"
            (string-upcase (symbol->string %module)))
    (format port "#define GUILE_AVAHI_~a_ENUMS_H~%"
            (string-upcase (symbol->string %module)))

    (format port "#include \"config.h\"~%")
    (format port "#include <avahi-common/watch.h>~%")

    (for-each (lambda (enum)
                (output-enum-declarations enum port)
                (output-enum->c-converter enum port)
                (output-c->enum-converter enum port))
              enums)
    (format port "#endif~%")))

(apply main (cdr (command-line)))

;;; arch-tag: 07d834ca-e823-4663-9143-6d22704fbb5b
