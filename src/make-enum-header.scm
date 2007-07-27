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
    (format port "/* Automatically generated, do not edit.  */~%~%")
    (format port "#ifndef GUILE_AVAHI_~a_ENUMS_H~%"
            (string-upcase (symbol->string %module)))
    (format port "#define GUILE_AVAHI_~a_ENUMS_H~%"
            (string-upcase (symbol->string %module)))

    (format port "#include \"config.h\"~%")
    (format port "#include <libguile.h>~%")
    (format port "#include <avahi-common/error.h>~%")
    (format port "#include <avahi-common/watch.h>~%")

    ;; Tweak so that `error/ok' is properly defined.
    (format port "#ifndef AVAHI_ERR_OK~%")
    (format port "# define AVAHI_ERR_OK AVAHI_OK~%")
    (format port "#endif~%")

    (for-each (lambda (enum)
                (output-enum-declarations enum port)
                (output-enum->c-converter enum port)
                (output-c->enum-converter enum port))
              enums)
    (format port "#endif~%")))

(apply main (cdr (command-line)))

;;; arch-tag: 07d834ca-e823-4663-9143-6d22704fbb5b
