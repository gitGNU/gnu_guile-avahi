;;; Guile-Avahi --- Guile bindings for Avahi.
;;; Copyright (C) 2007  Ludovic Court�s <ludo@gnu.org>
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
        (smobs (assq-ref %smobs %module)))
    (format port "/* Automatically generated, do not edit.  */~%~%")
    (format port "#ifndef GUILE_AVAHI_~a_SMOBS_H~%"
            (string-upcase (symbol->string %module)))
    (format port "#define GUILE_AVAHI_~a_SMOBS_H~%"
            (string-upcase (symbol->string %module)))

    (format port "#include <avahi-common/watch.h>~%")
    (format port "#include <avahi-common/simple-watch.h>~%")
    (format port "#include <avahi-client/client.h>~%")
    (format port "#include <libguile.h>~%")
    (format port "#include \"watch.h\"~%")

    (for-each (lambda (type)
                (output-smob-type-declaration type port)
                (output-c->smob-converter type port)
                (output-smob->c-converter type port))
              smobs)
    (format port "#endif~%")))

(apply main (cdr (command-line)))

;;; arch-tag: 7ae9c82f-a423-4251-9a58-6e2581267567
