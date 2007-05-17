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

(define-module (avahi build enums)
  :use-module (srfi srfi-1)
  :use-module (srfi srfi-9)
  :use-module (avahi build utils)

  :export (make-enum-type enum-type-subsystem enum-type-value-alist
           enum-type-c-type enum-type-get-name-function
           enum-type-automatic-get-name-function
           enum-type-smob-name
           enum-type-to-c-function enum-type-from-c-function

           output-enum-smob-definitions output-enum-definitions
           output-enum-declarations
           output-enum-definition-function output-c->enum-converter
           output-enum->c-converter

           %cipher-enum %mac-enum %compression-method-enum %kx-enum
           %protocol-enum %certificate-type-enum

           %avahi-common-enums %avahi-client-enums))

;;;
;;; This module helps with the creation of bindings for the C enumerate
;;; types.  It aims at providing strong typing (i.e., one cannot use an
;;; enumerate value of the wrong type) along with authenticity checks (i.e.,
;;; values of a given enumerate type cannot be forged---for instance, one
;;; cannot use some random integer as an enumerate value).  Additionally,
;;; Scheme enums representing the same C enum value should be `eq?'.
;;;
;;; To that end, Scheme->C conversions are optimized (a simple
;;; `SCM_SMOB_DATA'), since that is the most common usage pattern.
;;; Conversely, C->Scheme conversions take time proportional to the number of
;;; value in the enum type.
;;;


;;;
;;; Enumeration tools.
;;;

(define-record-type <enum-type>
  (%make-enum-type subsystem c-type enum-map get-name value-prefix)
  enum-type?
  (subsystem    enum-type-subsystem)
  (enum-map     enum-type-value-alist)
  (c-type       enum-type-c-type)
  (get-name     enum-type-get-name-function)
  (value-prefix enum-type-value-prefix))


(define (make-enum-type subsystem c-type values get-name . value-prefix)
  ;; Return a new enumeration type.
  (let ((value-prefix (if (null? value-prefix)
                          #f
                          (car value-prefix))))
    (%make-enum-type subsystem c-type
                     (make-enum-map subsystem values value-prefix)
                     get-name value-prefix)))


(define (make-enum-map subsystem values value-prefix)
  ;; Return an alist mapping C enum values (strings) to Scheme symbols.
  (define (value-symbol->string value)
    (string-upcase (scheme-symbol->c-name value)))

  (define (make-c-name value)
    (case value-prefix
      ((#f)
       ;; automatically derive the C value name.
       (string-append "AVAHI_" (string-upcase (symbol->string subsystem))
                      "_" (value-symbol->string value)))
      (else
       (string-append value-prefix (value-symbol->string value)))))

  (map (lambda (value)
         (cons (make-c-name value) value))
       values))

(define (enum-type-smob-name enum)
  ;; Return the C name of the smob type for ENUM.
  (string-append "scm_tc16_avahi_"
                 (scheme-symbol->c-name (enum-type-subsystem enum))
                 "_enum"))

(define (enum-type-smob-list enum)
  ;; Return the name of the C variable holding a list of value (SMOBs) for
  ;; ENUM.  This list is used when converting from C to Scheme.
  (string-append "scm_avahi_"
                 (scheme-symbol->c-name (enum-type-subsystem enum))
                 "_enum_values"))

(define (enum-type-to-c-function enum)
  ;; Return the name of the C `scm_to_' function for ENUM.
  (string-append "scm_to_avahi_"
                 (scheme-symbol->c-name (enum-type-subsystem enum))))

(define (enum-type-from-c-function enum)
  ;; Return the name of the C `scm_from_' function for ENUM.
  (string-append "scm_from_avahi_"
                 (scheme-symbol->c-name (enum-type-subsystem enum))))

(define (enum-type-automatic-get-name-function enum)
  ;; Return the name of an automatically-generated C function that returns a
  ;; string describing the given enum value of type ENUM.
  (string-append "scm_avahi_"
                 (scheme-symbol->c-name (enum-type-subsystem enum))
                 "_to_c_string"))


;;;
;;; C code generation.
;;;

(define (output-enum-smob-definitions enum port)
  (let ((smob     (enum-type-smob-name enum))
        (get-name (enum-type-get-name-function enum)))
    (format port "SCM_GLOBAL_SMOB (~a, \"~a\", 0);~%"
            smob (enum-type-subsystem enum))
    (format port "SCM ~a = SCM_EOL;~%"
            (enum-type-smob-list enum))

    (if (not (string? get-name))
        ;; Generate a "get name" function.
        (output-enum-get-name-function enum port))

    ;; Generate the printer and `->string' function.
    (let ((get-name (or get-name
                        (enum-type-automatic-get-name-function enum))))
      (let ((subsystem (scheme-symbol->c-name (enum-type-subsystem enum))))
        ;; SMOB printer.
        (format port "SCM_SMOB_PRINT (~a, ~a_print, obj, port, pstate)~%{~%"
                smob subsystem)
        (format port "  scm_puts (\"#<avahi-~a-enum \", port);~%"
                (enum-type-subsystem enum))
        (format port "  scm_puts (~a (~a (obj, 1, \"~a_print\")), port);~%"
                get-name (enum-type-to-c-function enum) subsystem)
        (format port "  scm_puts (\">\", port);~%")
        (format port "  return 1;~%")
        (format port "}~%")

        ;; Enum-to-string.
        (format port "SCM_DEFINE (scm_avahi_~a_to_string, \"~a->string\", "
                subsystem (enum-type-subsystem enum))
        (format port "1, 0, 0,~%")
        (format port "            (SCM enumval),~%")
        (format port "            \"Return a string describing ")
        (format port "@var{enumval}, a @code{~a} value.\")~%"
                (enum-type-subsystem enum))
        (format port "#define FUNC_NAME s_scm_avahi_~a_to_string~%"
                subsystem)
        (format port "{~%")
        (format port "  ~a c_enum;~%"
                (enum-type-c-type enum))
        (format port "  const char *c_string;~%")
        (format port "  c_enum = ~a (enumval, 1, FUNC_NAME);~%"
                (enum-type-to-c-function enum))
        (format port "  c_string = ~a (c_enum);~%"
                get-name)
        (format port "  return (scm_from_locale_string (c_string));~%")
        (format port "}~%")
        (format port "#undef FUNC_NAME~%")))))

(define (output-enum-definitions enum port)
  ;; Output to PORT the Guile C code that defines the values of ENUM-ALIST.
  (let ((subsystem (scheme-symbol->c-name (enum-type-subsystem enum))))
    (format port "  enum_values = SCM_EOL;~%")
    (for-each (lambda (c+scheme)
                (format port "  SCM_NEWSMOB (enum_smob, ~a, "
                        (enum-type-smob-name enum))
                (format port "(scm_t_bits) ~a);~%"
                        (car c+scheme))
                (format port "  enum_values = scm_cons (enum_smob, ")
                (format port "enum_values);~%")
                (format port "  scm_c_define (\"~a\", enum_smob);~%"
                        (symbol-append (enum-type-subsystem enum) '/
                                       (cdr c+scheme))))
              (enum-type-value-alist enum))
    (format port "  ~a = scm_permanent_object (enum_values);~%"
            (enum-type-smob-list enum))))

(define (output-enum-declarations enum port)
  ;; Issue header file declarations needed for the inline functions that
  ;; handle ENUM values.
  (format port "SCM_API scm_t_bits ~a;~%"
          (enum-type-smob-name enum))
  (format port "SCM_API SCM ~a;~%"
          (enum-type-smob-list enum)))

(define (output-enum-definition-function enums port)
  ;; Output a C function that does all the `scm_c_define ()' for the enums
  ;; listed in ENUMS.
  (format port "static inline void~%scm_avahi_define_enums (void)~%{~%")
  (format port "  SCM enum_values, enum_smob;~%")
  (for-each (lambda (enum)
              (output-enum-definitions enum port))
            enums)
  (format port "}~%"))

(define (output-c->enum-converter enum port)
  ;; Output a C->Scheme converted for ENUM.  This works by walking the list
  ;; of available enum values (SMOBs) for ENUM and then returning the
  ;; matching SMOB, so that users can then compare enums using `eq?'.  While
  ;; this may look inefficient, this shouldn't be a problem since (i)
  ;; conversion in that direction is rarely needed and (ii) the number of
  ;; values per enum is expected to be small.
  (format port "static inline SCM~%~a (~a c_obj)~%{~%"
          (enum-type-from-c-function enum)
          (enum-type-c-type enum))
  (format port "  SCM pair, result = SCM_BOOL_F;~%")
  (format port "  for (pair = ~a; scm_is_pair (pair); "
          (enum-type-smob-list enum))
  (format port "pair = SCM_CDR (pair))~%")
  (format port "    {~%")
  (format port "      SCM enum_smob;~%")
  (format port "      enum_smob = SCM_CAR (pair);~%")
  (format port "      if ((~a) SCM_SMOB_DATA (enum_smob) == c_obj)~%"
          (enum-type-c-type enum))
  (format port "        {~%")
  (format port "          result = enum_smob;~%")
  (format port "          break;~%")
  (format port "        }~%")
  (format port "    }~%")
  (format port "  return result;~%")
  (format port "}~%"))

(define (output-enum->c-converter enum port)
  (let* ((c-type-name (enum-type-c-type enum))
         (subsystem   (scheme-symbol->c-name (enum-type-subsystem enum))))

    (format port
            "static inline ~a~%~a (SCM obj, unsigned pos, const char *func)~%"
            c-type-name (enum-type-to-c-function enum))
    (format port "#define FUNC_NAME func~%")
    (format port "{~%")
    (format port "  SCM_VALIDATE_SMOB (pos, obj, ~a);~%"
            (string-append "avahi_" subsystem "_enum"))
    (format port "  return ((~a) SCM_SMOB_DATA (obj));~%"
            c-type-name)
    (format port "}~%")
    (format port "#undef FUNC_NAME~%")))

(define (output-enum-get-name-function enum port)
  ;; Output a C function that, when passed a C ENUM value, returns a C string
  ;; representing that value.
  (let ((function (enum-type-automatic-get-name-function enum)))
    (format port
            "static const char *~%~a (~a c_obj)~%"
            function (enum-type-c-type enum))
    (format port "{~%")
    (format port "  static const struct ")
    (format port "{ ~a value; const char *name; } "
            (enum-type-c-type enum))
    (format port "table[] =~%")
    (format port "    {~%")
    (for-each (lambda (c+scheme)
                (format port "       { ~a, \"~a\" },~%"
                        (car c+scheme) (cdr c+scheme)))
              (enum-type-value-alist enum))
    (format port "    };~%")
    (format port "  unsigned i;~%")
    (format port "  const char *name = NULL;~%")
    (format port "  for (i = 0; i < ~a; i++)~%"
            (length (enum-type-value-alist enum)))
    (format port "    {~%")
    (format port "      if (table[i].value == c_obj)~%")
    (format port "        {~%")
    (format port "          name = table[i].name;~%")
    (format port "          break;~%")
    (format port "        }~%")
    (format port "    }~%")
    (format port "  return (name);~%")
    (format port "}~%")))


;;;
;;; Actual enumerations.
;;;

(define %watch-event-enum
  (make-enum-type 'watch-event "AvahiWatchEvent"
                  '(in out err hup)
                  #f
                  "AVAHI_WATCH_"))


(define %avahi-common-enums
  ;; All enums.
  (list %watch-event-enum))


(define %client-state-enum
  (make-enum-type 'client-state "AvahiClientState"
                  '(s-registering s-running s-collision failure connecting)
                  #f
                  "AVAHI_CLIENT_"))

(define %client-flags-enum
  (make-enum-type 'client-flags "AvahiClientFlags"
                  '(ignore-user-config no-fail)
                  #f
                  "AVAHI_CLIENT_"))

(define %avahi-client-enums
  ;; Nothing so far.
  (list %client-state-enum %client-flags-enum))

;;; arch-tag: 9e3eb6bb-61a5-4e85-861f-1914ab9677b0
