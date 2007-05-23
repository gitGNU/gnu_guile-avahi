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

(define-module (avahi build smobs)
  :use-module (srfi srfi-9)
  :use-module (srfi srfi-13)
  :use-module (avahi build utils)
  :export (make-smob-type smob-type-tag smob-free-function
           smob-type-predicate-scheme-name
           smob-type-from-c-function smob-type-to-c-function

           output-smob-type-definition output-smob-type-declaration
           output-smob-type-predicate
           output-c->smob-converter output-smob->c-converter

           %avahi-common-smobs %avahi-client-smobs
           %avahi-publish-smobs %avahi-lookup-smobs))


;;;
;;; SMOB types.
;;;

(define-record-type <smob-type>
  (%make-smob-type c-name scm-name free-function)
  smob-type?
  (c-name         smob-type-c-name)
  (scm-name       smob-type-scheme-name)
  (free-function  smob-type-free-function))

(define (make-smob-type c-name scm-name . free-function)
  (%make-smob-type c-name scm-name
                   (if (null? free-function)
                       (string-append "avahi_"
                                      (scheme-symbol->c-name scm-name)
                                      "_deinit")
                       (car free-function))))

(define (smob-type-tag type)
  ;; Return the name of the C variable holding the type tag for TYPE.
  (string-append "scm_tc16_avahi_"
                 (scheme-symbol->c-name (smob-type-scheme-name type))))

(define (smob-type-predicate-scheme-name type)
  ;; Return a string denoting the Scheme name of TYPE's type predicate.
  (string-append (symbol->string (smob-type-scheme-name type)) "?"))

(define (smob-type-to-c-function type)
  ;; Return the name of the C `scm_to_' function for SMOB.
  (string-append "scm_to_avahi_"
                 (scheme-symbol->c-name (smob-type-scheme-name type))))

(define (smob-type-from-c-function type)
  ;; Return the name of the C `scm_from_' function for SMOB.
  (string-append "scm_from_avahi_"
                 (scheme-symbol->c-name (smob-type-scheme-name type))))


;;;
;;; C code generation.
;;;

(define (output-smob-type-definition type port)
  (format port "SCM_GLOBAL_SMOB (~a, \"~a\", 0);~%"
          (smob-type-tag type)
          (smob-type-scheme-name type))

  (format port "SCM_SMOB_FREE (~a, ~a_free, obj)~%{~%"
          (smob-type-tag type)
          (scheme-symbol->c-name (smob-type-scheme-name type)))
  (format port "  ~a c_obj;~%"
          (smob-type-c-name type))
  (format port "  c_obj = (~a) SCM_SMOB_DATA (obj);~%"
          (smob-type-c-name type))
  (format port "  ~a (c_obj);~%"
          (smob-type-free-function type))
  (format port "  return 0;~%")
  (format port "}~%"))

(define (output-smob-type-declaration type port)
  ;; Issue a header file declaration for the SMOB type tag of TYPE.
  (format port "SCM_API scm_t_bits ~a;~%"
          (smob-type-tag type)))

(define (output-smob-type-predicate type port)
  (define (texi-doc-string)
    (string-append "Return true if @var{obj} is of type @code{"
                   (symbol->string (smob-type-scheme-name type))
                   "}."))

  (let ((c-name (string-append "scm_avahi_"
                               (string-map (lambda (chr)
                                             (if (char=? chr #\-)
                                                 #\_
                                                 chr))
                                           (symbol->string
                                            (smob-type-scheme-name type)))
                               "_p")))
    (format port "SCM_DEFINE (~a, \"~a\", 1, 0, 0,~%"
            c-name (smob-type-predicate-scheme-name type))
    (format port "            (SCM obj),~%")
    (format port "            \"~a\")~%"
            (texi-doc-string))
    (format port "#define FUNC_NAME s_~a~%"
            c-name)
    (format port "{~%")
    (format port "  return (scm_from_bool (SCM_SMOB_PREDICATE (~a, obj)));~%"
            (smob-type-tag type))
    (format port "}~%#undef FUNC_NAME~%")))

(define (output-c->smob-converter type port)
  (format port "static inline SCM~%~a (~a c_obj)~%{~%"
          (smob-type-from-c-function type)
          (smob-type-c-name type))
  (format port "  SCM_RETURN_NEWSMOB3 (~a, (scm_t_bits) c_obj, "
          (smob-type-tag type))
  (format port "SCM_UNPACK (SCM_BOOL_F), SCM_UNPACK (SCM_BOOL_F));~%")
  (format port "}~%"))

(define (output-smob->c-converter type port)
  (format port "static inline ~a~%~a (SCM obj, "
          (smob-type-c-name type)
          (smob-type-to-c-function type))
  (format port "unsigned pos, const char *func)~%")
  (format port "#define FUNC_NAME func~%")
  (format port "{~%")
  (format port "  SCM_VALIDATE_SMOB (pos, obj, ~a);~%"
          (string-append "avahi_"
                         (scheme-symbol->c-name (smob-type-scheme-name type))))
  (format port "  return ((~a) SCM_SMOB_DATA (obj));~%"
          (smob-type-c-name type))
  (format port "}~%")
  (format port "#undef FUNC_NAME~%"))


;;;
;;; Actual SMOB types.
;;;

(define %poll-smob
  (make-smob-type "AvahiPoll *" 'poll
                  "scm_avahi_nop"))

(define %simple-poll-smob
  (make-smob-type "AvahiSimplePoll *" 'simple-poll
                  "avahi_simple_poll_free"))

(define %guile-poll-smob
  (make-smob-type "AvahiGuilePoll *" 'guile-poll
                  "avahi_guile_poll_free"))

(define %watch-smob
  (make-smob-type "AvahiWatch *" 'watch
                  "free"))

(define %timeout-smob
  (make-smob-type "AvahiTimeout *" 'timeout
                  "free"))

(define %avahi-common-smobs
  (list %poll-smob %simple-poll-smob %guile-poll-smob
        %watch-smob %timeout-smob))


(define %client-smob
  (make-smob-type "AvahiClient *" 'client
                  "scm_avahi_client_free"))

(define %avahi-client-smobs
  (list %client-smob))



(define %entry-group-smob
  (make-smob-type "AvahiEntryGroup *" 'entry-group
                  "scm_avahi_entry_group_free"))

(define %avahi-publish-smobs
  (list %entry-group-smob))


(define %domain-browser-smob
  (make-smob-type "AvahiDomainBrowser *" 'domain-browser
                  "avahi_domain_browser_free"))

(define %service-browser-smob
  (make-smob-type "AvahiServiceBrowser *" 'service-browser
                  "avahi_service_browser_free"))

(define %service-type-browser-smob
  (make-smob-type "AvahiServiceTypeBrowser *" 'service-type-browser
                  "avahi_service_type_browser_free"))

(define %service-resolver-smob
  (make-smob-type "AvahiServiceResolver *" 'service-resolver
                  "avahi_service_resolver_free"))

(define %host-name-resolver-smob
  (make-smob-type "AvahiHostNameResolver *" 'host-name-resolver
                  "avahi_host_name_resolver_free"))

(define %address-resolver-smob
  (make-smob-type "AvahiAddressResolver *" 'address-resolver
                  "avahi_address_resolver_free"))


(define %avahi-lookup-smobs
  (list %domain-browser-smob %service-browser-smob
        %service-type-browser-smob %service-resolver-smob
        %host-name-resolver-smob %address-resolver-smob))


;;; arch-tag: 26bf79ef-6dee-45f2-9e9d-2d209c518278
