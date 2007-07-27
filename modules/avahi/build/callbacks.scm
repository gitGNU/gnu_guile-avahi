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

(define-module (avahi build callbacks)
  :use-module (srfi srfi-1)
  :use-module (srfi srfi-9)
  :use-module (srfi srfi-13)

  :export (make-callback callback? callback-c-name callback-type-list
           output-callback-trampoline
           output-callback-trampoline-declaration

           %avahi-common-callbacks %avahi-client-callbacks
           %avahi-publish-callbacks %avahi-lookup-callbacks))

;;;
;;; This module facilitates the creation of Guile-mode call-backs.
;;; User-provided call-backs (Scheme procedures) may be called by Avahi I/O
;;; code (e.g., `iterate-simple-poll').  However, since Avahi I/O code runs
;;; in non-Guile mode, care must be taken to switch back to Guile mode when
;;; invoking the call-backs.  Procedure `output-callback-trampoline' does
;;; exactly this: it issues glue code that does the right thing to make sure
;;; the call-back is invoked in Guile mode, i.e., within `scm_with_guile ()'.
;;;


;;;
;;; Callbacks.
;;;

(define-record-type <callback>
  (make-callback c-name type-list)
  callback?
  (c-name     callback-c-name)
  (type-list  callback-type-list))

(define make-callback-id
  (let ((id 0))
    (lambda ()
      (set! id (+ 1 id))
      (number->string id))))

(define (callback-trampoline-name callback)
  ;; Return the name of the trampoline associated with CALLBACK.
  (string-append (callback-c-name callback)
                 "_trampoline"))


;;;
;;; C code generation.
;;;

(define (output-callback-trampoline callback port)
  ;; Output a trampoline that will invoke CALLBACK in Guile mode.
  (let* ((id              (make-callback-id))
         (type-list       (callback-type-list callback))
         (arg-count       (length type-list))
         (param-list      (unfold (lambda (count)
                                    (>= count arg-count))
                                  (lambda (num)
                                    (format #f "callback_arg_~a" num))
                                  (lambda (num)
                                    (+ 1 num))
                                  0))
         (c-name          (callback-c-name callback))
         (struct-name     (format #f "scm_avahi_callback_~a" id))
         (trampo-name     (format #f "scm_avahi_do_call_~a" id)))
    (define (output-arg-formals)
      (pair-for-each (lambda (type param-name)
                       (format port "~a ~a"
                               (car type) (car param-name))
                       (if (not (null? (cdr type)))
                           (display ", ")))
                     type-list
                     param-list))

    (define (output-struct-def)
      (format port "struct ~a~%{~%" struct-name)
      (for-each (lambda (type name)
                  (format port "  ~a ~a;~%"
                          type name))
                type-list
                param-list)
      (format port "};~%"))

    (define (output-trampoline)
      (format port "static void *~%~a (void *data)~%{~%"
              trampo-name)
      (format port "  struct ~a *~a = (struct ~a *) data;~%"
              struct-name struct-name struct-name)
      (format port "  ~a (" c-name)
      (pair-for-each (lambda (p)
                       (format port "~a->~a" struct-name (car p))
                       (if (null? (cdr p))
                           (format port ");~%")
                           (display ", ")))
                     param-list)
      (format port "  return NULL;~%")
      (format port "}~%"))

    (define (output-struct-assign)
      (format port "  struct ~a ~a;~%" struct-name struct-name)
      (for-each (lambda (type param-name)
                  (format port "  ~a.~a = ~a;~%"
                          struct-name param-name
                          param-name))
                type-list
                param-list))

    (output-struct-def)
    (output-trampoline)
    (format port "~%static void~%~a ("
            (callback-trampoline-name callback))
    (output-arg-formals)
    (format port ")~%{~%")
    (output-struct-assign)
    (format port "  (void) scm_with_guile (~a, &~a);~%"
            trampo-name struct-name)
    (format port "}~%")))

(define (output-callback-trampoline-declaration callback port)
  ;; Output the C declaration of the trampoline associated with CALLBACK.
  (format port "static void ~a ("
          (callback-trampoline-name callback))
  (pair-for-each (lambda (type)
                   (display (car type) port)
                   (if (not (null? (cdr type)))
                       (display ", ")))
                 (callback-type-list callback))
  (format port ");~%"))



;;;
;;; Actual callbacks.
;;;

(define %avahi-common-callbacks
  '())

(define %avahi-client-callbacks
  (list (make-callback "client_callback"
                       '("AvahiClient *" "AvahiClientState" "void *"))))

(define %avahi-publish-callbacks
  (list (make-callback "entry_group_callback"
                       '("AvahiEntryGroup *" "AvahiEntryGroupState"
                         "void *"))))

(define %avahi-lookup-callbacks
  (list (make-callback "domain_browser_callback"
                       '("AvahiDomainBrowser *"
                         "AvahiIfIndex" "AvahiProtocol" "AvahiBrowserEvent"
                         "const char *"
                         "AvahiLookupResultFlags" "void *"))
        (make-callback "service_type_browser_callback"
                       '("AvahiServiceTypeBrowser *"
                         "AvahiIfIndex" "AvahiProtocol" "AvahiBrowserEvent"
                         "const char *" "const char *"
                         "AvahiLookupResultFlags" "void *"))
        (make-callback "service_browser_callback"
                       '("AvahiServiceBrowser *"
                         "AvahiIfIndex" "AvahiProtocol" "AvahiBrowserEvent"
                         "const char *" "const char *" "const char *"
                         "AvahiLookupResultFlags" "void *"))

        (make-callback "service_resolver_callback"
                       '("AvahiServiceResolver *"
                         "AvahiIfIndex" "AvahiProtocol" "AvahiResolverEvent"
                         "const char *" "const char *" "const char *"
                         "const char *"
                         "const AvahiAddress *" "uint16_t"
                         "AvahiStringList *"
                         "AvahiLookupResultFlags" "void *"))
        (make-callback "host_name_resolver_callback"
                       '("AvahiHostNameResolver *"
                         "AvahiIfIndex" "AvahiProtocol" "AvahiResolverEvent"
                         "const char *"
                         "const AvahiAddress *"
                         "AvahiLookupResultFlags" "void *"))
        (make-callback "address_resolver_callback"
                       '("AvahiAddressResolver *"
                         "AvahiIfIndex" "AvahiProtocol" "AvahiResolverEvent"
                         "const AvahiAddress *"
                         "const char *"
                         "AvahiLookupResultFlags" "void *"))))


;;; arch-tag: ca2da3c1-d124-4920-a1cf-1bd0e5f0de66
