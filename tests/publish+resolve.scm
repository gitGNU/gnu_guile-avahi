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

;;;
;;; Excercise the resolution API, using the browsing and publication APIs to
;;; have something to resolve.
;;;

(use-modules (avahi)
             (avahi client)
             (avahi client publish)
             (avahi client lookup)
             (srfi srfi-1))

(define %service-type
  "_guile-avahi._tcp")

(define %service-name
  "guile-avahi-service")

(define %host-name
  "guile-avahi-host.local")

(define debug? #f)

(define (debug fmt . args)
  (if debug?
      (apply format #t fmt args)))


(dynamic-wind
    (lambda ()
      #t)

    (lambda ()
      (define service-browser #f)

      (define resolved-host-name? #f)
      (define resolved-service?   #f)
      (define resolved-address?   #f)

      (define (exit-if-done)
        ;; Exit if everything we wanted to encounter while browsing has been
        ;; discovered.
        (if (and resolved-host-name? resolved-address? resolved-service?)
            (exit #t)))


      (define (make-address-resolver-callback expected-host-name
                                              expected-address-type
                                              expected-address)
        (lambda (resolver interface protocol event
                 address-type address host-name
                 flags)
          (debug "address-resolver event: ~a~%" event)
          (debug "address-resolver: ~a ~a ~a ~a~%"
                 address-type address host-name flags)
          (debug "address-resolver: address: ~a~%"
                 (inet-ntop (if (eq? address-type protocol/inet)
                                AF_INET
                                AF_INET6)
                            address))
          (set! resolved-address?
                (or resolved-address?
                    (eq? event resolver-event/found)
                    (equal? %host-name expected-host-name)
                    (eq? expected-address-type address-type)
                    (equal? expected-address address)))))

      (define (host-name-resolver-callback resolver interface protocol event
                                           host-name address-type address
                                           flags)
        (debug "host-name-resolver event: ~a~%" event)
        (debug "host-name-resolver: ~a ~a ~a ~a~%"
               host-name address-type address flags)
        (debug "host-name-resolver: address: ~a~%"
               (inet-ntop (if (eq? address-type protocol/inet)
                              AF_INET
                              AF_INET6)
                          address))
        (set! resolved-host-name?
              (or resolved-host-name?
                  (eq? event resolver-event/found)
                  (equal? %host-name host-name)))
        (let* ((resolver-callback
                (make-address-resolver-callback host-name
                                                address-type address))
               (resolver
                (make-address-resolver (host-name-resolver-client resolver)
                                       interface/unspecified
                                       protocol/unspecified
                                       address-type address '()
                                       resolver-callback)))
          #t)
        (exit-if-done))

      (define (service-resolver-callback resolver interface protocol event
                                         service-name service-type domain
                                         host-name address-type address port
                                         txt flags)
        (debug "resolver event: ~a~%" event)
        (debug "resolver: ~a ~a ~a ~a ~a ~a ~a ~a ~a~%"
               service-name service-type domain host-name
               address-type address port txt flags)
        (set! resolved-service?
              (or resolved-service?
                  (eq? event resolver-event/found)
                  (equal? %service-name service-name)))
        (exit-if-done))

      (define (service-browser-callback browser interface protocol event
                                        service-name service-type domain
                                        flags)
        (debug "service: ~a ~a ~a ~a ~a~%"
               event service-name service-type domain flags)
        (if (equal? %service-name service-name)
            (let ((service
                   (make-service-resolver (service-browser-client browser)
                                          interface protocol
                                          service-name service-type domain
                                          protocol/unspecified '()
                                          service-resolver-callback))
                  (host-name
                   (make-host-name-resolver (service-browser-client browser)
                                            interface protocol %host-name
                                            protocol/unspecified '()
                                            host-name-resolver-callback)))
              #t))
        (exit-if-done))

      (define (make-group-callback client)
        (lambda (group state)
          ;;(format #t "group-state: ~a~%" state)
          (if (eq? state entry-group-state/established)
              (begin
                ;; Everything was published, so let's browse!
                (set! service-browser
                      (make-service-browser client
                                            interface/unspecified
                                            protocol/unspecified
                                            %service-type #f '()
                                            service-browser-callback))))))

      (define client-callback
        (let ((group #f))
          (lambda (client state)
            ;;(format #t "client: ~a~%" state)
            (if (eq? state client-state/s-running)
                (begin
                  ;; The client is ready so start publishing a service.
                  (set! group (make-entry-group client
                                                (make-group-callback client)))
                  (add-entry-group-service! group interface/unspecified
                                            protocol/unspecified '()
                                            %service-name
                                            %service-type #f #f
                                            1234 "scheme=yes" "java=no")
                  (add-entry-group-address! group interface/unspecified
                                            protocol/unspecified '()
                                            %host-name
                                            protocol/inet INADDR_LOOPBACK)
                  (commit-entry-group group))))))


      (exit (let* ((poll (make-simple-poll))
                   (client (make-client (simple-poll poll)
                                        (list
                                         client-flag/ignore-user-config)
                                        client-callback)))
              (and (client? client)
                   (run-simple-poll poll)))))

    (lambda ()
      ;; failure.
      (exit 1)))


;;; arch-tag: 640d4520-603b-45a0-9859-0d6e8fee2cfe
