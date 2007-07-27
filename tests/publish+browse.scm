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

;;;
;;; Excercise the lookup API, using the publication API to have something to
;;; browse.
;;;

(use-modules (avahi)
             (avahi client)
             (avahi client publish)
             (avahi client lookup)
             (avahi test))

(define %service-name
  (make-service-name))



(dynamic-wind
    (lambda ()
      #t)

    (lambda ()
      (define domain-browser #f)
      (define service-type-browser #f)
      (define service-browser #f)

      (define seen-service-type? #f)
      (define seen-service? #f)


      (define (domain-browser-callback browser interface protocol event
                                       domain flags)
        ;;(format #t "domain-browser: ~a ~a ~a~%"
        ;;        event domain flags)
        #t)

      (define (service-type-browser-callback browser interface protocol event
                                             service-type domain flags)
        ;;(format #t "service-type: ~a ~a ~a ~a~%"
        ;;        event service-type domain flags)
        (set! seen-service-type?
              (or seen-service-type?
                  (and (eq? event browser-event/new)
                       (equal? %service-type service-type)))))

      (define (service-browser-callback browser interface protocol event
                                        service-name service-type
                                        domain flags)
        ;;(format #t "service: ~a ~a ~a ~a ~a~%"
        ;;        event service-name service-type domain flags)
        (set! seen-service?
              (or seen-service?
                  (and (eq? event browser-event/new)
                       (memq lookup-result-flag/our-own flags)
                       (equal? %service-name service-name)))))

      (define (make-group-callback client)
        (lambda (group state)
          ;;(format #t "group-state: ~a~%" state)
          (if (eq? state entry-group-state/established)
              (begin
                ;; Everything was published, so let's browse!
                (set! domain-browser
                      (make-domain-browser client interface/unspecified
                                           protocol/unspecified
                                           #f
                                           domain-browser-type/browse
                                           '()
                                           domain-browser-callback))
                (set! service-type-browser
                      (make-service-type-browser client
                                                 interface/unspecified
                                                 protocol/unspecified
                                                 #f '()
                                                 service-type-browser-callback))
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
                  (commit-entry-group group))))))


      (exit (let* ((poll (make-simple-poll))
                   (client (make-client (simple-poll poll)
                                        (list
                                         client-flag/ignore-user-config)
                                        client-callback)))
              (and (client? client)
                   (iterate-simple-poll-until-true
                    poll
                    (lambda ()
                      (and seen-service-type? seen-service?
                           (begin
                             (free-service-browser! service-browser)
                             (free-service-type-browser!
                              service-type-browser))
                           (freed-service-browser? service-browser)
                           (freed-service-type-browser?
                            service-type-browser))))))))

    (lambda ()
      ;; failure.
      (exit 1)))


;;; arch-tag: f173da7a-afde-4510-bf8b-f2d60148d1fd
