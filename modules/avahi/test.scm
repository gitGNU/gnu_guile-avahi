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

(define-module (avahi test)
  :use-module (avahi)
  :export (%service-type make-service-name make-host-name
           %timeout-sec iterate-simple-poll-until-true))

;;;
;;; Facilities for the test suite.
;;;

(define %service-type
  ;; The default service type used for testing.
  "_guile-avahi._tcp")

(define (make-name-constructor prefix)
  (lambda ()
    (string-append "guile-avahi-" prefix "-"
                   (number->string (car (gettimeofday)) 16))))

(define make-service-name
  ;; Return a new, hopefully unique service name.
  (make-name-constructor "service"))

(define make-host-name
  ;; Return a new, hopefully unique host name.
  (make-name-constructor "host"))


(define %timeout-sec
  ;; Number of seconds after which the test is considered failed.
  5)

(define (iterate-simple-poll-until-true poll pred)
  "Iterate @var{poll} until @var{pred} is true or a reasonable deadline has
been reached.  Returns @var{#t} on success (i.e., if @var{pred} returned
true) and @var{#f} otherwise."

  (let ((start (gettimeofday)))
    (let loop ((now (gettimeofday)))
      (cond ((pred)
             #t)
            ((> (- (car now) (car start)) %timeout-sec)
             (format (current-error-port) "test timeout~%")
             #f)
            (else
             (begin
               (iterate-simple-poll poll 800)
               (loop (gettimeofday))))))))

;;; arch-tag: 2b0326ee-284c-430b-81cf-7e792671b838
