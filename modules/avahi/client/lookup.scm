;;; Guile-Avahi --- Guile bindings for Avahi.
;;; Copyright (C) 2007  Ludovic Court�s <ludovic.courtes@laas.fr>
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

(define-module (avahi client lookup)
  :use-module (avahi client)
  :export (domain-browser? make-domain-browser domain-browser-client
           service-browser? make-service-browser service-browser-client
           service-type-browser? make-service-type-browser
           service-type-browser-client

           lookup-flag->string
           lookup-flag/use-wide-area lookup-flag/use-multicast
           lookup-flag/no-txt lookup-flag/no-address

           domain-browser-type->string
           domain-browser-type/browse domain-browser-type/browse-default
           domain-browser-type/register domain-browser-type/register-default
           domain-browser-type/browse-legacy

           browser-event->string
           browser-event/new browser-event/remove
           browser-event/cache-exhausted browser-event/all-for-now
           browser-event/failure))

(load-extension "libguile-avahi-v-0" "scm_avahi_lookup_init")

;;; arch-tag: 9ab68bd4-4705-42e5-89c3-e02551be4d09