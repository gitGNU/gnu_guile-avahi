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

(define-module (avahi)
  :export (watch? watch-fd watch-events watch-user-data invoke-watch
           timeout? timeout-value timeout-user-data invoke-timeout

           poll? simple-poll? guile-poll?

           make-guile-poll guile-poll
           make-simple-poll simple-poll iterate-simple-poll run-simple-poll

           watch-event->string
           watch-event/in watch-event/out watch-event/err watch-event/hup

           error->string

           error/ok
           error/failure
           error/bad-state
           error/invalid-host-name
           error/invalid-domain-name
           error/no-network
           error/invalid-ttl
           error/is-pattern
           error/collision
           error/invalid-record
           error/invalid-service-name
           error/invalid-service-type
           error/invalid-port
           error/invalid-key
           error/invalid-address
           error/timeout
           error/too-many-clients
           error/too-many-objects
           error/too-many-entries
           error/os
           error/access-denied
           error/invalid-operation
           error/dbus-error
           error/disconnected
           error/no-memory
           error/invalid-object
           error/no-daemon
           error/invalid-interface
           error/invalid-protocol
           error/invalid-flags
           error/not-found
           error/invalid-config
           error/version-mismatch
           error/invalid-service-subtype
           error/invalid-packet
           error/invalid-dns-error
           error/dns-formerr
           error/dns-servfail
           error/dns-nxdomain
           error/dns-notimp
           error/dns-refused
           error/dns-yxdomain
           error/dns-yxrrset
           error/dns-nxrrset
           error/dns-notauth
           error/dns-notzone
           error/invalid-rdata
           error/invalid-dns-class
           error/invalid-dns-type
           error/not-supported
           error/not-permitted
           error/invalid-argument
           error/is-empty
           error/no-change))

(load-extension "libguile-avahi-v-0" "scm_avahi_common_init")


;;; arch-tag: 4efd650a-4839-4972-9119-1f01c957fc0d
