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

(define-module (avahi client publish)
  :use-module (avahi client)
  :export (entry-group? make-entry-group
           commit-entry-group reset-entry-group! entry-group-state
           empty-group-empty? empty-group-client add-entry-group-service!
           add-entry-group-service-subtype! update-entry-group-service!
           add-entry-group-address!

           alternative-service-name alternative-host-name

           entry-group-state->string
           entry-group-state/uncommited entry-group-state/registering
           entry-group-state/established entry-group-state/collision
           entry-group-state/failure

           publish-flag->string
           publish-flag/unique publish-flag/no-probe publish-flag/no-announce
           publish-flag/allow-multiple publish-flag/no-reverse
           publish-flag/no-cookie publish-flag/update
           publish-flag/use-wide-area publish-flag/use-multicast))

(load-extension "libguile-avahi-v-0" "scm_avahi_publish_init")

;;; arch-tag: 36180c98-3262-40a6-a90c-eb8f283e628e
