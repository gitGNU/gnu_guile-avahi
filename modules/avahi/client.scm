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

(define-module (avahi client)
  :use-module (avahi)
  :export (client? make-client

           client-state->string
           client-state/s-registering client-state/s-running
           client-state/s-collision client-state/failure
           client-state/connecting

           client-flag->string
           client-flag/ignore-user-config client-flag/no-fail))

(load-extension "libguile-avahi-v-0" "scm_avahi_client_init")

;;; arch-tag: 9dc3916b-12a3-4fde-aa91-95ee0969d8bf
