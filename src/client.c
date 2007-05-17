/* Guile-Avahi --- Guile bindings for Avahi.
   Copyright (C) 2007  Ludovic Courtès <ludovic.courtes@laas.fr>

   Guile-Avahi is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2.1 of the License, or (at your option) any later version.

   Guile-Avahi is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with Guile-Avahi; if not, write to the Free Software
   Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA  */

#ifdef HAVE_CONFIG_H
# include "config.h"
#endif

#include <avahi-common/watch.h>
#include <avahi-common/simple-watch.h>
#include <libguile.h>

#include "client-smobs.h"
#include "client-enums.h"



/* SMOB and enums type definitions.  */

#include "client-enums.i.c"
#include "client-smobs.i.c"


/* Initialization.  */
void
scm_avahi_client_init (void)
{
#include "client.c.x"

  scm_avahi_define_enums ();
}

/* arch-tag: bfed1cab-478e-4272-9cd3-884f47ce3506
 */
