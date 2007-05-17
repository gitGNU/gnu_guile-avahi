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

#include "watch.h"

#include "common-enums.h"
#include "common-smobs.h"



/* SMOB and enums type definitions.  */

static inline void
scm_avahi_nop (void *something)
{
  /* Do nothing.  */
}

#include "common-enums.i.c"
#include "common-smobs.i.c"



/* Initialization.  */
void
scm_avahi_common_init (void)
{
#include "common.c.x"

  scm_avahi_define_enums ();
  scm_avahi_init_watch ();
}

/* arch-tag: d880c883-2fe1-49a0-b0c8-cd5a45880cec
 */
