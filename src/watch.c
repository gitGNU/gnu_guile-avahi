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

#include <libguile.h>

#include "watch.h"
#include "common-smobs.h"
#include "common-enums.h"
#include "utils.h"


struct AvahiWatch
{
  int                 fd;
  int                 dead;
  AvahiWatchEvent     events;
  AvahiWatchCallback  callback;
  void               *userdata;
  SCM                 watch_smob;
  SCM                 stuff;

  AvahiGuilePoll     *guile_poll;
};

struct AvahiTimeout
{
  int                   dead;
  struct timeval        expiry;
  AvahiTimeoutCallback  callback;
  void                 *userdata;
  SCM                   timeout_smob;
  SCM                   stuff;

  AvahiGuilePoll       *guile_poll;
};



/* Watches.  */

static AvahiWatch *
watch_new (const AvahiPoll *api, int fd, AvahiWatchEvent events,
	   AvahiWatchCallback callback, void *userdata)
{
  AvahiWatch *watch;
  AvahiGuilePoll *guile_poll;

  guile_poll = (AvahiGuilePoll *) api->userdata;

  watch = scm_malloc (sizeof (*watch));
  watch->fd = fd;
  watch->dead = 0;
  watch->events = events;
  watch->callback = callback;
  watch->userdata = userdata;
  watch->guile_poll = guile_poll;

  watch->watch_smob = scm_gc_protect_object (scm_from_avahi_watch (watch));

  watch->stuff = scm_call_2 (guile_poll->new_watch, watch->watch_smob,
			     scm_from_int (fd));
  watch->stuff = scm_gc_protect_object (watch->stuff);

  return (watch);
}

static void
watch_update (AvahiWatch *watch, AvahiWatchEvent events)
{
  scm_call_4 (watch->guile_poll->update_watch_x,
	      watch->watch_smob, scm_from_int (watch->fd),
	      scm_from_avahi_watch_events (events),
	      watch->stuff);

  watch->events = events;
}

static AvahiWatchEvent
watch_get_events (AvahiWatch *watch)
{
  return (watch->events);
}

static void
watch_free (AvahiWatch *watch)
{
  scm_call_3 (watch->guile_poll->free_watch,
	      watch->watch_smob, scm_from_int (watch->fd),
	      watch->stuff);

  /* We don't free the C object here.  Instead, it will get freed eventually,
     when its Scheme representative gets GC'd.  */
  watch->fd = -1;
  watch->dead = 1;

  scm_gc_unprotect_object (watch->watch_smob);
  scm_gc_unprotect_object (watch->stuff);
}


/* Timeouts.  */

static AvahiTimeout *
timeout_new (const AvahiPoll *api, const struct timeval *tv,
	     AvahiTimeoutCallback callback, void *userdata)
{
  AvahiTimeout *timeout;
  AvahiGuilePoll *guile_poll;
  unsigned long usec;

  guile_poll = (AvahiGuilePoll *) api->userdata;

  timeout = scm_malloc (sizeof (*timeout));
  timeout->dead = 0;
  timeout->callback = callback;
  timeout->userdata = userdata;
  timeout->guile_poll = guile_poll;

  timeout->timeout_smob = scm_from_avahi_timeout (timeout);
  timeout->timeout_smob = scm_gc_protect_object (timeout->timeout_smob);

  usec = tv->tv_usec + (tv->tv_sec * 1000000UL);
  timeout->stuff = scm_call_2 (guile_poll->new_timeout, timeout->timeout_smob,
			       scm_from_ulong (usec));
  timeout->stuff = scm_gc_protect_object (timeout->stuff);

  return (timeout);
}

static void
timeout_update (AvahiTimeout *timeout, const struct timeval *tv)
{
  unsigned long usec;

  usec = tv->tv_usec + (tv->tv_sec * 1000000UL);
  scm_call_3 (timeout->guile_poll->update_timeout_x,
	      timeout->timeout_smob, scm_from_ulong (usec),
	      timeout->stuff);
  timeout->expiry = *tv;
}

static void
timeout_free (AvahiTimeout *timeout)
{
  scm_call_2 (timeout->guile_poll->free_timeout,
	      timeout->timeout_smob, timeout->stuff);

  /* We don't free the C object here.  Instead, it will get freed eventually,
     when its Scheme representative gets GC'd.  */
  timeout->dead = 1;

  scm_gc_unprotect_object (timeout->timeout_smob);
  scm_gc_unprotect_object (timeout->stuff);
}


/* Scheme bridge.  */

SCM_SMOB_APPLY (scm_tc16_avahi_watch, apply_avahi_watch,
		1, 0, 0,
		(SCM watch, SCM events))
#define FUNC_NAME "%apply-watch"
{
  int c_fd;
  AvahiWatch *c_watch;
  AvahiWatchEvent c_events;

  c_watch = scm_to_avahi_watch (watch, 1, FUNC_NAME);
  c_events = scm_to_avahi_watch_events (events, 2, FUNC_NAME);
  c_fd = c_watch->fd;


  c_watch->callback (c_watch, c_fd, c_events,
		     c_watch->userdata);

  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

SCM_SMOB_APPLY (scm_tc16_avahi_timeout, apply_avahi_timeout,
		0, 0, 0,
		(SCM timeout))
#define FUNC_NAME "%apply-timeout"
{
  AvahiTimeout *c_timeout;

  c_timeout = scm_to_avahi_timeout (timeout, 1, FUNC_NAME);

  c_timeout->callback (c_timeout, c_timeout->userdata);

  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME


/* Public API.  */

AvahiGuilePoll *
avahi_guile_poll_new (SCM new_watch, SCM update_watch_x, SCM free_watch,
		      SCM new_timeout, SCM update_timeout_x, SCM free_timeout)
{
  AvahiGuilePoll *guile_poll;

  guile_poll = scm_malloc (sizeof (*guile_poll));

  guile_poll->api.watch_new = watch_new;
  guile_poll->api.watch_update = watch_update;
  guile_poll->api.watch_get_events = watch_get_events;
  guile_poll->api.watch_free = watch_free;

  guile_poll->api.timeout_new = timeout_new;
  guile_poll->api.timeout_update = timeout_update;
  guile_poll->api.timeout_free = timeout_free;

  guile_poll->api.userdata = guile_poll;

  guile_poll->new_watch = new_watch;
  guile_poll->free_watch = free_watch;
  guile_poll->update_watch_x = update_watch_x;
  guile_poll->new_timeout = new_timeout;
  guile_poll->free_timeout = free_timeout;
  guile_poll->update_timeout_x = update_timeout_x;

  return (guile_poll);
}

const AvahiPoll *
avahi_guile_poll_get (AvahiGuilePoll *guile_poll)
{
  return (&guile_poll->api);
}

void
avahi_guile_poll_free (AvahiGuilePoll *guile_poll)
{
  free (guile_poll);
}


/* Initialization.  */

void
scm_avahi_init_watch (void)
{
#include "watch.c.x"
}
