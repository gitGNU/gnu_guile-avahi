#ifndef AVAHI_POLL_SUPPORT_H
#define AVAHI_POLL_SUPPORT_H

#include <avahi-common/watch.h>

#include <libguile.h>

typedef struct AvahiGuilePoll AvahiGuilePoll;

extern AvahiGuilePoll *avahi_guile_poll_new (SCM new_watch,
					     SCM free_watch,
					     SCM new_timeout,
					     SCM free_timeout);

extern const AvahiPoll *avahi_guile_poll_get (AvahiGuilePoll *guile_poll);

extern void avahi_guile_poll_free (AvahiGuilePoll *guile_poll);

extern void scm_avahi_init_watch (void);

#endif
