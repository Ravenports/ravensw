/*
 * Copyright (c) 2019 John Marino <draco@marino.st>
 */

#ifdef HAVE_CONFIG_H
#include "pkg_config.h"
#endif

#include <fcntl.h>
#ifndef HAVE_FLOCK
#include <errno.h>
#endif

#include "pkg.h"	/* pulls in <sys/file.h> */

int
port_flock(int fd, int operation)
{
#ifdef HAVE_FLOCK
	return(flock(fd, operation));
#else
	/* solaris */
	int rc = 0;

#if defined(F_SETLK) && defined(F_SETLKW)
	struct flock fl = {0};

	switch (operation & (LOCK_EX|LOCK_SH|LOCK_UN)) {
	case LOCK_EX:
		fl.l_type = F_WRLCK;
		break;

	case LOCK_SH:
		fl.l_type = F_RDLCK;
		break;

	case LOCK_UN:
		fl.l_type = F_UNLCK;
		break;

	default:
		errno = EINVAL;
		return -1;
	}

	fl.l_whence = SEEK_SET;
	rc = fcntl(fd, operation & LOCK_NB ? F_SETLK : F_SETLKW, &fl);

	if (rc && (errno == EAGAIN))
		errno = EWOULDBLOCK;
#endif

	return(rc);
#endif
}
