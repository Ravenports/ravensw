/*-
 * Copyright (c) 2011 The NetBSD Foundation, Inc.
 * All rights reserved.
 *
 * This code is derived from software contributed to The NetBSD Foundation
 * by Christos Zoulas.
 *
 * Copyright (c) 2009 David Schultz <das@FreeBSD.org>
 * All rights reserved.
 *
 * Copyright (c) 2019 John Marino <draco@marino.st>
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE NETBSD FOUNDATION, INC. AND CONTRIBUTORS
 * ``AS IS'' AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
 * TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
 * PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE FOUNDATION OR CONTRIBUTORS
 * BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
 * CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
 * SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
 * INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
 * CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
 * ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
 * POSSIBILITY OF SUCH DAMAGE.
 */

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <errno.h>
#include <string.h>
#include <limits.h>

#define PORT_POWER_OF_2(x)	((((x)-1)&(x))==0)
#define CHUNK_SIZE		128

/*
 * Expand *linep to hold at least len bytes (up to SSIZE_MAX + 1).
 * len cannot be zero (causes PORT_POWER_OF_2 to fail,
 * plus that doesn't even make sense as an argument).
 */
static inline int
expandtofit(char ** restrict linep, size_t len, size_t * restrict capp)
{
	char *newline;
	size_t newcap;

	if (len == 0) {
		errno = EINVAL;
		return (-1);
	}
	if (len > (size_t)SSIZE_MAX + 1) {
		errno = EOVERFLOW;
		return (-1);
	}
	if (len > *capp) {
		if (len == (size_t)SSIZE_MAX + 1) {
			/* avoid overflow */
			newcap = (size_t)SSIZE_MAX + 1;
		}
		else {
			/* Round buffer size to next power of 2 */
			if (!PORT_POWER_OF_2(len)) {
				len--;
				len |= len >> 1;
				len |= len >> 2;
				len |= len >> 4;
				len |= len >> 8;
				len |= len >> 16;
#if ULONG_MAX > 0xffffffffU
				len |= len >> 32;
#endif
				len++;
			}
			newcap = len;
		}
		newline = realloc(*linep, newcap);
		if (newline == NULL)
			return (-1);
		*capp = newcap;
		*linep = newline;
	}
	return (0);
}

/*
 * Append the src buffer to the *dstp buffer. The buffers are of
 * length srclen and *dstlenp, respectively, and dst has space for
 * *dstlenp bytes. After the call, *dstlenp and *dstcapp are updated
 * appropriately, and *dstp is reallocated if needed. Returns 0 on
 * success, -1 on allocation failure.
 */
static int
sappend(char ** restrict dstp, size_t * restrict dstlenp,
	size_t * restrict dstcapp, char * restrict src, size_t srclen)
{
	/* ensure room for srclen + dstlen + terminating NUL */
	if (expandtofit(dstp, srclen + *dstlenp + 1, dstcapp))
		return (-1);
	memcpy(*dstp + *dstlenp, src, srclen);
	*dstlenp += srclen;
	return (0);
}

/* single threaded version of getline */

ssize_t
port_getline(char ** restrict linep, size_t * restrict linecapp,
	FILE * restrict fp)
{
	char *endp;
	size_t resultlen;
	char chunk[CHUNK_SIZE];

	if (linep == NULL || linecapp == NULL) {
		errno = EINVAL;
		return(-1);
	}

	if (*linep == NULL)
		*linecapp = 0;

	/* If fp is at EOF already, we just need space for the NUL. */
	if (feof(fp)) {
		if (expandtofit(linep, 1, linecapp) == 0) {
			(*linep)[0] = '\0';
		}
		return(-1);
	}

	if (*linecapp == 0) {
		if (expandtofit(linep, BUFSIZ, linecapp) != 0)
			return(-1);
	}

	resultlen = 0;
	do
	{
		if (fgets(chunk, CHUNK_SIZE, fp) == NULL)
			return(-1);
		endp = memchr(chunk, '\n', CHUNK_SIZE);
		if (endp == NULL) { 
			/* No newline found, append entire chuck and keep going */
			if (sappend(linep, &resultlen, linecapp, chunk, CHUNK_SIZE-1) != 0)
				return(-1);
		}
		else {
			/* Newline found, append first part of chunk and exit loop */
			/* snarf the newline character too */
			endp++;
			if (sappend(linep, &resultlen, linecapp, chunk, endp - chunk) != 0)
				return(-1);
		}
	}
	while (endp == NULL);

	/* terminate the string */
	(*linep)[resultlen] = '\0';
	return(resultlen);
}
