/*
 * This file is covered by the Internet Software Consortium (ISC) License
 * Reference: ../License.txt
 */

/*
 * Copyright (c) 2014 Landon Fuller <landon@landonf.org>
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer
 *    in this position and unchanged.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE AUTHOR(S) ``AS IS'' AND ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
 * OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
 * IN NO EVENT SHALL THE AUTHOR(S) BE LIABLE FOR ANY DIRECT, INDIRECT,
 * INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
 * NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 * DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
 * THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
 * THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

#include <sys/param.h>
#include <sys/stat.h>

#include <assert.h>
#include <fcntl.h>
#include <stdarg.h>
#include <unistd.h>
#ifndef _WIN32
#include <pthread.h>

static pthread_mutex_t file_at_lock = PTHREAD_MUTEX_INITIALIZER;
static int file_at_dfd = -1;
static char saved_cwd[MAXPATHLEN];

/**
 * Acquire the cwd mutex and perform fchdir(dfd).
 *
 * On error, the mutex will be released automatically
 * and a non-zero value will be returned.
 *
 * @param dfd The directory file descriptor to be passed to fchdir(), or
 * AT_FDCWD to use the current working directory.
 * @return The fchdir() result.
 */
static int
file_chdir_lock(int dfd)
{
	int ret;

	pthread_mutex_lock(&file_at_lock);

	if (getcwd(saved_cwd, sizeof(saved_cwd)) == NULL)
		saved_cwd[0] = '\0';

	assert(file_at_dfd == -1);
	file_at_dfd = dfd;

	if (dfd == AT_FDCWD)
		return 0;

	ret = fchdir(dfd);
	if (ret != 0) {
		pthread_mutex_unlock(&file_at_lock);
		return(ret);
	} else {
		return(ret);
	}
}

/**
 * Release the cwd mutex.
 */
static void
file_chdir_unlock(int dfd)
{
	assert(file_at_dfd == dfd);
	file_at_dfd = -1;

	if (dfd == AT_FDCWD)
		return;

	if (saved_cwd[0] != '\0')
		chdir(saved_cwd);
	pthread_mutex_unlock(&file_at_lock);
}

int
port_faccessat(int fd, const char *path, int mode, int flag)
{
	int ret;

	if ((ret = file_chdir_lock(fd) != 0))
		return(ret);

	ret = access(path, mode);

	file_chdir_unlock(fd);
	return(ret);
}

int
port_unlinkat(int fd, const char *path, int flag)
{
	int ret;

	if ((ret = file_chdir_lock(fd) != 0))
		return(ret);

	if (flag & AT_REMOVEDIR) {
		ret = rmdir(path);
	} else {
		ret = unlink(path);
	}

	file_chdir_unlock(fd);
	return(ret);
}

int port_lstatat (int fd, const char *path, struct stat *buf)
{
  return fstatat(fd, path, sb, AT_SYMLINK_NOFOLLOW);
}

int
port_mkdirat(int fd, const char *path, int mode)
{
  int ret;

  if ((ret = file_chdir_lock(fd) != 0))
    return(ret);

  ret = mkdir (path, mode);

  file_chdir_unlock(fd);
  return(ret);
}

#endif
