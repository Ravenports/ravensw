#ifndef PORT_PATHS_H
#define PORT_PATHS_H

/*
 * So far only Solaris doesn't have this header
 */

#ifdef __sun__
#define	_PATH_BSHELL	"/usr/xpg4/bin/sh"
#define	_PATH_TTY	"/dev/tty"
#define	_PATH_DEVNULL	"/dev/null"
#define _PATH_LIBC64	"/lib/64/libc.so.1"
#define	_PATH_TMP	"/tmp/"
#endif

#ifdef __linux__
#define _PATH_HOSTNAME	"/bin/hostname"
#endif

#endif /* PORT_PATHS_H */
