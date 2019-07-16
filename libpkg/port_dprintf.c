/*
 * Copyright (c) 2019 John Marino <draco@marino.st>
 */

#include <stdio.h>
#include <unistd.h>
#include <stdarg.h>

int
port_dprintf(int fd, const char *fmt, ...)
{
	FILE *fp;
	int e;
	va_list ap;

	if ((e = dup(fd)) == -1)
		return -1;

	if ((fp = fdopen(e, "r+")) == NULL) {
		(void)close(e);
		return -1;
	}

	va_start(ap, fmt);
	e = vfprintf(fp, fmt, ap);
	va_end(ap);

	fclose(fp);
	return e;
}
