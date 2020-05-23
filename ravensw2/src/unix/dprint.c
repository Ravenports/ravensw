/*
 * This file is covered by the Internet Software Consortium (ISC) License
 * Reference: ../../License.txt
 */

#include <stdio.h>
#include <unistd.h>

int
dprint(int fd, const char *msg)
{
	FILE *fp;
	int e;

	if ((e = dup(fd)) == -1)
		return (-1);

	if ((fp = fdopen(e, "w")) == NULL) {
		(void)close(e);
		return (-1);
	}

	e = fprintf(fp, "%s", msg);

	fclose(fp);
	return (e);
}
