/*
 * This file is covered by the Internet Software Consortium (ISC) License
 * Reference: ../../License.txt
 */

#include <sys/stat.h>

int
vfs_dbdir_open(const char *path, int flags, int mode);

int
vfs_dbdir_access(const char *path, int mode);

int
vfs_dbdir_stat(const char * path, struct stat * sb);

int
vfs_dbdir_lstat(const char * path, struct stat * sb);

int
vfs_dbdir_unlink(const char *path);

int
vfs_dbdir_mkdir(const char *path, mode_t mode);
