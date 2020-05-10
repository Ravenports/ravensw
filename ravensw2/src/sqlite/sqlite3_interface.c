/*
 * This file is covered by the Internet Software Consortium (ISC) License
 * Reference: ../../License.txt
 */

#ifndef _WIN32
#include <unistd.h>
#include <sqlite3.h>
#include "sqlite3_interface.h"

void
rdb_syscall_overload(void)
{
	sqlite3_vfs	*vfs;

	vfs = sqlite3_vfs_find(NULL);
	vfs->xSetSystemCall(vfs, "open", (sqlite3_syscall_ptr)dbdir_open);
	vfs->xSetSystemCall(vfs, "access", (sqlite3_syscall_ptr)dbdir_access);
	vfs->xSetSystemCall(vfs, "stat", (sqlite3_syscall_ptr)dbdir_stat);
	vfs->xSetSystemCall(vfs, "lstat", (sqlite3_syscall_ptr)dbdir_lstat);
	vfs->xSetSystemCall(vfs, "unlink", (sqlite3_syscall_ptr)dbdir_unlink);
	vfs->xSetSystemCall(vfs, "mkdir", (sqlite3_syscall_ptr)dbdir_mkdir);
}
#endif
