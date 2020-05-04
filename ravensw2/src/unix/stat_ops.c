/*
 * This file is covered by the Internet Software Consortium (ISC) License
 * Reference: ../../License.txt
 */

#include <sys/types.h>
#include <sys/stat.h>

/* if fileowner == 0, root ownership and no group or other
   read access.  if fileowner != 0, require no other read
   access and group read access IFF the group ownership ==
   filegroup */

#ifdef _WIN32
#define S_IWGRP 0
#define S_IWOTH 0
#define S_ISLNK
#endif


int
bad_perms (int fileowner, int filegroup, struct stat *sb)
{
  if (fileowner == 0) {
    if ((sb->st_mode & (S_IWGRP|S_IWOTH)) != 0)
      return(1);
  } else {
    if ((sb->st_mode & S_IWOTH) != 0)
      return(1);
    if (sb->st_gid != filegroup && (sb->st_mode & S_IWGRP) != 0)
      return(1);
  }
  return(0);
}

int
wrong_owner (int fileowner, int filegroup, struct stat *sb)
{
  if (fileowner == 0) {
    if (sb->st_uid != fileowner)
      return(1);
  } else {
    if (sb->st_uid != 0 && sb->st_uid != fileowner && sb->st_gid != filegroup)
      return(1);
  }
  return(0);
}

long
get_mtime (struct stat *sb)
{
  return sb->st_mtime;
}

int
is_link (struct stat *sb)
{
  return S_ISLNK(sb->st_mode);
}

off_t
get_size (struct stat *sb)
{
  return sb->st_size;
}

