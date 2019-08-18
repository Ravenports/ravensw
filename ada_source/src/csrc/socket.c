/*
 * This file is covered by the Internet Software Consortium (ISC) License
 * Reference: ../License.txt
 */

#ifndef _WIN32
#include <sys/socket.h>
#include <sys/un.h>
#endif
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>
#include <string.h>

int
detect_IPC (const char *path)
{
  struct stat st;

  if (stat(path, &st) != 0) {
    return (-1);
  }

  if (S_ISFIFO(st.st_mode)) {
    return (1);
  }

#ifdef S_ISSOCK
  if (S_ISSOCK(st.st_mode)) {
    return (2);
  }
#endif

  return (3);
}

#ifdef _WIN32
int
connect_socket (const char *path, int newfd) {
  return (5);
}
#else
int
connect_socket (const char *path, int newfd)
{
  struct sockaddr_un sock;

  memset(&sock, 0, sizeof(struct sockaddr_un));
  sock.sun_family = AF_UNIX;

  if (strlen (path) >= sizeof(sock.sun_path)) {
    newfd = -1;
    return (3);
  }
  strncpy (sock.sun_path, path, sizeof(sock.sun_path));

  newfd = socket(AF_UNIX, SOCK_STREAM, 0);
  if (newfd == -1) {
    return (2);
  }

  if (connect(newfd, (struct sockaddr *)&sock, SUN_LEN(&sock)) == -1) {
    close (newfd);
    newfd = -1;
    return (4);
  }

  return (1);
}
#endif
