
/* this is only a prototype */

#include <sys/types.h>
#include <sys/socket.h>
#include <sys/syscall.h>
#include <sys/syslimits.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <errno.h>

const char* bknr_prefix = "/bknr";
const unsigned short server_port = 2931;

static int in_bknr = 0;
static sockfd = -1;

void
open_socket()
{
  if (sockfd == -1) {
    struct sockaddr_in serv_name;

    sockfd = socket(AF_INET, SOCK_STREAM, 0);

    if (sockfd == -1) {
      perror("socket creation for pathname translation failed");
      abort();
    }

    serv_name.sin_family = AF_INET;
    serv_name.sin_addr.s_addr = inet_addr("127.0.0.1");
    serv_name.sin_port = htons(server_port);

    if (connect(sockfd, (struct sockaddr*) &serv_name, sizeof serv_name) < 0) {
      perror("socket connect failed for pathname translation");
      abort();
    }
  }
}

char*
send_command(const char* command, const char* argument)
{
  open_socket();

#define DO_WRITE(buf) \
	if (write(sockfd, buf, strlen(buf)) != strlen(buf)) { \
 		perror("write error to pathname translation server"); \
		abort(); \
	}

  DO_WRITE(command);
  DO_WRITE(" ");
  DO_WRITE(argument);
  DO_WRITE("\n");

  /* xxx read must be much better, we can't assume to receive the whole 
   * answer in one read()
   */
  {
    size_t buflen = 2*PATH_MAX; /* there be buffer overflows */
    char* reply = calloc(1, buflen);
    int read_len = read(sockfd, reply, buflen);

    char* lf = strchr(reply, '\n');

    if (!lf) {
      fprintf(stderr, "received incomplete reply [%s] from pathname "
	      "translation server\n");
      abort();
    }

    *lf = 0;

    return reply;
  }
}

static int
is_bknr_path(const char* path)
{
  return
    (strncmp(path, bknr_prefix, strlen(bknr_prefix)) == 0)
    || (*path != '/' && in_bknr);
}

static char*
translate_path(const char* path)
{
  char* translated_path = 0;

  if (!is_bknr_path(path)) {
    /* none of our business */
    translated_path = strdup(path);

    if (!translated_path) {
      fprintf(stderr, "no memory available for pathname translation\n");
      abort();
    }
  } else {
    /* translate this path */
    translated_path = send_command("translate", path);
  }

  fprintf(stderr, "*************** translated %s to %s\n",
	  path, translated_path);

  return translated_path;
}

int
open(const char* path, int flags, int mode)
{
  char* translated_path = translate_path(path);
  int retval = syscall(SYS_open, translated_path, flags, mode);
  free(translated_path);
  return retval;
}

int
chdir(const char* path)
{
  if (*path == '/') {
    if (is_bknr_path(path)) {
      in_bknr = 1;
    } else {
      in_bknr = 0;
    }
  } else {
    if (in_bknr) {
      errno = ENOTDIR;
      fprintf(stderr, "*************** can't chdir in %s yet\n",
	      bknr_prefix);
      return -1;
    }
  }

  return syscall(SYS_chdir, path);
}
