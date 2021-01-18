
#include <stdio.h>

/* this wrapper is meant to be setuid root */

#define SMBPASSWD "/usr/local/bin/smbpasswd"

main(int argc, char* argv[])
{
	setuid(geteuid());
	argv++;
	execvp(SMBPASSWD, argv);
	perror(*argv);
}
