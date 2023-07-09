#include <stdio.h>
#include <unistd.h>
#include <sys/wait.h>
#include <stdlib.h>
#include <fcntl.h>
#include <errno.h>
#include <string.h>

#define BUF_SIZE 800
#define KEY "Opening transaction log lock"

typedef struct _process {
        int fd;
        char buf[BUF_SIZE + 10];
        size_t pos;
} process;

#define PCOUNT 2

process processes[PCOUNT];

int stream_child_tick(process *p) {
        if (p->pos == BUF_SIZE) {
                // Long line, can't do anything about it.
                p->pos = 0;
        }

        ssize_t nread = read(p->fd, p->buf + p->pos, BUF_SIZE - p->pos);
        if (nread < 0) {
                if (errno == EAGAIN || errno == EWOULDBLOCK
                    || errno == EINTR) {
                        return -1; // Try again
                } else {
                        perror("Failed to read from child");
                        exit(1);
                }
        }

        if (nread == 0) {
                return 0;
        }

        write(STDOUT_FILENO, p->buf + p->pos, nread);

        p->pos += nread;
        p->buf[p->pos] = 0;
        char *newline = strrchr(p->buf, '\n');
        if (newline) {
                newline[0] = 0;

                //printf("\n\nline is: %s\n\n", buf);
                if (strstr(p->buf, KEY)) {
                        printf("Found key!\n");
                        fflush(stdout);
                }

                int newlen = p->pos - (newline - p->buf) - 1;
                memmove(p->buf, newline+1, newlen);
                p->pos = newlen;

        }

        return nread;
}

void stream_child(process* p) {
        while (1) {
                int ret = stream_child_tick(p);
                if (ret == 0) {
                        goto cleanup;
                }
        }

cleanup:
        close(p->fd);
}

int child(int pipefd[2], char** argv) {
        close(pipefd[0]);
        printf("Message from child process 1\n");
        FILE *f = fdopen(pipefd[1], "w");
        if (!f) {
                perror("Failed to fdopen");
                return 1;
        }

        fprintf(f, "Inner message1\n");
        fflush(f);

        // Redirect stdout to the pipe
        if (dup2(pipefd[1], STDOUT_FILENO) < 0) {
                perror("Could not dup2");
                return -1;
        };
        /*
        if (dup2(pipefd[1], STDERR_FILENO) < 0) {
                perror("Could not dup2 on stderr");
                return -1;
                };*/

        close(pipefd[1]);

        printf("Message from child process\n");

        execvp(argv[1], argv + 1);
        perror("execvp failed");
        return 1;
}

int main(int argc, char** argv) {
        if (argc < 2) {
                fprintf(stderr, "Need arguments to launch\n");
                return -1;
        }
        printf("Launching command %s\n", argv[1]);
        fflush(stdout);

        int pipefd[2];
        if (pipe(pipefd) != 0) {
                perror("pipe failed");
                return -1;
        }
        printf("Got pipe!: %d, %d, %d\n", pipefd[0], pipefd[1], getpid());


        pid_t pid = fork();
        if (pid == -1) {
                perror("fork failed");
                return -1;
        }

        if (pid == 0) {  // child
                return child(pipefd, argv);
        }

        close(pipefd[1]);

        fcntl(pipefd[0], F_SETFL, O_NONBLOCK);

        FILE* child = fdopen(pipefd[0], "r");

        if (!child) {
                perror("fdopen");
                return 1;
        }

        process p;
        p.pos = 0;
        p.fd = pipefd[0];
        stream_child(&p);

        int wstatus = 0;
        pid_t wpid = waitpid(pid, &wstatus, 0);
        if (wpid < 0) {
                perror("waitpid failed");
                return 1;
        }

        int status = WEXITSTATUS(wstatus);
        if (status != 0) {
                fprintf(stderr, "Child process crashed with %d\n", status);
        }
        return status;
}
