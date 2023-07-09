#include <stdio.h>
#include <unistd.h>
#include <sys/wait.h>
#include <stdlib.h>
#include <fcntl.h>
#include <errno.h>
#include <string.h>
#include <sys/param.h>
#include <signal.h>
#include <stdlib.h>
#include <unistd.h>

#define BUF_SIZE 800
#define KEY "Opening transaction log lock"

typedef struct _process {
        int pid;
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

void cleanup(process* ps) {
        close(ps->fd);
        ps->fd = -1; // Indicator that we're dead.

        int wstatus = 0;
        pid_t wpid = waitpid(ps->pid, &wstatus, 0);
        if (wpid < 0) {
                perror("waitpid failed");
                exit(1);
        }

        int status = WEXITSTATUS(wstatus);
        if (status != 0) {
                fprintf(stderr, "Child process %d crashed with %d\n", ps->pid, status);
        }
}

void stream_children(process* ps, int pcount) {
        fd_set rfds;
        FD_ZERO(&rfds);
        int maxfd = 0;
        for (int i = 0; i < pcount; i++) {
                if (ps[i].fd >= 0) {
                        FD_SET(ps[i].fd, &rfds);
                        maxfd = MAX(ps[i].fd, maxfd);
                }
        }

        struct timeval tv;
        tv.tv_sec = 5;
        tv.tv_usec = 0;

        int retval = select(maxfd + 1, &rfds, NULL, NULL, &tv);

        if (retval == -1) {
                if (errno == EINTR) {
                        return;
                }
                perror("select() failed");
                exit(1);
        } else {
                printf("Got %d ready fds\n", retval);
                // retval is the count of fds
                for (int i = 0; i < pcount; i++) {
                        if (FD_ISSET(ps[i].fd, &rfds)) {
                                int ret = stream_child_tick(&ps[i]);
                                if (ret == 0) {
                                        cleanup(&ps[i]);
                                }
                        }
                }
        }

}

int count_active() {
        int count = 0;
        for (int i = 0; i < PCOUNT; i ++) {
                if (processes[i].fd >= 0) {
                        count ++;
                }
        }
        return count;
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

int unused_slot() {
        for (int i = 0; i < PCOUNT; i++) {
                if (processes[i].fd < 0) {
                        return i;
                }
        }
        return -1;
}

void launch(int argc, char** argv) {
        if (unused_slot() < 0) {
                fprintf(stderr, "Attempting to launch without unused slot\n");
                return;
        }
        printf("Launching command %s\n", argv[1]);
        fflush(stdout);

        int pipefd[2];
        if (pipe(pipefd) != 0) {
                perror("pipe failed");
                exit(1);
        }
        printf("Got pipe!: %d, %d, %d\n", pipefd[0], pipefd[1], getpid());


        pid_t pid = fork();
        if (pid == -1) {
                perror("fork failed");
                exit(1);
        }

        if (pid == 0) {  // child
                child(pipefd, argv);
                return;
        }

        close(pipefd[1]);

        fcntl(pipefd[0], F_SETFL, O_NONBLOCK);

        int slot = unused_slot();

        processes[slot].pos = 0;
        processes[slot].fd = pipefd[0];
        processes[slot].pid = pid;
}

int hup_called = 0;
int _argc;
char** _argv;

void hup_handler(int) {
        printf("Got SIGHUP!\n");
        hup_called = 1;
}

void setup_signals() {
        struct sigaction act = { 0 };
        act.sa_flags = SA_SIGINFO ;
        act.sa_handler = &hup_handler;
        if (sigaction(SIGHUP, &act, NULL) == -1) {
                perror("sigaction failed");
                exit(1);
        }

}

int main(int argc, char** argv) {
        _argc = argc;
        _argv = argv;

        if (argc < 2) {
                fprintf(stderr, "Need arguments to launch\n");
                return -1;
        }

        for (int i = 0; i < PCOUNT; i++) {
                processes[i].fd = -1;
        }

        setup_signals();

        launch(argc, argv);

        while (count_active()) {
                if (hup_called) {
                        launch(argc, argv);
                        hup_called = 0;
                }
                printf("in here\n");
                stream_children(processes, PCOUNT);
        }
}
