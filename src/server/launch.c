#include <stdio.h>
#include <unistd.h>
#include <sys/wait.h>
#include <stdlib.h>

void stream_child(FILE* child) {
        char *line = NULL;
        size_t len = 0;

        ssize_t nread;

        while ((nread = getline(&line, &len, child)) != -1) {
                printf("%s", line);
        }

        if (line)
                free(line);

        fclose(child);
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
        FILE* child = fdopen(pipefd[0], "r");

        if (!child) {
                perror("fdopen");
                return 1;
        }

        stream_child(child);

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
