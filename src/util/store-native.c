// Copyright 2018-Present Modern Interpreters Inc.
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.


#include <stdio.h>
#include <unistd.h>
#include <sys/file.h>

int util_store_file_lock(const char* filename) {
        int fd = open(filename, O_RDWR|O_CREAT, 0644);
        if (fd < 0) {
                perror("Failed to create lock");
                return -1;
        }

        if (flock(fd, LOCK_EX | LOCK_NB) < 0) {
                perror("failed to get exclusive lock");
                close(fd);
                return -10;
        }
        return fd;
}

int util_store_file_unlock(int fd) {
        if (fd < 0) {
                fprintf(stderr, "Incorrect fd used in unlock, disregarding");
                return 0;
        }

        if (flock(fd, LOCK_UN) < 0){
                perror("Failed to unlock");
                return -1;
        }

        close(fd);
        return 0;
}
