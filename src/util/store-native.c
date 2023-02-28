// Copyright 2018-Present Modern Interpreters Inc.
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.


#include <stdio.h>
#include <unistd.h>
#include <sys/file.h>

static int make_lock(const char* filename, int op) {
        int fd = open(filename, O_RDWR|O_CREAT, 0644);
        if (fd < 0) {
                perror("Failed to create lock");
                return -1;
        }

        if (flock(fd, op | LOCK_NB) < 0) {
                perror("failed to get lock");
                close(fd);
                return -10;
        }
        return fd;

}

int util_store_file_lock(const char* filename) {
		return make_lock(filename, LOCK_EX);
}

int util_store_file_lock_shared(const char* filename) {
		return make_lock(filename, LOCK_SH);
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
