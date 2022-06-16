#include <openssl/sha.h>
#include <openssl/md5.h>
#include <stdlib.h>
#include <string.h>

int tdrhq_sizeof_SHA256_CTX() {
        return sizeof(SHA256_CTX);
}

int tdrhq_sizeof_MD5_CTX() {
        return sizeof(MD5_CTX);
}
