#include <openssl/sha.h>
#include <stdlib.h>
#include <string.h>

int tdrhq_sizeof_SHA256_CTX() {
        return sizeof(SHA256_CTX);
}
