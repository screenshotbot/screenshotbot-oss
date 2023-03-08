function makeMatrix(a, b, c, d, e, f) {
    return new DOMMatrix([a, b, c, d, e, f]);
}

function _mat_mstar(a, b) {
    if (a instanceof DOMMatrixReadOnly && b instanceof DOMPointReadOnly) {
        return b.matrixTransform(a);
    } else if (b instanceof DOMMatrixReadOnly) {
        // Scalar times matrix. Tell me a better way to do this, please.
        return new DOMMatrixReadOnly([
            a * b.a,
            a * b.b,
            a * b.c,
            a * b.d,
            a * b.e,
            a * b.f]);
    } else {
        throw new Error("unimpl for " + a + " " + b);
    }
}

function _vec_vec3(a, b) {
    return new DOMPoint(a, b);
}

function _vec_v(a, b) { // this is actually `v-`.
    return new DOMPoint(a.x - b.x,
                        a.y - b.y);
}

function _vec_vstar(mult, vec) {
    return new DOMPoint(mult * vec.x,
                        mult * vec.y);
}

function _vec_vx3(vec) {
    return vec.x;
}

function _vec_vy3(vec) {
    return vec.y;
}

function _mat_mplus(v1, v2) {
    if (v1 instanceof DOMPointReadOnly && v2 instanceof DOMPointReadOnly) {
        return new DOMPoint(v1.x + v2.x,
                            v1.y + v2.y);
    } else if (v1 instanceof DOMMatrixReadOnly && v2 instanceof DOMMatrixReadOnly) {
        return new DOMMatrix([
            v1.a + v2.a,
            v1.b + v2.b,
            v1.c + v2.c,
            v1.d + v2.d,
            v1.e + v2.e,
            v1.f + v2.f]);
    } else {
        throw new Error("mplus unimplemented for " +  v1 + " " + v2);
    }
}
