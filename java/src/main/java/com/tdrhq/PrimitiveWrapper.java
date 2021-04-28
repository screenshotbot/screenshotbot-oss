// Copyright 2018-Present Modern Interpreters Inc.
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

package com.tdrhq;

public class PrimitiveWrapper {
    private Object obj;
    public PrimitiveWrapper(Object obj) {
        this.obj = obj;
    }

    /**
     *
     */
    public int getType() {
        if (obj instanceof Boolean) {
            return 1;
        } else if (obj instanceof Character) {
            return 2;
        }

        throw new RuntimeException("unsupported type:" + obj);
     }

    public Boolean asBoolean() {
        return (Boolean) obj;
    }

    public Character asCharacter() {
        return (Character) obj;
    }
}
