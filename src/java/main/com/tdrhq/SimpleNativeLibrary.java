// Copyright 2018-Present Modern Interpreters Inc.
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

package com.tdrhq;

import java.io.FileOutputStream;
import java.io.InputStream;
import java.io.OutputStream;
import java.io.Serializable;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.logging.Logger;
import org.apache.commons.lang3.reflect.ConstructorUtils;
import org.apache.commons.lang3.reflect.FieldUtils;

public class SimpleNativeLibrary implements Serializable {
    private static final long serialVersionUID = -1372385985412438808L;
    public static Object bfalse = new Object();
    public static Object btrue = new Object();

    public SimpleNativeLibrary() {
    }

    public static void copyResource(String resourceName, String outputFile) throws Exception {
        InputStream is = SimpleNativeLibrary.class.getResourceAsStream(resourceName);
        if (is == null) {
            throw new RuntimeException("Could not find resource: " + resourceName);
        }
        OutputStream os = new FileOutputStream(outputFile);

        byte[] buff = new byte[4096];
        int len;
        while ((len = is.read(buff)) > 0) {
            os.write(buff, 0, len);
        }

        os.close();
        is.close();
    }

    public static Object send_static_method_wrapped(Class klass, String methodName, Object[] args) throws Exception {
        Object ret = send_static_method(klass, methodName, args);
        return wrapPrimitives(ret);
    }

    public static Object wrapPrimitives(Object ret) {
        if (ret == null) {
            return ret;
        }

        if (ret instanceof Boolean) {
            return new PrimitiveWrapper(ret);
        }

        if (ret instanceof Character) {
            return new PrimitiveWrapper(ret);
        }

        return ret;
    }

    public static Object send_static_method(Class klass, String methodName, Object[] args) throws Exception {
        if (klass == null) {
            throw new RuntimeException("null class for " + methodName);
        }
        if (args == null) {
            //            args = new Object[0];
        }
        try {
            Object ret =  Whitebox.invokeStaticMethod(klass, methodName, args);
            return ret;
        } catch (NoSuchMethodException e) {
            throw new RuntimeException(e);
        } catch (InvocationTargetException e) {
            if (e.getTargetException() instanceof Exception) {
                throw (Exception) e.getTargetException();
            }

            e.printStackTrace();
            throw new RuntimeException(e);
        } catch (IllegalAccessException e) {
            throw new RuntimeException(e);
        }
    }


    public static Object send_method(Object o, String methodName, Object[] args) throws Throwable {
        try {
            return Whitebox.invokeMethod(o, methodName, args);
        } catch (NoSuchMethodException e) {
            e.printStackTrace();
            throw new RuntimeException(e);
        } catch (InvocationTargetException e) {
            e.printStackTrace();
            throw e.getCause();
        } catch (IllegalAccessException e) {
            e.printStackTrace();
            throw new RuntimeException(e);
        }
    }


    public static Object send_method_wrapped(Object o, String methodName, Object[] args) throws Throwable {
        return wrapPrimitives(send_method(o, methodName, args));
    }

    public static Object newInstance(Class klass, Object[] args)
        throws Throwable {
        try {
            return ConstructorUtils.invokeConstructor(klass, args);
        } catch (InvocationTargetException e) {
            throw e.getCause();
        }
    }

    public static Object newInstance_wrapped(Class klass, Object[] args)
        throws Throwable {
        if (args == null) {
            System.out.println("got null args");
            args = new Object[0];
        }
        if (klass == null) {
            throw new RuntimeException("klass is null");
        }

        try {
            System.out.println("Using args: " + klass + " " + args);
            return wrapPrimitives(newInstance(klass, args));
        } catch (Exception e) {
            e.printStackTrace();
            throw e;
        }
    }

    public static Object getLogger() {
        return Logger.getLogger("io.jipr.lisp");
    }

    public static Character charFromCodePoint(char ch) {
        return Character.valueOf(ch);
    }

    public static Object readStaticField(Class klass, String field) throws Exception {
        return FieldUtils.readStaticField(klass, field, true);
    }

    public static Object writeStaticField(Class klass, String field, Object val) throws Exception {
        FieldUtils.writeStaticField(klass, field, val);
        return val;
    }

    public static Object readField(Object o, String field) throws Exception {
        return FieldUtils.readField(o, field, true);
    }

    public static Object writeField(Object o, String field, Object val) throws Exception {
        FieldUtils.writeField(o, field, val);
        return val;
    }

    public static void throwJavaException(Exception e) throws Exception {
        throw e;
    }
}
