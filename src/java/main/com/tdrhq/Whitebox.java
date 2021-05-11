// Copyright 2018-Present Modern Interpreters Inc.
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

package com.tdrhq;

import java.lang.reflect.Array;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.Arrays;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import org.apache.commons.lang3.ArrayUtils;
import org.apache.commons.lang3.ClassUtils;
import org.apache.commons.lang3.reflect.MethodUtils;

class Whitebox {
    public final static Class[] EMPTY_CLASSES = new Class[0];

    public static class CacheKey {
        public final Class klass;
        public final String method;
        public final Class[] args;

        public CacheKey(Class klass, String method, Class[] args) {
            this.klass = klass;
            this.method = method;
            this.args = args;
        }

        @Override
        public boolean equals(Object _other) {
            CacheKey other = (CacheKey) _other;
            return klass.equals(other.klass) &&
                    method.equals(other.method) &&
                    Arrays.equals(args, other.args);
        }

        @Override
        public int hashCode() {
            return klass.hashCode() | method.hashCode() | Arrays.hashCode(args);
        }
    }

    static Map<CacheKey, Method> methodCache = new ConcurrentHashMap<>();

    static Object invokeMethod(Object o, String methodName, Object[] args) throws NoSuchMethodException, IllegalAccessException, InvocationTargetException {
        Method method = getMatchingAccessibleMethod(o.getClass(), methodName, args);
        if (method == null) {
            throw new RuntimeException("couldn't find an appropriate method");
        }

        // This *shouldn't* be needed, but the JVM causes some problems in particular
        // with some methods in ArrayList$Itr, oh well.
        method.setAccessible(true);

        return method.invoke(o, toVarArgs(method, args));
    }

    static Method getMatchingAccessibleMethod(Class klass, String methodName, Object[] args) {
        Class[] classes = EMPTY_CLASSES;

        if (args.length > 0) {
            classes = new Class[args.length];
            for (int i = 0; i < args.length; i++) {
                classes[i] = args[i] == null ? null : args[i].getClass();
            }
        }
        CacheKey cacheKey = new CacheKey(klass, methodName, classes);
        Method old = methodCache.get(cacheKey);
        if (old == null) {
            old = MethodUtils.getMatchingAccessibleMethod(klass, methodName, classes);
            if (old == null) {
                throw new RuntimeException("could not find appropriate method for: " + klass
                                           + ", " + methodName);
            }
            methodCache.put(cacheKey, old);
        }
        return old;
    }

    static Object invokeStaticMethod(Class klass, String methodName, Object[] args) throws NoSuchMethodException, IllegalAccessException, InvocationTargetException {
        Method method = getMatchingAccessibleMethod(klass, methodName, args);

        if (method == null) {
            throw new RuntimeException("couldn't find an appropriate method");
        }

        return method.invoke(null, toVarArgs(method, args));
    }



    private static Object[] toVarArgs(final Method method, Object[] args) {
        if (method.isVarArgs()) {
            final Class<?>[] methodParameterTypes = method.getParameterTypes();
            args = getVarArgs(args, methodParameterTypes);
        }
        return args;
    }

    /**
     * <p>Given an arguments array passed to a varargs method, return an array of arguments in the canonical form,
     * i.e. an array with the declared number of parameters, and whose last parameter is an array of the varargs type.
     * </p>
     *
     * @param args the array of arguments passed to the varags method
     * @param methodParameterTypes the declared array of method parameter types
     * @return an array of the variadic arguments passed to the method
     * @since 3.5
     */
    static Object[] getVarArgs(final Object[] args, final Class<?>[] methodParameterTypes) {
        if (args.length == methodParameterTypes.length
                && args[args.length - 1].getClass().equals(methodParameterTypes[methodParameterTypes.length - 1])) {
            // The args array is already in the canonical form for the method.
            return args;
        }

        // Construct a new array matching the method's declared parameter types.
        final Object[] newArgs = new Object[methodParameterTypes.length];

        // Copy the normal (non-varargs) parameters
        System.arraycopy(args, 0, newArgs, 0, methodParameterTypes.length - 1);

        // Construct a new array for the variadic parameters
        final Class<?> varArgComponentType = methodParameterTypes[methodParameterTypes.length - 1].getComponentType();
        final int varArgLength = args.length - methodParameterTypes.length + 1;

        Object varArgsArray = Array.newInstance(ClassUtils.primitiveToWrapper(varArgComponentType), varArgLength);
        // Copy the variadic arguments into the varargs array.
        System.arraycopy(args, methodParameterTypes.length - 1, varArgsArray, 0, varArgLength);

        if(varArgComponentType.isPrimitive()) {
            // unbox from wrapper type to primitive type
            varArgsArray = ArrayUtils.toPrimitive(varArgsArray);
        }

        // Store the varargs array in the last position of the array to return
        newArgs[methodParameterTypes.length - 1] = varArgsArray;

        // Return the canonical varargs array.
        return newArgs;
    }

}
