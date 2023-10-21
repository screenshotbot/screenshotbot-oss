package io.screenshotbot;

import android.app.Activity;
import android.app.Instrumentation;
import android.content.Intent;
import android.content.pm.PackageManager;
import android.os.Bundle;
import android.os.Debug;
import android.util.Log;
//import com.lispworks.Manager;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.io.PrintStream;
import java.lang.reflect.Constructor;

import android.content.Context;
import android.util.Log;
import com.lispworks.LispCalls;
import com.lispworks.Manager;
import com.lispworks.Manager.LispErrorReporter;
import java.io.File;
import java.io.InputStream;
import io.screenshotbot.LispworksManager;

public class LispworksManager {
    public static void initLispworks(Context targetContext, Context context, Runnable callback) {
        //System.loadLibrary("sbshowkase");
        File dataDir = targetContext.getDir("lispworks", 0);
        Manager.setRuntimeLispHeapDir(dataDir.toString());
        Manager.setLispTempDir(targetContext.getCacheDir().toString());
        Manager.setClassLoader(LispworksManager.class.getClassLoader());

        Manager.setErrorReporter(new LispErrorReporter() {
                @Override
                public boolean report(String errorString, String filename)  {
                    Log.i("SbInss", "LispWorks Error from " + filename + ": " + errorString);
                    // System.exit(1);
                    return false;
                }
            });

        int ret = Manager.init(context, "sbshowkase", new Runnable () {
                @Override
                public void run() {
                    if (Manager.status() < 0) {
                        Log.i("SbInss", "LispWorks failed to load: " + Manager.status() + " " + Manager.init_result_code());
                    }

                    Log.i("SbInss", "lispworks mInitString is: " + String.valueOf(Manager.mInitErrorString));

                    Log.i("SbInss", "callback called2");
                    try {
                        Object res = LispCalls.callObjectV("SCREENSHOTBOT/SHOWKASE/MAIN::SAMPLE");
                        Log.i("SbInss", "after the call");
                        Log.i("SbInss", "Got first response from lisp: " + res);
                    } catch (Exception e) {
                        Log.i("SbInss", "Got error" + e);
                    }

                    callback.run();

                }
            });

        Log.i("SbInss", "lispworks inited with " + ret);
        // try {
        //     Thread.sleep(20);
        // } catch (InterruptedException e) {
        //     throw new RuntimeException(e);
        // }

        //Log.i("SbInss", "calling into lisp");

        //Object res = LispCalls.callObjectV("cl:+", 2, 2);
        //Log.i("SbInss", "Got first response from lisp: " + res);

        //startSwank(4005);
    }

    public static void startSwank(int port) {
        Object res = LispCalls.callObjectV("screenshotbot/showkase/main:start-slynk", port);
        if (!"success".equals(res)) {
            throw new RuntimeException("Could not load swank");
        }
    }

}
