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
import java.io.*;
import android.os.*;
import android.graphics.*;

import com.facebook.testing.screenshot.ScreenshotRunner;
import org.junit.*;
import org.junit.runners.model.*;
import org.junit.runner.*;
import org.junit.rules.*;
import androidx.compose.ui.test.junit4.ComposeTestRule;

public class SbInstrumentation extends Instrumentation {
    @Override
    public void onCreate(Bundle arguments) {
        Log.i("SbInss", "onCreate called");
        super.onCreate(arguments);

        ScreenshotRunner.onCreate(this, arguments);

        Context targetContext = getTargetContext();
        Context context = getContext();
        LispworksManager.initLispworks(getTargetContext(), getContext(),
                                       new Runnable() {
                                           @Override
                                           public void run() {
                                               if (Looper.myLooper() == Looper.getMainLooper()) {
                                                   Log.i("SbInss", "callback is running on main thread");
                                               }
                                               Thread th = new Thread() {
                                                       @Override
                                                       public void run() {
                                                           LispCalls.callObjectV("SCREENSHOTBOT/SHOWKASE/MAIN::RUN", context, targetContext, SbInstrumentation.this);
                                                           Log.i("SbInss", "Done initing slynk");
                                                       }
                                                   };
                                               th.start();
                                               //finish(0, new Bundle());
                                           }
                                       }

                                       );

        start();
    }

    public void writeBitmap(Bitmap bitmap, String file) throws IOException {
        try (OutputStream os = new FileOutputStream(file)) {
            bitmap.compress(Bitmap.CompressFormat.PNG, 100, os);
        }
    }

    @Override
    public void onStart() {
        super.onStart();
        Log.i("SbInss", "onStart");

        Looper looper = Looper.getMainLooper();
        Handler handler = new Handler(looper);
        handler.post(new Runnable() {
                @Override
                public void run() {
                    Log.i("SbInss", "handler called");
                    Log.i("SbInss", "waiting 2 s");
                }
            });

        try {
            Thread.sleep(2000);
        } catch (InterruptedException e) {
            throw new RuntimeException(e);
        }
    }

    @Override
    public void onDestroy() {
        ScreenshotRunner.onDestroy();
        super.onDestroy();
    }

    public void applyTestRule(TestRule testRule, Runnable runnable) throws Throwable {
        Statement stmt = new Statement() {
                @Override
                public void evaluate() {
                    runnable.run();
                }
            };
        testRule.apply(stmt, Description.EMPTY).evaluate();
    }

}
