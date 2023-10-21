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


public class SbInstrumentation extends Instrumentation {
    @Override
    public void onCreate(Bundle arguments) {
        Log.i("SbInss", "onCreate called");
        super.onCreate(arguments);

        start();
    }


    @Override
    public void onStart() {
        Log.i("SbInss", "onStart");
        LispworksManager.initLispworks(getTargetContext(), getContext(),
                                       new Runnable() {
                                           @Override
                                           public void run() {
                                               LispCalls.callObjectV("SCREENSHOTBOT/SHOWKASE/MAIN::MAIN");
                                               //finish(0, new Bundle());
                                           }
                                       }

                                       );
        Log.i("SbInss", "waiting 2 s");

        try {
            Thread.sleep(2);
        } catch (InterruptedException e) {
            throw new RuntimeException(e);
        }
    }
}
