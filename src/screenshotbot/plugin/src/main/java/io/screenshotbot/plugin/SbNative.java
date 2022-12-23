package io.screenshotbot.plugin;

import com.lispworks.LispCalls;

public class SbNative {

    public static void loadLibrary() {
        ScreenshotbotPlugin.println("Loading lispworks file loaded");
        System.load("/home/arnold/builds/web/build/asdf-cache/lw-8.0.1-linux-x64/src/screenshotbot/sdk/java-so.so");
        ScreenshotbotPlugin.println("Lispworks file loaded");
    }

    public static void callFn(String name, Object... args) {
        loadLibrary();

        int ret = LispCalls.callIntA(name.toUpperCase(), args);
        assert(ret == 1);
    }
}
