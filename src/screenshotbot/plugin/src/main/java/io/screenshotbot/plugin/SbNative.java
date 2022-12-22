package io.screenshotbot.plugin;

public class SbNative {

    static {
        loadLibrary();
    }

    static void loadLibrary() {
        ScreenshotbotPlugin.println("Loading lispworks file loaded");
        System.load("/home/arnold/builds/web/build/asdf-cache/lw-8.0.1-linux-x64/src/screenshotbot/sdk/java-so.so");
        ScreenshotbotPlugin.println("Lispworks file loaded");
    }
}
