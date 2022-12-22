package io.screenshotbot.plugin;

import org.gradle.api.*;
import com.lispworks.LispCalls;

public class ScreenshotbotPlugin implements Plugin<Project>{

    static {
        loadLibrary();
    }

    static void loadLibrary() {
        System.out.println("Loading lispworks file loaded");
        System.load("/home/arnold/builds/web/build/asdf-cache/lw-8.0.1-linux-x64/src/screenshotbot/sdk/java-so.so");
        System.out.println("Lispworks file loaded");
    }

    @Override
    public void apply(Project project) {
        System.out.println("just before running stuff");
        System.out.println("got length: " + LispCalls.callIntV("CL-USER::SIMPLE-TEST", "foobar"));
    }
}
