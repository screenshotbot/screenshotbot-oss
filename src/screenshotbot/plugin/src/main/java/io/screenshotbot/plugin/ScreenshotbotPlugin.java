package io.screenshotbot.plugin;

import org.gradle.api.*;
import com.android.build.gradle.api.*;
import com.android.build.gradle.*;
import com.lispworks.LispCalls;

public class ScreenshotbotPlugin implements Plugin<Project>{

    public static class Extension {
    }

    private Extension screenshotbotExtension;

    static {
        loadLibrary();
    }

    static void loadLibrary() {
        System.out.println("Loading lispworks file loaded");
        System.load("/home/arnold/builds/web/build/asdf-cache/lw-8.0.1-linux-x64/src/screenshotbot/sdk/java-so.so");
        System.out.println("Lispworks file loaded");
    }

    public static void println(String str) {
        System.out.println(str);
    }

    @Override
    public void apply(Project project) {
        var extensions = project.getExtensions();

        screenshotbotExtension = extensions.create("screenshotbot",
                                                   Extension.class);

        var plugins = project.getPlugins();
        if (plugins.hasPlugin("com.facebook.testing.screenshot")) {
            extensions.configure(TestedExtension.class,
                                 (androidExtension) -> {
                                     generateFacebookTasks(androidExtension);
                                 });
        }
    }

    private TestedExtension getProjectExtension(Project project) {
        var extensions = project.getExtensions();
        var plugins = project.getPlugins();
        if (plugins.hasPlugin("com.android.application")) {
            return extensions.findByType(AppExtension.class);
        } else if (plugins.hasPlugin("com.android.library")) {
            return extensions.findByType(LibraryExtension.class);
        } else {
            throw new IllegalArgumentException("Screenshotbot plugin requires either an Android Application or Android Library plugin");
        }
    }

    public void generateFacebookTasks(TestedExtension androidExtension) {
        androidExtension.getTestVariants().all((variant) -> {
                println("variant: " + variant.toString());
            });
    }
}
