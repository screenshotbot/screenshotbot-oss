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
                                     generateFacebookTasks(project, androidExtension);
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

    private String capitalize(String input) {
        return input.substring(0, 1).toUpperCase() +
            input.substring(1);
    }

    public void generateFacebookTasks(Project project, TestedExtension androidExtension) {
        androidExtension.getTestVariants().all((variant) -> {
                // Create a screenshotbot task
                var name = "recordScreenshotbot" + capitalize(variant.getName());
                println("creating task: " + name);
                project.getTasks().register(name,
                                            RecordFacebookTask.class)
                    .configure((it) -> {
                            it.init(variant, screenshotbotExtension);
                        });
            });
    }
}
