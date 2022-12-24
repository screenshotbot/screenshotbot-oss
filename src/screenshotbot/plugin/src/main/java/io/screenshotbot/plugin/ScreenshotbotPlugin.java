package io.screenshotbot.plugin;

import org.gradle.api.*;
import com.android.build.gradle.api.*;
import com.android.build.gradle.*;
import com.lispworks.LispCalls;

public class ScreenshotbotPlugin implements Plugin<Project>{

    public static class Extension {
        private String channel;
        public void setChannel(String channel) {
            this.channel = channel;
        }

        public String getChannel() {
            return channel;
        }

        private String mApiKey;

        public void setApiKey(String apiKey) {
            mApiKey = apiKey;
        }

        public String getApiKey() {
            return mApiKey;
        }

        private String mApiSecret;

        public void setApiSecret(String apiSecret) {
            mApiSecret = apiSecret;
        }

        public String getApiSecret() {
            return mApiSecret;
        }

    }

    private Extension screenshotbotExtension;


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
                            it.init(variant,
                                    androidExtension,
                                    screenshotbotExtension);
                        });
            });
    }
}
