package io.screenshotbot.plugin;

import com.android.build.gradle.api.TestVariant;
import org.gradle.api.DefaultTask;
import org.gradle.api.tasks.Input;
import org.gradle.api.tasks.TaskAction;

public class RecordFacebookTask extends DefaultTask {

    public RecordFacebookTask() {
        setGroup("Screenshotbot Tasks");
    }

    public void init(TestVariant variant, ScreenshotbotPlugin.Extension extension) {
    }

    @TaskAction
    public void record() {
        ScreenshotbotPlugin.println("hello world");
    }
}
