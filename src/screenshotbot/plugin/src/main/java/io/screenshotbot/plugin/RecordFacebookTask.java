package io.screenshotbot.plugin;

import com.android.build.gradle.api.TestVariant;
import org.gradle.api.DefaultTask;
import org.gradle.api.tasks.Input;
import org.gradle.api.tasks.TaskAction;
import com.android.build.gradle.api.*;
import com.android.build.gradle.*;
import org.gradle.api.*;
import org.gradle.process.*;
import javax.inject.Inject;

public class RecordFacebookTask extends DefaultTask {

    private TestedExtension androidExtension;

    private ExecOperations execOperations;
    private TestVariant variant;
    private String testApplicationId;
    private ScreenshotbotPlugin.Extension extension;

    @Inject
    public RecordFacebookTask(ExecOperations execOperations) {
        this.execOperations = execOperations;
        setGroup("Screenshotbot Tasks");
    }

    public void init(TestVariant variant,
                     TestedExtension androidExtension,
                     ScreenshotbotPlugin.Extension extension) {
        //dependsOn(variant.getConnectedInstrumentTestProvider());
        this.androidExtension = androidExtension;
        this.variant = variant;
        this.extension = extension;

    }

    @TaskAction
    public void record() {
        /*
          Might be useful in the future, but it gives null for the moment:
          androidExtension.getAdbOptions().getInstallOptions()
         */

        String channel = extension.getChannel();
        if (channel == null) {
            channel = variant.getApplicationId();
        }

        SbNative.callFn(execOperations, "record-facebook-task",
                        androidExtension.getAdbExe().toString(),
                        variant.getApplicationId(),
                        channel);
        ScreenshotbotPlugin.println("hello world: " +
                                    androidExtension.getAdbExe().toString()
                                    );


    }
}
