package io.screenshotbot.plugin;

import org.gradle.api.*;

public class ScreenshotbotPlugin implements Plugin<Project>{
    @Override
    public void apply(Project project) {
        System.out.println("IN HERERERERER!");
        if (true) {
            throw new RuntimeException("shit");
        }
    }
}
