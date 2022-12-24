package io.screenshotbot.plugin;

import com.lispworks.LispCalls;
import org.gradle.process.*;

public class SbNative {

    public static String getExe() {
        return "/home/arnold/builds/web/build/asdf-cache/lw-8.0.1-linux-x64/src/screenshotbot/sdk/java";
    }

    public static void callFn(ExecOperations execOperations, Object... args) {
        var ret = execOperations.exec((execSpec) -> {
                execSpec.setExecutable(getExe());
                execSpec.args(args);
                execSpec.setStandardOutput(System.out);
            });
        ret.assertNormalExitValue();
    }
}
