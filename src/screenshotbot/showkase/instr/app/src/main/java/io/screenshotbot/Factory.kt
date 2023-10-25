package io.screenshotbot

import android.graphics.Bitmap
import androidx.compose.runtime.Composable
import androidx.compose.ui.test.junit4.ComposeTestRule
import androidx.compose.ui.test.junit4.createComposeRule
import androidx.compose.ui.test.junit4.createEmptyComposeRule
import androidx.compose.ui.test.junit4.AndroidComposeTestRule.*
import org.junit.runners.model.Statement
import sergio.sastre.uitesting.utils.activityscenario.ActivityScenarioForComposableRule
import sergio.sastre.uitesting.utils.activityscenario.ComposableConfigItem
import sergio.sastre.uitesting.utils.common.DisplaySize
import sergio.sastre.uitesting.utils.common.FontSize
import sergio.sastre.uitesting.utils.common.Orientation
import sergio.sastre.uitesting.utils.common.UiMode
import androidx.activity.compose.setContent
import com.facebook.testing.screenshot.Screenshot
import org.junit.runner.Description
import android.app.Activity
import androidx.test.core.app.ActivityScenario
import android.view.*
import android.graphics.*


fun createTestRule() : ComposeTestRule {
    return createComposeRule()
}

fun createEmptyTestRule() : ComposeTestRule {
    return createEmptyComposeRule()
}

fun ActivityScenarioForComposableRule.setContent(
    content: @Composable () -> Unit
): ActivityScenario<out Activity> =
    activityScenario.onActivity {
        it.setContent { content() }
    }

fun viewToBitmap(view: View): Bitmap? {
    val bmp = Bitmap.createBitmap(view.getWidth(), view.getHeight(),
                                  Bitmap.Config.ARGB_8888);
    val canvas = Canvas(bmp);
    view.draw(canvas);
    return bmp;
}

fun composableToBitmap(composable: @Composable () -> Unit): Bitmap? {
    val rule: ActivityScenarioForComposableRule = ActivityScenarioForComposableRule(
            config = ComposableConfigItem(
                    locale = "en",
                    uiMode = UiMode.DAY,
                    orientation = Orientation.PORTRAIT,
                    fontSize = FontSize.NORMAL,
                    displaySize = DisplaySize.NORMAL
            )
    )

    var bmp: Bitmap? = null

    val stmt = object : Statement() {
        override fun evaluate() {
            rule.setContent(composable);
            bmp = viewToBitmap(rule.composeView);
        }
    }

    rule.apply(stmt, Description.EMPTY);
    return bmp;
}
