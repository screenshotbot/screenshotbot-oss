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


fun createTestRule() : ComposeTestRule {
    return createComposeRule()
}

fun createEmptyTestRule() : ComposeTestRule {
    return createEmptyComposeRule()
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

    val stmt = object : Statement() {
        override fun evaluate() {
            rule.composeView.setContent(composable)
            Screenshot.snap(rule.composeView)
                    .record()
        }
    }

    rule.apply(stmt, Description.EMPTY);
    return null
}

