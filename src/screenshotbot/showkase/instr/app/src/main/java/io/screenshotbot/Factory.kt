package io.screenshotbot

import android.graphics.Bitmap
import androidx.compose.runtime.Composable
import androidx.compose.ui.test.junit4.ComposeTestRule
import androidx.compose.ui.test.junit4.createComposeRule
import androidx.compose.ui.test.junit4.createEmptyComposeRule
import androidx.compose.ui.test.junit4.AndroidComposeTestRule.*
import org.junit.runners.model.Statement
import androidx.activity.compose.setContent
import com.facebook.testing.screenshot.Screenshot
import org.junit.runner.Description
import android.app.Activity
import android.view.*
import android.graphics.*


fun createTestRule() : ComposeTestRule {
    return createComposeRule()
}

fun createEmptyTestRule() : ComposeTestRule {
    return createEmptyComposeRule()
}
