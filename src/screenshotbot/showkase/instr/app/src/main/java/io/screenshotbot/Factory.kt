package io.screenshotbot

import androidx.compose.ui.test.junit4.ComposeTestRule
import androidx.compose.ui.test.junit4.createComposeRule
import androidx.compose.ui.test.junit4.createEmptyComposeRule
import androidx.compose.ui.test.junit4.AndroidComposeTestRule.*

fun createTestRule() : ComposeTestRule {
    return createComposeRule()
}

fun createEmptyTestRule() : ComposeTestRule {
    return createEmptyComposeRule()
}
