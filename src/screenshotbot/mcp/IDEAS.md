
- "Subscribe #billing-team on all the billing channels"
- "This was an xcode update, see if any of the screenshot changes on abc0de are real regressions"
- "Which of the changes in this report are just antialiasing?" -- `compare_images`
  answers from the cached comparison a report already computed, so a model can
  triage a whole report's worth of changes in one pass without waiting on image
  processing. The diff image is transparent except for the changed pixels, which
  makes "a few characters moved" and "the whole layout shifted" tell apart at a
  glance, and `differenceValue` sorts the changes worth looking at to the top.

# Masks

- bulk edit masks
