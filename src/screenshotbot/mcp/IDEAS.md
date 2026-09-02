
- "Subscribe #billing-team on all the billing channels"
- "This was an xcode update, see if any of the screenshot changes on abc0de are real regressions"
- "Which of the changes in this report are just antialiasing?" -- `compare_images`
  answers from the cached comparison a report already computed, so a model can
  triage a whole report's worth of changes in one pass without waiting on image
  processing. The diff image is transparent except for the changed pixels, which
  makes "a few characters moved" and "the whole layout shifted" tell apart at a
  glance, and `differenceValue` sorts the changes worth looking at to the top.
- "Accept the screenshot changes on PR #123" -- `accept_report` signs a report
  off and updates the commit status Screenshotbot posted on the pull request,
  which is often what the merge is waiting on. This server has no view of pull
  requests, so pair it with a GitHub MCP server (or the `gh` CLI) to get from a
  PR to its report ids: Screenshotbot posts one check per channel and its
  details link is `/report/<report_id>`.
- "What's still unreviewed on PR #123?" -- an account using batches gets one
  check per commit rather than one per channel, and its details link is
  `/batch/<batch_id>`. `list_batch_reports` opens that up into a channel per
  line, with the report id for each one that changed, so the PR -> report ->
  accept path works for batched setups too. `status` is where the answer is:
  `action-required` means a person still has to look.

# Masks

- bulk edit masks
