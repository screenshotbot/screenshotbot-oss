
Version

* 17: Added isReleaseBranch to run model
* 16: Added metadata and run-metadata.
* 15: shard-spec was added to the API
* 14: When an API request fails, we set error code to 400 or 500
  (Previously, we would return 200 with a JSON body).
* 13: Added GET /api/run/:oid, and url field to screnshot
* 12: Added /api/analytics-event
* 11: Add batch model and POST /api/batch
* 10: Add author field to run
* 9: Introduced the installation URL in version
* 8: Add `tags` to recorder-run
* 7: POST /api/finalized-commit added
* 6: POST /api/unchanged-run added
* 5: PUT /api/image/blob now requires authentication
* 4: Added PUT for /api/run
* 3: added /api/failed-run
