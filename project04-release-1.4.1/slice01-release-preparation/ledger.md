# Slice01 ledger

| ID | Criterion | Verify | Significance | Origin | Status | Evidence | Notes |
|---|---|---|---|---|---|---|---|
| S01-01 | Bounded full manifest/readback receives explicit CDC clearance before any source/generated edit. | Inspect pristine source/read extents/readback and actual clearance chronology. | correctness-grade | Operator release preparation | open | | No inherited acceptance. |
| S01-02 | Only the three approved metadata/config edits land; runtime/API/tests/README/build policy remain unchanged. | Actual diff, manifest consult1.4.1, byte hashes and adjacent committed trailers. | correctness-grade | Operator release preparation | open | | No inherited acceptance. |
| S01-03 | Unpublished1.4.1 notes accurately explain nine fixes, compatibility/error distinctions and OTP additions. | Source/P03 finding map and actual validation; no wider-audit or publication claim. | correctness-grade | Operator release preparation | open | | No inherited acceptance. |
| S01-04 | All8 final native gates pass at1.4.1 including100%coverage. | Sequential real compile/xref/Dialyzer/EUnit/PropEr/cover/fmt/lint receipts. | correctness-grade | Operator release preparation | open | | No inherited acceptance. |
| S01-05 | Actual local package AND documentation archives are correctly built and inspected. | Metadata/version/files/source/README equality, docs index/module/README1.4.1 and unprefixed source-ref URLs; archive hashes. | correctness-grade | Operator release preparation | open | | No inherited acceptance. |
| S01-06 | Fresh unpacked package compiles and works as a consumer with correct app version. | No stale build/code path; application load1.4.1 and literal positive/negative repaired-route smoke. | correctness-grade | Operator release preparation | open | | No inherited acceptance. |
| S01-07 | Actual final-tree OTP20–29 preparation-PR matrix passes. | Parent publication and independently inspected ten-job/step/population data;20staticonly retained. | correctness-grade | Operator release preparation | open | | No inherited acceptance. |
| S01-08 | CDC independently reproduces/reviews all rows and completes close/retrospective. | Independent source/package/consumer/native/hosted review and all8-row disposition. | correctness-grade | Operator release preparation | open | | No inherited acceptance. |
