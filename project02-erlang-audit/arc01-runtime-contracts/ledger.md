# Arc01 ledger

| ID | Criterion | Verify | Significance | Origin | Status | Evidence | Notes |
|---|---|---|---|---|---|---|---|
| A01-01 | Slice01 date-contract diagnosis independently accepted. | Slice01 ledger and CDC review | correctness-grade | Arc plan | done | reproduced — Slice01 `cdc-verification.md`, final76eaf7a review | Five findings; no source repairs; native limits retained. |
| A01-02 | Slice02 arithmetic/interval diagnosis independently accepted. | Slice02 ledger and CDC review | correctness-grade | Arc plan | open | | |
| A01-03 | Slices compose into runtime diagnosis covering all15 exports with no source delta. | Fresh export-to-scope/report walk, rerun representative cross-slice witnesses, git diff baseline | serious | Arc plan | open | | Parent independent composition gate. |
| A01-04 | Findings, native-gate limits, carryover scope and retrospectives are reconciled honestly. | Arc close/report links, failed and independent observations, child retro walk | correctness-grade | Framework | open | | Arc02 stays roadmap-only. |
| A01-05 | Operator reviews arc and dispositions merge/next-arc decision. | Attributable human decision | serious | Operator endpoint | open | | Stop pending human review. |
