# Arc01 close packet — runtime contracts

2026-10-31. Assembler: CDC OpenAI GPT-6.1 Sol High; diagnosis authors: CC OpenAI GPT-6 Luna High in two separate slice contexts. Source `/Users/oubiwann/lab/erlsci/iso8601/.worktrees/project02-arc01-audit`, branch `codex/project02-arc01-audit`, exact pin `bcfd94a08fa7a427eb1469679897e4d92d808edf`, module SHA256 `b3dd4c61be8b47ec0cd91439f1d8deeee1ef5e5e38d075be3358a893f9ad3350`. Planning is orphan `planning`; final CC candidates76eaf7a and12ca2cd. This packet assembles accepted slice evidence. **Review-ready; independent arc composition and Operator gate remain open.** The assembler does not independently accept her own arc assembly.

## Capability and slice walk

The arc promised source-grounded runtime diagnosis of all 15 public functions and shared private helpers, reproducible public witnesses and concrete clean checks, with byte-identical source. Both planned slices delivered; no slice was dropped, replaced or added.

| Slice | Delivered result | Independent disposition |
|---|---|---|
| [Slice01 date contracts](slice01-date-contracts/closing-report.md) | Five date parsing/formatting/expanded APIs and shared date/week/ordinal/fraction/UTC paths;5 findings (4 Medium,1 Low) | Accepted at76eaf7a in [CDC verification](slice01-date-contracts/cdc-verification.md), all6 rows done; [retrospective](slice01-date-contracts/cdc-retrospective.md) |
| [Slice02 calendar/interval contracts](slice02-calendar-interval-contracts/closing-report.md) | Ten arithmetic/duration/interval APIs, shared offsets/clamping/sign/inheritance/bounds/rendering;4 findings (3 High,1 Medium) | Accepted at12ca2cd in [CDC verification](slice02-calendar-interval-contracts/cdc-verification.md), all7 rows done; [retrospective](slice02-calendar-interval-contracts/cdc-retrospective.md) |

## Runtime diagnosis and composition surface

The [runtime index](slice02-calendar-interval-contracts/artifacts/2026.10.31-audit-index.md) maps15/15 exports and all 74 explicit EPP function forms (70 names). Current semantic groups:38 date,14 arithmetic/duration,22 interval, residual0, no duplicate name/arity/line member. `gi/1` is duration-only; earlier39/13 grouping is explicitly superseded. Shared `apply_offset/4` and `datetime/1,2` boundaries are named. Population mapping plus full source inspection is not exhaustive input/clause proof or a claim that line coverage proves semantics.

| Stable ID | Severity | Diagnosis; details and exact controls in linked reports |
|---|---|---|
| ERL-DATE-001 | Medium | Invalid ISO-week inputs normalize into other dates |
| ERL-DATE-002 | Medium | Year-zero/negative-year zero-offset bypass accepts impossible dates |
| ERL-DATE-003 | Medium | Fraction rounding can format second60 without carrying the minute |
| ERL-DATE-004 | Low | Declared iodata shape exceeds nested-input parser behavior |
| ERL-DATE-005 | Medium | Exact fractional-hour/minute parsing rounds away subsecond units |
| ERL-CAL-001 | Medium | Below-year0 month arithmetic silently wraps or clamp recursion never terminates |
| ERL-CAL-002 | High | Ordinary parse_exact output has second0.0; duration composition crashes at calendar time conversion |
| ERL-CAL-003 | High | Formatter-produced+2007 interval start inherits end year200; signed full ends reject |
| ERL-CAL-004 | High | Zoned start/time-only end drops inherited offset, yielding a two-hour end-bound error |

Source-cited language partials: [date report](slice01-date-contracts/artifacts/2026.10.31-audit-results-erlang-date.md), [calendar report](slice02-calendar-interval-contracts/artifacts/2026.10.31-audit-results-erlang-calendar.md). Deduplicated total9 =3 High/5 Medium/1 Low. DATE005 parsing-rounding versus CAL002 already-float composition have distinct witnesses/failure modes despite shared offset machinery. CAL004 is grounded in README's component inheritance plus preserved resolved timezone intent; confidence medium-high is separate from consequence severity. No external ISO conformance certification is asserted. CAL001's exact below-floor error policy remains an API decision; expected bounded handling is not a prescribed error class.

### Evidence, replay and limits

CDC independently reproduced31 original date observations,10 fractional-unit comparisons (7 mismatches,3 controls,1e-12 tolerance),38 original calendar calls and64 refined calls, comparing complete outputs. Calendar hangs use monitored250ms workers, kill and confirmed DOWN plus the static installed-calendar lower-bound/unchanged-year recursion invariant; timeout alone is not proof. Fresh ordinary0.0 stack resolves calendar:time_to_seconds817 → datetime_to_gregorian_seconds259 → apply_offset499. Expected arithmetic/contract outcomes were assessed separately from exit0/stdout equality.

Replay artifacts, all retained with original failed/retried and reviewer-attributed logs:

- [date runner](slice01-date-contracts/artifacts/date-probes.escript), [stdout](slice01-date-contracts/artifacts/date-probes.out), [fraction runner](slice01-date-contracts/artifacts/fractional-unit-probes-iteration01.escript), [stdout](slice01-date-contracts/artifacts/fractional-unit-probes-iteration01.out), [date runlog](slice01-date-contracts/artifacts/runlog.md).
- [initial calendar module](slice02-calendar-interval-contracts/artifacts/calendar_interval_probes.erl), [stdout](slice02-calendar-interval-contracts/artifacts/calendar-interval-probes.out), [refined module](slice02-calendar-interval-contracts/artifacts/calendar_interval_probes_iteration01.erl), [stdout](slice02-calendar-interval-contracts/artifacts/calendar-interval-probes-iteration01.out), [calendar runlog](slice02-calendar-interval-contracts/artifacts/runlog.md).

Actual CDC calendar replay commands from repository root (isolated directory already created and owned):

```text
erlc -o /private/tmp/iso8601-p02-cdc .worktrees/project02-arc01-audit/src/iso8601.erl .worktrees/planning/project02-erlang-audit/arc01-runtime-contracts/slice02-calendar-interval-contracts/artifacts/calendar_interval_probes.erl
erlc -o /private/tmp/iso8601-p02-cdc .worktrees/planning/project02-erlang-audit/arc01-runtime-contracts/slice02-calendar-interval-contracts/artifacts/calendar_interval_probes_iteration01.erl
erl -noshell -pa /private/tmp/iso8601-p02-cdc -s calendar_interval_probes run -s init stop
erl -noshell -pa /private/tmp/iso8601-p02-cdc -s calendar_interval_probes_iteration01 run -s init stop
```

The date escripts use an explicit hardcoded source-worktree default ebin. CDC's actual earlier replays were `escript <date runner>` and `escript <fraction runner>` against the freshly rebuilt accepted native beam. Independent fresh-beam replay elsewhere must verify that pin/beam or make logged temporary copies changing only BeamDir; do not silently assume another -pa overrides the scripts. Parent independent composition uses its own fresh beam and15 exact-value API controls, with its final verdict to be appended below after stable assembly review.

Retained source-pinned Slice01 native CC and independent CDC results: compile/xref/Dialyzer/EUnit210/PropEr4×100/coverage100% pass locally onOTP28/rebar3.27.0. Fresh baseline used known cached source package archives after update errors, not stale May beams. Formatter/linter attempts exit1 after actual dev plugin update failures/commands unavailable. Full logs preserve them. No make-check/rebar-check aggregate, supportedOTP20–27 matrix or R15B03 pass is claimed; rebar/CI100% versus Make95% coverage gate discrepancy remains uncleared. Metadata-only refinements retain applicable proof rather than relabel unchanged runs as fresh. No source/test/config/dependency repair occurred.

## Arc ledger walk and accumulated changes

- A01-01 done/reproduced: Slice01 independent acceptance and six-row closure.
- A01-02 done/reproduced: Slice02 independent acceptance and seven-row closure.
- A01-03 **open:** parent independent arc composition, including export/helper/report/source reconciliation and representative cross-slice replay. Assembly inventory alone cannot close it.
- A01-04 done/reproduced at assembly scale: stable counts, native failures/limits, explicit carryover and both child/arc retrospectives reconciled here. Parent still checks this reconciliation as part of composition.
- A01-05 **open:** explicit Operator review, merge disposition and next-arc decision; stop at this endpoint.

[Arc-plan](arc-plan.md) version history: v1.0 two-slice runtime diagnosis; v1.1 Slice01 shared-helper IDs and public-spec carryover; v1.2 Slice02 completion, corrected semantic helper ownership and reduced-precision-zone carryover. No runtime scope or slice sequence expansion. Each slice needed two preserved refinements/max5. Current [ledger](ledger.md) and [arc retrospective](cdc-retrospective.md) retain the distinction between evidence, independent acceptance and methodology observations.

## Bubble-up and silent-drop check

The runtime capability has landed at slice scope and is assembled for independent composition. Every promised runtime export/helper logical unit is mapped; signed/zero durations, fractional anchors, floor/wrap/termination, full/signed/inherited/zone endpoints and both separators have observed controls or explicit dispositions. Intentionally unsupported basic-start/month-day-time inheritance and year<=0 nonzero offsets are not invented defects. No runtime delivery was silently deferred; two known contract obligations remain explicitly uncleared at their planned later scale.

Project02 [plan](../project-plan.md) and [ledger](../ledger.md) remain incomplete. Arc02 must audit full tests/property oracles, all 91-line coverage-escript contracts, app/package/build/tooling/public types/OTP support, historical claims and final combined language report/index/modernization synthesis. Specific watch items: parse_exact129 calendar-return spec versus float second (private40 permitsfloat) and reduced-precision zoned parser rejection299–387. BareP zero serialization follows preserved omission/always-P intent; no defect/standards conclusion was added. Historical12 archives/30OPEN rows remain unchanged from213bfc7, not retroactively accepted.

Direct Operator steering during this arc added Projectv1.2 roadmap Arc03 for OTP28/29 CI plus only necessary compatibility fixes. Existing20–27 stays supported unless a reproduced blocker/Operator decision says otherwise; current CI still20–27. Primary-doc candidate substrates and source-pin reconciliation requirements are in the project plan. This explicit later exception does not authorize unrelated runtime repairs or execution past the current checkpoint. Arc02/03 have no detailed directories or execution. No source merge is manufactured for this evidence-only arc; no push, PR, merge, release, publication, issue or external message occurred.

CDC assembly checks: `git diff --check`, source no-diff/status versusbcfd94a, unchanged Project01 versus213bfc7, both child ledger populations (6/6 and7/7 done/reproduced), all 45 current Project02 authored local links and all 12 existing Project02 commit coauthor blocks passed at this assembly cutoff. Arc02/03 directories are absent. These are structural/boundary receipts, not independent arc acceptance.

## Independent composition gate

Pending parent review of this stable assembled candidate. This section is reserved for an attributable independent verdict, exact command/evidence receipts, final pins and any remaining limits. Operator row remains open regardless of composition result.
