# Slice02 runlog — calendar and interval contracts

## Assignment pins and read receipts — 2026-10-31

- Assignment: `cc-prompt.md`, read completely (41 lines) at planning `ae1511e4cf917c385b0a9b3602c4dc951b408a72`; the prompt's nine inspection/evidence sections and exact owned paths govern this run.
- Source: `/Users/oubiwann/lab/erlsci/iso8601/.worktrees/project02-arc01-audit`, `codex/project02-arc01-audit`, exact HEAD `bcfd94a08fa7a427eb1469679897e4d92d808edf`; `src/iso8601.erl` SHA-256 `b3dd4c61be8b47ec0cd91439f1d8deeee1ef5e5e38d075be3358a893f9ad3350` (744 lines). Initial source status clean. Installed Erlang/OTP 28, ERTS 16.1.1, rebar3 3.27.0; configured project matrix remains OTP20–27 and historical floor R15B03, neither established by this runtime. Existing source beam timestamp was 2026-10-31 15:51:36 CDT; it is prior Slice01-owned native output and not treated as a new build or evidence.
- Planning: orphan `planning`, exact HEAD above; initial status clean. The sole added file at intake is this Slice02 runlog under its authorized artifacts directory.

### Required full source / repository reading

Read complete text, with bounded outputs and recovered truncations where noted: repository `README.md` (239 lines), `AGENTS.md` (110); pinned source `src/iso8601.erl` (1–744), `src/iso8601.app.src` (1–21), `test/iso8601_tests.erl` (1–1069), `test/prop_iso8601.erl` (1–111), `rebar.config` (1–89), `Makefile` (1–96), `.github/workflows/ci.yml` (1–92), `elvis.config` (1–23), and `scripts/check_coverage.escript` (1–91). The test/executable readings are context and witnesses, not a complete Arc02 test-quality/escript audit. Planning `AGENTS.md` (1–16), Project02 plan (1–67), Arc01 plan (1–30), Slice02 plan (1–29), its ledger (1–13), this assignment, and the accepted Slice01 report (full 1–end, including all findings and scale/accounting) were read. Slice01 `cdc-verification.md` (1–46) and required runlog evidence were read in full sections below.

Required prior Slice01 runlog sections read: native validation and raw output/failures from lines 84–248; CDC review/reproduction sections for compile, xref, Dialyzer, EUnit210, PropEr4×100, coverage100%, formatter/linter, probe reruns, source-diff boundary, and fractional probes (lines 257–683); refinement01/02 preserved history (684–746). These are prior CC/CDC evidence only. Accepted Slice01 verdict and its retained independent receipts are not recast as fresh Slice02 execution.

Historical sources read fully: preserved `0003-time-intervals-design.md` (1–214) and `interval-hardening-prompt.md` (1–122), treating both solely as intent/provenance; `expanded-year-fixes-prompt.md` Background and Fix sections only. It documents intended `year <= 0` non-zero-offset restriction; it is not authority to execute an archived repair. No historical acceptance is inferred.

### Required skills and full Erlang guide receipts

Read the complete collaboration-framework `SKILL.md`; required sections of `knowledge/engineering-methods/guides/01-engineering-methodology.md` (Notes for Codex, Roles/shared invariants, Two-Contributor Workflow, Autonomous Mode, Codex interpretation), and `07-implementation-prompt-authoring.md` (Required reading and CC intake); project-management `guides/README.md` (Notes for Codex, Expedited and Autonomous Mode) and all required closing-report/bubble-up sections of `04-closing-slices.md`. Also read complete `code-auditing/SKILL.md`, its guides `01-audit-scope-and-map.md`, `02-findings-and-severity.md`, `03-scale-aware-auditing.md`, `05-audit-to-hardening-handoff.md`; complete `work-verification/SKILL.md`, guides `01-ledger-discipline.md`, `02-evidence-strength.md`, `03-row-closure.md`, and guide05 sections “Evidence During Correction Review” and higher-scale verification. Truncated outputs encountered while reading were recovered by focused rereads before relying on those texts.

Read complete `erlang-guidelines/SKILL.md` and all 17 direct guide chapters, with `11-anti-patterns.md` first, then 01–10 and 12–17, each from first through final line. Full extents at this skill revision: SKILL 1–284; chapters 01 1–489, 02 1–355, 03 1–440, 04 1–479, 05 1–443, 06 1–446, 07 1–467, 08 1–452, 09 1–439, 10 1–502, 11 1–571, 12 1–354, 13 1–355, 14 1–388, 15 1–355, 16 1–368, 17 1–317. Long outputs for 04, 07, 08, 10, 11, 14 and 16 were split/recovered in bounded chunks; recovered spans include 04 ~180–330, 07 ~140–300, 08 SUP-06–18, 10 ~200–340, 11 ~260–350, 14 ~160–270, 16 ~140–270. Repository OTP/EDoc/`undefined_or`/private-record/exported-calendar-type constraints override guide-27 defaults; the library itself needs no supervisor.

### Source-cited contract readback before fresh probes

`src/iso8601.erl:60–94`: add/subtract time dispatch through offset arithmetic; day/month/year helpers accept datetime or timestamp; month/year paths clamp to a last valid date, while day arithmetic uses Gregorian-day conversion. Negative arguments are meaningful; no one-step inverse/clamp equivalence is promised. `149–195`: durations parse into ordered sign/year/month/day/hour/minute/second components, and application delegates to the shared ordered composer. `197–232`: interval parser accepts duration-only and classified start/end, start/duration, duration/end; bounds reject duration-only and format emits canonical slash form. `491–542`: offsets round once at seconds conversion; non-positive years retain the zero-rounded-offset special case and reject nonzero offsets; month/year clamp recursion decrements day until installed calendar validation succeeds. `561–744`: components apply years→months→days→time; interval separators include slash and legacy double hyphen; endpoints can inherit date fields/time zone by parsing reconstructed strings; month-day/time-only and zero-year behaviors must be distinguished from unverified ISO conformance. No failure class is prescribed where EDoc/API contract is silent.

Current interval probes must separate reduced-precision zoned-start parser behavior from inheritance itself. CDC reported before handoff: `2007-12-14T13:30+02:00/...` raises `badarg`, while full-seconds `2007-12-14T13:30:00+02:00/15:30:00` resolves to UTC 11:30/15:30 and explicit end `15:30:00+02:00` resolves to UTC 11:30/13:30. This is a distinct prior observation, not a Slice02 reproduction; it suggests a precision-specific control is needed before claiming zone inheritance behavior.

Accepted Slice01 contributes five stable findings (4 Medium, 1 Low), not new Slice02 counts. Cross-reference DATE002 (expanded-year invalid calendar validation) and DATE005 (fractional-hour/minute conversion rounding) if shared helpers appear; DATE005 does not subsume the separate fractional-anchor `function_clause` or duration/month helper behavior absent separate evidence. Arc02 retains the public `parse_exact/1` return spec mismatch (`src/iso8601.erl:129`, documented/exercised fractional second vs `calendar:datetime()`); no declaration that all public types are reconciled. Accepted source/history evidence excludes intended year<=0 nonzero-offset rejection. Project map is 16 tracked entries and includes `scripts/check_coverage.escript`, deferred to Arc02; this report will not characterize it as absent.

### Evidence bookkeeping

Data queries (rebar/runtime versions, status, source SHA and prior beam mtime) were kept separate from text reading. No fresh behavioral probe, native validation, generated index, or semantic conclusion predates this intake record. Subsequent sections distinguish CC fresh observations, earlier CDC input, and Slice01 retained evidence.

## Fresh probe and EPP execution

### Initial harness errors retained

The first probe file was authored in escript form, but its initial invocation passed the beam path as script arguments. That invocation exited normally but every intended call returned `error:undef`, so it is **not semantic evidence**. `escript -pa /private/tmp/iso8601-slice02-beam <script>` then failed immediately with `escript: illegal operation on a directory`; using `--emu_args '-pa ...'` still left the probe module off the path and reproduced only `undef`. A BSD/macOS `sed` conversion attempt failed with `extra characters after \\ at the end of c command`. These harness/setup failures were corrected by converting the runner to the retained ordinary Erlang module `artifacts/calendar_interval_probes.erl`; the invalid escript copy was removed. The runner was compiled into the isolated temporary beam path and all observations below come from the successful module execution, never the `undef` attempts.

### Fresh build and public calls

Pinned source compiled freshly with `erlc -o /private/tmp/iso8601-slice02-beam .worktrees/project02-arc01-audit/src/iso8601.erl` (OTP28; exit0). Probe module compiled with `erlc -o /private/tmp/iso8601-slice02-beam artifacts/calendar_interval_probes.erl`; runner executed with `erl -noshell -pa /private/tmp/iso8601-slice02-beam -s calendar_interval_probes run -s init stop`; transcript is complete in `calendar-interval-probes.out`. The runner contains 38 labeled public/installed-calendar cases, including four underflow calls each isolated in its own monitored child with a 250 ms outer timeout, `kill`, and receive-`DOWN` confirmation. All four reported `{timeout,250,killed,down_confirmed}`. Year zero control completed; installed calendar control returned `{true,false,false}` for `{0,1,1}`, `{-1,1,1}`, `{-1,1,0}`. The full transcript also preserves all exact values/exceptions for the four findings, candidate dispositions, interval forms/separators, and clean controls.

Key independent comparisons from that transcript:

- Date/time arithmetic controls: year-boundary +1 second and subtraction both return `{{2018,1,1},{0,0,0}}`; leap-day add-day returns `2016-02-29`; month-end and leap-year clamping return `2017-02-28`.
- Duration controls: negative `-P1Y` yields 2016 from 2017; `P1M1D` from 2020-01-31 returns 2020-03-01; signed duration/end bounds return 2009-05-11/2008-05-11 for `-P1Y/2008-05-11` as the component/sign code specifies.
- Interval controls: four tags parse, slash and `--` both parse, abbreviated day and UTC time cases resolve; duration-only bounds is `badarg`; formatting returns canonical binaries. Malformed two-duration and three-part forms are `badarg`.
- Discriminators: zoned full-second inherited time returns UTC 11:30/15:30 while explicit `+02:00` end returns 11:30/13:30; minute-precision zoned start raises `badarg`. Valid standalone `+0002007` parses as year2007; interval abbreviated end returns year0 and explicit expanded end raises `badarg`, while unsigned 2007 controls pass. Same fractional anchor passes through `add_days` and causes `function_clause` in `apply_duration`/`interval_bounds`.

### EPP production population receipt

Fresh command: `erl -noshell -eval '{ok, Fs} = epp:parse_file("/Users/oubiwann/lab/erlsci/iso8601/.worktrees/project02-arc01-audit/src/iso8601.erl", [], []), Forms = lists:sort([{N,A,L} || {function,L,N,A,_} <- Fs]), Exports = lists:append([Es || {attribute,_,export,Es} <- Fs]), io:format("forms=~B distinct_names=~B exports=~B~nexports=~p~nforms=~p~n", [length(Forms), length(lists:usort([N || {N,_,_} <- Forms])), length(Exports), lists:sort(Exports), Forms]), halt().'` (exit0). Result: 74 explicit function forms, 70 names, 15 exports. The exact sorted export list and source-line function-form population are in the runtime index; all 74 disjointly group as date parse/construct/format39, arithmetic/duration13, interval22; residual0. This independently matches CDC’s preflight count without relying on it. EPP forms, not regular-expression matches or clause counts, are the denominator.

Installed implementation inspected locally, not web: `/opt/homebrew/Cellar/erlang/28.1.1/lib/erlang/lib/stdlib-7.1/src/calendar.erl:862–883`. `valid_date/3` delegates to `valid_date1/3`; its only valid-date clause requires `Y >= 0`, valid month and `D > 0`, then bounds against `last_day_of_the_month`. This is the static invariant for the underflow loop: negative adjusted year remains fixed; only `D` becomes `1,0,-1,...`; `calendar:valid_date` stays false, so recursion has no terminating state.

### Prior native gate evidence retained, not rerun

Revalidated same source HEAD/hash, OTP28/rebar3 versions and unchanged tracked build/test configuration. Read Slice01 native logs and CDC independent rerun records before retention; the installed OTP28 PLT/dependencies/beam were known from those records. Slice01 accepted/CDC-reproduced outputs remain compile/xref/Dialyzer/EUnit210/PropEr4×100/coverage100% pass, with formatter/linter plugin update failures and exit1. These observations are earlier, independently authored runs; not fresh Slice02 validation. No native dependency update or full-gate loop was invoked for the unchanged source. OTP20–27, R15B03, aggregate aliases, all-test-quality and full escript audit remain unverified/deferred.

## Self-review and final pre-commit boundary

- Rechecked every report and runtime-index relative link: Project02 plan, Slice01 report, Slice02 report/log/runner/transcript all resolve. EPP function groups are disjoint: 39 + 13 + 22 = 74; reported overloads reconcile 74 forms vs70 names; export crosswalk has15 rows; residual/unmapped helpers0. Stable count reconciles Slice01 5 + Slice02 4 =9 (8 Medium,1 Low).
- `git -C .worktrees/project02-arc01-audit diff --exit-code bcfd94a --` exited0; source checkout status porcelain was empty; source SHA-256 still `b3dd4c61be8b47ec0cd91439f1d8deeee1ef5e5e38d075be3358a893f9ad3350`. No tracked source, test, config, build, or package file changed.
- `git -C .worktrees/planning diff --check -- project02-erlang-audit/arc01-runtime-contracts/slice02-calendar-interval-contracts` exited0. Planning mutations are only Slice02 `artifacts/**`, `closing-report.md`, and evidence/status cells in `ledger.md`. No plan/prompt/Slice01/reviewer/history file is touched.
- CC's close walks all seven rows: S02-01 through S02-06 are `attested`, not CDC-reproduced/accepted; S02-07 remains open for CDC verification/retrospective and parent/Operator gates. No Arc02 work was started.

## CDC independent initial-candidate review — 2026-10-31

Reviewer Sol High, candidatecb26ad8, sourcebcfd94a. Fresh `erlc -o /private/tmp/iso8601-p02-cdc .worktrees/project02-arc01-audit/src/iso8601.erl <Slice02>/artifacts/calendar_interval_probes.erl` exit0; `erl -noshell -pa /private/tmp/iso8601-p02-cdc -s calendar_interval_probes run -s init stop` exit0. `cmp` against original transcript exit0: all38 labeled cases byte-identical, including four bounded workers. Original exact stdout remains `calendar-interval-probes.out` atcb26ad8; this rerun is CDC-owned, not CC.

Fresh EPP extraction (same documented selection) versus all index name/arity@line tokens:74 unique=74 actual, missing[], extra[];15 exports independently counted. Initial reviewer token regex omitted digits in names (73); corrected regex includes digits and recovers `date_at_w01_1/1`, yielding74. No semantic source discrepancy. Source diff baseline/status and planning diff-check exit0. Retained Slice01 native inputs/evidence unchanged; no rerun.

CDC extra witness source and commands (all exit0) below: `erlc -o /private/tmp/iso8601-p02-cdc /private/tmp/iso8601-p02-cdc/review_extra.erl`; `erl -noshell -pa /private/tmp/iso8601-p02-cdc -s review_extra run -s init stop`. These supplement review and are not CC evidence.

```erlang
-module(review_extra).
-export([run/0]).
run() ->
    try iso8601:apply_duration({{2017,1,1},{0,0,0.5}},"P1D") catch C:R:S -> io:format("fraction stack ~p:~p ~p~n",[C,R,S]) end,
    D=iso8601:parse_exact("2017-01-01"),
    io:format("parse_exact ordinary=~p~n",[D]),
    try iso8601:apply_duration(D,"P1D") of X -> io:format("composition ~p~n",[X]) catch C1:R1 -> io:format("composition ~p:~p~n",[C1,R1]) end,
    lists:foreach(fun({A,M})->io:format("months ~p ~p => ~p~n",[A,M,iso8601:add_months(A,M)]) end,[{{{0,1,1},{0,0,0}},-1},{{{1,1,1},{0,0,0}},-13},{{{1,1,1},{0,0,0}},-12}]),
    io:format("formatted=~p parsed=~p~n",[iso8601:format_expanded({{2007,2,15},{0,0,0}}),iso8601:parse("+2007-02-15")]).
```

```text
fraction stack error:function_clause [{calendar,time_to_seconds,
                                       [{0,0,0.5}],
                                       [{file,"calendar.erl"},{line,817}]},
                                      {calendar,
                                       datetime_to_gregorian_seconds,1,
                                       [{file,"calendar.erl"},{line,259}]},
                                      {iso8601,apply_offset,4,
                                       [{file,
                                         ".worktrees/project02-arc01-audit/src/iso8601.erl"},
                                        {line,499}]},
                                      {review_extra,run,0,
                                       [{file,
                                         "/private/tmp/iso8601-p02-cdc/review_extra.erl"},
                                        {line,4}]},
                                      {init,start_it,1,[]},
                                      {init,start_em,1,[]},
                                      {init,do_boot,3,[]}]
parse_exact ordinary={{2017,1,1},{0,0,0.0}}
composition error:function_clause
months {{0,1,1},{0,0,0}} -1 => {{0,12,1},{0,0,0}}
months {{1,1,1},{0,0,0}} -13 => {{0,12,1},{0,0,0}}
months {{1,1,1},{0,0,0}} -12 => {{0,1,1},{0,0,0}}
formatted=<<"+2007-02-15T00:00:00Z">> parsed={{2007,2,15},{0,0,0}}
```

Installed OTP source independently inspected: calendar.erl249–261,810–828,862–885; seconds conversion leaf requires integer H/M/S. Public application spec is src192–196, not162–164. Source apply_months509–520 truncates at AY=0; floor-wrap witnesses need explicit accounting beside nontermination. See reviewer-owned `cdc-verification.md` for pending R1–R3; no acceptance yet.

## Refinement01 intake and source-cited readback — 2026-10-31

Current assignment is `cc-prompt-iteration01.md`, read fully (20 lines), issued at planning `7683fb41a72b6ec30f0d9d2afe61f084a045fade`; CDC `cdc-verification.md` (21 lines), including its independent initial review and R1–R4, was read fully. Planning was clean at that head; source remains clean at `bcfd94a08fa7a427eb1469679897e4d92d808edf`, with unchanged module SHA-256 `b3dd4c61be8b47ec0cd91439f1d8deeee1ef5e5e38d075be3358a893f9ad3350`. Refinement01 is the first refinement under the five-pass maximum. CDC’s 38-case byte-identical rerun, EPP identity review and extra stack/month witnesses are reviewer-owned retained inputs, not this CC turn’s fresh evidence.

Before new probes, reread current report (all101 lines), current index (all55), runner (all72), transcript (all129), ledger (all13), closing report (all39), current prompt/review, guide `code-auditing/guides/02-findings-and-severity.md` (all124 lines), assigned source paths `509–542`, `192–232`, `494–505`, `549–552`, and source `721–744`. Reread retained EUnit timestamp-dispatch controls `test/iso8601_tests.erl:334–341`, plus the archived interval implementation prompt `time-intervals-impl-prompt.md:118–132` and interval hardening prompt `:100–122` to distinguish accepted design intent from old scope. Required guide severity states High means correctness risk under realistic operating conditions; uncertainty is not a severity rationale. The archive explicitly says formatter omits zero components but always emits `P` and emits `T` only when any time component is nonzero; current `format_duration/1` does exactly this. This is implementation-history intent, not a standards audit.

Refinement readback: R1 tests month-floor wrapping in addition to the already-proven negative-year nontermination. `apply_months_offset/2` computes `AY`, then `(AY-1) div 12` and `AY rem 12`; Erlang truncating division maps AY=0 to year0/month12, which is calendar-valid but not the mathematical target. `-12` from year1 January is the within-floor control. A simple Year<0 guard would miss the AY=0 case. R2 must test ordinary `parse_exact("2017-01-01")` output (`0.0` seconds), match `apply_duration/2` against a zero-integer anchor, and capture the exception stack; source application is `192–196`, with `apply_offset/4` at499, not the incorrect `162–164` cited in the first draft. R3 requires the public formatter itself to produce `+2007`; the prior “malformed four-digit plus candidate” label is unsupported because `format_expanded/1` intentionally emits that spelling for positive year2007 and `parse/1` accepts it. R4 adds zero `P0D`/`PT0S` parse, application, bounds and serialization; bare `P` output follows historical zero-component omission/always-P intent and is not automatically a finding.

Source/error readback: `apply_months_offset/2` performs truncating `div` before month correction (`509–520`); `find_last_valid_date/1` then decrements day without changing year/month (`537–542`). Existing year0 boundary is valid under installed OTP calendar; dates below year0 are not, so the two floor behaviors require separate exact witnesses in the same CAL001 root finding. The fractional-anchor path reaches `calendar:datetime_to_gregorian_seconds/1` via `apply_offset/4`; CDC’s stack is a prior observation and will not be called fresh. Ordinary exact parsing converts zero fractional remainder to float0.0, so the user-facing gap is routine rather than limited to explicit fractional literals. Assessment below will choose a severity based on realistic impact and the guide definition, not match CDC’s label by authority. CAL003’s expanded-year issue must be retested from the formatter-produced `+2007` and compare unsigned controls. CAL004 shifts a bounded but ordinary interval by its offset; consequence and confidence are distinct questions and will be explicitly assessed.

No new runtime probe or validation has run in this refinement before this readback. Next evidence will be a new fresh run of the existing probe module after adding R1–R4 cases; original runner/transcript remain recoverable at commit `cb26ad8` and CDC initial-review output remains labelled as reviewer evidence.

## Refinement01 fresh run and disposition — 2026-10-31

Current assignment `cc-prompt-iteration01.md` was executed from planning base `7683fb41a72b6ec30f0d9d2afe61f084a045fade`; source remains `bcfd94a08fa7a427eb1469679897e4d92d808edf`, branch `codex/project02-arc01-audit`, module SHA-256 `b3dd4c61be8b47ec0cd91439f1d8deeee1ef5e5e38d075be3358a893f9ad3350`. Planning was dirty only for the earlier authorized intake/readback before probes; source status was clean and source diff from the pin empty.

Fresh commands, OTP28, isolated temporary beam:

```text
erlc -o /private/tmp/iso8601-slice02-refinement01-beam .worktrees/project02-arc01-audit/src/iso8601.erl
erlc -o /private/tmp/iso8601-slice02-refinement01-beam .worktrees/planning/project02-erlang-audit/arc01-runtime-contracts/slice02-calendar-interval-contracts/artifacts/calendar_interval_probes_iteration01.erl
erl -noshell -pa /private/tmp/iso8601-slice02-refinement01-beam -s calendar_interval_probes_iteration01 run -s init stop
```

Both compilations and the runner exited0. The complete stdout was captured without trimming to `artifacts/calendar-interval-probes-iteration01.out` (282 lines; 64 labeled calls). During harness refinement, one exploratory interval-bounds stack call initially supplied a tuple that did not match the public interval record and failed at `iso8601:interval_bounds/1` line214; that was an invalid-input harness mistake, not semantic evidence. The runner was corrected to use public `parse_interval("2017-01-01T00:00:00.0Z/P1D")`, rerun from fresh beams, and now captures the intended calendar leaf stack in the retained transcript. The original `calendar-interval-probes.out` remains the initial 38-case artifact at `cb26ad8`; CDC’s byte-identical rerun remains reviewer-owned and is not recast as this execution.

New/changed witnesses in the refinement transcript:

- **R1:** year0 January minus1 month and year1 January minus13 months both return `{{0,12,1},{0,0,0}}`; year1 January minus12 returns valid floor `{{0,1,1},{0,0,0}}`. Existing four recursion cases remain in monitored workers, each `{timeout,250,killed,down_confirmed}`. Source `apply_months_offset/2` at509–520 computes `AY`, `(AY-1) div 12`, and `AY rem 12`; with Erlang truncation AY=0 maps to year0/month12. Installed calendar year0/year−1 validity control is `{true,false,false}`. A Year<0 guard alone would not address the silent wrap. The below-floor error policy remains unspecified.
- **R2:** `parse_exact("2017-01-01")` returns `{{2017,1,1},{0,0,0.0}}`; ordinary `apply_duration` fails with `function_clause`, and matched integer timestamp control returns `{{2017,1,2},{0,0,0}}`. Captured stack in full output is `calendar:time_to_seconds/1` line817 → `calendar:datetime_to_gregorian_seconds/1` line259 → `iso8601:apply_offset/4` line499. An interval using `2017-01-01T00:00:00.0Z/P1D` fails at the same calendar leaf through `interval_bounds/1` line217. The prior invalid tuple-shaped hand-probe was a harness/input mistake; it is not retained as semantic evidence and the corrected public interval input is the only relevant stack witness. Current public application declaration is source192–196; prior report’s162–164 location was corrected.
- **R3:** `format_expanded({{2007,2,15},{0,0,0}})` freshly returns `<<"+2007-02-15T00:00:00Z">>`; `parse/1` accepts that value. Reusing the formatter output as interval start with `/03-14` returns end year200; explicit `+2007-03-14` end raises `badarg`. The valid `+0002007` discriminator still returns year0 for abbreviated end / `badarg` for explicit end; unsigned2007 control yields year2007. The fresh runner no longer labels `+2007` malformed.
- **R4:** `P0D`, `PT0S`, and bare `P` parse to zero fields, apply without changing the integer timestamp, and resolve date-anchored bounds to identical endpoints. All three interval forms serialize as `2017-01-01T00:00:00Z/P`; duration-only bare `P` serializes as `P`. This matches archived omission/always-P intent (`time-intervals-impl-prompt.md:118–132`) and current `format_duration/1` source721–744, so no finding is added. Existing EUnit timestamp-dispatch cases `test/iso8601_tests.erl:334–341` remain cited retained evidence and were not rerun.

Severity disposition under `code-auditing/guides/02-findings-and-severity.md`: CAL002 and CAL003 are High because normal parsed or formatter-produced values incur a direct crash or wrong endpoint year. CAL004 is High because ordinary full-seconds zoned-start/time-only-end input returns bounds two hours later than README’s omitted-component inheritance promise and the resolved timezone-inheritance intent; confidence remains medium-high because current README does not name timezone independently and archived intent is not an ISO authority. CAL001 remains Medium: it includes silent wrong-date behavior as well as a hang, but the trigger is precisely crossing the representable year-zero floor and the public out-of-range policy is unspecified. No finding quota is applied. Current Slice02 severity is 3 High / 1 Medium; adding retained Slice01’s4 Medium/1 Low yields Arc01’s9 IDs as3 High/5 Medium/1 Low. Stable IDs and DATE002/DATE005 deduplication remain unchanged.

Current close does not supersede independent obligations: S02-01–06 remain CC-attested/proposed pending CDC verification; S02-07 remains open for reviewer-owned close/retrospective and parent/Operator gate. No source/test/config edit, native gate rerun, supported-OTP claim, Arc02 work, or external action occurred.
