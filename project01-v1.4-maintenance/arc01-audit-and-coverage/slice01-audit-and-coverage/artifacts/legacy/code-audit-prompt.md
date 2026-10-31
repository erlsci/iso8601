# CC brief — full code-quality audit of iso8601 (Erlang)

Run a full code-quality audit of the iso8601 library. **Diagnosis only — do
NOT edit, stage, or commit any source. A later round applies fixes.** The
deliverable is written reports under `workbench/`, nothing else.

## Authoritative method — load these first, in order

1. **`priv/ai/CODE-AUDIT.md`** — the audit contract. Follow its Preparation,
   Scope, Output, Stance, and "What to hunt for" sections. Everything below
   *adapts* it to this repo; where they agree, CODE-AUDIT.md wins on format.
2. **`priv/ai/erlang/SKILL.md`** and its loading instructions, then **every
   file under `priv/ai/erlang/guides/`**. Load **`11-anti-patterns.md` first** —
   it is the canonical hunt list. Work through its items and grep the codebase
   for each; do not fall back to generic Erlang knowledge where a guide exists.
3. **`CLAUDE.md`** (repo root) — the project conventions. Several things that
   might *look* like findings are deliberate here; read it so you don't file
   intentional decisions as bugs (see "Honor these intentional decisions" below).

Note: CODE-AUDIT.md's language auto-detection table doesn't list Erlang. For
this repo the audited language is **Erlang**, slug **`erlang`**, with
`priv/ai/erlang/` as the skill/guide set. There are no other languages to audit.

## Scope

Every `.erl` file in `src/`, `test/`, and `examples/` (if present). Currently:
`src/iso8601.erl` (the whole library — one module), `test/iso8601_tests.erl`
(eunit), `test/prop_iso8601.erl` (PropEr). Skip `_build/` and any generated
trees. No source files excluded from review. Treat it as code you're handing to
a senior Erlang reviewer who ships it to Hex users next week.

## The three axes Duncan asked for

**1. Proper Erlang idioms.** Module layout, naming, return conventions
(`{ok,_}`/`{error,_}` vs in-band sentinels), clause/guard design, records vs
maps vs proplists, binary/iolist handling, spec coverage on exported functions,
edoc usage. Judge against the guides, not taste.

**2. Anti-patterns.** Drive from `11-anti-patterns.md`. Pay particular attention,
given this is a parser, to: defensive `try/catch` that swallows or rewrites
errors; catch-all clauses (`_ -> ...`) that mask exhaustiveness; building atoms
from input (`list_to_atom` on untrusted data); accepting ranges in assertions
where exact values are computable (in the tests); and silently-dropped errors.

**3. OTP 20+ support — first-class, spell it out.** The library's CI floor is
OTP 20 (CLAUDE.md claims a build aspiration back to R15B03). Independently verify
nothing relies on features newer than the floor. Enumerate every BIF, stdlib
call, and syntax form used, and check each against its introduction release.
Known traps to check explicitly:
   - **`try ... catch Class:Reason:Stacktrace`** (3-arg stacktrace form) — OTP 21+.
     Using it breaks OTP 20. Grep for it.
   - **`maybe ... end`** expression — OTP 25/27. (The `undefined_or` type name
     already exists *because* of this clash — confirm the keyword itself is unused.)
   - **`-doc` / `-moduledoc`** (EEP-48) — OTP 27; CLAUDE.md forbids them, use edoc.
   - **`maps:iterator/1`, `maps:next/1`** — OTP 21. **New `string` module**
     (`string:trim/lexemes/...`, Unicode-aware) — OTP 20 but with behavioral
     differences from old `string`. **`lists:enumerate`** — OTP 25.
     **`floor/1`,`ceil/1`** as guard BIFs — OTP 20 (ok at floor, not below).
   - Anything in `calendar`, `erlang`, `binary`, `re` with a post-20 introduction.

   For each item that exceeds the floor, cite file:line and name the minimum OTP
   it requires. Note that CI compiles + xrefs on OTP 20, so *missing* functions
   would surface there — your added value is catching post-20 **behavioral**
   reliance and confirming the R15B03 aspiration is (or isn't) actually met, since
   CI doesn't test below 20.

## Honor these intentional decisions (not findings)

- The `calendar`-style types (`year/0`, `month/0`, …, `timestamp/0`) are
  **exported on purpose** as a caller convenience — do not flag them as leaked
  internals or recommend opaque types.
- `undefined_or` is named that way deliberately (the OTP-27 `maybe` clash).
- Let-it-crash on malformed input (e.g. `error(badarg)`, function_clause) is the
  intended parse-failure contract, not a robustness gap — judge defensiveness
  against that, but DO flag any place that swallows a *real* error.
- The public API is stable and widely depended on; if a finding implies an API or
  exported-type change, mark it clearly and put the backward-compat cost in the Fix.

## Output (per CODE-AUDIT.md)

Run `date +%Y.%m.%d` and use it as `<DATE>` (don't hallucinate it). Write:

- **`workbench/<DATE>-audit-results-erlang.md`** — per-language report:
  1. Executive summary (3–5 sentences; note if no architecture doc exists).
  2. Findings grouped: correctness/soundness; API design & invariants; error
     handling; concurrency/runtime safety (likely thin — single module, no
     processes); testing; performance; idioms/style. Highest severity first.
  3. Each finding: **Severity** (Blocker/High/Medium/Low — a commitment, not a
     hedge), **Location** (`file:LINE`), **What's wrong**, **Why it's wrong**
     (the failure mode, not a restatement), **Fix** (concrete, at the cited line).
  4. **OTP-compatibility** findings belong under correctness — give them their own
     clearly-labelled cluster so the version analysis is easy to find.
  5. **Things I looked for and did not find** — at least five clean checks.
- **`workbench/<DATE>-audit-index.md`** — date, project root, language marked
  *audited*, finding counts by severity, link to the report, and the
  architecture-doc discovery note.

## Stance & reporting

Don't soft-pedal — a real bug reads "fix this," not "consider this." Every
finding lands on a specific `file:line`; no generic advice. Passing tests and a
clean compile are not evidence of correctness — hunt what the tests don't cover.
Write to the floor: distinguish "verified by reading/grepping X" from "I believe
X," and label genuine open design tradeoffs as "open question" at the end of their
category. You produce the audit and self-assess; a separate reviewer verifies it —
do not mark your own findings verified.
