# Design — ISO 8601 Time Intervals (issue #3, target 1.4)

Status: draft for review. Closes #3 when implemented.

## 1. Goal & framing

Add ISO 8601 *time interval* support to iso8601. This is purely **additive** — new
functions and a new exported type. No existing function, signature, return shape,
or exported type changes, so it's non-breaking and fits a minor (1.4).

An interval is the time between two points, where each end is a datetime, a date,
or a duration. The standard defines four forms (ISO 8601-1:2019 §4.4):

| # | Form | Example |
|---|------|---------|
| 1 | start / end | `2007-03-01T13:00:00Z/2008-05-11T15:30:00Z` |
| 2 | start / duration | `2007-03-01T13:00:00Z/P1Y2M10DT2H30M` |
| 3 | duration / end | `P1Y2M10DT2H30M/2008-05-11T15:30:00Z` |
| 4 | duration only | `P1Y2M10DT2H30M` (needs external context) |

Separator: a solidus `/` (default). The standard also permits a double hyphen
`--` "by mutual agreement" (and historically `2000--2002`); we support both.

## 2. Scope

**In scope**
- All four forms above, with both `/` and `--` separators.
- Endpoint element **inheritance** in the start/end form (the abbreviated-end
  rule — see §5), e.g. `2007-12-14T13:30/15:30`.
- A faithful parse (preserves which form was given) plus a resolver that computes
  concrete `{Start, End}` bounds.

**Out of scope (forward-compat noted, not implemented)**
- **Repeating intervals** (`Rn/...`, e.g. `R5/2008-03-01T13:00:00Z/P1Y2M10DT2H30M`).
  The Wikipedia section #3 links doesn't cover them, and they're a separable
  feature. The chosen representation (§4) must not *preclude* a later `repeat`
  field/wrapper, but we won't build it now.
- `format_interval/1` (the inverse) — desirable for symmetry with `format/1`, but
  proposed as a fast-follow once the parse representation is settled. See §9.

## 3. Reused building blocks

The feature leans almost entirely on existing internals:
- Endpoints are parsed with the existing `parse/1` (or `parse_exact/1` for
  fractional seconds), which already normalizes timezones to UTC and fills missing
  lower-order fields (e.g. `parse("2012")` → `{{2012,1,1},{0,0,0}}`).
- The duration component reuses `parse_duration/1` and its `datetime_plist()` result.
- Bound resolution reuses the duration arithmetic in `apply_duration/2` — including
  the **sign handling just added in the F-1 fix**, which is what makes the
  `duration/end` case (compute Start = End − Duration) fall out cleanly.

## 4. Public API & representation

### 4.1 New functions

```erlang
-spec parse_interval(iodata()) -> interval().
%% Faithful parse: preserves which of the four forms was supplied.

-spec interval_bounds(interval()) -> {datetime(), datetime()}.
%% Resolve to concrete {Start, End} datetimes. Errors on the duration-only
%% form (no anchor to resolve against).
```

`parse_interval/1` deliberately does **not** resolve, so no information is lost
(was it start/end, or start/duration?). `interval_bounds/1` is the separate,
explicit resolution step. This split keeps each function honest and lets callers
that only want the raw structure avoid imposing assumptions.

### 4.2 The representation — the key decision

**Recommendation: a form-tagged tuple.**

```erlang
-type duration() :: datetime_plist().   %% the parse_duration/1 result
-type interval() ::
        {interval, start_end,      datetime(), datetime()}
      | {interval, start_duration, datetime(), duration()}
      | {interval, duration_end,   duration(), datetime()}
      | {interval, duration,       duration()}.
```

Rationale:
- **Matchable.** "One of four forms" is exactly what a tagged union expresses;
  callers `case` on the form tag.
- **Consistent with the library.** iso8601 exposes plain tuples (`datetime()`,
  `timestamp()`), not maps or records — this fits.
- **Dodges a reserved word.** A map with an `end` key forces the quoted-atom
  papercut `#{'end' := E}`; the tuple avoids it entirely.
- **Forward-compatible.** Repeating intervals can later wrap this, e.g.
  `{interval, repeating, pos_integer() | infinity, interval()}`, without changing
  the existing four shapes.

**Alternative considered: a map** `#{start => ..., 'end' => ..., duration => ...}`
with `undefined` for absent fields. More uniform and trivially extensible, but it
imposes the `'end'` quoting on every caller and departs from the library's tuple
idiom. *Open question for Duncan in §10.*

`interval()` and `duration()` are added to `-export_type`.

## 5. Element inheritance (the hard part)

In the start/end form, the end may omit its most-significant components, which are
then inherited from the start (timezone included). The canonical cases:

| Input | End means | Inherited |
|-------|-----------|-----------|
| `2007-12-14T13:30/15:30` | `2007-12-14T15:30` | date |
| `2008-02-15/03-14` | `2008-03-14` | year |
| `2007-11-13/15` | `2007-11-15` | year + month |
| `2007-11-13T09:00/15T17:00` | `2007-11-15T17:00` | year + month |

### 5.1 Proposed algorithm

1. Parse the **start** fully (it must be a complete, standalone representation).
   Retain its raw token string and its timezone designator.
2. Classify the **end** fragment by its leading structure, to find the
   highest-order component it specifies:
   - contains `T`, or is pure time (`:` present, no `-`) → time-led; inherit the
     whole **date** from start (and, if the fragment starts at hour, the date; if
     it starts at `DDThh`, inherit year+month).
   - contains `-` in a date position (e.g. `MM-DD`) → date-led below year; inherit
     **year** from start.
   - a bare day-of-month (no separators), with a full-date start → inherit
     **year + month** from start.
3. Textually prepend the inherited components (and append the start's timezone if
   the end omits one) to form a complete end string, then parse it with `parse/1`.
4. **Genuinely ambiguous fragments** (e.g. a bare 2-digit token whose role can't be
   pinned from separators and the start's granularity) → `error(badarg)` with the
   ambiguity documented, rather than guessing.

### 5.2 Phasing recommendation

Inheritance is where the edge cases and ambiguity live. Recommend implementing in
two milestones *within* the 1.4 work:

- **Phase A** — full (non-abbreviated) endpoints for all four forms + both
  separators. Unambiguous, covers the majority of real-world strings, ships value
  immediately.
- **Phase B** — end-element inheritance per §5.1, with the four canonical cases as
  tests and explicit `badarg` on ambiguity.

Gate B behind A so a slip on the gnarly part doesn't hold the rest.

## 6. Bound resolution semantics

`interval_bounds/1`:
- `start_end` → `{Start, End}` directly.
- `start_duration` → `{Start, Start + Duration}`.
- `duration_end` → `{End − Duration, End}` (apply the duration negated — the F-1
  sign machinery already does this).
- `duration` → `error(badarg)` (no anchor; document that bounds require an anchored
  interval).

Implementation note: factor the offset-application core out of `apply_duration/2`
so it can be driven by an already-parsed `duration()` plist, instead of
re-stringifying. `apply_duration/2`'s public behavior is unchanged.

## 7. Edge cases & decisions

- **Date-only endpoints** (`2007-11-13`) parse to midnight via the existing
  `parse/1` semantics, so a date interval resolves to midnight bounds. This loses
  the "any time that day" nuance but is a reasonable, documented normalization.
  Don't invent a separate date-interval type for 1.4.
- **Timezone inheritance**: the end inherits the start's tz when it omits one
  (step 3). Mixed explicit tzs are kept as-is (each endpoint normalized to UTC by
  `parse/1`).
- **`--` vs `/`**: split on `/` first; if absent, split on `--`. Safe because
  datetimes use single `-` and durations contain no `-`, so `--` only ever appears
  as the separator.
- **Fractional seconds** in endpoints: route through `parse_exact/1` when a
  fraction is present, consistent with the rest of the library.
- **Zero / negative durations**: durations are non-negative in interval context per
  the standard; a signed duration (`-P1Y`) in an interval is unusual — decide
  whether to accept and apply, or reject. *Open question, §10.*

## 8. Error handling

Consistent with the rest of the library: invalid interval strings →
`error(badarg)` (let it crash at the parse edge). No `{error, _}` tuples — that
would diverge from `parse/1`/`parse_duration/1`. `interval_bounds/1` on a
duration-only interval also raises `badarg`.

## 9. Backward compatibility

Strictly additive: new `parse_interval/1`, `interval_bounds/1`, and exported types
`interval/0`, `duration/0`. No change to any existing export, spec, or type. OTP
20+ floor preserved (no post-20 constructs). No `vsn` change in the feature commits
themselves — Duncan bumps to 1.4 at release. `format_interval/1` is a proposed
fast-follow, not part of the minimal close.

## 10. Decisions (resolved 2026-05-25)

1. **Representation**: **form-tagged tuple** (§4.2) — matches the repo's age and
   tuple idiom.
2. **`duration()` type**: **alias the existing `datetime_plist()`** — reuse the
   public shape callers already know from `parse_duration/1`.
3. **Signed durations**: **supported** — accept and apply `-P1Y` etc. in interval
   context.
4. **`format_interval/1`**: **in scope for 1.4** (not a fast-follow).
5. **Element inheritance (Phase B)**: **in 1.4** — ship the full feature.

All four forms, both separators, inheritance, signed durations, and
parse/bounds/format land together in 1.4.

## 11. Testing strategy

- **eunit**: one generator per form; both separators; each §5 inheritance case;
  the date-only/midnight normalization; tz inheritance; and `badarg` cases
  (malformed, ambiguous fragment, bounds-on-duration-only).
- **PropEr**: round-trip `parse_interval(format_interval(I)) =:= I` once formatting
  exists; and an invariant that for `start_duration`, `interval_bounds` yields
  `End = Start + Duration` (cross-checked against `apply_duration/2`).
- Keep the suite at 100% line coverage, per the project gate.
