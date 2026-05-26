# iso8601

[![Build Status][gh-actions-badge]][gh-actions]
[![Coverage][coverage-badge]][project]
[![Tag][tag-badge]][tag]
[![Erlang Version][erl-badge]][erl]
[![Downloads][hex downloads]][hex package]

*An ISO 8601 date formatting and parsing library for Erlang*

[![iso8601 project logo][logo]][logo-large]

#### Contents

* [About](#about-)
* [New in 1.4](#new-in-14-)
* [Usage](#usage-)
* [Known Deficiencies](#known-deficiencies-)
* [License](#license-)

## About [&#x219F;](#contents)

The ``erlang_iso8601`` library was originally created by Sean Sawyer in 2011. In 2016, Sean handed off maintenance of the library to the [erlsci](https://github.com/erlsci) Github org at which point the project (and repo) was renamed to simply ``iso8601``, matching its Erlang app name:

* ``git clone https://github.com/erlsci/iso8601.git``

Thanks to Github's forwarding for project renames and moves, the following still work:

* ``git clone https://github.com/seansawyer/erlang_iso8601.git``
* ``git clone https://github.com/erlsci/erlang_iso8601.git``

## New in 1.4 [&#x219F;](#contents)

* **Time interval support.** New `parse_interval/1`, `interval_bounds/1`, and
  `format_interval/1` functions handle all four ISO 8601 interval forms
  (`start/end`, `start/duration`, `duration/end`, and `duration`-only), accept
  both the `/` and `--` separators, support signed durations, and resolve
  abbreviated end points by inheriting the missing leading components from the
  start (e.g. `2007-11-13/15`). See the interval examples in [Usage](#usage-).
* **Expanded year representation.** `parse/1` and `parse_exact/1` now accept a
  leading `+` and more than four digits (e.g. `+10000-01-01`), staying
  `calendar`-compatible. Negative (astronomical) years — for paleontological and
  geological timescales — are available through the opt-in `parse_expanded/1` and
  `format_expanded/1`, which return an `expanded_datetime()` with an `integer()`
  year, leaving `parse/1`'s contract unchanged.
* **Correctness fixes.** `apply_duration/2` now honors a negative sign (a
  duration like `-P1Y` is subtracted rather than added); year arithmetic on a
  leap day (e.g. adding a year to Feb 29) now clamps to a valid date instead of
  producing an impossible one; and stray debug output during ordinal-date
  parsing has been removed.
* **Microsecond precision** is available via `parse_exact/1`, which preserves
  fractional seconds as a float. Note that floats cannot represent every decimal
  fraction exactly, so values are reliable to microsecond precision via rounding
  rather than being bit-exact.

## Usage [&#x219F;](#contents)

Add it to your `rebar.config` deps:

```erlang
{iso8601, "~> 1.4"}
```

Format a timestamp or calendar datetime tuple:

```erlang
1> iso8601:format(now()).
<<"2012-02-16T01:06:19Z">>
2> iso8601:format(calendar:universal_time()).
<<"2012-02-16T01:06:48Z">>
```

Parse a date string or binary:

```erlang
3> iso8601:parse(<<"2012-02-16T01:06:48Z">>).
{{2012,2,16},{1,6,48}}
4> iso8601:parse("2012-02-16T01:06:48Z").
{{2012,2,16},{1,6,48}}
```

Add 1 hour, 2 minutes and 3 seconds to a datetime tuple:

```erlang
5> Datetime = iso8601:parse(<<"2012-02-16T01:06:48Z">>).
{{2012,2,16},{1,6,48}}
6> iso8601:add_time(Datetime, 1, 2, 3).
{{2012,2,16},{2,8,51}}
```

Subtract 1 hour, 2 minutes and 3 seconds from a datetime tuple:

```erlang
5> Datetime = iso8601:parse(<<"2012-02-16T01:06:48Z">>).
{{2012,2,16},{1,6,48}}
6> iso8601:subtract_time(Datetime, 1, 2, 3).
{{2012,2,16},{0,4,45}}
```

Fractional times:

```erlang
7> iso8601:parse("20120203T040506.50").
{{2012,2,3},{4,5,7}}
8> iso8601:parse_exact("20120203T040506.50").
{{2012,2,3},{4,5,6.50}}
```

Parse durations:

```erlang
9> iso8601:parse_duration("+P6Y3M1DT1H1M1.1S").
[{sign, "+"}, {years, 6}, {months, 3}, {days, 1}, {hours, 1}, {minutes, 1}, {seconds, 1}]
10> iso8601:parse_duration("PT6M").
[{sign, []}, {years, 0}, {months, 0}, {days, 0},{hours, 0}, {minutes, 6}, {seconds, 0}]
```

Parse a time interval. `parse_interval/1` preserves which of the four forms was
given, tagging the result so it stays matchable:

```erlang
11> iso8601:parse_interval("2007-03-01T13:00:00Z/2008-05-11T15:30:00Z").
{interval,start_end,{{2007,3,1},{13,0,0}},{{2008,5,11},{15,30,0}}}
12> iso8601:parse_interval("2007-03-01T13:00:00Z/P1Y2M10DT2H30M").
{interval,start_duration,{{2007,3,1},{13,0,0}},
          [{sign,[]},{years,1},{months,2},{days,10},
           {hours,2},{minutes,30},{seconds,0}]}
13> iso8601:parse_interval("P1Y2M10DT2H30M/2008-05-11T15:30:00Z").
{interval,duration_end,
          [{sign,[]},{years,1},{months,2},{days,10},
           {hours,2},{minutes,30},{seconds,0}],
          {{2008,5,11},{15,30,0}}}
14> iso8601:parse_interval("P1Y2M10DT2H30M").
{interval,duration,
          [{sign,[]},{years,1},{months,2},{days,10},
           {hours,2},{minutes,30},{seconds,0}]}
```

An abbreviated end inherits the missing leading components from the start, and
the `--` separator is accepted as well as `/`:

```erlang
15> iso8601:parse_interval("2007-11-13/15").
{interval,start_end,{{2007,11,13},{0,0,0}},{{2007,11,15},{0,0,0}}}
16> iso8601:parse_interval("2000--2002").
{interval,start_end,{{2000,1,1},{0,0,0}},{{2002,1,1},{0,0,0}}}
```

Resolve an interval to concrete `{Start, End}` datetimes with
`interval_bounds/1` (the `start/duration` and `duration/end` forms are computed
from the duration; a `duration`-only interval has no anchor and raises
`badarg`):

```erlang
17> iso8601:interval_bounds(iso8601:parse_interval("2007-03-01T13:00:00Z/P1Y2M10DT2H30M")).
{{{2007,3,1},{13,0,0}},{{2008,5,11},{15,30,0}}}
```

Format an interval back to a binary with `format_interval/1`:

```erlang
18> iso8601:format_interval(iso8601:parse_interval("2007-03-01T13:00:00Z/P1Y2M10DT2H30M")).
<<"2007-03-01T13:00:00Z/P1Y2M10DT2H30M">>
```

Parse an expanded-representation year. Positive expanded years (an explicit `+`
and any number of digits, e.g. years beyond `9999`) work with `parse/1`, which
stays `calendar`-compatible:

```erlang
19> iso8601:parse("+0002007-01-01").
{{2007,1,1},{0,0,0}}
20> iso8601:parse("+10000-01-01").
{{10000,1,1},{0,0,0}}
```

Negative (astronomical) years — where year `0` is 1 BCE, `-1` is 2 BCE, and so on
— use `parse_expanded/1` / `format_expanded/1`, which return an
`expanded_datetime()` with an `integer()` year. `parse/1` rejects negative years
so its `calendar`-compatible contract is preserved:

```erlang
21> iso8601:parse_expanded("-0001-01-01").
{{-1,1,1},{0,0,0.0}}
22> iso8601:format_expanded({{-44,3,15},{0,0,0}}).
<<"-0044-03-15T00:00:00Z">>
```

`parse_expanded/1` preserves fractional seconds (like `parse_exact/1`), so the
seconds field comes back as a float. Negative years are supported in UTC (`Z`) or
offset-free form; combining a negative year with a non-zero UTC offset or a
week-date raises `badarg`.

## Known Deficiencies [&#x219F;](#contents)

* Does not support repeating intervals (`Rn/...`).
* Expanded year representation is supported in extended (hyphenated) format only,
  not in basic format (e.g. `+0002007-01-01`, not `+00020070101`).

See the [open issues](https://github.com/erlsci/iso8601/issues)
for more info.

## Donating

A donation account for supporting development on this project has been set up
on Liberapay here:

* [https://liberapay.com/erlsci-iso8601/donate](https://liberapay.com/erlsci-iso8601/donate)

You can learn more about Liberapay on its [Wikipedia entry][libera-wiki] or on the
service's ["About" page][libera-about].

[libera-wiki]: https://en.wikipedia.org/wiki/Liberapay
[libera-about]: https://liberapay.com/about/

## License [&#x219F;](#contents)

```
The MIT License (MIT)

Copyright © 2011-2014, Sean Sawyer
Copyright © 2012, Tristan Sloughter
Copyright © 2016-2026, Erlang-Aided Enrichment Center
```

[//]: --Named-Links--

[project]: https://github.com/erlsci/iso8601
[gh-actions-badge]: https://github.com/erlsci/iso8601/workflows/ci/badge.svg
[gh-actions]: https://github.com/erlsci/iso8601/actions
[coverage-badge]: https://img.shields.io/badge/coverage-100%25-brightgreen
[tag]: https://github.com/erlsci/iso8601/releases/latest
[tag-badge]: https://img.shields.io/github/tag/erlsci/iso8601.svg
[erl]: http://www.erlang.org/downloads
[erl-badge]: https://img.shields.io/badge/erlang-%E2%89%A520-blue.svg
[logo]: priv/images/logo.png
[logo-large]: priv/images/logo-large.png
[hex package]: https://hex.pm/packages/iso8601
[hex downloads]: https://img.shields.io/hexpm/dt/iso8601.svg
