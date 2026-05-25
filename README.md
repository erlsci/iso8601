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
* [Usage](#usage-)
* [Known Deficiencies](#known-deficiencies-)
* [License](#license-)

## About [&#x219F;](#contents)

The ``erlang_iso8601`` library was originally created by Sean Sawyer in 2011. In 2016, Sean handed off maintenance of the library to the [erlsci](https://github.com/erlsci) Github org at which point the project (and repo) was renamed to simply ``iso8601``, matching its Erlang app name:

* ``git clone https://github.com/erlsci/iso8601.git``

Thanks to Github's forwarding for project renames and moves, the following still work:

* ``git clone https://github.com/seansawyer/erlang_iso8601.git``
* ``git clone https://github.com/erlsci/erlang_iso8601.git``

## Usage [&#x219F;](#contents)

Add it to your `rebar.config` deps:

```erlang
{iso8601, {git, "https://github.com/erlsci/iso8601.git", {tag, "1.3.2"}}}
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

## Known Deficiencies [&#x219F;](#contents)

* Does not support expanded year representation.
* Does not support intervals.

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
