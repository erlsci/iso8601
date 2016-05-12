# iso8601

*ISO 8601 dates formating and parsing for Erlang*

## About

The ``erlang_iso8601`` library was originally created by Sean Sawyer in 2011. In 2016, Sean handing off maintenance of the library to the [erlsci](https://github.com/erlsci) Github org and renamed the repo to simply ``iso8601``, matching its Erlang app name:

* ``git clone https://github.com/erlsci/iso8601.git``

Thanks to Github's forwarding for project renames and moves, the following still work:

* ``git clone https://github.com/seansawyer/erlang_iso8601.git``
* ``git clone https://github.com/erlsci/erlang_iso8601.git``

## Usage

Add it to your `rebar.config` deps:

```erlang
{'iso8601', ".*", {git, "https://github.com/erlsci/iso8601.git", {tag, "1.1.1"}}}
```

Or for `rebar3`:

```erlang
{'iso8601', {git, "https://github.com/erlsci/iso8601.git", {tag, "1.1.1"}}}
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

## Known Deficiencies

* Does not support expanded year representation.
* Does not support fractional times.
* Does not support ordinal dates.
* Does not support intervals.

See the [open issues](https://github.com/erlsci/iso8601/issues)
for more info.
