# erlang_iso8601 #

Formats and parses ISO 8601 dates. This fork incorporates various improvements
such as [sl45sms](https://www.github.com/sl45sms) parse durations patch aswell
as [tsloughter](https://www.github.com/tsloughter) decimal seconds patch

## Usage ##

Add it to your `rebar.config` deps:

```erlang
{'iso8601', ".*", {git, "git@github.com:kivra/erlang_iso8601.git", {tag, "1.2.0"}}}
```

Format a timestamp or calendar datetime tuple:

```erlang
> iso8601:now().
<<"2012-02-16T01:06:19Z">>
> iso8601:format(calendar:universal_time()).
<<"2012-02-16T01:06:48Z">>
```

Parse a date string or binary:

```erlang
> iso8601:parse(<<"2012-02-16T01:06:48Z">>).
{{2012,2,16},{1,6,48}}
> iso8601:parse("2012-02-16T01:06:48Z").
{{2012,2,16},{1,6,48}}
```

Add 1 hour, 2 minutes and 3 seconds to a datetime tuple:

```erlang
> Datetime = iso8601:parse(<<"2012-02-16T01:06:48Z">>).
{{2012,2,16},{1,6,48}}
> iso8601:add_time(Datetime, 1, 2, 3).
{{2012,2,16},{2,8,51}}
```

Get interval datetimes list examples:

```erlang
> Intervals = iso8601:parse_interval("R5/2008-03-01T13:00:00Z/P1Y2M10DT2H30M").
[{{2009,5,11},{15,30,0}},
 {{2010,7,21},{18,0,0}},
 {{2011,10,1},{20,30,0}},
 {{2013,12,11},{23,0,0}},
 {{2015,2,22},{1,30,0}}]
> Intervals = iso8601:parse_interval("2008-03-01T13:00:00Z/P1Y2M10DT2H30M").
> Intervals = iso8601:parse_interval("R5/P1Y2M10DT2H30M").
> Intervals = iso8601:parse_interval("R5/P1Y/2008-03-01T13:00:00Z").
```

## Known deficiencies ##

* Does not support expanded year representation.
* Does not support fractional times.
* Does not support ordinal dates.

See the [open issues](https://github.com/seansawyer/erlang_iso8601/issues)
for more info.
