# iso8601 1.4.1 — unpublished release-note draft

Draft release notes for the nine runtime repairs already integrated on `main`.

## Fixes

- Preserve the full positive expanded year when an abbreviated interval endpoint inherits its year or month from the start.
- Inherit an omitted endpoint timezone from the start after the end's abbreviated date and time have been completed in their local context. A complete endpoint remains independent, and an explicit end timezone takes precedence.
- Reject arithmetic targets below year zero with `badarg`, avoiding silent wraparound and nontermination; valid arithmetic can still reach year zero.
- Preserve fractional seconds when applying durations to fractional datetime anchors.
- Normalize rounded formatter fractions across the next second, minute, or date when microsecond rounding carries.
- Preserve fractional-hour and fractional-minute values in `parse_exact/1` and `parse_expanded/1`. The legacy `parse/1` behavior remains: fractional hours and minutes round to whole seconds, while an explicit fractional-seconds field is truncated.
- Validate ISO week dates against both the weekday range and the weeks present in that ISO week-year, including week 53.
- Reject impossible month/day combinations in raw input for year zero and negative proleptic years before applying offsets; previously, an invalid raw tuple could be returned.
- Accept byte iodata consistently in `parse/1`, `parse_exact/1`, `parse_expanded/1`, and `parse_interval/1`. Invalid interval input that is not valid byte iodata now raises `badarg` rather than the prior incidental `badmatch` reason.

No public functions or types were added. Negative expanded years still reject week-date input and nonzero UTC offsets. `add_time/4` rejects nonzero adjustments when its input year is zero. `parse_duration/1` retains its separate string-input contract.

## Compatibility

This patch release adds Erlang/OTP 28 and 29 to CI while retaining OTP 20 through 27. OTP 20 continues to run compile, xref, and Dialyzer only; EUnit, PropEr, and coverage run on OTP 21 through 29.
