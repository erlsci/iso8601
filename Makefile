PROJECT = iso8601

# Options
ERLC_OPTS  = +debug_info +nowarn_shadow_vars +warnings_as_errors
EUNIT_OPTS = [verbose]

# Standard targets
include erlang.mk

# eof
