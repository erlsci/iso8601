PROJECT = iso8601

include erlang.mk

# Compile flags
ERLC_COMPILE_OPTS= +debug_info \
                   +compressed \
                   +report \
                   +warn_export_all \
                   +warn_export_vars \
                   +warn_shadow_vars \
                   +warn_unused_function \
                   +warn_deprecated_function \
                   +warn_obsolete_guard \
                   +warn_unused_import \
                   +nowarn_export_vars
