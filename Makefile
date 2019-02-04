PROJECT = giphy
PROJECT_DESCRIPTION = A fun little giphy app that lets you save your favorite photos
PROJECT_VERSION = 0.1.0

DEPS = cowboy jiffy lager
dep_cowboy_commit = 2.6.1

DEP_PLUGINS = cowboy

# this must be first
include erlang.mk

# Compile flags
ERLC_COMPILE_OPTS= +'{parse_transform, lager_transform}'

# Append these settings
ERLC_OPTS += $(ERLC_COMPILE_OPTS)
TEST_ERLC_OPTS += $(ERLC_COMPILE_OPTS)
