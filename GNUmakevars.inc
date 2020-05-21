# Prerequisite: US_WEB_TOP must be set.


# Useful to target for example the root of the current layer (ex: to locate the
# relevant, layer-local '_build' directory):
#
ifndef LAYER_TOP
	LAYER_TOP := $(US_WEB_TOP)
endif


# Project section.


# PROJECT_NAME should be defined on a per-project basis.
ifndef PROJECT_NAME
	PROJECT_NAME := US-Web
endif


# REBAR3_PROJECT_NAME should be defined on a per-project basis.
ifndef REBAR3_PROJECT_NAME
	REBAR3_PROJECT_NAME := us_web
endif


# The uniquely-defined version of this layer, for all uses, including rebar:
US_WEB_VERSION := 0.0.3


# PROJECT_VERSION should be defined on a per-project basis:
ifndef PROJECT_VERSION
	PROJECT_VERSION := $(US_WEB_VERSION)
endif


ifndef PACKAGE_TOP
	PACKAGE_TOP := $(US_WEB_TOP)
endif


US_WEB_RELEASE_BASE := us-web-$(PROJECT_VERSION)

US_WEB_RELEASE_ARCHIVE_ZIP := $(US_WEB_RELEASE_BASE).zip
US_WEB_RELEASE_ARCHIVE_BZ2 := $(US_WEB_RELEASE_BASE).tar.bz2
US_WEB_RELEASE_ARCHIVE_XZ  := $(US_WEB_RELEASE_BASE).tar.xz


# Source section.
US_WEB_SRC := $(US_WEB_TOP)/src

# BEAM path section.
US_WEB_BEAM := $(US_WEB_TOP)/src


MONITOR_SCRIPT = $(US_WEB_TOP)/priv/bin/monitor-us-web.sh


# We rely on Myriad, WOOPER, Traces and US-Common as well:
BEAM_DIRS += $(US_WEB_BEAM_DIRS)


ifndef VM_NAME
	VM_NAME := us_web_debug
endif


ifndef VM_TEST_NAME
	VM_TEST_NAME := us_web_test
endif


ifndef US_CONFIG_FILE
	US_CONFIG_FILE := $(US_TOP)/conf/us.config
endif


# See Myriad counterpart variable (MYRIAD_BEAM_DIRS) for explanations:
US_WEB_BEAM_DIRS := $(US_WEB_BEAM)


# For later reuse in the next layer of the software stack:
US_WEB_PLT_FILE := $(US_WEB_TOP)/us_web.plt


# This is the merged PLT of the level just below in the software stack.
# For us-web, it is the PLT of 'us-common':
#
ifndef PREDECESSOR_PLT
   PREDECESSOR_PLT = $(US_COMMON_PLT_FILE)
endif


# We define the path to the US-Common layer:
#
# (it may be a symbolic link pointing to the actual US-Common package to be
# used)
#
US_COMMON_TOP = $(US_WEB_TOP)/../us_common
