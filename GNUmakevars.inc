# Prerequisite: US_WEB_TOP must be set.


# To understand why taking into account such conditionals requires to specify
# them first, and in each layer, rather than at the end of the last one, refer
# to the section about conditionals at the top of the GNUmakevars.inc of Myriad.


# Overall settings section.

#EXECUTION_TARGET = production

ifndef EXECUTION_TARGET

	# Other possible value: production
	EXECUTION_TARGET = development

endif


ifeq ($(EXECUTION_TARGET),development)

	#$(info Execution target is development)

	ENABLE_DEBUG = true

else ifeq ($(EXECUTION_TARGET),production)

	#$(info Execution target is production)

	ENABLE_DEBUG = false

else

	$(error Invalid US-Web execution target '$(EXECUTION_TARGET)'; expecting either 'development' or 'production')

endif


# Enforce default values:


# Defining (optional) layer-specific flags, typically used by in-code
# conditional compilation primitives (see cond_utils for their management):

# Topics covered by flags:
#  - execution: VM-level information, notably related to the code path
#  - sni: regarding Server Name Indication
#  - handlers: regarding the web handlers spawned per request (thus which must
# remain efficient)
#
# Defining debug flags (at least the most verbose are by default commented-out):
ifeq ($(ENABLE_DEBUG),true)
	# Could imply leec_debug_sni:
	#US_WEB_DEBUG_FLAGS += -Dus_web_debug_execution
	#US_WEB_DEBUG_FLAGS += -Dus_web_debug_sni
	#US_WEB_DEBUG_FLAGS += -Dus_web_debug_handlers
	#US_WEB_DEBUG_FLAGS += -Dus_web_debug_log_analysis
endif

# Defining check flags:
ifeq ($(EXECUTION_TARGET),development)
	# Does not exist: US_WEB_CHECK_FLAGS += -Dus_web_check_xx
endif


# We ensured that us_web_security must be set in order the compilation to
# succeed:

# Trade-off between accessibility/security:
#US_WEB_SECURITY_FLAGS += -Dus_web_security=relaxed

# Better security, at the expense of universality:
US_WEB_SECURITY_FLAGS += -Dus_web_security=strict


ERLANG_COMPILER_OPT_BASE += $(US_WEB_DEBUG_FLAGS)    \
							$(US_WEB_CHECK_FLAGS)    \
							$(US_WEB_SECURITY_FLAGS)




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


# PROJECT_SHORT_NAME should be defined on a per-project basis.
ifndef PROJECT_SHORT_NAME
	PROJECT_SHORT_NAME := us_web
endif


# PROJECT_NAME_FOR_REBAR should be defined on a per-project basis.
ifndef PROJECT_NAME_FOR_REBAR
	PROJECT_NAME_FOR_REBAR := $(PROJECT_SHORT_NAME)
endif


# The uniquely-defined version of this layer, for all uses, including rebar:
US_WEB_VERSION := 1.0.17


# PROJECT_VERSION should be defined on a per-project basis:
ifndef PROJECT_VERSION
	PROJECT_VERSION := $(US_WEB_VERSION)
endif


# Variable name intentionally not including the current layer, for more generic
# rules:

ifndef VERSION_FOR_REBAR
	VERSION_FOR_REBAR := $(PROJECT_VERSION)
endif


ifndef PACKAGE_NAME
	PACKAGE_NAME := $(PROJECT_NAME)
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
US_WEB_BEAM_DIR := $(US_WEB_TOP)/src


# In an OTP/rebar3-style application layout (a convention now used by our native
# build as well), at compilation time, libraries making use of US-Common will
# expect its includes to be located in:
#
# (lower layers taken in charge in turn)
#
US_COMMON_OTP_INC := -I$(US_COMMON_TOP)/../us_common/include/

INC += $(US_COMMON_OTP_INC)


ifndef VM_NAME
	VM_NAME := us_web_debug
endif


ifndef VM_TEST_NAME
	VM_TEST_NAME := us_web_test
endif


ifndef US_CONFIG_FILE
	US_CONFIG_FILE := $(US_TOP)/conf/us.config
endif


MONITOR_SCRIPT = $(US_WEB_TOP)/priv/bin/monitor-us-web.sh


# Nitrogen section.

USE_NITROGEN = false
#USE_NITROGEN = true

NITROGEN_BEAM_DIRS = $(NITROGEN_CORE_TOP)/ebin $(NITROGEN_DEPS_BEAM_DIRS)

NITROGEN_DEPS_DIRS = cf erlware_commons nitro_cache nprocreg qdate \
					 qdate_localtime rekt simple_bridge stacktrace_compat

NITROGEN_DEPS_BEAM_DIRS = $(patsubst %, $(NITROGEN_CORE_TOP)/deps/%/ebin, $(NITROGEN_DEPS_DIRS))



# See Myriad counterpart variable (MYRIAD_BEAM_DIRS) for explanations:
US_WEB_BEAM_DIRS := $(US_WEB_BEAM_DIR) $(US_WEB_TOP)/..


# We rely on Myriad, WOOPER, Traces and US-Common as well, and, if enabled,
# Nitrogen:
#
BEAM_DIRS += $(US_WEB_BEAM_DIRS) $(NITROGEN_BEAM_DIRS)


# As behaviour-defining modules (such as the nitrogen_core's route_handler one)
# must be found when *compiling* modules (such as us_web_nitrogen_route_handler)
# implementing these behaviours:
#
NITROGEN_BEHAVIOUR_OPT = -pz $(NITROGEN_CORE_TOP)/ebin

ERLANG_COMPILER_EXTRA_OPTS = $(NITROGEN_BEHAVIOUR_OPT)


# The default path where a US-Web native build is to be deployed on a server:
# (useful to synchronise sources)
#
US_NATIVE_DEPLOY_ROOT := /opt/universal-server/us_web-native

# For later reuse in the next layer of the software stack:
US_WEB_PLT_FILE := $(US_WEB_TOP)/us_web.plt


# This is the merged PLT of the level just below in the software stack.
# For us-web, it is the PLT of 'us-common':
#
ifndef PREDECESSOR_PLT
   PREDECESSOR_PLT = $(US_COMMON_PLT_FILE)
endif


# We define the path to the US-Common layer (possibly available as a symbolic
# link).
#
# For the sake of these build-related makefiles, US or Ceylan dependencies will
# be searched (in that order, the one enforced in otp_utils) in a local
# _checkouts child directory (if checkout directories are used and if the
# top-level package of interest is the current one), otherwise in sibling
# directories (i.e. whose build root is at the same level as this one).
#
# This last case happens notably if not using checkout directories, or if using
# them and if all the packages mentioned here are themselves dependencies (in
# which case they are all sitting together in the _checkouts child directory).
#
# Note also we rely on the names of build roots (ex: myriad) as opposed to
# project/repository names (ex: Ceylan-Myriad):
#
# First priority is an existing checkout directory:
ifneq ($(wildcard $(US_WEB_TOP)/_checkouts/us_common/.),)
	US_COMMON_TOP = $(US_WEB_TOP)/_checkouts/us_common
else
	# Second, a sibling one is searched for:
	ifneq ($(wildcard $(US_WEB_TOP)/../us_common/.),)
		US_COMMON_TOP = $(US_WEB_TOP)/../us_common
   else
		# Otherwise most probably in a 'rebar3 compile' context:
		ifneq ($(wildcard $(US_WEB_TOP)/_build/default/lib/us_common/.),)
			US_COMMON_TOP = $(US_WEB_TOP)/_build/default/lib/us_common
		else
			FORCE_EVAL := $(error Unable to find the build root of US-Common from '$(realpath .)'.)
		endif
	endif
endif


# We do the same for LEEC:
ifneq ($(wildcard $(US_WEB_TOP)/_checkouts/leec/.),)
	# By default, expect a checkout directory:
	LEEC_TOP = $(US_WEB_TOP)/_checkouts/leec
else
	# Second, a sibling one is searched for:
	ifneq ($(wildcard $(US_WEB_TOP)/../leec/.),)
		LEEC_TOP = $(US_WEB_TOP)/../leec
   else
		# Otherwise most probably in a 'rebar3 compile' context:
		ifneq ($(wildcard $(US_WEB_TOP)/_build/default/lib/leec/.),)
			LEEC_TOP = $(US_WEB_TOP)/_build/default/lib/leec
		else
			FORCE_EVAL := $(error Unable to find the build root of Ceylan-LEEC from '$(realpath .)'.)
		endif
	endif
endif


ifeq ($(USE_NITROGEN),true)

	# And for any nitrogen_core available:
	ifneq ($(wildcard $(US_WEB_TOP)/_checkouts/nitrogen_core/.),)
		# By default, expect a checkout directory:
		NITROGEN_CORE_TOP = $(US_WEB_TOP)/_checkouts/nitrogen_core
	else
		# Second, a sibling one is searched for:
		ifneq ($(wildcard $(US_WEB_TOP)/../nitrogen_core/.),)
			NITROGEN_CORE_TOP = $(US_WEB_TOP)/../nitrogen_core
		else
			# Otherwise most probably in a 'rebar3 compile' context:
			ifneq ($(wildcard $(US_WEB_TOP)/_build/default/lib/nitrogen_core/.),)
				NITROGEN_CORE_TOP = $(US_WEB_TOP)/_build/default/lib/nitrogen_core
			else
				FORCE_EVAL := $(warning Warning: unable to find a build root for Nitrogen-core from '$(realpath .)'.)
			endif
		endif
	endif

endif
