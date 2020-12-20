US_WEB_TOP = .


.PHONY: help help-intro help-us-web register-version-in-header register-us-web \
		list-beam-dirs add-prerequisite-plts link-plt stats                    \
		all all-rebar3 compile                                                 \
		ensure-dev-release ensure-prod-release                                 \
		release release-dev release-prod                                       \
		export-release just-export-release upgrade-us-common                   \
		sync-full-us-web                                                       \
		start debug debug-as-release start-as-release status                   \
		stop stop-as-release                                                   \
		log cat-log tail-log check-web-availability                            \
		inspect monitor-development monitor-production                         \
		kill shell test test-interactive                                       \
		clean clean-logs real-clean clean-otp-build-tree clean-rebar-cache     \
		info info-local info-conditionals


MODULES_DIRS = src doc conf priv test


# To override the 'all' default target with a parallel version:
BASE_MAKEFILE = true

# Base directory for the build:
BUILD_ROOT := _build


## Default build section.

# Base directory for the default build:
DEFAULT_BASE := $(BUILD_ROOT)/default

# Base directory for default releases:
DEFAULT_REL_BASE := $(DEFAULT_BASE)/rel

# Base directory of the default us_web release:
US_WEB_DEFAULT_REL_DIR := $(DEFAULT_REL_BASE)/us_web

US_DEFAULT_REL_EXEC := $(US_WEB_DEFAULT_REL_DIR)/bin/us_web



## Production build section.

# Base directory for the production build:
PROD_BASE := $(BUILD_ROOT)/prod

# Base directory for production releases:
PROD_REL_BASE := $(PROD_BASE)/rel

# Base directory of the production us_web release:
US_WEB_PROD_REL_DIR := $(PROD_REL_BASE)/us_web


# Not ':=', to resolve version:
RELEASE_NAME = us_web-$(US_WEB_VERSION).tar.gz

RELEASE_PATH = $(US_WEB_PROD_REL_DIR)/$(RELEASE_NAME)

RELEASE_DEPLOY_SCRIPT = $(US_WEB_TOP)/priv/bin/deploy-us-web-release.sh


EXPORT_TARGET := $(WEB_SRV):/tmp


LOG_DIR := $(US_WEB_DEFAULT_REL_DIR)/log

VM_LOG_FILES := $(LOG_DIR)/erlang.log.*


# SIMPLE_BRIDGE_LIB_DIR := $(DEFAULT_BASE)/lib/simple_bridge

# SIMPLE_BRIDGE_CONFIG := $(SIMPLE_BRIDGE_LIB_DIR)/etc/simple_bridge.config


# Now the default build system is our own. Use the 'all-rebar3' target if
# preferring the rebar3 way of building.
#
all:


all-rebar3:
	@$(REBAR3_EXEC) upgrade
	@$(REBAR3_EXEC) compile


# Default target:
help: help-intro help-us-web


help-intro:
	@echo " Following main make targets are available for package $(PACKAGE_NAME):"


help-us-web:
	@cd $(US_COMMON_TOP) && $(MAKE) -s help-us-common


register-version-in-header:
	@if [ -z "$(VERSION_FILE)" ] ; then \
	echo "Error, no version file defined." 1>&2 ; exit 52 ; else \
	$(MAKE) register-us-web ; fi


register-us-web:
	@echo "-define( us_web_version, \"$(US_WEB_VERSION)\" )." >> $(VERSION_FILE)


# Useful to extract internal layout for re-use in upper layers:
list-beam-dirs:
	@for d in $(US_WEB_BEAM_DIRS) ; do echo $$(readlink -f $$d) ; done


add-prerequisite-plts: link-plt


# As upper layers may rely on the 'us_web' naming:
link-plt:
	@if [ ! "$(PLT_FILE)" = "$(US_WEB_PLT_FILE)" ]; then ln -s --force $(PLT_FILE) $(US_WEB_PLT_FILE) ; fi


stats:
	@$(MAKE_CODE_STATS) $(US_WEB_TOP)


# The 'compile' target just by itself would not recompile a US-Web source file
# that would have been changed:
#
#all: compile


compile: rebar3-create-app-file
	@echo "  Compiling us_web from $$(pwd)"
	@$(REBAR3_EXEC) compile


# Ensures a relevant development release is available.
ensure-dev-release:
	@if [ ! -f "$(US_DEFAULT_REL_EXEC)" ] ; then \
	echo "No $(US_DEFAULT_REL_EXEC) found, building a development release." ; $(MAKE) -s release-dev ; fi


# Ensures a relevant production release is available.
ensure-prod-release:
	@if [ ! -f "$(US_DEFAULT_REL_EXEC)" ] ; then \
	echo "No $(US_DEFAULT_REL_EXEC) found, building a production release." ; $(MAKE) -s release-prod ; fi



# The 'clean-otp-build-tree' target is run before generating a release, as
# otherwise past elements (ex: in link with .app files) will be re-used.

release: release-prod


# Probably that '@$(REBAR3_EXEC) tar' exceeds what is needed:
#
# ('compile' is not needed either: same happens because of 'release')
#
release-dev: clean-otp-build-tree rebar3-create-app-file rebar.config #compile #update-release
	@echo "  Generating OTP us_web release in development mode, using $(shell rebar3 -v)"
	@$(REBAR3_EXEC) release
	@cd $(US_WEB_DEFAULT_REL_DIR)/releases && /bin/ln -sf $(US_WEB_VERSION) latest-release


# Rebuilding all dependencies ('compile' implied):
# (yes, 'tar', not 'release')
#
release-prod: real-clean rebar3-create-app-file set-rebar-conf
	@echo "  Generating OTP us_web release $(US_WEB_VERSION) from scratch in production mode, using $(shell rebar3 -v)"
	@$(REBAR3_EXEC) as prod tar


# Rebuilding the "normal" version thereof (not the testing or Hex one):
rebar.config: conf/rebar.config.template
	@$(MAKE) -s set-rebar-conf


# Just rebuilding us-web:
#
# (not updating rebar config for example to take into account changes in release
# version, as the new release archive contains paths with the former version...)
#
# Note: 'rebar3 compile' / 'compile' dependency target not needed here (as implied).
#
release-prod-light:
	@echo "  Generating OTP us_web release $(US_WEB_VERSION) in production mode"
	@$(REBAR3_EXEC) as prod tar


export-release: release-prod-light just-export-release


just-export-release:
	@echo "  Exporting production release $(RELEASE_NAME) to $(EXPORT_TARGET)"
	@scp $(SP) $(RELEASE_PATH) $(RELEASE_DEPLOY_SCRIPT) $(EXPORT_TARGET)
	-@$(NOTIFIER_TOOL)


# So that any change in US-Common repository can be used here:
#
# (note: for actual US development, prefer using a _checkouts directory for all
# relevant dependencies)
#
upgrade-us-common:
	@$(REBAR3_EXEC) upgrade us_common


# Synchronises all {Ceylan,US}-related sources of US-Web to the specified server
# location:
#
sync-full-us-web:
	@cd $(MYRIAD_TOP) && $(MAKE) -s sync-to-server
	@cd $(WOOPER_TOP) && $(MAKE) -s sync-to-server
	@cd $(TRACES_TOP) && $(MAKE) -s sync-to-server
	@cd $(LEEC_TOP) && $(MAKE) -s sync-to-server
	@cd $(US_COMMON_TOP) && $(MAKE) -s sync-to-server
	@$(MAKE) -s sync-to-server


# Not used anymore, as the simple_bridge configuration file does not seem to be
# taken into account:
#
update-release:
	@echo "  Updating $(SIMPLE_BRIDGE_CONFIG)"
	@cat $(SIMPLE_BRIDGE_CONFIG) | sed 's|{handler, simple_bridge_handler_sample}|{handler, us_web_handler}|' | sed 's|%% {backend, yaws}|{backend,cowboy}|' > $(SIMPLE_BRIDGE_CONFIG).tmp && /bin/mv $(SIMPLE_BRIDGE_CONFIG).tmp $(SIMPLE_BRIDGE_CONFIG)


# Release shall have been generated beforehand:
#
# (CTRL-C twice in the console will not be sufficient to kill this instance, use
# 'make kill' instead)
#
# (apparently 'start' was replaced with 'foreground' or 'daemon', when using a
# prebuilt version of rebar3; we use rather the former as we prefer using the
# same, relevant inquiry scripts in all contexts)
#
# Note also that the daemon option will not return (yet would still run if
# exiting with CTRL-C); launching it in the background is thus preferable.
#
start: kill clean-logs compile
	@echo "Starting the us_web release (EPMD port: $(EPMD_PORT)):"
	@#export ERL_EPMD_PORT=$(EPMD_PORT) ; $(US_WEB_DEFAULT_REL_DIR)/bin/us_web daemon &
	@export ERL_EPMD_PORT=$(EPMD_PORT) ; $(US_WEB_DEFAULT_REL_DIR)/bin/us_web start &
	@sleep 1 ; $(MAKE) -s log


debug:
	@echo " Running us_web for debug natively (EPMD port: $(EPMD_PORT))"
	@cd src && $(MAKE) us_web_exec CMD_LINE_OPT="--batch"


debug-as-release: ensure-dev-release
	@echo " Running us_web for debug as a release (EPMD port: $(EPMD_PORT))"
	@killall java 2>/dev/null ; export ERL_EPMD_PORT=$(EPMD_PORT) ; $(MAKE) -s start || $(MAKE) -s log


# Not tested yet, as we use releases in production mode, through systemd.
start-as-release:
	@echo "Starting a us_web release (EPMD port: $(EPMD_PORT)):"
	@$(US_WEB_TOP)/priv/bin/start-us-web.sh



# A rule such as the following would be bound to fail because of a non-matching
# cookie:
#
#	-@export ERL_EPMD_PORT=$(EPMD_PORT) ; $(US_WEB_DEFAULT_REL_DIR)/bin/us_web status
status:
	@echo "Status of the us_web release (EPMD port: $(EPMD_PORT)):"
	@$(US_WEB_TOP)/priv/bin/get-us-web-status.sh


# A rule such as the following would be bound to fail because of a non-matching
# cookie:
#
#  @export ERL_EPMD_PORT=$(EPMD_PORT) ; $(US_WEB_DEFAULT_REL_DIR)/bin/us_web stop || ( echo "Stop failed" ; $(MAKE) -s log )
#
# Note: will probably not work due to the VM cookie having been changed; use
# 'stop-brutal' instead, if run with 'start' or 'debug':
#
stop:
	@echo "Stopping us_web release (EPMD port: $(EPMD_PORT)):"
	@export ERL_EPMD_PORT=$(EPMD_PORT) ; $(US_WEB_DEFAULT_REL_DIR)/bin/us_web stop || $(MAKE) -s log


# Useful typically if the runtime cookie was changed:
stop-brutal: kill


# A rule such as the following would be bound to fail because of a non-matching
# cookie:
#
#	-@export ERL_EPMD_PORT=$(EPMD_PORT) ; $(US_WEB_DEFAULT_REL_DIR)/bin/us_web stop
#
# Note: only applies when the target instance has been started as a release.
#
# A rule such as the following would be bound to fail because of a non-matching
# cookie:
#
#  @export ERL_EPMD_PORT=$(EPMD_PORT) ; $(US_WEB_DEFAULT_REL_DIR)/bin/us_web stop || ( echo "Stop failed" ; $(MAKE) -s log )
#
stop-as-release:
	@echo "Stopping the us_web release (EPMD port: $(EPMD_PORT)):"
	@$(US_WEB_TOP)/priv/bin/stop-us-web.sh


# Note: this target works; one should just scroll upward sufficiently in one's
# terminal.
#
log: cat-log
#log: tail-log


# Useful to debug when crashing:
cat-log:
	@sleep 1
	@cat $(VM_LOG_FILES)


# When more stable; preferred to more:
tail-log:
	@tail --lines=20 -f $(VM_LOG_FILES)


# Settings supposed in line with a local, debug US-Web configuration file:
check-web-availability:
	@wget http://localhost:8080 -O -


# run_erl here, not beam.smp:
inspect:
	-@ps -edf | grep run_erl | grep -v grep 2>/dev/null || echo "(no run_erl process)"
	-@ps -edf | grep epmd | grep -v grep 2>/dev/null || echo "(no epmd process)"
	@$(MAKE) -s log


# Monitors a US-Web server that (already) runs in development mode, from
# specified US config:
#
monitor-development:
	@$(MONITOR_SCRIPT) us-monitor-for-development.config


# Monitors a US-Web that (already) runs in production mode, from specified US
# config:
#
monitor-production:
	@$(MONITOR_SCRIPT) us-monitor-for-production.config


kill:
	-@killall epmd run_erl 2>/dev/null || true


test: test-interactive


# Shorthand:
shell: test-interactive


# us_web auto-booted:
#
# One may paste 'io:format( \"~s\", [ us_web:get_runtime_configuration() ] ).'
# in following shell:
#
test-interactive: compile
	@$(REBAR3_EXEC) shell


# Creates the symbolic links that allow the make system to find its Ceylan
# prerequisites (once they have been cloned once by rebar3; for that, just
# comment-out the include at bottom, and run 'make release'):
#
# (now in _checkouts/)
#
#links:
#	@cd ../ ; for p in myriad wooper traces us_common; do ln -s $$p ; done


clean-local: clean-log
	@echo "  Cleaning from $$(pwd)"
	@$(REBAR3_EXEC) clean


# Web-related logs already removed by the recursive 'clean' target:
clean-logs:
	-@/bin/rm -f $(VM_LOG_FILES)


real-clean: clean clean-otp-build-tree clean-rebar-cache
	@echo "  Real cleaning from $$(pwd)"
	-@/bin/rm -f rebar.lock


clean-otp-build-tree:
	@echo "  Cleaning OTP build tree"
	-@/bin/rm -rf _build


clean-rebar-cache:
	-@/bin/rm -rf $(HOME)/.cache/rebar3


info: info-local


info-local:
	@echo "US_WEB_DEFAULT_REL_DIR = $(US_WEB_DEFAULT_REL_DIR)"
	@echo "US_DEFAULT_REL_EXEC = $(US_DEFAULT_REL_EXEC)"
	@echo "VM_LOG_FILES = $(VM_LOG_FILES)"
	@echo "RELEASE_PATH = $(RELEASE_PATH)"
	@echo "US_COMMON_TOP = $(US_COMMON_TOP)"
	@echo "LEEC_TOP = $(LEEC_TOP)"
	@echo "TRACES_TOP = $(TRACES_TOP)"
	@echo "WOOPER_TOP = $(WOOPER_TOP)"
	@echo "MYRIAD_TOP = $(MYRIAD_TOP)"
	@echo "REBAR3_EXEC = $(REBAR3_EXEC)"
	@echo "REBAR_INCS = $(REBAR_INCS)"


info-conditionals:
	@echo "US_WEB_DEBUG_FLAGS = $(US_WEB_DEBUG_FLAGS)"
	@echo "US_WEB_CHECK_FLAGS = $(US_WEB_CHECK_FLAGS)"


include $(US_WEB_TOP)/GNUmakesettings.inc
