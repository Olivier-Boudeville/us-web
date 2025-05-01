#!/bin/sh

# Stops a US-Web instance, supposedly already running as a native build on the
# current host.

# Script possibly:
# - placed in /usr/local/bin of a gateway
# - run from systemctl, as root, as:
# 'systemctl stop us-web-as-native-build.service'
#
# (hence to be triggered by /etc/systemd/system/us-web-as-native-build.service)
#
# Note: if run through systemd, all outputs of this script (standard and error
# ones) are automatically redirected by systemd to its journal.
#
# To consult them, use:
#   $ journalctl --pager-end --unit=us-web-as-native-build.service

# See also:
#  - kill-us-web-native-build.sh
#  - start-us-web-native-build.sh
#  - deploy-us-web-native-build.sh



# Implementation notes:

# To stop US-Web, we only have to know the target host (which is here the local
# one), the node name ('us_web' here) and the right cookie and EPMD port, which
# requires thus to find and read the relevant US (not US-Web) configuration
# file. For that we rely our helper shell scripts, and thus we have to locate
# the right US-Web installation as a first move.


# We use a dedicated stop client rather than pipes, as currently pipes are found
# unresponsive:
#
# $ to_erl /tmp/launch-erl-1258957
#Attaching to /tmp/launch-erl-1258957 (^D to exit)
#
#^L

# And no entered command is managed (only CTRL-C and CTL-D work), altough the
# TERM environment variable was set before run_erl and to_erl.



usage="Usage: $(basename $0) [US_CONF_DIR]: stops a US-Web server, running as a native build, based on a US configuration directory specified on the command-line (which must end by a 'universal-server' directory), otherwise found through the default US search paths.

The US-Web installation itself will be looked up relatively to this script, otherwise in the standard path applied by our deploy-us-web-native-build.sh script.

Example: '$0 /opt/test/universal-server' is to read /opt/test/universal-server/us.config."


if [ "$1" = "-h" ] || [ "$1" = "--help" ]; then

	echo "${usage}"

	exit 0

fi


# Not necessarily run as root.


# Either this script is called during development, directly from within a US-Web
# installation, in which case this installation shall be used, or (typically if
# called through systemd) the standard US-Web base directory shall be targeted:
#
this_script_dir="$(dirname $0)"

# Possibly:
local_us_web_install_root="${this_script_dir}/../.."

# Checks based on the 'priv' subdirectory for an increased reliability:
if [ -d "${local_us_web_install_root}/priv" ]; then

	us_web_install_root="$(realpath ${local_us_web_install_root})"
	echo "Selecting US-Web development native build in clone-local '${us_web_install_root}'."

else

	# The location enforced by deploy-us-web-native-build.sh:
	us_web_install_root="/opt/universal-server/us_web-native-deployment/us_web"
	echo "Selecting US-Web native build in standard server location '${us_web_install_root}'."

	if [ ! -d "${us_web_install_root}/priv" ]; then

		echo "  Error, no valid US-Web native build found, neither locally (as '$(realpath ${local_us_web_install_root})') nor at the '${us_web_install_root}' standard server location." 1>&2

		exit 15

	fi

fi


if [ $# -gt 1 ]; then

	echo "  Error, extra argument expected.
${usage}" 1>&2

	exit 10

fi


# XDG_CONFIG_DIRS defined, so that the US server as well can look it up (not
# only these scripts):
#
# (avoiding empty path in list)
#
if [ -n "${XDG_CONFIG_DIRS}" ]; then

	xdg_cfg_dirs="${XDG_CONFIG_DIRS}:/etc/xdg"

else

	xdg_cfg_dirs="/etc/xdg"

fi


maybe_us_config_dir="$1"

if [ -n "${maybe_us_config_dir}" ]; then

	# Otherwise would remain in the extra arguments transmitted in CMD_LINE_OPT:
	shift

	case "${maybe_us_config_dir}" in

		/*)
			# Already absolute, OK:
			echo "Using specified absolute directory '${maybe_us_config_dir}'."
			;;
		*)
			# Relative, to be made absolute:
			maybe_us_config_dir="$(pwd)/${maybe_us_config_dir}"
			echo "Transformed specified relative directory in '${maybe_us_config_dir}' absolute one."
			;;
	esac

	if [ ! -d "${maybe_us_config_dir}" ]; then

		echo "  Error, the specified US configuration directory, '${maybe_us_config_dir}', does not exist." 1>&2

		exit 20

	fi

	# Better for messages output:
	maybe_us_config_dir="$(realpath ${maybe_us_config_dir})"

	# As a 'universal-server/us.config' suffix will be added to each candidate
	# configuration directory, we remove the last directory:
	#
	candidate_dir="$(dirname ${maybe_us_config_dir})"

	xdg_cfg_dirs="${candidate_dir}:${xdg_cfg_dirs}"

fi


epmd="$(which epmd 2>/dev/null)"

if [ ! -x "${epmd}" ]; then

	echo "  Error, no EPMD executable found." 1>&2

	exit 8

fi



#echo "Stopping US-Web native build, as following user: $(id)"

# We need first to locate the us-web-common.sh script:

# Location expected also by us-common.sh afterwards:
cd "${us_web_install_root}"

# Will source in turn us-common.sh:
us_web_common_script_name="us-web-common.sh"
us_web_common_script="priv/bin/${us_web_common_script_name}"

if [ ! -f "${us_web_common_script}" ]; then

	echo "  Error, unable to find ${us_web_common_script_name} script (not found as '${us_web_common_script}', while being in '$(pwd)')." 1>&2
	exit 35

fi


# Will source in turn us-common.sh:
us_web_common_script_name="us-web-common.sh"
us_web_common_script="priv/bin/${us_web_common_script_name}"

if [ ! -f "${us_web_common_script}" ]; then

	echo "  Error, unable to find ${us_web_common_script_name} script (not found as '${us_web_common_script}', while being in '$(pwd)')." 1>&2
	exit 35

fi


# Hints for the helper scripts:
export us_launch_type="native"

#echo "Sourcing '${us_web_common_script}'."
. "${us_web_common_script}" #1>/dev/null

# We expect a pre-installed US configuration file to exist:
#echo "Reading US configuration file:"
read_us_config_file "${maybe_us_config_dir}" #1>/dev/null

# Now we also need to read the US-Web configuration file here, to fetch any EPMD
# port there:
#
#echo "Reading US-Web configuration file:"
read_us_web_config_file #1>/dev/null

# Not needed:
#secure_authbind


echo " -- Stopping US-Web natively-built application (EPMD port: ${us_web_epmd_port})..."


# We must stop the VM with the right (Erlang) cookie, i.e. the actual runtime
# one, not the dummy, original one:
#
# Commented-out, as we can actually specify it directly on the command-line:
#update_us_web_config_cookie


if [ -n "${vm_cookie}" ]; then
	#echo "Using cookie '${vm_cookie}'."
	cookie_env="COOKIE=${vm_cookie}"
else
	cookie_env=""
fi

cd src/apps || exit 17

# No sudo or authbind necessary here, no US_* environment variables either:
echo XDG_CONFIG_DIRS="${xdg_cfg_dirs}" make -s us_web_stop_exec EPMD_PORT=${us_web_epmd_port} CMD_LINE_OPT="$* --target-cookie ${vm_cookie}"


# A correct way of passing environment variables:
XDG_CONFIG_DIRS="${xdg_cfg_dirs}" make -s us_web_stop_exec EPMD_PORT=${us_web_epmd_port} CMD_LINE_OPT="$* --target-cookie ${vm_cookie}"

res=$?

if [ $res -eq 0 ]; then

	echo "  (stop success reported)"
	echo

	# If wanting to check or have more details:
	#inspect_us_web_launch_outcome

	exit 0

else

	# Despite following message: 'Node 'us_web_app@127.0.0.1' not responding to
	# pings."

	echo "  Error: stop failure reported (code '$res')" 1>&2
	echo

	#inspect_us_web_launch_outcome

	# Finally wanting pseudo-failure to propagate:
	res=0

fi

# Restore original cookie file:
#/bin/mv -f "${backup_vm_args_file}" "${vm_args_file}"
#/bin/cp -f "${backup_vm_args_file}" "${vm_args_file}"


# Unfortunately, at least in some cases, EPMD will believe that this just
# shutdown US-Web instance is still running; as the next epmd command may not
# work (if -relaxed_command_check was not used at EPMD startup; we nevertheless
# ensure that it is the case if using our start scripts), the only remaining
# solution would be to kill this EPMD; yet this may impact other running Erlang
# applications (see kill-us-web.sh to minimise the harm done), so it is not done
# here.

# Previously any (possibly the default) US-level EPMD port applied here, now the
# US-Web one applies unconditionally:

#if [ -n "${us_web_epmd_port}" ]; then

#   echo "Using user-defined US-Web EPMD port ${us_web_epmd_port}."
#   export ERL_EPMD_PORT="${us_web_epmd_port}"

#else

#   # Using the default US-Web EPMD port (see the EPMD_PORT make variable),
#   # supposing that the instance was properly launched (see the 'launch-epmd'
#   # make target):

#   echo "Using default US-Web EPMD port ${default_us_web_epmd_port}."

#   export ERL_EPMD_PORT="${default_us_web_epmd_port}"

#fi

# Already resolved by us-web-common.sh:
echo "Using, for the US-Web EPMD port, ${us_web_epmd_port}."
export ERL_EPMD_PORT="${us_web_epmd_port}"

if ! ${epmd} -stop us_web; then

	echo "  Error while unregistering the US-Web server from the EPMD daemon at port ${ERL_EPMD_PORT}." 1>&2

	exit 25

fi

# kill-us-web.sh may also be your last-resort friend.

exit ${res}
