#!/bin/sh

# Stops a US-Web instance, supposedly already running as a native build on the
# current host.

# Script typically meant to be:
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
#  - deploy-us-web-native-build.sh
#  - stop-us-web-native-build.sh
#  - start-universal-server.sh



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



# Either this script is called during development, directly from within a US-Web
# installation, in which case this installation shall be used, or (typically if
# called through systemd) the standard US-Web base directory shall be targeted:
#
this_script_dir="$(dirname $0)"

# Possibly:
local_us_web_install_root="${this_script_dir}/../.."

# Check:
if [ -d "${local_us_web_install_root}/priv" ]; then

	us_web_install_root="${local_us_web_install_root}"
	echo "Selecting US-Web development native build in '${us_web_install_root}'."

else

	# The location enforced by deploy-us-web-native-build.sh:
	us_web_install_root="/opt/universal-server/us_web-native"
	echo "Selecting US-Web native build in standard location '${us_web_install_root}'."

	if [ ! -d "${us_web_install_root}" ]; then

		echo "  Error, no US-Web native build found, neither locally (as '$(realpath ${local_us_web_install_root})') nor at the '${us_web_install_root}' standard location." 1>&2

		exit 15

	fi

fi

usage="Usage: $(basename $0) [US_CONF_DIR]: stops a US-Web server, running as a native build, based on a US configuration directory specified on the command-line, otherwise found through the default US search paths. The US-Web installation itself will be looked up in '$(realpath ${us_web_install_root})'."

if [ ! -d "${us_web_install_root}/priv" ]; then

	echo "  Error, unable to locate the root of the target US-Web native build (tried '$(realpath ${us_web_install_root})').
${usage}" 1>&2
		exit 5

fi


if [ "$1" = "-h" ] || [ "$1" = "--help" ]; then

	echo "${usage}"

	exit 0

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
export US_LAUNCH_TYPE="native"

#echo "Sourcing '${us_web_common_script}'."
. "${us_web_common_script}" 1>/dev/null

#echo "Reading US configuration file:"
read_us_config_file $1 1>/dev/null

# Actually we do not need to read the US-Web configuration file here:
#echo "Reading US-Web configuration file:"
#read_us_web_config_file #1>/dev/null

#secure_authbind


echo " -- Stopping US-Web natively-built application as user '${us_web_username}' (EPMD port: ${erl_epmd_port}) with '${us_web_exec}'..."


# We must stop the VM with the right (Erlang) cookie, i.e. the actual runtime
# one, not the dummy, original one:
#
# Commented-out, as we can actually specify it directly on the command-line:
#update_us_web_config_cookie


if [ -n "${vm_cookie}" ]; then
	echo "Using cookie '${vm_cookie}'."
	cookie_env="COOKIE=${vm_cookie}"
else
	cookie_env=""
fi

cd src/apps

# No sudo or authbind necessary here, no US_* environment variables either:
echo make -s us_web_stop_exec EPMD_PORT=${erl_epmd_port} CMD_LINE_OPT="$* --target-cookie ${vm_cookie}"

make -s us_web_stop_exec EPMD_PORT=${erl_epmd_port} CMD_LINE_OPT="$* --target-cookie ${vm_cookie}"
res=$?

if [ $res -eq 0 ]; then

	echo "  (stop success reported)"
	echo

	# If wanting to check or have more details:
	#inspect_us_web_log

	exit 0

else

	# Despite following message: 'Node 'us_web_app@127.0.0.1' not responding to
	# pings."

	echo "  Error: stop failure reported (code '$res')" 1>&2
	echo

	#inspect_us_web_log

	# Finally wanting pseudo-failure to propagate:
	res=0

fi

# Restore original cookie file:
#/bin/mv -f "${backup_vm_args_file}" "${vm_args_file}"
#/bin/cp -f "${backup_vm_args_file}" "${vm_args_file}"


# Will generally not work (-relaxed_command_check not having been used):
epmd -port ${erl_epmd_port} -stop us_web

# So 'killall epmd' may also be your friend, although it may affect other
# applications such as the Universal server itself.

exit $res
