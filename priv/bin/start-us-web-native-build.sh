#!/bin/sh

# Starts a US-Web instance, to be run as a native build (on the current host).

# Script typically meant to be:
# - placed in /usr/local/bin of a gateway
# - run from systemctl, as root, as:
# 'systemctl start us-web-as-native-build.service'
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
#  - kill-us-web.sh: to ensure, for a proper testing, that no previous instance
#    lingers


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
	us_web_install_root="/opt/universal-server/us_web-native/us_web"
	echo "Selecting US-Web native build in standard server location '${us_web_install_root}'."

	if [ ! -d "${us_web_install_root}/priv" ]; then

		echo "  Error, no valid US-Web native build found, neither locally (as '$(realpath ${local_us_web_install_root})') nor at the '${us_web_install_root}' standard server location." 1>&2

		exit 15

	fi

fi


usage="Usage: $(basename $0) [US_CONF_DIR]: starts a US-Web server, to run as a native build, based on a US configuration directory specified on the command-line, otherwise found through the default US search paths. The US-Web installation itself will be looked up in '${us_web_install_root}'."


if [ "$1" = "-h" ] || [ "$1" = "--help" ]; then

	echo "${usage}"

	exit 0

fi

maybe_us_config_dir="$1"

if [ -n "${maybe_us_config_dir}" ]; then

	if [ ! -d "${maybe_us_config_dir}" ]; then

		echo "  Error, the specified US configuration directory, '${{maybe_us_config_dir}', does not exist." 1>&2

		exit 20

	fi

fi


#echo "Starting US-Web, to run as a native build with following user: $(id)"

# We need first to locate the us-web-common.sh script:

# Location expected also by us-common.sh afterwards:
cd "${us_web_install_root}" || exit

# As expected by us-web-common.sh for the VM logs:
log_dir="${us_web_install_root}/log"
if [ ! -d "${log_dir}" ]; then

	mkdir "${log_dir}"

fi

# Will source in turn us-common.sh:
us_web_common_script_name="us-web-common.sh"
us_web_common_script="priv/bin/${us_web_common_script_name}"

if [ ! -f "${us_web_common_script}" ]; then

	echo "  Error, unable to find ${us_web_common_script_name} script (not found as '${us_web_common_script}', while being in '$(pwd)')." 1>&2
	exit 35

fi


# Hint for the helper scripts:
us_launch_type="native"

#echo "Sourcing '${us_web_common_script}'."
. "${us_web_common_script}" #1>/dev/null

# We expect a pre-installed US configuration file to exist:
read_us_config_file "${maybe_us_config_dir}" #1>/dev/null

read_us_web_config_file #1>/dev/null

secure_authbind

prepare_us_web_launch

cd src || exit


# Note that a former instance of EPMD may wrongly report that a node with the
# target name is still running (whereas no Erlang VM is even running). Apart
# from killing this EPMD instance (jeopardising any other running Erlang
# application), no solution exists apparently (names cannot be unregistered from
# EPMD, as we do not launch it with -relaxed_command_check).

echo
echo " -- Starting US-Web natively-built application as user '${us_web_username}' (EPMD port: ${erl_epmd_port}, whereas log directory is '${us_web_vm_log_dir}')..."


# Previously the '--deep' authbind option was used; apparently the minimal depth
# is 6:

#echo /bin/sudo -u ${us_web_username} VM_LOG_DIR="${us_web_vm_log_dir}" US_APP_BASE_DIR="${US_APP_BASE_DIR}" US_WEB_APP_BASE_DIR="${US_WEB_APP_BASE_DIR}" ${cookie_env} ${epmd_opt} ${authbind} --depth 6 make -s us_web_exec_service

# XDG_CONFIG_DIRS defined, so that the US server can find it as well:

/bin/sudo -u ${us_web_username} XDG_CONFIG_DIRS="${maybe_us_config_dir}" VM_LOG_DIR="${us_web_vm_log_dir}" US_APP_BASE_DIR="${US_APP_BASE_DIR}" US_WEB_APP_BASE_DIR="${US_WEB_APP_BASE_DIR}" ${cookie_env} ${epmd_opt} ${authbind} --depth 6 make -s us_web_exec_service

res=$?

# If a launch time-out keeps on progressing, this might be the sign that a
# previous US-Web instance is running.

if [ ${res} -eq 0 ]; then

	# Unfortunately may still be a failure (ex: if a VM with the same name was
	# already running, start failed, not to be reported as a success)

	echo "  (authbind success reported)"

	# If wanting to check or have more details:
	inspect_us_web_log

	# Better diagnosis than the previous res code:
	# (only renamed once construction is mostly finished)
	#
	# Not wanting to diagnose too soon, otherwise we might return a failure code
	# and trigger the brutal killing by systemd of an otherwise working us_web:
	#
	sleep 4

	# trace_file supposedly inherited from us-web-common.sh:
	if [ -f "${trace_file}" ]; then

		echo "  (success assumed, as '${trace_file}' found)"
		exit 0

	else

		# For some unknown reason, if the start fails (ex: because a web root
		# does not exist), this script will exit quickly, as expected, yet
		# 'systemctl start' will wait for a long time (most probably because of
		# a time-out).
		#
		echo "  (failure assumed, as '${trace_file}' not found)"
		exit 100

	fi

else

	echo "  Error: authbind failure reported (code '$res')" 1>&2
	echo

	inspect_us_web_log

	exit ${res}

fi
