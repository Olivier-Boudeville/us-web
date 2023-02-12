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


if [ ! $(id -u) -eq 0 ]; then

	# As operations like chown will have to be performed:
	echo "  Error, this script must be run as root.
${usage}" 1>&2
	exit 5

fi


# XDG_CONFIG_DIRS defined, so that the US server as well (not only these
# scripts) can look it up:
#
xdg_cfg_dirs="${XDG_CONFIG_DIRS}:/etc/xdg"


maybe_us_config_dir="$1"

if [ -n "${maybe_us_config_dir}" ]; then

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

	candidate_dir="$(dirname $(realpath ${maybe_us_config_dir}))"

	xdg_cfg_dirs="${candidate_dir}:${xdg_cfg_dirs}"

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


#echo "epmd_make_opt=${epmd_make_opt}"

# Launching explicitly a properly-condigured EPMD for US-Web (preferably from
# the current weakly-privileged user):
#
# (note that a former instance of EPMD may wrongly report that a node with the
# target name is still running, whereas no Erlang VM even exists)
#
make -s launch-epmd ${epmd_make_opt} || exit


if [ -n "${erl_epmd_port}" ]; then
	epmd_start_msg="configured EPMD port ${erl_epmd_port}"
else
	epmd_start_msg="default US-Web port ${default_us_web_epmd_port}"
fi


echo
echo " -- Starting US-Web natively-built application as user '${us_web_username}', on ${epmd_start_msg}, VM log expected in '${us_web_vm_log_dir}/erlang.log.1'..."

# Apparently variables may be indifferently set prior to make, directly in the
# environment (like 'XDG_CONFIG_DIRS=xxx make TARGET') or as arguments (like
# 'make TARGET XDG_CONFIG_DIRS=xxx'), even if there are a sudo and an authbind
# in the equation.

# Previously the '--depth' authbind option was used, and apparently a depth of 6
# was sufficient; but there is little interest in taking such risks.

#echo Starting US-Web: /bin/sudo -u ${us_web_username} ${authbind} --deep make -s us_web_exec_service XDG_CONFIG_DIRS="${xdg_cfg_dirs}" VM_LOG_DIR="${us_web_vm_log_dir}" US_APP_BASE_DIR="${US_APP_BASE_DIR}" US_WEB_APP_BASE_DIR="${US_WEB_APP_BASE_DIR}" ${cookie_env} ${epmd_make_opt}

/bin/sudo -u ${us_web_username} ${authbind} --deep make -s us_web_exec_service XDG_CONFIG_DIRS="${xdg_cfg_dirs}" VM_LOG_DIR="${us_web_vm_log_dir}" US_APP_BASE_DIR="${US_APP_BASE_DIR}" US_WEB_APP_BASE_DIR="${US_WEB_APP_BASE_DIR}" ${cookie_env} ${epmd_make_opt}

res=$?

# If a launch time-out keeps on progressing, this might be the sign that a
# previous US-Web instance is running.

if [ ${res} -eq 0 ]; then

	# Unfortunately may still be a failure (e.g. if a VM with the same name was
	# already running, start failed, not to be reported as a success)

	echo "  (authbind success reported)"

	# If wanting to check or have more details:
	inspect_us_web_log

	# Not wanting to diagnose too soon, otherwise we might return a failure code
	# and trigger the brutal killing by systemd of an otherwise working us_web:
	#
	sleep 4

	# Better diagnosis than the previous res code:
	#
	# (only renamed once construction is mostly finished; trace_file supposedly
	# inherited from us-web-common.sh)
	#
	if [ -f "${trace_file}" ]; then

		echo "  (success assumed, as '${trace_file}' found)"
		exit 0

	else

		# For some unknown reason, if the start fails (e.g. because a web root
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
