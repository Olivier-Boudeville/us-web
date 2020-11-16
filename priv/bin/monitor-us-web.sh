#!/bin/sh

us_monitor_config_filename="us-monitor.config"

usage="$(basename $0) [US_MONITOR_CONFIG_FILE]: monitors the traces emitted by a US-Web instance possibly running on a remote host based, unless specified otherwise, on a '${us_monitor_config_filename}' configuration file, found in a US configuration directory specified on the command-line, otherwise found through the default US search paths.
Example of use: './monitor-us-web.sh us-monitor-for-development.config', this file being located in the ~/.config/universal-server directory."


if [ "$1" = "-h" ] || [ "$1" = "--help" ]; then

	echo "${usage}"

	exit 0

fi


if [ -n "$1" ]; then

	us_monitor_config_filename="$1"
	shift

fi


us_web_install_root=$(realpath $(dirname $0)/../..)

# Will source in turn us-common.sh:
us_web_common_script_name="us-web-common.sh"
us_web_common_script="${us_web_install_root}/priv/bin/${us_web_common_script_name}"

if [ ! -f "${us_web_common_script}" ]; then

	echo "  Error, unable to find ${us_web_common_script_name} script (not found in '${us_web_common_script}')." 1>&2
	exit 35

fi

#echo "Sourcing '${us_web_common_script}'."
. "${us_web_common_script}" 1>/dev/null



read_us_config_file $1 1>/dev/null


# No specific update/check needs regarding vm.args, as the runtime cookie is
# updated on the fly.


#echo "us_config_dir = ${us_config_dir}"


us_monitor_config_file="${us_config_dir}/${us_monitor_config_filename}"

if [ ! -f "${us_monitor_config_file}" ]; then

	echo "  Error, no us-monitor configuration file found (no '${us_monitor_config_file}')." 1>&2

	exit 5

fi

#echo "Using us-monitor configuration file '${us_monitor_config_file}'."


# us-monitor configuration content, read once for all, with comments (%)
# removed:

us_monitor_base_content=$(/bin/cat "${us_monitor_config_file}" | sed 's|^[[:space:]]*%.*||1')


us_web_hostname=$(echo "${us_monitor_base_content}" | grep us_web_hostname | sed 's|^[[:space:]]*{[[:space:]]*us_web_hostname,[[:space:]]*"||1' | sed 's|"[[:space:]]*}.$||1')

if [ -z "${us_web_hostname}" ]; then

	echo "  Error, not remote US-Web hostname specified (no us_web_hostname defined)." 1>&2
	exit 10

fi


# Finally disabled, as a host that does not answer to ping would trigger a too
# long time-out:
#
#if ! ping -c 1 ${us_web_hostname} 1>/dev/null 2>&1 ; then

	# Not a fatal error, as not all servers answer pings:
	#echo "  Error, unable to ping the '${us_web_hostname}' remote US-Web hostname." 1>&2
	#exit 15

	#echo "  Warning: unable to ping the '${us_web_hostname}' remote US-Web hostname." 1>&2

#fi

#echo "Using '${us_web_hostname}' as remote US-Web hostname."

# Could have been done in the Erlang part:
remote_vm_cookie=$(echo "${us_monitor_base_content}" | grep remote_vm_cookie | sed 's|^[[:space:]]*{[[:space:]]remote_vm_cookie,[[:space:]]*||1' | sed 's|[[:space:]]*}.$||1')


if [ -z "${remote_vm_cookie}" ]; then

	if [ -z "${vm_cookie}" ]; then

		echo " Error, no cookie defined for the remote US-Web host (remote_vm_cookie) nor for the base cookie (vm_cookie)." 1>&2

	else

		echo "No cookie defined for the remote US-Web host, using the base one (defined in us.config's vm_cookie): ${vm_cookie}."
		remote_vm_cookie="${vm_cookie}"

	fi

else

	#echo "Using cookie defined for the remote US-Web host (remote_vm_cookie): ${remote_vm_cookie}."
	:

fi


# Needing from the start of the upcoming VM:
if [ -z "${erl_epmd_port}" ]; then

	echo "No Erlang EPMD port specified, not interfering with context defaults."
	epmd_opt=""

else

	echo "Using specified EPMD port, '${erl_epmd_port}'."
	epmd_opt="ERL_EPMD_PORT=${erl_epmd_port}"

fi

script_dir=$(dirname $0)

app_dir=${script_dir}/../../src/apps/

cd ${app_dir}


# Any argument(s) specified to this script shall be interpreted as a plain,
# extra one:
#
make -s us_web_monitor_exec EPMD_PORT=${erl_epmd_port} CMD_LINE_OPT="$* --config-file ${us_monitor_config_file} --target-cookie ${remote_vm_cookie}"
