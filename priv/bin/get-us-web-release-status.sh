#!/bin/sh

# Script typically meant to be placed in /usr/local/bin of a gateway, for a
# rebar3-based US-Web release (now a US-Web native release is more recommended).
#
# Complementary to running, as root: 'systemctl status us-web.service'


# See also get-us-web-native-build-status.sh, start-us-web.sh and
# stop-us-web.sh.

usage="Usage: $(basename $0): returns the status of a supposedly locally-running US-Web release."

if [ "$1" = "-h" ] || [ "$1" = "--help" ]; then

	echo "${usage}"

	exit 0

fi


#echo "Starting US-Web as following user: $(id)"

# We need first to locate the us-web-common.sh script:

release_base="/opt/universal-server/us_web"

us_web_rel_root=$(/bin/ls -d -t ${release_base}-* 2>/dev/null | head -n 1)

if [ ! -d "${us_web_rel_root}" ]; then

	if ! /bin/ls -d -t ${release_base}-* 1>/dev/null 2>&1; then

		echo "  Error, no US-Web release found in the '${release_base}' directory." 1>&2

		exit 30

	else

		echo "  Error, unable to locate the root of the target US-Web release (tried '${us_web_rel_root}' in ${release_base} (from '$(pwd)')." 1>&2

		exit 31

	fi

fi


# Location expected also by us-common.sh afterwards:
cd "${us_web_rel_root}"

# Will source in turn us-common.sh:
us_web_common_script_name="us-web-common.sh"
us_web_common_script="lib/us_web-latest/priv/bin/${us_web_common_script_name}"

if [ ! -f "${us_web_common_script}" ]; then

	echo "  Error, unable to find ${us_web_common_script_name} script (not found as '${us_web_common_script}', while being in '$(pwd)')." 1>&2
	exit 35

fi

#echo "Sourcing '${us_web_common_script}'."
. "${us_web_common_script}" 1>/dev/null



# Comment redirections for more details:

read_us_config_file $1 #1>/dev/null

read_us_web_config_file #1>/dev/null


# No specific update/check needs regarding vm.args (no VM launched).

echo
echo " -- Getting status of the us_web application possibly running as user '${us_web_username}' (EPMD port: ${erl_epmd_port}), from '${us_web_rel_dir}'..."


# Yes, twice:
ps_base_opts="-w -w"

# Possibly useful as well: lstart, start_time
ps_format_opts="etime,user,pid,args"

echo
echo "Processes: [launched since] [command]"

echo
echo " -- processes about us_web (expecting run_erl and us_web itself):"
# (removed: '--user ${us_web_username}' to detect for all users)
/bin/ps ${ps_base_opts} -e -o ${ps_format_opts} | grep "bin/us_web" | grep -v grep

echo
echo " -- processes for EPMD:"
# (removed: '--user ${us_web_username}' to detect for all users)
/bin/ps ${ps_base_opts} -e -o ${ps_format_opts} | grep "bin/epmd" | grep -v grep
echo

echo
echo " -- EPMD listed names:"

if [ -n "${erl_epmd_port}" ]; then

	epmd_port_opt="-port ${erl_epmd_port}"

fi

epmd ${epmd_port_opt} -names
echo

journalctl -xe --unit us-web.service --no-pager --lines=20

# If not finding a us-web log file, might be the sign that us-web is actually
# not running:
#
inspect_us_web_launch_outcome


# Would not be easy to implement, as it would require to extract from the US-Web
# configuration file at least 'http_tcp_port' and a virtual host from 'routes'...

#sleep 1
#echo
#echo "Checking website availability:"
#wget http://localhost:8080 -O - | head


exit 0
