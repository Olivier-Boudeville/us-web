#!/bin/sh

# A script to diagnose the status of a local US-Web instance.

# Script typically meant to be placed in /usr/local/bin of a gateway
#
# Complementary to running, as root:
# 'systemctl status us-web-as-native-build.service'.


# See also the {start,stop,kill,control,monitor}-us-web.sh scripts.

# To debug such scripts, comment out the silencing redirections around the calls
# to read_us_*config_file calls.


usage="Usage: $(basename $0): returns the status of a supposedly locally-running US-Web (native) instance."

if [ "$1" = "-h" ] || [ "$1" = "--help" ]; then

	echo "${usage}"

	exit 0

fi


if [ ! $# -eq 0 ]; then

	echo "  Error, no argument expected.
${usage}" 1>&2

	exit 10

fi


# Getting the real path is useful, as this script may be actually run through a
# symlink located elsewhere (e.g. in /usr/local/bin):
#
this_script_dir="$(dirname $(realpath $0))"

local_us_web_install_root="${this_script_dir}/../.."

if [ -d "${local_us_web_install_root}/priv" ]; then

	us_web_install_root="$(realpath ${local_us_web_install_root})"
	#echo "Selecting US-Web development native build in '${us_web_install_root}'."

else

	echo "  Error, no valid US-Web native build found, the '$(realpath ${local_us_web_install_root})' location was expected." 1>&2

	exit 15

fi


# We need first to locate the us-web-common.sh script:

# Location expected also by us-common.sh afterwards:
cd "${us_web_install_root}" || exit 40



# Will source in turn us-common.sh:
us_web_common_script_name="us-web-common.sh"
us_web_common_script="${us_web_install_root}/priv/bin/${us_web_common_script_name}"

if [ ! -f "${us_web_common_script}" ]; then

	echo "  Error, unable to find ${us_web_common_script_name} script (not found as '${us_web_common_script}', while being in '$(pwd)')." 1>&2
	exit 35

fi


# Hint for the helper scripts:
us_launch_type="native"

#echo "Sourcing '${us_web_common_script}'."
. "${us_web_common_script}" 1>/dev/null

# We expect a pre-installed US configuration file to exist:
read_us_config_file "$1" 1>/dev/null

read_us_web_config_file 1>/dev/null


# No specific update/check needs regarding vm.args (no VM launched).

echo
echo "Getting the status of the US-Web application possibly running from '${us_web_install_root}' as user '${us_web_username}', on US-Web EPMD port ${us_web_epmd_port}..."

# Yes, twice:
ps_base_opts="-w -w"

# Possibly useful as well: lstart, start_time
ps_format_opts="etime,user,pid,args"

echo
echo "(format for processes: [launched since] [as user] [as PID] [with command])"
echo

echo " -- processes about US-Web (expecting run_erl and the us_web VM process itself):"
# (removed: '--user ${us_web_username}' to detect for all users)
/bin/ps ${ps_base_opts} -e -o ${ps_format_opts} | grep "exec us_web_app" | grep -v grep
echo

echo " -- all local EPMD processes:"
# (removed: '--user ${us_web_username}' to detect for all users)
/bin/ps ${ps_base_opts} -e -o ${ps_format_opts} | grep "bin/epmd" | grep -v grep
echo


echo " -- names listed by this US-Web EPMD instance:"
epmd -port ${us_web_epmd_port} -names
echo


echo " -- corresponding logs recorded by systemd:"
journalctl -e --unit us-web-as-native-build.service --no-pager --lines=50
echo


echo " -- corresponding US-Web applicative logs:"
# If not finding a us-web log file, might be the sign that US-Web is actually
# not running:
#
inspect_us_web_launch_outcome


# Would not be easy to implement, as it would require to extract from the US-Web
# configuration file at least 'http_tcp_port' and a virtual host from
# 'routes'...

#sleep 1
#echo
#echo "Checking website availability:"
#wget http://localhost:8080 -O - | head
