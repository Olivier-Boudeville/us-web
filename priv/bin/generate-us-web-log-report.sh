#!/bin/sh

us_web_remote_access_config_filename="us-web-remote-access.config"

usage="Usage: $(basename $0) [--config US_WEB_REMOTE_ACCESS_CONFIG_FILE] [--domain DOMAIN] [--host VIRTUAL_HOST]: requests the US-Web instance (possibly running on a remote host) to generate the HTML report corresponding to the log analysis for the specified virtual host of the specified domain.
This US-Web instance is found either based on a default '${us_web_remote_access_config_filename}' configuration filename or on a specified one, both looked-up in the US configuration directory found through the default US search paths.

If no virtual host is specified, then the reports of all virtual hosts for the specified domain will be generated.
If no domain is specified, then the reports of all the virtual hosts of all the hosted domains will be generated.

Then the corresponding report(s) can be browsed directly through any meta website declared.

Examples of use:
   './$(basename $0) us-web-remote-access-for-development.config --domain foobar.org --host baz', this configuration file being located in the standard US configuration search paths, for example the ~/.config/universal-server/ directory, a report being generated for the baz.foobar.org FQDN.
   './$(basename $0)': looks up ${us_web_remote_access_config_filename} through the default US search paths and requests the corresponding US-Web instance to generate reports for all its hosted websites."

target_domain=""
target_host=""

while [ $# -gt 0 ]; do

	token_eaten=1

	if [ "$1" = "-h" ] || [ "$1" = "--help" ]; then
		echo "${usage}"
		exit
		token_eaten=0
	fi

	if [ "$1" = "--config" ]; then
		shift
		if [ -z "$1" ]; then
			echo "  Error, no configuration file specified after --config.
${usage}" 1>&2
			exit 10
		fi
		us_web_remote_access_config_filename="$1"
		token_eaten=0
	fi

	if [ "$1" = "--domain" ]; then
		shift
		if [ -z "$1" ]; then
			echo "  Error, no domain specified after --domain.
${usage}" 1>&2
			exit 10
		fi
		target_domain="$1"
		token_eaten=0
	fi

	if [ "$1" = "--host" ]; then
		shift
		if [ -z "$1" ]; then
			echo "  Error, no host specified after --host.
${usage}" 1>&2
			exit 10
		fi
		target_host="$1"
		token_eaten=0
	fi


	if [ $token_eaten -eq 1 ]; then
		echo "Error, unexpected argument ('$1').
${usage}." 1>&2
		exit 5
	fi

	shift

done

#echo "us_web_remote_access_config_filename = ${us_web_remote_access_config_filename}"
#echo "target_domain = ${target_domain}"
#echo "target_host = ${target_host}"


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

# Now that us_config_dir is known:
uw_cfg_file="${us_config_dir}/${us_web_remote_access_config_filename}"

if [ ! -f "${uw_cfg_file}" ]; then

	echo "  Error, no US-Web configuration file found (no '${uw_cfg_file}')." 1>&2

	exit 5

fi

#echo "Using US-Web configuration file '${uw_cfg_file}'."


# US-Web configuration content, read once for all, with comments (%) removed:

uw_cfg_base_content=$(/bin/cat "${uw_cfg_file}" | sed 's|^[[:space:]]*%.*||1')


us_web_hostname=$(echo "${uw_cfg_base_content}" | grep us_web_hostname | sed 's|^[[:space:]]*{[[:space:]]*us_web_hostname,[[:space:]]*"||1' | sed 's|"[[:space:]]*}.$||1')

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
remote_vm_cookie=$(echo "${uw_cfg_base_content}" | grep remote_vm_cookie | sed 's|^[[:space:]]*{[[:space:]]remote_vm_cookie,[[:space:]]*||1' | sed 's|[[:space:]]*}.$||1')


if [ -z "${remote_vm_cookie}" ]; then

	if [ -z "${vm_cookie}" ]; then

		echo " Error, no cookie defined for the remote US-Web host (remote_vm_cookie) nor for the base cookie (vm_cookie)." 1>&2

	else

		echo "No cookie defined for the remote US-Web host, using the base one (defined in us.config's vm_cookie): ${vm_cookie}."
		remote_vm_cookie="${vm_cookie}"

	fi

else

	if [ ! "$(echo "${remote_vm_cookie}" | wc -w)" = "1" ]; then

		echo " Error, invalid remote VM cookie obtained ('${remote_vm_cookie}'). Multiple 'remote_vm_cookie' configuration keys defined?" 1>&2

		exit 57

	fi

	#echo "Using cookie defined for the remote US-Web host (remote_vm_cookie): ${remote_vm_cookie}."

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


domain_opt=""

if [ -n "${target_domain}" ]; then
	domain_opt="--domain ${target_domain}"
fi


host_opt=""

if [ -n "${target_host}" ]; then
	host_opt="--host ${target_host}"
fi


# Any argument(s) specified to this script shall be interpreted as a plain,
# extra one:
#
make -s us_web_generate_report_exec EPMD_PORT=${erl_epmd_port} CMD_LINE_OPT="$* --config-file ${uw_cfg_file} --target-cookie ${remote_vm_cookie} ${domain_opt} ${host_opt}"
