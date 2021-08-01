# Common script facilities relating to the US-Web server.
#
# Allows to avoid code duplication. Meant to be sourced, not directly executed.
#
# Used for example by start-us-web-*.sh and stop-us-web-*.sh.


# The us_launch_type environment variable might have been set by the caller
# (typically to the "native" value) so that these helper scripts can locate more
# easily specific directories (ex: for the VM logs).


# We expect us_web_install_root to be already set by the caller:
if [ -z "${us_web_install_root}" ]; then

	echo "  Error, no us_web_install_root set by the caller." 1>&2
	exit 10

fi

if [ ! -d "${us_web_install_root}/priv" ]; then

	echo "  Error, invalid us_web_install_root ('$(realpath ${us_web_install_root})') set by the caller." 1>&2
	exit 11

fi


# Determining us_common_root:

us_web_script_root="${us_web_install_root}/priv/bin"
#echo "US-Web script root: ${us_web_script_root}"

us_common_root_in_checkouts="${us_web_script_root}/../../_checkouts/us_common"
us_common_root_in_build="${us_web_script_root}/../../_build/default/lib/us_common"
us_common_root_in_sibling="${us_web_script_root}/../../../us_common"

# To be evaluated from rel_root=/opt/universal-server/us_web-x.y.z (see
# deploy-us-web-release.sh):
#
us_common_in_deployed_release=$(/bin/ls -d -t lib/us_common-* 2>/dev/null | head -n 1)


if [ -d "${us_common_root_in_checkouts}" ]; then

	us_common_root="${us_common_root_in_checkouts}"

elif [ -d "${us_common_root_in_build}" ]; then

	us_common_root="${us_common_root_in_build}"

elif [ -d "${us_common_root_in_sibling}" ]; then

	us_common_root="${us_common_root_in_sibling}"

elif [ -d "${us_common_in_deployed_release}" ]; then

	us_common_root="${us_common_in_deployed_release}"

else

	echo "  Error, no US-Common root found: searched from $(pwd) as checkout '${us_common_root_in_checkouts}', as _build '${us_common_root_in_build}', as sibling '${us_common_root_in_sibling}' and as release '${us_common_in_deployed_release}'." 1>&2
	exit 95

fi

us_common_root="$(realpath ${us_common_root})"

echo "US-Common root found as '${us_common_root}'."

# Defined to avoid that us-common.sh sets paths like app or log relative to its
# own root:
#
us_actual_root="${us_web_install_root}"


# As depends on it:
us_common_script="${us_common_root}/priv/bin/us-common.sh"

if [ -f "${us_common_script}" ]; then

	. "${us_common_script}" 1>/dev/null

else

	echo "  Error, no US-Common script (${us_common_script}) found." 1>&2
	exit 100

fi



# Sets notably: us_web_config_file, us_web_username, us_web_app_base_dir,
# us_web_log_dir, us_web_rel_dir, us_web_exec.
#
# read_us_config_file must have been run beforehand.
#
read_us_web_config_file()
{

	us_web_config_filename=$(echo "${us_base_content}" | grep us_web_config_filename | sed 's|^[[:space:]]*{[[:space:]]*us_web_config_filename,[[:space:]]*"||1' | sed 's|"[[:space:]]*}.$||1')

	us_web_default_config_filename="us-web.config"

	if [ -z "${us_web_config_filename}" ]; then

		us_web_config_filename="${us_web_default_config_filename}"

		echo "No US-Web configuration filename specified, using default one, '${us_web_config_filename}'."

	else

		echo "Using specified US-Web configuration filename, '${us_web_config_filename}'."

	fi

	us_web_config_file="${base_path}/${app_dir}/${us_web_config_filename}"

	echo "Looking up '${us_web_config_file}'..."

	if [ ! -f "${us_web_config_file}" ]; then

		echo "  Error, US-Web configuration filename '${us_web_config_filename}' not found (looked up as '${us_web_config_file}')." 1>&2
		exit 110

	fi

	echo "Using US-Web configuration file '${us_web_config_file}'."


	# US-Web configuration data content, read once for all, with comments (%)
	# removed:
	#
	us_web_base_content=$(/bin/cat "${us_web_config_file}" | sed 's|^[[:space:]]*%.*||1')

	us_web_username=$(echo "${us_web_base_content}" | grep us_web_username | sed 's|^{[[:space:]]*us_web_username,[[:space:]]*"||1' | sed 's|"[[:space:]]*}.$||1')

	if [ -z "${us_web_username}" ]; then

		us_web_username="${USER}"

		if [ -z "${us_web_username}" ]; then

			echo "  Error, no USER environment variable set, whereas not username specified in configuration file." 1>&2
			exit 120

		fi

		echo "No web username specified, using current one, '${us_web_username}'."

	else

		echo "Using specified web username, '${us_web_username}'."

	fi

	#echo "us_web_username = $us_web_username"


	us_web_app_base_dir=$(echo "${us_web_base_content}" | grep us_web_app_base_dir | sed 's|^{[[:space:]]*us_web_app_base_dir,[[:space:]]*"||1' | sed 's|"[[:space:]]*}.$||1')

	if [ -z "${us_web_app_base_dir}" ]; then

		# Environment variable as last-resort:
		if [ -z "${US_WEB_APP_BASE_DIR}" ]; then

			if [ "${us_launch_type}" = "native" ]; then

				# As sourced from us_web directly:
				us_web_app_base_dir="$(pwd)"

				echo "No base directory specified for the US-Web application nor US_WEB_APP_BASE_DIR environment variable set, deriving it, in a native context, from the current directory, and trying '${us_web_app_base_dir}'."

			else

				# Wild guess:
				us_web_app_base_dir=$(/bin/ls -d ${us_app_base_dir}/../../*/us_web 2>/dev/null | xargs realpath)

				echo "No base directory specified for the US-Web application nor US_WEB_APP_BASE_DIR environment variable set, deriving it from the release-dependent US application one: trying '${us_web_app_base_dir}'."

			fi

		else

			us_web_app_base_dir="${US_WEB_APP_BASE_DIR}"
			echo "No base directory specified for the US-Web application, using the value of the US_WEB_APP_BASE_DIR environment variable, trying '${us_web_app_base_dir}'."

		fi


	else

		echo "Using the specified base directory for the US-Web application, '${us_web_app_base_dir}'."

	fi

	if [ ! -d "${us_web_app_base_dir}" ]; then

		echo "  Error, the base directory determined for the US-Web application, '${us_web_app_base_dir}', is not an existing directory." 1>&2
		exit 130

	fi

	us_web_data_dir=$(echo "${us_web_base_content}" | grep us_web_data_dir | sed 's|^{[[:space:]]*us_web_data_dir,[[:space:]]*"||1' | sed 's|"[[:space:]]*}.$||1')

	if [ -z "${us_web_data_dir}" ]; then

		# Maybe as an environment variable:
		if [ -z "${US_WEB_DATA_DIR}" ]; then

			# Then default is:
			us_web_data_dir="${us_web_app_base_dir}/us-web-data"

			if [ ! -d "${us_web_data_dir}" ]; then

				echo "Creating US-Web data directory '${us_web_data_dir}'."
				mkdir "${us_web_data_dir}"

			fi

		else

			us_web_data_dir="${US_WEB_DATA_DIR}"
			if [ ! -d "${us_web_data_dir}" ]; then

				echo "  Error, the US-Web data directory specified through the US_WEB_DATA_DIR environment variable (as '${us_web_data_dir}') is not an existing directory." 1>&2
				exit 135

			fi

		fi

	else

		if [ ! -d "${us_web_data_dir}" ]; then

			echo "  Error, the US-Web data directory specified in the US-Web configuration file (as '${us_web_data_dir}') is not an existing directory." 1>&2
			exit 140

		fi

	fi

	us_cert_dir="${us_web_data_dir}/certificates"


	# VM-level logs (not based on us_web_log_dir - which is dedicated to the
	# web-related ones):
	#
	# (note that us_web_vm_log_dir is for US-Web what us_log_dir is for
	# US-Common)
	#
	# (typically here in production mode, as a standard release, or as a sibling
	# native directory)
	#
	us_web_vm_log_dir="${us_web_app_base_dir}/log"

	if [ ! -d "${us_web_vm_log_dir}" ]; then

		saved_log_dir="${us_web_vm_log_dir}"

		# Maybe in development mode then (i.e. as a rebar3 build tree):
		us_web_vm_log_dir="${us_web_app_base_dir}/_build/default/rel/us_web/log"

		if [ ! -d "${us_web_vm_log_dir}" ]; then

			# Not an error per se, may happen for example when running a new
			# release:

			#echo "  Error, no US-Web VM log directory found: neither '${saved_log_dir}' (as a standard release) nor '${us_web_vm_log_dir}' (as a rebar3 build tree). Possibly a release not even started?" 1>&2
			#exit 140

			#echo "Warning: no US-Web VM log directory found, neither '${saved_log_dir}' (as a standard release) nor '${us_web_vm_log_dir}' (as a rebar3 build tree)."

			# Not finding this directory is normal, as in the context of a newly
			# deployed release, it would be created only when starting that
			# release; not creating it from here, as already done later, in
			# prepare_us_web_launch.
			#
			#us_web_vm_log_dir="LACKING_US_WEB_VM_LOG_DIR"
			us_web_vm_log_dir="${saved_log_dir}"

			# As a sibling native build is expected to have created it beforehand:
			echo "Will use, for US-Web VM log directory, '${us_web_vm_log_dir}' (not created yet), in the context of a standard OTP release."

		else

			echo "Rebar3 build tree detected, US-Web VM log directory found as '${us_web_vm_log_dir}'."

		fi

	else

		if [ "${us_launch_type}" = "native" ]; then

			echo "Native build installation detected, VM logs expected in '${us_web_vm_log_dir}'."

		else

			echo "Standard OTP release detected (most probably already launched at least once), US-Web VM log directory found as '${us_web_vm_log_dir}'."

		fi

	fi


	# If not in a native build (where no 'us_web_exec' applies), hunting down
	# the us_web executable:
	#
	if [ ! "${us_launch_type}" = "native" ]; then

		# Supposing first that the path of a real release has been specified;
		# for example: "/opt/universal-server/us-web-x.y.z":
		#
		us_web_rel_dir="${us_web_app_base_dir}"

		us_web_exec="${us_web_app_base_dir}/bin/us_web"

		if [ ! -x "${us_web_exec}" ]; then

			saved_exec="${us_web_exec}"

			# Maybe then in a rebar3 build tree:
			us_web_rel_dir="${us_web_app_base_dir}/_build/default/rel/us_web"

			us_web_exec="${us_web_rel_dir}/bin/us_web"

			if [ ! -x "${us_web_exec}" ]; then

				echo "  Error, the specified US-Web application base directory ('${us_web_app_base_dir}') does not seem to be a legit one: no '${saved_exec}' (not a standard release) nor '${us_web_exec}' (not a rebar3 build tree)." 1>&2
				exit 150

			else

				echo "Rebar3 build tree detected, US-Web application found as '${us_web_exec}'."
			fi

		else

			echo "Standard OTP release detected, US-Web application found as '${us_web_exec}'."

		fi

	fi


	us_web_log_dir=$(echo "${us_web_base_content}" | grep us_web_log_dir | sed 's|^{[[:space:]]*us_web_log_dir,[[:space:]]*"||1' | sed 's|"[[:space:]]*}.$||1')

	if [ -z "${us_web_log_dir}" ]; then

		us_web_log_dir="/var/log/universal-server/us-web"
		echo "No base directory specified for web logs (no 'us_web_log_dir' entry in the US-Web configuration file '${us_web_config_file}'), trying default log directory '${us_web_log_dir}'."

	else

		# Checks whether absolute:
		if [[ "${us_web_log_dir:0:1}" == / || "${us_web_log_dir:0:2}" == ~[/a-z] ]]; then

			echo "Using directly specified directory for web logs, '${us_web_log_dir}'."

		else

			# If it is relative, it is relative to the US-Web application base
			# directory:
			#
			us_web_log_dir="${us_web_app_base_dir}/${us_web_log_dir}"
			echo "Using specified directory for web logs (made absolute), '${us_web_log_dir}'."

		fi

	fi

	if [ ! -d "${us_web_log_dir}" ]; then

		echo "  Error, no US-Web log directory (for web-level logs) found ('${us_web_log_dir}')." 1>&2
		exit 160

	fi

	echo "US-Web (web-level) logs expected in the '${us_web_log_dir}' directory."

}



# Updates the relevant US-Web vm.args release file with any user-defined Erlang
# cookie the VM switched to.
#
# The relevant US configuration file must have been run beforehand (see
# read_us_config_file).
#
# See also: the reciprocal restore_us_web_config_cookie function.
#
update_us_web_config_cookie()
{

	if [ -n "${vm_cookie}" ]; then

		# Let's auto-generate on the fly a vm.args with the right runtime cookie
		# (as it was changed at startup):

		base_rel_cfg_dir="${us_web_app_base_dir}/releases/latest-release"

		if [ ! -d "${base_rel_cfg_dir}" ]; then

			echo "  Error, the base configuration directory for the release, '${base_rel_cfg_dir}' (obtained from '${us_web_app_base_dir}'), could not be found." 1>&2

			exit 170

		fi

		vm_args_file="${base_rel_cfg_dir}/vm.args"

		if [ ! -f "${vm_args_file}" ]; then

			echo " Error, the release vm.args file could not be found (searched for '${vm_args_file}')." 1>&2

			exit 180

		fi

		# The original VM args (typically including a safe, dummy cookie):
		original_vm_args_file="${base_rel_cfg_dir}/.vm.args.original"

		# Do not overwrite original information (ex: if update was run twice
		# with no restore in-between):
		#
		if [ ! -f "${original_vm_args_file}" ]; then

			/bin/mv -f "${vm_args_file}" "${original_vm_args_file}"

		else

			/bin/rm -f "${vm_args_file}"

		fi

		# So in all cases, here original_vm_args_file exists, and vm_args_file
		# not.

		# Avoid reading and writing in the same file:
		/bin/cat "${original_vm_args_file}" | /bin/sed "s|-setcookie.*|-setcookie ${vm_cookie}|1" > "${vm_args_file}"

		#/bin/cp -f "${vm_args_file}" "${base_rel_cfg_dir}/vm.args-for-inspection.txt"

		#echo "Content of newer vm.args:" 1>&2
		#/bin/cat "${vm_args_file}" 1>&2

		# Leaving as is original_vm_args_file, for any future use.

		echo "US-Web vm.args updated with cookie ${vm_cookie}."

		# So both files exist.

	else

		echo "(no cookie defined, no vm.args updated)"

	fi

}



# Restores the original VM args file, after it has been updated, to avoid
# leaking the actual runtime cookie in any future command-line.
#
# (reciprocal function of update_us_web_config_cookie)
#
restore_us_web_config_cookie()
{

	# Depends on whether a specific cookie had been defined:

	if [ -n "${vm_cookie}" ]; then

		if [ -z "${original_vm_args_file}" ]; then

			# No prior update_us_web_config_cookie call?
			echo "  Error, filename of original VM args not set (abnormal)." 1>&2

			exit 190

		else

			if [ -f "${original_vm_args_file}" ]; then

				/bin/mv -f "${original_vm_args_file}" "${vm_args_file}"

			fi

		fi

	fi

}



# Prepares a proper launch of US-Web.
#
# read_us_web_config_file must have been run beforehand.
#
prepare_us_web_launch()
{

	# Not wanting to look at older, potentially misleading logs:

	echo "Removing any logs in '${us_web_vm_log_dir}'."

	# Ensuring that directory exists:
	mkdir -p ${us_web_vm_log_dir}

	/bin/rm -f ${us_web_vm_log_dir}/erlang.log.* ${us_web_vm_log_dir}/run_erl.log 2>/dev/null

	# Avoiding warnings about keys being overwritten:

	echo "Removing any LEEC key files in '${us_cert_dir}'."

	# Covers two patterns of such files:
	/bin/rm -f ${us_cert_dir}/leec-agent-private*.key* 2>/dev/null

	# Not removing the state files of a log analyzer, to keep history.

	# Needed as a sign that any future start succeeded:
	trace_file="${us_web_vm_log_dir}/us_web.traces"
	echo "Removing any '${trace_file}' trace file."
	/bin/rm -f "${trace_file}" 2>/dev/null

	echo "Fixing permissions to ${us_web_username}:${us_groupname} (as $(id -un))."

	# So that the VM can write its logs despite authbind:
	chown ${us_web_username}:${us_groupname} ${us_web_vm_log_dir}

	# Raising the maximum number of opened file descriptors:
	ulimit -n 64000

}


# Inspects the VM logs of US-Web (beware of ancient entries being displayed).
#
# read_us_web_config_file must have been run beforehand.
#
inspect_us_web_log()
{

	# (run_erl.log not that useful)

	# A bit of waiting to let log file be created and written:
	sleep 1

	# See https://erlang.org/doc/embedded/embedded_solaris.html to understand
	# the naming logic of erlang.log.* files.
	#
	# The goal here is only to select the latest-produced of these rotated log
	# files:
	#
	us_web_vm_log_file=$(/bin/ls -t ${us_web_vm_log_dir}/erlang.log.* 2>/dev/null | head -n 1)
	# Apparently a log file may vanish/be replaced, so:
	attempts=1
	max_attempts=8

	while [ ! ${attempts} -eq ${max_attempts} ] && [ ! -f "${us_web_vm_log_file}" ]; do

		if [ -z "${us_web_vm_log_file}" ]; then
			echo "(no VM log file found, attempt ${attempts}/${max_attempts})"
		else
			# Might happen:
			echo "(VM log file '${us_web_vm_log_file}' not found, attempt ${attempts}/${max_attempts})"
		fi

		sleep 1
		attempts=$(expr ${attempts} + 1)
		us_web_vm_log_file=$(/bin/ls -t ${us_web_vm_log_dir}/erlang.log.* 2>/dev/null | head -n 1)

	done

	# A common problem is: "Protocol 'inet_tcp': the name xxx@yyy seems to be in
	# use by another Erlang node". This may not even be true (no VM running),
	# just a lingering EPMD believing this node still exists.

	echo

	if [ -f "${us_web_vm_log_file}" ]; then

		echo "EPMD names output:"
		epmd -port ${erl_epmd_port} -names

		echo
		echo "Displaying the end of '${us_web_vm_log_file}':"

		# A sufficient height is *necessary*:
		tail --lines=80 "${us_web_vm_log_file}"

		# Extra information might be gathered from traces_via_otp.traces and/or
		# us_web.traces.

	else

		echo "No VM log file found, aborting." 1>&2
		exit 200

	fi

}
