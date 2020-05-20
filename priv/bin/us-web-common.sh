# Common script facilities relating to the us-web server.
#
# Allows to avoid code duplication. Meant to be sourced, not directly executed.
#
# Used for example by start-us-web.sh and stop-us-web.sh.


# Determining us_common_root:

us_web_script_root=$(dirname $0)
echo "US-Web script root: ${us_web_script_root}"

us_common_root_in_checkouts="${us_web_script_root}/../../_checkouts/us_common"
us_common_root_in_build="${us_web_script_root}/../../_build/default/lib/us_common"

if [ -d "${us_common_root_in_checkouts}" ] ; then

	us_common_root="${us_common_root_in_checkouts}"

else

	if [ -d "${us_common_root_in_build}" ] ; then

		us_common_root="${us_common_root_in_build}"

	else

			echo "  Error, no us-common root found (neither ${us_common_root_in_checkouts} nor ${us_common_root_in_build}) found." 1>&2
			exit 95

	fi

fi

# As depends on it:
us_common_script="${us_common_root}/priv/bin/us-common.sh"

if [ -f "${us_common_script}" ] ; then

	. "${us_common_script}"

else

	echo "  Error, no us-common script (${us_common_script}) found." 1>&2
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

	if [ -z "${us_web_config_filename}" ] ; then

		us_web_config_filename="${us_web_default_config_filename}"

		echo "No us-web configuration filename specified, using default one, '${us_web_config_filename}'."

	else

		echo "Using specified us-web configuration filename, '${us_web_config_filename}'."

	fi

	us_web_config_file="${base_path}/${app_dir}/${us_web_config_filename}"

	echo "Looking up '${us_web_config_file}'..."

	if [ ! -f "${us_web_config_file}" ] ; then

		echo "  Error, us-web configuration filename '${us_web_config_filename}' not found." 1>&2
		exit 110

	fi

	echo "Using us-web configuration file '${us_web_config_file}'."


	# us-web configuration data content, read once for all, with comments (%)
	# removed:
	#
	us_web_base_content=$(/bin/cat "${us_web_config_file}" | sed 's|^[[:space:]]*%.*||1')

	us_web_username=$(echo "${us_web_base_content}" | grep us_web_username | sed 's|^{[[:space:]]*us_web_username,[[:space:]]*"||1' | sed 's|"[[:space:]]*}.$||1')

	if [ -z "${us_web_username}" ] ; then

		us_web_username="${USER}"

		if [ -z "${us_web_username}" ] ; then

			echo " Error, no USER environment variable set, whereas not username specified in configuration file." 1>&2
			exit 120

		fi

		echo "No web username specified, using current one, '${us_web_username}'."

	else

		echo "Using specified web username, '${us_web_username}'."

	fi

	#echo "us_web_username = $us_web_username"


	us_web_app_base_dir=$(echo "${us_web_base_content}" | grep us_web_app_base_dir | sed 's|^{[[:space:]]*us_web_app_base_dir,[[:space:]]*"||1' | sed 's|"[[:space:]]*}.$||1')

	if [ -z "${us_web_app_base_dir}" ] ; then

		# Environment variable as last-resort:
		if [ -z "${US_WEB_APP_BASE_DIR}" ] ; then

			# Wild guess:
			us_web_app_base_dir=$(/bin/ls -d ${us_app_base_dir}/../../*/us_web 2>/dev/null | xargs realpath)

			echo "No base directory specified for the us-web application nor US_WEB_APP_BASE_DIR environment variable set, deriving it from the US application one: trying '${us_web_app_base_dir}'."

		else

			us_web_app_base_dir="${US_WEB_APP_BASE_DIR}"
			echo "No base directory specified for the us-web application, using the value of the US_WEB_APP_BASE_DIR environment variable, trying '${us_web_app_base_dir}'."

		fi


	else

		echo "Using the specified base directory for the us-web application, '${us_web_app_base_dir}'."

	fi

	if [ ! -d "${us_web_app_base_dir}" ] ; then

		echo "  Error, the base directory determined for the us-web application, '${us_web_app_base_dir}', is not an existing directory." 1>&2
		exit 130

	fi


	# VM-level logs (not based on us_web_log_dir - which is dedicated to the
	# web-related ones):
	#
	# (note that us_web_vm_log_dir is for us-web what us_log_dir is for
	# us-common)
	#
	# (typically here in production mode, as a standard release)
	#
	us_web_vm_log_dir="${us_web_app_base_dir}/log"

	if [ ! -d "${us_web_vm_log_dir}" ] ; then

		saved_log_dir="${us_web_vm_log_dir}"

		# Maybe in development mode then (i.e. as a rebar3 build tree):
		us_web_vm_log_dir="${us_web_app_base_dir}/_build/default/rel/us_web/log"

		if [ ! -d "${us_web_vm_log_dir}" ] ; then

			# Not an error per se, may happen for example when running a new
			# release:

			#echo "  Error, no us-web VM log directory found: neither '${saved_log_dir}' (as a standard release) nor '${us_web_vm_log_dir}' (as a rebar3 build tree). Possibly a release not even started?" 1>&2
			#exit 140

			echo "No us-web VM log directory found, neither '${saved_log_dir}' (as a standard release) nor '${us_web_vm_log_dir}' (as a rebar3 build tree)."

		else

			echo "Rebar3 build tree detected, us-web VM log directory found as '${us_web_vm_log_dir}'."
		fi

	else

		echo "Standard OTP release detected, us-web VM log directory found as '${us_web_vm_log_dir}'."

	fi

	us_web_vm_log_file="${us_web_vm_log_dir}/erlang.log.1"


	# Supposing first the path of a real release having been specified; for
	# example: "/opt/universal-server/us-web-x.y.z":
	#
	us_web_rel_dir="${us_web_app_base_dir}"

	us_web_exec="${us_web_app_base_dir}/bin/us_web"

	if [ ! -x "${us_web_exec}" ] ; then

		saved_exec="${us_web_exec}"

		# Maybe then in a rebar3 build tree:
		us_web_rel_dir="${us_web_app_base_dir}/_build/default/rel/us_web"

		us_web_exec="${us_web_rel_dir}/bin/us_web"

		if [ ! -x "${us_web_exec}" ] ; then

			echo "  Error, the specified us-web application base directory ('${us_web_app_base_dir}') does not seem to be a legit one: no '${saved_exec}' (not a standard release) nor '${us_web_exec}' (not a rebar3 build tree)." 1>&2
			exit 150

		else

			echo "Rebar3 build tree detected, us-web application found as '${us_web_exec}'."
		fi

	else

		echo "Standard OTP release detected, us-web application found as '${us_web_exec}'."

	fi


	us_web_log_dir=$(echo "${us_web_base_content}" | grep us_web_log_dir | sed 's|^{[[:space:]]*us_web_log_dir,[[:space:]]*"||1' | sed 's|"[[:space:]]*}.$||1')

	if [ -z "${us_web_log_dir}" ] ; then

		us_web_log_dir="/var/log"
		echo "No base directory specified for web logs (no 'us_web_log_dir' entry in the us-web configuration file '${us_web_config_file}'), trying default log directory '${us_web_log_dir}'."

	else

		# Checks whether absolute:
		if [[ "${us_web_log_dir:0:1}" == / || "${us_web_log_dir:0:2}" == ~[/a-z] ]] ; then

			echo "Using directly specified directory for web logs, '${us_web_log_dir}'."

		else

			# If it is relative, it is relative to the us-web application base
			# directory:
			#
			us_web_log_dir="${us_web_app_base_dir}/${us_web_log_dir}"
			echo "Using specified directory for web logs (made absolute), '${us_web_log_dir}'."

		fi

	fi

	if [ ! -d "${us_web_log_dir}" ] ; then

		echo "  Error, no us-web log directory (for web-level logs) found ('${us_web_log_dir}')." 1>&2
		exit 160

	fi

	echo "us-web (web-level) logs expected in the '${us_web_log_dir}' directory."

}



# Updates the relevant us-web vm.args release file with any user-defined Erlang
# cookie the VM switched to.
#
# The relevant US configuration file must have been run beforehand (see
# read_us_config_file).
#
# See also: the reciprocal restore_us_web_config_cookie function.
#
update_us_web_config_cookie()
{

	if [ -n "${vm_cookie}" ] ; then

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
		if [ ! -f "${original_vm_args_file}" ] ; then

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

		echo "us-web vm.args updated with cookie ${vm_cookie}."

		# So both files exist.

	else

		echo "(no cookie defined, no vm.args updated)"

	fi

}



# Restores the original VM args file, after it has been updated, to avoid
# leaking the actual runtime cookie in any future command-line.
#
# (reciprocal fucntion of update_us_web_config_cookie)
#
restore_us_web_config_cookie()
{

	# Depends on whether a specific cookie had been defined:

	if [ -n "${vm_cookie}" ] ; then

		if [ -z "${original_vm_args_file}" ] ; then

			# No prior update_us_web_config_cookie call?
			echo "  Error, filename of original VM args not set (abnormal)." 1>&2

			exit 190

		else

			if [ -f "${original_vm_args_file}" ] ; then

				/bin/mv -f "${original_vm_args_file}" "${vm_args_file}"

			fi

		fi

	fi

}



# Prepares a proper launch of us-web.
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

	# Needed as a sign that any future start succeeded:
	trace_file="${us_log_dir}/us_web.traces"
	echo "Removing any '${trace_file}' trace file."
	/bin/rm -f "${trace_file}" 2>/dev/null

	echo "Fixing permissions."

	# So that the VM can write its logs despite authbind:
	chown ${us_web_username}:${us_groupname} ${us_web_vm_log_dir}

}


# Inspects the VM logs of us-web (beware of ancient entries being displayed).
#
# read_us_web_config_file must have been run beforehand.
#
inspect_us_web_log()
{

	# (run_erl.log not that useful)

	# A common problem is: "Protocol 'inet_tcp': the name xxx@yyy seems to be in
	# use by another Erlang node". This may not even be true (no VM running),
	# just a lingering EPMD believing this node still exists.


	# Waits a bit if necessary while any writing takes place:
	if [ ! -f "${us_web_vm_log_file}" ] ; then
		sleep 1
	fi

	echo
	if [ -f "${us_web_vm_log_file}" ] ; then

		echo "EPMD names output:"
		epmd -port ${erl_epmd_port} -names

		echo
		echo "Displaying the end of '${us_web_vm_log_file}':"

		# Still a bit of waiting, otherwise any error may not have been reported
		# yet:
		#
		sleep 1

		# A sufficient height is *necessary*:
		tail --lines=80 "${us_web_vm_log_file}"

	else

		echo " Error, no us-web VM log file found (no '${us_web_vm_log_file}')." 1>&2
		exit 200

	fi

}
