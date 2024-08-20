#!/bin/sh

usage="
Usage: $(basename $0) [-h|--help] [--configure-test] [--run-test]: clones and builds from scratch a fully functional US-Web environment in the current directory; then, if enabled: configures a test instance thereof, and runs it.

Creates a full installation where most dependencies are sibling directories of US-Web, symlinked in checkout directories, so that code-level upgrades are easier to perform than in an OTP/rebar3 context.

The prerequisites expected to be already installed are:
 - Erlang/OTP (see http://myriad.esperide.org/#prerequisites)
 - rebar3 (see http://myriad.esperide.org/#getting-rebar3), for dependencies that are not ours
 - [optional] Awstats (see http://www.awstats.org/)

If the execution of a test instance is enabled, no server shall already be running at TCP port #8080."

# See also deploy-us-web-native-build.sh to install US-Web for production.


# Tells whether dependencies shall be fetched (downloaded/cloned):
do_fetch=0

# Tells whether dependencies shall be built:
do_build=0

# Tells whether a test US-Web install shall be configured:
do_configure_test=1

# Tells whether a previously configured US-Web install shall be auto-run:
do_run_test=1


token_eaten=0

while [ $token_eaten -eq 0 ]; do

	token_eaten=1

	if [ "$1" = "--configure-test" ]; then
		shift
		do_configure_test=0
		token_eaten=0
	fi

	if [ "$1" = "--run-test" ]; then
		shift
		do_run_test=0
		token_eaten=0
	fi

	if [ "$1" = "-h" ] || [ "$1" = "--help" ]; then
		echo "${usage}"
		exit
		token_eaten=0
	fi

done


if [ ! $# -eq 0 ]; then

	echo "  Error, unexpected argument(s): '$*'.
${usage}" 1>&2
	exit 4

fi

# Applies activity dependencies:
if [ $do_run_test -eq 0 ]; then

	do_configure_test=0

fi

if [ $do_configure_test -eq 0 ]; then

	do_build=0

fi

if [ $do_build -eq 0 ]; then

	do_fetch=0

fi


checkout_opt="--checkout"

# To avoid typos:
checkout_dir="_checkouts"


github_base="https://github.com/Olivier-Boudeville"



# Note that this mode of obtaining US-Web does not rely on rebar3 for US-Web
# itself, even if it used at least for some dependencies (e.g. LEEC).
#
# This does not lead to duplications (e.g. Myriad being built once in the
# context of LEEC and also once for the other packages), thanks to _checkouts
# containing symlinks.





# Checking first, prior to any action:


erlc="$(which erlc 2>/dev/null)"

# No version checked:
if [ ! -x "${erlc}" ]; then

	echo "  Error, no Erlang compiler (erlc) found. Consider installing Erlang first, possibly thanks to our dedicated script, ${github_base}/Ceylan-Myriad/blob/master/conf/install-erlang.sh." 1>&2

	exit 10

fi


rebar3="$(which rebar3 2>/dev/null)"

# No version checked either:
if [ ! -x "${rebar3}" ]; then

	echo "  Error, rebar3 not found. Consider installing it first, one may refer to http://myriad.esperide.org/#getting-rebar3." 1>&2

	exit 11

fi


if [ $do_fetch -eq 0 ]; then

	git="$(which git 2>/dev/null)"

	if [ ! -x "${git}" ]; then

		echo "  Error, no 'git' tool found." 1>&2
		exit 18

	fi

fi


make="$(which make 2>/dev/null)"

if [ ! -x "${make}" ]; then

	echo "  Error, no 'make' tool found." 1>&2
	exit 19

fi


if [ $do_fetch -eq 0 ]; then

	for d in us-web cowboy us-common letsencrypt-erlang traces wooper myriad; do

		if [ -d "${d}" ]; then

			echo "  Error, a '${d}' directory exists already, remove it first." 1>&2
			exit 26

		fi

	done

fi


if [ $do_configure_test -eq 0 ]; then

	config_dir="${HOME}/.config/universal-server"

	us_config_filename="${config_dir}/us.config"

	if [ -f "${us_config_filename}" ]; then

		echo "  Error, a prior US configuration file exists, '${us_config_filename}'. Not overwriting it with the test one, please remove it first." 1>&2

		exit 15

	fi


	us_web_config_filename="${config_dir}/us-web-for-tests.config"

	if [ -f "${us_web_config_filename}" ]; then

		echo "  Error, a prior test US-Web configuration file exists, '${us_web_config_filename}'. Not overwriting it, please remove it first." 1>&2

		exit 17

	fi

fi


base_install_dir="$(realpath $(pwd))"

us_web_dir="${base_install_dir}/us_web"

prereq_install_dir="${base_install_dir}"

log_file="${base_install_dir}/us-web-install.log"

if [ -f "${log_file}" ]; then

	/bin/rm -f "${log_file}"

fi

echo
echo "   Installing US-Web in ${base_install_dir}..." | tee --append "${log_file}"
echo "   (install log in ${log_file})" | tee --append "${log_file}"
echo | tee --append "${log_file}"




if [ $do_fetch -eq 0 ]; then

	# First US-Web itself, so that any _checkouts directory can be created
	# afterwards:
	#
	cd "${base_install_dir}"


	clone_opts="--quiet"

	echo "Fetching the relevant repositories:" | tee --append "${log_file}"

	echo " - cloning US-Web"

	${git} clone ${clone_opts} ${github_base}/us-web us_web 1>>"${log_file}" 2>&1

	if [ ! $? -eq 0 ]; then

		echo " Error, unable to obtain US-Web." 1>&2
		exit 40

	fi


   # The explicit build of Cowboy is needed due to a rebar3 bug encountered when
   # building US-Web (see the comment in the 'building US-Web' section).

	echo " - cloning Cowboy" | tee --append "${log_file}"

	#cowboy_git_id="git@github.com:ninenines/cowboy.git"
	cowboy_git_id="https://github.com/ninenines/cowboy.git"

	${git} clone ${clone_opts} ${cowboy_git_id} 1>>"${log_file}" 2>&1

	if [ ! $? -eq 0 ]; then

		echo " Error, unable to obtain Cowboy." 1>&2
		exit 35

	fi

	# A lot safer than relying on the tip of the master branch:
	cowboy_tag="2.8.0"

	if [ -n "${cowboy_tag}" ]; then

		echo " - setting Cowboy to tag '${cowboy_tag}'"

		cd cowboy
		${git} checkout tags/${cowboy_tag}
		if [ ! $? -eq 0 ]; then

			echo " Error, unable to set Cowboy to tag '${cowboy_tag}'." 1>&2
			exit 36

		fi

		cd ..

	fi


	echo " - cloning US-Common" | tee --append "${log_file}"

	${git} clone ${clone_opts} ${github_base}/us-common us_common 1>>"${log_file}" 2>&1

	if [ ! $? -eq 0 ]; then

		echo " Error, unable to obtain US-Common." 1>&2
		exit 35

	fi



	echo " - cloning LEEC (Ceylan fork of letsencrypt-erlang)" | tee --append "${log_file}"

	${git} clone ${clone_opts} ${github_base}/letsencrypt-erlang leec 1>>"${log_file}" 2>&1

	if [ ! $? -eq 0 ]; then

		echo " Error, unable to obtain LEEC (Let's Encrypt Erlang with Ceylan)." 1>&2
		exit 32

	fi



	echo " - cloning Ceylan-Traces" | tee --append "${log_file}"

	${git} clone ${clone_opts} ${github_base}/Ceylan-Traces traces 1>>"${log_file}" 2>&1

	if [ ! $? -eq 0 ]; then

		echo " Error, unable to obtain Ceylan-Traces." 1>&2
		exit 30

	fi



	echo " - cloning Ceylan-WOOPER" | tee --append "${log_file}"

	${git} clone ${clone_opts} ${github_base}/Ceylan-WOOPER wooper 1>>"${log_file}" 2>&1

	if [ ! $? -eq 0 ]; then

		echo " Error, unable to obtain Ceylan-WOOPER." 1>&2
		exit 25

	fi


	echo " - cloning Ceylan-Myriad" | tee --append "${log_file}"

	${git} clone ${clone_opts} ${github_base}/Ceylan-Myriad myriad 1>>"${log_file}" 2>&1

	if [ ! $? -eq 0 ]; then

		echo " Error, unable to obtain Ceylan-Myriad." 1>&2
		exit 20

	fi

fi


if [ ${do_build} -eq 0 ]; then

	cd "${base_install_dir}"

	echo
	echo "Building all packages for US-Web:" | tee --append "${log_file}"

	# For Myriad, WOOPER and Traces, we prefer relying on our own good old build
	# system (i.e. not on rebar3).

	echo " - building Ceylan-Myriad" | tee --append "${log_file}"
	cd myriad && ${make} all 1>>"${log_file}" 2>&1
	if [ ! $? -eq 0 ]; then
		echo " Error, the build of Ceylan-Myriad failed." 1>&2
		exit 50
	fi
	cd ..

	# Our build; uses Myriad's sibling tree:
	echo " - building Ceylan-WOOPER" | tee --append "${log_file}"
	cd wooper && ${make} all 1>>"${log_file}" 2>&1
	if [ ! $? -eq 0 ]; then
		echo " Error, the build of Ceylan-WOOPER failed." 1>&2
		exit 55
	fi
	cd ..

	# Our build; uses Myriad's and WOOPER's sibling trees:
	echo " - building Ceylan-Traces" | tee --append "${log_file}"
	cd traces && ${make} all 1>>"${log_file}" 2>&1
	if [ ! $? -eq 0 ]; then
		echo " Error, the build of Ceylan-Traces failed." 1>&2
		exit 60
	fi
	cd ..

	# Only for US-Web (not for the past prerequisites of LEEC):
	# (implies cowlib and ranch)
	#
	echo " - building Cowboy" | tee --append "${log_file}"
	cd cowboy && ${rebar3} compile 1>>"${log_file}" 2>&1
	if [ ! $? -eq 0 ]; then
		echo " Error, the build of Cowboy failed." 1>&2
		exit 65
	fi
	cd ..

	# Apart from Myriad (used as a checkout to point to the same, unique install
	# thereof here), LEEC has dependencies of its own (jsx otherwise jiffy,
	# shotgun, elli, getopt, yamerl, erlang_color), so, even if not all of them
	# are actually needed by our use case (making use only on jsx), we prefer
	# relying on rebar3 (it used to be convenient when indirect dependencies
	# such as cowlib or gun were also induced):
	#
	echo " - building LEEC" | tee --append "${log_file}"
	cd leec && mkdir ${checkout_dir} && cd ${checkout_dir} && ln -s ../../myriad && cd ..

	if [ ! $? -eq 0 ]; then
		echo " Error, the pre-build of LEEC failed." 1>&2
		exit 65
	fi

	# Relies on rebar3, so that prerequisites such as shotgun are managed):
	${make} all-rebar3 1>>"${log_file}" 2>&1
	if [ ! $? -eq 0 ]; then
		echo " Error, the build of LEEC failed." 1>&2
		exit 66
	fi

	cd "${base_install_dir}"

	# US-Common does not introduce third-party dependencies, so going for our
	# native build, which thus uses Myriad's, WOOPER's and Traces' sibling
	# trees:
	#
	echo " - building US-Common" | tee --append "${log_file}"
	cd us_common && ${make} all 1>>"${log_file}" 2>&1
	if [ ! $? -eq 0 ]; then
		echo " Error, the build of US-Common failed." 1>&2
		exit 70
	fi
	cd ..

	# US-Web has for external dependencies only cowboy; we used to rely on a
	# rebar3-based build here, and thus checkouts for most of our libraries,
	# yet, due to a rebar3 bug, Traces was attempted to be built again, which
	# failed as the WOOPER parse transform was not found, as WOOPER had not been
	# built before - in spite of the dependency; so now we are not using rebar3
	# for US-Web either, which requires installing by ourselves cowboy:
	#
	echo " - building US-Web" | tee --append "${log_file}"
	cd us_web && mkdir ${checkout_dir} && cd ${checkout_dir} && ln -s ../../myriad && ln -s ../../wooper && ln -s ../../traces && ln -s ../../us_common && ln -s ../../leec && ln -s ../../cowboy && cd ..

	# Our build; uses Ceylan's sibling trees:
	${make} all 1>>"${log_file}" 2>&1
	if [ ! $? -eq 0 ]; then
		echo " Error, the build of US-Web failed." 1>&2
		exit 75
	fi
	cd ..

fi


if [ $do_configure_test -eq 0 ]; then

	echo " - configuring a US-Web test instance" | tee --append "${log_file}"

	if [ ! -d "${config_dir}" ]; then

		mkdir "${config_dir}"

	fi

	cd "${config_dir}"


	us_common_dir="${prereq_install_dir}/us_common"

	us_cfg_for_test="${us_common_dir}/priv/for-testing/us.config"

	if [ ! -f "${us_cfg_for_test}" ]; then

		echo " Error, '${us_cfg_for_test}' not found." 1>&2
		exit 70

	fi

	# Already checked that does not exist:
	/bin/ln -s "${us_cfg_for_test}"


	us_web_cfg_for_test="${us_web_dir}/priv/for-testing/us-web-for-tests.config"

	if [ ! -f "${us_web_cfg_for_test}" ]; then

		echo " Error, '${us_web_cfg_for_test}' not found." 1>&2
		exit 75

	fi

	# Already checked that does not exist:
	/bin/ln -s "${us_web_cfg_for_test}"

fi


if [ $do_run_test -eq 0 ]; then

	cd "${us_web_dir}"

	echo

	echo "   Building and launching a test US-Web native application" | tee --append "${log_file}"
	${make} debug
	#${make} debug-as-release

	res=$?

	if [ $res -eq 0 ]; then

		echo " US-Web launched, please point a browser to http://localhost:8080 to check test sites." | tee --append "${log_file}"

	else

		echo " Unable to build and launch the US-Web Server (error code: $res)." 1>&2

		exit $res

	fi

fi
