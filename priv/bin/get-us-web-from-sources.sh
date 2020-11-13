#!/bin/sh

# Tells whether dependencies shall be built:
do_build=0

checkout_opt="--checkout"

# To avoid typos:
checkout_dir="_checkouts"

usage="
Usage: $(basename $0) [-h|--help]: clones and builds a fully functional US-Web environment in the current directory, then configures a test instance thereof, and runs it.
 Creates a basic installation where most dependencies are sibling directories of US-Web, symlinked in checkout directories.

The prerequisites expected to be already installed are:
 - Erlang/OTP (see http://myriad.esperide.org/#prerequisites)
 - rebar3 (see http://myriad.esperide.org/#getting-rebar3)
 - [optional] Awstats (see http://www.awstats.org/)

For the testing, no server shall already be running at TCP port #8080."

github_base="https://github.com/Olivier-Boudeville"



# Note that this mode of obtaining US-Web does not rely on rebar3 for US-Web
# itself, even if it used at least for some dependencies (ex: LEEC).
#
# This leads to duplications (ex: Myriad is built once in the context of LEEC
# and also once for the other packages).



if [ $# -eq 1 ]; then

	if [ "$1" = "-h" ] || [ "$1" = "--help" ]; then

		echo "${usage}"
		exit 0

	else

		echo "Error, invalid option specified ('$1')." 1>&2
		exit 3

	fi

	shift

fi

if [ ! $# -eq 0 ]; then

	echo "${usage}"

	exit 5

fi




# Checking first:

erlc=$(which erlc 2>/dev/null)

# No version checked:
if [ ! -x "${erlc}" ]; then

	echo "  Error, no Erlang compiler (erlc) found. Consider installing Erlang first, possibly thanks to our dedicated script, ${github_base}/Ceylan-Myriad/blob/master/conf/install-erlang.sh." 1>&2

	exit 10

fi


rebar3=$(which rebar3 2>/dev/null)

# No version checked either:
if [ ! -x "${rebar3}" ]; then

	echo "  Error, rebar3 not found. Consider installing it first, one may refer to http://myriad.esperide.org/#getting-rebar3." 1>&2

	exit 11

fi


git=$(which git 2>/dev/null)

if [ ! -x "${git}" ]; then

	echo "  Error, no 'git' tool found." 1>&2
	exit 18

fi


make=$(which make 2>/dev/null)

if [ ! -x "${make}" ]; then

	echo "  Error, no 'make' tool found." 1>&2
	exit 19

fi


config_dir="${HOME}/.config/universal-server"

us_config_filename="${config_dir}/us.config"

if [ -f "${us_config_filename}" ]; then

	echo "  Error, a prior US configuration file exists, '${us_config_filename}'. Not overwriting it with the test one, please remove it first." 1>&2

	exit 15

fi


us_web_config_filename="${config_dir}/us-web-for-tests.config"

if [ -f "${us_web_config_filename}" ]; then

	echo "  Error, a prior US-Web configuration file exists, '${us_web_config_filename}'. Not overwriting it, please remove it first." 1>&2

	exit 17

fi


base_install_dir="$(pwd)"

us_web_dir="${base_install_dir}/us_web"

prereq_install_dir="${base_install_dir}"


echo
echo "   Installing US-Web in ${base_install_dir}..."
echo



# First US-Web itself, so that any _checkouts directory can be created afterwards:
cd "${base_install_dir}"


clone_opts="--quiet"

echo "Getting the relevant repositories:"

echo " - cloning US-Web"

${git} clone ${clone_opts} ${github_base}/us-web us_web

if [ ! $? -eq 0 ]; then

	echo " Error, unable to obtain US-Web." 1>&2
	exit 40

fi


# The explicit build of Cowboy is needed due to a rebar3 bug encountered when
# building US-Web (see the comment in the 'building US-Web' section).

echo " - cloning Cowboy"

${git} clone ${clone_opts} git@github.com:ninenines/cowboy.git

if [ ! $? -eq 0 ]; then

	echo " Error, unable to obtain Cowboy." 1>&2
	exit 35

fi


echo " - cloning US-Common"

${git} clone ${clone_opts} ${github_base}/us-common us_common

if [ ! $? -eq 0 ]; then

	echo " Error, unable to obtain US-Common." 1>&2
	exit 35

fi



echo " - cloning LEEC (Ceylan fork of letsencrypt-erlang)"

${git} clone ${clone_opts} ${github_base}/letsencrypt-erlang leec

if [ ! $? -eq 0 ]; then

	echo " Error, unable to obtain LEEC (Let's Encrypt Erlang with Ceylan)." 1>&2
	exit 32

fi



echo " - cloning Ceylan-Traces"

${git} clone ${clone_opts} ${github_base}/Ceylan-Traces traces

if [ ! $? -eq 0 ]; then

	echo " Error, unable to obtain Ceylan-Traces." 1>&2
	exit 30

fi



echo " - cloning Ceylan-WOOPER"

${git} clone ${clone_opts} ${github_base}/Ceylan-WOOPER wooper

if [ ! $? -eq 0 ]; then

	echo " Error, unable to obtain Ceylan-WOOPER." 1>&2
	exit 25

fi


echo " - cloning Ceylan-Myriad"

${git} clone ${clone_opts} ${github_base}/Ceylan-Myriad myriad

if [ ! $? -eq 0 ]; then

	echo " Error, unable to obtain Ceylan-Myriad." 1>&2
	exit 20

fi


if [ ${do_build} -eq 0 ]; then

	echo
	echo "Building these packages:"

	# For Myriad, WOOPER and Traces, we prefer to rely on our own good old build
	# system (i.e. not on rebar3).

	echo " - building Ceylan-Myriad"
	cd myriad && ${make} all 1>/dev/null
	if [ ! $? -eq 0 ]; then
		echo " Error, the build of Ceylan-Myriad failed." 1>&2
		exit 50
	fi
	cd ..

	# Our build; uses Myriad's sibling tree:
	echo " - building Ceylan-WOOPER"
	cd wooper && ${make} all 1>/dev/null
	if [ ! $? -eq 0 ]; then
		echo " Error, the build of Ceylan-WOOPER failed." 1>&2
		exit 55
	fi
	cd ..

	# Our build; uses Myriad's and WOOPER's sibling trees:
	echo " - building Ceylan-Traces"
	cd traces && ${make} all 1>/dev/null
	if [ ! $? -eq 0 ]; then
		echo " Error, the build of Ceylan-Traces failed." 1>&2
		exit 60
	fi
	cd ..

	# Only for US-Web (not for prerequisites of LEEC):
	# (implies cowlib and ranch)
	#
	echo " - building Cowboy"
	cd cowboy && ${rebar3} compile 1>/dev/null
	if [ ! $? -eq 0 ]; then
		echo " Error, the build of Cowboy failed." 1>&2
		exit 65
	fi
	cd ..

	# Apart from Myriad (used as a checkout to point to the same, unique install
	# thereof here), LEEC has dependencies of its own (shotgun, jsx otherwise
	# jiffy, elli, getopt, yamerl, erlang_color), so, even if not all of them
	# are actually needed by our use case, we prefer relying on rebar3 (as
	# indirect dependencies, such as cowlib or gun, are also induced):
	#
	echo " - building LEEC"
	cd leec && mkdir ${checkout_dir} && cd ${checkout_dir} && ln -s ../../myriad && cd ..

	if [ ! $? -eq 0 ]; then
		echo " Error, the pre-build of LEEC failed." 1>&2
		exit 65
	fi

	# Relies on rebar3, so that prerequisites such as shotgun are managed):
	${make} all-rebar3 1>/dev/null
	if [ ! $? -eq 0 ]; then
		echo " Error, the build of LEEC failed." 1>&2
		exit 66
	fi

	# So that later US-Web is able to find the element of the 'leec'
	# application:
	#
	cd _build/default/lib/ && ln -s letsencrypt leec && cd leec/ebin && ln -s letsencrypt.app leec.app

	if [ ! $? -eq 0 ]; then
		echo " Error, the post-build of LEEC failed." 1>&2
		exit 67
	fi

	cd "${base_install_dir}"

	# US-Common does not introduce third-party dependencies, so going for our
	# native build, which thus uses Myriad's, WOOPER's and Traces' sibling
	# trees:
	#
	echo " - building US-Common"
	cd us_common && ${make} all 1>/dev/null
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
	# for US-Web either, which requires installing by ourselves cowboy):
	#
	echo " - building US-Web"
	cd us_web && mkdir ${checkout_dir} && cd ${checkout_dir} && ln -s ../../myriad && ln -s ../../wooper && ln -s ../../traces && ln -s ../../us_common && ln -s ../../leec && ln -s ../../cowboy && cd ..

	# Relies on rebar3:
	${make} all 1>/dev/null
	if [ ! $? -eq 0 ]; then
		echo " Error, the build of US-Web failed." 1>&2
		exit 75
	fi
	cd ..

fi



echo " - configuring a US-Web test instance"

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



cd ${us_web_dir}

echo

echo "   Building and launching a test US-Web native application"
${make} debug

#echo "   Building and launching a test US-Web release"
#${make} debug-as-release

res=$?

if [ $res -eq 0 ]; then

	echo " US-Web launched, please point a browser to http://localhost:8080 to check test sites."

else

	echo " Unable to build and launch the US-Web Server (error code: $res)." 1>&2

	exit $res

fi
