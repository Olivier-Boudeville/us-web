#!/bin/sh

# Note: relying on the master branch for all clones.


# Deactivations useful for testing:

# Tells whether repositories shall be cloned:
do_clone=1

# Tells whether dependencies shall be built:
do_build=1

do_launch=0


checkout_opt="--checkout"

# To avoid typos:
checkout_dir="_checkouts"

usage="
Usage: $(basename $0) [-h|--help] [INSTALL_DIR]: clones and builds a fully functional US-Web environment in the specified directory (otherwise in the current one), then configures an instance thereof, and runs it.
 Creates a basic installation where most dependencies are sibling directories of US-Web, symlinked in checkout directories.

The prerequisites expected to be already installed are:
 - Erlang/OTP (see http://myriad.esperide.org/#prerequisites)
 - rebar3 (see http://myriad.esperide.org/#getting-rebar3)
 - [optional] Awstats (see http://www.awstats.org/)
"
github_base="https://github.com/Olivier-Boudeville"



# Note that this mode of obtaining US-Web does not rely on rebar3 for US-Web
# itself, even if it used at least for some dependencies (ex: LEEC, so that its
# own dependencies are automatically managed).
#
# This leads to duplications (ex: Myriad is built once in the context of LEEC
# and also once for the other packages).

if [ $# -eq 0 ]; then

	base_install_dir="$(pwd)"

elif [ $# -eq 1 ]; then

	if [ "$1" = "-h" ] || [ "$1" = "--help" ]; then

		echo "${usage}"
		exit 0

	else

		base_install_dir="$1"
		if [ ! -d "${install_dir}" ]; then

			echo "  Error, specified installation directory, '${install_dir}', does not exist." 1>&2
			exit 4


		fi

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



echo
echo "   Installing US-Web in '${base_install_dir}'..."
echo


cd "${base_install_dir}"

# First US-Web itself, so that any _checkouts directory can be created afterwards:

clone_opts="--quiet"

if [ $do_clone -eq 0 ]; then

	echo "Getting the relevant repositories:"


	echo " - cloning US-Web"

	# A specific branch might be selected.

	${git} clone ${clone_opts} ${github_base}/us-web us_web

	if [ ! $? -eq 0 ]; then

		echo " Error, unable to obtain US-Web." 1>&2
	exit 40

	fi


	# The explicit build of Cowboy is needed due to a rebar3 bug encountered
	# when building US-Web (see the comment in the 'building US-Web' section).

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
	# are actually needed by our use case (elli, getopt, yamerl, erlang_color of
	# no use here), we prefer relying on rebar3 (as indirect dependencies, such
	# as cowlib or gun, are also induced):
	#
	echo " - building LEEC"

	# Modifying LEEC so that it relies on the same, common Myriad build tree (as
	# a checkout) rather than on its own version (as a _build dependency;
	# removed to avoid a possibly cause of confusion):
	#
	cd leec && /bin/rm -rf _build/default/lib/myriad/ && mkdir ${checkout_dir} && cd ${checkout_dir} && ln -s ../../myriad && cd ..

	if [ ! $? -eq 0 ]; then
		echo " Error, the pre-build of LEEC failed." 1>&2
		exit 65
	fi

	# Relies on rebar3, so that prerequisites such as shotgun are managed:
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

	# US-Common does not introduce third-party dependencies, so going again for
	# our native build, which thus uses Myriad's, WOOPER's and Traces' sibling
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
	# yet, due to a rebar3 bug (probably in the management of checkouts), Traces
	# was attempted to be built again, which failed as the WOOPER parse
	# transform was not found, as WOOPER had not been built before - in spite of
	# the dependency; so now we are not using rebar3 for US-Web either, which
	# required installing cowboy by ourselves):
	#
	echo " - building US-Web"
	cd us_web && mkdir ${checkout_dir} && cd ${checkout_dir} && ln -s ../../myriad && ln -s ../../wooper && ln -s ../../traces && ln -s ../../us_common && ln -s ../../leec && ln -s ../../cowboy && cd ..

	# Our build; uses Ceylan's sibling trees:
	${make} all 1>/dev/null
	if [ ! $? -eq 0 ]; then
		echo " Error, the build of US-Web failed." 1>&2
		exit 75
	fi
	cd ..

fi


# Not checking specifically the expected US and US-Web configuration files:
# running US-Web will tell us whethe they exist and are legit.

echo


if [ $do_launch -eq 0 ]; then

	echo "   Running US-Web native application"
	cd us_web/src && ${make} us_web_exec

	res=$?

	if [ $res -eq 0 ]; then

		echo " US-Web launched, please point a browser to http://localhost:8080 to check test sites."

	else

		echo " Unable to build and launch the US-Web Server (error code: $res)." 1>&2

		exit $res

	fi

fi
