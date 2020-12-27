#!/bin/sh

# Note: unless specified, relying on the master branch for all clones.


# Deactivations useful for testing:

# Tells whether repositories shall be cloned (if not done already):
do_clone=0

# Tells whether dependencies shall be built (if not done already):
do_build=0

do_launch=0

no_launch_opt="--no-launch"

checkout_opt="--checkout"

# To avoid typos:
checkout_dir="_checkouts"
priv_dir="priv"

base_us_dir="/opt/universal-server"

# To be able to coexist with OTP releases (named as us_web-${archive_version});
# relative to base_us_dir:
#
native_install_dir="us_web-native"


usage="
Usage: $(basename $0) [-h|--help] [${no_launch_opt}] [BASE_US_DIR]: deploys (clones and builds) locally, as a normal user (sudo requested only whenever necessary), a fully functional US-Web environment natively (i.e. from its sources, not as an integrated OTP release) in the specified base directory (otherwise in the default '${base_us_dir}' directory), as '${native_install_dir}', then launches it (unless requested not to, with the '${no_launch_opt}' option).
 Creates a basic installation where most dependencies are sibling directories of US-Web, symlinked in checkout directories, so that code-level upgrades are easier to perform than in an OTP/rebar3 context.

The prerequisites expected to be already installed are:
 - Erlang/OTP (see http://myriad.esperide.org/#prerequisites)
 - rebar3, for the prerequisites that rely on it (see http://myriad.esperide.org/#getting-rebar3)
 - [optional] Awstats (see http://www.awstats.org/)
"
github_base="https://github.com/Olivier-Boudeville"


# See also the deploy-us-web-release.sh script to deploy OTP releases of US-Web
# instead (we prefer and better support the current mode of operation, though).



# Note that this mode of obtaining US-Web does not rely on rebar3 for US-Web
# itself, even if it used at least for some dependencies (ex: LEEC, so that its
# own dependencies are automatically managed).
#
# This could lead to duplications (ex: Myriad being built once in the context of
# LEEC and also once for the other packages), yet we use checkout directories to
# avoid that.

if [ $# -eq 1 ]; then

	if [ "$1" = "-h" ] || [ "$1" = "--help" ]; then

		echo "${usage}"
		exit 0

	elif [ "$1" = "${no_launch_opt}" ]; then

		echo "(auto-launch disabled)"
		do_launch=1

	else

		# If specified, must exist:
		base_us_dir="$1"
		if [ ! -d "${base_us_dir}" ]; then

			echo "  Error, specified installation directory, '${base_us_dir}', does not exist." 1>&2
			exit 4


		fi

	fi

	shift

fi

if [ ! $# -eq 0 ]; then

	echo "${usage}"

	exit 5

fi


# Just to avoid error messages if running from a non-existing directory:
cd /


# Checking first:

if [ $(id -u ) -eq 0 ]; then

	echo "  Error, this script must not be run as root (sudo will be requested only when necessary)." 1>&2
	exit 5

fi

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


make=$(which make 2>/dev/null)

if [ ! -x "${make}" ]; then

	echo "  Error, no 'make' tool found." 1>&2
	exit 19

fi

echo "Requesting sudoer rights for the operations that require it:"
sudo echo

base_us_dir_created=1

if [ ! -d "${base_us_dir}" ]; then

	echo " Creating US base directory '${base_us_dir}'."
	# Most permissive, will be updated later in the installation:
	sudo /bin/mkdir --mode=777 "${base_us_dir}"
	base_us_dir_created=0

fi


# Typically a release-like '/opt/universal-server/us_web-native', containing all
# dependencies:
#
abs_native_install_dir="${base_us_dir}/${native_install_dir}"

# The US-Web tree itself:
us_web_dir="${abs_native_install_dir}/us_web"


echo
echo "   Installing US-Web in '${abs_native_install_dir}'..."
echo



# First US-Web itself, so that any _checkouts directory can be created
# afterwards:

if [ $do_clone -eq 0 ]; then

	cd "${base_us_dir}"

	if [ -d "${native_install_dir}" ]; then

		echo "  Error, target installation directory, '${base_us_dir}/${native_install_dir}', already exists. Remove it first." 1>&2

		exit 20

	fi

	# Parent already exists by design; ensuring any normal user can write the
	# content to install next:
	#
	( sudo mkdir --mode=777 "${native_install_dir}" ) && cd "${native_install_dir}"

	clone_opts="--quiet"

	git=$(which git 2>/dev/null)

	if [ ! -x "${git}" ]; then

		echo "  Error, no 'git' tool found." 1>&2
		exit 18

	fi

	echo "Getting the relevant repositories (as $(id -un)):"


	echo " - cloning US-Web"

	${git} clone ${clone_opts} ${github_base}/us-web us_web

	if [ ! $? -eq 0 ]; then

		echo " Error, unable to obtain US-Web." 1>&2
	exit 40

	fi

	# A specific branch might be selected:
	#target_branch="certificate-support"
	target_branch="master"
	cd us_web && ${git} checkout ${target_branch} 1>/dev/null && cd ..
	if [ ! $? -eq 0 ]; then

		echo " Error, unable to switch to US-Web branch '${target_branch}'." 1>&2
		exit 45

	fi


	# The explicit build of Cowboy is needed due to a rebar3 bug encountered
	# when building US-Web (see the comment in the 'building US-Web' section).

	echo " - cloning Cowboy"

	#cowboy_git_id="git@github.com:ninenines/cowboy.git"
	cowboy_git_id="https://github.com/ninenines/cowboy.git"

	${git} clone ${clone_opts} ${cowboy_git_id}
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


	echo " - cloning Ceylan-LEEC"

	${git} clone ${clone_opts} ${github_base}/letsencrypt-erlang leec

	if [ ! $? -eq 0 ]; then

		echo " Error, unable to obtain Ceylan-LEEC." 1>&2
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

	ln -sf us_web/conf/GNUmakefile-for-native-root GNUmakefile

	# Back in ${base_us_dir}:
	cd ..

	# Designates this install as the latest one then.
	#
	# Rare option needed, otherwise apparently mistook for a directory resulting
	# in an incorrect link:
	#
	sudo /bin/ln -sf --no-target-directory "${native_install_dir}" us_web-latest

fi



if [ ${do_build} -eq 0 ]; then

	cd "${abs_native_install_dir}"

	echo
	echo "Building these packages (as $(id -un)):"

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
	# Note that rebar3 will still create a leec/_build/default/checkouts/myriad
	# directory with its own BEAMs, that may become obsolete. This directory
	# should simply remain out of the code path.
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
	cd ..

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

	# Post-install: fixing permissions and all.

	# This will also by useful for any next launch:

	# Not wanting the files of that US-Web install to remain owned by the
	# deploying user, so trying to apply a more proper user/group; for that we
	# have to determine them from the configuration files, and this to read and
	# use them:
	#
	# (will source in turn us-common.sh)
	us_web_common_script="${priv_dir}/bin/us-web-common.sh"

	if [ ! -f "${us_web_common_script}" ]; then

		echo "Error, unable to find us-web-common.sh script (not found in '${us_web_common_script}')." 1>&2
		exit 35

	fi

	# Hint for the helper scripts:
	us_launch_type="native"
	us_web_install_root="${us_web_dir}"

	#echo "Sourcing '${us_web_common_script}' from $(pwd)."
	. "${us_web_common_script}" #1>/dev/null

	read_us_config_file #1>/dev/null

	read_us_web_config_file #1>/dev/null


	# First permissions (chmod), then owner/group (chown):

	dir_perms="770"

	# abs_native_install_dir/* rather than only us_web_dir, as the dependencies
	# shall also have their permissions updated:

	echo " Changing the permissions of deployed roots in '${abs_native_install_dir}' to ${dir_perms}."
	if ! sudo chmod ${dir_perms} ${abs_native_install_dir}/*; then

		echo "Error, changing permissions of deployed roots in '${abs_native_install_dir}' to ${dir_perms} failed." 1>&2

		exit 40

	fi

	echo " Changing the permissions of deployed root '${abs_native_install_dir}' itself to ${dir_perms}."
	if ! sudo chmod ${dir_perms} ${abs_native_install_dir}; then

		echo "Error, changing permissions of deployed root '${abs_native_install_dir}' itself to ${dir_perms} failed." 1>&2

		exit 41

	fi

	if [ ${base_us_dir_created} -eq 0 ]; then

		echo " Changing the permissions of base US install directory '${base_us_dir}' to ${dir_perms}."

		if ! sudo chmod ${dir_perms} "${base_us_dir}"; then

			echo "Error, changing permissions of '${base_us_dir}' failed." 1>&2

			exit 42

		fi

	fi


	# Now owner/group; not leaving deployed content as initial user:

	if [ -n "${us_web_username}" ]; then

		chown_spec="${us_web_username}"

		if [ -n "${us_groupname}" ]; then
			chown_spec="${chown_spec}:${us_groupname}"
		fi

		echo " Changing recursively owner/group of all deployed elements as ${chown_spec} from '${abs_native_install_dir}'."
		if ! sudo chown --recursive "${chown_spec}" "${abs_native_install_dir}"; then

			echo "Error, changing recursively user/group owner (as ${chown_spec}) from '${abs_native_install_dir}' failed." 1>&2

			exit 45

		fi

		if [ ${base_us_dir_created} -eq 0 ]; then

			echo " Changing also the owner/group of base US install directory '${base_us_dir}' as '${chown_spec}'."

			# This one is not recursive:
			if ! sudo chown "${chown_spec}" "${base_us_dir}"; then

				echo "Error, changing owner/group owner of '${base_us_dir}' (as '${chown_spec}') failed." 1>&2

				exit 46

			fi

		fi

	fi


	echo
	echo "Native US-Web built and ready in ${abs_native_install_dir}."

fi


# Not checking specifically the expected US and US-Web configuration files:
# running US-Web will tell us whethe they exist and are legit.

echo


if [ $do_launch -eq 0 ]; then

	# Not expecting here a previous native instance to run.

	# Absolute path; typically in '/opt/universal-server/us_web-native/us_web':
	if [ ! -d "${us_web_dir}" ]; then

		echo " Error, the target US-Web directory, '${us_web_dir}', does not exist." 1>&2
		exit 75

	fi

	echo "   Running US-Web native application (as '$(id -un)' initially)"

	# Simplest: cd src && ${make} us_web_exec

	cd "${us_web_dir}" || exit 80


	# Actual cookie managed there:
	start_script="${priv_dir}/bin/start-us-web-native-build.sh"

	if [ ! -x "${start_script}" ]; then

		echo " Error, no start script found (no '${start_script}' found)." 1>&2
		exit 30

	fi


	cd "${base_us_dir}"

	# Automatic shutdown (that was deferred as much as possible) of any prior
	# US-Web release running:
	#
	for d in $(/bin/ls -d us_web-*.*.* 2>/dev/null) ; do

		exec="${d}/bin/us_web"

		echo "Testing for ${exec}..."

		if [ -x "${exec}" ]; then
			echo " Trying to stop gracefully any prior release in ${d}."
			sudo ${exec} stop 1>/dev/null 2>&1
		fi

	done

	echo "### Launching US-Web native build now"

	cd "${abs_native_install_dir}/us_web" || exit 81

	# Will switch to the US-Web configured user:
	sudo ${start_script}
	res=$?

	if [ $res -eq 0 ]; then

		echo " US-Web launched (start script reported success)."

	else
		echo " Error, start script ('${start_script}') failed (code: $res)." 1>&2

		exit $res

	fi

	#sleep 1

	# Maybe use get-us-web-native-build-status.sh in the future.


else

	echo "(no auto-launch enabled; one may execute, as root, 'systemctl restart us-web-as-native-build.service')"

fi
