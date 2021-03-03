#!/bin/sh

# See also:
# - the get-us-web-from-sources.sh script to install a test version of US-Web
# - the deploy-us-web-release.sh script to deploy OTP releases of US-Web instead

# (we prefer and better support the current, native mode of operation enforced
# by this script, though)



# Note: unless specified, relying on the master branch for all clones.

# Deactivations useful for testing:

# Tells whether repositories shall be cloned (if not done already):
do_clone=0

# Tells whether dependencies shall be built (if not done already):
do_build=0

do_launch=0

no_launch_opt="--no-launch"

checkout_opt="--checkout"

support_nitrogen=1

# To avoid typos:
checkout_dir="_checkouts"
priv_dir="priv"

base_us_dir="/opt/universal-server"


# To be able to coexist with OTP releases (named as us_web-${archive_version});
# relative to base_us_dir:
#
#native_install_dir="us_web-native"
native_install_dir="us_web-native-$(date '+%Y%m%d')"

nitrogen_option="--support-nitrogen"

usage="
Usage: $(basename $0) [-h|--help] [${no_launch_opt}] [-n|${nitrogen_option}] [BASE_US_DIR]: deploys (clones and builds) locally, as a normal user (sudo requested only whenever necessary), a fully functional US-Web environment natively (i.e. from its sources, not as an integrated OTP release) in the specified base directory (otherwise in the default '${base_us_dir}' directory), as '${native_install_dir}', then launches it (unless requested not to, with the '${no_launch_opt}' option).

The '${nitrogen_option}' option will enable the support for Nitrogen-based websites.

Creates a full installation where most dependencies are sibling directories of US-Web, symlinked in checkout directories, so that code-level upgrades are easier to perform than in an OTP/rebar3 context.

The prerequisites expected to be already installed are:
 - Erlang/OTP (see http://myriad.esperide.org/#prerequisites)
 - rebar3 (see http://myriad.esperide.org/#getting-rebar3), for dependencies that are not ours
 - [optional] Awstats (see http://www.awstats.org/)
"

github_base="https://github.com/Olivier-Boudeville"


# Note that this mode of obtaining US-Web does not rely on rebar3 for US-Web
# itself, even if it used at least for some dependencies (ex: LEEC, so that its
# own dependencies - mostly a JSON parser - are automatically managed).
#
# This does not lead to duplications (ex: Myriad being built once in the context
# of LEEC and also once for the other packages), thanks to _checkouts
# directories containing symlinks whenever appropriate.

token_eaten=0

while [ $token_eaten -eq 0 ]; do

	token_eaten=1

	if [ "$1" = "-h" ] || [ "$1" = "--help" ]; then
		echo "${usage}"
		exit 0
	fi

	if [ "$1" = "-n" ] || [ "$1" = "${nitrogen_option}" ]; then
		echo "(Nitrogen support enabled)"
		support_nitrogen=1
		token_eaten=0
	fi

	if [ "$1" = "${no_launch_opt}" ]; then
		echo "(auto-launch disabled)"
		do_launch=1
		token_eaten=0
	fi

	if [ $token_eaten -eq 0 ]; then
		shift
	fi

done

# Acts as a default option catchall as well:
if [ -n "$1" ]; then
	base_us_dir="$1"
	# If specified, must exist:
	if [ ! -d "${base_us_dir}" ]; then

		echo "  Error, specified installation directory, '${base_us_dir}', does not exist.
${usage}" 1>&2
		exit 4

	fi

fi

#echo "support_nitrogen = ${support_nitrogen}"
#echo "do_launch = ${do_launch}"
#echo "base_us_dir = ${base_us_dir}"



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

	# To avoid "Already on 'master'":
	if [ "${target_branch}" != "master" ]; then

		cd us_web && ${git} checkout ${target_branch} 1>/dev/null && cd ..
		if [ ! $? -eq 0 ]; then

			echo " Error, unable to switch to US-Web branch '${target_branch}'." 1>&2
			exit 42

		fi

	fi

	# The explicit build of Cowboy is needed due to a rebar3 bug encountered
	# when building US-Web (see the comment in the 'building US-Web' section).

	echo " - cloning Cowboy"

	#cowboy_git_id="git@github.com:ninenines/cowboy.git"
	cowboy_git_id="https://github.com/ninenines/cowboy.git"

	${git} clone ${clone_opts} ${cowboy_git_id}
	if [ ! $? -eq 0 ]; then

		echo " Error, unable to obtain Cowboy." 1>&2
		exit 50

	fi

	# A lot safer than relying on the tip of the master branch:
	cowboy_tag="2.8.0"

	if [ -n "${cowboy_tag}" ]; then

		echo " - setting Cowboy to tag '${cowboy_tag}'"

		cd cowboy
		${git} checkout tags/${cowboy_tag}
		if [ ! $? -eq 0 ]; then

			echo " Error, unable to set Cowboy to tag '${cowboy_tag}'." 1>&2
			exit 52

		fi

		cd ..

	fi


	if [ $support_nitrogen -eq 0 ]; then

		# After more studies, building nitrogen_core takes care of the following
		# dependencies: cf erlware_commons nitro_cache nprocreg qdate
		# qdate_localtime rekt simple_bridge stacktrace_compat Its rebar.config
		# lists an additional one, sync, which is not needed in production, so
		# ultimately only nitrogen_core is actually needed.

		echo " - cloning nitrogen_core"

		${git} clone ${clone_opts} git://github.com/nitrogen/nitrogen_core
		if [ ! $? -eq 0 ]; then

			echo " Error, unable to obtain nitrogen_core." 1>&2
			exit 80

		fi

		nitrogen_core_tag="v2.4.0"

		if [ -n "${nitrogen_core_tag}" ]; then

			echo " - setting nitrogen_core to tag '${nitrogen_core_tag}'"

			cd nitrogen_core
			${git} checkout tags/${nitrogen_core_tag}
			if [ ! $? -eq 0 ]; then

				echo " Error, unable to set nitrogen_core to tag '${nitrogen_core_tag}'." 1>&2
				exit 82

			fi

			cd ..

		fi


		# echo " - cloning simple_bridge"

		# ${git} clone ${clone_opts} git://github.com/nitrogen/simple_bridge
		# if [ ! $? -eq 0 ]; then

		#	echo " Error, unable to obtain ." 1>&2
		#	exit 60

		# fi

		# simple_bridge_tag="v2.1.0"

		# if [ -n "${simple_bridge_tag}" ]; then

		#	echo " - setting simple_bridge to tag '${simple_bridge_tag}'"

		#	cd simple_bridge
		#	${git} checkout tags/${simple_bridge_tag}
		#	if [ ! $? -eq 0 ]; then

		#		echo " Error, unable to set simple_bridge to tag '${simple_bridge_tag}'." 1>&2
		#		exit 62

		#	fi

		#	cd ..

		# fi


		# echo " - cloning qdate"

		# ${git} clone ${clone_opts} git://github.com/choptastic/qdate
		# if [ ! $? -eq 0 ]; then

		#	echo " Error, unable to obtain qdate." 1>&2
		#	exit 70

		# fi

		# qdate_tag="0.5.0"

		# if [ -n "${qdate_tag}" ]; then

		#	echo " - setting qdate to tag '${qdate_tag}'"

		#	cd qdate
		#	${git} checkout tags/${qdate_tag}
		#	if [ ! $? -eq 0 ]; then

		#		echo " Error, unable to set qdate to tag '${qdate_tag}'." 1>&2
		#		exit 72

		#	fi

		#	cd ..

		# fi


		# echo " - cloning nprocreg"

		# ${git} clone ${clone_opts} git://github.com/nitrogen/nitrogen_core
		# if [ ! $? -eq 0 ]; then

		#	echo " Error, unable to obtain nprocreg." 1>&2
		#	exit 70

		# fi

		# nprocreg_tag="v0.3.0"

		# if [ -n "${nprocreg_tag}" ]; then

		#	echo " - setting nprocreg to tag '${nprocreg_tag}'"

		#	cd nprocreg
		#	${git} checkout tags/${nprocreg_tag}
		#	if [ ! $? -eq 0 ]; then

		#		echo " Error, unable to set nprocreg to tag '${nprocreg_tag}'." 1>&2
		#		exit 72

		#	fi

		#	cd ..

		# fi

		# echo " - cloning sync"

		# ${git} clone ${clone_opts} git://github.com/rustyio/sync
		# if [ ! $? -eq 0 ]; then

		#	echo " Error, unable to obtain sync." 1>&2
		#	exit 90

		# fi

		# sync_tag="v0.2.0"

		# if [ -n "${sync_tag}" ]; then

		#	echo " - setting sync to tag '${sync_tag}'"

		#	cd sync
		#	${git} checkout tags/${sync_tag}
		#	if [ ! $? -eq 0 ]; then

		#		echo " Error, unable to set sync to tag '${sync_tag}'." 1>&2
		#		exit 92

		#	fi

		#	cd ..

		# fi


		# echo " - cloning nitro_cache"

		# ${git} clone ${clone_opts} git://github.com/choptastic/nitro_cache
		# if [ ! $? -eq 0 ]; then

		#	echo " Error, unable to obtain nitro_cache." 1>&2
		#	exit 100

		# fi

		# nitro_cache_tag="0.5.0"

		# if [ -n "${nitro_cache_tag}" ]; then

		#	echo " - setting nitro_cache to tag '${nitro_cache_tag}'"

		#	cd nitro_cache
		#	${git} checkout tags/${nitro_cache_tag}
		#	if [ ! $? -eq 0 ]; then

		#		echo " Error, unable to set nitro_cache to tag '${nitro_cache_tag}'." 1>&2
		#		exit 102

		#	fi

		#	cd ..

		# fi

	fi


	echo " - cloning US-Common"

	${git} clone ${clone_opts} ${github_base}/us-common us_common

	if [ ! $? -eq 0 ]; then

		echo " Error, unable to obtain US-Common." 1>&2
		exit 35

	fi


	echo " - cloning Ceylan-LEEC (Ceylan fork of letsencrypt-erlang)"

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

	# Beware, running just 'make all' will update cowboy/ebin but not
	# _build/default/lib/cowboy/ebin - which is nevertheless the only one in the
	# US-Web code path, so the only one that matters.
	#
	cd cowboy && ${rebar3} compile 1>/dev/null
	if [ ! $? -eq 0 ]; then
		echo " Error, the build of Cowboy failed." 1>&2
		exit 65
	fi
	cd ..

	if [ $support_nitrogen -eq 0 ]; then


		echo " - building nitrogen_core"

		cd nitrogen_core

		${make} all
		if [ ! $? -eq 0 ]; then
			echo " Error, the build of nitrogen_core failed." 1>&2
			exit 70
		fi

		cd ..


		# Next dependencies not explicitly needed finally:

		# echo " - building simple_bridge"

		# cd simple_bridge

		# ${make} all
		# if [ ! $? -eq 0 ]; then
		#	echo " Error, the build of simple_bridge failed." 1>&2
		#	exit 70
		# fi

		# cd ..


		# echo " - building qdate"

		# cd qdate

		# ${make} all
		# if [ ! $? -eq 0 ]; then
		#	echo " Error, the build of qdate failed." 1>&2
		#	exit 75
		# fi

		# cd ..


		# echo " - building nprocreg"

		# cd nprocreg

		# ${make} all
		# if [ ! $? -eq 0 ]; then
		#	echo " Error, the build of nprocreg failed." 1>&2
		#	exit 80
		# fi

		# cd ..


	fi

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
	# Thanks to checkout, no need to /bin/rm -rf leec/_build/default/lib/myriad.
	#
	cd leec && mkdir ${checkout_dir} && cd ${checkout_dir} && ln -s ../../myriad && cd ..

	if [ ! $? -eq 0 ]; then
		echo " Error, the pre-build of LEEC failed." 1>&2
		exit 65
	fi

	# Relies either on rebar3, so that prerequisites such as the JSON parser are
	# managed (otherwise our native build could be used, yet then the extra
	# dependencies - namely JSX - shall be available separately):
	#
	${make} all-rebar3 USE_SHOTGUN=false 1>/dev/null

	# Relying on Erlang-native httpc instead (through Myriad's web_utils):
	#${make} all USE_SHOTGUN=false 1>/dev/null

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

	# Add a convenient certificate-related makefile:
	if [ -n "${us_cert_dir}" ]; then

		if [ ! -d "${us_cert_dir}" ]; then

			mkdir -p "${us_cert_dir}"

		fi

		previous_dir="$(pwd)"

		cd "${us_cert_dir}"

		ln -s ${us_web_dir}/conf/GNUmakefile-for-certificates GNUmakefile

		cd "${previous_dir}"

	fi

	# First permissions (chmod), then owner/group (chown):

	dir_perms="770"

	# abs_native_install_dir/* rather than only us_web_dir, as the dependencies
	# shall also have their permissions updated:

	echo " Changing the permissions of deployed roots in '${abs_native_install_dir}' to ${dir_perms}."

	# Not wanting to select non-directories:
	dirs=$(/bin/ls -d ${abs_native_install_dir}/*/)

	if ! sudo chmod ${dir_perms} ${dirs}; then

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
