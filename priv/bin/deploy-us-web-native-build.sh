#!/bin/sh

# Copyright (C) 2020-2025 Olivier Boudeville

# A script to automatically deploy a US-Web native build from scratch (provided
# that Erlang is already available).
#
# This is the most common procedure, especially if using the '--no-launch'
# command-line option.


# See also:
# - the get-us-web-from-sources.sh script to install a test version of US-Web
# - the deploy-us-web-release.sh script to deploy OTP releases of US-Web instead
#
# (we prefer and better support the current, native mode of operation enforced
# by this script, though)

# Note: this file has been used as a blueprint for its US-Main counterpart
# (deploy-us-main-release.sh); they shall remain in sync as much as possible.

# (standalone script)


# We notably want to ensure that from now the created directories allow users of
# the US group to write, so that a regular user can be added to the US group and
# operate on a deployed tree (for example to update it with rsync); however the
# group of the content on the remote end will be the main group of the user
# there, which may not be the US one.
#
# For security, 'others' are deprived of all rights.
#
umask u=rwx,g=rwx,o=


# Note: unless specified, relying on the master branch for all clones.

# Deactivations useful for testing:

# Tells whether repositories shall be cloned (if not done already):
do_clone=0
#do_clone=1

# Tells whether dependencies shall be built (if not done already):
#
# (best left enabled, as this step is almost immediately gone through if builds
# have already been done, and it has for side effect to read the US
# configuration files, which is needed for deployment afterwards)
#
do_build=0
#do_build=1

do_launch=0
no_launch_opt="--no-launch"

#checkout_opt="--checkout"

root_exec_allowed=1
allow_root_exec_opt="--allow-root-exec"

# Not by default:
support_nitrogen=1


# To avoid typos:
#checkout_dir="_checkouts"
priv_dir="priv"

base_us_dir="/opt/universal-server"


# To be able to coexist with OTP releases (named as us_web-${archive_version});
# relative to base_us_dir, and more convenient if timestamped (so that multiple
# versions can easily coexist):
#
native_install_dir="us_web-native-deployment-$(date '+%Y%m%d')"


nitrogen_option="--support-nitrogen"

usage="
Usage: $(basename $0) [-h|--help] [${no_launch_opt}] [${allow_root_exec_opt}] [-n|${nitrogen_option}] [BASE_US_DIR]: deploys (clones and builds) locally, as a normal user (sudo requested only whenever necessary), a fully functional US-Web environment natively (i.e. from its sources, not as an integrated OTP release) in the specified base directory (otherwise in the default '${base_us_dir}' directory), as '${native_install_dir}', then launches it (unless requested not to, with the '${no_launch_opt}' option).

Options:
 ${no_launch_opt}: installs US-Web, but does not launch it automatically (possibly because a prior install is still running)
 ${allow_root_exec_opt}: allows this script to run as root (mostly useful for continuous integration)
 ${nitrogen_option}: enables the support of a Nitrogen-based website (mostly obsolete)

Creates a full installation where most dependencies are sibling directories of US-Web, symlinked in checkout directories, so that code-level upgrades are easier to perform than in an OTP/rebar3 context.

The prerequisites expected to be already installed are:
 - Erlang/OTP (see http://myriad.esperide.org/#prerequisites)
 - [optional] Awstats (see http://www.awstats.org/)"


# Not used anymore:
# - [partly obsolete] rebar3 (see http://myriad.esperide.org/#getting-rebar3) was used, for dependencies that are not ours, yet we gave up relying on it, at least for LEEC, as it led to too much trouble (trying to recompile Myriad for obscure reasons, and of course failing)


# Will thus install the following US-Web prerequisites:
# - Myriad, WOOPER and Traces
# - LEEC
# - US-Common
# - Cowboy

our_github_base="https://github.com/Olivier-Boudeville"


# Note that this mode of obtaining US-Web does not rely on rebar3 for US-Web
# itself, even if it was used at least for some dependencies (e.g. LEEC, so that
# its own dependencies - mostly a JSON parser - were automatically managed).
#
# This does not lead to duplications (e.g. Myriad being built once in the
# context of LEEC and also once for the other packages), thanks to _checkouts
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
		support_nitrogen=0
		token_eaten=0
	fi

	if [ "$1" = "${no_launch_opt}" ]; then
		echo "(auto-launch disabled)"
		do_launch=1
		token_eaten=0
	fi

	if [ "$1" = "${allow_root_exec_opt}" ]; then
		echo "(root execution enabled)"
		root_exec_allowed=0
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

		echo "  Error, the specified installation directory, '${base_us_dir}', does not exist (from '$(pwd)').
${usage}" 1>&2
		exit 4

	fi

	# Ensure is absolute:
	case "${base_us_dir}" in

		/*) : ;;
		*) base_us_dir="$(realpath ${base_us_dir})" ;;

	esac

fi

#echo "base_us_dir = ${base_us_dir}"


# Selects the (build-time) execution target for all Ceylan layers:
#
# We prefer now 'production', as we want a US-Web server to be robust (e.g. by
# restarting any failed child):
#
#execution_target="development"
execution_target="production"

ceylan_opts="EXECUTION_TARGET=${execution_target}"



# $0 may already be absolute:
case "$0" in

    /*)
		log_dir="$(dirname $0)";;

    *)
		log_dir="$(pwd)";;

esac

log_file="${log_dir}/$(basename $0).log"


echo "Writing in log file '${log_file}'."

if [ -f "${log_file}" ]; then
	/bin/rm -f "${log_file}"
fi


display_and_log()
{

	echo "$*" | tee --append "${log_file}"

}


#display_and_log "support_nitrogen = ${support_nitrogen}"
#display_and_log "do_launch = ${do_launch}"
#display_and_log "base_us_dir = ${base_us_dir}"
#display_and_log "ceylan_opts = ${ceylan_opts}"


# Just to avoid error messages if running from a non-existing directory:
cd /


# Checking first:

if [ $root_exec_allowed -eq 1 ]; then

	if [ "$(id -u)" = "0" ]; then

		echo "  Error, this script must not be run as root (sudo will be requested only when necessary)." 1>&2
		exit 5

	fi

fi


erlc="$(which erlc 2>/dev/null)"

# No version checked:
if [ ! -x "${erlc}" ]; then

	echo "  Error, no Erlang compiler (erlc) found. Consider installing Erlang first, possibly thanks to our dedicated script, ${our_github_base}/Ceylan-Myriad/blob/master/conf/install-erlang.sh." 1>&2

	exit 10

fi


rebar3="$(which rebar3 2>/dev/null)"

# No version checked either:
if [ ! -x "${rebar3}" ]; then

	echo "  Error, rebar3 not found. Consider installing it first, one may refer to http://myriad.esperide.org/#getting-rebar3." 1>&2

	exit 11

fi


make="$(which make 2>/dev/null)"

if [ ! -x "${make}" ]; then

	echo "  Error, no 'make' tool found." 1>&2
	exit 18

fi


display_and_log "Securing sudoer rights for the upcoming operations that require it."
if ! sudo echo; then

	echo "  Error, sudo failed." 1>&2
	exit 19

fi


base_us_dir_created=1

if [ ! -d "${base_us_dir}" ]; then

	display_and_log " Creating US base directory '${base_us_dir}'."
	# Most permissive, will be updated later in the installation:
	sudo /bin/mkdir --mode=777 "${base_us_dir}"
	base_us_dir_created=0

fi


# Typically a release-like '/opt/universal-server/us_web{-native,}-deployment'
# tree, containing all dependencies:
#
abs_native_install_dir="${base_us_dir}/${native_install_dir}"

# The US-Web tree itself:
us_web_dir="${abs_native_install_dir}/us_web"



display_and_log "   Installing US-Web in '${abs_native_install_dir}'..."
display_and_log



if [ $do_clone -eq 0 ]; then

	cd "${base_us_dir}"

	if [ -d "${native_install_dir}" ]; then

		echo "  Error, target installation directory, '${base_us_dir}/${native_install_dir}', already exists (from '$(pwd)'). Remove it first (preferably as root, as US-Web may be set to run as a specific user that would then own some log files in this tree)." 1>&2

		exit 20

	fi

	# Parent already exists by design; ensuring any normal user can write the
	# content to install next:
	#
	(sudo mkdir --mode=777 "${native_install_dir}") && cd "${native_install_dir}"

	clone_opts="--quiet"

	git="$(which git 2>/dev/null)"

	if [ ! -x "${git}" ]; then

		echo "  Error, no 'git' tool found." 1>&2
		exit 18

	fi


	display_and_log "Getting the relevant repositories (as $(id -un)):"


	# First US-Web itself, so that any _checkouts directory can be created
	# afterwards:
	#
	display_and_log " - cloning US-Web"

	if ! ${git} clone ${clone_opts} "${our_github_base}/us-web" us_web; then

		echo " Error, unable to obtain US-Web." 1>&2
		exit 40

	fi

	# A specific branch might be selected:
	us_web_branch="master"
	#us_web_branch="wildcard-certificate-support"

	# To avoid "Already on 'master'":
	if [ "${us_web_branch}" != "master" ]; then

		cd us_web && ${git} switch "${us_web_branch}" 1>>"${log_file}" && cd ..
		if [ ! $? -eq 0 ]; then

			echo " Error, unable to switch to US-Web branch '${us_web_branch}'." 1>&2
			exit 41

		fi

	fi

	ln -sf us_web/conf/GNUmakefile-for-native-root GNUmakefile


	# Superseded by the built-in 'json' parser:
	# display_and_log " - cloning jsx"

	# if ! ${git} clone ${clone_opts} https://github.com/talentdeficit/jsx.git; then

	#   echo " Error, unable to obtain jsx parser." 1>&2
	#   exit 38

	# fi


	display_and_log " - cloning Ceylan-Myriad"

	if ! ${git} clone ${clone_opts} "${our_github_base}/Ceylan-Myriad" myriad; then

		echo " Error, unable to obtain Ceylan-Myriad." 1>&2
		exit 20

	fi

	# A specific branch might be selected:
	myriad_branch="master"
	#myriad_branch="opengl-augmentation"

	# To avoid "Already on 'master'":
	if [ "${myriad_branch}" != "master" ]; then

		cd myriad && ${git} switch "${myriad_branch}" 1>>"${log_file}" && cd ..
		if [ ! $? -eq 0 ]; then

			echo " Error, unable to switch to Ceylan-Myriad branch '${myriad_branch}'." 1>&2
			exit 21

		fi

	fi


	display_and_log " - cloning Ceylan-LEEC (Ceylan fork of letsencrypt-erlang)"

	if ! ${git} clone ${clone_opts} "${our_github_base}/Ceylan-LEEC" leec; then

		echo " Error, unable to obtain Ceylan-LEEC." 1>&2
		exit 32

	fi

	# A specific branch might be selected:
	leec_branch="master"
	#leec_branch="wildcard-certificate-support"

	# To avoid "Already on 'master'":
	if [ "${leec_branch}" != "master" ]; then

		display_and_log " - setting Ceylan-LEEC to branch '${leec_branch}'"

		cd leec

		if ! ${git} switch "${leec_branch}"; then

			echo " Error, unable to set Ceylan-LEEC to branch '${leec_branch}'." 1>&2
			exit 33

		fi

		cd ..

	fi


	display_and_log " - cloning Ceylan-WOOPER"

	if ! ${git} clone ${clone_opts} "${our_github_base}/Ceylan-WOOPER" wooper; then

		echo " Error, unable to obtain Ceylan-WOOPER." 1>&2
		exit 25

	fi


	display_and_log " - cloning Ceylan-Traces"

	if ! ${git} clone ${clone_opts} "${our_github_base}/Ceylan-Traces" traces; then

		echo " Error, unable to obtain Ceylan-Traces." 1>&2
		exit 30

	fi


	display_and_log " - cloning US-Common"

	if ! ${git} clone ${clone_opts} "${our_github_base}/us-common" us_common; then

		echo " Error, unable to obtain US-Common." 1>&2
		exit 35

	fi


	# The explicit build of Cowboy is needed due to a rebar3 bug encountered
	# when building US-Web (see the comment in the 'building US-Web' section).

	display_and_log " - cloning Cowboy"

	#cowboy_git_id="git@github.com:ninenines/cowboy.git"
	cowboy_git_id="https://github.com/ninenines/cowboy.git"

	if ! ${git} clone ${clone_opts} ${cowboy_git_id}; then

		echo " Error, unable to obtain Cowboy." 1>&2
		exit 50

	fi

	# A lot safer than relying on the tip of the master branch:
	cowboy_tag="2.13.0"

	if [ -n "${cowboy_tag}" ]; then

		display_and_log " - setting Cowboy to tag '${cowboy_tag}'"

		cd cowboy

		if ! ${git} -c advice.detachedHead=false checkout tags/${cowboy_tag}; then

			echo " Error, unable to set Cowboy to tag '${cowboy_tag}'." 1>&2
			exit 52

		fi

		cd ..

	fi


	if [ $support_nitrogen -eq 0 ]; then

		# After more studies, building nitrogen_core takes care of the following
		# dependencies: cf erlware_commons nitro_cache nprocreg qdate
		# qdate_localtime rekt simple_bridge stacktrace_compat
		#
		# Its rebar.config lists an additional one, sync, which is not needed in
		# production, so ultimately only nitrogen_core is actually needed.
		#
		# We strongly recommend building all these dependencies through the
		# build of nitrogen_core, as their build is rather non-standard (older
		# rebar), fragile, complex (release-based).
		#
		# Now we prefer using our forks of the only two dependencies that had to
		# be modified, nitrogen_core and simple_bridge:

		nitro_fork_branch="master"
		#nitro_fork_branch="testing"

		display_and_log " - cloning nitrogen_core"

		#nitrogen_core_git_id="https://github.com/nitrogen/nitrogen_core"
		nitrogen_core_git_id="${our_github_base}/nitrogen_core"

		if ! ${git} clone ${clone_opts} "${nitrogen_core_git_id}"; then

			echo " Error, unable to obtain nitrogen_core." 1>&2
			exit 80

		fi

		#nitrogen_core_tag="v2.4.0"
		nitrogen_core_tag=""

		if [ -n "${nitrogen_core_tag}" ]; then

			display_and_log " - setting nitrogen_core to tag '${nitrogen_core_tag}'"

			cd nitrogen_core

			if ! ${git} checkout "tags/${nitrogen_core_tag}"; then

				echo " Error, unable to set nitrogen_core to tag '${nitrogen_core_tag}'." 1>&2
				exit 82

			fi

			cd ..

		fi

		if [ -n "${nitro_fork_branch}" ]; then

			display_and_log " - setting nitrogen_core to branch '${nitro_fork_branch}'"

			cd nitrogen_core

			if ! ${git} switch "${nitro_fork_branch}"; then

				echo " Error, unable to set nitrogen_core to branch '${nitro_fork_branch}'." 1>&2
				exit 83

			fi

			cd ..

		fi


		# Deactivated, as nitrogen_core will manage directly its dependencies:

		#display_and_log " - cloning simple_bridge"

		##simple_bridge_git_id="https://github.com/nitrogen/simple_bridge"
		#simple_bridge_git_id="${our_github_base}/simple_bridge"

		#if ! ${git} clone ${clone_opts} "${simple_bridge_git_id}"; then

		#	echo " Error, unable to obtain simple_bridge." 1>&2
		#	exit 60

		#fi

		#simple_bridge_tag="v2.1.0"
		#simple_bridge_tag=""

		#if [ -n "${simple_bridge_tag}" ]; then

		#	display_and_log " - setting simple_bridge to tag '${simple_bridge_tag}'"

		#	cd simple_bridge

		#	if ! ${git} checkout "tags/${simple_bridge_tag}"; then

		#		echo " Error, unable to set simple_bridge to tag '${simple_bridge_tag}'." 1>&2
		#		exit 62

		#	fi

		#	cd ..

		#fi

		#if [ -n "${nitro_fork_branch}" ]; then

		#	display_and_log " - setting simple_bridge to branch '${nitro_fork_branch}'"

		#	cd simple_bridge

		#	if ! ${git} switch "${nitro_fork_branch}"; then

		#		echo " Error, unable to set simple_bridge to branch '${nitro_fork_branch}'." 1>&2
		#		exit 63

		#	fi

		#	cd ..

		#fi

		# display_and_log " - cloning qdate"

		# if ! ${git} clone ${clone_opts} https://github.com/choptastic/qdate; then

		#	echo " Error, unable to obtain qdate." 1>&2
		#	exit 70

		# fi

		# qdate_tag="0.5.0"

		# if [ -n "${qdate_tag}" ]; then

		#	display_and_log " - setting qdate to tag '${qdate_tag}'"

		#	cd qdate
		#	if ! ${git} switch tags/${qdate_tag}; then

		#		echo " Error, unable to set qdate to tag '${qdate_tag}'." 1>&2
		#		exit 72

		#	fi

		#	cd ..

		# fi


		# display_and_log " - cloning nprocreg"

		# if ! ${git} clone ${clone_opts} https://github.com/nitrogen/nprocreg; then

		#	echo " Error, unable to obtain nprocreg." 1>&2
		#	exit 70

		# fi

		# nprocreg_tag="v0.3.0"

		# if [ -n "${nprocreg_tag}" ]; then

		#	display_and_log " - setting nprocreg to tag '${nprocreg_tag}'"

		#	cd nprocreg
		#	if ! ${git} switch tags/${nprocreg_tag}; then

		#		echo " Error, unable to set nprocreg to tag '${nprocreg_tag}'." 1>&2
		#		exit 72

		#	fi

		#	cd ..

		# fi

		# display_and_log " - cloning sync"

		# if ! ${git} clone ${clone_opts} https://github.com/rustyio/sync; then

		#	echo " Error, unable to obtain sync." 1>&2
		#	exit 90

		# fi

		# sync_tag="v0.2.0"

		# if [ -n "${sync_tag}" ]; then

		#	display_and_log " - setting sync to tag '${sync_tag}'"

		#	cd sync
		#
		#	if !${git} switch tags/${sync_tag}; then

		#		echo " Error, unable to set sync to tag '${sync_tag}'." 1>&2
		#		exit 92

		#	fi

		#	cd ..

		# fi


		# display_and_log " - cloning nitro_cache"

		# if ! ${git} clone ${clone_opts} https://github.com/choptastic/nitro_cache; then

		#	echo " Error, unable to obtain nitro_cache." 1>&2
		#	exit 100

		# fi

		# nitro_cache_tag="0.5.0"

		# if [ -n "${nitro_cache_tag}" ]; then

		#	display_and_log " - setting nitro_cache to tag '${nitro_cache_tag}'"

		#	cd nitro_cache
		#	if !${git} switch tags/${nitro_cache_tag}; then

		#		echo " Error, unable to set nitro_cache to tag '${nitro_cache_tag}'." 1>&2
		#		exit 102

		#	fi

		#	cd ..

		# fi

	fi

fi



if [ ${do_build} -eq 0 ]; then

	cd "${abs_native_install_dir}"

	display_and_log
	display_and_log "Building these packages as $(id -un), with Erlang $(erl -eval '{ok, V} = file:read_file( filename:join([code:root_dir(), "releases", erlang:system_info(otp_release), "OTP_VERSION"]) ), io:fwrite(V), halt().' -noshell) and following Ceylan options: ${ceylan_opts}, from '$(pwd)':"

	# ('json' now used instead)
	#
	# Previously building our own standalone version of jsx; rebar3 required.
	#
	# The resulting BEAM files are both in 'ebin' and in
	# '_build/default/lib/jsx/ebin':
	#
	# display_and_log " - building jsx"
	# cd jsx && ${rebar3} compile 1>>"${log_file}"
	# if [ ! $? -eq 0 ]; then
	#   echo " Error, the build of jsx failed." 1>&2
	#   exit 90
	# fi

	# Otherwise may not be found by US-Web:
	# ln -s _build/default/lib/jsx/ebin
	#
	# cd ..


	# As much as possible, notably for our developments, we prefer relying on
	# our any vanilla good old build system (i.e. not on rebar3).

	display_and_log " - building Ceylan-Myriad"
	cd myriad && ${make} all ${ceylan_opts} 1>>"${log_file}"
	if [ ! $? -eq 0 ]; then
		echo " Error, the build of Ceylan-Myriad failed." 1>&2
		exit 50
	fi
	cd ..

	# Our build; uses Myriad's sibling tree:
	display_and_log " - building Ceylan-WOOPER"
	cd wooper && ${make} ${ceylan_opts} all 1>>"${log_file}"
	if [ ! $? -eq 0 ]; then
		echo " Error, the build of Ceylan-WOOPER failed." 1>&2
		exit 55
	fi
	cd ..

	# Our build; uses Myriad's and WOOPER's sibling trees:
	display_and_log " - building Ceylan-Traces"
	cd traces && ${make} ${ceylan_opts} all 1>>"${log_file}"
	if [ ! $? -eq 0 ]; then
		echo " Error, the build of Ceylan-Traces failed." 1>&2
		exit 60
	fi
	cd ..



	# Only for US-Web (not for prerequisites of LEEC):
	# (implies cowlib and ranch)
	#
	display_and_log " - building Cowboy"

	# Beware, running just 'make all' will update cowboy/ebin but not
	# _build/default/lib/cowboy/ebin - which is nevertheless the only one in the
	# US-Web code path, so the only one that matters.
	#
	cd cowboy && ${rebar3} compile 1>>"${log_file}"
	if [ ! $? -eq 0 ]; then
		echo " Error, the build of Cowboy failed." 1>&2
		exit 65
	fi
	cd ..

	if [ $support_nitrogen -eq 0 ]; then

		display_and_log " - building nitrogen_core"

		cd nitrogen_core

		# The included rebar is most probably obsolete:
		if ! ${make} update-rebar all; then
			echo " Error, the build of nitrogen_core failed." 1>&2
			exit 70
		fi

		cd ..


		# Next dependencies not explicitly needed finally:

		# display_and_log " - building simple_bridge"

		# cd simple_bridge

		# if ! ${make} all; then
		#   echo " Error, the build of simple_bridge failed." 1>&2
		#   exit 70
		# fi

		# cd ..


		# display_and_log " - building qdate"

		# cd qdate

		# if ! ${make} all; then
		#   echo " Error, the build of qdate failed." 1>&2
		#   exit 75
		# fi

		# cd ..


		# display_and_log " - building nprocreg"

		# cd nprocreg

		# if ! ${make} all; then
		#   echo " Error, the build of nprocreg failed." 1>&2
		#   exit 80
		# fi

		# cd ..

	fi

	# Apart from Myriad (used as a checkout to point to the same, unique install
	# thereof here), LEEC had dependencies of its own (shotgun, jsx otherwise
	# jiffy, elli, getopt, yamerl, erlang_color), so, even if not all of them
	# are actually needed by our use case (elli, getopt, yamerl, erlang_color
	# are of no use here), we preferred relying on rebar3 (as indirect
	# dependencies, such as cowlib or gun, were also induced); then the only
	# real external dependency of LEEC was jsx, yet it is now superseded by the
	# Erlang built-in JSON parser, 'json' (not to mention that now certbot is
	# mostly used instead of performing ACME calls).
	#
	display_and_log " - building LEEC"

	# Obsolete, as not using rebar3 anymore:

	# Modifying LEEC so that it relies on the same, common Myriad build tree (as
	# a checkout) rather than on its own version (as a _build dependency;
	# removed to avoid a possible cause of confusion):
	#
	# Note that rebar3 will still create a leec/_build/default/checkouts/myriad
	# directory with its own BEAMs, that may become obsolete. This directory
	# should simply remain out of the code path.
	#
	# Thanks to checkout, no need to /bin/rm -rf leec/_build/default/lib/myriad.
	#
	# Another consequence is that, from this _checkout/myriad, jsx will not be
	# found; symlinking it as well with 'cd ${checkout_dir} && ln -s
	# ../_build/default/lib/jsx' would not suffice either (as LEEC in turn would
	# not find an ebin for jsx; what a mess!) so we just deactivate the check
	# here:
	#
	#cd leec && mkdir ${checkout_dir} && cd ${checkout_dir} && ln -s ../../myriad && cd ..
    cd leec

	#if [ ! $? -eq 0 ]; then
	#   echo " Error, the pre-build of LEEC failed." 1>&2
	#   exit 65
	#fi

	# Relies either on rebar3, so that prerequisites such as the JSON parser are
	# managed (otherwise our native build could be used, yet then the extra
	# dependencies - namely JSX - shall be available separately):
	#
	# Relying on Erlang-native httpc instead (through Myriad's web_utils):
	${make} all USE_SHOTGUN=false 1>>"${log_file}"

	# We did not declare the need for JSON support at this point, as this would
	# have led to the Myriad-based lookup of a proper jsx install - whereas it
	# was not available yet (it was actually triggered by this make target);
	# anyway jsx is not used anymore:

	# With rebar3, now yields: '===> {missing_module,app_facilities}':
	#if ! ${make} all-rebar3 ${ceylan_opts} USE_JSON=false USE_SHOTGUN=false 1>>"${log_file}"; then

	# Too much trouble with rebar3, now using our build:
	if ! ${make} all ${ceylan_opts} USE_JSON=false USE_SHOTGUN=false 1>>"${log_file}"; then
		echo " Error, the build of LEEC failed." 1>&2
		exit 66
	fi
	cd ..

	# We used to create a symlink to the jsx installed with LEEC, so that US-Web
	# later could find it during its own build; it was then made useless (LEEC
	# here found its jsx as an OTP application, see otp_utils) and was bound to
	# lead to clashes between jsx installs in the code path):
	#
	#ln -s leec/_build/default/lib/jsx/


	# US-Common does not introduce third-party dependencies, so going again for
	# our native build, which thus uses Myriad's, WOOPER's and Traces' sibling
	# trees:
	#
	display_and_log " - building US-Common"
	cd us_common && ${make} all ${ceylan_opts} 1>>"${log_file}"
	if [ ! $? -eq 0 ]; then
		echo " Error, the build of US-Common failed." 1>&2
		exit 85
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
	display_and_log " - building US-Web"
	# Removed: '&& ln -s ../../jsx'; anyway rebar3 not used anymore.
	#cd us_web && mkdir ${checkout_dir} && cd ${checkout_dir} && ln -s ../../myriad && ln -s ../../wooper && ln -s ../../traces && ln -s ../../us_common && ln -s ../../leec && ln -s ../../cowboy && cd ..
	cd us_web

	# Our build; uses Ceylan's sibling trees:
	if ! ${make} all ${ceylan_opts} 1>>"${log_file}"; then
		echo " Error, the build of US-Web failed." 1>&2
		exit 95
	fi

	# Post-install: fixing permissions and all.

	# This will also by useful for any next launch:

	# Not wanting the files of this US-Web install to remain owned by the
	# deploying user, so trying to apply a more proper user/group; for that we
	# have to determine them from the configuration files, and thus to locate,
	# read and use them:
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

	display_and_log "Sourcing '${us_web_common_script}' from $(pwd)."
	. "${us_web_common_script}" 1>>"${log_file}"

	read_us_config_file #1>>"${log_file}"

	read_us_web_config_file #1>>"${log_file}"

	# Add a convenient certificate-related makefile:
	if [ -n "${us_cert_dir}" ]; then

		if [ ! -d "${us_cert_dir}" ]; then

			mkdir -p "${us_cert_dir}"

		fi

		previous_dir="$(pwd)"

		cd "${us_cert_dir}"

		ln -sf ${us_web_dir}/conf/GNUmakefile-for-certificates GNUmakefile

		cd "${previous_dir}"

	fi

	# First permissions (chmod), then owner/group (chown):

	dir_perms="770"

	# abs_native_install_dir/* rather than only us_web_dir, as the dependencies
	# shall also have their permissions updated:

	display_and_log " Changing the permissions of deployed roots in '${abs_native_install_dir}' to ${dir_perms}."

	# Not wanting to select non-directories:
	dirs="$(/bin/ls -d ${abs_native_install_dir}/*/)"

	if ! sudo chmod ${dir_perms} ${dirs}; then

		echo "Error, changing permissions of deployed roots in '${abs_native_install_dir}' to ${dir_perms} failed." 1>&2

		exit 40

	fi

	display_and_log " Changing the permissions of deployed root '${abs_native_install_dir}' itself to ${dir_perms}."
	if ! sudo chmod ${dir_perms} "${abs_native_install_dir}"; then

		echo "Error, changing permissions of deployed root '${abs_native_install_dir}' itself to ${dir_perms} failed." 1>&2

		exit 41

	fi

	if [ ${base_us_dir_created} -eq 0 ]; then

		display_and_log " Changing the permissions of base US install directory '${base_us_dir}' to ${dir_perms}."

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

		display_and_log " Changing recursively owner/group of all deployed elements as ${chown_spec} from '${abs_native_install_dir}'."
		if ! sudo chown --recursive "${chown_spec}" "${abs_native_install_dir}"; then

			echo "Error, changing recursively user/group owner (as ${chown_spec}) from '${abs_native_install_dir}' failed." 1>&2

			exit 45

		fi

		if [ ${base_us_dir_created} -eq 0 ]; then

			display_and_log " Changing also the owner/group of base US install directory '${base_us_dir}' as '${chown_spec}'."

			# This one is not recursive:
			if ! sudo chown "${chown_spec}" "${base_us_dir}"; then

				echo "Error, changing owner/group owner of '${base_us_dir}' (as '${chown_spec}') failed." 1>&2

				exit 46

			fi

		fi

	fi


	# Final touch for the build:

	cd "${base_us_dir}"

	# Designates this install as the latest one then.
	#
	# Rare option needed, otherwise apparently mistook for a directory resulting
	# in an incorrect link:
	#
	sudo /bin/ln -sf --no-target-directory "${native_install_dir}" us_web-native-deployment
	sudo /bin/ln -sf --no-target-directory us_web-native-deployment us_web-latest

	display_and_log
	display_and_log "Native US-Web built and ready in ${abs_native_install_dir}."

fi



# Not checking specifically the expected US and US-Web configuration files:
# running US-Web will tell us whether they exist and are legit.

display_and_log


if [ $do_launch -eq 0 ]; then

	# Not expecting here a previous native instance to run.

	# Absolute path; typically in
	# '/opt/universal-server/us_web-native-deployment-*/us_web':
	#
	if [ ! -d "${us_web_dir}" ]; then

		echo " Error, the target US-Web directory, '${us_web_dir}', does not exist." 1>&2
		exit 75

	fi

	display_and_log "   Running US-Web native application (as '$(id -un)' initially, from '${us_web_dir}')"

	# Simplest: cd src && ${make} us_web_exec

	cd "${us_web_dir}" || exit 80


	# Actual cookie managed there:
	start_script="${priv_dir}/bin/start-us-web-native-build.sh"

	if [ ! -x "${start_script}" ]; then

		echo "  Error, no start script found (no '${start_script}' found)." 1>&2
		exit 30

	fi


	cd "${base_us_dir}"

	# Automatic shutdown (that was deferred as much as possible) of any prior
	# US-Web release running:
	#
	for d in $(/bin/ls -d us_web-*.*.* 2>/dev/null); do

		exec="${d}/bin/us_web"

		display_and_log "Testing for ${exec}..."

		if [ -x "${exec}" ]; then
			display_and_log " Trying to stop gracefully any prior release in ${d}."
			sudo ${exec} stop 1>>"${log_file}" 2>&1
		fi

	done

	display_and_log "### Launching US-Web native build now, specifying '${us_config_dir}' as US configuration directory"

	cd "${abs_native_install_dir}/us_web" || exit 81


	# The next start script will switch to the US-Web configured user; this
	# requires an US configuration file to be found, and we have to specify here
	# the one that was previously selected, as otherwise running the next start
	# script thanks to sudo may not select the proper configuration file
	# (typically if the intended one is located in the
	# ~/.config/universal-server directory of the launching user):

	# Needing to specify the US configuration *directory* (not file):
	sudo ${start_script} "${us_config_dir}"
	res=$?

	if [ $res -eq 0 ]; then

		display_and_log " US-Web launched (start script reported success)."

	else
		echo "  Error, start script ('${start_script}') failed (code: ${res})." 1>&2

		exit ${res}

	fi

	#sleep 1

	# Maybe use get-us-web-native-build-status.sh in the future.

	display_and_log "Deployment done; consider running our 'us_web/priv/bin/monitor-us-web.sh' script if wanting more detailed information regarding this launched instance."

	if [ -n "${US_WEB_POST_DEPLOY_CMD}" ]; then

		display_and_log "US_WEB_POST_DEPLOY_CMD found set to '${US_WEB_POST_DEPLOY_CMD}'; executing this command now."

		if ${US_WEB_POST_DEPLOY_CMD}; then

			echo "Execution of US_WEB_POST_DEPLOY_CMD succeeded."

		else

			echo "  Error, the execution of US_WEB_POST_DEPLOY_CMD failed." 1>&2

        fi

	else

		display_and_log "No US_WEB_POST_DEPLOY_CMD environment variable set; if relevant, consider executing manually now any certbot deploy hook that would be needed, typically if using some TLS certificate(s) for other servers (like CalDAV/CardDAV ones)."

	fi

else

	display_and_log "(no auto-launch enabled; one may decide to enable or disable certificate generation - see the 'certificate_support' key in US-Web configuration file - and execute, as root, 'systemctl daemon-reload && systemctl restart us-web-as-native-build.service; sleep 150; systemctl status us-web-as-native-build.service' - the sleep allowing hopefully to wait for the end of any certificate renewal procedure (count at least 2 minutes per domain if relying on the dns-01 challenge; hence having 5 domains results in, at startup, 10 minutes of waiting until HTTP and HTTPS become ready) - and check, possibly with wget, that the expected virtual hosts are available indeed)."

	display_and_log "Any prior US-Web instance that would still linger could be removed thanks to our 'kill-us-web.sh' script. Use 'journalctl -eu us-web-as-native-build.service' to consult the corresponding systemd-level logs."

    display_and_log "Finally, if certificates were to be generated, consider also executing manually after launch any certbot deploy hook that would be needed, typically if using some TLS certificate(s) for other servers (like CalDAV/CardDAV ones)."

fi


new_log_file="${base_us_dir}/$(basename $0).log"

echo "(moving finally log file '${log_file}' to '${new_log_file}')"
sudo /bin/mv -f "${log_file}" "${new_log_file}"
