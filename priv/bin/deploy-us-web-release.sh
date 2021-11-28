#!/bin/sh

usage="Usage: $(basename $0): deploys (installs and runs) locally a US-Web release."

# This script is at least currently deprecated in favor of
# deploy-us-web-native-build.sh, which deploys instead a native (locally built
# from sources) version of US-Web.

# TO-DO: does not run as root and uses sudo whenever necessary.


# Disabled since my https://github.com/erlware/relx/pull/809 pull request has
# been integrated:
#
patch_for_cookie_env()
{

	cd "${rel_root}/bin"

	patch_file="us_web-cookie-management.patch"

   # Note: do not clean-up whitespaces in this script, otherwise this patch will
   # become faulty.

   cat << '__HERE_DOC__' > "${patch_file}"
--- us_web.source       2020-08-11 21:51:04.724167964 +0200
+++ us_web.target       2020-08-11 21:56:14.507137897 +0200
@@ -647,17 +647,19 @@

 # Extract the target cookie
 # Do this before relx_get_nodename so we can use it and not create a ~/.erlang.cookie
-COOKIE_ARG="$(grep '^-setcookie' "$VMARGS_PATH" || true)"
-DEFAULT_COOKIE_FILE="$HOME/.erlang.cookie"
-if [ -z "$COOKIE_ARG" ]; then
-    if [ -f "$DEFAULT_COOKIE_FILE" ]; then
-        COOKIE="$(cat "$DEFAULT_COOKIE_FILE")"
+if [ -z "$COOKIE" ]; then
+    COOKIE_ARG="$(grep '^-setcookie' "$VMARGS_PATH" || true)"
+    DEFAULT_COOKIE_FILE="$HOME/.erlang.cookie"
+    if [ -z "$COOKIE_ARG" ]; then
+        if [ -f "$DEFAULT_COOKIE_FILE" ]; then
+            COOKIE="$(cat "$DEFAULT_COOKIE_FILE")"
+        else
+            echo "No cookie is set or found. This limits the scripts functionality, installing, upgrading, rpc and getting a list of versions will not work."
+        fi
	 else
-        echo "No cookie is set or found. This limits the scripts functionality, installing, upgrading, rpc and getting a list of versions will not work."
+        # Extract cookie name from COOKIE_ARG
+        COOKIE="$(echo "$COOKIE_ARG" | awk '{print $2}')"
	 fi
-else
-    # Extract cookie name from COOKIE_ARG
-    COOKIE="$(echo "$COOKIE_ARG" | awk '{print $2}')"
 fi

 # User can specify an sname without @hostname
__HERE_DOC__


   if [ ! -f "${patch_file}" ]; then

	   echo "  Error, patch file (${patch_file}) could not be created." 1>&2
	   exit 80

   fi

   patch_tool=$(which patch 2>/dev/null)

   if [ ! -x "${patch_tool}" ]; then

	   echo "  Error, not patch tool available." 1>&2
	   exit 85

   fi

   if ! ${patch_tool} -u -b us_web -i "${patch_file}"; then

	   echo " Error, the patching of us_web with ${patch_file} failed." 1>&2
	   exit 90

   fi

}



if [ "$1" = "-h" ] || [ "$1" = "--help" ]; then

	echo "${usage}"

	exit 0

fi


# Just to avoid error messages if running from a non-existing directory:
cd /


if [ ! $(id -u ) -eq 0 ]; then

	echo "  Error, this script must be run as root." 1>&2
	exit 5

fi

archive_dir="/tmp"


# Selecting the last line shall yield the highest (hence more recent) version.
#
# Used to rely on alphabetical order, yet did not manage properly x.10 being
# more recent than x.9:
#
#rel_archive=$(/bin/ls -1 ${archive_dir}/us_web-*.tar.gz | tail -1 2>/dev/null)

# Now relying on timestamps (newest first):
rel_archive=$(/bin/ls -t ${archive_dir}/us_web-*.tar.gz | head -n 1 2>/dev/null)

if [ -z "${rel_archive}" ]; then

	echo "  Error, no US-Web release archive found in '${archive_dir}'." 1>&2
	exit 10

fi


archive_version=$(echo ${rel_archive} |sed "s|^${archive_dir}/us_web-||1" | sed 's|.tar.gz$||1')

echo " Detected US-Web version: '${archive_version}'."

archive_name="us_web-${archive_version}.tar.gz"

archive_path="${archive_dir}/${archive_name}"


if [ ! -f "${archive_path}" ]; then

	echo "  Error, US-Web release archive '${archive_path}' is not a file." 1>&2
	exit 15

fi


base_install_dir="/opt/universal-server"

base_install_dir_created=1

# Not leaving it as root either:
if [ ! -d "${base_install_dir}" ]; then

	echo " Creating base directory '{base_install_dir}'."
	/bin/mkdir "${base_install_dir}"
	base_install_dir_created=0

fi

cd "${base_install_dir}"

# Automatic shutdown of any prior US-Web release running:
for d in $(/bin/ls -d us_web-*.*.* 2>/dev/null) ; do

	exec="${d}/bin/us_web"

	#echo "Testing for ${exec}..."

	if [ -x "${exec}" ]; then
		echo " Trying to stop gracefully any prior release in ${d}."
		${exec} stop 1>/dev/null 2>&1
	fi

done


rel_dir="us_web-${archive_version}"

# Not wanting to inherit from remaining elements:
if [ -d "${rel_dir}" ]; then

	echo " Removing already-existing ${rel_dir}."
	/bin/rm -rf ${rel_dir}

fi

mkdir "${rel_dir}"

# Rare option needed, otherwise apparently mistook for a directory resulting in
# an incorrect link:
#
/bin/ln -sf --no-target-directory "${rel_dir}" us_web-latest

cd "${rel_dir}"

# Now we let the archive at its original place:
#/bin/mv -f "${archive_path}" .

if ! tar xzf "${archive_path}"; then

	echo " Error, the archive '${archive_name}' could not be decompressed in '$(pwd)'." 1>&2

	exit 20

fi

rel_root="${base_install_dir}/${rel_dir}"

cd "${rel_root}/lib"

/bin/ln -sf "${rel_dir}" us_web-latest

# To facilitate finding vm.args (ex: for a proper stop w.r.t. cookie):
cd "${rel_root}/releases"

/bin/ln -sf "${archive_version}" latest-release


# Just a check:
rel_exec="${rel_root}/bin/us_web"

if [ -x "${rel_exec}" ]; then

	echo " US-Web release ready in '${rel_exec}'."

else

	echo "Error, no release executable found (no '${rel_exec}' found)." 1>&2
	exit 25

fi


priv_dir="${rel_root}/lib/${rel_dir}/priv"

# Actual cookie managed there:
start_script="${priv_dir}/bin/start-us-web-release.sh"

if [ ! -x "${start_script}" ]; then

	echo "Error, no start script found (no '${start_script}' found)." 1>&2
	exit 30

fi


# We are in ${rel_root} now, so that the us-common.sh script (and thus the
# us_common base first) can be found with relative links from us-web-common.sh:
#
# (so we will be typically in /opt/universal-server/us_web-x.y.z from now on)
cd "${rel_root}"


# Not wanting the files of that US-Web install to remain owned by root, so
# trying to apply a more proper user/group; for that we have to determine them
# from the configuration files:
#
# (will source in turn us-common.sh)
us_web_common_script="${priv_dir}/bin/us-web-common.sh"

if [ ! -f "${us_web_common_script}" ]; then

	echo "Error, unable to find us-web-common.sh script (not found in '${us_web_common_script}')." 1>&2
	exit 35

fi

#echo "Sourcing '${us_web_common_script}' from $(pwd)."
. "${us_web_common_script}" 1>/dev/null

read_us_config_file 1>/dev/null

read_us_web_config_file 1>/dev/null


echo " Changing, from '${rel_root}', the owner of release files to '${us_web_username}' and their group to '${us_groupname}'."

if ! chown --recursive ${us_web_username}:${us_groupname} ${rel_root}; then

	echo "Error, changing user/group owner from '${rel_root}' failed." 1>&2

	exit 40

fi


if [ ${base_install_dir_created} -eq 0 ]; then

	if ! chown ${us_web_username}:${us_groupname} ${base_install_dir}; then

		echo "Error, changing user/group owner of '${base_install_dir}' failed." 1>&2

		exit 45

	fi

fi

# By default, no auto-launch:
do_launch=1

if [ $do_launch -eq 0 ]; then

	echo "### Launching US-Web release now"

	${start_script}
	res=$?

	if [ ! $res -eq 0 ]; then

		echo " Error, start script ('${start_script}') failed (code: $res)." 1>&2

		exit 200

	fi

	sleep 1

	${rel_root}/bin/us_web status

	log_dir="${rel_root}/log"

	# See https://erlang.org/doc/embedded/embedded_solaris.html to understand
	# the naming logic of erlang.log.* files.
	#
	# The goal here is only to select the latest-produced of these rotated log files:
	#
	us_web_vm_log_file=$(/bin/ls -t ${log_dir}/erlang.log.* 2>/dev/null | head -n 1)

	if [ -f "${us_web_vm_log_file}" ]; then

		tail -f "${us_web_vm_log_file}"

	else

		echo "(no VM log file found; tried '${us_web_vm_log_file}')"

	fi

else

	echo "(no auto-launch enabled; one may execute, as root, 'systemctl restart us-web-as-release.service')"

fi
