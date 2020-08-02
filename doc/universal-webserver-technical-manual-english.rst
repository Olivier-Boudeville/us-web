.. _Top:


.. title:: Welcome to the Universal Webserver documentation

.. comment stylesheet specified through GNUmakefile


.. role:: raw-html(raw)
   :format: html

.. role:: raw-latex(raw)
   :format: latex

.. comment Would appear too late, can only be an be used only in preamble:
.. comment :raw-latex:`\usepackage{graphicx}`
.. comment As a result, in this document at least a '.. figure:: XXXX' must
.. exist, otherwise: 'Undefined control sequence \includegraphics.'.


:raw-html:`<a name="universal-webserver_top"></a>`

:raw-html:`<div class="banner"><p><em>Universal Webserver documentation</em> <a href="http://us-web.esperide.org">browse latest</a> <a href="https://olivier-boudeville.github.io/us-web/index.html">browse mirror</a> <a href="universal-webserver-technical-manual-english.pdf">get PDF</a> <a href="#universal-webserver_top">go to top</a> <a href="#universal-webserver_bottom">go to bottom</a> <a href="mailto:about(dash)universal-webserver(at)esperide(dot)com?subject=[Universal%20Webserver]%20Remark">email us</a></p></div>`



:raw-html:`<center><img src="us-web-title.png" width="70%"></img></center>`
:raw-latex:`\includegraphics[scale=1.6]{us-web-title.png}`



===============================================
Technical Manual of the ``Universal Webserver``
===============================================


:Organisation: Copyright (C) 2019-2020 Olivier Boudeville
:Contact: about (dash) universal-webserver (at) esperide (dot) com
:Creation date: Saturday, May 2, 2020
:Lastly updated: Sunday, August 2, 2020
:Status: Work in progress
:Version: 0.0.6
:Dedication: Users and maintainers of the ``Universal Webserver``.
:Abstract:

	The `Universal Webserver <http://us-web.esperide.org/>`_, part of the `Universal Server <https://github.com/Olivier-Boudeville/Universal-Server>`_ umbrella project, provides a multi-domain, multi-virtualhost webserver integrating various web-related services.

	We present here a short overview of these services, to introduce them to newcomers.

	The next level of information is to read the corresponding `source files <https://github.com/Olivier-Boudeville/us-web>`_, which are intensely commented and generally straightforward.


.. meta::
   :keywords: Universal Webserver


:raw-latex:`\pagebreak`

.. contents:: Table of Contents
	:depth: 3


:raw-latex:`\pagebreak`

--------
Overview
--------

We present here a short overview of the services offered by the *Universal Webserver*, to introduce them to newcomers.

The goal of **US-Web** is to provide an integrated, extensible web framework in order:

- to better operate websites based on `virtual hosting <https://en.wikipedia.org/wiki/Virtual_hosting>`_, so that a networked computer can serve as many websites corresponding to as many domains as wanted; this involves reading and interpreting vhost and other configuration information, handling properly 404 errors, producing access logs that are adequate for web analytics, rotating all logs, etc.
- to link to the `Universal Server <https://github.com/Olivier-Boudeville/us-main>`_ optionally (i.e. if available, knowing both should be able to run in the absence of the other), in order to offer a web front-end for it

Beyond this document, the next level of information about US-Web is to read the corresponding `source files <https://github.com/Olivier-Boudeville/us-web>`_, which are intensely commented and generally straightforward.


----------------------
Easy Testing of US-Web
----------------------

Provided that no server already runs at TCP port #8080, just downloading the `get-us-web-from-sources.sh <https://github.com/Olivier-Boudeville/us-web/blob/master/priv/bin/get-us-web-from-sources.sh>`_ script and running it with no specific parameter should suffice.

It should clone, build and run a test server, that should be available at `http://localhost:8080 <`http://localhost:8080>`_.




-----------
Layer Stack
-----------

From the highest level to the lowest, as summarised `here <https://github.com/Olivier-Boudeville/us-web>`_, a software stack involving the Universal Webserver usually comprises:

- the *Universal Webserver* services themselves (i.e. this `US-Web <http://us-web.esperide.org/>`_ layer)
- `Cowboy <https://github.com/ninenines/cowboy>`_ (for a small, fast and modern web framework)
- [optional] `Awstats <http://www.awstats.org/>`_ (for the analysis of access log files)
- `US-Common <http://us-common.esperide.org/>`_ (for US base facilities)
- `Ceylan-Traces <http://traces.esperide.org>`_ (for advanced runtime traces)
- `Ceylan-WOOPER <http://wooper.esperide.org>`_ (for OOP)
- `Ceylan-Myriad <http://myriad.esperide.org>`_ (as an Erlang toolbox)
- `Erlang <http://erlang.org>`_ (for the compiler and runtime)
- `GNU/Linux <https://en.wikipedia.org/wiki/Linux>`_

The shorthand for ``Universal Webserver`` (a.k.a. ``US-Web``) is ``uw``.

:raw-latex:`\pagebreak`




-----------------
Server Deployment
-----------------

For that the ``prod`` profile defined in the context of rebar3 shall be used.

Currently we prefer re-using the (supposedly already installed) local Erlang environment on the server (to be shared across multiple services), so by default ERTS is not included in a US-Web release.

Sources are not included either, as we prefer re-rolling a release to editing and compiling code directly on a server.

To generate from scratch such a (mostly) standalone release, one may use::

 $ make release-prod

It should generate a tarball such as ``us_web-x.y.z.tar.gz``


The ``export-release`` make target allows in the same movement to lightly update a pre-existing release and also to transfer it to any target server, designated by setting the ``WEB_SRV`` (make or environment) variable to the FQDN of this server.

So we recommend running::

 $ make export-release
  Generating rebar3 us_web.app file
  Compiling us_web from XXX/us_web
  ===> Verifying dependencies...
  ===> Compiling myriad
  Hiding unsupported sources
  Populating the include symlinks
  [...]


We recommend installing a release in ``REL_BASE_ROOT=/opt/universal-server``::

 $ mv /tmp/us_web-x.y.z.tar.gz ${REL_BASE_ROOT}
 $ cd ${REL_BASE_ROOT}
 $ tar xvf us_web-x.y.z.tar.gz

Then various steps are necessary in order to have a functional release running satisfactorily.

We automated the full deployment process of US-Web on a server for that: once the release has been transferred to that server (possibly thanks to the aforementioned ``export-release`` target, possibly to the ``/tmp`` directory of that server), one may rely on our `deploy-us-web-release.sh <https://github.com/Olivier-Boudeville/us-web/blob/master/priv/bin/deploy-us-web-release.sh>`_ script. One may also just take inspiration from it in order to devise one's deployment scheme.

Let's say from now on that the UNIX name chosen for the US user is ``us-user``, the one of the US-web user is ``us-web-user`` and the US group (containing both users, and possibly only them) is ``us-group``.

Using that script boils down to running, as root::

 $ /tmp/deploy-us-web-release.sh
 Detected US-Web version: 'x.y.z'.
 Trying to stop gracefully any prior release in us_web-x.y.z.
 Removing already-existing us_web-x.y.z.
 US-Web release ready in '/opt/universal-server/us_web-x.y.z/bin/us_web'.
 Changing, from '/opt/universal-server/us_web-x.y.z', the owner of release
 files to 'us-web-user' and their group to 'us-group'.
 (no auto-launch enabled)

Note that the goal of that deployment phase is to start from a clean state, and as such it will try to stop any already running US-Web instance (for all possible versions thereof).

Then US-Web is fully *deployed*. Once properly *configured*, it will be able to be *launched* for good.

Some related information are specified below.



-----------------------------------
Configuring the Universal-Webserver
-----------------------------------

As explained in `start-us-web.sh <https://github.com/Olivier-Boudeville/us-web/blob/master/priv/bin/start-us-web.sh>`_ and in `class_USWebConfigServer.erl <https://github.com/Olivier-Boudeville/us-web/blob/master/src/class_USWebConfigServer.erl>`_, the US configuration files will be searched through various locations.

The main, overall US configuration file (``us.config``) is found based on a series of directories that are gone through in an orderly manner; the first directory to host such a file becomes *the* US configuration directory.

The other US-related configuration files (ex: any ``us-web.config``) are specified directly in the main one (``us.config``), through specific keys (ex: ``us_web_config_filename``); they may be either absolutely defined, or relatively to the US configuration directory.

Let's name ``US_CFG_ROOT`` the actual directory in which they all lie; it is typically either ``~/.config/universal-server/`` (in development mode), or ``/etc/xdg/universal-server/`` (in production mode).

Note that, as these files might contain sensitive information (ex: Erlang cookies), they shall be duly protected.

Then we should have in terms of permissions, supposing the ``us.config`` designates, in its ``us_web_config_filename`` entry, ``foobar-us-web-for-production.config`` as the name of the US-Web configuration file [#]_, ``640``::

 -rw-r----- 1 us-user     us-group [...] us.config
 -rw-r----- 1 us-web-user us-group [...] foobar-us-web-for-production.config

.. [#] They shall be in the same ``US_CFG_ROOT`` directory (discussed below), and may be symbolic links.



In a Development Setting
========================

The US main configuration file, ``us.config``, is in a directory (``US_CFG_ROOT``) that is ``~/.config/universal-server/`` here. This US-level configuration file will reference a US-Web counterpart configuration file, probably in the same directory.

The US-Web configuration file may define a ``us_web_app_base_dir`` entry. If not, this application directory will then be found thanks to the ``US_WEB_APP_BASE_DIR`` environment variable (if defined, typically through ``~/.bashrc``); otherwise, as a last resort, an attempt to guess it will be done.

The US webserver may be then run thanks to ``make debug``, from the relevant ``us_web`` directory (typically the root of a GIT clone located in the user's directory).

In such a development context, in ``us_web/conf/sys.config``, we recommend to leave the batch mode disabled (just let the default ``{is_batch,false}``), so that a direct, graphical trace supervision is enabled (provided that a relevant trace supervisor is available, see `Traces <http://traces.esperide.org/#trace-supervision-browsing>`_ for that).



In a Production Setting
=======================

The start/stop management scripts will be run initially as root (possibly through ``systemd``) and must access the ``us.config`` file. Then, once run, ``us_web`` will most probably switch to a dedicated user (see the ``us_web_username`` entry in the US-Web configuration file), who will need in turn to be able to read the ``us.config`` file and any related one (ex: for US-Web, here supposed to be named ``foobar-us-web-for-production.config``).

As a result, a relevant configuration directory (denoted ``US_CFG_ROOT`` in this document), in that shared setting, is the standard ``/etc/xdg`` one, resulting in the ``/etc/xdg/universal-server`` directory to be used.

As mentioned, care must be taken so that ``root`` and also the US and US-Web users can read the content of that directory - at least the US and US-Web configuration files in it - and that the other users cannot.

For that, a dedicated ``us-group`` group can be created, and any web user (ex: ``us-web-user``) shall belong to that group. For example::

 $ id us-web-user
 uid=1002(us-web-user) gid=1002(us-web-user) groups=1002(us-web-user),
  1007(us-group)


Then, in ``/etc/xdg/universal-server``, for the US and US-Web configuration files::

 $ chown us-user us.config
 $ chown us-web-user foobar-us-web-for-production.config

 $ us_files="us.config foobar-us-web-for-production.config"
 $ chgrp us-group ${us_files}
 $ chmod 640 ${us_files}

 $ chgrp us-group /etc/xdg/universal-server
 $ chmod 700 /etc/xdg/universal-server

We recommend directly setting the ``us_web_app_base_dir`` configuration entry to the relevant, absolute path.

Let's name here ``US_WEB_REL_ROOT`` the root of the US-Web release of interest (ex: corresponding to ``${REL_BASE_ROOT}/us_web-latest/``) and ``US_WEB_APP_ROOT`` the root of the corresponding US-Web application (ex: corresponding to ``${US_WEB_REL_ROOT}/lib/us_web-latest/``).

A ``systemd`` service shall be declared for US-Web, in ``/etc/systemd/system``; creating there, as root, a symbolic link to ``${US_WEB_APP_ROOT}/priv/conf/us-web.service`` will suffice.

This service requires ``start-us-web.sh`` and ``stop-us-web.sh``. Adding for user convenience ``get-us-web-status.sh``, they should all be symlinked that way, still as root::

 $ cd /usr/local/bin
 $ for f in start-us-web.sh stop-us-web.sh get-us-web-status.sh; \
   do ln -s ${US_WEB_APP_ROOT}/priv/bin/$f ; done


The log base directory (see the ``log_base_directory`` entry) shall be created and writable; for example::

 $ LOG_DIR=/var/log/universal-server
 $ mkdir -p ${LOG_DIR}
 $ chown us-user ${LOG_DIR}
 $ chgrp us-group ${LOG_DIR}
 $ chmod 770 ${LOG_DIR}


In such a production context, in ``sys.config`` (typically located in ``${US_WEB_REL_ROOT}/releases/latest-release``), we recommend to enable batch mode (just set ``{is_batch,true}``), so that by default no direct, graphical trace supervision is triggered (a server usually does not have a X server anyway).

Instead the traces may then be supervised and browsed remotely (at any time, and any number of times), from a graphical client (provided that a relevant trace supervisor is available locally, see `Traces <http://traces.esperide.org/#trace-supervision-browsing>`_ for that), by running the `monitor-us-web.sh <https://github.com/Olivier-Boudeville/us-web/blob/master/priv/bin/monitor-us-web.sh>`_ script.

For that the relevant settings (notably which server host shall be targeted, with what cookie) shall be stored in that client, in a ``us-monitor.config`` file that is typically located in the ``~/.config/universal-server`` directory.




-------------------------------
Running the Universal-Webserver
-------------------------------

Note that the Erlang versions used to produce the release (typically in a development computer) and run it (typically in a production server) must match.

Supposing a vhost to be served by US-Web is ``baz.foobar.org``, to avoid being confused by your browser, a better way is to test whether a US-Web instance is already running thanks to ``wget`` or ``links``::

 $ wget http://baz.foobar.org -O -


This will display the fetched content directly on the console (not creating a file).

Indeed, when testing a website, fully-fledged browsers such as Firefox may be quite misleading as they may attempt to hide issues, and tend to cache a lot of information (not actually reloading the current page), even if the user held Shift and clicked on "Reload". Do not trust browsers!

One may disable temporarily the cache by opening the developer toolbox (``Ctrl+Shift+I`` or ``Cmd+Opt+I`` on Mac), clicking on the settings button (near the top right), scrolling down to the ``Advanced settings`` (on the bottom right), checking the option ``Disable Cache (when toolbox is open)`` and refreshing the page. ``wget`` may still be a better, simpler, more reliable solution.



Stopping any prior instance first
=================================

From now on, we will suppose the current directory is ``US_WEB_APP_ROOT``.

The ``stop-us-web.sh`` script can be used for stopping a US-Web release, typically simply as::

 $ priv/bin/stop-us-web.sh


In development mode, still from the root of US-Web, one might use ``make stop-brutal`` to operate regardless of dynamically-changed cookies, while in a production setting one would go preferably for::

 $ systemctl stop us-web.service



Launching the US-Web Server
===========================

In development mode, from the root of US-Web, one may use ``make debug``, while, in production mode, the US-Web server can be launched either with its dedicated script ``start-us-web.sh`` or, even better, directly through::

 $ systemctl start us-web.service



----------------------------
Monitoring the US-Web Server
----------------------------



Local Monitoring
================

Here operations will be done directly on the server.


Overall Local Inquiry
---------------------

The ``get-us-web-status.sh`` script (still in ``priv/bin``, as all US-Web shell scripts) may then be used to investigate any problem in a streamlined, integrated way.

Alternate (yet often less convenient) solutions are to run ``systemctl status us-web.service`` or, often with better results, ``journalctl -xe --unit us-web.service --no-pager`` to get some insights.


General Logs
------------

A deeper level of detail can be obtained by inspecting the *general* logs (as opposed to the *webserver* ones, discussed in next section), which regroup the VM-level, technical ones and/or the applicative ones.

**VM logs** are written in the ``${REL_BASE_ROOT}/log`` directory (ex: ``/opt/universal-server/us_web-latest/log``), which is created when the release is started first. Generally, ``run_erl.log`` (if launched as a daemon) will not help much, whereas the latest Erlang log file (``ls -lrt erlang.log.*``) is likely to contain relevant information.

As for our higher-level, **applicative traces**, they are stored in the the ``us_web.traces`` file, in the directory defined in the ``us_web_log_dir`` entry of the US-Web configuration file. This last file is specified in turn in the relevant ``us.config`` configuration file (see the ``us_web_config_filename`` key for that), which, in development mode, is itself typically found in ``~/.config/universal-server`` while, in production mode, is likely located in ``/etc/xdg/universal-server``.

In practice, this trace file is generally found:

- in development mode, in ``priv/for-testing/log``, relatively to the base directory specified in ``us_app_base_dir``
- in production mode, generally in ``/var/log/universal-server``



Webserver Logs
--------------

These logs, which are maybe less useful for troubleshooting, designate access and error logs, per virtual host (ex: for a virtual host ``VH``, ``access-for-VH.log`` and ``error-for-VH.log``). Their previous versions are archived in timestamped, compressed files (ex: ``error-for-VH.Y-M-D-at-H-M-S.xz``).

These files will be written in the directory designated by the ``us_web_log_dir`` entry of the US-Web configuration file. Refer to the previous section to locate this file.



Remote Monitoring
=================

Here the state of a US-Web instance remotely, with no shell connection to its server.

First of all, is this US-Web instance available? Check with::

 $ wget http://baz.foobar.org -O -

As for the applicative traces, they may be monitored remotely as well, thanks to the ``monitor-us-web.sh`` script.





--------------
Extra Features
--------------



Auto-generated Meta Website
===========================

If requested, at server start-up, a "meta" website - i.e. a website sharing information about the other websites being hosted by that server - can be generated and made available through a dedicated virtual host and web root.

For that, in the US-Web configuration file, among the user-specified ``routes``, one may add the following element in the list of virtual host entries associated to a given domain (ex: ``foobar.org``)::

 {"mymeta", "My-meta-generated", meta}


This will generate a suitable website in the ``My-meta-generated`` subdirectory of the default web root (as, here, the specified directory is not an absolute one), and this website will be available as ``mymeta.foobar.org`` (of course both ``mymeta`` and ``My-meta-generated`` are examples; these names can be freely chosen).

Currently no specific access control is enforced for this website (thus by default anyone knowing or able to guess its virtual hostname can access this website).



Icon (favicon) Management
=========================

This designates the little image that is displayed by browsers on the tab of the website being visited.

A default icon file can be defined, it is notably used in order to generate better-looking 404 pages.

To do so, the ``icon_path`` key in the US-Web handler state shall be set to the path of such file (possibly a symbolic link), relatively to the content root of the corresponding virtual host.

In the context of the (default) US-Web static web handler, if such a ``common/default-icon.png`` exists (ex: obtained thanks to this kind of `generator <https://favicon.io/favicon-generator/>`_), it is automatically registered in ``icon_path``.



CSS Management
==============

A default CSS file can be defined, notably in order to generate better-looking 404 pages.

To do so, the ``css_path`` key in the US-Web handler state shall be set to the path of such file (possibly a symbolic link), relatively to the content root of the corresponding virtual host.

In the context of the (default) US-Web static web handler, if such a ``common/default.css`` exists, it is automatically registered in ``css_path``.



Error 404 Management
====================

Should some requested web page not be found:

- a suitable 404 page is automatically generated by the US-Web server, and returned to the caller
- the error is appropriately logged

A 404 image can be optionally displayed instead of the "0" of "404". To do so, the ``image_404`` key in the US-Web handler state shall be set to the path of such image (possibly a symbolic link), relatively to the content root of the corresponding virtual host.

In the context of the (default) US-Web static web handler, if such a ``images/404.png`` exists, it is automatically registered in ``image_404``.



Site Customisation
==================

As mentioned in the sections above, if there is a file (regular or symbolic link), from the content root of a hosted static website, in:

-  ``common/default.css``, then this CSS will be used for all generated pages (ex: the one for errors such as 404 ones)
-  ``images/default-icon.png``, then this image will be used as an icon (the small image put on browser tabs next to their labels) for all generated pages
-  ``images/404.png``, then this image will be used for the "0" in the main "404" label of the page denoting a content not found

Each content root is expected to contain at least a (proper) ``index.html`` file (possibly a symbolic link).



---------------------
Usage Recommendations
---------------------

In terms of security, we would advise:

- to stick to the **latest stable version** of all software involved (including US-Web and all its stack, Erlang, and the operating system itself)

- to apply a streamlined, reproducible **deployment process**, preferably based on our `deploy-us-web-release.sh <https://github.com/Olivier-Boudeville/us-web/blob/master/priv/bin/deploy-us-web-release.sh>`_ script

- to rely on dedicated, **different, low-privileged users and groups** for US and US-Web, which both rely on ``authbind``; refer to our `start-us-web.sh <https://github.com/Olivier-Boudeville/us-web/blob/master/priv/bin/start-us-web.sh>`_ script for that; see also the ``us_username`` key of US-Common's `us.config <https://github.com/Olivier-Boudeville/us-common/blob/master/priv/for-testing/us.config>`_, and the ``us_web_username`` key of the US-Web configuration file it refers to

- still in ``us.config``, to set:

  - a strong-enough Erlang **cookie**: set the ``vm_cookie`` key to a well-chosen value, possibly a random one deriving from an output of ``uuidgen``
  - possibly a **limited TCP port range** (see the ``tcp_port_range`` key)
  - the **execution context** to ``production`` (see the ``execution_context`` key)

- to use also the `stop-us-web.sh <https://github.com/Olivier-Boudeville/us-web/blob/master/priv/bin/stop-us-web.sh>`_ counterpart script, and to have them triggered through ``systemd``; we provide a corresponding `us-web.service <https://github.com/Olivier-Boudeville/us-web/blob/master/conf/us-web.service>`_ **unit file** for that, typically to be placed in ``/etc/systemd/system`` and whose ``ExecStart/ExecStop`` paths shall preferably be symlinks pointing to the latest deployed US-Web release (ex: ``/opt/universal-server/us_web-latest``)

- to ensure that a **firewall** blocks everything from the Internet by default, including the EPMD port(s) (i.e. both the default Erlang one and any non-standard one specified through the ``epmd_port`` key defined in ``us.config``); one may get inspiration from our `iptables.rules-Gateway.sh <https://github.com/Olivier-Boudeville/Ceylan-Hull/blob/master/iptables.rules-Gateway.sh>`_ script for that

- no need to advertise specifically a virtual host in your DNS; for example, so that a ``baz.foobar.org`` is available, only ``foobar.org`` has to be declared in the DNS records (even a ``*.foobar.org`` wildcard is not necessary) for the corresponding website to be available; as a result, unless the full name of that virtual host is disclosed or a (typically brute-force) guessing succeeds, that virtual host will remain private by default (useful as a first level of protection, notably for any meta website)

- to **monitor regularly** both:

  - the US-Web server itself (see our `monitor-us-web.sh <https://github.com/Olivier-Boudeville/us-web/blob/master/priv/bin/monitor-us-web.sh>`_ script for that, relying on the trace supervisor provided by the `Ceylan-Traces <http://traces.esperide.org>`_ layer)
  - the remote, browser-based, accesses made to the hosted websites, typically by enabling the US-Web "meta" feature, generating and updating automatically a dedicated website displaying in one page all hosted websites and linking to their web analysis report; refer to the ``log_analysis`` key of the US-Web configuration file (ex: see `us-web-for-tests.config <https://github.com/Olivier-Boudeville/us-web/blob/master/priv/for-testing/us-web-for-tests.config>`_ as an example thereof)


.. _`free software`:


-------
Licence
-------

The ``Universal Webserver`` is licensed by its author (Olivier Boudeville) under the `GNU Affero General Public License <https://www.gnu.org/licenses/agpl-3.0.en.html>`_ as published by the Free Software Foundation, either version 3 of this license, or (at your option) any later version.

This allows the use of the Universal Webserver code in a wide a variety of software projects, while still maintaining copyleft on this code, ensuring improvements are shared.

We hope indeed that enhancements will be back-contributed (ex: thanks to merge requests), so that everyone will be able to benefit from them.



---------------------------------
Current Stable Version & Download
---------------------------------

As mentioned, the single, direct prerequisites of the `Universal Webserver <https://github.com/Olivier-Boudeville/Universal Webserver>`_ are:

- `Cowboy <https://github.com/ninenines/cowboy>`_ (version 2.8 or above)
- `Awstats <http://www.awstats.org/>`_ as an optional, runtime-only dependency (version 7.8 or above)
- `US-Common <http://us-common.esperide.org/>`_

The latter relies on `Ceylan-Traces <https://github.com/Olivier-Boudeville/Ceylan-Traces>`_, which implies in turn `Ceylan-WOOPER <https://github.com/Olivier-Boudeville/Ceylan-WOOPER>`_, then `Ceylan-Myriad <https://github.com/Olivier-Boudeville/Ceylan-Myriad>`_ and `Erlang <http://erlang.org>`_.

We prefer using GNU/Linux, sticking to the latest stable release of Erlang, and building it from sources, thanks to GNU ``make``.

We recommend indeed obtaining Erlang thanks to a manual installation; refer to the corresponding `Myriad prerequisite section <http://myriad.esperide.org#prerequisites>`_  for more precise guidelines.

The build of the US-Web server is driven by `rebar3 <https://www.rebar3.org/>`_, which can be obtained by following our `guidelines <http://myriad.esperide.org/#getting-rebar3>`_.

.. _getting-awstats:

If a tool for web analysis is needed (typically if enabling a meta website), this tool must be installed beforehand. Currently US-Web supports Awstats, which can be obtained thanks to your distribution of choice (ex for Arch Linux: ``pacman -S awstats`` [#]_) .

.. [#] To avoid a future reading access error, execute after installation: ``chmod -R +r /usr/share/webapps/awstats/icon``.


If wanting to be able to operate on the source code of the `Ceylan <https://github.com/Olivier-Boudeville/Ceylan>`_ and/or `US <https://github.com/Olivier-Boudeville/Universal-Server>`_ dependencies, you may define appropriate symbolic links in a ``_checkouts`` directory created at the root one's ``US-Web`` clone, these links pointing to relevant GIT repositories (see the ``create-us-web-checkout`` make target for that).




Using Cutting-Edge GIT
======================

This is the installation method that we use and recommend; the Universal Webserver ``master`` branch is meant to stick to the latest stable version: we try to ensure that this main line always stays functional (sorry for the pun). Evolutions are to take place in feature branches and to be merged only when ready.

Once Erlang (see `here <http://myriad.esperide.org/index.html#getting-erlang>`_), rebar3 (see `here <http://myriad.esperide.org/index.html#getting-rebar3>`_) and possibly Awstats (see `here <#getting-awstats>`_) are available, it should be just a matter of executing our `get-us-web-from-sources.sh <https://github.com/Olivier-Boudeville/us-web/tree/master/priv/bin/get-us-web-from-sources.sh>`_ script for downloading and building all dependencies at once, and run a test server (use its ``--help`` option for more information.

For example:

.. code:: bash

  $ cd /tmp
  $ wget https://raw.githubusercontent.com/Olivier-Boudeville/us-web/master/priv/bin/get-us-web-from-sources.sh
  $ sh ./get-us-web-from-sources.sh --checkout
  Switching to checkout mode.

   Installing US-Web in /tmp...

   Cloning into 'us_web'...
   [...]
   ===> Compiling us_web
   Starting the us_web release (EPMD port: 4526):
   [...]
   US-Web launched, please point a browser to http://localhost:8080 to
	check test sites.

  $ firefox http://localhost:8080 &


One shall then see a text-only page such as::

 This is static website D. This is the one you should see if pointing
 to the default virtual host corresponding to the local host. This
 shows that the US-Web server is up and running.


Understanding the role of the `main US configuration file <https://github.com/Olivier-Boudeville/us-common/blob/master/priv/for-testing/us.config>`_ and of the corresponding `US-Web configuration file <https://github.com/Olivier-Boudeville/us-web/blob/master/priv/for-testing/us-web-for-tests.config>`_ for this test should be fairly straightforward.

Based on that, devising one's version of them should allow to have one's US-Web server running at the cost of very little efforts.



:raw-html:`<a name="otp"></a>`

.. _`otp-build`:

OTP Considerations
==================

As discussed in these sections of `Myriad <http://myriad.esperide.org/myriad.html#otp>`_, `WOOPER <http://wooper.esperide.org/index.html#otp>`_, `Traces <http://traces.esperide.org/index.html#otp>`_ and `US-Common <http://us-common.esperide.org/index.html#otp>`_, the Universal Webserver *OTP application* is generated out of the build tree, ready to result directly in an *(OTP) release*. For that we rely on `rebar3 <https://www.rebar3.org/>`_, `relx <https://github.com/erlware/relx>`_ and (possibly) `hex <https://hex.pm/>`_.

Then we benefit from a standalone, complete Universal Webserver able to host as many virtual hosts on any number of domains as needed.

As for Myriad, WOOPER, Traces and US-Common, most versions of the Universal Webserver will be also published as `Hex packages <https://hex.pm/packages/us_web>`_.

For more details, one may have a look at `rebar.config.template <https://github.com/Olivier-Boudeville/us-web/blob/master/conf/rebar.config.template>`_, the general rebar configuration file used when generating the Universal Webserver OTP application and release (implying the automatic management of all its dependencies).



---------------
Troubleshooting
---------------

If having deployed a release (typically by running ``deploy-us-web-release.sh``) and ``systemctl restart us-web.service`` failed, start by executing::

 $ systemctl status us-web.service

It should return some appropriate information.

Most common sources of failures are:

- there is **already a program listening at the target TCP port** (typically port 80) designated for US-Web; one may check for example with ``lsof -i:80``, otherwise with ``netstat --tcp --all --program | grep ':80'``
- there may be a **prior, lingering US-Web** installation that is still running in the background; one may check for example with ``ps -edf |grep us_web``
- the **EPMD daemon** of interest (possibly running on a non-standard TCP port) may wrongly believe that a prior US-Web is running, and thus prevent a new one to be launched; simple solution: ``killall epmd``

If the problem remains, time to perform some investigation, refer to the `Local Monitoring`_ section.



-----
Hints
-----

Various keys (ex: ``us_app_base_dir``) may be defined in the US configuration files (ex: in a ``{us_app_base_dir, "/opt/some_dir"}`` entry), from which other elements may derive (ex: paths). To denote the value associated to a key, we surround in this documentation the key with ``@`` characters.

For example ``@us_app_base_dir@/hello.txt`` would correspond here to ``/opt/some_dir/hello.txt``.



Development vs Production Mode
==============================

Should a mismatch be detected between the compile-time execution target and the runtime, user-configured, execution context, a warning will be issued in the traces.

When building a fresh release thanks to ``make release-dev``, the corresponding action (``rebar3 release``) will build that release with the base settings, hence in development mode (so not "as prod" - i.e. not with the ``prod`` profile, hence not selecting our ``production`` execution target).

Note that all dependencies (thus of course including Myriad, WOOPER and Traces) are built by rebar3 with their ``prod`` settings. As a result, relying on ``basic_utils:get_execution_target/0`` will only tell us about ``Myriad`` (thus always built in production mode), not any other layer (including ``US-Web``). A US-Web trace allows to check all relevant modes, notably that, in production, the production settings apply indeed.




Development Hints
=================


Operating directly from within the rebar build tree
---------------------------------------------------

(not recommended in a development phase)

If having modified and recompiled a Ceylan prerequisite (ex: WOOPER), then, before generating the release, run from its root (ex: ``us_web/_build/default/lib/wooper``)::

  $ make rebar3-copy-beams REBAR_BUILD_DIR=../../

Or, simply run ``make rebar3-compile REBAR_BUILD_DIR=../../`` (and no need to run ``make release`` afterwards).



Operating from ``_checkouts`` build trees
-----------------------------------------

(recommended in a development phase, as a lot more flexible/unified than the previous method)

Create a ``us_web/_ckeckouts`` directory containing symbolic links to repositories of dependencies (ex: ``myriad``) that may be updated locally.

.. Then, when needing to repopulate the suitable ``ebin`` directory:



Configuration Hints
===================


Batch Mode
----------

One can update ``us_web/conf/sys.config`` in order to toggle batch mode (ex: to enable/disable a graphical trace supervision) after build time.

It deals with the configuration of the ``Traces`` application, so it comes very early at start-up (at application boot, hence before US-Web specific settings can be read, so this option would be difficult to put in the US configuration files).



Location of Applications
------------------------

The location of the US-Web application is bound to differ depending on the context of use (development/production deployments, host-wise, etc.), whereas it is needed to be known at the very least by its start/stop scripts.

So its path must be specified (most preferably as an absolute directory) in the US-Web configuration file. However, at least for test configuration files, relying on an absolute directory would not be satisfactory, as the user may install the US-Web framework at any place and testing should not require a manual configuration update.

As a result, should an application location (ex: US-Web) not be specified in the configuration, it will be looked-up in the shell environment (using the ``US_WEB_APP_BASE_DIR`` variable) for that and, should this variable not be set, as a last-resort an attempt to guess that location will be done.



Web-related hints
-----------------

- most paths (ex: ``default_web_root``, in the US-Web configuration) can be defined as **relative** ones (mostly useful for embedded tests; otherwise absolute paths shall be preferred); in this case they will be relative to the runtime current directory, typically ``[...]/us_web/_build/default/rel/us_web/`` in development mode
- the ``default_domain_catch_all`` atom allows to designate any **domain-level host** (ex: ``foobar.org``) that did not match previous host rules
- in the context of a given host (ex: ``foobar.org``), the ``default_vhost_catch_all`` atom allows to designate any of its **virtual hosts** (ex: ``bar``, to be understood as ``bar.foobar.org``) that did not match previous path rules
- refer to ``us_web/priv/for-testing`` for an example setup and configuration files
- the web roots shall be owned by the user running US-Web (ex: ``chown -R us-web-user:us-group /opt/www``)


.. comment Deprecated now that these settings are enforced through the US configuration files:

.. comment - update ``build/default/rel/us_web/releasesx.y.z/vm.args`` for example to change  the node name to avoid clashes
.. comment - set a different EPMD port if a prior EPMD daemon is running with a different user (ex: ``us-web-user``), possibly on a default port (Erlang: ``4369``, redefined by Myriad and above to: ``4526``); to do so, use for example ``export ERL_EPMD_PORT=4469 ; make start`` or, preferable, define or change in ``GNUmakevars.inc``, ``EPMD_PORT``





Execution Hints
===============

- the current working directory of a US-Web instance deployed thanks to ``deploy-us-web-release.sh`` is ``/opt/universal-server/us_web-x.y.z``

- if unable to connect, the firewall (ex: ``iptables -L``) might be the culprit! Note that the whole US framework tends to rely on a specific TCP range (ex: ``50000-55000``) for inter-VM communications

- to debug (once batch mode has been enabled/disabled), one may use the ``debug`` make target, from the tree root

- to test server-side errors, one may create, in a web root, a directory that cannot be traversed (ex: ``chmod 000 my-dir``) and direct one's browser to a content supposedly located in this directory [#]_; note that, if requesting instead that faulty directory itself (not an element within), then (whether or not a trailing ``/`` is specified), an error 403 (``ERROR 403: Forbidden``) is returned instead (corresponds to the ``case A`` in the corresponding sources)

- to test host matching (ex: for a ``baz`` virtual host), including the default catch-all even on a computer having no specific public DNS entries, one may simply add in ``/etc/hosts`` entries like::

	127.0.0.1 foobar-test.net baz.foobar-test.net other.foobar-test.net


.. [#] See ``priv/for-testing/test-static-website-A/to-test-for-errors``, which was created precisely for that. Note that its permissions have been restored to sensible values, as otherwise that directory was blocking the OTP release generation.


- log rotation results in timestamped, compressed files such as ``access-for-bar.localhost.log.2019-12-31-at-22h-03m-35s.xz``; note that the timestamp corresponds to the moment at which the rotation took place (hence not the time range of these logs, more an upper bound of it)





Monitoring Hints
================


In terms of (UNIX) Processes
----------------------------

A running US-Web server will not be found by looking up ``beam`` or ``beam.smp`` through ``ps``; as an OTP release, it relies first on the ``run_erl`` launcher, like shown, based on ``ps -edf``, in::

 UID         PID    PPID  C STIME TTY TIME     CMD
 us-web-user 767067    1  0 Feb15 ?   00:00:00 /usr/local/lib/erlang/erts-x.y/bin/run_erl
   -daemon /tmp/erl_pipes/us_web@MY_FQDN/ /opt/universal-server/us_web-US_WEB_VERSION/log
   exec "/opt/universal-server/us_web-US_WEB_VERSION/bin/us_web" "console" ''
   --relx-disable-hooks

This can be interpreted as:

- not running as root, but as a dedicated, weakly privileged user
- its parent process (PPID) is the first overall process (as a daemon)
- STIME is the time when the process started
- no associated TTY (runs detached in the background)

This launcher created the main, central, ``us_web`` (UNIX) process, parent of all the VM worker (system) threads.

``pstree -u`` (or ``ps -e --forest``) tells us about the underlying process hierarchy::

 [...]
   |-run_erl(us-web-user)---beam.smp---erl_child_setup---inet_gethost---inet_gethost
   |                             |-158*[{beam.smp}]
 [...]


The 158 threads must correspond to:

- 128 async ones (``-A 128``)
- 30 "normal" threads (on a computer having a single CPU with 8 cores with Hyperthreading, hence 16 logical cores)

Using ``htop``, one can see that the ``run_erl`` process spawned a ``us_web`` one (namely ``/opt/universal-server/us_web-US_WEB_VERSION/bin/us_web``) that is far larger in (VIRT) memory (ex: 5214MB, instead of 5832KB for the former).

``us_web`` in turn created the numerous threads.

``RSS/RSZ`` (*Resident Set Size*) is a far better metric than ``VIRT/VSZ`` (*Virtual Memory Size*); indeed ``VIRT = RSS + SWP`` and:

 - ``RSS`` shows how much memory is allocated to that process and is in RAM; it does not include memory that is swapped out; it includes memory from shared libraries (as long as the pages from those libraries are actually in memory), and all stack and heap memory used
 - ``VIRT`` includes all memory that the process can access, including memory that is swapped out, memory that is allocated but not used, and memory that is from shared libraries

Knowing that with ``ps`` one may add ``-L`` to display thread information and ``-f`` to have full-format listing, a base command to monitor the US-Web processes is: ``ps -eww -o rss,pid,args | grep us_web``, with:

 - ``-e``: select all processes
 - ``-w`` (once or twice): request wide output
 - ``-o rss,pid,args``: display RSS memory used (in kilobytes), PID and full command line


(apparently there is no direct way of displaying human-readable sizes)


See also our `list-processes-by-size.sh <https://github.com/Olivier-Boudeville/Ceylan-Hull/blob/master/list-processes-by-size.sh>`_ script; typical use::

 $ list-processes-by-size.sh
	Listing running processes by decreasing resident size in RAM (total size in KiB):
  RSS  PID      COMMAND
 [...]
 1204  1695242  /usr/local/lib/erlang/erts-11/bin/run_erl -daemon /tmp/erl_pipes/us_web@somehost.foobar.org/ /opt/universal-server/us_web-x.y.z/log exec "/opt/universal-server/us_web-x.y.z/bin/us_web" "console" ''  --relx-disable-hooks
 67776 1695243 /opt/universal-server/us_web-x.y.z/bin/us_web -A 128 -- -root /opt/universal-server/us_web-x.y.z -progname opt/universal-server/us_web-x.y.z/bin/us_web -- -home /home/us-web-user -epmd_port 4526 -- -boot /opt/universal-server/us_web-x.y.z/releases/x.y.z/start -mode embedded -boot_var ERTS_LIB_DIR /usr/local/lib/erlang/lib -config /opt/universal-server/us_web-x.y.z/releases/x.y.z/sys.config -name us_web -setcookie us_web_initial_cookie -- -- console --relx-disable-hooks --
 [...]

This confirms that, even if higher VIRT sizes can be reported (ex: 5214M, hence roughly 5GB), the RSS size may be 67776 (KB, hence 66 MB), i.e. very little, and does not vary much.

Indeed, in terms of RSS use (for a few domains, each with a few low-traffic websites, if that matters), we found:

 - at start-up: only about 67MB
 - after 5 days: just 68 MB
 - after 24 days: 76 MB
 - after 55 days: 88 MB



Trace Monitoring
----------------

Use the ``us_web/priv/bin/monitor-us-web.sh`` script in order to monitor the traces of an already running, possibly remote, US-Web instance.

Note that the monitored US-Web instance will be by default the one specified in any ``us-monitor.config`` file located in the US configuration directory.

One may specify on the command-line another configuration file if needed, such as ``us-monitor-for-development.config``.



Node Test & Connection
----------------------

If desperate enough, one may also run, possibly from another host, and based on the settings to be found in the configuration files::

 $ ERL_EPMD_PORT=XXXX erl -name tester -setcookie CCCC -kernel inet_dist_listen_min MIN inet_dist_listen_max MAX

 1> net_adm:ping('us_web@foobar.org').
 pong

Then one may switch to the *Job control mode* (JCL) by pressing ``Ctrl-G`` then ``r`` to start a remote job on the US-Web node.






.. include:: us-web-access-logging.rst

..
  Not relevant here:
  .. include:: us-geolocation.rst


.. Not ready yet: include us-certificates.rst



--------------------
Planned Enhancements
--------------------

- **https support**: certificate management (based on LetsEncrypt in Erlang, and regularly renewed thanks to the embedded scheduler)
- **Nitrogen** and/or **Zotonic** support, in addition to static websites


-------------------------------
About Nitrogen (future) Support
-------------------------------

- "*It is strongly recommended to catch static files with the static_paths setting. simple_bridge does not serve large static files in an optimal way (it loads the files into memory completely before sending)*"
- `How Nitrogen processes requests <https://rshestakov.wordpress.com/2013/02/17/how-nitrogen-processes-requests/>`_
- `How to add Nitrogen and Cowboy as dependency libs to your erlang application <https://rshestakov.wordpress.com/2013/03/03/how-to-add-nitrogen-and-cowboy-as-dependecy-libs-to-your-erlang-application/>`_




.. comment - a universal server and a universal webserver must be able to run independently (knowing that, at build time, only the US Webserver depends on the US Server); as a consequence, for example the US-Web configuration server should not request its overall US counterpart about the location of the configuration directory, but it should determine it by itself; so the elected design is that both use the same logic (as a static method) to locate the US configuration file; as a result, storing in that file the registration name of the US configuration server is meaningful for both: the US server can register itself accordingly, and the webserver can locate it if it exists; if not, the US web will create an instance thereof and use it afterwards


-------
Support
-------

Bugs, questions, remarks, patches, requests for enhancements, etc. are to be reported to the `project interface <https://github.com/Olivier-Boudeville/us-web>`_ (typically `issues <https://github.com/Olivier-Boudeville/us-web/issues>`_) or directly at the email address mentioned at the beginning of this document.



-------------
Please React!
-------------

If you have information more detailed or more recent than those presented in this document, if you noticed errors, neglects or points insufficiently discussed, drop us a line! (for that, follow the Support_ guidelines).



-----------
Ending Word
-----------

Have fun with the Universal Webserver!

.. comment Mostly added to ensure there is at least one figure directive,
.. otherwise the LateX graphic support will not be included:

.. figure:: us-web-title.png
   :alt: Universal Webserver logo
   :width: 50%
   :align: center

:raw-html:`<a name="us-web_bottom"></a>`
