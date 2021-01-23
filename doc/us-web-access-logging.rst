
----------------------------------
Standard Log Generation & Analysis
----------------------------------

The objective in this section is to have US-Web generate access logs like the standard webservers do, so that standard tools able to produce log analyses can be used afterwards. They may generate HTML pages, which can in turn be appropriately hosted directly by the US-Web framework itself.

US-Web (rather than a ``crontab``) takes control of log writing, for full control onto the start, stop and rotation behaviours.

For write efficiency, It is done by an (Erlang) process specific to a given virtual host (see ``class_USWebLogger``), and a task ring is used for synchronicity and load balancing with regard to log report generation.


For each virtual host (ex: ``baz.foobar.org``), following log files shall be generated:

- ``access-for-baz.foobar.org.log``
- ``error-for-baz.foobar.org.log``


They are to be stored by default in the ``/var/log`` directory, which can be overridden in the US-Web configuration file thanks to the ``us_web_log_dir`` key.

At the Cowboy level, logging could be done as a `middleware <https://ninenines.eu/docs/en/cowboy/2.7/guide/middlewares/>`_, yet we preferred to perform it directly in the handler (typically the static one, ``us_web_static``), presumably for a better control.

Once access logs are produced, a specific tool is to generate reports out of them, as discussed in the next section.



Web Analytics Software: Choice of Tool
======================================

The desired properties for such a tool are mainly: available as free software, trustable, standard, actively and well-maintained, running locally (as opposed to remote services such as Google Analytics), standalone, not resource-demanding, controllable, generating a local analysis (static) website, based only on basic webserver logs (not on a dedicated database, not on markers to be added to all webpages such as 1-pixel images), virtual-host compliant.


`Various tools <https://en.wikipedia.org/wiki/List_of_web_analytics_software>`_ can be considered, including (best spotted candidates, by increasing level of interest):

- `The Webalizer <http://www.webalizer.org/>`_: simple reports only, not maintained since 2013?
- `OWA <http://www.openwebanalytics.com/>`_: for professional, store-like business
- `Matomo <https://matomo.org/log-analytics/>`_: `interesting <https://en.wikipedia.org/wiki/Matomo_(software)>`_, yet a bit too complete / integrated; requires a dedicated database
- `GoAccess <https://goaccess.io/GoAccess>`_: a good candidate, almost no dependency, actively maintained, beautiful reports, supports GeoLite2, but more real-time oriented (more like a web monitor) and with less in-depth metrics
- `AWStats <https://en.wikipedia.org/wiki/AWStats>`_: old yet still maintained, real community-based open-source software, very complete, probably the most relevant in the list, whose code is apparently now maintained `here <https://github.com/eldy/awstats>`_

So we finally retained `AWStats <https://awstats.sourceforge.io/>`_.



Log Format
==========

The most suitable log format that we spotted (see `[1] <https://awstats.sourceforge.io/docs/awstats_faq.html#PERSONALIZEDLOG>`_ and `[2] <https://awstats.sourceforge.io/docs/awstats_config.html#LogFormat>`_ for more information) is named "*NCSA combined with several virtualhostname sharing same log file*".

Its correct definition is exactly::

  LogFormat="%virtualname %host %other %logname %time2 %methodurl %code %bytesd %refererquot %uaquot"


For example::

  virtualserver1 62.161.78.73 - - 2020-02-02 01:59:02 "GET /page.html HTTP/1.1" \
  200 1234 "http://www.from.com/from.htm" "Mozilla/4.0 (compatible; MSIE 5.01; Windows NT 5.0)"


This format is better than the "Apache combined logs" (combined, not common) log format, as containing the virtual host (important); note that for this second format, precisely named "*Apache or Lotus Notes/Domino native combined log format (NCSA combined/XLF/ELF log format)*" would be defined as::

 LogFormat="%host %other %logname %time1 %methodurl %code %bytesd %refererquot %uaquot


For example::

 62.161.78.73 - - [dd/mmm/yyyy:hh:mm:ss +0x00] "GET /page.html HTTP/1.1" \
 200 1234 "http://www.from.com/from.htm" "Mozilla/4.0 (compatible; MSIE 5.01; Windows NT 5.0)"


Field descriptions for this last format: `[1] <https://en.wikipedia.org/wiki/Common_Log_Format>`_, `[2] <https://httpd.apache.org/docs/current/logs.html#accesslog>`_, `[3] <http://fileformats.archiveteam.org/wiki/Combined_Log_Format>`_, `[4] <https://stackoverflow.com/questions/9234699/understanding-apaches-access-log>`_.

See also regarding Awstats log formats: `[1] <https://www.internetofficer.com/awstats/log-format/>`_, `[2] <https://awstats.sourceforge.io/docs/awstats_faq.html#LOGFORMAT>`_, `[3] <https://wiki.archlinux.org/index.php/AWStats>`_.

In all these cases, the log separator is a single space (hence ``LogSeparator=" "``).



Awstats Management
==================


Awstats Installation
--------------------

On Arch Linux, one should follow `these guidelines <https://wiki.archlinux.org/index.php/AWStats>`_ (example for version 7.8):

.. code:: bash

 $ pacman -Sy --needed awstats


Awstats will then be installed in ``/usr/share``, including the ``/usr/share/webapps/awstats/cgi-bin/awstats.pl`` script and the ``/usr/share/webapps/awstats/icon/`` directory.


Some permission fixes (to be done as root) might be needed first:

.. code:: bash

 $ chmod +r /usr/share/webapps/awstats/icon/os/*



Awstats Configuration
---------------------

Log analysis will be triggered periodically by the US-Web server rather than on-demand via CGI Perl scripts, and its result, i.e. the web pages generated from the access logs, will be available in the meta website (ex: ``mymeta.foobar.org``; refer to `Auto-generated Meta Website`_ for more information).

More precisely, and as already mentioned, in the US-Web log directory (see ``us_web_log_dir``), dedicated access and error log files will be generated for each known virtual host. For example the accesses to a ``baz.foobar.org`` virtual host will be written by the US-Web server in a corresponding ``access-for-baz.foobar.org.log`` file.

At server start-up, the US-Web meta module (``us_web_meta``) will have generated a suitable Awstats configuration file (namely ``awstats.baz.foobar.org.conf``) that will trigger the generation of the corresponding static web pages (``awstats.baz.foobar.org.*``, notably ``awstats.baz.foobar.org.html``) in the web root of the meta website.

These configuration files are now placed in ``/usr/local/etc/awstats`` (they were previously in the ``conf`` subdirectory of the root specified in ``us_web_app_base_dir``).

Indeed, if starting from version 7.8, Awstats allows these configuration files to be specified as absolute paths, its previous versions:

- either required such configuration files to be in ``/etc/awstats``, ``/usr/local/etc/awstats``, ``/etc`` or in the same directory as the ``awstats.pl`` script file
- or, if the configuration files could be specified as absolute paths, the generated pages would then include some faulty links because of that


US-Web retained the most controllable, less "system" directory, ``/usr/local/etc/awstats``. All these locations are mostly root-only, whereas the US-Web server is designed to run as a normal, non-privileged user and is to generate there these Awstats configuration files.

Such a target directory shall thus be created beforehand, and made writable by the user specified in ``us_web_username``.

.. Rather that requesting the user to compile its own version of Awstats, we retained ``/usr/local/etc/awstats``

.. All Awstats configuration files will be generated in the ``us_web/conf`` directory.

Each virtual host (say: ``baz.foobar.org``) will have its configuration file deriving from ``priv/conf/awstats.template.conf``, where the following patterns will be replaced by relevant ones (through keyword-based replacements):

- ``US_WEB_VHOST_LOG_FILE`` to become the full path to the corresponding access log (ex: ``access-for-baz.foobar.org.log``, in ``us_web_log_dir``)
- ``US_WEB_VHOST_DOMAIN`` to become the virtual host domain (ex: ``baz.foobar.org``)
- ``US_WEB_LOG_ANALYSIS_DATA_DIR`` to become the directory in which the working data (ex: state files) of the web analyzer (here Awstats) shall be written

Awstats icons are copied to the ``icon`` directory at the root of the meta website.


The Awstats database, typically located in ``/var/local/us-web/data``, will be updated once an access log file will be rotated; just after, this log file will be compressed and archived under a relevant filename, such as ``access-for-baz.foobar.org.log.2020-2-1-at-19h-48m-12s.xz``.



Awstats Troubleshooting
-----------------------

Various issues may prevent log reports to be available.

Let's try with a real US-Web uncompressed log file first (ex: ``xz -d access-vhost-catchall.log.test.xz``), supposing that it corresponds to a ``my-test`` virtual host).

Then configure Awstats (ex: through a ``/usr/local/etc/awstats/awstats.my-test.conf`` file) to process that log file; for that, run on that host:

.. code:: bash

 $ perl /usr/share/awstats/tools/awstats_configure.pl

Then, to debug the whole process, use, as root:

.. code:: bash

  $ rm -f /usr/share/webapps/awstats/cgi-bin/awstats*.txt ; echo ;
	 LANG=C /usr/share/webapps/awstats/cgi-bin/awstats.pl
	   -config=my-test -showdropped

Most problems should become visible then.

To do the same for a series of web logs in the context of US-Web, one can have them analysed first thanks to:

.. code:: bash

 $ for f in /usr/local/etc/awstats/awstats-*conf; do echo ;
	LANG=C /usr/share/webapps/awstats/cgi-bin/awstats.pl
	  -config=$f -update ; done


Then all web reports can be generated manually with:

.. code:: bash

 $ for f in /usr/local/etc/awstats/awstats-*conf; do echo ;
	LANG=C /usr/share/webapps/awstats/cgi-bin/awstats.pl
	  -config=$f -output ; done


..
  Note:: Currently we do not perform log analysis anymore, due to bugs in Awstats (at least 7.7.1, build 20180105):

  - only the main page for a given site (ex: ``awstats-for-foo.bar.org.html``) is generated: the configuration file (ex: ``awstats-for-foo.bar.org.conf``) does not specify a target generation location, and the main page is just output on standard output and redirected by US-Web to the right main file
  - moreover, even if these other files were generated (and in the correct place), the link to them from the main page would be invalid, as it includes their full path (ex: pointing to ``awstats./var/local/us-web/data/awstats-vhost-configs/awstats-for-foo.bar.org.conf.osdetail.html``)

   A solution would be to have Awstats fixed (unlikely?) or to allow the US-Web server to write to one of the only awstats-enabled system directories, such as ``/usr/local/etc/awstats``.

 .. ex:
	- /var/XXX/www/Meta-XXX/awstats-for-XXX.org.html
	- /var/local/us-web/data/awstats-vhost-configs/awstats-for-XXX.org.conf
	- <a href="awstats./var/local/us-web/data/awstats-vhost-configs/awstats-for-XXX.conf.osdetail.html"


Geolocation with Awstats
========================

`Multiple plugins <https://awstats.sourceforge.io/docs/awstats_contrib.html>`_ exist for that.

`Apparently <https://github.com/eldy/awstats/issues/86>`_, none is able to load the new GeoIP2 format (see also `this <https://github.com/eldy/awstats/issues/114>`_).

As a consequence: topic dropped for the moment.
