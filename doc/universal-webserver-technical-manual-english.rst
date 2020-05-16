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
:Lastly updated: Friday, May 15, 2020
:Status: Work in progress
:Version: 0.0.1
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

The next level of information is to read the corresponding `source files <https://github.com/Olivier-Boudeville/us-web>`_, which are intensely commented and generally straightforward.


-----------
Layer Stack
-----------

From the highest level to the lowest, as summarised `here <https://github.com/Olivier-Boudeville/us-web>`_, a software stack involving the Universal Webserver usually is like:

- the *Universal Webserver* services themselves (i.e. this `us-web <http://us-web.esperide.org/>`_ layer)
- `Cowboy <https://github.com/ninenines/cowboy>`_ (for a small, fast and modern web framework)
- [optional] `Awstats <http://www.awstats.org/>`_ (for the analysis of access log files)
- `US-Common <http://us-common.esperide.org/>`_ (for US base facilities)
- `Ceylan-Traces <http://traces.esperide.org>`_ (for advanced runtime traces)
- `Ceylan-WOOPER <http://wooper.esperide.org>`_ (for OOP)
- `Ceylan-Myriad <http://myriad.esperide.org>`_ (as an Erlang toolbox)
- `Erlang <http://erlang.org>`_ (for the compiler and runtime)
- `GNU/Linux <https://en.wikipedia.org/wiki/Linux>`_

The shorthand for ``Universal Webserver`` is ``uw``.

:raw-latex:`\pagebreak`


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

- `Cowboy <https://github.com/ninenines/cowboy>`_
- `Awstats <http://www.awstats.org/>`_ (as an optional, runtime-only dependency)
- `US-Common <http://us-common.esperide.org/>`_

The latter relies on `Ceylan-Traces <https://github.com/Olivier-Boudeville/Ceylan-Traces>`_, which implies in turn `Ceylan-WOOPER <https://github.com/Olivier-Boudeville/Ceylan-WOOPER>`_, then `Ceylan-Myriad <https://github.com/Olivier-Boudeville/Ceylan-Myriad>`_ and `Erlang <http://erlang.org>`_.

We prefer using GNU/Linux, sticking to the latest stable release of Erlang, and building it from sources, thanks to GNU ``make``.

We recommend indeed obtaining Erlang thanks to a manual installation (refer to the corresponding `Myriad prerequisite section <http://myriad.esperide.org#prerequisites>`_  for more precise guidelines), Awstats thanks to your distribution of choice (ex: ``pacman -S awstats``) and, for all Erlang-related software, to rely on rebar3.

One wanting to be able to operate on the source code of these dependencies may define appropriate symbolic links in a ``_checkouts`` directory created at the root one's ``us-web`` clone, these links pointing to relevant GIT repositories.




Using Cutting-Edge GIT
======================

This is the installation method that we use and recommend; the Universal Webserver ``master`` branch is meant to stick to the latest stable version: we try to ensure that this main line always stays functional (sorry for the pun). Evolutions are to take place in feature branches and to be merged only when ready.

Once Erlang, Cowboy and possibly Awstats are available, it should be just a matter of executing:

.. code:: bash

 $ git clone https://github.com/Olivier-Boudeville/Ceylan-Myriad myriad
 $ cd myriad && make all && cd ..

 $ git clone https://github.com/Olivier-Boudeville/Ceylan-WOOPER wooper
 $ cd wooper && make all && cd ..

 $ git clone https://github.com/Olivier-Boudeville/Ceylan-Traces traces
 $ cd traces && make all && cd ..

 $ git clone https://github.com/Olivier-Boudeville/us-common
 $ cd us-common && make all

 $ git clone https://github.com/Olivier-Boudeville/us-web
 $ cd us-web && make all



Running a corresponding test just then boils down to:

.. code:: bash

 $ make debug


.. Should LogMX be installed and available in the PATH, the test may simply become:

.. .. code:: bash

..  $ make class_USScheduler_run


:raw-html:`<a name="otp"></a>`

.. _`otp-build`:

Using OTP-Related Build/Runtime Conventions
===========================================

As discussed in these sections of `Myriad <http://myriad.esperide.org/myriad.html#otp>`_, `WOOPER <http://wooper.esperide.org/index.html#otp>`_, `Traces <http://traces.esperide.org/index.html#otp>`_ and `US-Common <http://us-common.esperide.org/index.html#otp>`_, we added the (optional) possibility of generating a Universal Webserver *OTP application* out of the build tree, ready to result directly in an *(OTP) release*. For that we rely on `rebar3 <https://www.rebar3.org/>`_, `relx <https://github.com/erlware/relx>`_ and `hex <https://hex.pm/>`_.

Then we benefit from a standalone, complete Universal Webserver able to host as many virtual hosts on any number of domains as needed.

As for Myriad, WOOPER, Traces and US-Common, most versions of the Universal Webserver are also published as `Hex packages <https://hex.pm/packages/us_web>`_.

For more details, one may have a look at `rebar.config.template <https://github.com/Olivier-Boudeville/us-web/blob/master/conf/rebar.config.template>`_, the general rebar configuration file used when generating the Universal Webserver OTP application and release (implying the automatic management of all its dependencies).


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
