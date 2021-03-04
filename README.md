[![Erlang CI](https://github.com/Olivier-Boudeville/us-web/actions/workflows/erlang-ci.yml/badge.svg)](https://github.com/Olivier-Boudeville/us-web/actions/workflows/erlang-ci.yml)
# us-web

![](/doc/us-web-title.png)

This is the repository of the **Universal Webserver**, which is a multi-domain, multi-virtualhost webserver integrating various services:
 * log generation and rotation, and, if enabled, log analysis, then made available through a "meta" website
 * TLS certificate use and automatic, scheduled renewal, resulting in a secured-enough webserver (as assessed by a grade of at least B obtained at the SSL Labs server test)
 
The Universal Webserver is a part of the [Universal Server](https://github.com/Olivier-Boudeville/Universal-Server) project.

Please refer to the [Universal Webserver official documentation](http://us-web.esperide.org) for further information, otherwise to its [mirror](http://olivier-boudeville.github.io/us-web/).

The US Webserver depends on [us-common](https://github.com/Olivier-Boudeville/us-common/), [Cowboy](https://github.com/ninenines/cowboy) and possibly [Awstats](http://www.awstats.org/), and may be used with or without the Universal Server itself (defined in [us-main](https://github.com/Olivier-Boudeville/us-main/)).

The 'master' branch is aimed to be the current stable version of this layer.
