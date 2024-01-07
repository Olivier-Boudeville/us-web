% Description of the US-Web OTP active application, typically used by rebar3.

% Note: if this file is named us_web.app, it is a *generated* file, whose real
% source is conf/us_web.app.src, from which _build/lib/us_web/ebin/us_web.app is
% obtained and copied to ebin/us_web.app; finally src/us_web.app.src is a mere
% symlink to this last file, so we have:
%
% ./conf/us_web.app.src [only real source]
% ./_build/lib/us_web/ebin/us_web.app
% ./ebin/us_web.app
% ./src/us_web.app.src -> ../ebin/us_web.app
%
% For more information see the Ceylan-Myriad 'create-app-file' make target and
% its associated comments.

% See also:
% - http://erlang.org/doc/man/app.html
% - https://learnyousomeerlang.com/building-otp-applications


{application, us_web,
 [{description, "US-Web, the OTP active application offering web services in the context of the Universal Server framework (see http://us-web.esperide.org)"},
  {vsn, "1.1.3"},
  {registered, [us_web]},

 % Active application:
  %
  % (no specific relevant startup argument to specify here)
  %
  {mod, {us_web_app, []}},

  % Regarding:
  %  - US-Common, see http://us.esperide.org/us.html#otp
  %  - Traces, see http://traces.esperide.org/traces.html#otp
  %  - WOOPER, see http://wooper.esperide.org/wooper.html#otp
  %  - Myriad, see http://myriad.esperide.org/myriad.html#otp


  % Myriad is a dependency of Wooper, which is itself a dependency of Traces,
  % dependency of US-Common (dependency of this US-Web); as such they may not be
  % listed here, however we stay conservative.
  %
  % Regarding Nitrogen and its dependencies, finally we do not list them here
  % and have instead US-Web activate them iff a Nitrogen-based website is
  % declared (inserting them in the standard scheme implemented by otp_utils
  % would be tricky anyway, as for example
  % nitrogen_core/deps/nprocreg/ebin/nprocreg.app would have to be found).

  % Non-Nitrogen version:
  {applications, [kernel, stdlib, sasl, cowboy, myriad, wooper, traces, us_common, leec]},

  % With Nitrogen:
  %{applications, [kernel, stdlib, sasl, cowboy, myriad, wooper, traces, us_common, leec,
  %                nprocreg, nitro_cache, nitrogen_core, simple_bridge]},
  {env,[]},

  % Flat hierarchy in ebin here:
  {modules, [class_USCertificateManager, class_USWebConfigServer, class_USWebLogger, us_web_app, us_web_generate_report_app, us_web_handler, us_web_leec_handler, us_web_meta, us_web_monitor_app, us_web_nitrogen_anchor, us_web_nitrogen_handler, us_web_port_forwarder, us_web_static, us_web_stop_app, us_web_sup]},

  {licenses, ["US-Web is licensed by its author (Olivier Boudeville) under the GNU Affero General Public License (AGPL), version 3.0 or later"]},

  {links, [ {"Official website", "http://us-web.esperide.org" },
			{"Github", "https://github.com/Olivier-Boudeville/us-web"} ]}

  %{exclude_files, []}

 ]}.
