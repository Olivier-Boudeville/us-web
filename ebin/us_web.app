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
% For more information see the Ceylan-Myriad 'rebar3-create-app-file' make
% target and its associated comments.

% See also:
% - http://erlang.org/doc/man/app.html
% - https://learnyousomeerlang.com/building-otp-applications


{application, us_web,
 [{description, "US-Web, the OTP active application offering web services in the context of the Universal Server framework (see http://us-web.esperide.org)"},
  {vsn, "0.0.2"},
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
  %

  % myriad is a dependency of wooper, which is itself a dependency of traces,
  % dependency of us_common (dependency of this us_web); as such they may not be
  % listed here, however we stay conservative;
  %
  {applications, [kernel, stdlib, sasl, cowboy, myriad, wooper, traces, us_common]},
  {env,[]},

  % Flat hierarchy in ebin here:
  {modules, [class_USWebConfigServer, class_USWebLogger, us_web_app, us_web_handler, us_web_meta, us_web_monitor_app, us_web_static, us_web_sup]},

  {licenses, ["US-Web is licensed by its author (Olivier Boudeville) under the GNU Affero General Public License (AGPL), version 3.0 or later"]},

  {links, [ {"Official website", "http://us-web.esperide.org" },
			{"Github", "https://github.com/Olivier-Boudeville/us-web"} ]}

  %{exclude_files, []}

 ]}.
