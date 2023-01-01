% Copyright (C) 2019-2023 Olivier Boudeville
%
% This file belongs to the US-Web project, a part of the Universal Server
% framework.
%
% This program is free software: you can redistribute it and/or modify it under
% the terms of the GNU Affero General Public License as published by the Free
% Software Foundation, either version 3 of the License, or (at your option) any
% later version.
%
% This program is distributed in the hope that it will be useful, but WITHOUT
% ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
% FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more
% details.
%
% You should have received a copy of the GNU Affero General Public License along
% with this program. If not, see <http://www.gnu.org/licenses/>.
%
% Author: Olivier Boudeville [olivier (dot) boudeville (at) esperide (dot) com]
% Creation date: Wednesday, December 25, 2019.


% @doc <b>Root OTP supervisor</b> of the us_web application.
%
% Directly created by us_web_app.
%
-module(us_web_sup).

-behaviour(supervisor).


-export([ start_link/0, start_link/1, stop/0 ]).

-export([ init/1 ]).

-define( server_registration_name, ?MODULE ).
-define( server_registration_scope, local ).

% For the general_web_settings record:
-include("class_USWebConfigServer.hrl").


% Shorthand:
-type application_run_context() :: otp_utils:application_run_context().



% Implementation notes:
%
% X.509 certificates may have to be generated (thanks to LEEC), but for that a
% HTTP webserver must be up and running.
%
% So the order of operations is:
% 1. read the configuration settings
% 2. start a corresponding HTTP server, and LEEC itself
% 3. when webserver is running, initiate (if appropriate) the certificate
% generation
% 4. be notified (through the US-Web Let's Encrypt handler) that certificates
% are ready
% 5. starts the corresponding HTTPS server



% @doc Starts and links the US-Web supervisor, with OTP conventions.
%
% (function probably useless)
%
-spec start_link() -> otp_utils:supervisor_pid().
start_link() ->
	supervisor:start_link( { ?server_registration_scope,
		?server_registration_name }, ?MODULE, _DefaultArgs=[ as_otp_release ] ).



% @doc Starts and links the US-Web supervisor, with OTP conventions or not.
-spec start_link( application_run_context() ) -> otp_utils:supervisor_pid().
start_link( AppRunContext ) ->
	supervisor:start_link( { ?server_registration_scope,
		?server_registration_name }, ?MODULE, _DefaultArgs=[ AppRunContext ] ).



% @doc Initialises the main US-Web OTP supervisor.
-spec init( [ application_run_context() ] ) ->
				{ 'ok', { supervisor:sup_flags(), supervisor:child_spec() } }.
init( _Args=[ AppRunContext ] ) ->

	otp_utils:check_application_run_context( AppRunContext ),

	% Preparing a trace bridge to collect traces and errors:

	BinTraceEmitterName = <<"Supervisor">>,

	BinTraceCategory = <<"US.US-Web.Supervision">>,

	AggregatorPid =
		class_TraceAggregator:get_aggregator( _LaunchAggregator=false ),

	BridgeSpec = { BinTraceEmitterName, BinTraceCategory, AggregatorPid },

	trace_bridge:register( BridgeSpec ),

	trace_bridge:debug_fmt( "Starting us_web supervisor (run context: ~ts)...",
							[ AppRunContext ] ),

	% Watchdog check every 15 minutes:
	AggregatorPid ! { enableWatchdog, [ _PeriodInSecs=15*60 ] },

	% The logic below shall better be in a (single) supervised child, for a
	% better logic separation.

	% The overall US (not US-Web) configuration server will be either found or
	% created by the US-Web configuration one:
	%
	USWebCfgServerPid =
		class_USWebConfigServer:new_link( self(), AppRunContext ),

	% Implicit synchronisation:
	USWebCfgServerPid ! { getWebConfigSettings, [], self() },

	#general_web_settings{
			http_dispatch_rules=HttpDispatchRules,
			http_tcp_port=MaybeHttpTCPPort,
			https_dispatch_rules=MaybeHttpsDispatchRules,
			https_tcp_port=MaybeHttpsTCPPort,
			certificate_support=CertSupport,
			https_transport_info=MaybeHttpsTranspOpts,
			dh_key_path=MaybeBinDHKeyPath,
			ca_cert_key_path=MaybeBinCaKeyPath } = receive

		{ wooper_result, GenWebSettings } ->
				trace_bridge:debug_fmt( "Received general web settings:~n  ~p",
										[ GenWebSettings ] ),
				GenWebSettings

			%after 15000 ->
			%   trace_bridge:error(
			%       "Time-out while waiting for web settings." ),
			%   throw( us_web_sup_settings_time_out )

	end,

	trace_bridge:debug( "Starting Cowboy webserver..." ),

	HttpProtoOptMap = #{ max_keepalive => 1024,
						 max_connections => infinity,
						 env => #{ dispatch => HttpDispatchRules } },

	HttpMsg = case MaybeHttpTCPPort of

		undefined ->
			NoHttpMsg = "No HTTP listening enabled.",
			trace_bridge:info( NoHttpMsg ),
			NoHttpMsg;

		HttpTCPPort ->

			trace_bridge:debug_fmt( "Listening for the HTTP scheme "
									"at port #~B.", [ HttpTCPPort ] ),

			% At least currently, the cowboy application is considered to be an
			% external dependency, and as such we rely on its using of its
			% vanilla supervision tree, not linked to the US-Web one, so this is
			% not a child of this supervisor:

			HttpTransportOpts = [ { port, HttpTCPPort } ],

			trace_bridge:debug_fmt( "The http transport options are:~n ~p~n~n"
				"~nThe http protocol options are:~n  ~p",
				[ HttpTransportOpts, HttpProtoOptMap ] ),

			case cowboy:start_clear( us_web_http_listener, HttpTransportOpts,
									 HttpProtoOptMap ) of

				{ ok, _HttpRanchListenerSupPid } ->
					StartHttpMsg = text_utils:format( "The HTTP server is now "
						"ready, running at http://localhost:~p.",
						[ HttpTCPPort ] ),
					trace_bridge:notice( StartHttpMsg ),
					StartHttpMsg;

				{ error, HttpError } ->

					trace_bridge:error_fmt( "Unable to start a cowboy HTTP "
						"listener at TCP port #~B, error being: ~p.~n~n"
						"The http transport options were:~n  ~p.~n~n"
						"The http protocol options were: ~n  ~p.~n",
						[ HttpTCPPort, HttpError, HttpTransportOpts,
						  HttpProtoOptMap ] ),

					throw( { webserver_launch_failed, http_scheme, HttpTCPPort,
							 HttpError } )

			end

	end,

	% Now that an HTTP webserver is running (hopefully), we can trigger the
	% generation of any needed X.509 certificates (as a webserver is needed to
	% validate challenges sent by the ACME server); the main objective here is
	% proper synchronisation:
	%
	case CertSupport of

		renew_certificates ->

			trace_bridge:debug(
				"Requesting the renewal of the X.509 certificates." ),

			USWebCfgServerPid ! { renewCertificates, [], self() },

			receive

				% Note the plural; does not imply that all certificates could be
				% immediately obtained:
				%
				{ wooper_result, certificate_renewals_over } ->
					trace_bridge:debug( "Certificates renewed." )

			end;

		_ ->
			ok

	end,

	HttpsMsg = case MaybeHttpsTCPPort of

		undefined ->
			NoHttpsMsg = "No HTTPS listening enabled.",
			trace_bridge:info( NoHttpsMsg ),
			NoHttpsMsg;

	   % Using TLS here:
		HttpsTCPPort ->
			% No StartHttpsMsg useful here:
			trace_bridge:debug_fmt(
				"Listening for the HTTPS scheme at port #~B.",
				[ HttpsTCPPort ] ),

			HttpsProtoOptMap = #{ max_keepalive => 1024,
				max_connections => infinity,
				env => #{ dispatch => MaybeHttpsDispatchRules } },

			% Transport options for the main, default host (ex: "foobar.org"),
			% containing notably the path to its PEM certificate and to its
			% private key, and related SNI information.
			%
			% Useful if the hostname is not received in the SSL handshake,
			% e.g. if the browser does not support SNI.
			%
			% Refer to https://ninenines.eu/docs/en/ranch/2.0/manual/ranch_ssl/.
			%
			{ MainTranspOpts, SNIVhInfos } = case MaybeHttpsTranspOpts of

				undefined ->
					throw( no_main_https_transport_options );

				MTOpts ->
					MTOpts

			end,

			Ciphers = class_USCertificateManager:get_recommended_ciphers(),

			% Inspired from
			% http://ezgr.net/increasing-security-erlang-ssl-cowboy/:
			%
			GenericHttpsOpts = [
				{ port, HttpsTCPPort },
				{ versions, class_USWebConfigServer:get_protocol_versions() },

				% Server Name Indication (virtual) hosts:
				{ sni_hosts, SNIVhInfos },

				{ ciphers, Ciphers },
				{ secure_renegotiate, true },
				{ reuse_sessions, true },
				{ honor_cipher_order, true } ],

			BaseHttpsOpts = case MaybeBinDHKeyPath of

				undefined ->
					GenericHttpsOpts;

				BinDHKeyFilePath ->
					[ { dhfile,
							text_utils:binary_to_string( BinDHKeyFilePath ) }
						| GenericHttpsOpts ]

			end,

			WithCaHttpsOpts = case MaybeBinCaKeyPath of

				undefined ->
					BaseHttpsOpts;

				BinCaKeyPath ->
					[ { cacertfile,
							text_utils:binary_to_string( BinCaKeyPath ) }
						| BaseHttpsOpts ]

			end,

			HttpsTransportOpts = MainTranspOpts ++ WithCaHttpsOpts,

			trace_bridge:debug_fmt( "The https transport options are:~n ~p~n~n"
				"~nThe https protocol options are:~n  ~p",
				[ HttpsTransportOpts, HttpsProtoOptMap ] ),

			case cowboy:start_tls( us_web_https_listener,
								HttpsTransportOpts, HttpsProtoOptMap ) of

				{ ok, _HttpsRanchListenerSupPid } ->
					URLMsg = text_utils:format( "The HTTPS server is now ready,"
						" running at https://localhost:~p.", [ HttpsTCPPort ] ),

					trace_bridge:notice( URLMsg ),

					URLMsg;

				{ error, HttpsError } ->
					trace_bridge:error_fmt( "Unable to start a cowboy HTTPS "
						"listener at TCP port #~B, error being:~n  ~p.~n~n"
						"Transport options were:~n  ~p.~n~n"
						"Protocol options were:~n  ~p.~n",
						[ HttpsTCPPort, HttpsError, HttpsTransportOpts,
						  HttpsProtoOptMap ] ),

					throw( { webserver_launch_failed, https_scheme,
							 HttpsTCPPort, HttpsError } )

			end

	end,

	% See
	% https://ninenines.eu/docs/en/cowboy/2.8/guide/listeners/#_secure_tls_listener
	% for https.

	SupSettings = otp_utils:get_supervisor_settings(
		% If a child process terminates, only that process is restarted:
		_RestartStrategy=one_for_one,
		class_USWebConfigServer:get_execution_target() ),

	% WebManagerSpec = #{
	%   id => us_web_manager_bridge_id,
	%   start => { us_web, start_link, [] },
	%   restart => otp_utils:get_restart_setting( ExecTarget ),
	%   shutdown => infinity,
	%   type => supervisor,
	%   modules => [ us_web_manager_bridge_sup ] },

	% ChildSpecs = [ WebManagerSpec ],

	% Currently no worker or lower-level supervisor to supervise (we rely on
	% the cowboy supervision tree for that):
	%
	ChildSpecs = [],

	% Intentionally not trace_bridge:
	trace_utils:info_fmt( "~ts~n~ts", [ HttpMsg, HttpsMsg ] ),

	% A problem is that the US-Web configuration server shall be created from
	% here (to access configuration information from that supervisor), but then
	% it should be terminated from the stop/0 function - which thus shall have
	% some means to know the PID of this server). Only pragmatic solution found
	% is to use the application environment for that:
	%
	Env = [ { _Param=us_web_config_server_pid, _V=USWebCfgServerPid } ],

	application:set_env( _Config=[ { us_web, Env } ] ),

	{ ok, { SupSettings, ChildSpecs } }.



% @doc Stops the US-Web main supervisor, with OTP conventions or not.
%
% Note: unlike supervisor_bridge, no supervisor:terminate/* callback exists.
%
-spec stop() -> void().
stop() ->

	{ ok, USWebCfgSrvPid } =
		application:get_env( _Param=us_web_config_server_pid ),

	trace_bridge:warning_fmt( "Stopping US-Web, including its configuration "
							  "server ~w.", [ USWebCfgSrvPid ] ),

	USWebCfgSrvPid ! delete.
