% Copyright (C) 2019-2020 Olivier Boudeville
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


% Root supervisor of the us_web application.
%
% Directly created by us_web_app.
%
-module(us_web_sup).

-behaviour(supervisor).

-export([ start_link/0 ]).

-export([ init/1 ]).

-define( server_registration_name, ?MODULE ).


start_link() ->
	supervisor:start_link( { local, ?server_registration_name }, ?MODULE, [] ).



init( _Args=[] ) ->

	trace_utils:trace( "Starting us_web supervisor..." ),

	% The logic below shall better be in a (single) supervised child, for a
	% better logic separation.

	% The overall US (not US-Web) configuration server will be either found or
	% created by the US-Web configuration one:
	%
	USWebCfgServerPid = class_USWebConfigServer:new_link( self() ),

	% Implicit synchronisation:
	USWebCfgServerPid ! { getWebConfigSettings, [], self() },

	{ DispatchRules, MaybeHttpTCPPort, MaybeHttpsTCPPort, MaybeSNIHostsInfo } =
			receive

		{ wooper_result, WebSettings } ->
			WebSettings

	end,

	trace_utils:debug( "Starting Cowboy webserver..." ),

	ProtoOpts = #{ env => #{ dispatch => DispatchRules } },

	HttpProtoOpts = ProtoOpts,

	case MaybeHttpTCPPort of

		undefined ->
			trace_utils:trace( "No HTTP listening enabled." );

		HttpTCPPort ->
			trace_utils:debug_fmt( "Listening for the HTTP scheme "
								   "at port #~B.", [ HttpTCPPort ] ),

			% At least currently, the cowboy application is considered to be an
			% external dependency, and as such we rely on its using of its
			% vanilla supervision tree, not linked to the US-Web one, so this is
			% not a child of this supervisor:

			HttpTransportOpts = [ { port, HttpTCPPort } ],

			case cowboy:start_clear( us_web_http_listener, HttpTransportOpts,
									 HttpProtoOpts ) of

				{ ok, _HttpRanchListenerSupPid } ->
					ok;

				{ error, HttpError } ->

					trace_utils:error_fmt( "Unable to start a cowboy HTTP "
						"listener at TCP port #~B, error being: ~p.~n"
						"(protocol options were ~p).",
						[ HttpTCPPort, HttpError, ProtoOpts ] ),

					throw( { webserver_launch_failed, http_scheme,
							 HttpTCPPort, HttpError } )

			end

	end,

	%case undefined of
	case MaybeHttpsTCPPort of

		undefined ->
			trace_utils:trace( "No HTTPS listening enabled." );

	   % Using TLS here:
		HttpsTCPPort ->

			trace_utils:debug_fmt( "Listening for the HTTPS scheme "
								   "at port #~B.", [ HttpsTCPPort ] ),

			{ PEMCertFilePath, SNIVhInfos } = MaybeSNIHostsInfo,

			% Refer to https://ninenines.eu/docs/en/ranch/2.0/manual/ranch_ssl/:
			% (currently we do not include {key, CertKeyPath} entries short of
			% knowing the interest of it)
			%
			HttpsTransportOpts = [

				{ port, HttpsTCPPort },

				% Path to the PEM certificate corresponding to the default host
				% (ex: "foobar.org"), if the hostname is not received in the SSL
				% handshake, e.g. if the browser does not support SNI: (ex:
				% "foobar.org.crt")
				%
				{ certfile, PEMCertFilePath },

				% Server Name Indication (virtual) hosts:
				{ sni_hosts, SNIVhInfos } ],

			trace_utils:debug_fmt( "https transport options: ~p~n"
				"protocol options: ~p", [ HttpsTransportOpts, ProtoOpts ] ),

			case cowboy:start_tls( us_web_https_listener, HttpsTransportOpts,
								   ProtoOpts ) of

				{ ok, _HttpsRanchListenerSupPid } ->
					ok;

				{ error, HttpsError } ->

					trace_utils:error_fmt( "Unable to start a cowboy HTTPS"
						"listener at TCP port #~B, error being: ~p.~n"
						"(transport options were ~p, while protocol "
						"options were ~p).",
						[ HttpsTCPPort, HttpsError, HttpsTransportOpts,
						  ProtoOpts ] ),

					throw( { webserver_launch_failed, https_scheme,
							 HttpsTCPPort, HttpsError } )

			end

	end,

	% See
	% https://ninenines.eu/docs/en/cowboy/2.8/guide/listeners/#_secure_tls_listener
	% for https.

	SupSettings = otp_utils:get_supervisor_settings(
					_RestartStrategy=one_for_one,
					class_USWebConfigServer:get_execution_target() ),

	%% WebManagerSpec = #{ id => us_web_manager,
	%%					start => { us_web, start_link, [] },
	%%					restart => permanent,
	%%					shutdown => 2000,
	%%					type => worker,
	%%					modules => [ us_web ] },

	%% ChildSpecs = [ WebManagerSpec ],

	% Currently no worker or lower-level supervisor to supervise:
	ChildSpecs = [],

	case MaybeHttpTCPPort of

		undefined ->
			ok;

		SomeHttpTCPPort ->
			trace_utils:info_fmt( "One may test this server running at "
								  "http://localhost:~p", [ SomeHttpTCPPort ] )

	end,

	case MaybeHttpsTCPPort of

		undefined ->
			ok;

		SomeHttpsTCPPort ->
			trace_utils:info_fmt( "One may test this server running at "
								  "https://localhost:~p", [ SomeHttpsTCPPort ] )

	end,


	{ ok, { SupSettings, ChildSpecs } }.
