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

	% The overall US (not us-web) configuration server will be either found or
	% created by the us-web configuration one:
	%
	USWebCfgServerPid = class_USWebConfigServer:new_link( self() ),

	USWebCfgServerPid ! { getWebConfigSettings, [], self() },

	{ DispatchRules, HttpTCPPort } = receive

		{ wooper_result, WebSettings } ->
			WebSettings

	end,

	trace_utils:debug_fmt( "Starting Cowboy webserver, listening for the HTTP "
						   "scheme at port #~B", [ HttpTCPPort ] ),

	ProtoOpts = #{ env => #{ dispatch => DispatchRules } },

	% At least currently, the cowboy application is considered to be an external
	% dependency, and as such we rely on its using of its vanilla supervision
	% tree, not linked to the US-Web one, so this is not a child of this supervisor:
	%
	case cowboy:start_clear( http, [ { port, HttpTCPPort } ], ProtoOpts ) of

		{ ok, _RanchListenerPid } ->
			ok;

		{ error, Error } ->
			trace_utils:error_fmt( "Unable to start a cowboy http listener at "
								   "TCP port #~B, error being: ~p.~n"
								   "(protocol options were ~p).",
								   [ HttpTCPPort, Error, ProtoOpts ] ),
			throw( { webserver_launch_failed, http_scheme, HttpTCPPort } )

	end,

	% See
	% https://ninenines.eu/docs/en/cowboy/2.7/guide/listeners/#_secure_tls_listener
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

	trace_utils:info_fmt( "One may test this server running at "
						  "http://localhost:~p", [ HttpTCPPort ] ),

	{ ok, { SupSettings, ChildSpecs } }.
