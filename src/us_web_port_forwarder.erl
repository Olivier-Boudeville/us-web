% Copyright (C) 2020-2025 Olivier Boudeville
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
% Creation date: Friday, August 7, 2020.

-module(us_web_port_forwarder).

-moduledoc """
**Cowboy-compliant port forwarder** for US-Web: automatically forwards a request
aimed at a given TCP port (e.g. 80) to another port (e.g. 443, for an automatic
promotion of http into https).
""".

-export([ init/2 ]).


% Inspired from rabbit_cowboy_redirect.erl in
% https://github.com/rabbitmq/rabbitmq-web-dispatch/blob/master/src/.


-doc "State carried by the process in charge of a request.".
-type handler_state() :: net_utils:tcp_port().



% For server_header_id:
-include("us_web_defines.hrl").



-doc """
Initialises this handler.

 This handler initialisation performs the requested TCP port redirection.
""".
-spec init( cowboy_req:req(), handler_state() ) ->
									us_web_handler:handler_return().
init( Req, HandlerState=TargetTCPPort ) ->

	cond_utils:if_defined( us_web_debug_handlers,
		class_TraceEmitter:register_as_bridge(
			_Name=text_utils:format( "Port forward handler for port #~B",
									 [ TargetTCPPort ] ),
			_Categ="Port forwarder handler" ) ),

	%trace_bridge:debug_fmt( "Request ~p to be redirected to port #~B.",
	%                        [ Req, TargetTCPPort ] ),

	% Typically redirecting from TCP port #80 (http) to #443 (https):
	FixedURI = cowboy_req:uri( Req, #{ scheme => <<"https">>,
									   port => TargetTCPPort } ),

	% Using 301 ("Moved Permanently") is the best practice for upgrading users
	% from HTTP to HTTPS (see https://en.wikipedia.org/wiki/HTTP_301):
	%
	RedirectedReq = cowboy_req:reply( _Status=301,
		_Headers=#{ <<"location">> => FixedURI,
					<<"server">> => ?server_header_id },
		Req ),

	{ ok, RedirectedReq, HandlerState }.
