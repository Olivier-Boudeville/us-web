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
% Creation date: Monday, December 12, 2022.
%
-module(us_web_nitrogen_handler).


-export([ init/2, init/3, handle/2, terminate/3 ] ).


% Should be in an hrl if actually used by class_USWebConfigServer:
-record( us_web_nitro_handler_state, {

	headers,
	body

	% The root of the Nitrogen-based site:
	%nitro_doc_root :: bin_directory_path()

 } ).


init( Req, MaybeHandlerState ) ->
	%trace_bridge:debug_fmt( "us_web_nitrogen_handler:init/2 called "
	%   "with ~p and ~p", [ Req, HandlerState ] ),
	handle( Req, MaybeHandlerState ).


% Not called:
init( _Transport, Req, _Opts ) ->
	%trace_bridge:debug( "Init/3 for Nitrogen" ),
	{ ok, Req, #us_web_nitro_handler_state{} }.


handle( Req, HandlerState ) ->

	%trace_bridge:debug_fmt( "Handling request ~p ~p", [ Req,
	% code_utils:is_beam_in_path(simple_bridge) ] ),

	% Not set ('undefined'):
	%{ok, DocRoot} = application:get_env(cowboy, document_root),

	% Not necessary now that anyway the VM current directory had to be changed
	% to accommodate templates and al:
	%
	%DocRoot = HandlerState#us_web_nitro_handler_state.nitro_doc_root,

	% Sufficient:
	DocRoot = <<".">>,

	RequestBridge = simple_bridge:make_request( cowboy_request_bridge,
												{ Req, DocRoot } ),

	ResponseBridge = simple_bridge:make_response( cowboy_response_bridge,
												  RequestBridge ),

	nitrogen:init_request( RequestBridge, ResponseBridge ),

	{ ok, NewReq } = nitrogen:run(),

	{ ok, NewReq, HandlerState }.



terminate( _Reason, _Req, _State ) ->
	ok.
