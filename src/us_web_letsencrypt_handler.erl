% Copyright (C) 2020-2020 Olivier Boudeville
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
% Creation date: Monday, August 3, 2020.


% Let's Encrypt-compliant handler for us_web, in order to answer ACME
% challenges.
%
% See https://github.com/Olivier-Boudeville/letsencrypt-erlang#slave
%
-module(us_web_letsencrypt_handler).


-export([ init/2, handle/2, terminate/3 ]).


% Actually []:
-type handler_state() :: any().


-spec init( cowboy_req:req(), handler_state() ) ->
				  us_web_handler:handler_return().
init( Req, HandlerState ) ->

	trace_utils:debug_fmt( "Request ~p to be handled for letsencrypt, while "
		"handler state is ~p...", [ Req, HandlerState ] ),

	BinHost = cowboy_req:host( Req ),

	% Corresponds to "/.well-known/acme-challenge/:token":
	% (returns undefined is token not set in URI)
	%
	Token = case cowboy_req:binding( _Name=token, Req ) of

		undefined ->
			throw( { no_token_defined, Req } );

		Tk ->
			Tk

	end,

	% Returns 'error' if token+thumbprint are not available:
	Thumbprints = letsencrypt:get_challenge(),

	{ ok, Reply } = case maps:get( Token, Thumbprints, _Default=undefined ) of

		Thumbprints ->
			trace_utils:debug_fmt( "For host '~s', token '~p' found in "
				"thumbprints '~p'.", [ BinHost, Token, Token ] ),
			cowboy_req:reply( 200,
				[ #{ <<"content-type">> => <<"text/plain">> } ],
				Thumbprints, Req );

		_Other ->
			trace_utils:error_fmt( "For host '~s', token '~p' not found in "
				"thumbprints '~p'.", [ BinHost, Token, Token ] ),
			cowboy_req:reply( 404, Req )

	end,

	{ ok, Reply, no_state }.



handle( Req, HandlerState ) ->
	trace_utils:trace_fmt( "Handle called for request ~p.", [ Req ] ),
	{ ok, Req, HandlerState }.


terminate( Reason, Req, _HandlerState ) ->
	trace_utils:trace_fmt( "Terminate called for request ~p; reason: ~p.",
						   [ Req, Reason ] ),
	ok.