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
% Creation date: Monday, August 3, 2020.

-module(us_web_leec_handler).

-moduledoc """
**LEEC-compliant handler** for US-Web, whose role is to answer ACME challenges
thanks to LEEC, in order to complete the verification procedure.

See <https://leec.esperide.org/#usage-example> for more details.
""".


-export([ init/2, handle/2, terminate/3 ]).


% Type shorthands:

-type cert_manager_pid() :: class_USCertificateManager:manager_pid().

-type handler_state() :: cert_manager_pid().


% For server_header_id:
-include("us_web_defines.hrl").



-doc """
Initialises this LEEC handler.

This handler initialisation tries to serve the relevant challenge tokens to the
requesting http client (supposed to be an ACME server currently trying to read
the thumbprints from the well-known ACME URL).
""".
-spec init( cowboy_req:req(), handler_state() ) ->
									us_web_handler:handler_return().
init( Req, _HandlerState=CertManagerPid ) ->

	cond_utils:if_defined( us_web_debug_handlers,
		class_TraceEmitter:register_as_bridge(
			_Name=text_utils:format( "LEEC handler corresponding to "
				"certificate manager ~w", [ CertManagerPid ] ),
				_Categ="LEEC handler" ) ),

    % Force-enabled to investigate why so many challenges are requested:
	%cond_utils:if_defined( us_web_debug_handlers,
        trace_bridge:debug_fmt(
            "Request ~p to be handled on behalf of LEEC, while "
            "handler state is the PID of the associated certificate "
            "manager: ~w.", [ Req, CertManagerPid ] ), % ),

    % Apparently in some cases the current thumbprint challenges are requested
    % (through an access to the ACME URL for tokens) whereas they should not
    % (just roughly one week after the certificate generation).

	% We request the corresponding challenge to the associated (stable, fixed)
	% certificate manager, which will send in turn a corresponding request to
	% the (current, possibly respawning) LEEC FSM that will answer directly to
	% this handler (rather than to the certification manager) to avoid useless
	% message exchanges.
	%
	% A bit of interleaving (note that it is a oneway, not a request, as the
	% answer will come directly from the corresponding LEEC FSM):
	%
	CertManagerPid ! { getChallenge, [ _TargetPid=self() ] },

    % Suspected to be sometimes killed before reaching further.

	BinHost = cowboy_req:host( Req ),

	%trace_bridge:debug_fmt( "BinHost: ~p.", [ BinHost ] ),

	% Corresponds to "/.well-known/acme-challenge/:token":
	% (returns undefined is token not set in URI)
	%
	Token = case cowboy_req:binding( _BindingName=token, Req ) of

		undefined ->
			trace_bridge:error_fmt( "No ACME token set in: ~p.", [ Req ] ),
			throw( { no_token_defined, Req } );

		Tk ->
			%trace_bridge:debug_fmt( "Token: ~p.", [ Tk ] ),
			Tk

	end,

	ChallengeTimeoutMs = 30000,

	% Returns 'error' if token+thumbprint are not available, or 'no_challenge'
	% if being in 'idle' state:
	%
	Thumbprints = receive

		{ leec_result, Thmbprnts } ->
			%cond_utils:if_defined( us_web_debug_handlers,
				trace_bridge:debug_fmt( "Received thumbprints: ~p.",
										[ Thmbprnts ] ), % ),
			Thmbprnts

		% Just for debugging:
		%Other ->
		%   trace_bridge:error_fmt( "Unexpected answer while waiting for "
		%       "thumbprints: ~p.", [ Other ] ),
		%   throw( { unexpected_thumbprint_answer, Other } )

	after ChallengeTimeoutMs ->
		trace_bridge:error_fmt( "Time-out, no challenge received "
			"for host '~ts' after ~ts.",
			[ BinHost, time_utils:duration_to_string( ChallengeTimeoutMs ) ] ),

		throw( { challenge_timeout_for, BinHost, ChallengeTimeoutMs } )

	end,

	Reply = case maps:get( Token, Thumbprints, _Default=undefined ) of

		undefined ->
			trace_bridge:error_fmt( "For host '~ts', token '~p' not found "
				"among thumbprints '~p'.", [ BinHost, Token, Thumbprints ] ),
			cowboy_req:reply( 404, Req#{ server => ?server_req_id } );

		TokenThumbprint ->

			cond_utils:if_defined( us_web_debug_handlers,
				trace_bridge:debug_fmt( "For host '~ts', token '~p' found "
					"associated to '~p', among thumbprints '~p'.",
					[ BinHost, Token, TokenThumbprint, Thumbprints ] ) ),

			cowboy_req:reply( 200,
				#{ <<"content-type">> => <<"text/plain">> },
				TokenThumbprint, Req#{ server => ?server_req_id } )

	end,

	{ ok, Reply, no_state }.



-doc "Handles the specified request (not expected to be used).".
handle( Req, HandlerState ) ->
	trace_bridge:debug_fmt( "Handle called for request ~p.", [ Req ] ),
	{ ok, Req, HandlerState }.



-doc "Terminates this handler.".
terminate( _Reason=normal, _Req, _HandlerState ) ->
	ok;

terminate( Reason, Req, _HandlerState ) ->

	trace_bridge:error_fmt( "Terminate called for request ~p; reason: ~p.",
							[ Req, Reason ] ),

	ok.
