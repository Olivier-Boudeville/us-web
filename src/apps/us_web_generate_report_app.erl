% Copyright (C) 2020-2022 Olivier Boudeville
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
% Creation date: Sunday, January 19, 2020.


% @doc Actual US-Web trigger logic for the <b>generation of reports about access
% logs</b>, as a (Myriad) application.
%
% Typically called through the us_web/priv/bin/generate-us-web-log-report.sh
% script.
%
% Designed to request a US-Web instance to generate log reports, typically from
% any remote host able to connect to the VM hosting that instance.
%
% Currently this generation is better driven automatically, thanks to a
% scheduler.
%
-module(us_web_generate_report_app).


-export([ exec/0 ]).


% For default_us_web_config_server_registration_name:
-include("us_web_defines.hrl").

% For update_code_path_for_myriad/0 and all:
-include_lib("myriad/include/myriad_script_include.hrl").



% @doc Runs the report generation app.
-spec exec() -> no_return().
exec() ->

	% First, enable all possible helper code (hence to be done first of all):
	update_code_path_for_myriad_from_module(),

	% Allows to support both OTP conventions and ad hoc, automatic ones:
	wooper_utils:start_for_app(),

	{ CfgFilePath, MaybeDomainBinStr, MaybeHostBinStr } =
		init_from_command_line(),

	Cfg = file_utils:read_terms( CfgFilePath ),

	%trace_utils:debug_fmt( "Read configuration from '~ts': ~p",
	%                       [ CfgFilePath, Cfg ] ),

	TargetNodeName = get_target_node_name( Cfg ),

	app_facilities:display( "Connecting to node '~ts'.", [ TargetNodeName ] ),

	case net_adm:ping( TargetNodeName ) of

		pong ->
			ok;

		pang ->
			trace_utils:error_fmt( "Unable to connect to '~ts'.~nIf the target "
				"node is really running and is named exactly like that, check "
				"that the cookies match and, finally, that no firewall is in "
				"the way (ex: a server may filter the EPMD port of interest).",
				[ TargetNodeName ] ),

			throw( { unable_to_connect_to, TargetNodeName } )

	end,

	% Otherwise the remote node could not be known before use:
	global:sync(),

	% Note: us_web_config_server_registration_name

	%app_facilities:display( "Globally registered names: ~w.",
	%                        [ global:registered_names() ] ),

	UWCfgRegName = get_us_web_cfg_server_name( Cfg ),

	%app_facilities:display( "Looking up US-Web config server by name: ~ts.",
	%                        [ UWCfgRegName ] ),

	UWCfgRegScope = ?us_web_config_server_registration_scope,

	UWCfgPid = naming_utils:get_registered_pid_for( UWCfgRegName,
		naming_utils:registration_to_look_up_scope( UWCfgRegScope ) ),

	%trace_utils:debug_fmt( "Found PID of US-Web configuration server: ~w.",
	%                       [ UWCfgPid ] ),

	UWCfgPid ! { generateLogAnalysisReports,
					[ MaybeDomainBinStr, MaybeHostBinStr ], self() },

	app_facilities:display( "Waiting for the acknowledgement of the generation "
							"of requested log analysis report(s)." ),

	receive

		{ wooper_result, report_generation_success } ->
			app_facilities:display(
				"Log analysis report(s) successfully generated." ),
			basic_utils:stop();

		{ wooper_result, R={ report_generation_failed, ErrorReason } } ->
			trace_utils:error_fmt( "Log analysis report generation failed; "
				"reason:~n  ~p", [ ErrorReason ] ),
			throw( R )

	end.


% @doc Initialises this application from the command line.
init_from_command_line() ->

	% To force options for testing:
	%ArgTable = shell_utils:generate_argument_table( "--help" ),

	ArgTable = shell_utils:get_argument_table(),

	%trace_utils:debug_fmt( "Argument table: ~ts",
	%                       [ list_table:to_string( ArgTable ) ] ),

	% Argument expected to be set by the caller script:
	{ CfgFilePath, ConfigShrunkTable } =
			case list_table:extract_entry_if_existing( '-config-file',
													   ArgTable ) of

		false ->
			throw( no_configuration_file_set );

		{ [ [ CfgPath ] ], CfgShrunkTable } ->
			case file_utils:is_existing_file_or_link( CfgPath ) of

				true ->
					{ CfgPath, CfgShrunkTable };

				false ->
					throw( { configuration_file_not_found, CfgPath } )

			end;

		{ OtherCfgArg, _CfgTable } ->
			throw( { unexpected_configuration_argument, OtherCfgArg } )


	end,

	%trace_utils:debug_fmt( "Configuration file: '~ts'.", [ CfgFilePath ] ),

	% Argument also expected to be set by the caller script:
	{ RemoteCookie, CookieShrunkTable } =
			case list_table:extract_entry_if_existing( '-target-cookie',
													   ConfigShrunkTable ) of

		false ->
			throw( no_target_cookie_set );

		{ [ [ Cookie ] ], CookShrunkTable } ->
			{ text_utils:string_to_atom( Cookie ), CookShrunkTable };

		{ OtherCookieArg, _CookTable } ->
			throw( { unexpected_cookie_argument, OtherCookieArg } )

	end,

	trace_utils:debug_fmt( "Setting remote cookie: '~ts'.", [ RemoteCookie ] ),

	net_utils:set_cookie( RemoteCookie ),


	% Arguments possibly set by the caller script:

	{ MaybeDomainBinStr, DomainShrunkTable } =
			case list_table:extract_entry_if_existing( '-domain',
													   CookieShrunkTable ) of

		false ->
			{ undefined, CookieShrunkTable };

		{ [ [ DomainStr ] ], DomShrunkTable } ->
			{ text_utils:string_to_binary( DomainStr ), DomShrunkTable };

		{ OtherDomainArg, _DomTable } ->
			throw( { unexpected_domain_argument, OtherDomainArg } )

	end,

	%trace_utils:debug_fmt( "Selected target domain: '~ts'.",
	%                       [ MaybeDomainBinStr ] ),


	{ MaybeHostBinStr, HostShrunkTable } =
			case list_table:extract_entry_if_existing( '-host',
													   DomainShrunkTable ) of

		false ->
			{ undefined, DomainShrunkTable };

		{ [ [ HostStr ] ], HstShrunkTable } ->
			{ text_utils:string_to_binary( HostStr ), HstShrunkTable };

		{ OtherHostArg, _HstTable } ->
			throw( { unexpected_host_argument, OtherHostArg } )

	end,

	%trace_utils:debug_fmt( "Selected target host: '~ts'.",
	%                       [ MaybeHostBinStr ] ),


	case list_table:is_empty( HostShrunkTable ) of

		true ->
			ok;

		false ->
			throw( { unexpected_arguments,
					 list_table:enumerate( HostShrunkTable ) } )

	end,

	{ CfgFilePath, MaybeDomainBinStr, MaybeHostBinStr }.



% @doc Returns the target node name.
get_target_node_name( Cfg ) ->

	RemoteHostname = list_table:get_value( us_web_hostname, Cfg ),

	%trace_utils:debug_fmt( "Remote host: '~ts'.", [ RemoteHostname ] ),

	%NodeStringName =
	case net_utils:localnode() of

		local_node ->
			throw( { node_not_networked, node() } );

		_N ->
			%text_utils:atom_to_string( N )
			ok

	end,

	% Note that a single, hardcoded node name is used here:
	text_utils:string_to_atom( "us_web" ++ [ $@ | RemoteHostname ] ).



% @doc Returns the registration name of the target US-Web configuration server.
get_us_web_cfg_server_name( Cfg ) ->

	% Maybe a specific name is defined in the US-Web configuration file?

	RegNameKey = us_web_config_server_registration_name,

	case list_table:has_entry( RegNameKey, Cfg ) of

		true ->
			RegName = list_table:get_value( RegNameKey, Cfg ),
			%trace_utils:debug_fmt( "Registration name of the US-Web "
			%   "configuration read from configuration file: '~ts'.",
			%   [ RegName ] ),
			RegName;

		false ->
			DefRegName = ?default_us_web_config_server_registration_name,
			%trace_utils:debug_fmt( "No registration name for the US-Web "
			%   "configuration found in configuration file, using "
			%   "default one: '~ts'.", [ DefRegName ] ),
			DefRegName

	end.
