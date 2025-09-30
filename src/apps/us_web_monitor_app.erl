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
% Creation date: Sunday, January 19, 2020.

-module(us_web_monitor_app).

-moduledoc """
Actual US-Web **trace monitoring logic**, as a (Myriad) application.

Typically called through the `us_web/priv/bin/monitor-us-web.sh` script.

Designed to monitor a US-Web instance typically from any remote host able to
connect to the VM hosting that instance.
""".


-export([ exec/0 ]).


% For update_code_path_for_myriad/0 and all:
-include_lib("myriad/include/myriad_script_include.hrl").

% For trace_aggregator_name:
-include_lib("traces/include/class_TraceAggregator.hrl").



-doc "Runs this monitoring app.".
-spec exec() -> no_return().
exec() ->

	% No app_start here, hence we need the following (see
	% traces_for_apps:app_start/2 for a detailed explanation):
	%
	erlang:process_flag( trap_exit, false ),

	{ ActualTargetNodeName, IsVerbose, CfgTable, FinalArgTable } =
        us_client:setup( _ServerPrefix=us_web ),

	list_table:is_empty( FinalArgTable ) orelse
		throw( { unexpected_arguments,
				 list_table:enumerate( FinalArgTable ) } ),


	AggregatorName = ?trace_aggregator_name,

	%app_facilities:display( "Looking up aggregator by name: ~ts.",
	%                        [ AggregatorName ] ),

	% The trace aggregator is expected to run in the target node, but to be
	% registered there only locally, to avoid clashing with any other
	% aggregator:
	%
	AggregatorPid = naming_utils:get_locally_registered_pid_for(
		AggregatorName, ActualTargetNodeName ),

	IsVerbose andalso
        app_facilities:display( "Creating now a local trace listener." ),

	TraceListenerPid = case us_client:get_tcp_port_range( CfgTable ) of

		undefined ->
			class_TraceListener:synchronous_new_link( AggregatorPid,
				_CloseListenerPid=self() );

		{ MinTCPPort, MaxTCPPort } ->
			class_TraceListener:synchronous_new_link( AggregatorPid,
				MinTCPPort, MaxTCPPort, _CloseListenerPid=self() )

	end,

	IsVerbose andalso app_facilities:display(
                        "Waiting for the trace listener to be closed." ),

	receive

		{ trace_listening_finished, TraceListenerPid } ->
			IsVerbose andalso app_facilities:display( "Trace listener closed." )

	end,

	% ?app_stop should not be used here as its wait_for_any_trace_supervisor
	% macro would wait for a non-launched supervisor.
	%
	% ?app_stop_without_waiting_for_trace_supervisor() is not used either, as
	% no aggregator was started from that test.
	%
	app_facilities:finished().
