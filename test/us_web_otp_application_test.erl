% Copyright (C) 2021-2025 Olivier Boudeville
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
% Creation date: Friday, February 26, 2021.

-module(us_web_otp_application_test).

-moduledoc """
Testing of **US-Web as an OTP active application**, directly from within its
code base (hence without needing to create a separate, mock-up test OTP release
for that).
""".


% For run/0 export and test traces:
-include_lib("traces/include/traces_for_tests.hrl").


% For default_us_web_{config,scheduler}_server_registration_name:
% (path not set yet from here)
%
-include("us_web_defines.hrl").



-doc "Runs the actual test.".
test_us_web_application( OrderedAppNames ) ->

    test_facilities:display( "Starting the US-Web OTP active application." ),

    test_facilities:display( "The version of this currently tested US-Web "
        "library is ~ts (i.e. ~w).", [
            class_USWebCentralServer:get_us_app_version_string(),
            class_USWebCentralServer:get_us_app_version() ] ),

    % We did not trap EXIT messages, as we wanted this test to crash (thanks to
    % the links below) in case of problem (and not to receive an EXIT message
    % bound not to be read, as it happened when no US configuration file was
    % found).
    %
    % However such tests may crash even when stopping (normally) applications,
    % as apparently an OTP application has its child processes terminated with
    % reason 'shutdown' (not 'normal').
    %
    % So now this test process traps EXIT messages, and ensures that none
    % besides {'EXIT',P,shutdown}, P being the PID of a US-Common process, is
    % received (actually for US-Common no such message is received, unlike for
    % the WOOPER counterpart test case).
    %
    false = erlang:process_flag( trap_exit, true ),

    % We have to link notably to the upcoming US-Web configuration
    % server. However starting an application does not provide a means of
    % knowing its PID (none returned) and this server is not registered under a
    % fixed name, as it may be read from the US-Web configuration file. So this
    % test has also to locate and read that file (for the US counterpart
    % configuration file) in order to determine the name to query:

    { BinCfgDir, USCfgFilePath, USCfgTable, USCfgRegName, USCfgRegScope } =
        us_common_otp_application_test:get_us_information(),

    % Now we can take care of the US-Web counterpart configuration file:
    { USWebCfgTable, USWebCfgFilePath } = case
            class_USWebCentralServer:get_configuration_table( USCfgTable,
                                                             BinCfgDir ) of

        { ok, P } ->
            P;

        { error, CfgDiagnosedError } ->
            basic_utils:throw_diagnosed( CfgDiagnosedError, USCfgFilePath )

    end,

    test_facilities:display( "Read US-Web configuration from '~ts'.",
                             [ USWebCfgFilePath ] ),

    { USWebCfgRegName, USWebCfgRegScope, USWebSchedRegName, USWebSchedRegScope,
      USWebRegMsg } = case
            class_USWebCentralServer:get_registration_info( USWebCfgTable ) of

        { ok, Q } ->
            Q;

        { error, RegDiagnosedError } ->
            basic_utils:throw_diagnosed( RegDiagnosedError )

    end,

    trace_bridge:info( USWebRegMsg ),

    % Now we are able to link to the future US-Web configuration server and
    % scheduler.


    % No ?test_start/?test_stop here, as we start/stop Traces through
    % OTP-related operations.
    %
    % If in batch mode (not in a release, hence no sys.config read here, so only
    % the --batch command-line option matters here), the trace aggregator will
    % record that a trace supervisor is wanted later (iff renamed), otherwise
    % (not in batch mode), no trace supervisor is wanted at all.
    %
    otp_utils:start_applications( OrderedAppNames ),

    % This is fully optional for this US-Web test, yet we prefer also linking to
    % US-related elements:
    %
    USCfgSrvPid = naming_utils:wait_for_registration_of( USCfgRegName,
        naming_utils:registration_to_lookup_scope( USCfgRegScope ) ),

    % The top-level user process may not be aware that an OTP application fails
    % (e.g. because its main process crashed), which is a problem for a test. So
    % here we link explicitly this test process to the US configuration server,
    % to have a chance of detecting issues:
    %
    erlang:link( USCfgSrvPid ),

    % Now linking to the actual US-Web of interest for this test:

    % First the US-Web configuration server:
    USWebCfgSrvPid = naming_utils:wait_for_registration_of( USWebCfgRegName,
        naming_utils:registration_to_lookup_scope( USWebCfgRegScope ) ),

    erlang:link( USWebCfgSrvPid ),

    % Then the US-Web scheduler:

    USWebSchedPid = naming_utils:wait_for_registration_of( USWebSchedRegName,
        naming_utils:registration_to_lookup_scope( USWebSchedRegScope ) ),

    erlang:link( USWebSchedPid ),


    % If not in batch mode, this renaming will trigger the launch of the trace
    % supervisor whose activation was deferred until then:
    %
    traces_utils:name_trace_file_from( ?MODULE ),

    ?test_info( "Starting the US-Web OTP active application." ),

    ?test_info_fmt( "US-Web version: ~p.",
                    [ system_utils:get_application_version( us_web ) ] ),

    TestPort = 8080,
    TestUrl = "index.html",

    ?test_info_fmt( "As a test, attempting to fetch page '~ts' from that "
        "just-launched US-Web instance, supposedly running on localhost, "
        "at TCP port #~B.", [ TestUrl, TestPort ] ),

    URI = text_utils:format( "http://localhost:~B/~ts", [ TestPort, TestUrl ] ),

    ExpectedContentBin = file_utils:read_whole(
        "../priv/for-testing/test-static-website-D/index.html" ),

    web_utils:start(),

    case web_utils:get( URI, _Headers=[], _HttpOptions=[] ) of

        { _StatusCode=200, _HeaderMap, Body } ->
            case Body of

                ExpectedContentBin ->
                    ?test_info_fmt( "Read, from the US-Web test instance, the "
                        "expected content ('~ts'), end-to-end test succeeded.",
                        [ Body ] );

                OtherStr ->
                    trace_bridge:error_fmt( "Read a content from the US-Web "
                        "test instance, yet not the expected one: read '~ts' "
                        "instead of '~ts'.", [ OtherStr, ExpectedContentBin ] ),
                    throw( { unexpected_web_content, OtherStr } )

            end;

        { StatusCode, HeaderMap, Body } ->
            trace_bridge:error_fmt( "Reading URI '~ts' from the US-Web test "
                "instance failed: status code is ~B (~ts), headers are ~p "
                "and body is ~p.",
                [ URI, StatusCode, web_utils:http_status_class_to_string(
                    web_utils:get_http_status_class( StatusCode ) ),
                HeaderMap, Body ] ),
            throw( { web_content_reading_failed, StatusCode, URI } )

    end,

    web_utils:stop(),

    % Of course shall be sent before the stopping of Traces:
    ?test_info( "Successful test (not fully ended yet) of the US-Web OTP "
                "application." ),

    % Including US-Web:
    ?test_info_fmt( "Stopping all user applications (~p).",
                    [ OrderedAppNames ] ),

    otp_utils:stop_user_applications( OrderedAppNames ),


    trace_utils:debug_fmt( "Waiting for the termination of the US-Web "
                           "configuration server (~w).", [ USWebCfgSrvPid ] ),

    receive

        { 'EXIT', USWebCfgSrvPid, normal } ->
            ok

        after 1000 ->
            ok

    end,


    trace_utils:debug_fmt( "Waiting for the termination of the US-Web "
                           "scheduler (~w).", [ USWebSchedPid ] ),

    receive

        { 'EXIT', USWebSchedPid, normal } ->
            ok

        after 1000 ->
            ok

    end,


    trace_utils:debug_fmt( "Waiting for the termination of the US-Common "
                           "configuration server (~w).", [ USCfgSrvPid ] ),

    receive

        { 'EXIT', USCfgSrvPid, normal } ->
            ok

        after 1000 ->
            ok

    end,

    % None expected to be left:
    basic_utils:check_no_pending_message(),

    test_facilities:display(
        "Successful end of test of the US-Web OTP application." ).



-doc """
Runs this application test.

Note that the `{us_web, us_common, traces, wooper, myriad}.app` files will have
to be found and used for this test to succeed: US-Web, US-Common, Traces, WOOPER
and Myriad must be already available as prerequisite, fully-built OTP
applications.
""".
-spec run() -> no_return().
run() ->

    test_facilities:start( ?MODULE ),

    % Build root directory from which sibling prerequisite applications may be
    % found:
    %
    BuildRootDir = "..",

    OrderedAppNames =
        otp_utils:prepare_for_execution( _ThisApp=us_web, BuildRootDir ),

    trace_bridge:info_fmt( "Resulting applications to start, in order: ~w.",
                           [ OrderedAppNames ] ),

    test_us_web_application( OrderedAppNames ),

    test_facilities:stop().
