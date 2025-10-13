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
% Creation date: Saturday, February 15, 2020.


% General web settings, typically useful for the us_web supervisor:
-record( general_web_settings, {

    % Rules so that http accesses are managed by the right handlers:
    http_dispatch_rules :: class_USWebCentralServer:dispatch_rules(),

    % TCP port (e.g. 80) to serve any http content:
    http_tcp_port :: option( net_utils:tcp_port() ),


    % Rules so that https accesses are managed by the right handlers:
    https_dispatch_rules :: class_USWebCentralServer:dispatch_rules(),

    % TCP port (e.g. 443) to serve any https content:
    https_tcp_port :: option( net_utils:tcp_port() ),


    % Tells whether certificates shall be used, and how:
    certificate_support :: class_USWebCentralServer:cert_support(),

    % Including for SNI information:
    https_transport_info ::
        option( class_USWebCentralServer:https_transport_info() ),

    % The absolute path to the Diffie-Helman key file (if any) to ensure a
    % secure key exchange with Forward Secrecy:
    %
    dh_key_path :: option( file_utils:bin_file_path() ),

    % The absolute path to the certificate authority PEM file (if any) for the
    % chain of trust:
    %
    ca_cert_key_path :: option( file_utils:bin_file_path() ) } ).



% The full configuration information regarding a virtual host (e.g.
% "bar.foo.org"):
%
-record( vhost_config_entry, {

    % The subdomain (e.g. <<"bar">>) corresponding to this virtual host:
    virtual_host :: class_USWebCentralServer:vhost_id(),

    % The parent host (e.g. <<"foo.org">>) of this virtual one:
    parent_host :: class_USWebCentralServer:domain_id(),

    % The kind of virtual host (e.g. 'static'):
    kind :: class_USWebCentralServer:web_kind(),

    % The content (absolute) directory (as a binary) associated to this virtual
    % host:
    %
    content_root :: file_utils:bin_directory_path(),

    % The PID of the web logger in charge of this virtual host:
    logger_pid :: class_USWebLogger:logger_pid(),

    % The PID of the certificate manager (if any) for this virtual
    % host, in charge of certificate generation and renewal:
    %
    cert_manager_pid :: option( class_USCertificateManager:manager_pid() ) } ).



% Information about the analysis of web access logs:
-record( web_analysis_info, {

    % For example 'awstats':
    tool :: class_USWebCentralServer:log_analysis_tool_name(),


    % Full path to the main analyzer of access logs, to update the database
    % thereof:
    %
    % (typically awstats.pl, possibly in /usr/share/webapps/awstats/cgi-bin/)
    %
    update_tool_path :: option( file_utils:bin_executable_path() ),


    % Full path to the executable in charge of the generation of HTML reports:
    %
    % (typically awstats_buildstaticpages.pl, possibly in
    % /usr/share/awstats/tools)
    %
    report_tool_path :: file_utils:bin_executable_path(),


    % As read from template configuration file:
    template_content :: text_utils:bin_string(),

    % Where the configuration file shall be generated:
    conf_dir :: file_utils:bin_directory_path(),

    % Where the state files of the log analyzer will be stored:
    state_dir :: file_utils:bin_directory_path(),

    % Where the HTML reports will be generated:
    web_content_dir :: file_utils:bin_directory_path() } ).
