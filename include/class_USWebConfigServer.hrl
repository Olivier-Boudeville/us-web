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
% Creation date: Saturday, February 15, 2020.


% General web settings, typically useful for the us_web supervisor:
-record( general_web_settings, {

	% Rules so that http accesses are managed by the right handlers:
	http_dispatch_rules :: class_USWebConfigServer:dispatch_rules(),

	% TCP port (ex: 80) to serve any http content:
	http_tcp_port :: maybe( net_utils:tcp_port() ),


	% Rules so that https accesses are managed by the right handlers:
	https_dispatch_rules :: class_USWebConfigServer:dispatch_rules(),

	% TCP port (ex: 443) to serve any https content:
	https_tcp_port :: maybe( net_utils:tcp_port() ),


	% Tells whether certificates shall be used, and how:
	certificate_support :: class_USWebConfigServer:cert_support(),

	% Including for SNI information:
	https_transport_info ::
			maybe( class_USWebConfigServer:https_transport_info() ),

	% The absolute path to the Diffie-Helman key file (if any) to ensure a
	% secure key exchange with Forward Secrecy:
	%
	dh_key_path :: maybe( file_utils:bin_file_path() ),

	% The absolute path to the certificate authority PEM file (if any) for the
	% chain of trust:
	%
	ca_cert_key_path :: maybe( file_utils:bin_file_path() ) } ).



% The full configuration information regarding a virtual host (ex:
% "bar.foo.org"):
%
-record( vhost_config_entry, {

	% The subdomain (ex: <<"bar">>) corresponding to this virtual host:
	virtual_host :: net_utils:bin_host_name(),

	% The parent host (ex: <<"foo.org">>) of this virtual one:
	parent_host :: class_USWebConfigServer:domain_id(),

	% The kind of virtual host (ex: 'static'):
	kind :: class_USWebConfigServer:web_kind(),

	% The content (absolute) directory (as a binary) associated to this virtual
	% host:
	%
	content_root :: file_utils:bin_directory_path(),

	% The PID of the web logger in charge of this virtual host:
	logger_pid :: class_USWebLogger:server_pid(),

	% The PID of the certificate manager (if any) for this virtual
	% host, in charge of certificate generation and renewal:
	%
	cert_manager_pid :: maybe( class_USCertificateManager:manager_pid() ) } ).



% Information about the analysis of web access logs:
-record( web_analysis_info, {

	% Ex: 'awstats'.
	tool :: class_USWebConfigServer:log_analysis_tool_name(),


	% Full path to the main analyzer of access logs, to update the database
	% thereof:
	%
	% (typically awstats.pl, possibly in /usr/share/webapps/awstats/cgi-bin/)
	%
	update_tool_path :: maybe( file_utils:bin_executable_path() ),


	% Full path to the executable in charge of the generation of HTML reports:
	%
	% (typically awstats_buildstaticpages.pl, possibly in
	% /usr/share/awstats/tools)
	%
	report_tool_path :: file_utils:bin_executable_path(),


	% As read from template configuration file:
	template_content :: binary(),

	% Where the configuration file shall be generated:
	conf_dir :: file_utils:bin_directory_path(),

	% Where the state files of the log analyzer will be stored:
	state_dir :: file_utils:bin_directory_path(),

	% Where the HTML reports will be generated:
	web_content_dir :: file_utils:bin_directory_path() } ).
