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
% Creation date: Saturday, February 15, 2020.


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

	% The identifier of task for certificate renewal (if any), as assigned by
	% the scheduler:
	%
	cert_task_id :: maybe( class_USScheduler:task_id() ) } ).



% Information about the analysis of web access logs:
-record( web_analysis_info, {

		   % Ex: 'awstats':
		   tool :: class_USWebConfigServer:log_analysis_tool_name(),

		   % Full path to the analyzer main executable:
		   %
		   % (typically awstats_buildstaticpages.pl)
		   %
		   tool_path :: file_utils:bin_executable_path(),

		   % Full path to nay analyzer helper:
		   %
		   % (typically awstats.pl)
		   %
		   helper_path :: maybe( file_utils:bin_executable_path() ),


		   % As read from template configuration file:
		   template_content :: binary(),

		   % Where the configuration file shall be generated:
		   conf_dir :: file_utils:bin_directory_path(),

		   % Where the HTML reports will be generated:
		   web_content_dir :: file_utils:bin_directory_path() } ).