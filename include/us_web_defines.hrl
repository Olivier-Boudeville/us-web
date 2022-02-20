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


% Common US-Web defines.

-define( default_us_web_config_server_registration_name, us_web_config_server ).
-define( default_us_web_scheduler_registration_name, us_web_scheduler ).

% Global, as we can configure the corresponding name:
-define( us_web_config_server_registration_scope, global_only ).

% Local, as fixed name:
-define( us_web_scheduler_registration_scope, local_only ).


% Allows to spoof the webserver identity when sending back information (always
% fun, yet not working currently):
%
% (typically instead of "Cowboy")
%
%-define( server_header_id, <<"Apache/2.4.1 (Unix)">> ).
-define( server_header_id, <<"Apache/2.4.1 (Unix)">> ).

% To discriminate with headers:
-define( server_req_id, <<"Apache/2.4.2 (Unix)">> ).


% Same from the upper US-Common level:
%-include_lib("us_common/include/us_common_defines.hrl").
