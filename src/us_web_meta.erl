% Copyright (C) 2019-2020 Olivier Boudeville
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
% Creation date: Wednesday, December 25, 2019.


% Module to handle 'meta' websites, i.e. (generated) websites about websites, to
% help their quick monitoring.
%
-module(us_web_meta).


-export([ get_page_header/0, get_page_body/5, get_page_footer/0 ]).

% To silence functions not used anymore:
-export([ get_vhost_description/2 ]).


% For the vhost_config_entry record:
-include("class_USWebConfigServer.hrl").


% Shorthands:
-type html_element() :: web_utils:html_element().
-type domain_id() :: class_USWebConfigServer:domain_id().
-type vhost_config_table() :: class_USWebConfigServer:vhost_config_table().
-type vhost_config_entry() :: class_USWebConfigServer:vhost_config_entry().
-type domain_config_table() :: class_USWebConfigServer:domain_config_table().
-type meta_web_settings() :: class_USWebConfigServer:meta_web_settings().
-type tcp_port() :: net_utils:tcp_port().

-type maybe_url() :: maybe( text_utils:string() ).



% Returns the HTML header for a meta page.
-spec get_page_header() -> html_element().
get_page_header() ->
	"<!DOCTYPE html>
<html lang=\"EN\">
  <head>
	<title>Meta website</title>
	<meta http-equiv=\"Content-Type\" content=\"text/html; charset=utf-8\">
	<meta http-equiv=\"Content-Language\" content=\"en\">
	<style>
	  body { background-color: #d2d2d2 ;}

	  table, th, td {
		  border: 1px solid black;
		  border-collapse: collapse;
		  padding: 15px;
	  }

	  /* Taken from https://stackoverflow.com/questions/166160/how-can-i-scale-the-content-of-an-iframe */

	  .wrap
	  {

		  width: 640px;
		  height: 384px;
		  padding: 0;
	  }

	  .frame
	  {

		  width: 1280px;
		  height: 768px;

		  -ms-transform: scale(0.5);
		  -moz-transform: scale(0.5);
		  -o-transform: scale(0.5);
		  -webkit-transform: scale(0.5);
		  transform: scale(0.5);

		  -ms-transform-origin: 0 0;
		  -moz-transform-origin: 0 0;
		  -o-transform-origin: 0 0;
		  -webkit-transform-origin: 0 0;
		  transform-origin: 0 0;
	  }
/* Taken from http://www.w3.org/Style/Examples/007/menus.html */

div.banner {
  background: #aaaaaa;
  margin: 0;
  font-size: 70%;
  font-weight: bold;
  line-height: 1.0;
  text-align: center;
  position: fixed;
  top: 1em;
  left: auto;
  width: 11em;
  right: 1em;
}


div.banner p {
  color: #555555;
  background: #999999;
  margin-left: 0em;
  padding:  0em;
  font-family: Arial, sans-serif;
  border: thin outset #333333;
  margin-top: 0%;
  margin-bottom: 0%;
  margin-left: 0%;
  margin-right: 0%;
}


div.banner a, div.banner em { display: block; margin: 0em; }
div.banner a, div.banner em { border-top: 1px groove #EEEEEE; } /* separator */
div.banner a:first-child { border-top: none; }
div.banner em { color: #FFFFFF; font-weight: bold; } /* head of column */

div.banner a:link { color: #EEEEEE; text-decoration: none;  }
div.banner a:visited { color: #CCCCCC; text-decoration: none;  }
div.banner a:hover { color: #000000; background: #FFFFFF;  }
	</style>
  </head>
".



% Returns the HTML body for a meta page.
-spec get_page_body( tcp_port(), domain_config_table(),
					 time_utils:timestamp(), meta_web_settings(), boolean() ) ->
						   html_element().
get_page_body( Port, DomainCfgTable, StartTimestamp, MetaWebSettings,
			   LogAnalysisEnabled ) ->

	text_utils:format(
		"<body>"
		"<div class=\"banner\"><p><em>Meta website</em>"
		"<a href=\"#meta_top\">go to top</a> "
		"<a href=\"#meta_bottom\">go to bottom</a>"
		"</p></div>"
		"<h1>Welcome to this automatically-generated <b>meta</b> "
		"website</h1>~n~n"
		"<p>Server started on ~s, running on TCP port #~B.</p>~n"
		"<a name=\"meta_top\"></a>"
		"<p><center><a href=\"#meta_bottom\">[Go to bottom]</a></center></p>"
		"<p>Excluding this meta website, ~s</p>~n",
		[ time_utils:get_textual_timestamp( StartTimestamp ), Port,
		  describe_hosting( Port, DomainCfgTable, MetaWebSettings,
							LogAnalysisEnabled ) ] ).



% Describes (in HTML) the current hosting.
describe_hosting( Port, DomainCfgTable,
	  _MetaWebSettings={ MetaDomainId, MetaVhostId, _BinMetaContentRoot },
	  LogAnalysisEnabled ) ->

	% Ex: MaybeBinMetaURL="http://meta.foobar.org:8080".
	MaybeMetaBaseURL = case LogAnalysisEnabled of

		true ->
			text_utils:format( "http://~s.~s:~B",
							   [ MetaVhostId, MetaDomainId, Port ] );

		false ->
			undefined

	end,

	% Even if this would work (tested as such), we prefer not including (and
	% displaying recursively) the 'meta' website within itself, so we remove it
	% first:

	VHostsIncludingMeta = table:get_value( MetaDomainId, DomainCfgTable ),

	VHostsWithoutMeta = table:remove_existing_entry( MetaVhostId,
													 VHostsIncludingMeta ),

	MetaLessCfgTable = table:add_entry( MetaDomainId, VHostsWithoutMeta,
										DomainCfgTable ),

	case table:enumerate( MetaLessCfgTable ) of

		[] ->
			"no domain referenced";

		SingleDomPair = [ { Domain, _VHostCfgTable } ] ->
			text_utils:format( "a single domain referenced, <b>~s</b>: ~s",
				[ Domain,
				  % Kept as we want to include the enclosing table:
				  get_domain_descriptions( Port, SingleDomPair,
										   MaybeMetaBaseURL ) ] );

		DomPairs ->
			text_utils:format( "~B domains referenced:~n~s",
			  [ length( DomPairs ),
				get_domain_descriptions( Port, DomPairs, MaybeMetaBaseURL ) ] )

	end.



% Returns the HTML description for the specified domains.
-spec get_domain_descriptions( tcp_port(),
   [ { domain_id(), vhost_config_table() } ], maybe_url() ) -> html_element().
get_domain_descriptions( Port, DomPairs, MaybeMetaBaseURL ) ->

	DomStrings = lists:sort(
		[ get_domain_description( Port, Dom, VCfgTable, MaybeMetaBaseURL )
		  || { Dom, VCfgTable } <- DomPairs ] ),

	web_utils:get_unordered_list( DomStrings ).



% Returns the HTML description for the specified domain.
-spec get_domain_description( tcp_port(), domain_id(),
					   vhost_config_table(), maybe_url() ) -> html_element().
get_domain_description( Port, _Domain=default_domain_catch_all,
						VCfgTable, MaybeMetaBaseURL ) ->
	text_utils:format( "for the domain catch-all (note that the corresponding "
		"URLs are likely not resolvable), ~s",
		[ get_vhost_descriptions( Port, VCfgTable, MaybeMetaBaseURL ) ] );

get_domain_description( Port, Domain, VCfgTable, MaybeMetaBaseURL ) ->
	text_utils:format( "for domain <b>~s</b>, ~s",
		[ Domain, get_vhost_descriptions( Port, VCfgTable,
										  MaybeMetaBaseURL ) ] ).




% Returns the HTML description for the specified virtual hosts.
-spec get_vhost_descriptions( tcp_port(), vhost_config_table(),
							  maybe_url() ) -> html_element().
get_vhost_descriptions( Port, VHostCfgTable, MaybeMetaBaseURL ) ->

	% (domain better fetched from the vhost entries)

	%trace_utils:debug_fmt( "Vhost configuration table: ~s",
	%					   [ table:to_string( VHostCfgTable ) ] ),

	case table:values( VHostCfgTable ) of

		[] ->
			"no virtual host defined";

		V=[ _VHostCfgEntry ] ->
			text_utils:format( "a single virtual host defined: ~s",
			%	[ get_vhost_description( Port, VHostCfgEntry ) ] );
				[ get_table_for( V, Port, MaybeMetaBaseURL ) ] );

		VHostCfgEntries ->
			%VStrings = lists:sort( [ get_vhost_description( Port, VE )
			%			 || VE <- VHostCfgEntries ] ),
			text_utils:format( "~B virtuals hosts defined:~n~s",
				[ length( VHostCfgEntries ),
			%	  web_utils:get_unordered_list( VStrings ) ] )
				  get_table_for( VHostCfgEntries, Port, MaybeMetaBaseURL ) ] )

	end.



% Returns the HTML description for the specified virtual host.
-spec get_vhost_description( tcp_port(), vhost_config_entry() ) ->
								   html_element().
get_vhost_description( Port, #vhost_config_entry{ virtual_host=VHost,
												  parent_host=ParentHost } ) ->

	{ URLVHost, NameVHost } = case VHost of

		default_vhost_catch_all ->
			{ "NON_EXISTING_VHOST", "*" };

		_ ->
			{ VHost, VHost }

	end,

	{ URLDomain, NameDomain } = case ParentHost of

		default_domain_catch_all ->
			{ "NON_EXISTING_DOMAIN", "*" };

		_ ->
			{ ParentHost, ParentHost }

	end,

	TargetURL = text_utils:format( "http://~s.~s:~B/",
								   [ URLVHost, URLDomain, Port ] ),

	TargetName = text_utils:format( "http://~s.~s:~B/",
								   [ NameVHost, NameDomain, Port ] ),

	ToBegin = case VHost of

		default_vhost_catch_all ->
			"virtual host catch-all is ";

		_ ->
			text_utils:format( "<b>~s</b> is ", [ VHost ] )

	end,

	ToAdd = case ParentHost of

		default_domain_catch_all ->
			"(not expected to be routable)";

		_ ->
			""

	end,

	text_utils:format( "~s<a href=\"~s\" target=\"_blank\">[~s]</a> ~s",
					   [ ToBegin, TargetURL, TargetName, ToAdd ] ).



% Returns an HTML table suitable for specified vhost entries.,
-spec get_table_for( [ vhost_config_entry() ], tcp_port(), maybe_url() ) ->
						   html_element().
get_table_for( VHostCfgEntries, Port, MaybeMetaBaseURL ) ->

	Begin = "
<p>
  <center>
	<table style=\"width:90%\">

	  <tr>
		<th>Virtual Host</th>
		<th>Index Page</th>
		<th>404 Page</th>
	  </tr>
",

	Cells = [ get_cell_for( VHEnt, Port, MaybeMetaBaseURL )
			  || VHEnt <- VHostCfgEntries ],

	End = "
	</table>
  </center>
</p>
",

	[ Begin, Cells, End ].



% Returns the HTML code for specified table cell.
get_cell_for( #vhost_config_entry{ virtual_host=VHost,
								   parent_host=ParentHost },
			  Port, MaybeMetaBaseURL ) ->

	{ DisplayVHost, URLVHost } = case VHost of

		default_vhost_catch_all ->
			{ "*", "ANY_VIRTUAL_HOST" };

		_ ->
			{ VHost, VHost }

	end,

	{ DisplayParentHost, URLParentHost } = case ParentHost of

		default_domain_catch_all ->
			{ "*", "ANY_DOMAIN" };

		_ ->
			{ ParentHost, ParentHost }

	end,

	URL = text_utils:format( "http://~s.~s:~B",
							 [ URLVHost, URLParentHost, Port ] ),

	DisplayFullHost = text_utils:format( "~s.~s",
										 [ DisplayVHost, DisplayParentHost ] ),

	LogAnalysisLink = case MaybeMetaBaseURL of

		undefined ->
			"";

		_ ->
			% Opening a new (blank) page is preferred, to avoid reloading more
			% frequently than needed the whole Meta page, which may be very
			% heavy (as including many full websites):
			%
			PageFilename = class_USWebLogger:get_file_prefix_for( ParentHost,
									VHost, _Tool=awstats ) ++ ".html",
			text_utils:format( "<a href=\"~s/~s\" target=\"_blank\">[log analysis]</a>",
							   [ MaybeMetaBaseURL, PageFilename ] )

	end,

	text_utils:format( "
  <tr>

	<td><center><a href=\"~s\">~s</a> <br>~s</center></td>

	<td>
	  <div class=\"wrap\">
		<iframe src=\"~s\" class=\"frame\"></iframe>
	  </div>
	</td>

	<td>
	  <div class=\"wrap\">
		<iframe src=\"~s/NON_EXISTING_PAGE.html\" class=\"frame\"></iframe>
	  </div>
	</td>

  </tr>
	", [ URL, DisplayFullHost, LogAnalysisLink, URL, URL ] ).



% Returns the HTML footer for a meta page.
-spec get_page_footer() -> html_element().
get_page_footer() ->
	"<p><center><a href=\"#meta_top\">[Go to top]</a></center></p>"
	"<a name=\"meta_bottom\"></a>"
	"</body>"
	"</html>".
