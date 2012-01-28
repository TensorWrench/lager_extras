-module(lager_ansi_color_decorator).

%%
%% Include files
%%
-include_lib("lager/include/lager.hrl").
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.
%%
%% Exported Functions
%%
-export([format/2]).

-define(END_COLOR,"\e[0m").


%% @doc
%% Outputs the message and all of it's metadata in GELF format.
%% Config is a prop list that contains:
%%  {formatter, {Formatter,Config}}  % The formatter and it's config that we will wrap.  Required.
%%  {algorithm, gzip | compress | zip } % The algorithm to use for compression.  Default: gzip
%%
-spec format(#lager_log_message{},list()) -> iolist().
format(#lager_log_message{severity_as_int=Level}=Message,Config) ->
	{Formatter,FormatConfig}=proplists:get_value(formatter,Config,no_formatter_specified),
	[proplists:get_value(lager_util:num_to_level(Level),Config,default_color(Level)),
	 Formatter:format(Message,FormatConfig),
	 ?END_COLOR].

default_color(?DEBUG) -> "\e[0;37m";
default_color(?INFO) -> "\e[1;38m";
default_color(?NOTICE) -> "\e[1;36m";
default_color(?WARNING) ->  "\e[1;33m";
default_color(?ERROR) -> "\e[1;31m";
default_color(?CRITICAL) -> "\e[5m\e[1;43m";
default_color(?ALERT) -> "\e[5m\e[1;45m";
default_color(?EMERGENCY) -> "\e[5m\e[1;41m".

-ifdef(TEST).

manual_test() ->
	F=fun(L) ->
		V=format(#lager_log_message{timestamp={"Day","Time"},
									  message="Message",
									  severity_as_int=lager_util:level_to_num(L),
									  metadata=[]},
				   [{formatter,{lager_default_formatter,[]}}]),
		?debugMsg(V)
	end,
	[ F(L) || L <- [debug,info,notice,warning,error,critical,alert,emergency]].


-endif.

