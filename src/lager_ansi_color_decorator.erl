% Copyright 2012 Tensor Wrench LLC.  All rights reserved.
% https://github.com/TensorWrench/lager_extras

% Redistribution and use in source and binary forms, with or without
% modification, are permitted provided that the following conditions are
% met:
%
%     * Redistributions of source code must retain the above copyright
% notice, this list of conditions and the following disclaimer.
%     * Redistributions in binary form must reproduce the above
% copyright notice, this list of conditions and the following disclaimer
% in the documentation and/or other materials provided with the
% distribution.
%     * Neither the name of TensorWrench,LLC nor the names of its
% contributors may be used to endorse or promote products derived from
% this software without specific prior written permission.
%
% THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
% "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
% LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
% A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
% OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
% SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
% LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
% DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
% THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
% (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
% OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
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
%%  {debug | info | notice | warning | error | critical | alert | emergency, color()} % override the color for a particular log level.  Optional.
%%
-spec format(lager_msg:lager_msg(),list()) -> iolist().
format(Message,Config) ->
	{Formatter,FormatConfig}=proplists:get_value(formatter,Config,no_formatter_specified),
	[proplists:get_value(lager_msg:severity(Message),Config,default_color(lager_msg:severity_as_int(Message))),
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
      V=format(lager_msg:new("Message", {"Day","Time"}, L, [], []),
				   [{formatter,{lager_default_formatter,[]}}]),
		?debugMsg(V)
	end,
	[ F(L) || L <- [debug,info,notice,warning,error,critical,alert,emergency]].


-endif.

