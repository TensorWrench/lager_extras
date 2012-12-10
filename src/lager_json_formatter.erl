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
-module(lager_json_formatter).

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

%%
%% API Functions
%%
-spec format(lager_msg:lager_msg(), list()) -> iolist().
format(LagerMsg, Config) ->
	Encoder=lager_extras_mochijson2:encoder([{handler,fun json_handler/1},{utf8,proplists:get_value(utf8,Config,true)}]),
	Encoder(LagerMsg).

json_handler(LagerMsg) ->
  Message = lager_msg:message(LagerMsg),
  Metadata = lager_msg:metadata(LagerMsg),
  {Date, Time} = lager_msg:timestamp(LagerMsg),
  Severity = lager_msg:severity(LagerMsg),
	{struct,[{date,list_to_binary(Date)},
			 {time,list_to_binary(Time)},
			 {severity,Severity},
			 {message,iolist_to_binary(Message)} 
			| [ {K,make_printable(V)} ||  {K,V} <- Metadata]]}.

make_printable(A) when is_atom(A) orelse is_binary(A) orelse is_number(A) -> A;
make_printable(P) when is_pid(P) -> iolist_to_binary(pid_to_list(P));
make_printable(Other) -> iolist_to_binary(io_lib:format("~p",[Other])).


-ifdef(TEST).
basic_test_() ->
	[{"Just a plain message.",
	  	?_assertEqual(iolist_to_binary(<<"{\"date\":\"Day\",\"time\":\"Time\",\"severity\":\"error\",\"message\":\"Message\"}">>),
                  iolist_to_binary(format(lager_msg:new("Message", {"Day","Time"}, error, [], []),
            %% #lager_log_message{timestamp={"Day","Time"},
															  %% message="Message",
															  %% severity_as_int=lager_util:level_to_num(error),
															  %% metadata=[]},
									   [])))
	  },
	 {"Just a plain with standard metadata.",
	  	?_assertEqual(iolist_to_binary([<<"{\"date\":\"Day\",\"time\":\"Time\",\"severity\":\"error\",\"message\":\"Message\"">>,
										<<",\"module\":\"">>,atom_to_list(?MODULE),$",
										<<",\"function\":\"my_function\"">>,
										<<",\"line\":999">>,
									    <<",\"pid\":\"\\\"">>, pid_to_list(self()),<<"\\\"\"">>,
										"}"

									  ]),
                  iolist_to_binary(format(lager_msg:new("Message",{"Day","Time"}, error, [{module,?MODULE},{function,my_function},{line,999},{pid,pid_to_list(self())}], []),
            
            %% #lager_log_message{timestamp={"Day","Time"},
															  %% message="Message",
															  %% severity_as_int=lager_util:level_to_num(error),
															  %% metadata=[{module,?MODULE},{function,my_function},{line,999},{pid,pid_to_list(self())}]},
									   [])))
	  }	 
	 ].


-endif.

