%% Author: jason
%% Created: Jan 27, 2012
%% Description: TODO: Add description to lager_json_formatter
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
-spec format(#lager_log_message{},list()) -> iolist().
format(#lager_log_message{}=Msg,[]) ->
	format(Msg,[date, " ", time," [",severity,"] ",pid, " ", message, "\n"]);
format(#lager_log_message{}=Message,Config) ->
	Encoder=lager_extras_mochijson2:encoder([{handler,fun json_handler/1},{utf8,proplists:get_value(utf8,Config,true)}]),
	Encoder(Message).

%% -record(lager_log_message,{
%% 						   destinations,
%% 						   metadata,
%% 						   severity_as_int,
%% 						   timestamp,
%% 						   message
%% 						   }).
json_handler(#lager_log_message{message=Message,metadata=Metadata,timestamp={Date,Time},severity_as_int=Level}) ->
	Severity=lager_util:num_to_level(Level),
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
				      iolist_to_binary(format(#lager_log_message{timestamp={"Day","Time"},
															  message="Message",
															  severity_as_int=lager_util:level_to_num(error),
															  metadata=[]},
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
				      iolist_to_binary(format(#lager_log_message{timestamp={"Day","Time"},
															  message="Message",
															  severity_as_int=lager_util:level_to_num(error),
															  metadata=[{module,?MODULE},{function,my_function},{line,999},{pid,pid_to_list(self())}]},
									   [])))
	  }	 
	 ].


-endif.

