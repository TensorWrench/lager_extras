-module(lager_gelf_formatter).

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



%% @doc
%% Outputs the message and all of it's metadata in GELF format.
%% Config is a prop list that contains:
%%  {host, Name}  % name of the host to report.  default: "erlang"
%%  {facility, Name} % facility name used to report. default: "erlang"
%%  {short_message_size, N::integer()} % first N bytes of the message will be the short message. default: 80
%%  {utf8,boolean()} % whether to use utf8 or not in the JSON.  default: true 
-spec format(#lager_log_message{},list()) -> iolist().
format(#lager_log_message{}=Msg,[]) ->
	format(Msg,[date, " ", time," [",severity,"] ",pid, " ", message, "\n"]);
format(#lager_log_message{}=Message,Config) ->
	Encoder=lager_extras_mochijson2:encoder([{utf8,proplists:get_value(utf8,Config,true)}]),
	Encoder(convert(Message,Config)).

convert(#lager_log_message{message=Message,metadata=Metadata,severity_as_int=Level},Config) ->
	ShortMessageSize=proplists:get_value(short_message_size,Config,80),
	LongMessage=iolist_to_binary(Message),
	{Mega,Sec,Micro} = os:timestamp(),
	Timestamp=Mega*1000000000 + Sec*1000000 + Micro,
	case size(LongMessage) =< ShortMessageSize of 
		true -> ShortMessage=LongMessage;
		_ -> <<ShortMessage:ShortMessageSize/binary,_/binary>> = LongMessage
	end,
	{struct,[{version,<<"1.0">>},
			 {level,Level},
			 {short_message,ShortMessage},
			 {long_message,LongMessage},
			 {timestamp,Timestamp},
			 {line, proplists:get_value(line,Metadata,-1)},
			 {file, proplists:get_value(module,Metadata,unknown)},
			 {host,proplists:get_value(host,Config,erlang)},
			 {facility,proplists:get_value(facility,Config,erlang)}
			| [ {make_key(K),make_printable(V)} ||  {K,V} <- Metadata]]}.

make_printable(A) when is_atom(A) orelse is_binary(A) orelse is_number(A) -> A;
make_printable(P) when is_pid(P) -> iolist_to_binary(pid_to_list(P));
make_printable(Other) -> iolist_to_binary(io_lib:format("~p",[Other])).

make_key(id) -> <<"_user_id">>;
make_key(K) when is_atom(K) -> list_to_binary(["_" | atom_to_list(K)]).

-ifdef(TEST).
to_property([K,V]) ->
	{binary_to_atom(K, utf8),V}.

basic_test_() ->
	[{"Just a plain message.",
	  	fun() -> 
	  		<<${,$",Bin/binary>>=iolist_to_binary(format(#lager_log_message{timestamp={"Day","Time"},
										  message="Message",
										  severity_as_int=lager_util:level_to_num(error),
										  metadata=[]},
									   [])),
 	        Properties=[ to_property(re:split(P,"\"?:\"?",[{return,binary}])) || P <- re:split(Bin, "\"?,\"?|\"}", [{return, list},trim])],
			?assertMatch(<<"1.0">>,proplists:get_value(version,Properties)),
			?assertMatch(<<"3">>,proplists:get_value(level,Properties)),
			?assertMatch(<<"Message">>,proplists:get_value(short_message,Properties)),
			?assertMatch(<<"Message">>,proplists:get_value(long_message,Properties))
		end
	  },
	 {"Just a plain with standard metadata.",
	  	fun() -> 
	  		<<${,$",Bin/binary>>= iolist_to_binary(format(#lager_log_message{timestamp={"Day","Time"},
															  message="Message",
															  severity_as_int=lager_util:level_to_num(error),
															  metadata=[{module,?MODULE},{function,my_function},{line,999},{pid,pid_to_list(self())}]},[])),
 	        Properties=[ to_property(re:split(P,"\"?:\"?",[{return,binary}])) || P <- re:split(Bin, "\"?,\"?|\"}", [{return, list},trim])],
			?assertMatch(<<"lager_gelf_formatter">>,proplists:get_value(file,Properties)),
			?assertMatch(<<"999">>,proplists:get_value(line,Properties)),
			?assertMatch(<<"my_function">>,proplists:get_value('_function',Properties)),
			?assertMatch(<<"\\\"<", _/binary>>,proplists:get_value('_pid',Properties))
		end
	  }	 
	 ].


-endif.

