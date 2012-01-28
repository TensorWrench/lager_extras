-module(lager_zlib_decorator).

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
%%  {formatter, {Formatter,Config}}  % The formatter and it's config that we will wrap.  Required.
%%  {algorithm, gzip | compress | zip } % The algorithm to use for compression.  Default: gzip
%%
-spec format(#lager_log_message{},list()) -> iolist().
format(#lager_log_message{}=Message,Config) ->
	{Formatter,FormatConfig}=proplists:get_value(formatter,Config,no_formatter_specified),
	Method=proplists:get_value(algorithm,Config,gzip),
	zlib:Method(Formatter:format(Message,FormatConfig)).

-ifdef(TEST).
basic_test_() ->
	[{"Just a plain message with default(gzip) algorithm.",
	  	fun() -> 
	  		Bin=iolist_to_binary(format(#lager_log_message{timestamp={"Day","Time"},
										  message="Message",
										  severity_as_int=lager_util:level_to_num(error),
										  metadata=[]},
									   [{formatter,{lager_default_formatter,[message]}}])),
			%starts with the magic numbers for gzip
			?assertMatch(<<31,139,_/binary>>,Bin), 
			?assertMatch(<<"Message">>,zlib:gunzip(Bin))
		end
	  },
	 {"Just a plain message with compress algorithm.",
	  	fun() -> 
	  		Bin=iolist_to_binary(format(#lager_log_message{timestamp={"Day","Time"},
										  message="Message",
										  severity_as_int=lager_util:level_to_num(error),
										  metadata=[]},
									   [{formatter,{lager_default_formatter,[message]}},{algorithm,compress}])),
			%starts with the magic numbers for zlib
			?assertMatch(<<120,156,_/binary>>,Bin), 
			?assertMatch(<<"Message">>,zlib:uncompress(Bin))
		end
	  },
	 {"Just a plain message with zip algorithm.",
	  	fun() -> 
	  		Bin=iolist_to_binary(format(#lager_log_message{timestamp={"Day","Time"},
										  message="Message",
										  severity_as_int=lager_util:level_to_num(error),
										  metadata=[]},
									   [{formatter,{lager_default_formatter,[message]}},{algorithm,zip}])),
			?assertMatch(<<"Message">>,zlib:unzip(Bin))
		end
	  },
	  {"Should error if no formatter specified",?_assertError({badmatch,no_formatter_specified},format(#lager_log_message{},[]))}
   ].


-endif.

