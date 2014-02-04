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
-spec format(lager_msg:lager_msg(),list()) -> iolist().
format(Message,Config) ->
	{Formatter,FormatConfig}=proplists:get_value(formatter,Config,no_formatter_specified),
	Method=proplists:get_value(algorithm,Config,gzip),
	zlib:Method(Formatter:format(Message,FormatConfig)).

-ifdef(TEST).
basic_test_() ->
	[{"Just a plain message with default(gzip) algorithm.",
	  	fun() -> 
          Bin=iolist_to_binary(format(lager_msg:new("Message", {"Day","Time"}, error, [], []),
									   [{formatter,{lager_default_formatter,[message]}}])),
			%starts with the magic numbers for gzip
			?assertMatch(<<31,139,_/binary>>,Bin), 
			?assertMatch(<<"Message">>,zlib:gunzip(Bin))
		end
	  },
	 {"Just a plain message with compress algorithm.",
	  	fun() -> 
          Bin=iolist_to_binary(format(lager_msg:new("Message", {"Day","Time"},error,[], []),
									   [{formatter,{lager_default_formatter,[message]}},{algorithm,compress}])),
			%starts with the magic numbers for zlib
			?assertMatch(<<120,156,_/binary>>,Bin), 
			?assertMatch(<<"Message">>,zlib:uncompress(Bin))
		end
	  },
	 {"Just a plain message with zip algorithm.",
	  	fun() -> 
          Bin=iolist_to_binary(format(lager_msg:new("Message", {"Day","Time"},error, [], []),
									   [{formatter,{lager_default_formatter,[message]}},{algorithm,zip}])),
			?assertMatch(<<"Message">>,zlib:unzip(Bin))
		end
	  },
	  {"Should error if no formatter specified",?_assertError({badmatch,no_formatter_specified},format(lager_msg:new("Message", {"Day","Time"},error,[], []),[]))}
   ].


-endif.

