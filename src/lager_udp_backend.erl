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
-module(lager_udp_backend).

-behaviour(gen_event).

-export([init/1, handle_call/2, handle_event/2, handle_info/2, terminate/2,
         code_change/3]).

-record(state, {name, address, port, socket, level, formatter,format_config}).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-compile([{parse_transform, lager_transform}]).
-endif.

-include_lib("lager/include/lager.hrl").

check_config({host,undefined}) -> throw(host_must_not_be_undefined);
check_config({port,undefined}) -> throw(port_must_not_be_undefined);
check_config({port,P}) when not is_integer(P) -> throw(port_must_be_integer);
check_config({port,P}) when P<1 orelse P>65536 -> throw(port_out_of_range);
check_config({inet_family,F}) when F =/= inet6 orelse F=/= inet -> throw({invalued_inet_family,{allowed,[inet,inet6]}});
check_config({level,L}) -> case lists:member(L,?LEVELS) of true -> true; _ -> throw({invalid_level,L}) end;
check_config(_) -> true.

%% @private
init(Params)->
    Level        = proplists:get_value(level, Params, debug),
    Formatter    = proplists:get_value(formatter,Params,lager_default_formatter),
    FormatConfig = proplists:get_value(format_config,Params,[]),
    InetFamily   = proplists:get_value(inet_family,Params,inet),

    Host         = proplists:get_value(host,Params,undefined),
    Port         = proplists:get_value(port,Params,undefined),
    
    Name         = proplists:get_value(name, Params, {Host,Port}),  

    
    check_config({host,Host}),
    check_config({port,Port}),
    check_config({level,Level}),
    {ok,Address} = inet:getaddr(Host,InetFamily),

    % active=false since we never want to receive.  Buffer will fill and packets
    % will start bouncing
    {ok,Socket} = gen_udp:open(0,[binary,{active,false}]),

    {ok, #state{level=lager_util:level_to_num(Level), 
        name={?MODULE,Name},
        address=Address,
        port=Port,
        socket=Socket,
        formatter=Formatter, 
        format_config=FormatConfig}
    }.


%% @private
handle_call(get_loglevel, #state{level=Level} = State) ->
    {ok, Level, State};
handle_call({set_loglevel, Level}, State) ->
    case lists:member(Level, ?LEVELS) of
        true ->
            {ok, ok, State#state{level=lager_util:level_to_num(Level)}};
        _ ->
            {ok, {error, bad_log_level}, State}
    end;
handle_call(_Request, State) ->
    {ok, ok, State}.

%% @private
handle_event(Message,#state{level=L,formatter=Formatter,format_config=FormatConfig} = State) ->
    case lager_backend_utils:is_loggable(Message, L, State#state.name) of
        true ->
            Msg=Formatter:format(Message,FormatConfig),
            ok=gen_udp:send(State#state.socket,State#state.address,State#state.port,Msg),
            {ok, State};
        false ->
            {ok, State}
    end;
handle_event(_Event, State) ->
    {ok, State}.

%% @private
handle_info(_Info, State) ->
    {ok, State}.

%% @private
terminate(_Reason, _State) ->
    ok.

%% @private
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

-ifdef(TEST).

-define(TEST_CONFIG(Address,Port),[{level,info},{name,test},{formatter,lager_default_formatter},{format_config,[message]},{host,Address},{port,Port}]).

do_init() ->
    % Test pretends to be the server, open a server connection
    {ok,Socket}=gen_udp:open(0,[binary,{active,true}]),
    {ok,{Address,Port}}=inet:sockname(Socket),
    
    % configure to talk to the test
    ?MODULE:init(?TEST_CONFIG(Address,Port)).

basic_test_() ->
    [
     {"regular logging",
      fun() ->
              {ok,State}=do_init(),
              
              % Send a message
              ?MODULE:handle_event(#lager_log_message{message= <<"Test message">>,severity_as_int=?INFO},State),
              receive
                  {udp, _Socket, _IP, _InPortNo, Packet} -> ?assertMatch(<<"Test message">>, Packet)
                  after 500 -> throw(did_not_receive)
              end
      end
     },
     {"Test respects severity threshold",
      fun() ->
              {ok,State}=do_init(),
              
              % Send a message
              ?MODULE:handle_event(#lager_log_message{message= <<"Test message">>,severity_as_int=?DEBUG,destinations=[]},State),
              receive
                  {udp, _Socket2, _IP, _InPortNo, Packet} -> throw({should_not_have_received,Packet})
                  after 500 -> ok
              end
      end
     },
     {"Test direct destination",
      fun() ->
              {ok,State}=do_init(),
              
              % Send a message
              ?MODULE:handle_event(#lager_log_message{message= <<"Test message">>,severity_as_int=?DEBUG,destinations=[{?MODULE,test}]},State),
              receive
                  {udp, _Socket2, _IP, _InPortNo, Packet} -> ?assertMatch(<<"Test message">>, Packet)
                  after 1000 -> throw(did_not_receive)
              end
      end
     }
    ].

-endif.
