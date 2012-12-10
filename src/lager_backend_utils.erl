%% Author: jason
%% Created: Jan 16, 2012
%% Description: TODO: Add description to lager_backend_utils
-module(lager_backend_utils).

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
-export([is_loggable/3]).

%%
%% API Functions
%%

-spec is_loggable(lager_msg:lager_msg(), integer(), term()) -> boolean().
is_loggable(LagerMsg, SeverityThreshold, MyName)->
  %%   ?debugFmt("is_loggable: Severity=~p, Threshold=~p, Destinations=~p, MyName=~p", [Severity,SeverityThreshold,Destinations,MyName]),
  Severity = lager_msg:severity_as_int(LagerMsg), 
  Destinations = lager_msg:destinations(LagerMsg),
  Severity =< SeverityThreshold orelse lists:member(MyName, Destinations).


-ifdef(TEST).

is_loggable_test_() ->
  [
    {"Loggable by severity only", ?_assert(is_loggable(lager_msg:new("message", {"date","time"}, alert, [],[]),2,me))},
    {"Not loggable by severity only", ?_assertNot(is_loggable(lager_msg:new("message", {"date","time"}, critical, [],[]),1,me))},
    {"Loggable by severity with destination", ?_assert(is_loggable(lager_msg:new("message", {"date","time"}, alert, [],[you]),2,me))},
    {"Not loggable by severity with destination", ?_assertNot(is_loggable(lager_msg:new("message", {"date","time"}, critical, [],[you]),1,me))},
    {"Loggable by destination overriding severity", ?_assert(is_loggable(lager_msg:new("message", {"date","time"}, critical, [],[me]),1,me))}
    ].

-endif.
