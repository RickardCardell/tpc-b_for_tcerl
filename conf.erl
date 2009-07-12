%%%-------------------------------------------------------------------
%%% File    : conf.erl
%%% Author  :  <kaptenzoom@HP>
%%% Description : 
%%%
%%% Created : 10 Jul 2009 by  <kaptenzoom@HP>
%%%-------------------------------------------------------------------
-module(conf).

-compile(export_all).



test() ->
    FName = "tc.conf",
    Conf = case file:consult(FName) of
	       {error, Rsn} ->
		   io:format("No ~~/.kredinit file found,  "
			     " need to create one (or bad format: ~p - ~p~n",
			     [FName, Rsn]),throw({error,Rsn});
	       {ok, L} ->
		   L
	   end,
    io:format("L: ~p ~n",[Conf]).



lk(Key, List) ->
    lists:keysearch(Key, 1, List).

lk(Key, List, Def) ->
    case lists:keysearch(Key, 1, List) of
	false -> {value, {Key, Def}};
	Val   -> Val
    end.
