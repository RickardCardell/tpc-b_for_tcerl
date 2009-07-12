-module(go).
-compile(export_all).
-define(sync_treshhold,700000).


sync(Tabs) when is_list(Tabs) ->
    Res = [ sync(T) || T <- Tabs],
    [R || R <- Res, R =/= ok ];
sync(Tab) ->
    {ok, Port} = tcbdbsrv:get_tab (Tab), 
    {T, Res} = timer:tc(tcbdbets,sync,[Port]),
    case T > sync_treshhold of
	true -> io:format(" ~p ",[T div 10000 / 100]);
	_ -> ok
    end,
    Res.
	    



loop_sync(Every) ->
    receive
	stop -> io:format(" stopped "), {ok, stopped}
    after Every ->		
	    Res2= try (sync([account, branch, teller, history])) of
		      Res  -> Res
		  catch
		      E:R -> {E,R}
		  end,
	    if Res2 =:= [] -> io:format("S");
	       true -> io:format("X")
	    end,
	    loop_sync(Every)
    end.

		
