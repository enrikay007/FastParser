-module(sheet).

%% API
-export([ sheet/0
        , cell/2
        , add_viewer/2
        , remove_viewer/2
        , get_viewers/1
        , set_value/2
        , get_value/1
        ]).

%%%===================================================================
%%% API
%%%===================================================================


sheet() ->
    {ok, spawn(fun() -> sheetServer(dict:new()) end)}.


             
    
cell (S,A) ->
     % {S, A} = init(),
     % {ok, spawn(fun() -> cellServer(S, A) end)}
  %   Ref = make_ref(),
     C  = rpc(S, {add_cell, A}),
          rpc(S, {add_name, {A,C}}),
    {ok,C}.
    
add_viewer(C,P) ->
    rpc(C, {add_viewer,P}).
		
remove_viewer(C, P) ->
    rpc(C, {remove_viewer,{C, P}}).
	
set_value(C, V) ->
    noResponse(C, V).
 	
     
% add_viewer(C, P) ->
    % rpc(C, {addviewer, P}).
    
% Sheet() -> 
% add_viewer(C,P) ->
	% P ! {self(), get_viewer},
	% receive
		% {P, {ok,C}} -> 
			% C ! {self(),{add_viewer,{C,P}}},
			% receive
				% {C, ok} -> "viewer has been added";
				% {C, {error,Error2}} -> Error2
			% end;
		% {P, {error,Error1}} -> Error1
	% end.

%%%===================================================================
%%% Internal functions
%%%===================================================================

% rpc(C,P) ->
	% C ! {self(), P},
	% receive
		% {C, P} -> P
	% end.
	
	
  
noResponse(Pid, Request) ->
	Pid ! {self(), Request}.

% get the name of a Pid
get_Viewer(Pid) ->
	rpc(Pid,get_viewer).

get_cells(S) ->
    rpc(S, get_cells).
  
get_value(C) ->
	rpc(C, get_value).
 
get_viewers(Pid) ->
	rpc(Pid, get_viewers).

%broadcast a message (M) to all of (P) friends within radius (R)
broadcast(P, M, R) ->
	MsgRef = make_ref(),
	noResponse(P, {broadcastMsg, {P,MsgRef, M, R}}).



  %% You don't really need this add_viewer in the sheet server, it is just a little confusing. 
  
   %% {From, {add_viewer,{C,P}}} ->
   %%  case dict:is_key(C, P) of 
   %%      false -> {ok, dict:store(C, P)},
   %%               reply(From, ok);
   %%      true  -> {{error, C, is_already_there},
   %%               From ! {self(), {error, "Name is already on the friend list"}}},
    %%	sheetServer(Database)
  % Alternative clause for true, throwing an exception
	%true  -> throw({error, Name, is_already_there})
   %% end;
   
   
   
   sheetServer(Database) ->
	receive
  
    {From, {add_cell, A}} ->
      C  = spawn_link(fun () -> 
          cellServer(dict:store(msgRefs,[],
		               dict:store(msgs,[],
		               dict:store(viewerlist,[],
		               dict:store(viewer, A, 
                   dict:store(value, undefined,
                   dict:store(sheetServer, From,
		               dict:new()))))))) end),
         From ! {self(), C}, 
         sheetServer(Database);
      
    {From, get_cells} -> 
      {ok, SheetList} = dict:find(sheetlist, Database),
      From ! {self(), SheetList}, 
        sheetServer(Database);

  
		{From, {{add_name,{A, C}}}} ->
			{ok,SheetList} = dict:find(sheetlist, Database),
                       case lists:member({A, C} , SheetList) of
                            false ->
			        NewSheetList = [ {A, C} | SheetList],
			        NDatabase = dict:store(sheetlist,NewSheetList, Database),
			        From ! {self(), {ok, {A, C}}},
			        sheetServer(NDatabase);
                            true -> 
                                From ! {self(), {error, "Name is already on the friend list"}},
                                sheetServer(Database)
                        end;
   {From, Other} ->
		From ! {self(), {error, {Other}}},
			sheetServer(Database)
	end.

    
  
cellServer(Database) ->
	receive
		{From, get_viewer} ->
			From ! {self(), dict:find(viewer, Database)},
			cellServer(Database);
		{From, get_viewers} ->
			{ok, ViewerList} = dict:find(viewerlist, Database), 
			From ! {self(), ViewerList},
			cellServer(Database);
		{From, {add_viewer,P}} ->
			{ok,ViewerList} = dict:find(viewerlist, Database),
                        case lists:member(P, ViewerList) of
                            false ->
			        NewViewerList = [P | ViewerList],
			        NDatabase = dict:store(viewerlist,NewViewerList, Database),
              
			         From ! {self(), ok},
			        cellServer(NDatabase);
                            true -> 
                                From ! {self(), {error, "Cell is already on the view list"}},
                                cellServer(Database)
                        end;

						
						
						
%%%  Remove viewer implemented		
		{From, {remove_viewer, {C,P}}} ->
		{ok,ViewerList} = dict:find(viewerlist, Database),
			case lists:member(P, ViewerList) of
					true ->
				NewViewerList = lists:delete(P, ViewerList),
				NDatabase = dict:store(viewerlist, NewViewerList, Database),
				From ! {self(), ok},
			    cellServer(NDatabase);
					false  -> 
						 From ! {self(), {error, "Cell is not on view list"}},
                         cellServer(Database)
                      end;
%%%  Remove viewer implemented			
			
	{From, {set_value, V}} -> 
	  io:format("Set value:~p~n", [V]),
      case V of {formula, F, Deps} ->  
        {ok, S} = dict:find(sheetServer, Database),
		CellList = get_cells(S),
		CellListPids = lists:map(fun(X) -> element(2, S) end, CellList), %% gets the Pids of all the cells in the sheetserver S
		io:format("Get value of Cell ~p~n", ["Hello2"]),
		lists:map(fun(X) -> remove_viewer(X, self()) end, CellListPids), %% removes all old viewers of self() = Calling cells pid
		io:format("Get value of Cell ~p~n", ["Hello2"]),
		ListofPids = lists:map(fun(X) -> lists:keyfind(X, 1, CellList) end, Deps), %% gets Pids to all name in Deps
		io:format("Get value of Cell ~p~n", [Deps]),
		lists:map(fun(X) -> add_viewer(X, self()) end, ListofPids), %% adds all the cells pids in ListofPids as viewers of self().  
		Value = lists:map(fun(X) -> get_value(X) end, ListofPids),%% gets list of value from ListofPids. 
		io:format("Get value of Cell ~p~n", [Value]),
	     %%NewValue  = F(Value),
	  	 NDatabase = dict:store(value, V, Database),
		cellServer(NDatabase);
	   
	  _ ->  
    
			NDatabase = dict:store(value, V, Database),
			io:format("Get value of Cell h ~p~n", [V]),
			cellServer(NDatabase)
    end;
          
		{From, get_value} ->
			{ok, Value} = dict:find(value, Database),
				io:format("Get value of Cell ~p~n", [Value]),
				From ! {self(), Value},
				cellServer(Database);
				
			
			
		{_, {broadcastMsg, Package}} ->
			{P, MsgRef, M, R} = Package,  % Unpack the values
			{ok,MsgRefs} = dict:find(msgRefs,Database), % Get the List of Refs
			%first check if the message is already on the list to stop resending it
			case lists:member(MsgRef,MsgRefs) of
				false ->
					NewMsgRefs = [MsgRef | MsgRefs],
					{ok, Messages} = dict:find(msgs, Database),
					NewMsgs = [{P,M} | Messages],
                                        %then we have to check the radius for resending
					case R > 0 of
						true -> 	
							%first get the friendlist and then send to friends with (R-1)
							{ok, ViewerList} = dict:find(viewerlist, Database),
							lists:map(fun({_,Pid}) -> Pid ! {self(), {broadcastMsg, {P,MsgRef, M, R-1}}}  end, ViewerList),
							true;

						false -> false
						%cease the sending process
					end,
					cellServer(dict:store(msgRefs,NewMsgRefs,
						     dict:store(msgs, NewMsgs, Database)));
				true ->
					cellServer(Database)
			end;
			{From, getmessages} ->
				{ok, Messages} = dict:find(msgs, Database),
				From ! {self(), Messages},
				cellServer(Database);
		{From, Other} ->
			From ! {self(), {error, {Other}}},
			cellServer(Database)
	end.

  
  
% sheet() -> 
    

% rpc(Pid,Request) ->
	% Pid ! {self(), Request},
	% receive
		% {Pid, Response} -> Response
	% end.



%%% Coordinator
% cellServer(S, A) ->
     % receive
        % {From, {add, S, A}} -> 
            % {Name,_,_} = Contact,
            % case dict:is_key(Name, Contacts) of 
                % false ->
                    % From ! {self(), ok},
                    % loop(dict:store(Name, Contact, Contacts));
                % true -> 
                    % From ! {self(), {error, Name, is_already_there}},
                    % Cell(Contacts)
            % end;
        % {From, list_all} ->
            % List = dict:to_list(Contacts),
            % From ! {self(), {ok, lists:map(fun({_, C}) -> C end, List)}},
            % loop(Contacts);
        % {From, {update, Contact}} ->
            % {Name,_,_} = Contact,
            % NewContacts = dict:erase(Name, Contacts),
            % From ! {self(), ok},
            % loop(dict:store(Name, Contact, NewContacts));
        % {From, Other} ->
            % From ! {self(), {error,unknow_request, Other}},
            % loop(Contacts)
    % end.


%%% Coordinator
% Sheet() ->
     % receive
        % {From, {add, S}} -> 
            % {Name,_,_} = Contact,
            % case dict:is_key(Name, Contacts) of 
                % false ->
                    % From ! {self(), ok},
                    % loop(dict:store(Name, Contact, Contacts));
                % true -> 
                    % From ! {self(), {error, Name, is_already_there}},
                    % Cell(Contacts)
            % end;
        % {From, list_all} ->
            % List = dict:to_list(Contacts),
            % From ! {self(), {ok, lists:map(fun({_, C}) -> C end, List)}},
            % loop(Contacts);
        % {From, {update, Contact}} ->
            % {Name,_,_} = Contact,
            % NewContacts = dict:erase(Name, Contacts),
            % From ! {self(), ok},
            % loop(dict:store(Name, Contact, NewContacts));
        % {From, Other} ->
            % From ! {self(), {error,unknow_request, Other}},
            % loop(Contacts)
    % end.
    
    
    % receive
  % {From, stop} ->
      % io:format("~p stopping~n", [self()]),
      % lists:foreach(fun stop_async/1, MapperList),
      % stop_async(Reducer),
      % reply_ok(From);
    % {From, {job, {MapFun, RedFun, RedInit, Data}}} ->
        % io:format("starting job~n"),
        % send message to the reducer fist (but I am wandering why I cannot do it later?)
        % Reducer ! {startJob, {self(), RedFun, RedInit, length(Data)}},
        %%% async(Reducer, {start_job, {self(), RedFun, RedInit, length(Data)}}),

        % send message to every single mapper (Mapper) to start the job
        % lists:foreach(fun(Mapper) -> Mapper ! {startJob, MapFun} end, MapperList),
        %%%%lists:foreach(fun(Mapper) -> async(Mapper, {start_job, MapFun}) end, MapperList),
        % and the data to the mappers
        %%%%send_data(MapperList, Data),


        % integrate the results
        %%%%Result = get_reducer_result(Reducer),
        % Result = rpc(Reducer, whatever),
        %%%%reply_ok(From, Result),
        %%%cell(S, A)
    %%%%end.


%%% Communication primitives

async(Pid, Msg) ->
    Pid ! Msg.

rpc(Pid, Request) ->
    Pid ! {self(), Request},
    receive
	{Pid, Response} ->
	    Response
    end.

reply(From, Msg) ->
    From ! {self(), Msg}.


%%% Server loops
