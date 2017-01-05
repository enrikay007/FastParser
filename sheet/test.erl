-module(test).
-export([test/0]).

c(sheet). 
{ok,sheet}
241> c(test).    
{ok,test}
242> test:test().

test() -> 

	{ok, S} = sheet:sheet(),
	io:format("~p~n", ["Hello0"]),	
	{ok, CellPid1} = sheet:cell(S, er),
	io:format("~p~n", ["Hello11"]),
	{ok, CellPid2} = sheet:cell(S, 'fe'),
	io:format("~p~n", ["Hello13"]),
	{ok, CellPid4} = sheet:cell(S, test2),
	io:format("~p~n", ["Hello14"]),
	{ok, CellPid5} = sheet:cell(S, test3),
	io:format("~p~n", ["Hello14"]),
	sheet:add_viewer(CellPid1, CellPid2),
	io:format("~p~n", ["Hello14"]),
	sheet:add_viewer(CellPid1, CellPid5),
	sheet:add_viewer(CellPid1, CellPid4),
	io:format("~p~n", ["Hello15"]),
	sheet:get_viewers(CellPid1),
	io:format("~p~n", ["Hello16"]),
	sheet:remove_viewer(CellPid1, CellPid5),
	io:format("~p~n", ["Hello17"]),
	sheet:set_value(CellPid1, newValue),
	io:format("~p~n", ["Hello18"]),
	sheet:get_viewers(CellPid1),
	io:format("~p~n", ["Hello19"]),
	sheet:get_value(CellPid1),	
	sheet:set_value(CellPid1, ne),
	sheet:get_value(CellPid1).

