-module(hex).
-export([
         bin_to_hexstr/1,
         hexstr_to_bin/1,
         int/1,
         list_to_hexstr/1,
         int_to_hexstr/1
        ]).


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Converts Int to Hex
%%
%% @spec int(HexChar) -> Int
%% @end
%%--------------------------------------------------------------------

int(C) when $0 =< C, C =< $9 ->
    C - $0;
int(C) when $A =< C, C =< $F ->
    C - $A + 10;
int(C) when $a =< C, C =< $f ->
    C - $a + 10.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Converts String to HexStr
%%
%% @spec list_to_hexstr(List) -> HexStr
%% @end
%%--------------------------------------------------------------------

list_to_hexstr([]) ->
    [];
list_to_hexstr([H|T]) ->
    [int_to_hexstr(H) | list_to_hexstr(T)].

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Converts Binary to HexStr
%%
%% @spec bin_to_hexstr(<<Binary>>) -> HexStr
%% @end
%%--------------------------------------------------------------------

bin_to_hexstr(Bin) ->
    list_to_hexstr(binary_to_list(Bin)).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Converts Binary to HexStr
%%
%% @spec hexstr_to_bin(HexStr) -> <<Binary>>
%% @end
%%--------------------------------------------------------------------

hexstr_to_bin(S) ->
    list_to_binary(hexstr_to_list(S)).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Converts HexStr to List
%%
%% @spec hexstr_to_list(HexStr) -> List
%% @end
%%--------------------------------------------------------------------

hexstr_to_list([X,Y|T]) ->
    [int(X)*16 + int(Y) | hexstr_to_list(T)];
hexstr_to_list([]) ->
    [].

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Converts Integer to HexStr
%%
%% @spec int_to_hexstr(Int) -> HexStr
%% @end
%%--------------------------------------------------------------------

int_to_hexstr(Value) ->
    Hex = string:to_upper(lists:flatten(io_lib:format("~.16B", [Value]))),
    HexLen = trunc(length(Hex)) rem 2 + trunc(length(Hex)),
    Format = "~"++integer_to_list(HexLen)++"."++integer_to_list(HexLen-1)++".0s",
    lists:flatten(io_lib:format(Format,[Hex])).

