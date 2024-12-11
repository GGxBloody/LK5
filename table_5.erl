-module(table_5).
-export([run/0, benchmark/1, create_table/1, add_element/3, update_element/3, delete_element/3, get_element/3]).

run() ->
    Mechanisms = [list, proplist, map, ets, gb_trees],
    Results = lists:map(fun benchmark/1, Mechanisms),
    io:format("~nРезультати тестів:~n~p~n", [Results]).


benchmark(Mechanism) ->
    Keys = lists:seq(1, 10000),  
    io:format("Тестуємо ~p...~n", [Mechanism]),

    {AddTime, _} = timer:tc(fun() ->
        Table = create_table(Mechanism),
        lists:foldl(fun(Key, AccTable) -> 
            add_element(Mechanism, AccTable, Key)
        end, Table, Keys)
    end),
    io:format("Час додавання: ~p~n", [AddTime]),

    {UpdateTime, _} = timer:tc(fun() ->
        Table = create_table(Mechanism),
        lists:foreach(fun(Key) -> 
            update_element(Mechanism, Table, Key)
        end, Keys)
    end),
    io:format("Час оновлення: ~p~n", [UpdateTime]),

    {DeleteTime, _} = timer:tc(fun() ->
        Table = create_table(Mechanism),
        lists:foreach(fun(Key) -> 
            delete_element(Mechanism, Table, Key)
        end, Keys)
    end),
    io:format("Час видалення: ~p~n", [DeleteTime]),

    {ReadTime, _} = timer:tc(fun() ->
        Table = create_table(Mechanism),
        lists:foreach(fun(Key) -> 
            get_element(Mechanism, Table, Key)
        end, Keys)
    end),
    io:format("Час читання: ~p~n", [ReadTime]),

    {Mechanism, AddTime, UpdateTime, DeleteTime, ReadTime}.




create_table(list) -> [];
create_table(proplist) -> [];
create_table(map) -> #{};  
create_table(ets) -> ets:new(benchmark_table, [set, public]);  
create_table(gb_trees) -> gb_trees:empty().

add_element(list, Table, Key) -> [{Key, value} | Table];
add_element(proplist, Table, Key) -> [{Key, value} | Table];
add_element(map, Table, Key) -> maps:put(Key, value, Table);
add_element(ets, Table, Key) -> ets:insert(Table, {Key, value}), Table;
add_element(gb_trees, Table, Key) -> gb_trees:insert(Key, value, Table).

update_element(list, Table, Key) ->
    case lists:keyfind(Key, 1, Table) of
        false -> Table;  % Якщо ключ не знайдено
        _ -> lists:keyreplace(Key, 1, Table, {Key, new_value})
    end;
update_element(proplist, Table, Key) ->
    case lists:keyfind(Key, 1, Table) of
        false -> Table;
        _ -> lists:keyreplace(Key, 1, Table, {Key, new_value})
    end;
update_element(map, Table, Key) -> maps:put(Key, new_value, Table);
update_element(ets, Table, Key) -> ets:insert(Table, {Key, new_value}), Table;
update_element(gb_trees, Table, Key) -> gb_trees:insert(Key, new_value, Table).

delete_element(list, Table, Key) -> lists:keydelete(Key, 1, Table);
delete_element(proplist, Table, Key) -> lists:keydelete(Key, 1, Table);
delete_element(map, Table, Key) -> maps:remove(Key, Table);
delete_element(ets, Table, Key) -> ets:delete(Table, Key), Table;
delete_element(gb_trees, Table, Key) ->
    case gb_trees:is_empty(Table) of
        true -> Table;  % Якщо дерево порожнє, повертаємо його
        false -> gb_trees:delete(Key, Table)  % Видаляємо ключ із дерева
    end.

get_element(list, Table, Key) -> lists:keyfind(Key, 1, Table);
get_element(proplist, Table, Key) -> lists:keyfind(Key, 1, Table);
get_element(map, Table, Key) -> maps:get(Key, Table, undefined);  % Повертає undefined, якщо ключ не знайдено
get_element(ets, Table, Key) ->
    case ets:lookup(Table, Key) of
        [] -> undefined;
        [{_, Value}] -> Value
    end;
get_element(gb_trees, Table, Key) ->
    case gb_trees:lookup(Key, Table) of
        none -> undefined;
        {value, Value} -> Value
    end.
