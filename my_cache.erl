-module(my_cache).
-export([create/1, insert/3, insert/4, lookup/2, delete_obsolete/1]).

%% Створення таблиці
create(TableName) ->
    ets:new(TableName, [named_table, public, set]),
    ok.

%% Додавання елемента без обмеження часу життя
insert(TableName, Key, Value) ->
    case ets:info(TableName) of
        undefined -> {error, no_such_table};
        _ ->
            ets:insert(TableName, {Key, Value, infinity}),
            ok
    end.

%% Додавання елемента з обмеженням часу життя
insert(TableName, Key, Value, TTL) when is_integer(TTL), TTL > 0 ->
    case ets:info(TableName) of
        undefined -> {error, no_such_table};
        _ ->
            Expiration = erlang:system_time(seconds) + TTL,
            ets:insert(TableName, {Key, Value, Expiration}),
            ok
    end.

%% Пошук елемента
lookup(TableName, Key) ->
    case ets:info(TableName) of
        undefined ->
            {error, no_such_table};
        _ ->
            case ets:lookup(TableName, Key) of
                [] -> undefined;
                [{_, Value, infinity}] -> Value;
                [{_, Value, Expiration}] ->
                    CurrentTime = erlang:system_time(seconds),
                    if
                        Expiration > CurrentTime -> Value;
                        true -> undefined
                    end
            end
    end.

%% Видалення застарілих елементів
delete_obsolete(TableName) ->
    case ets:info(TableName) of
        undefined -> {error, no_such_table};
        _ ->
            CurrentTime = erlang:system_time(seconds),
            ObsoleteKeys = [Key || {Key, _, Expiration} <- ets:tab2list(TableName),
                            Expiration =/= infinity, Expiration =< CurrentTime],
            lists:foreach(fun(Key) -> ets:delete(TableName, Key) end, ObsoleteKeys),
            ok
    end.
