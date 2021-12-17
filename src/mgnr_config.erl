-module(mgnr_config).

-export([postfix_storage/2]).

-type storage_opts() :: mg_core_storage:options().

-spec postfix_storage(_Prefix, storage_opts()) -> storage_opts().
postfix_storage(_, {mg_core_storage_memory, _} = Storage) ->
    Storage;
postfix_storage(Postfix, {mg_core_storage_riak, #{bucket := Bucket} = Options}) ->
    {mg_core_storage_riak, Options#{bucket := mg_core_utils:concatenate_namespaces(Bucket, Postfix)}}.
