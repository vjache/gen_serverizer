# gen_serverizer utility application (Erlang)

## Intro

This app allows to expose some local functionality as a gen_server. 
It is possible to specify the whole module to be exposed or bind 
call to some registry name (`local`, `global`, {`via`, ...}).

## Motivation

Its quite annoying to write a simple `gen_server` each time when I want 
to have some functionality from remote Node, and it is not good idea 
to use `rpc`, due to its use of explicit `Node` name because of I do 
not want my system know about such a "physical" entity as 'Node' name. 
For me, its good idea to have functionality accesible remotely through 
'gen_server:call'. This is the purpose why this app created.

## Example

```erlang
   1> gen_serverizer:create_global_server_from_module(lists).
   {global,lists}
   2> gen_server:call({global, lists}, {max, [1,2,3,4,5,6]}).
   {ok, 6}
```

