-module(audiocapture).

-on_load(load_nif/0).
-export([start/2, stop/1, load_nif/0]).

load_nif() ->
  Path = case code:lib_dir(?MODULE,priv) of
    {error, _} -> "priv";
    Else -> Else
  end,
  erlang:load_nif(Path ++ "/audiocapture", 0).


start(Rate, Channels) ->
  Capture = real_start(Rate, Channels),
  {ok, Capture}.

real_start(Rate, Channels) -> erlang:error(nif_not_loaded).

stop(_Capture) ->
  erlang:error(not_implemented).
