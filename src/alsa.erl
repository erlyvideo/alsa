-module(alsa).

-on_load(load_nif/0).
-export([start/2, start/3, stop/1, load_nif/0]).

load_nif() ->
  Path = case code:lib_dir(?MODULE,priv) of
    {error, _} -> "priv";
    Else -> Else
  end,
  erlang:load_nif(Path ++ "/alsa", 0).


start(Rate, Channels) ->
  start(Rate, Channels, -1).

start(Rate, Channels, Device) ->
  Capture = real_start(Rate, Channels, Device),
  {ok, Capture}.

real_start(_Rate, _Channels, _Device) -> erlang:error(nif_not_loaded).

stop(_Capture) ->
  erlang:error(not_implemented).
