-module(audiocapture).

-export([start/3, stop/1]).


start(Device, Rate, Channels) ->
  Path = case code:lib_dir(?MODULE,priv) of
    {error, _} -> "priv";
    Else -> Else
  end,
  case erl_ddll:load_driver(Path, audiocapture_drv) of
  	ok -> ok;
  	{error, already_loaded} -> ok;
  	{error, Error} -> exit({error, {could_not_load_driver,erl_ddll:format_error(Error)}})
  end,
  Capture = open_port({spawn, audiocapture_drv}, [binary]),

  <<"ok">> = port_control(Capture, 1, <<Device, Channels, Rate:16>>),
  {ok, Capture}.

stop(_Capture) ->
  erlang:error(not_implemented).
