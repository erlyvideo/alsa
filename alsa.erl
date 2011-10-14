#!/usr/local/bin/escript -pa ebin

main(_) ->
  io:format("Hi~n"),
  alsa:start(32000, 2),
  receive
    Msg -> io:format("~p~n", [Msg])
  end,
  io:format("Bye~n"),
  ok.


