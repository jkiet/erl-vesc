-module(uart).

-export([open/1, close/1, send/2, listen/0]).

open(Dev) ->
  SerialPort = serial:start([{open, Dev}, {speed, 115200}]),
  {ok, SerialPort}.

close(SerialPort) ->
  SerialPort ! {close},
  ok.
  
send(SerialPort, Payload) ->
  SerialPort ! {send, frame:form(Payload)},
  ok.

listen() ->
  listen(<<>>).

listen(Buffer) ->
  receive
    % Receive data from the serial port on the caller's PID.
    {data, Bytes} ->
      case frame:scan(<<Buffer/binary, Bytes/binary>>) of
        {continue, B} ->
          listen(B);
        X ->
          X
      end
  after
    1000 ->
      {timeout, Buffer}
end.
