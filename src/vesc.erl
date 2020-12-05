-module(vesc).

-define(MAX_INT_16, 65535).
-define(MAX_INT_32, 4294967295).

-define(CMD_VERSION, 0).
-define(CMD_PROBES, 4).
-define(CMD_THRUST, 6).
-define(CMD_REBOOT, 29).


-export([ini/1, fin/1, cmd/2, cmd/3]).

signed16(Int) when Int > (?MAX_INT_16 bsr 1) ->
  Int - ?MAX_INT_16;
signed16(Int) ->
  Int.

signed32(Int) when Int > (?MAX_INT_32 bsr 1) ->
  Int - ?MAX_INT_32;
signed32(Int) ->
  Int.

command(version, _) ->
  {<<0>>, true};
command(probes, _) ->
  {<<4>>, true};
command(thrust, Val) ->
  X = trunc(Val * 1000),
  {<<6, X:32>>, false};
command(reboot, _) ->
  {<<?CMD_REBOOT>>, false}.

response(<<?CMD_VERSION, FW:2/binary, HW:3/binary, 0, UUID/binary>>) ->
  {version, #{
    fw => binary_to_list(FW),
    hw => binary_to_integer(HW),
    uuid => binary_to_list(UUID)}};
response(<<?CMD_PROBES,
  Tfet:2/binary, Tmotor:2/binary,
  Imotor:4/binary, Iin:4/binary,
  Id:4/binary, Iq:4/binary,
  DutyCycle:2/binary, Rpm:4/binary,
  Uin:2/binary,
  AHr:4/binary, AHrCh:4/binary,
  WHr:4/binary, WHrCh:4/binary,
  Tacho:4/binary, TachoAbs:4/binary,
  FaultCode:1/binary,
  T/binary>>) ->
  {probes, #{
    t_fet => signed16(binary:decode_unsigned(Tfet)) / 10,
    t_motor => signed16(binary:decode_unsigned(Tmotor)) / 10,
    i_motor => signed32(binary:decode_unsigned(Imotor)) / 100,
    i_in => signed32(binary:decode_unsigned(Iin)) / 100,
    i_d =>  signed32(binary:decode_unsigned(Id)) / 100,
    i_q =>  signed32(binary:decode_unsigned(Iq)) / 100,
    duty => signed16(binary:decode_unsigned(DutyCycle)) / 1000,
    rpm => signed32(binary:decode_unsigned(Rpm)),
    u_in => signed16(binary:decode_unsigned(Uin)) / 10,
    amp_hr => signed32(binary:decode_unsigned(AHr)) / 10000,
    amp_hr_ch => signed32(binary:decode_unsigned(AHrCh)) / 10000,
    watt_hr => signed32(binary:decode_unsigned(WHr)) / 10000,
    watt_hr_ch => signed32(binary:decode_unsigned(WHrCh)) / 10000,
    tacho => signed32(binary:decode_unsigned(Tacho)),
    tacho_abs => signed32(binary:decode_unsigned(TachoAbs)),
    fault_code => binary:decode_unsigned(FaultCode),
    unknown => binary_to_list(T)}};
response(X) ->
  {unknown, X}.

cmd(Port, Cmd) ->
  cmd(Port, Cmd, nil).

cmd(Port, Cmd, Arg) ->
  case command(Cmd, Arg) of
    {Bytes, true} ->
      uart:send(Port, Bytes),
      case uart:listen() of
        {ok, Payload} ->
          {ok, response(Payload)};
        X ->
          X
      end;
    {Bytes, false} ->
      uart:send(Port, Bytes),
      ok
  end.


ini(Dev) ->
  uart:open(Dev).

fin(Port) ->
  uart:close(Port).
