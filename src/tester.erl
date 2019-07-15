-module(tester).

-export([main/0]).


aux_next_pos([_ | [S | _]] ) ->
  {Node,_} = S,
  Node;
aux_next_pos(_) ->
  arrived.

main() ->
  % dim_env:tester().
  % List = [{inord,tail},{isud,asd},{c,addd}],
  % List = [{inord,tail}],
  % aux_next_pos(List).

  Pid = spawn(dim_env, tester, []),
  register(dim_env, Pid).

  % vehicle:start_link({environment, car1, "Fiat", [inord2,inord1]}),
  % vehicle:start_link({environment, car2, "Ferrari", [isud2,isud1]}).
