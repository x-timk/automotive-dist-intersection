-module(tester).

-export([main/0]).


main() ->

  Pid = spawn(dim_env, tester, []),

  %% Importante. Devo registrare environment con nome dim_env. Le auto useranno questo nome per contattare env
  register(dim_env, Pid).

