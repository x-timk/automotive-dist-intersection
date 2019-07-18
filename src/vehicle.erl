-module(vehicle).
-behaviour(gen_statem).

%% Export API pubbliche
-export([send_posfree_resp/2, send_disc/2]).

%% Export API solo tra veicoli, valutare se serve
% -export([send_hello/2, send_wait/3]).

-define(NAME, panda).

%% Uso macro per definire i possibili eventi
-define(ONTOP, ontop).
-define(ONTAIL, ontail).
-define(WAIT, wait).
-define(DISC, disc).
-define(HELLO, hello).
-define(STOP, stop).

%% Timeout events
-define(DISC_TM, disc_tm).
-define(ELECT_TM, elect_tm).

%% Evento scatenato quando la auto vuole muoversi alla prossima posizione
-define(CAR_MV, car_mv).

%% Eventi bully
-define(BULLY_ELECT, elect).
-define(BULLY_COORD, coord).
-define(BULLY_ANS, ans).
-define(BULLY_NO_ANS, no_ans).

%% Eventi sensoristica
-define(POSFREE_RESP,posfree_resp).

%% Valori Timeout di stati
-define(STATE_DISCOVER_TIMEOUT, 5000).
-define(STATE_ELECTION_TIMEOUT, 5000).
-define(BULLY_ANSWER_TIMEOUT, 1000).


%% Messaggio richiesta movimento veicolo

-export([start_link/1]).
-export([init/1, callback_mode/0, terminate/3]).
-export([inqueue/3, discover/3, election/3, slave/3, master/3]).
%% -export([print/3]).

%% tupla contenente tutte le info che l'auto si porta dietro tra i vari stati
-record(cardata, {
    env,
    name,
    desc,
    route,
    neighbourPids,
    speed,
    isLeader = false
    }).


print(CarData, Format, What) ->
  Def = io_lib:format("CAR ~p: ", [get_name(CarData)]),
  Usr = io_lib:format(Format, What),
  io:format(Def ++ Usr ++ "~n").
  % io:format("PID " ++ MyPid ++ ": " ++ What ++ "~n").

%% state_functions: Events are handled by one callback function per state. 
%% %% Una funzione di gestione specifica per ogni stato
%% in alternativa posso definire un unica funzione di callback generale con:
%% callback_mode() ->
  % handle_event_function.
callback_mode() ->
  [state_functions, state_enter].


%% Faccio partire la macchina a stati finiti: (viene spawnato un processo a livello pratico)
%% 1) Parametro: registro localmente la state machine con nome "machine_test"
%%    Se non la registro ci devo accedere tramite PID
%% 2) Nome del modulo dove si trovano le funzioni di callback (?MODULE è una macro per indicare questo stesso modulo)
%% 3) Parametro che viene passato alla funzione di init del modulo ?MODULE
%% 4) Lista di eventuali opzioni avanzate per la state machine
%% L'idea è che quando faccio spawn di una macchina la lancio con i parametri {Env, Name, Desc, Route, Speed}
start_link(Data) ->
  {Env, Name, Desc, Route, Speed} = Data,
  CarData = #cardata{env = Env, name = Name, desc = Desc, route = Route, neighbourPids = [], speed = Speed},
  gen_statem:start_link({local,CarData#cardata.name}, ?MODULE, CarData, []).


%% chiamata se riesco a registrare correttamente la state machine.
%% Valore di ritorno atteso è una tupla del tipo
%% {ok, nome_stato_iniziale, Var} dove Var sara' nel nostro caso il record cardata che mi porto via stato per stato 
init(CarData) ->
  print(CarData, "<<~s>> Car SPAWNED!", [?FUNCTION_NAME]),
  print(CarData, "<<~s>> Car Data is ~p", [?FUNCTION_NAME, CarData]),
  {ok, inqueue, CarData, [{timeout, 2000, ?CAR_MV}]}.

%% Quando invio un messaggio dall'esterno alla state machine 
%% (esempio "invio a car1 un disc da parte di car332" sarebbe: car1 ! {disc, car332})
%% quest'ultima se lo vede arrivare di default come la tupla {info, {disc, car332}}
%% Nel caso io non voglia avere quell'info posso chiamare gen_statem:cast
%% 
% disc(Data_Evento) ->
%   gen_statem:cast(?NAME, {disc, Data_Evento}).
%% In questo modo i messaggi disc arriverebbero come {cast, {disc, car332}}


get_neighbours(CarData) ->
  CarData#cardata.neighbourPids.

%% Resetto i vicini conosciuti (la uso quando torno in discover dopo un election fallita)
reset_neighbours(CarData) ->
  NewCarData = #cardata{env = CarData#cardata.env, 
                        name = CarData#cardata.name, 
                        desc = CarData#cardata.desc, 
                        route = CarData#cardata.route, 
                        neighbourPids = [],
                        speed = CarData#cardata.speed},
  NewCarData.

%% Aggiungo Car ai vicini conosciuti
update_neighbours(CarData, Car) ->
  Neighbour = CarData#cardata.neighbourPids,
  NewNeighbour = rem_dups([ Car | Neighbour]),
  NewCarData = #cardata{env = CarData#cardata.env, 
                        name = CarData#cardata.name, 
                        desc = CarData#cardata.desc, 
                        route = CarData#cardata.route, 
                        neighbourPids = NewNeighbour,
                        speed = CarData#cardata.speed},
  NewCarData.


%% Da usare quando mi sono mosso per cancellare dalla rotta la posizione che ho appena lasciato
%% L'idea e' che in cardata.route ho la lista dei nodi che occuperò (compreso il nodo attuale)
pop_position(CarData) ->
  Cars = CarData#cardata.route,
  NewCars = aux_pop_position(Cars),
  NewCarData = #cardata{env = CarData#cardata.env, 
                        name = CarData#cardata.name, 
                        desc = CarData#cardata.desc, 
                        route = NewCars, 
                        neighbourPids = CarData#cardata.neighbourPids,
                        speed = CarData#cardata.speed},
  NewCarData.

aux_pop_position([]) ->
  [];
aux_pop_position([_|T]) ->
  T.

%% Restituisco tupla {nodo, tipo}
current_pos(CarData) ->
  aux_current_pos(CarData#cardata.route).

aux_current_pos([]) ->
  {finish, arrived};
aux_current_pos([H | _]) ->
  H.

%% Restituisco tupla {nodo, tipo}
next_pos(CarData) ->
  aux_next_pos(CarData#cardata.route).

aux_next_pos([_ | [S | _]] ) ->
  S;
aux_next_pos(_) ->
  arrived.

get_env(CarData) ->
  CarData#cardata.env.

get_name(CarData) ->
  CarData#cardata.name.

get_route(CarData) ->
  CarData#cardata.route.

%% Rimuovo duplicati da una lista
rem_dups(List) ->
    Set = sets:from_list(List),
    sets:to_list(Set).

%% Se Cond è true ritorna X, altrimenti ritorna Y
bool(Cond, X, Y) ->
  if
    is_boolean(Cond) andalso Cond -> X;
    is_boolean(Cond) andalso not Cond -> Y;
    not is_boolean(Cond) -> erlang:error(badarg)
  end.

is_empty([]) -> true;
is_empty([_ | _]) -> false.

move_to_next_pos(CarData) ->
  {CurrentPos, _} = current_pos(CarData),
  {NextPos, _}= next_pos(CarData),
  dim_env:notify_move(get_name(CarData), CurrentPos, NextPos),
  pop_route_leg(CarData).

pop_route_leg(CarData = #cardata{route=Route}) ->
  CarData#cardata{route=tl(Route)}.

%% Vehicle PUBLIC API
send_disc(DestCar, FromCar) ->
  gen_statem:cast(DestCar, {?DISC, FromCar}).

send_posfree_resp(Car, Data) ->
  gen_statem:cast(Car, {?POSFREE_RESP, Data}).

send_hello(Car, FromCar) ->
  gen_statem:cast(Car, {?HELLO, FromCar}).

send_wait(Car, FromCar, Reason) ->
  gen_statem:cast(Car, {?WAIT, {FromCar, Reason}}).

send_bully_elect(Car) ->
  try
    gen_statem:call(Car, ?BULLY_ELECT, ?BULLY_ANSWER_TIMEOUT)
  catch
    exit:{timeout,_} -> ?BULLY_NO_ANS
  end.

send_bully_coord(Car) ->
  gen_statem:cast(Car, ?BULLY_COORD).

inqueue(enter, _OldState, CarData) ->
  print(CarData, "Entered in <<~s>> state", [?FUNCTION_NAME]),
  {keep_state, CarData};

inqueue(cast, {?DISC, _FromCar}, CarData) ->
  print(CarData, "<<~s>>:: Received Event DISC, but I am not in discovery. Ignoring this message", [?FUNCTION_NAME]),
  print(CarData, "<<~s>>:: Remaining in this state", [?FUNCTION_NAME]),
  {next_state, inqueue, CarData};


%% Se ricevo un timeout generico di tipo ?CAR_MV significa che l'auto vorrebbe
%% muoversi, dunque devo interrogare sensore prossimità
inqueue(timeout, ?CAR_MV, CarData) ->
  print(CarData,
        "<<~s>>:: Received event ~p. I Will query proximity sensor.",
        [?FUNCTION_NAME, ?CAR_MV]),
  MvTimeOut = {timeout, CarData#cardata.speed, ?CAR_MV},
  {NextNode, NextNodeType} = next_pos(CarData),
  %% chiedo al sensore se la posizione davanti e' libera
  IsFree = dim_env:req_prox_sensor_data(get_name(CarData), NextNode),
  print(CarData,
        "<<~s>>:: Received Event posfree_resp ~p",
        [?FUNCTION_NAME, IsFree]),
  case {IsFree, NextNodeType} of
    {false, _} ->
      {next_state, inqueue, CarData, [MvTimeOut]};
    {true, tail_node} ->
      print(CarData, "Moving to next node", []),
      NewCarData = move_to_next_pos(CarData),
      {next_state, inqueue, NewCarData, [MvTimeOut]};
    {true, top_node} ->
      print(CarData, "Moving to next node and going into <<discover>>", []),
      NewCarData = move_to_next_pos(CarData),
      {next_state, discover, NewCarData}
  end;

%% Gestisco tutti gli altri messaggi che arrivano quando sono nello stato "inqueue"
%% Message corrisponde al tipo di evento ricevuto (ho usato macro)
%% MessageData e' l'eventuale payload del messaggio
%% CarData e' il record contenente le informazioni globali della macchina
inqueue(info, Msg, CarData) ->
  print(CarData, "<<~s>>:: Received INFO UNKNOWN EVENT. Ignoring this message: ~p", [?FUNCTION_NAME, Msg]),
  {next_state, inqueue, CarData}.


discover(enter, _OldState, CarData) ->
  print(CarData, "Entered in <<~s>> state", [?FUNCTION_NAME]),
  NewCarData = reset_neighbours(CarData),
  dim_env:broadcast_disc({get_name(NewCarData), get_route(NewCarData)}),
  {keep_state,NewCarData, [{state_timeout, ?STATE_DISCOVER_TIMEOUT, ?DISC_TM}]};


discover(cast, {?DISC, Msg={FromCar, _Route}}, CarData) ->
  print(CarData, "<<~s>>:: Received Event ~p", [?FUNCTION_NAME, ?DISC]),
  print(CarData, "Remaining in state <<~s>> and adding ~p to my neighbours",
       [?FUNCTION_NAME, Msg]),
  % salvo id macchina da cui ho ricevuto disc
  NewCarData = update_neighbours(CarData, Msg),
  % NewCarData = #cardata{name = CarData#cardata.name, desc = CarData#cardata.desc, route = CarData#cardata.route, neighbourPids = [ToProc | CarData#cardata.neighbourPids]},
  % Invio hello al mittente, segnalo che ci sono anche io.
  send_hello(FromCar, {get_name(NewCarData), get_route(NewCarData)}),
  {next_state, discover, NewCarData};

discover(state_timeout, ?DISC_TM, CarData) ->
  print(CarData, "<<~s>>:: Received event ~p", [?FUNCTION_NAME, ?DISC_TM]),
  print(CarData, "<<~s>>:: Passing to State <<election>> with neighbours: ~p",
        [?FUNCTION_NAME, get_neighbours(CarData)]),
  {next_state, election, CarData, [{state_timeout, ?STATE_ELECTION_TIMEOUT, ?ELECT_TM}]};

discover(cast, {?HELLO, Msg}, CarData) ->
  print(CarData, "<<~s>>:: Received Event ~p", [?FUNCTION_NAME, ?HELLO]),
  print(CarData, "Remaining in state <<~s>> and adding ~p to my neighbours",
        [?FUNCTION_NAME, Msg]),

  NewCarData = update_neighbours(CarData, Msg),
  {next_state, discover, NewCarData};

discover(cast, {?WAIT, {_FromCar, _Reason}}, CarData) ->
  print(CarData, "<<~s>>:: Received Event ~p", [?FUNCTION_NAME, ?WAIT]),
  print(CarData, "Remaining in state <<~s>> and resetting timeout",
        [?FUNCTION_NAME]),
  timer:sleep(2000),
  repeat_state_and_data;

discover({call, From}, ?BULLY_ELECT, CarData) ->
  {next_state, election, CarData, [{reply, From, ?BULLY_ANS}]};
discover(cast, ?BULLY_COORD, CarData) ->
  %% Avendo implementato il Bully come da libro questo stato dovrebbe essere un errore
  {next_state, election, CarData};

discover(info, Msg, CarData) ->
  print(CarData, "<<~s>>:: Received INFO UNKNOWN EVENT. Ignoring this message: ~p", [?FUNCTION_NAME, Msg]),
  {next_state, discover, CarData};
discover(A, B, C) ->
  print(C, "EventType: ~p Content: ~p", [A, B]).


election(enter, _OldState, CarData) ->
  print(CarData, "Entered in <<~s>> state", [?FUNCTION_NAME]),
  Me = get_name(CarData),
  Others = lists:map(fun({C,_}) -> C end, get_neighbours(CarData)),
  print(CarData, "Me: ~p Others: ~p", [Me, Others]),
  GreaterPids = lists:filter(fun(Other) -> Other > Me end, Others),
  print(CarData, "greater Pids: ~p", [GreaterPids]),
  Answers = lists:map(fun(Other) ->
                          {Other, send_bully_elect(Other)}
                      end,
                      GreaterPids),
  {BullyAns, _BullyNoAns} = lists:partition(
                             fun({_, Ans}) -> Ans =:= ?BULLY_ANS end,
                             Answers
                            ),
  %% Se BullyANs è vuoto allora sono il leader
  %% Se sono il leader non ncessito di un election timeout
  {IsLeader, ElectionTimeOut} = bool(is_empty(BullyAns),
                                     {true, 0},
                                     {false, ?STATE_ELECTION_TIMEOUT}),
  {keep_state,
   CarData#cardata{isLeader=IsLeader},
   [{state_timeout, ElectionTimeOut, ?ELECT_TM}]};

election(state_timeout, ?ELECT_TM, CarData = #cardata{isLeader=true}) ->
  {next_state, master, CarData};

election(state_timeout, ?ELECT_TM, CarData = #cardata{isLeader=false}) ->
  %% Elezione fallita, torno in discover
  NewCarData = reset_neighbours(CarData),
  {next_state, discover, NewCarData};

election(cast, {?DISC, {FromCar, _Route}}, CarData) ->
  % salvo id macchina da cui ho ricevuto disc
  % Invio hello al mittente, segnalo che ci sono anche io.
  send_wait(FromCar, get_name(CarData), "i'm in election state"),
  % salvo id macchina da cui ho ricevuto disc
  {keep_state, CarData};

%% TODO: un po tutto quanto... da implementare i messaggi di wait

election(cast, {?HELLO, {FromCar, _Route}}, CarData) ->
  % salvo id macchina da cui ho ricevuto disc
  % Invio hello al mittente, segnalo che ci sono anche io.
  send_wait(FromCar, get_name(CarData), "I'm in election"),
  keep_state_and_data;

election({call, From}, ?BULLY_ELECT, _CarData) ->
  {keep_state_and_data, [{reply, From, ?BULLY_ANS}]};
election(cast, ?BULLY_COORD, CarData) ->
  {next_state, slave, CarData};

election(info, Msg, CarData) ->
  print(CarData, "<<~s>>:: Received INFO UNKNOWN EVENT. Ignoring this message: ~p", [?FUNCTION_NAME, Msg]),
  {next_state, election, CarData}.

slave(enter, _OldState, CarData) ->
  print(CarData, "Slave waiting the master", []),
  keep_state_and_data;
slave(cast,{?DISC, {FromCar, _Route}}, CarData) ->
  %% Invio wait al mittente
  send_wait(FromCar, get_name(CarData), "I'm a SLAVE"),
  keep_state_and_data;
slave(cast, _, _) ->
  keep_state_and_data;
slave(info, From, CarData) ->
  From ! CarData,
  keep_state_and_data.

master(enter, _OldState, CarData) ->
  print(CarData, "Master ready to coordinate", []),
  lists:map(fun({Car,_}) -> send_bully_coord(Car) end, CarData#cardata.neighbourPids),
  keep_state_and_data;
master(cast,{?DISC, {FromCar, _Route}}, CarData) ->
  %% Invio wait al mittente.
  send_wait(FromCar, get_name(CarData), "I'm the LEADER"),
  keep_state_and_data;

master(cast, _, _) ->
  keep_state_and_data;
master(info, From, CarData) ->
  From ! CarData,
  keep_state_and_data.


terminate(_Reason, _State, _Data) ->
    ok.
