-module(vehicle).
-behaviour(gen_statem).
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

%% Eventi sensoristica
-define(POSFREE_RESP,posfree_resp).

%% Valori Timeout di stati
-define(STATE_DISCOVER_TIMEOUT, 5000).
-define(STATE_ELECTION_TIMEOUT, 5000).


%% Messaggio richiesta movimento veicolo

-export([start_link/1]).
-export([init/1, callback_mode/0, terminate/3]).
-export([inqueue/3, discover/3, election/3]).
-export([print/2]).

%% tupla contenente tutte le info che l'auto si porta dietro tra i vari stati
-record(cardata, {
    env,
    name,
    desc,
    route,
    neighbourPids,
    speed
    }).


print(CarData, What) ->
  Car = atom_to_list(CarData#cardata.name),
  io:format("CAR " ++ Car ++ ": " ++ "~p~n", [What]).
  % io:format("PID " ++ MyPid ++ ": " ++ What ++ "~n").

%% state_functions: Events are handled by one callback function per state. 
%% %% Una funzione di gestione specifica per ogni stato
%% in alternativa posso definire un unica funzione di callback generale con:
%% callback_mode() ->
  % handle_event_function.
callback_mode() ->
  state_functions.


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
  print(CarData, "init:: Car Data is " ++ lists:flatten(io_lib:format("~p", [CarData]))),
  print(CarData, "init:: Starting state is inqueue"),
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

%% Se ricevo un timeout generico di tipo ?CAR_MV significa che l'auto vorrebbe muoversi, dunque devo interrogare sensore prossimita'
inqueue(timeout, ?CAR_MV, CarData) ->
  State = "inqueue:: ",
  print(CarData, State ++ "Received event car_mv. I Will query proximity sensor."),
  print(CarData, State ++ "Remaining to State <<inqueue>>"),

  {NextNode,_} = next_pos(CarData),
  %% chiedo al sensore se la posizione davanti e' libera
  %% la risposta la riceverò sotto forma di evento POSFREE_RESP
  get_env(CarData) ! { posfree, get_name(CarData), {NextNode} },
  %% Rimango nello stesso stato, imposto sempre timeout per il movimento
  {next_state, inqueue, CarData, [{timeout, CarData#cardata.speed, ?CAR_MV}]};

%% Gestisco tutti gli altri messaggi che arrivano quando sono nello stato "inqueue"
%% Message corrisponde al tipo di evento ricevuto (ho usato macro)
%% MessageData e' l'eventuale payload del messaggio
%% CarData e' il record contenente le informazioni globali della macchina
inqueue(info, {Message, MessageData}, CarData) ->
  print(CarData, Message),
  State = "inqueue:: ",
  case Message of
    %% Ho ricevuto una response dal sensore di prossimita'
    %% Se il sensore conferma che davanti e' libero e non sono in un top_node posso muovermi
    ?POSFREE_RESP ->
      print(CarData, State ++ "Received Event posfree_resp"),
      print(CarData, MessageData),
      case MessageData of
        %% Se la prossima posizione e' libera
        false -> 
          print(CarData, current_pos(CarData)),
          {CurrentPos,CurrentPosType} = current_pos(CarData),
          {NexPos,_} = next_pos(CarData),
          case CurrentPosType of
            %% Se il nodo e' di tipo tail
            tail_node ->   
              %% Mi posso muovere, quindi mando messaggio all'env, tolgo il primo elemento dalla route e vado in queue
              get_env(CarData) ! {move, CarData#cardata.name, {CurrentPos, NexPos}},
              NewCarData = pop_position(CarData),
              {next_state, inqueue, NewCarData, [{timeout, CarData#cardata.speed, ?CAR_MV}]};
            %% Se il nodo e' top
            top_node ->
              print(CarData, State ++ "I'm on a top node going in discover"),
              % posso mandare disc
              CarData#cardata.env ! {disc, CarData#cardata.name},
              {next_state, discover, CarData, [{state_timeout, ?STATE_DISCOVER_TIMEOUT, ?DISC_TM}]}
          end;
        %% Se la prossima posizione e' occupata non faccio nulla
        true -> 
          print(CarData, "Next position occupied"),
          {next_state, inqueue, CarData, [{timeout, CarData#cardata.speed, ?CAR_MV}]}
      end;
    ?ONTOP -> 
      print(CarData, State ++ "Received Event ONTOP"),
      print(CarData, State ++ "Passing to state <<discover>>"),
      CarData#cardata.env ! {disc, CarData#cardata.name},
      {next_state, discover, CarData, [{state_timeout, ?STATE_DISCOVER_TIMEOUT, ?DISC_TM}]};
    ?ONTAIL ->
      print(CarData, State ++ "Received Event ONTAIL"),
      print(CarData, State ++ "Remaining in state <<inqueue>>"),
      {next_state, inqueue, CarData, [{timeout, CarData#cardata.speed, ?CAR_MV}]};
    ?STOP ->
      print(CarData, State ++ "Received Event STOP"),
      print(CarData, State ++ "Remaining in state <<inqueue>>"),
      {next_state, inqueue, CarData};
    ?DISC ->
      print(CarData, State ++ "Received Event DISC, but I am not in discovery. Ignoring this message"),
      print(CarData, State ++ "Remaining in state <<inqueue>>"),
      {next_state, inqueue, CarData, [{timeout, CarData#cardata.speed, ?CAR_MV}]};
    _Other ->
      print(CarData, State ++ "Received UNKNOWN EVENT. Ignoring this message"),
      print(CarData, State ++ "Remaining in state <<inqueue>>"),
      {next_state, inqueue, CarData, [{timeout, CarData#cardata.speed, ?CAR_MV}]}
  end.

%% Gestisco timeout stato discover passando in election


discover(state_timeout, ?DISC_TM, CarData) ->
  State = "discover:: ",
  print(CarData, State ++ "Received event disc_tm"),
  print(CarData, State ++ "Passing to State <<election>>"),
  print(CarData, State ++ "Current neighbours"),
  print(CarData, get_neighbours(CarData)),
  {next_state, election, CarData, [{state_timeout, ?STATE_ELECTION_TIMEOUT, ?ELECT_TM}]};

discover(info, {Message, MessageData}, CarData) ->
  State = "discover:: ",
  print(CarData, Message),
  case Message of
    ?DISC -> 
      print(CarData,  State ++ "Received Event DISC"),
      print(CarData,  State ++ "Remaining in state <<discover>> and adding car to my neighbours"),
      FromProc = MessageData,
      % salvo id macchina da cui ho ricevuto disc
      NewCarData = update_neighbours(CarData, FromProc),
      % NewCarData = #cardata{name = CarData#cardata.name, desc = CarData#cardata.desc, route = CarData#cardata.route, neighbourPids = [ToProc | CarData#cardata.neighbourPids]},
      % Invio hello al mittente, segnalo che ci sono anche io.
      FromProc ! {hello, get_name(NewCarData)},
      {next_state, discover, NewCarData};
    ?HELLO ->
      print(CarData,  State ++ "Received Event HELLO"),
      print(CarData,  State ++ "Remaining in state <<discover>>"),
      FromProc = MessageData,
      NewCarData = update_neighbours(CarData, FromProc),
      % NewCarData = #cardata{name = CarData#cardata.name, desc = CarData#cardata.desc, route = CarData#cardata.route, neighbourPids = [FromProc | CarData#cardata.neighbourPids]},
      {next_state, discover, NewCarData};
    ?WAIT ->
      print(CarData,  State ++ "Received Event WAIT"),
      print(CarData,  State ++ "Remaining in state <<discover>> and resetting timeout"),
      {next_state, discover, CarData, [{state_timeout, ?STATE_DISCOVER_TIMEOUT, ?DISC_TM}]};
    ?BULLY_ELECT ->
      print(CarData, State ++ "Receive Event BULLY_ELECT"),
      print(CarData, State ++ "Passing to State <<election>>"),
      print(CarData, State ++ "Current neighbours"),
      print(CarData, get_neighbours(CarData)),
      {next_state, election, CarData, [{state_timeout, ?STATE_ELECTION_TIMEOUT, ?ELECT_TM}]};
    _Other ->
      print(CarData, State ++ "Received UNKNOWN EVENT"),
      print(CarData, State ++ "Remaining in state <<inqueue>>"),
      {next_state, discover, CarData}
  end.


election(state_timeout, ?ELECT_TM, CarData) ->
  State = "election:: ",
  print(CarData, State ++ "Received event elect_tm"),
  print(CarData, State ++ "Passing to State <<discover>>"),
  %% Elezione fallita, torno in discover
  CarData#cardata.env ! {disc, CarData#cardata.name},
  NewCarData = reset_neighbours(CarData),
  {next_state, discover, NewCarData, [{state_timeout, ?STATE_DISCOVER_TIMEOUT, ?DISC_TM}]};


%% TODO: un po tutto quanto... da implementare i messaggi di wait
election(info, {Message, MessageData}, CarData) ->
  print(CarData, Message),
  State = "election:: ",
  case Message of
    ?DISC -> 
      print(CarData,  State ++ "Received Event DISC"),
      print(CarData,  State ++ "Remaining in state <<election>>"),
      % salvo id macchina da cui ho ricevuto disc
      {next_state, election, CarData, [{state_timeout, ?STATE_ELECTION_TIMEOUT, ?ELECT_TM}]};
    ?HELLO ->
      print(CarData,  State ++ "Received Event ONTAIL"),
      print(CarData,  State ++ "Remaining in state <<election>>"),
      {next_state, election, CarData, [{state_timeout, ?STATE_ELECTION_TIMEOUT, ?ELECT_TM}]};
    ?WAIT ->
      print(CarData,  State ++ "Received Event WAIT"),
      print(CarData,  State ++ "Remaining in state <<election>> and resetting timeout"),
      {next_state, election, CarData, [{state_timeout, ?STATE_ELECTION_TIMEOUT, ?ELECT_TM}]};

    _Other ->
      print(CarData,  State ++ "Received UNKNOWN EVENT"),
      print(CarData,  State ++ "Remaining in state <<inqueue>>"),
      {next_state, election, CarData, [{state_timeout, ?STATE_ELECTION_TIMEOUT, ?ELECT_TM}]}
  end.



terminate(_Reason, _State, _Data) ->
    ok.
