-module(dim_env).
-export([go/0,t/0]).

%% Export Public API genserver
-export([spawn_car/6, req_prox_sensor_data/2, broadcast_disc/1, notify_move/3, delete_car/2, request_towtruck/1]).

%% gen_event export stuff
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).
-include_lib("stdlib/include/assert.hrl").

-record(world, {
  dir,
  undir,
  gui_mbox = mbox,
  gui_node = 'jv@Altro-MB.local'
}).

%% portata antenna wireless a bordo di ogni auto
-define(PROX_RANGE, 7).
-define(GENSERVER_CALL_TIMEOUT, 10000).

-behaviour(gen_server).

%% Simple print to console
%% print( What) ->
%%   MyPid = pid_to_list(self()) ++ ":",
%%   io:format("ENV " ++ MyPid ++ ": " ++ "~p~n", [What]).

print(Format, Args) ->
  Usr = io_lib:format(Format, Args),
  io:format("ENV: " ++ Usr ++ "~n").

create_world() ->
  #world{
     dir= create_graph(directed),
     undir= create_graph(undirected)
    }.


create_graph(GType) ->
  G = graph:empty(GType, f),
  % NORD
  % Nord IN
  INord1 = graph:add_vertex(G, i_nord1, {top_node, []}),
  INord2 = graph:add_vertex(G, i_nord2, {tail_node, []}),
  INord3 = graph:add_vertex(G, i_nord3, {tail_node, []}),
  graph:add_edge(G, INord3, INord2, 1),
  graph:add_edge(G, INord2, INord1, 1),

  % Nord OUT
  ONord1 = graph:add_vertex(G, o_nord1, {out_node, []}),
  ONord2 = graph:add_vertex(G, o_nord2, {tail_node, []}),
  ONord3 = graph:add_vertex(G, o_nord3, {tail_node, []}),
  graph:add_edge(G, ONord2, ONord3, 1),
  graph:add_edge(G, ONord1, ONord2, 1),

  % EST
  % Est IN
  IEst1 = graph:add_vertex(G, i_est1, {top_node, []}),
  IEst2 = graph:add_vertex(G, i_est2, {tail_node, []}),
  IEst3 = graph:add_vertex(G, i_est3, {tail_node, []}),
  graph:add_edge(G, IEst3, IEst2, 1),
  graph:add_edge(G, IEst2, IEst1, 1),

  % Est OUT
  OEst1 = graph:add_vertex(G, o_est1, {out_node, []}),
  OEst2 = graph:add_vertex(G, o_est2, {tail_node, []}),
  OEst3 = graph:add_vertex(G, o_est3, {tail_node, []}),
  graph:add_edge(G, OEst2, OEst3, 1),
  graph:add_edge(G, OEst1, OEst2, 1),

  % SUD
  % Sud IN
  ISud1 = graph:add_vertex(G, i_sud1, {top_node, []}),
  ISud2 = graph:add_vertex(G, i_sud2, {tail_node, []}),
  ISud3 = graph:add_vertex(G, i_sud3, {tail_node, []}),
  graph:add_edge(G, ISud3, ISud2, 1),
  graph:add_edge(G, ISud2, ISud1, 1),

  % Sud OUT
  OSud1 = graph:add_vertex(G, o_sud1, {out_node, []}),
  OSud2 = graph:add_vertex(G, o_sud2, {tail_node, []}),
  OSud3 = graph:add_vertex(G, o_sud3, {tail_node, []}),
  graph:add_edge(G, OSud2, OSud3, 1),
  graph:add_edge(G, OSud1, OSud2, 1),


  % OVEST
  % Ovest IN
  IOvest1 = graph:add_vertex(G, i_ovest1, {top_node, []}),
  IOvest2 = graph:add_vertex(G, i_ovest2, {tail_node, []}),
  IOvest3 = graph:add_vertex(G, i_ovest3, {tail_node, []}),
  graph:add_edge(G, IOvest3, IOvest2, 1),
  graph:add_edge(G, IOvest2, IOvest1, 1),

  % Ovest OUT
  OOvest1 = graph:add_vertex(G, o_ovest1, {out_node, []}),
  OOvest2 = graph:add_vertex(G, o_ovest2, {tail_node, []}),
  OOvest3 = graph:add_vertex(G, o_ovest3, {tail_node, []}),
  graph:add_edge(G, OOvest2, OOvest3, 1),
  graph:add_edge(G, OOvest1, OOvest2, 1),


  %% CENTRO
  CNordOvest = graph:add_vertex(G, c_nordovest, {cross_node, []}),
  CNordEst = graph:add_vertex(G, c_nordest, {cross_node, []}),
  CSudOvest = graph:add_vertex(G, c_sudovest, {cross_node, []}),
  CSudEst = graph:add_vertex(G, c_sudest, {cross_node, []}),
  CCentro = graph:add_vertex(G, c_center, {cross_node, []}),

  %% Collegamenti Nord <-> Centro
  graph:add_edge(G, INord1, CNordOvest, 1),
  graph:add_edge(G, CNordEst, ONord1, 1),

  %% Collegamenti Est <-> Centro
  graph:add_edge(G, IEst1, CNordEst, 1),
  graph:add_edge(G, CSudEst, OEst1, 1),

  %% Collegamenti Sud <-> Centro
  graph:add_edge(G, ISud1, CSudEst, 1),
  graph:add_edge(G, CSudOvest, OSud1, 1),

  %% Collegamenti Ovest <-> Centro
  graph:add_edge(G, IOvest1, CSudOvest, 1),
  graph:add_edge(G, CNordOvest, OOvest1, 1),




  %% Collegamenti Centrali

  % graph:add_edge(G, CNordOvest, CSudOvest, 2),
  graph:add_edge(G, CNordOvest, CCentro, 1.414),

  % graph:add_edge(G, CSudOvest, CSudEst, 2),
  graph:add_edge(G, CSudOvest, CCentro, 1.414),

  % graph:add_edge(G, CSudEst, CNordEst, 2),
  graph:add_edge(G, CSudEst, CCentro, 1.414),

  % graph:add_edge(G, CNordEst, CNordOvest, 2),
  graph:add_edge(G, CNordEst, CCentro, 1.414),

  graph:add_edge(G, CCentro, CNordOvest,1.414),
  graph:add_edge(G, CCentro, CNordEst,1.414),
  graph:add_edge(G, CCentro, CSudOvest,1.414),
  graph:add_edge(G, CCentro, CSudEst,1.414),

  CNord = graph:add_vertex(G, c_nord, {cross_node, []}),
  CEst = graph:add_vertex(G, c_est, {cross_node, []}),
  CSud = graph:add_vertex(G, c_sud, {cross_node, []}),
  COvest = graph:add_vertex(G, c_ovest, {cross_node, []}),

  graph:add_edge(G, CNordOvest, COvest, 1),
  graph:add_edge(G, COvest, CSudOvest, 1),
  graph:add_edge(G, CSudEst, CEst, 1),
  graph:add_edge(G, CEst, CNordEst, 1),
  graph:add_edge(G, CNordEst, CNord, 1),
  graph:add_edge(G, CNord, CNordOvest, 1),
  graph:add_edge(G, CSudOvest, CSud, 1),
  graph:add_edge(G, CSud, CSudEst, 1),


  G.

get_min_path(G, V1, V2) ->
  ShortestPaths = dijkstra:run(G,V1),
  get_shortest_path(ShortestPaths, V2).


get_shortest_path(ShortestsPaths, V2) ->
  {V2, Res} = lists:keyfind(V2, 1, ShortestsPaths),
  case Res of
    {_, Path} -> Path;
    unreachable -> Res
  end.

get_neighboorhood(G, V, Range) ->
  ShortestPaths = dijkstra:run(G, V),
  lists:filtermap(
    fun({Node, Res}) ->
        case Res of
          {Dist, _} when Dist > 0, Dist =< Range  -> {true, Node};
          _ -> false
        end
    end, ShortestPaths
  ).

%% Restituisco lista di auto vicine a Car
get_car_neighboorhood(G, Car, Range) ->
  V = get_car_vertex(G, Car),
  Neighbour = get_neighboorhood(G, V, Range),
  Neighbour_vertices = get_vertex_cars_array(G, Neighbour),
  %% Cancello me stesso dalla lista dei vicini
  Res = lists:delete(Car, aux_get_car_neighboorhood(G, Neighbour_vertices)),
  Res.

aux_get_car_neighboorhood(_, []) ->
  [];
aux_get_car_neighboorhood(G, [H | Neighbour]) ->
  {_, Cars} = H,
  lists:append(Cars, aux_get_car_neighboorhood(G, Neighbour)).

is_node_occupied(G, V) ->
  case graph:get_vertex_label(G, V) of
    {_, {_, [_ | _]}} -> true;
    {_, {_, []}} -> false;
    false -> false
  end.

%% Aggiungo macchina Car in nodo V del grafo
add_car_to_vertex(G, V, Car) ->
  {_, {Type, Cars}} = graph:get_vertex_label(G, V),
  graph:add_vertex(G, V, {Type, [Car | Cars]}).

%% Cancello una macchina Car da nodo V del grafo
delete_car_from_vertex(G, V, Car) ->
  {_, {Type, Cars}} = graph:get_vertex_label(G, V),
  %% ?assert(lists:member(Car, Cars), io:format("Trying to remove ~p from ~p~n", [Car, Cars])),
  NewCars = lists:delete(Car, Cars),
  %% tmele: devo modificare il vertice esistente
  graph:add_vertex(G, V, {Type, NewCars} ).

%% Muovo una macchina Car da vertice From a vertice To
move_from_to(G, Car, From, To) ->
  delete_car_from_vertex(G, From, Car),
  add_car_to_vertex(G, To, Car),
  G.

%% Data una macchina trovo il vertice nel grafo dove si trova
get_car_vertex(G, Car) ->
  AllVertex = graph:vertices(G),
  aux_get_car_vertex(G, AllVertex, Car).

aux_get_car_vertex(_, [], _) ->
  false;
aux_get_car_vertex(G, [H|T], Car) ->
  {_, {_, Cars}} = graph:get_vertex_label(G, H),
  case lists:member(Car, Cars) of
    false -> aux_get_car_vertex(G, T, Car);
    true -> H
  end.

%% Scorro lista di auto e mando disc a tutti
%% Simulo broadcast dall'auto
broadcast_discover(_, []) ->
  ok;
broadcast_discover(FromCar, [H | T]) ->
  % H ! {disc, FromCar},
  vehicle:send_disc(H, FromCar),
  broadcast_discover(FromCar, T).

%% Da lista vertici restituisco array con tupla del tipo {nodo, tipo}
get_vertex_type_array(_, []) ->
  [];
get_vertex_type_array(G, [H|T]) ->
  {Node, {Type, _}} = graph:get_vertex_label(G, H),
  [{Node, Type} | get_vertex_type_array(G, T)].

%% Da lista vertici restituisco array con tupla del tipo {nodo, [macchine]}
get_vertex_cars_array(_, []) ->
  [];
get_vertex_cars_array(G, [H|T]) ->
  {Node, {_, Cars}} = graph:get_vertex_label(G, H),
  [{Node, Cars} | get_vertex_cars_array(G, T)].

%% Funzione per creare una nuova auto:
%% Name: atomo che corrispondera' a id registrato, e che viene usato per inviare messaggi all'auto
%% Desc: descrizione (non utilizzata da nessuna parte)
%% VStart: posizione iniziale auto
%% VStop: posizione finale
%% Speed: velocita' movimento (es: con un valore 2000 l'auto tenta di muoversi ogni 2 secondi)
add_car_to_graph(W, Name, Desc, VStart, Vstop, Speed, Prio) ->
  Path = get_min_path(W#world.dir, VStart, Vstop),
  {Res, _Ot} = vehicle:start_link({?MODULE, Name, Desc, get_vertex_type_array(W#world.dir, Path), Speed, Prio}),
  case Res of
    ok -> add_car_to_vertex(W#world.undir, VStart, Name);
    _Other -> ko
  end.


%% Send to java gui
jgui_update_graph(Node, Mbox, G) ->
  {Mbox, Node} ! {undirgraph, {get_vertex_cars_array(G, graph:vertices(G))}}.

%% gen_event stuff
init(_Args) ->
  process_flag(trap_exit, true),
  World = create_world(),
  {ok,World}.

delete_car(CarName, Pos) -> 
  gen_server:call(?MODULE, {delete_car, {CarName, Pos}}, ?GENSERVER_CALL_TIMEOUT).

spawn_car(CarName, CarDesc, StartPos, EndPos, Speed, Prio) ->
  try
    gen_server:call(?MODULE, {spawn_car, {CarName, CarDesc, StartPos, EndPos, Speed, Prio}}, ?GENSERVER_CALL_TIMEOUT)
  catch
    exit:{timeout,_} -> {error, timeout};
    _:_ -> {error, timeout}
  end.
broadcast_disc(FromCar) ->
  gen_server:cast(?MODULE, {disc, FromCar}).

request_towtruck(Node) ->
  gen_server:cast(?MODULE, {check_fault, Node}).

%% Request proximity sensor data
req_prox_sensor_data(FromCar, Position) ->
  try
    gen_server:call(?MODULE, {posfree, {FromCar, Position}}, ?GENSERVER_CALL_TIMEOUT)
  catch
    exit:{timeout,_} -> false;
    _:_ -> false
  end.

notify_move(CarName, CurrentPos, NextPos) ->
  gen_server:call(?MODULE, {move, {CarName, {CurrentPos, NextPos}}}, ?GENSERVER_CALL_TIMEOUT).



handle_cast({disc, Msg={FromCar, _Route}},W) ->
  Cars = get_car_neighboorhood(W#world.undir, FromCar, ?PROX_RANGE),
  broadcast_discover(Msg, Cars),
  {noreply, W};
handle_cast({check_fault, Node}, W = #world{undir = G}) ->
  {Node, {Type, Cars}} = graph:get_vertex_label(G, Node),
  NewCars = lists:filter(fun (Car) ->
                             not vehicle:is_stalled(Car)
                         end,
                         Cars),
  print("~p", [NewCars]),
  graph:add_vertex(G, Node, {Type, NewCars}),
  {noreply, W}.



handle_call({posfree, {_FromCar, Position}}, _From, W) ->
  % vehicle:send_posfree_resp(FromCar, is_node_occupied(W#world.undir, Position)),
  {reply, not is_node_occupied(W#world.undir, Position), W};

handle_call({move, {CarName, {CurrentPos, NextPos}}}, _From, W) ->
  move_from_to(W#world.undir, CarName, CurrentPos, NextPos),
  %% Test eredis lib
  % {ok, C} = eredis:start_link(),
  % eredis:q(C, ["PUBLISH", "graph", lists:flatten(io_lib:format("~p", get_vertex_cars_array(W#world.undir, graph:vertices(W#world.undir)) ))]),
  % eredis_client:stop(C),
  print("Received move", []),
  %% Stampo posizione auto nel grafo
  print("~p", [get_vertex_cars_array(W#world.undir, graph:vertices(W#world.undir))]),
  jgui_update_graph(W#world.gui_node, W#world.gui_mbox, W#world.undir),
  {reply, ok, W};

handle_call({delete_car, {CarName, Pos}}, _From, W) ->
  print("DELETE REQUEST",[]),
  delete_car_from_vertex(W#world.undir, Pos, CarName),
  jgui_update_graph(W#world.gui_node, W#world.gui_mbox, W#world.undir),
  {reply, ok, W};

handle_call({spawn_car, {CarName, CarDesc, StartPos, EndPos, Speed, Prio}}, _From, W) ->
  print("SPAWN REQUEST",[]),
  Occupied = is_node_occupied(W#world.undir, StartPos),
  case Occupied of
    false ->
      add_car_to_graph(W, CarName, CarDesc, StartPos, EndPos, Speed, Prio),
      print("~p", [get_vertex_cars_array(W#world.undir, graph:vertices(W#world.undir))]),
      jgui_update_graph(W#world.gui_node, W#world.gui_mbox, W#world.undir),
      {reply, CarName, W};
    true ->
      {reply, "Already occupied", W}
    end.


handle_info(D1, W) ->
  print("INFO D1: ~p W: ~p", [D1, W]),
  {noreply, W}.


terminate(_Args, _Fd) ->
  ok.

go() ->
  World = create_world(),
  gen_server:start_link({local, ?MODULE}, dim_env, World, []).

t() ->
  World = create_world(),
  gen_server:start_link({local, ?MODULE}, dim_env, World, []),
  
  dim_env:spawn_car(car1, "LanciaDelta", i_nord2, o_sud3, 1000, 1),
  timer:sleep(1500),

  dim_env:spawn_car(car2, "LanciaDelta", i_est3, o_sud3, 1000, 0),
  timer:sleep(1500),

  dim_env:spawn_car(car3, "LanciaDelta", i_sud3, o_nord3, 1000, 2),
  timer:sleep(1500),

  dim_env:spawn_car(car4, "LanciaDelta", i_ovest3, o_sud3, 1000, 0),
  timer:sleep(1500),

  ok.

  % gen_event:add_handler(dim_env, dim_env, []).
