-module(dim_env).
-export([main/0, tester/0]).
-include_lib("stdlib/include/assert.hrl").

-record(world, {
  dir,
  undir
}).

%% portata antenna wireless a bordo di ogni auto
-define(PROX_RANGE, 7).

setup() ->
  print( "Setup").
  % code:add_path("../lib/erlang-algorithms/"),
  % register(?MODULE, self()).

%% Simple print to console
print( What) ->
  MyPid = pid_to_list(self()) ++ ":",
  io:format("ENV " ++ MyPid ++ ": " ++ "~p~n", [What]).

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
  graph:add_edge(G, INord3, INord2, 2),
  graph:add_edge(G, INord2, INord1, 2),

  % Nord OUT
  ONord1 = graph:add_vertex(G, o_nord1, {top_node, []}),
  ONord2 = graph:add_vertex(G, o_nord2, {tail_node, []}),
  ONord3 = graph:add_vertex(G, o_nord3, {tail_node, []}),
  graph:add_edge(G, ONord2, ONord3,2),
  graph:add_edge(G, ONord1, ONord2,2),

  % EST
  % Est IN
  IEst1 = graph:add_vertex(G, i_est1, {top_node, []}),
  IEst2 = graph:add_vertex(G, i_est2, {tail_node, []}),
  IEst3 = graph:add_vertex(G, i_est3, {tail_node, []}),
  graph:add_edge(G, IEst3, IEst2, 2),
  graph:add_edge(G, IEst2, IEst1, 2),

  % Est OUT
  OEst1 = graph:add_vertex(G, o_est1, {top_node, []}),
  OEst2 = graph:add_vertex(G, o_est2, {tail_node, []}),
  OEst3 = graph:add_vertex(G, o_est3, {tail_node, []}),
  graph:add_edge(G, OEst2, OEst3,2),
  graph:add_edge(G, OEst1, OEst2,2),

  % SUD
  % Sud IN
  ISud1 = graph:add_vertex(G, i_sud1, {top_node, []}),
  ISud2 = graph:add_vertex(G, i_sud2, {tail_node, []}),
  ISud3 = graph:add_vertex(G, i_sud3, {tail_node, []}),
  graph:add_edge(G, ISud3, ISud2, 2),
  graph:add_edge(G, ISud2, ISud1, 2),

  % Sud OUT
  OSud1 = graph:add_vertex(G, o_sud1, {top_node, []}),
  OSud2 = graph:add_vertex(G, o_sud2, {tail_node, []}),
  OSud3 = graph:add_vertex(G, o_sud3, {tail_node, []}),
  graph:add_edge(G, OSud2, OSud3,2),
  graph:add_edge(G, OSud1, OSud2,2),


  % OVEST
  % Ovest IN
  IOvest1 = graph:add_vertex(G, i_ovest1, {top_node, []}),
  IOvest2 = graph:add_vertex(G, i_ovest2, {tail_node, []}),
  IOvest3 = graph:add_vertex(G, i_ovest3, {tail_node, []}),
  graph:add_edge(G, IOvest3, IOvest2, 2),
  graph:add_edge(G, IOvest2, IOvest1, 2),

  % Ovest OUT
  OOvest1 = graph:add_vertex(G, o_ovest1, {top_node, []}),
  OOvest2 = graph:add_vertex(G, o_ovest2, {tail_node, []}),
  OOvest3 = graph:add_vertex(G, o_ovest3, {tail_node, []}),
  graph:add_edge(G, OOvest2, OOvest3,2),
  graph:add_edge(G, OOvest1, OOvest2,2),


  %% CENTRO
  CNordOvest = graph:add_vertex(G, c_nordovest, {cross_node, []}),
  CNordEst = graph:add_vertex(G, c_nordest, {cross_node, []}),
  CSudOvest = graph:add_vertex(G, c_sudovest, {cross_node, []}),
  CSudEst = graph:add_vertex(G, c_sudest, {cross_node, []}),
  CCentro = graph:add_vertex(G, c_center, {cross_node, []}),

  %% Collegamenti Nord <-> Centro
  graph:add_edge(G, INord1, CNordOvest, 2),
  graph:add_edge(G, CNordEst, ONord1, 2),

  %% Collegamenti Est <-> Centro
  graph:add_edge(G, IEst1, CNordEst, 2),
  graph:add_edge(G, CSudEst, OEst1, 2),

  %% Collegamenti Sud <-> Centro
  graph:add_edge(G, ISud1, CSudEst, 2),
  graph:add_edge(G, CSudOvest, OSud1, 2),

  %% Collegamenti Ovest <-> Centro
  graph:add_edge(G, IOvest1, CSudOvest, 2),
  graph:add_edge(G, CNordOvest, OOvest1, 2),


  %% Collegamenti Centrali

  graph:add_edge(G, CNordOvest, CSudOvest, 2),
  graph:add_edge(G, CNordOvest, CCentro, 1.414),

  graph:add_edge(G, CSudOvest, CSudEst, 2),
  graph:add_edge(G, CSudOvest, CCentro, 1.414),

  graph:add_edge(G, CSudEst, CNordEst, 2),
  graph:add_edge(G, CSudEst, CCentro, 1.414),

  graph:add_edge(G, CNordEst, CNordOvest, 2),
  graph:add_edge(G, CNordEst, CCentro, 1.414),

  graph:add_edge(G, CCentro, CNordOvest,1.414),
  graph:add_edge(G, CCentro, CNordEst,1.414),
  graph:add_edge(G, CCentro, CSudOvest,1.414),
  graph:add_edge(G, CCentro, CSudEst,1.414),

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
  % print( "Car Neighbour are:::"),
  % print( Neighbour_vertices),
  % print( Car),
  % print( Res),
  Res.

aux_get_car_neighboorhood(_, []) ->
  [];
aux_get_car_neighboorhood(G, [H | Neighbour]) ->
  {_, Cars} = H,
  % print( Cars),
  lists:append(Cars, aux_get_car_neighboorhood(G, Neighbour)).

is_node_occupied(G, V) ->
  case graph:get_vertex_label(G, V) of
    {_, {_, [_ | _]}} -> true;
    {_, {_, []}} -> false
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
  H ! {disc, FromCar},
  broadcast_discover(FromCar, T).

%% Loop principale environment
loop(W) ->
  receive
    %% se ricevo messaggio disc simulo broadcast rigirandolo a tutte le auto a portata
    {disc,FromCar} ->
      % print( "Received disc!!"),
      Cars = get_car_neighboorhood(W#world.undir, FromCar, ?PROX_RANGE),
      % print( "Neighbours are: " ++ Cars),
      broadcast_discover(FromCar, Cars),
      print(get_vertex_cars_array(W#world.undir, graph:vertices(W#world.undir)));
    %% se ricevo una richiesta da sensore di prossimita' controllo ed invio come risposta all'auto
    %% un messaggio postfree_resp
    %% false se il nodo e' libero
    %% true se il nodo e' occupato
    {posfree, FromCar,{V}} ->
      % print( is_node_occupied(W#world.undir, V)),
      FromCar ! {posfree_resp, is_node_occupied(W#world.undir, V)},
      print(get_vertex_cars_array(W#world.undir, graph:vertices(W#world.undir)));
    %% la macchina FromCar si muove e quindi notifica environment che aggiorna grafo
    {move, FromCar, {From, To}} ->
      move_from_to(W#world.undir, FromCar, From, To),
      %% Stampo posizione auto nel grafo
      print(get_vertex_cars_array(W#world.undir, graph:vertices(W#world.undir)));
    %% Ogni altro messaggio lo ignoro
    _Other ->
      print( _Other),
      print( get_vertex_cars_array(W#world.undir, graph:vertices(W#world.undir)))
  end,
  %% Rimango in loop all'infinito
  loop(W).

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
spawn_car(W, Name, Desc, VStart, Vstop, Speed) ->
  Path = get_min_path(W#world.dir, VStart, Vstop),
  add_car_to_vertex(W#world.undir, VStart, Name),
  vehicle:start_link({?MODULE, Name, Desc, get_vertex_type_array(W#world.dir, Path), Speed}).

main() ->
  setup(),
  print( ciao),
  W = create_world(),
  get_min_path(W#world.dir,i_nord1, o_nord3).
  % add_car_to_vertex(W#world.undir, i_nord1, ferrari),
  % delete_car_from_vertex(W#world.undir, i_nord1, ferrari),
  % print( is_node_occupied(W#world.undir, i_nord1)),
  % get_neighboorhood(W#world.undir, i_nord1, 7.0),
  % get_car_vertex(W#world.undir, ferrari),
  % loop(W).

%% Funzione per testare il funzionamento del sistema
tester() ->
  setup(),
  W = create_world(),
  spawn_car(W, car1, "LanciaDelta", i_nord3, o_est3, 3000),
  timer:sleep(1000),
  spawn_car(W, car2, "Renault5", i_sud3, o_est3, 3000),
  timer:sleep(1000),
  spawn_car(W, car3, "Ferrari", i_est3, o_sud3, 3000),
  timer:sleep(1000),
  spawn_car(W, car4, "Panda", i_ovest3, o_nord3, 3000),
  timer:sleep(10000),
  spawn_car(W, car5, "Clio", i_ovest3, o_sud3, 3000),


  % spawn_car(W#world.undir, car2, "FiatPanda", i_est3, o_ovest3),
  loop(W). 

