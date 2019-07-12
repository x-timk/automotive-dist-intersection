-module(dim_env).
-export([main/0]).

-record(world, {
  dir,
  undir
}).

setup() ->
  code:add_path("../lib/erlang-algorithms/").

%% Simple print to console
print(What) ->
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
    {_, Path} -> {result, Path};
    unreachable -> {result, Res}
  end.

main() ->
  setup(),
  print(ciao),
  W = create_world(),
  get_min_path(W#world.dir,i_nord1, o_nord3).
