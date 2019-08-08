-module(gui).

-behaviout(wx_object).

-include_lib("wx/include/wx.hrl").

%% Client API
-export([start/0]).
%% wx_object behaviour
-export([init/1, terminate/2, handle_event/2, handle_cast/2, handle_sync_event/3]).
%% Notify API
-export([update_car_position/2, track_new_car/1]).


-define(POS_UPDATE, update_car_pos).
-define(NEW_CAR, track_car).

%% Drawing constants
-define(CAR_HEIGHT, 0.4).
-define(CAR_LENGTH, 0.6).
-define(GRID_HEIGHT, 11).
-define(GRID_WIDTH, 11).
-define(PI_QUARTERS, 0.7853981633974483).

-record(state,
        {
         win,
         canvas,
         cars,
         nodes_position,
         bg_info,
         bg_bitmap
        }).

-record(car,
        {
         name,
         route,
         state
        }).

start() ->
  wx_object:start_link({local, ?MODULE}, ?MODULE, [], []).

init(_Options) ->
  wx:new(),
  NodesPosition = init_nodes_position(),
  Route = annotate_route_with_direction([i_est1,c_nordest, c_center, c_sudovest, o_sud1], NodesPosition),
  Cars = #{car1 => #car{name=car1, route=Route},
           car2 => #car{name=car2, route=tl(Route)},
           car3 => #car{name=car3, route=tl(tl(Route))},
           car4 => #car{name=car4, route=tl(tl(tl(Route)))},
           car5 => #car{name=car5, route=tl(tl(tl(tl(Route))))}
          },
  process_flag(trap_exit, true),
  Frame = wxFrame:new(wx:null(),
                      ?wxID_ANY,
                      "Distribute Intersection Management",
                      [{size,{1000,500}}]),

  MB = wxMenuBar:new(),
  File = wxMenu:new(),
  wxMenu:append(File, ?wxID_EXIT, "&Quit"),

  wxMenuBar:append(MB, File, "&Erlang"),

  wxFrame:setMenuBar(Frame,MB),

  wxFrame:connect(Frame, close_window),
  wxFrame:connect(Frame, command_menu_selected),

  TopSplitter = wxSplitterWindow:new(Frame,[]),% [{style, ?wxSP_BORDER}]),

  wxSplitterWindow:setSashGravity(TopSplitter,   0.5),
  InfoPanel = wxPanel:new(TopSplitter),
  InfoPanelSizer = wxStaticBoxSizer:new(?wxVERTICAL,
                                        InfoPanel,
                                        [{label, "Cars"}]),
  wxPanel:setSizer(InfoPanel, InfoPanelSizer),
  Text1 = wxTextCtrl:new(InfoPanel,
                         ?wxID_ANY,
                         [{style, ?wxTE_READONLY bor ?wxTE_CENTRE},
                          {value, "Info delle macchine"}]),
  wxSizer:add(InfoPanelSizer, Text1, [{proportion, 1}, {flag, ?wxEXPAND}]),
  %% wxSizer:add(InfoPanelSizer, Text2, [{proportion, 1}, {flag, ?wxEXPAND}]),
  SimulationPanel = wxPanel:new(TopSplitter, []),% {style, ?wxFULL_REPAINT_ON_RESIZE}]),
  SimulationSizer = wxBoxSizer:new(?wxVERTICAL),
  DrawingSizer = wxStaticBoxSizer:new(?wxVERTICAL, SimulationPanel,
                               [{label, "Simulation"}]),
  %% SimulationSizer = wxStaticBoxSizer:new(?wxVERTICAL,
  %%                                     DrawingPanel,
  %%                                     [{label, "Simulation"}]),
  DrawingPanel = wxPanel:new(SimulationPanel,
                             [{style, ?wxFULL_REPAINT_ON_RESIZE}]),
  wxSizer:add(DrawingSizer, DrawingPanel,
              [{flag, ?wxEXPAND}, {proportion, 1}]),
  wxSizer:add(SimulationSizer, DrawingSizer,
              [{flag, ?wxEXPAND}, {proportion, 1}]),
  wxPanel:setSizer(SimulationPanel, SimulationSizer),
  wxSizer:layout(SimulationSizer),
  %% Text2 = wxTextCtrl:new(DrawingPanel,
  %%                        ?wxID_ANY,
  %%                        [{style, ?wxTE_READONLY bor ?wxTE_CENTRE},
  %%                         {value, "Info delle macchine"}]),
  %% wxSizer:add(DrawingSizer, Text2, [{proportion, 1}, {flag, ?wxEXPAND}]),
  wxPanel:connect(DrawingPanel, paint, [callback]),
  wxPanel:connect(DrawingPanel, erase_background, [callback]),
  wxSplitterWindow:splitVertically(TopSplitter, InfoPanel, SimulationPanel,
                                   [{sashPosition, 0}]),

  {W, H} = wxPanel:getSize(DrawingPanel),
  BgBitmap = wxBitmap:new(W, H),
  %% MDC = wxMemoryDC:new(BgBitmap),
  BackgroundInfo = init_background_info(NodesPosition),
  %% CDC = wxClientDC:new(DrawingPanel),
  %% GCDC = wxGCDC:new(MDC),
  %% drawBackground(GCDC, BackgroundInfo, {W, H}),
  %% wxMemoryDC:destroy(MDC),

  wxFrame:show(Frame),
  wxWindow:refresh(DrawingPanel),
  wxSplitterWindow:setSashGravity(TopSplitter,   1.0),
  {Frame, #state{
             win=Frame,
             canvas=DrawingPanel,
             cars=Cars,
             nodes_position=NodesPosition,
             bg_info=BackgroundInfo,
             bg_bitmap=BgBitmap}}.


terminate(_Reason, #state{win=Frame}) ->
  io:format("Calling terminate for reason: ~p~n", [_Reason]),
  wxFrame:destroy(Frame),
  wx:destroy(),
  io:format("Bye!~n").


%%%%%%%%%%%%
%% Callbacks


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Sync event from callback events, paint event must be handled in callbacks
%% otherwise nothing will be drawn on windows.
handle_sync_event(#wx{event=#wxPaint{}}, _wxObj,
                  _State = #state{canvas=Canvas, cars=Cars,
                                  nodes_position=NodesPos,
                                  bg_info=BgInfo}) ->
  %% {W, H} = wxPanel:getSize(Canvas),
  %% GraphBitmap = wxBitmap:new(W, H),
  %% DC = wxMemoryDC:new(GraphBitmap),
  PaintDC = wxPaintDC:new(Canvas),
  drawBackground(PaintDC, BgInfo),
  drawCars(PaintDC, NodesPos, Cars),
  %% draw(PaintDC, NodesPos),
  %% wxDC:blit(CDC, {0,0},
  %%           {wxBitmap:getWidth(GraphBitmap), wxBitmap:getHeight(GraphBitmap)},
  %%           DC, {0,0}),
  wxWindowDC:destroy(PaintDC),
  %% wxMemoryDC:destroy(DC),
  %% wxBitmap:destroy(GraphBitmap),
  ok;

handle_sync_event(#wx{event=#wxErase{}}, _wxObj,
                  _State = #state{}) ->
  ok.

%% Async Events
handle_event(#wx{event=#wxClose{}}, State = #state{}) ->
  io:format("Closing window~n", []),
  {stop, normal, State};

handle_event(#wx{event=#wxCommand{type=command_menu_selected}}, State = #state{}) ->
  io:format("Closing window~n", []),
  {stop, normal, State}.


%% Notify Events
handle_cast({?POS_UPDATE, Car, Position}, State = #state{cars=Cars}) ->
  NewCars = maps:update_with(Car,
                             fun(C) ->
                                 [_, Position | Route] = C#car.route,
                                 C#car{route=[Position|Route]}
                             end,
                             Cars),
  {noreply, State#state{cars=NewCars}};

handle_cast({?NEW_CAR, Car}, State = #state{cars=Cars}) ->
  Name = proplist:get_value(name, Car),
  Route = proplist:get_value(route, Car),
  NewCars = maps:put(Name, #car{name=Name, route=Route}, Cars),
  {noreply, State#state{cars=NewCars}}.

%%%%%%%%%%%%%%%
%% Notify API
update_car_position(Car, Position) ->
  wx_object:cast(?MODULE, {?POS_UPDATE, Car, Position}).

track_new_car(Car) ->
  wx_object:cast(?MODULE, {?NEW_CAR, Car}).



%%%%%%%%%%%%%%%%
%% Local Functions
init_nodes_position() ->
  %% Nodes are placed in a 10x10 grid with the intersection center at (5,5).
  #{
    i_nord1 => {4, 3},
    i_nord2 => {4, 2},
    i_nord3 => {4, 1},
    o_nord1 => {6, 3},
    o_nord2 => {6, 2},
    o_nord3 => {6, 1},
    i_est1 => {7, 4},
    i_est2 => {8, 4},
    i_est3 => {9, 4},
    o_est1 => {7, 6},
    o_est2 => {8, 6},
    o_est3 => {9, 6},
    i_sud1 => {6, 7},
    i_sud2 => {6, 8},
    i_sud3 => {6, 9},
    o_sud1 => {4, 7},
    o_sud2 => {4, 8},
    o_sud3 => {4, 9},
    i_ovest1 => {3, 6},
    i_ovest2 => {2, 6},
    i_ovest3 => {1, 6},
    o_ovest1 => {3 , 4},
    o_ovest2 => {2 , 4},
    o_ovest3 => {1 , 4},
    c_nordovest => {4, 4},
    c_nordest => {6, 4},
    c_sudovest => {4, 6},
    c_sudest => {6, 6},
    c_center => {5, 5}
   }.

annotate_route_with_direction(Route, Positions) ->
  RouteWithNext = zip(Route, tl(Route)),
  {RouteDir, {LastNode, LastDir}} =
    lists:mapfoldl(
      fun ({CurrentNode, NextNode}, {_,PrevNodeDirection}) ->
          %% Current Position
          {CX, CY} = maps:get(CurrentNode, Positions),
          %% Next Position
                        {NX, NY} = maps:get(NextNode, Positions),
          %% Vector Direction
          DX = NX - CX,
          DY = NY - CY,
          Direction = math:atan2(DY,DX)+math:pi()/2,
          PrevDir = if
                      PrevNodeDirection == undefined -> Direction;
                      true -> PrevNodeDirection
                    end,
          DiffAngle = Direction - PrevDir,
          NormalizedAngle = math:acos(abs(math:cos(DiffAngle))),
          ReducedDiffAngle = if
                               NormalizedAngle >= ?PI_QUARTERS -> DiffAngle/2;
                               NormalizedAngle < ?PI_QUARTERS -> 0.0
                             end,
          {{CurrentNode, Direction-ReducedDiffAngle}, {NextNode, Direction}}
      end,
      {undefined, undefined},
      RouteWithNext),
  RouteDir ++ [{LastNode, LastDir}].

init_background_info(RoadPositions) ->
  GridColumns = lists:seq(0,?GRID_WIDTH-1),
  GridRows = lists:seq(0,?GRID_HEIGHT-1),
  GridCells = [{X,Y} || X <- GridColumns, Y <- GridRows],
  RoadCells = maps:values(RoadPositions),
  GridCellsInfo =
    lists:map(
      fun (Pos) ->
          IsRoad = lists:member(Pos, RoadCells),
          if
            IsRoad -> {Pos, road};
            not IsRoad -> {Pos, grass}
          end
      end,
      GridCells),
  GridCellsInfo.

draw(DC, Nodes = #{i_est1 := Pt1, o_sud1 := Pt2, c_nordovest := Pt3}) ->
  {W, H} = wxDC:getSize(DC),
  Normalize = fun({X, Y}) ->
                  {trunc((X+0.5)/?GRID_WIDTH*W), trunc((Y+0.5)/?GRID_HEIGHT*H)}
              end,
  %% Center = fun({X, Y}) -> {X - 0.5, Y -0.5} end,
  %% wxDC:setBrush(DC, ?wxWHITE_BRUSH),
  %% wx:foreach(fun ({_Node, {XC, YC}}) ->
  %%                Point = Normalize({XC-0.5, YC-0.5}),
  %%                Size = Normalize({1, 1}),
  %%                wxDC:drawRectangle(DC, Point, Size)
  %%            end,
  %%            maps:to_list(Nodes)),
  Overlay = wxOverlay:new(),
  ODC = wxDCOverlay:new(Overlay, DC),
  wxDC:setBrush(DC, ?wxRED_BRUSH),
  GC = wxGraphicsContext:create(DC),
  wxGraphicsContext:setPen(GC, ?wxRED_PEN),
  Path = wxGraphicsContext:createPath(GC),
  wxGraphicsPath:moveToPoint(Path, Normalize(Pt1)),
  {X1, _Y1} = Normalize(Pt1),
  {X2, Y2} = Normalize(Pt2),
  {X3, Y3} = Normalize(Pt3),
  %% wxGraphicsPath:addLineToPoint(Path, )
  wxGraphicsPath:addArcToPoint(Path, X3, Y3, X2, Y2, abs(X1-X2)),
  wxGraphicsContext:strokePath(GC, Path),
  %% wxDC:drawArc(DC,
  %%              Normalize(Pt1),
  %%              Normalize(Pt2),
  %%              Normalize(C)),
  wxGraphicsObject:destroy(Path),
  wxGraphicsObject:destroy(GC),
  wxDCOverlay:destroy(ODC),
  wxOverlay:destroy(Overlay).

drawCars(DC, NodePositions, Cars) ->
  {W, H} = wxDC:getSize(DC),
  GC = wxGraphicsContext:create(DC),
  wx:foreach(fun ({_Name, Car = #car{route=[_Pos | _]}}) ->
                 drawCar(GC, Car, NodePositions, {W,H})
                 %% Text = atom_to_list(_Name),
                 %% {TW, TH} = wxDC:getTextExtent(DC, Text),
                 %% Normalize = fun({X, Y}) ->
                 %%                 {trunc(X/10*W), trunc(Y/10*H)}
                 %%             end,
                 %% {CX, CY} = Normalize(maps:get(_Pos, NodePositions)),
                 %% PX = CX - (TW div 2),
                 %% PY = CY - (TH div 2),
                 %% {BW, BH} = Normalize({ 1,1 }),
                 %% wxDC:drawLabel(DC, Text,
                 %%                {PX, PY, BW, BH })
             end,
             maps:to_list(Cars)),
  wxGraphicsObject:destroy(GC).

drawCar(GC,
        _Car = #car{name=_Name, route=[{CurrentNode, CarDirection} | _]},
        NodePositions,
        {WW, WH}) ->
  {X,Y} = maps:get(CurrentNode, NodePositions),
  %% CarDirection = case maps:get(CurrentNode, NodeDirections) of
  %%                  undefined ->
  %%                    NextNode = hd(Route),
  %%                    %% Current Position
  %%                    {CX, CY} = maps:get(CurrentNode, NodePositions),
  %%                    %% Next Position
  %%                    {NX, NY} = maps:get(NextNode, NodePositions),
  %%                    %% Vector Direction
  %%                    DX = NX - CX,
  %%                    DY = NY - CY,
  %%                    Direction = math:atan2(DY,DX)+math:pi()/2;
  %%                    %% Direction/2;
  %%                  N -> (N+90)*math:pi()/180
  %%                end,
  Path = wxGraphicsContext:createPath(GC),
  wxGraphicsPath:moveToPoint(Path, -?CAR_HEIGHT/2, -?CAR_LENGTH/2),
  %% wxGraphicsPath:addCircle(Path, -?CAR_HEIGHT/2, -?CAR_LENGTH/2, 0.1),
  wxGraphicsPath:addLineToPoint(Path, ?CAR_HEIGHT/2, -?CAR_LENGTH/2),
  wxGraphicsPath:addLineToPoint(Path, ?CAR_HEIGHT/2, ?CAR_LENGTH/2),
  wxGraphicsPath:addLineToPoint(Path, -?CAR_HEIGHT/2, ?CAR_LENGTH/2),
  wxGraphicsPath:closeSubpath(Path),
  XScale = WW / ?GRID_WIDTH,
  YScale = WH / ?GRID_HEIGHT,
  Matrix = wxGraphicsContext:createMatrix(GC),
  wxGraphicsMatrix:scale(Matrix, XScale, YScale),
  wxGraphicsMatrix:translate(Matrix, X+0.5, Y+0.5),
  wxGraphicsMatrix:rotate(Matrix, CarDirection),
  wxGraphicsPath:transform(Path, Matrix),
  wxGraphicsContext:setBrush(GC, ?wxBLUE_BRUSH),
  wxGraphicsContext:fillPath(GC, Path),
  wxGraphicsObject:destroy(Matrix),
  wxGraphicsObject:destroy(Path).

drawBackground(DC, InfoGrid) ->
  {WW, WH} = wxDC:getSize(DC),
  GC = wxGraphicsContext:create(DC),
  wx:foreach(
    fun ({{X,Y}, Type}) ->
        XScale = WW /?GRID_WIDTH,
        YScale = WH /?GRID_HEIGHT,
        Normalize = fun({XLocal, YLocal}) ->
                        {XLocal*XScale, YLocal*YScale}
                    end,
        {RX, RY} = Normalize({X, Y}),
        {RW, RH} = Normalize({1,1}),
        case Type of
          grass ->
            wxGraphicsContext:setBrush(GC, ?wxGREEN_BRUSH),
            wxGraphicsContext:setPen(GC, ?wxGREEN_PEN);
            %% wxDC:setBrush(MemoryDC, ?wxGREEN_BRUSH),
            %% wxDC:setPen(MemoryDC, ?wxGREEN_PEN);
          road ->
            wxGraphicsContext:setBrush(GC, ?wxGREY_BRUSH),
            wxGraphicsContext:setPen(GC, ?wxGREY_PEN)
            %% wxDC:setBrush(MemoryDC, ?wxGREY_BRUSH),
            %% wxDC:setPen(MemoryDC, ?wxGREY_PEN)
        end,
        wxGraphicsContext:drawRectangle(GC,RX, RY, RW, RH)
        %% wxDC:drawRectangle(MemoryDC, RX, RY, RW, RH)
    end,
    InfoGrid),
  wxGraphicsObject:destroy(GC).

%% Zip with Haskell semantics
zip(L1, L2) -> zip_acc(L1, L2, []).
zip_acc(_, [], Acc) -> lists:reverse(Acc);
zip_acc([], _, Acc) -> lists:reverse(Acc);
zip_acc([H1 | T1], [H2 | T2], Acc) ->
  zip_acc(T1, T2, [{H1, H2} | Acc]).
