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

-record(state,
        {
         win,
         cars,
         node_position,
         canvas
        }).

-record(car,
        {
         name,
         position,
         state
        }).

start() ->
  wx_object:start_link({local, ?MODULE}, ?MODULE, [], []).

init(_Options) ->
  wx:new(),
  Cars = #{car1 => #car{name=car1, position=i_est1}},
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
  DrawingPanel = wxPanel:new(TopSplitter, [{style, ?wxFULL_REPAINT_ON_RESIZE}]),
  DrawingSizer = wxStaticBoxSizer:new(?wxVERTICAL,
                                      DrawingPanel,
                                      [{label, "Simulation"}]),
  wxPanel:setSizer(DrawingPanel, DrawingSizer),
  Text2 = wxTextCtrl:new(DrawingPanel,
                         ?wxID_ANY,
                         [{style, ?wxTE_READONLY bor ?wxTE_CENTRE},
                          {value, "Info delle macchine"}]),
  wxSizer:add(DrawingSizer, Text2, [{proportion, 1}, {flag, ?wxEXPAND}]),
  wxPanel:connect(DrawingPanel, paint, [callback]),
  wxSplitterWindow:splitVertically(TopSplitter, InfoPanel, DrawingPanel,
                                   [{sashPosition, 0}]),
  wxFrame:show(Frame),
  wxWindow:refresh(DrawingPanel),
  wxSplitterWindow:setSashGravity(TopSplitter,   1.0),
  {Frame, #state{win=Frame, canvas=DrawingPanel, cars=Cars}}.


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
                  _State = #state{canvas=Canvas, cars=Cars}) ->
  {W, H} = wxPanel:getSize(Canvas),
  GraphBitmap = wxBitmap:new(W, H),
  DC = wxMemoryDC:new(GraphBitmap),
  CDC = wxWindowDC:new(Canvas),
  draw(CDC, initNodePositions()),
  drawCars(CDC, initNodePositions(), Cars),
  %% wxDC:blit(CDC, {0,0},
  %%           {wxBitmap:getWidth(GraphBitmap), wxBitmap:getHeight(GraphBitmap)},
  %%           DC, {0,0}),
  wxWindowDC:destroy(CDC),
  wxMemoryDC:destroy(DC),
  wxBitmap:destroy(GraphBitmap),
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
                             fun(C) -> C#car{position=Position} end,
                             Cars),
  {noreply, State#state{cars=NewCars}};

handle_cast({?NEW_CAR, Car}, State = #state{cars=Cars}) ->
  Name = proplist:get_value(name, Car),
  Pos = proplist:get_value(position, Car),
  NewCars = maps:put(Name, #car{name=Name, position=Pos}, Cars),
  {noreply, State#state{cars=NewCars}}.

%%%%%%%%%%%%%%%
%% Notify API
update_car_position(Car, Position) ->
  wx_object:cast(?MODULE, {?POS_UPDATE, Car, Position}).

track_new_car(Car) ->
  wx_object:cast(?MODULE, {?NEW_CAR, Car}).



%%%%%%%%%%%%%%%%
%% Local Functions
initNodePositions() ->
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


draw(DC, Nodes = #{c_nordest := Pt1, c_sudovest := Pt2, c_nordovest := Pt3}) ->
  {W, H} = wxDC:getSize(DC),
  Normalize = fun({X, Y}) ->
                  {trunc(X/10*W), trunc(Y/10*H)}
              end,
  Center = fun({X, Y}) -> {X - 0.5, Y -0.5} end,
  wxDC:setBrush(DC, ?wxWHITE_BRUSH),
  wx:foreach(fun ({_Node, {XC, YC}}) ->
                 Point = Normalize({XC-0.5, YC-0.5}),
                 Size = Normalize({1, 1}),
                 wxDC:drawRectangle(DC, Point, Size)
             end,
             maps:to_list(Nodes)),
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

drawCars(DC, Nodes, Cars) ->
  {W, H} = wxDC:getSize(DC),
  wx:foreach(fun ({Name, #car{position=Pos}}) ->
                 Text = atom_to_list(Name),
                 {TW, TH} = wxDC:getTextExtent(DC, Text),
                 Normalize = fun({X, Y}) ->
                                 {trunc(X/10*W), trunc(Y/10*H)}
                             end,
                 {CX, CY} = Normalize(maps:get(Pos, Nodes)),
                 PX = CX - (TW div 2),
                 PY = CY - (TH div 2),
                 {BW, BH} = Normalize({ 1,1 }),
                 wxDC:drawLabel(DC, Text,
                                {PX, PY, BW, BH })
             end,
             maps:to_list(Cars)).
