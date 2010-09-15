%% -*- erlang-indent-level: 2 -*-
%%% Created :  6 Nov 2008 by Mats Cronqvist <masse@kreditor.se>

%% @doc
%% @end

-module('hi').
-author('Mats Cronqvist').

-export([start/0]).

-include_lib("wx/include/wx.hrl").

start() -> start("hi.xrc").

start(XRC) ->
  WX = wx:new(),
  Xrc = wxXmlResource:get(),
  true = wxXmlResource:load(Xrc, rc_dir(XRC)),
  Frame = wxFrame:new(),
  myframe(WX,Frame),
  wxFrame:show(Frame),
  loop(Frame),
  wx:destroy().

rc_dir(File) ->
  SelfDir = filename:dirname(code:which(?MODULE)),
  filename:join([SelfDir,File]).

loop(Frame) ->
  receive 
    #wx{id=Id, event=#wxCommand{}} ->
      handle_command(Id, Frame),
      loop(Frame);
    #wx{event=#wxClose{}} ->
      catch wxWindows:'Destroy'(Frame),
      ok;
    Ev = #wx{} ->
      io:format("Got ~p ~n", [Ev]),
      loop(Frame)
  end.

handle_command(Id,Frame) ->
  io:fwrite("id:~w; frame:~w~n~n",[Id,Frame]).

myframe(Parent, Frame) ->
  Xrc = wxXmlResource:get(),
  wxXmlResource:loadFrame(Xrc, Frame, Parent, "main_frame"),
  %% wxTopLevelWindow:setIcon(Frame, wxXmlResource:loadIcon(Xrc,"appicon")),
  %% Load and setup menubar
  wxFrame:setMenuBar(Frame, wxXmlResource:loadMenuBar(Xrc, "main_menu")),
  %% hmm wxSystemOptions::SetOption ( wxT("msw.remap"), 0 );
  wxFrame:setToolBar(Frame, wxXmlResource:loadToolBar(Xrc, Frame, "main_toolbar")),
  wxFrame:createStatusBar(Frame, [{number,1}]),
  ok = wxFrame:connect(Frame, close_window), 
  connect(Frame).

connect(Frame) ->    
  Menues = [unload_resource_menuitem, reload_resource_menuitem,
	    non_derived_dialog_tool_or_menuitem, derived_tool_or_menuitem,
	    controls_tool_or_menuitem, uncentered_tool_or_menuitem,
	    custom_class_tool_or_menuitem, platform_property_tool_or_menuitem,
	    art_provider_tool_or_menuitem, variable_expansion_tool_or_menuitem
	   ],
  wxFrame:connect(Frame,command_menu_selected, [{id, ?wxID_EXIT}]),
  wxFrame:connect(Frame,command_menu_selected, [{id, ?wxID_ABOUT}]),
  [connect_xrcid(Str,Frame) || Str <- Menues],
  ok.

connect_xrcid(Name,Frame) ->
  ID = wxXmlResource:getXRCID(atom_to_list(Name)),
  wxFrame:connect(Frame,command_menu_selected,[{id,ID}]).
