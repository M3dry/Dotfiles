import System.Exit

import qualified Data.Map.Strict as M

import XMonad

import XMonad.Prompt

import qualified XMonad.StackSet as W

import XMonad.Actions.SwapPromote
import XMonad.Actions.UpdatePointer
import XMonad.Actions.WithAll
import XMonad.Actions.WorkspaceNames
import XMonad.Actions.GridSelect
import XMonad.Actions.UpdateFocus
import XMonad.Actions.CycleWS
import qualified XMonad.Actions.FlexibleResize as Flex
import XMonad.Actions.Minimize
import XMonad.Actions.TopicSpace

import XMonad.Hooks.CurrentWorkspaceOnTop
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.FloatNext
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.Place
import XMonad.Hooks.RefocusLast
import XMonad.Hooks.TaffybarPagerHints (pagerHints)
import XMonad.Hooks.WorkspaceHistory (workspaceHistoryHook)
import XMonad.Hooks.InsertPosition
import XMonad.Hooks.Minimize

import XMonad.Layout.CenteredMaster
import XMonad.Layout.Grid
import qualified XMonad.Layout.Magnifier as Magni
import XMonad.Layout.NoBorders
import XMonad.Layout.Reflect
import qualified XMonad.Layout.Renamed as Renamed
import XMonad.Layout.ResizableTile
import XMonad.Layout.Simplest
import XMonad.Layout.SubLayouts
import XMonad.Layout.Tabbed
import XMonad.Layout.WindowNavigation
import XMonad.Layout.ToggleLayouts
import XMonad.Layout.Minimize
import XMonad.Layout.BoringWindows

import XMonad.Util.EZConfig
import XMonad.Util.SpawnOnce
import XMonad.Util.NamedScratchpad

myTerminal = "st "
myTerminalPath path = myTerminal ++ "-d " ++ path

myEmacs = "emacsclient -c "
myEmacsDir dir = myEmacs ++ "-e \"(dired \\\"" ++ dir ++ "\\\")\""

myModMask = mod4Mask

myTopicItems =
    [ TI       "1" "my-stuff/Projects/Rust/list" spawnShell
    , noAction "2" "."
    , noAction "3" "."
    , noAction "4" "."
    , TI       "5" ".config/flake"               spawnShell
    , TI       "6" "my-stuff/Org"                (currentTopicDir myTopicConfig >>= \dir -> spawn $ myEmacsDir dir)
    , inHome   "7"                               (spawn "firefox")
    , inHome   "8"                               (spawn "chromium")
    , inHome   "9"                               (spawn "spotify")
    ]

myTopicConfig = def
    { topicDirs          = tiDirs myTopicItems
    , topicActions       = tiActions myTopicItems
    , defaultTopicAction = const (pure ())
    , defaultTopic       = "7"
    }

spawnShell = currentTopicDir myTopicConfig >>= \dir -> spawn $ myTerminalPath dir
goto = switchTopic myTopicConfig
toggleTopic = switchNthLastFocusedByScreen myTopicConfig 1

myFont size = "xft:ComicCodeLigatures Nerd Font:pixelsize=" ++ show size

myPrompt =
    def
        { bgColor = "#0f111b"
        , fgColor = "#eeffff"
        , borderColor = "#c792ea"
        , promptBorderWidth = 2
        , font = myFont 16
        , height = 44
        , position = Bottom
        }

myGridNav = makeXEventhandler $ shadowWithKeymap navKeyMap navDefaultHandler
  where
    navKeyMap =
      M.fromList
        [ ((0, xK_Escape), cancel)
        , ((0,xK_Return) , select)
        , ((0, xK_slash) , substringSearch myGridNav)
        , ((0, xK_h)     , move (-1,0)  >> myGridNav)
        , ((0, xK_l)     , move (1,0)   >> myGridNav)
        , ((0, xK_j)     , move (0,1)   >> myGridNav)
        , ((0, xK_k)     , move (0,-1)  >> myGridNav)
        , ((0, xK_space) , setPos (0,0) >> myGridNav)
        ]
    navDefaultHandler = const myGridNav

spawnSelected' :: [(String, String)] -> X ()
spawnSelected' lst = gridselect conf lst >>= flip whenJust spawn
    where conf = def
                   { gs_cellheight   = 40
                   , gs_cellwidth    = 180
                   , gs_cellpadding  = 6
                   , gs_originFractX = 0.5
                   , gs_originFractY = 0.5
                   , gs_navigate     = myGridNav
                   , gs_font         = myFont 12
                   }

gridSystem =
  [ ("St", "st")
  , ("Alacritty", "alacritty")
  , ("Htop", myTerminal ++ "-e htop")
  , ("Neovim", myTerminal ++ "-e neovim")
  , ("Emacs", myEmacs)
  , ("Firefox", "firefox")
  , ("Chromium", "chromium")
  ]

gridConfig launcher =
  [ ("Doom", launcher "~/.config/flake/dots/doom/")
  , ("Neovim", launcher "~/.config/flake/dots/nvim/")
  , ("Xmonad", launcher "~/.config/flake/dots/xmonad/")
  , ("Taffybar", launcher "~/.config/flake/dots/taffybar/")
  , ("eww", launcher "~/.config/flake/dots/eww/")
  , ("scripts", launcher "~/.config/flake/dots/bin/")
  , ("dots", launcher "~/.config/flake/dots/")
  , ("nixos", launcher "~/.config/flake/")
  ]

myTabConfig =
    def
        { activeColor = "#ff5370"
        , activeBorderWidth = 0
        , inactiveColor = "#292d3e"
        , inactiveBorderWidth = 0
        }

myLayout = boringWindows $ toggleLayouts (noBorders Full) (tile ||| ltile ||| centeredmaster ||| grid ||| bstack ||| tstack ||| tabs)
  where
    centeredmaster = Renamed.renamed [Renamed.Replace "cmaster"] $ minimize $ smartBorders $ centerMaster Grid
    grid = Renamed.renamed [Renamed.Replace "grid"] $ minimize $ smartBorders $ configurableNavigation noNavigateBorders $ addTabs shrinkText myTabConfig $ subLayout [] Simplest $ magni Grid
    tabs = Renamed.renamed [Renamed.Replace "tabs"] $ minimize $ smartBorders $ tabbed shrinkText myTabConfig
    tstack = Renamed.renamed [Renamed.Replace "tstack"] $ reflectVert bstack
    bstack = Renamed.renamed [Renamed.Replace "bstack"] $ Mirror tile
    ltile = Renamed.renamed [Renamed.Replace "ltile"] $ reflectHoriz tile
    tile = Renamed.renamed [Renamed.Replace "tile"] $ minimize $ smartBorders $ magni $ configurableNavigation noNavigateBorders $ addTabs shrinkText myTabConfig $ subLayout [] Simplest $ ResizableTall 1 (3 / 100) (1 / 2) []
    magni = Magni.magnifierczOff 1.3

myScratchpads =
  [ NS "terminal" (term "-t scratchpad" "") (findT "scratchpad") centerFloat
  , NS "cmus" (term "-c cmuspad" "cmus") (findC "cmuspad") nonFloating
  , NS "qalc" (term "-t qalcpad" "qalc") (findT "qalcpad") centerFloat
  ]
  where
    term x y = myTerminal ++ x ++ " -e " ++ y
    findT x = title =? x
    findC x = className =? x
    centerFloat =
      customFloating $ W.RationalRect l t w h
        where
          h = 0.7
          w = 0.7
          t = (1 - h) / 2
          l = (1 - h) / 2


myKeys c =
    mkKeymap c $
        [ ("M-C-q", io exitSuccess)
        , ("M-S-q", spawn "if type xmonad; then xmonad --recompile && xmonad --restart; else xmessage xmonad not in \\$PATH: \"$PATH\"; fi")
        , ("M-c", kill)
        , ("M-q", killOthers)
        , ("M1-C-<KP_Down>", spawn "xkill")
        -- APPS
        , ("M-<Return>", spawnShell)
        , ("M-e e", spawn myEmacs)
        , ("M-e v", spawn $ myEmacs ++ "-e '(+vterm/here nil)'")
        , ("M-S-<Return>", spawn "dmenu_run_history -F -l 5 -g 10 -p 'Run'")
        , ("M-d v", spawn "audio prompt")
        , ("M-d c", spawn "calc")
        , ("M-d p", spawn "passmenu2 -F -p 'Passwords:'")
        , ("M-d q", spawn "shut")
        , ("M1-C-s", spawn "spotify")
        , ("M-u", spawn "SNIPPATH=\"${HOME}/my-stuff/Pictures/snips/$(date +'%F-%T').png\"; import \"${SNIPPATH}\" && xclip -selection clipboard -t image/png \"${SNIPPATH}\"")
        , ("<XF86AudioLowerVolume>", spawn "pamixer -d 1; audio update")
        , ("<XF86AudioRaiseVolume>", spawn "pamixer -i 1; audio update")
        -- SCRATCHPADS
        , ("M-i <Return>", spawnHereNamedScratchpadAction myScratchpads "terminal")
        , ("M-i c", spawnHereNamedScratchpadAction myScratchpads "cmus")
        , ("M-i q", spawnHereNamedScratchpadAction myScratchpads "qalc")
        , ("M-i d", dynamicNSPAction "dyn")
        , ("M-i t", withFocused $ toggleDynamicNSP "dyn")
        -- GRIDSELECT
        , ("M-g s", spawnSelected' gridSystem)
        , ("M-g e", spawnSelected' $ gridConfig myEmacsDir)
        , ("M-g t", spawnSelected' $ gridConfig myTerminalPath)
        -- LAYOUTS
        , ("M-t t", sendMessage $ JumpToLayout "tile")
        , ("M-t l", sendMessage $ JumpToLayout "ltile")
        , ("M-t b", sendMessage $ JumpToLayout "bstack")
        , ("M-t s", sendMessage $ JumpToLayout "tstack")
        , ("M-t a", sendMessage $ JumpToLayout "tabs")
        , ("M-t g", sendMessage $ JumpToLayout "grid")
        , ("M-t c", sendMessage $ JumpToLayout "cmaster")
        -- LAYOUT MANIPULATION
        , ("M-h", sendMessage Shrink)
        , ("M-l", sendMessage Expand)
        , ("M-S-h", sendMessage MirrorShrink)
        , ("M-S-l", sendMessage MirrorExpand)
        , ("M-S-j", siftDown)
        , ("M-S-k", siftUp)
        , ("M-;", swapPromote' False)
        , ("M-z", sendMessage Magni.Toggle) -- magnify
        , ("M-f", do
                    sendMessage ToggleLayout
                    sendMessage ToggleStruts)
        , ("M-m", withFocused minimizeWindow)
        , ("M-S-m", withLastMinimized maximizeWindowAndFocus)
        -- MOVING AROUND WINDOWS
        , ("M-j", focusDown)
        , ("M-k", focusUp)
        , ("M-<Space>", focusMaster)
        -- WORKSPACE MANIPULATION
        , ("M-r", renameWorkspace myPrompt)
        , ("M-n", sendMessage ToggleStruts)
        , ("M-b [", sendMessage (IncMasterN 1))
        , ("M-b ]", sendMessage (IncMasterN (-1)))
        -- FLOATS
        , ("M-s", withFocused $ windows . W.sink)
        , ("M-S-s", sinkAll)
        , ("M-S-f", toggleFloatNext)
        -- TABS
        , ("M-C-h", sendMessage $ pullGroup L)
        , ("M-C-l", sendMessage $ pullGroup R)
        , ("M-C-k", sendMessage $ pullGroup U)
        , ("M-C-j", sendMessage $ pullGroup D)
        , ("M-C-m", withFocused (sendMessage . MergeAll))
        , ("M-C-u", withFocused (sendMessage . UnMerge))
        , ("M-S-C-u", withFocused (sendMessage . UnMergeAll))
        , ("M-C-n", onGroup W.focusUp')
        , ("M-C-p", onGroup W.focusDown')
        -- MONITORS
        , ("M-.", nextScreen)
        , ("M-,", prevScreen)
        , ("M-S-.", shiftNextScreen)
        , ("M-S-,", shiftPrevScreen)
        , ("M-C-.", shiftNextScreen >> nextScreen)
        , ("M-C-,", shiftPrevScreen >> prevScreen)
        , ("M-C-S-.", swapNextScreen)
        , ("M-C-S-,", swapPrevScreen)
        -- TOPICS
        , ("M-a", currentTopicAction myTopicConfig)
        , ("M1-<Tab>", toggleTopic)
        -- LAYOUT MOVING
        , ("M-]", moveTo Next emptyWS)
        , ("M-[", moveTo Prev emptyWS)
        , ("M-S-]", shiftTo Next emptyWS)
        , ("M-S-[", shiftTo Prev emptyWS)
        ]
        -- ++ [ ("M-" ++ k, toggleOrView i)
        --    | (i, k) <- zip myWorkspaces (map show [1 ..])
        --    ]
        -- ++ [ ("M-S-" ++ k, windows =<< shiftRLWhen refocusingIsActive i)
        --    | (i, k) <- zip myWorkspaces (map show [1 ..])
        --    ]
        -- ++ [ ("M-C-" ++ k, swapWithCurrent i)
        --    | (i, k) <- zip myWorkspaces (map show [1 ..])
        --    ]
        ++ [ ("M-" ++ k, toggleOrView i)
           | (i, k) <- zip (topicNames myTopicItems) (map show [1 ..])
           ]
        ++ [ ("M-S-" ++ k, windows =<< shiftRLWhen refocusingIsActive i)
           | (i, k) <- zip (topicNames myTopicItems) (map show [1 ..])
           ]
        ++ [ ("M-C-" ++ k, swapWithCurrent i)
           | (i, k) <- zip (topicNames myTopicItems) (map show [1 ..])
           ]

myMouseBinds (XConfig {XMonad.modMask = modMask}) = M.fromList
    [ ((modMask, button1), \w -> focus w >> mouseMoveWindow w)
    -- , ((modMask, button2), \w -> windows . W.sink w)
    , ((modMask, button3), \w -> focus w >> Flex.mouseResizeWindow w)
    ]

myLogHook = workspaceHistoryHook <> currentWorkspaceOnTop <> refocusLastLogHook <> updatePointer (0.5, 0.5) (0, 0)

myHandleEventHook = refocusLastWhen (refocusingIsActive <||> isFloat) <> focusOnMouseMove <> minimizeEventHook <> handleEventHook def

myManageHook = namedScratchpadManageHook myScratchpads <> placeHook (inBounds (underMouse (0.5, 0.5))) <> floatNextHook <> insertPosition Below Newer <> composeAll []

main = do
    xmonad $
        docks $
            ewmhFullscreen $
                workspaceNamesEwmh $
                    ewmh $
                        pagerHints $
                            def
                                { terminal = myTerminal
                                , modMask = myModMask
                                , normalBorderColor = "#0f111b"
                                , focusedBorderColor = "#c792ea"
                                , workspaces = topicNames myTopicItems
                                , borderWidth = 2
                                , layoutHook = avoidStruts myLayout
                                , handleEventHook = myHandleEventHook
                                , startupHook = adjustEventInput
                                , logHook = myLogHook
                                , manageHook = myManageHook
                                , keys = myKeys
                                , mouseBindings = myMouseBinds
                                , focusFollowsMouse = True
                                }
