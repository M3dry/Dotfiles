import qualified Data.Map.Strict as M
import System.Exit

import XMonad

import XMonad.Prompt

import qualified XMonad.StackSet as W

import XMonad.Actions.SwapPromote
import XMonad.Actions.UpdatePointer
import XMonad.Actions.WithAll
import XMonad.Actions.WorkspaceNames
import XMonad.Actions.GridSelect

import XMonad.Hooks.CurrentWorkspaceOnTop
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.FloatNext
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.Place
import XMonad.Hooks.RefocusLast
import XMonad.Hooks.TaffybarPagerHints (pagerHints)

import XMonad.Layout.CenteredMaster
import XMonad.Layout.Grid
import XMonad.Layout.Magnifier
import XMonad.Layout.NoBorders
import XMonad.Layout.Reflect
import XMonad.Layout.Renamed
import XMonad.Layout.ResizableTile
import XMonad.Layout.Simplest
import XMonad.Layout.SubLayouts
import XMonad.Layout.Tabbed
import XMonad.Layout.WindowNavigation

import XMonad.Util.EZConfig
import XMonad.Util.SpawnOnce
import XMonad.Util.NamedScratchpad

myTerminal = "st "
myTerminalPath path = myTerminal ++ "-d " ++ path

myEmacs = "emacsclient -c "

myModMask = mod4Mask

myWorkspaces = ["1", "2", "3", "4", "5", "6", "7", "8", "9"]

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

myLayout = tile ||| ltile ||| centeredmaster ||| grid ||| bstack ||| tstack ||| tabs
  where
    centeredmaster = renamed [Replace "cmaster"] $ smartBorders $ centerMaster Grid
    grid = renamed [Replace "grid"] $ smartBorders $ configurableNavigation noNavigateBorders $ addTabs shrinkText myTabConfig $ subLayout [] Simplest $ magni Grid
    tabs = renamed [Replace "tabs"] $ smartBorders $ tabbed shrinkText myTabConfig
    tstack = renamed [Replace "tstack"] $ reflectVert bstack
    bstack = renamed [Replace "bstack"] $ Mirror tile
    ltile = renamed [Replace "ltile"] $ reflectHoriz tile
    tile = renamed [Replace "tile"] $ smartBorders $ magni $ configurableNavigation noNavigateBorders $ addTabs shrinkText myTabConfig $ subLayout [] Simplest $ ResizableTall 1 (3 / 100) (1 / 2) []
    magni = magnifierczOff 1.3

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
        , ("M-<Return>", spawn myTerminal)
        , ("M-e e", spawn myEmacs)
        , ("M-e v", spawn $ myEmacs ++ "-e '(+vterm/here nil)'")
        , ("M-S-<Return>", spawn "dmenu_run_history -F -l 5 -g 10 -p 'Run'")
        , ("M-d v", spawn "audio prompt")
        , ("M-d c", spawn "calc")
        , ("M-d p", spawn "passmenu2 -F -p 'Passwords:'")
        , ("M-d q", spawn "shut")
        , ("M1-C-s", spawn "spotify")
        , ("M-p", spawn "pcmanfm")
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
        , ("M-g e", spawnSelected' $ gridConfig (myEmacs ++))
        , ("M-g t", spawnSelected' $ gridConfig myTerminalPath)
        -- LAYOUTS
        , ("M-<Space>", sendMessage NextLayout)
        , ("M-S-<Space>", setLayout $ XMonad.layoutHook c)
        , ("M-m t", sendMessage $ JumpToLayout "tile")
        , ("M-m l", sendMessage $ JumpToLayout "ltile")
        , ("M-m b", sendMessage $ JumpToLayout "bstack")
        , ("M-m s", sendMessage $ JumpToLayout "tstack")
        , ("M-m a", sendMessage $ JumpToLayout "tabs")
        , ("M-m g", sendMessage $ JumpToLayout "grid")
        , ("M-m c", sendMessage $ JumpToLayout "cmaster")
        -- LAYOUT MANIPULATION
        , ("M-h", sendMessage Shrink)
        , ("M-l", sendMessage Expand)
        , ("M-S-j", sendMessage MirrorShrink)
        , ("M-S-k", sendMessage MirrorExpand)
        , ("M-;", swapPromote' False)
        , ("M-z", sendMessage Toggle) -- magnify
        -- MOVING AROUND WINDOWS
        , ("M-j", windows W.focusDown)
        , ("M-k", windows W.focusUp)
        -- WORKSPACE MANIPULATION
        , ("M-r", renameWorkspace myPrompt)
        , ("M-n", sendMessage ToggleStruts)
        , ("M-[", sendMessage (IncMasterN 1))
        , ("M-]", sendMessage (IncMasterN (-1)))
        -- FLOATS
        , ("M-s", withFocused $ windows . W.sink)
        , ("M-S-s", sinkAll)
        , ("M-f", toggleFloatNext)
        -- TABS
        , ("M-C-h", sendMessage $ pullGroup L)
        , ("M-C-l", sendMessage $ pullGroup R)
        , ("M-C-k", sendMessage $ pullGroup U)
        , ("M-C-j", sendMessage $ pullGroup D)
        , ("M-C-m", withFocused (sendMessage . MergeAll))
        , ("M-C-u", withFocused (sendMessage . UnMergeAll))
        , ("M-C-.", onGroup W.focusUp')
        , ("M-C-,", onGroup W.focusDown')
        ]
        ++ [ ("M-" ++ k, windows $ W.greedyView i)
           | (i, k) <- zip myWorkspaces (map show [1 ..])
           ]
        ++ [ ("M-S-" ++ k, windows =<< shiftRLWhen refocusingIsActive i)
           | (i, k) <- zip myWorkspaces (map show [1 ..])
           ]
        ++ [ ("M-C-" ++ k, swapWithCurrent i)
           | (i, k) <- zip myWorkspaces (map show [1 ..])
           ]
        ++ [ ("M-" ++ m ++ k, screenWorkspace i >>= flip whenJust (windows . f))
           | (k, i) <- zip [",", "."] [0 ..]
           , (f, m) <- [(W.view, ""), (W.shift, "S-")]
           ]

myLogHook = currentWorkspaceOnTop <> refocusLastLogHook <> updatePointer (0.5, 0.5) (0, 0)

myManageHook = composeAll []

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
                                , normalBorderColor = "#4e5579"
                                , focusedBorderColor = "#5fafff"
                                , workspaces = myWorkspaces
                                , borderWidth = 2
                                , layoutHook = avoidStruts myLayout
                                , handleEventHook = refocusLastWhen (refocusingIsActive <||> isFloat) <> handleEventHook def
                                , logHook = myLogHook
                                , manageHook = namedScratchpadManageHook myScratchpads <> placeHook (inBounds (underMouse (0.5, 0.5))) <> floatNextHook <> myManageHook <> manageHook def
                                , keys = myKeys
                                }
