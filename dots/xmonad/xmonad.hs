import qualified Data.List as L
import Data.Maybe
import System.Exit
import Control.Monad;
import Control.Arrow (first)
import Data.Char
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
import XMonad.Actions.OnScreen

import XMonad.Hooks.CurrentWorkspaceOnTop
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.FloatNext
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.Place
import XMonad.Hooks.RefocusLast
import XMonad.Hooks.TaffybarPagerHints
import XMonad.Hooks.WorkspaceHistory
import qualified XMonad.Hooks.InsertPosition as IP
import XMonad.Hooks.Minimize
import XMonad.Hooks.DynamicProperty
import XMonad.Hooks.UrgencyHook

import XMonad.Layout.CenteredMaster
import XMonad.Layout.Grid
import XMonad.Layout.Magnifier
import XMonad.Layout.NoBorders
import XMonad.Layout.Reflect
import qualified XMonad.Layout.Renamed as Renamed
import XMonad.Layout.ResizableTile
import XMonad.Layout.Simplest
import XMonad.Layout.SubLayouts
import XMonad.Layout.Tabbed
import XMonad.Layout.WindowNavigation
import XMonad.Layout.Minimize
import XMonad.Layout.BoringWindows

import XMonad.Util.NamedActions
import XMonad.Util.EZConfig
import XMonad.Util.SpawnOnce
import XMonad.Util.NamedScratchpad
import qualified XMonad.Util.ExtensibleState as XS

killOnWS :: String -> X ()
killOnWS t = do
    XState { windowset = ws } <- get
    let w = L.find (\w -> t == W.tag w) $ W.workspaces ws
        in (when (isJust w) $ killWindows $ W.integrate' (W.stack $ fromJust w))
  where
    killWindows :: [Window] -> X ()
    killWindows [x] = do killWindow x
    killWindows (x:xs) = do
        killWindow x
        killWindows xs

-- PROJECT TOGGLE
newtype ProjectWSToggle = ProjectWSToggle Bool
instance ExtensionClass ProjectWSToggle where
    initialValue = ProjectWSToggle False

getToggleWS :: X Bool
getToggleWS = do
    ProjectWSToggle toggle <- XS.get :: X ProjectWSToggle
    return toggle

toggleProjectWS = do
    ProjectWSToggle toggle <- XS.get :: X ProjectWSToggle
    XS.put $ ProjectWSToggle (not toggle)

projectFromI :: Int -> (Bool -> Bool) -> X (Maybe Topic)
projectFromI i mod = do
    toggle <- getToggleWS
    if mod toggle && length projectTopics >= i then
        return $ Just $ projectTopics !! (i - 1)
    else if mod toggle && length generalTopics >= i then
        return Nothing
    else
        return $ Just $ generalTopics !! (i - 1)

projectToggleDo :: (String, String) -> (Topic -> X ()) -> [(String, X ())]
projectToggleDo (key, projectKey) func =
    [ (k ++ show i,
        do
            Just topic <- projectFromI i mod
            func topic)
    | i <- [1..9]
    , (k, mod) <- [(key, id), (projectKey, not)]
    ]

-- MONITOR actions
monitorAction :: (String, String) -> Maybe (Topic -> X ()) -> Focus -> [(String, X ())]
monitorAction (generalKey, projectKey) func focus =
    [ ("M-C-" ++ k ++ key ++ " " ++ show i,
        do
            Just t <- projectFromI i mod
            whenJust func (\f -> f t)
            sc >>= (windows . onScreen (W.greedyView t) focus)
            pagerHintsLogHook)
    | i <- [1..9]
    , (key, mod) <- [(generalKey, id), (projectKey, not)]
    , (k, sc) <- zip [", ", ". "] [screenBy (-1), screenBy 1]
    ]

myTerminal = "st"
myTerminalDir path = myTerminal ++ " -d " ++ path

myEmacs = "emacsclient -c "
myEmacsDir dir = myEmacs ++ "-e \"(dired \\\"" ++ dir ++ "\\\")\""

myFont size = "xft:ComicCodeLigatures Nerd Font:pixelsize=" ++ show size

myModMask = mod4Mask

myTopicItems =
    [ inHome   "1"                                          (spawn "xmessage what do you think you're doing?" >> do
                                                                                                                   Just t <- projectFromI 4 id
                                                                                                                   windows $ W.greedyView t)
    , inHome   "2"                                          (spawn "chromium")
    , inHome   "3"                                          (spawn "spotify")
    , inHome   "4"                                          (spawn "obsidian")
    , noAction "5"          "."
    , noAction "6"          "."
    , noAction "7"          "."
    , noAction "8"          "."
    , noAction "9"          ".config/flake"
    , noAction "Nfetch"     "my-stuff/Projects/Rust/nfetch"
    , noAction "Quandeth"   "my-stuff/Projects/Rust/nfetch"
    , noAction "Nap"        "my-stuff/Projects/Rust/nfetch"
    ]

myTopicConfig = def
    { topicDirs          = tiDirs myTopicItems
    , topicActions       = tiActions myTopicItems
    , defaultTopicAction = const (pure ())
    , defaultTopic       = "1"
    }

inDir :: (String -> String) -> X ()
inDir cmd = currentTopicDir myTopicConfig >>= spawn . cmd

projectTopics :: [Topic]
projectTopics = filter (\topic -> (ord (head topic) - ord '0') > 9) (topicNames myTopicItems)
generalTopics :: [Topic]
generalTopics = filter (\topic -> (ord (head topic) - ord '0') <= 9) (topicNames myTopicItems)

myDefaultPrompt = def
    { bgColor = "#0f111b"
    , fgColor = "#eeffff"
    , bgHLight = "#c792ea"
    , fgHLight = "#0f111b"
    , borderColor = "#c792ea"
    , promptBorderWidth = 2
    , font = myFont 16
    , height = 44
    , position = Bottom
    , promptKeymap = M.union vimLikeXPKeymap (M.fromList $ map (first $ (,) controlMask) [ (xK_c, quit) ])
    , completionKey = (0, xK_Tab)
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
    where
        conf = def
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
  , ("Htop", myTerminal ++ " -e htop")
  , ("Neovim", myTerminal ++ " -e neovim")
  , ("Emacs", myEmacs)
  , ("Pcmanfm", "pcmanfm")
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

myLayout = boringWindows (tile ||| ltile ||| centeredmaster ||| grid ||| bstack ||| tstack ||| tabs)
  where
    centeredmaster = Renamed.renamed [Renamed.Replace "cmaster"] $ minimize $ smartBorders $ centerMaster Grid
    grid = Renamed.renamed [Renamed.Replace "grid"] $ minimize $ smartBorders $ configurableNavigation noNavigateBorders $ addTabs shrinkText myTabConfig $ subLayout [] Simplest $ magni Grid
    tabs = Renamed.renamed [Renamed.Replace "tabs"] $ minimize $ noBorders $ tabbed shrinkText myTabConfig
    tstack = Renamed.renamed [Renamed.Replace "tstack"] $ reflectVert bstack
    bstack = Renamed.renamed [Renamed.Replace "bstack"] $ Mirror tile
    ltile = Renamed.renamed [Renamed.Replace "ltile"] $ reflectHoriz tile
    tile = Renamed.renamed [Renamed.Replace "tile"] $ minimize $ smartBorders $ magni $ configurableNavigation noNavigateBorders $ addTabs shrinkText myTabConfig $ subLayout [] Simplest $ ResizableTall 1 (3 / 100) (1 / 2) []
    magni = magnifierczOff 1.3

myScratchpads =
  [ NS "terminal" (term "-t scratchpad" "") (findT "scratchpad") centerFloat
  , NS "cmus" (term "-c cmuspad" "cmus") (findC "cmuspad") centerFloat
  , NS "qalc" (term "-t qalcpad" "qalc") (findT "qalcpad") centerFloat
  ]
  where
    term x y = myTerminal ++ " " ++ x ++ " -e " ++ y
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
        [ ("M-C-q q", io exitSuccess)
        , ("M-C-q r", spawn "if type xmonad; then xmonad --recompile && xmonad --restart; else xmessage xmonad not in \\$PATH: \"$PATH\"; fi")
        , ("M-q", kill)
        , ("M-S-q", killOthers)
        , ("M1-C-<KP_Down>", spawn "xkill")
        -- APPS
        , ("M-<Return>", inDir myTerminalDir)
        , ("M-e e", inDir myEmacsDir)
        , ("M-e v", spawn $ myEmacs ++ "-e '(+vterm/here nil)'")
        , ("M-S-<Return>", spawn "dmenu_run_history -F -l 5 -g 10 -p 'Run'")
        , ("M-d v", spawn "audio prompt")
        , ("M-d c", spawn "calc")
        , ("M-d p", spawn "passmenu2 -F -p 'Passwords:'")
        , ("M-d q", spawn "shut")
        , ("M-u", spawn "SNIPPATH=\"${HOME}/my-stuff/Pictures/snips/$(date +'%F-%T').png\"; import \"${SNIPPATH}\" && xclip -selection clipboard -t image/png \"${SNIPPATH}\"")
        , ("M1-C-l", spawn "slock")
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
        , ("M-g t", spawnSelected' $ gridConfig myTerminalDir)
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
        , ("M-;", swapHybrid' True)
        , ("M-z", sendMessage Toggle) -- magnify
        , ("M-f", spawn "xdotool windowstate --toggle FULLSCREEN $(xdotool getwindowfocus)")
        , ("M-m", withFocused minimizeWindow)
        , ("M-S-m", withLastMinimized maximizeWindowAndFocus)
        -- MOVING AROUND WINDOWS
        , ("M-j", focusDown)
        , ("M-k", focusUp)
        , ("M-<Space>", focusMaster)
        -- WORKSPACE MANIPULATION
        , ("M-r", renameWorkspace myDefaultPrompt)
        , ("M-n", sendMessage ToggleStruts)
        , ("M-[ n", sendMessage (IncMasterN 1))
        , ("M-] n", sendMessage (IncMasterN (-1)))
        -- FLOATS
        , ("M-s",   withFocused $ windows . W.sink)
        , ("M-S-s", sinkAll)
        , ("M-S-f", toggleFloatNext)
        -- TABS
        , ("M-C-h",   sendMessage $ pullGroup L)
        , ("M-C-l",   sendMessage $ pullGroup R)
        , ("M-C-k",   sendMessage $ pullGroup U)
        , ("M-C-j",   sendMessage $ pullGroup D)
        , ("M-C-m",   withFocused (sendMessage . MergeAll))
        , ("M-C-u",   withFocused (sendMessage . UnMerge))
        , ("M-S-C-u", withFocused (sendMessage . UnMergeAll))
        , ("M-[ t",   onGroup W.focusDown')
        , ("M-] t",   onGroup W.focusUp')
        -- MONITORS
        , ("M-.",     nextScreen)
        , ("M-,",     prevScreen)
        , ("M-S-.",   shiftNextScreen)
        , ("M-S-,",   shiftPrevScreen)
        , ("M-C-S-.", swapNextScreen)
        , ("M-C-S-,", swapPrevScreen)
        -- TOPICS
        , ("M-a", currentTopicAction myTopicConfig)
        , ("M-<Tab>", switchNthLastFocusedByScreen myTopicConfig 1)
        -- WORKSPACE MOVING
        , ("M-] w", moveTo Next emptyWS)
        , ("M-[ w", moveTo Prev emptyWS)
        , ("M-] S-w", shiftTo Next emptyWS)
        , ("M-[ S-w", shiftTo Prev emptyWS)
        , ("M-p t", toggleProjectWS)
        ]
        ++ projectToggleDo ("M-", "M-p ") toggleOrView
        ++ projectToggleDo ("M-S-", "M-p S-") (windows <=< shiftRLWhen refocusingIsActive)
        ++ projectToggleDo ("M-C-", "M-p C-") swapWithCurrent
        ++ projectToggleDo ("M-C-q ", "M-C-q S-") killOnWS
        ++ monitorAction ("s", "s p ") Nothing FocusCurrent
        ++ monitorAction ("f", "f p ") Nothing FocusNew
        ++ monitorAction ("S-s", "S-s p") (Just $ windows . W.shift) FocusCurrent
        ++ monitorAction ("S-f", "S-f p") (Just $ windows . W.shift) FocusNew

myMouseBinds (XConfig {XMonad.modMask = modMask}) = M.fromList
    [ ((modMask, button1), \w -> focus w >> mouseMoveWindow w >> windows W.shiftMaster)
    , ((modMask, button2), windows . (W.shiftMaster .) . W.focusWindow)
    , ((modMask .|. shiftMask, button2), windows . W.sink)
    , ((modMask, button3), \w -> focus w >> Flex.mouseResizeWindow w >> windows W.shiftMaster)
    ]

myLogHook =
    workspaceHistoryHook <>
    masterHistoryHook <>
    currentWorkspaceOnTop <>
    refocusLastLogHook

myHandleEventHook =
    refocusLastWhen (refocusingIsActive <||> isFloat) <>
    focusOnMouseMove <>
    minimizeEventHook <>
    dynamicPropertyChange "WM_NAME" (composeAll
    [ title =? "Spotify" --> doShift "3"
    ]) <> handleEventHook def

myManageHook =
    namedScratchpadManageHook myScratchpads <>
    --placeHook (inBounds (underMouse (0.5, 0.5))) <>
    floatNextHook <>
    IP.insertPosition IP.Below IP.Newer <>
    composeAll
    [ className =? "Navigator"        --> doShift "1"
    , className =? "Chromium-browser" --> doShift "2"
    , className =? "obsidian"         --> doShift "4"
    ]

main = do
    xmonad $ docks $ ewmhFullscreen $ setEwmhActivateHook doAskUrgent $ workspaceNamesEwmh $ ewmh $ pagerHints $
        def
            { terminal = myTerminal
            , modMask = myModMask
            , normalBorderColor = "#0f111b"
            , focusedBorderColor = "#c792ea"
            , workspaces = topicNames myTopicItems
            , borderWidth = 2
            , layoutHook = avoidStruts myLayout
            , handleEventHook = myHandleEventHook
            , startupHook =
                adjustEventInput <>
                (do
                    Just t <- projectFromI 4 id
                    windows $ W.greedyView t)
            , logHook = myLogHook
            , manageHook = myManageHook
            , keys = myKeys
            , mouseBindings = myMouseBinds
            , focusFollowsMouse = True
            }
