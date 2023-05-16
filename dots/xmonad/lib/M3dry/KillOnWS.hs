module M3dry.KillOnWS (killOnWS) where

import XMonad
import XMonad.StackSet
import qualified XMonad.StackSet as W

import qualified Data.List as L
import Data.Maybe
import Control.Monad

killOnWS :: String -> X ()
killOnWS t = do
    XState { windowset = ws } <- get
    let w = L.find (\w -> t == tag w) $ W.workspaces ws
        in (when (isJust w) $ killWindows $ integrate' (stack $ fromJust w))

killWindows :: [Window] -> X ()
killWindows [x] = do killWindow x
killWindows (x:xs) = do
    killWindow x
    killWindows xs
