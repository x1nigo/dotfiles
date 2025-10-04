-- =====================================================================
-- x1nigo's xmonad config
-- https://github.com/x1nigo/dotfiles/blob/main/.config/xmonad/xmonad.hs
-- =====================================================================

-- =======
-- Imports
-- =======

import XMonad
import XMonad.Util.EZConfig (additionalKeysP)
import XMonad.Util.Loggers
import XMonad.Util.ClickableWorkspaces -- requires `xdotool` in $PATH + UnsafeXMonadLog in `xmobar`

import qualified XMonad.StackSet as W
import qualified Data.Map as M

import XMonad.Hooks.StatusBar.PP (wrap, xmobarColor, xmobarBorder, xmobarPP, shorten, xmobarStrip, PP(..))
import XMonad.Hooks.StatusBar
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks (avoidStruts, ToggleStruts(..))
import XMonad.Hooks.ManageHelpers (isFullscreen, isDialog, doFullFloat)

import XMonad.Layout.Spacing
import XMonad.Layout.Renamed
import XMonad.Layout.LayoutModifier
import XMonad.Layout.ToggleLayouts

import XMonad.Layout.NoBorders (noBorders, lessBorders, Ambiguity(OnlyScreenFloat))
import XMonad.Layout.ThreeColumns
import XMonad.Layout.Spiral
import XMonad.Layout.Grid
import XMonad.Layout.CenteredMaster
import XMonad.Layout.SimplestFloat

-- =========
-- Variables
-- =========

myTerminal :: String
myTerminal = "st"

myBrowser :: String
myBrowser = "zen-browser"

myFileManager :: String
myFileManager = "lfup"

myBorderWidth :: Dimension
myBorderWidth = 2

myNormalColor :: String
myNormalColor = "#282828"

myFocusedColor :: String
myFocusedColor = "#870000"

mySpacing :: Integer -> l a -> XMonad.Layout.LayoutModifier.ModifiedLayout Spacing l a
mySpacing i = spacingRaw False (Border i i i i) True (Border i i i i) True

-- ==========
-- Workspaces
-- ==========

myWorkspaces = [" 1 ", " 2 ", " 3 ", " 4 ", " 5 ", " 6 ", " 7 ", " 8 ", " 9 "]
-- myWorkspaces = [" www ", " dev ", " art ", " vid ", " mus ", " virt ", " fx ", " sys ", " null "]
-- myWorkspaces = [" one ", " two ", " three ", " four ", " five ", " six ", " seven ", " eight ", " nine "]

-- =====
-- Hooks
-- =====

myManageHook :: ManageHook
myManageHook = composeAll
    [ className =? "termfloat" --> doFloat
    , isDialog                 --> doFloat
    , isFullscreen             --> doFullFloat
    ]

-- ====
-- Main
-- ====

main :: IO ()
main = do
      xmonad
    . ewmhFullscreen
    . ewmh
    . withEasySB mySB defToggleStrutsKey
    $ myConf

mySB = statusBarProp "xmobar $HOME/.config/xmobar/xmobarrc" (clickablePP myPP)

myPP = def
    { ppSep             = " <fc=#373737>|</fc> "
    , ppTitle           = xmobarColor "#d7d7f7" "" . wrap " " " " . shorten 70
    , ppTitleSanitize   = xmobarStrip
    , ppCurrent         = xmobarColor "#57d7f7" "". wrap "" " " . xmobarBorder "Bottom" "#5757d7" 3
    , ppHidden          = xmobarColor "#ff8747" "" . wrap "" " "
    , ppHiddenNoWindows = xmobarColor "#373737" "" . wrap "" " "
    , ppLayout          = xmobarColor "#f74747" "" . wrap " " " "
    , ppUrgent          = xmobarColor "#f78747" "" . wrap "!" "!"
    , ppOrder           = \[ws,l,t] -> [ws,l,t]
    }

-- =====================================
-- Custom configuration and keybindings
-- =====================================

myConf = def
    { modMask            = mod4Mask
    , layoutHook         = myLayoutHook
    , manageHook         = myManageHook
    , normalBorderColor  = myNormalColor
    , focusedBorderColor = myFocusedColor
    , borderWidth        = myBorderWidth
    , workspaces         = myWorkspaces
    }
    `additionalKeysP`
        [ ("M-<Return>",              spawn (myTerminal))
        , ("M-S-<Return>",            spawn (myTerminal ++ " -c termfloat"))
        , ("M-d",                     spawn "dmenu_run -p 'RUN:' -l 6 -g 8")
        , ("M-r",                     spawn (myTerminal ++ " -e " ++ myFileManager))
        , ("M-w",                     spawn (myBrowser))
        , ("M-e",                     spawn "emacs")
        , ("M-S-b",                   spawn "dm-bookmark")
        , ("M-v",                     spawn "dm-videos")
        , ("M-x",                     spawn "dm-wallpaper -d")
        , ("M-S-x",                   spawn "dm-wallpaper -x")
        , ("M-'",                     spawn (myTerminal ++ " -c termfloat -f monospace:size=16 -g 50x20 -e bc -lq"))
        , ("M-<Insert>",              spawn "dm-insert")
        , ("M-`",                     spawn "dm-emoji")
        , ("M-u",                     spawn "dm-unicode")
        , ("M-<Backspace>",           spawn "dm-system")
        , ("<XF86AudioMute>",         spawn "wpctl set-mute @DEFAULT_AUDIO_SINK@ toggle")
        , ("<XF86AudioMicMute>",      spawn "wpctl set-mute @DEFAULT_AUDIO_SOURCE@ toggle")
        , ("<XF86AudioRaiseVolume>",  spawn "wpctl set-volume @DEFAULT_AUDIO_SINK@ 5%+")
        , ("<XF86AudioLowerVolume>",  spawn "wpctl set-volume @DEFAULT_AUDIO_SINK@ 5%-")
        , ("<XF86MonBrightnessUp>",   spawn "brightnessctl s 5%+")
        , ("<XF86MonBrightnessDown>", spawn "brightnessctl s 5%-")
        , ("M-<F1>",                  spawn "readme")
        , ("M-<F2>",                  spawn "dm-fonts")
        , ("M-<F3>",                  spawn (myTerminal ++ " -e pulsemixer"))
        , ("M-<F4>",                  spawn "dm-display")
        , ("M-<F12>",                 spawn "xmonad --restart" )
        , ("M-C-r",                   spawn "xmonad --recompile" )
        , ("<Print>",                 spawn "dm-printscreen")
        , ("M-f",                     sendMessage (Toggle "monocle") >> sendMessage ToggleStruts) -- fullscreen toggle
        , ("M-q",                     kill)
        ]

-- =======
-- Layouts
-- =======

tall     = renamed [Replace "tall"]
           $ mySpacing 6
           $ Tall 1 (3/100) (1/2)

spirals  = renamed [Replace "spirals"]
           $ mySpacing 6
           $ spiral (6/7)

threeCol = renamed [Replace "threeCol"]
           $ mySpacing 6
           $ ThreeCol 1 (3/100) (1/2)

grid     = renamed [Replace "grid"]
           $ mySpacing 6
           $ GridRatio (4/3)

cmaster  = renamed [Replace "cmaster"]
           $ mySpacing 6
           $ centerMaster Grid

monocle  = renamed [Replace "monocle"]
           $ Full

floating = renamed [Replace "floating"]
           $ simplestFloat

myLayoutHook = lessBorders OnlyScreenFloat $ avoidStruts $ toggleLayouts (noBorders monocle)
                                                         $ tall     |||
                                                           spirals  |||
                                                           threeCol |||
                                                           grid     |||
                                                           cmaster  |||
                                                           floating
