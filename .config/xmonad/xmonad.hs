import XMonad
import XMonad.Util.EZConfig (additionalKeysP)
import XMonad.Util.Loggers
import qualified XMonad.StackSet as W
import qualified Data.Map as M

import XMonad.Hooks.DynamicLog (dynamicLogWithPP, wrap, xmobarPP, xmobarColor, shorten, PP(..))
import XMonad.Hooks.StatusBar
import XMonad.Hooks.StatusBar.PP
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks (avoidStruts)
import XMonad.Hooks.ManageHelpers (isFullscreen, doFullFloat)

import XMonad.Layout.Spacing
import XMonad.Layout.Renamed
import XMonad.Layout.LayoutModifier

import XMonad.Layout.NoBorders (noBorders, lessBorders, Ambiguity(OnlyFloat))
import XMonad.Layout.ThreeColumns
import XMonad.Layout.Spiral

myTerminal :: String
myTerminal = "st"

myBrowser :: String
myBrowser = "zen-browser"

myFileManager :: String
myFileManager = "lfup"

myBorderWidth :: Dimension
myBorderWidth = 3

myNormalColor :: String
myNormalColor = "#282828"

myFocusedColor :: String
myFocusedColor = "#570000"

-- windowCount :: X (Maybe String)
-- windowCount = gets $ Just . show . length . W.integrate' . W.stack . W.workspace . W.current . windowset

mySpacing :: Integer -> l a -> XMonad.Layout.LayoutModifier.ModifiedLayout Spacing l a
mySpacing i = spacingRaw False (Border i i i i) True (Border i i i i) True

myWorkspaces = [" 1 ", " 2 ", " 3 ", " 4 ", " 5 ", " 6 ", " 7 ", " 8 ", " 9 "]
-- myWorkspaces = [" www ", " dev ", " doc ", " vid ", " pix ", " mus ", " vbox ", " art ", " sys "]

main :: IO ()
main = xmonad
     $ ewmhFullscreen
     $ ewmh
     $ withEasySB (statusBarProp "xmobar ~/.config/xmobar/xmobarrc" (pure myXmobarPP)) defToggleStrutsKey
     $ myConfig

myXmobarPP :: PP
myXmobarPP = def
    { ppSep             = " | "
    , ppTitle           = white . wrap " " " " . shorten 60
    , ppTitleSanitize   = xmobarStrip
    , ppCurrent         = blue . wrap " " " " . xmobarBorder "Bottom" "#dc2800" 2
    , ppHidden          = white . wrap " " " "
    , ppHiddenNoWindows = black . wrap " " " "
    , ppLayout          = red . wrap " " " "
    , ppUrgent          = red . wrap (yellow "!") (yellow "!")
    -- , ppExtras          = [windowCount]
    , ppOrder           = \[ws, l, t] -> [ws, l, t]
    }
    where
        black, red, green, yellow, blue, magenta, cyan, white :: String -> String
        black    = xmobarColor "#373737" ""
        red      = xmobarColor "#f75757" ""
        green    = xmobarColor "#87d7a7" ""
        yellow   = xmobarColor "#ffa747" ""
        blue     = xmobarColor "#57d7f7" ""
        magenta  = xmobarColor "#8787f7" ""
        cyan     = xmobarColor "#87d7f7" ""
        white    = xmobarColor "#ebdbb2" ""

myConfig = def
    { modMask            = mod4Mask
    , layoutHook         = lessBorders OnlyFloat $ avoidStruts $ myLayout
    , manageHook         = myManageHook
    , normalBorderColor  = myNormalColor
    , focusedBorderColor = myFocusedColor
    , borderWidth        = myBorderWidth
    , workspaces         = myWorkspaces
    }
    `additionalKeysP`
        [ ("M-<Return>",              spawn (myTerminal))
        , ("M-d",                     spawn "dmenu_run -h 26")
        , ("M-r",                     spawn (myTerminal ++ " -e " ++ myFileManager))
        , ("M-w",                     spawn (myBrowser))
        , ("M-b",                     spawn "bookmarker")
        , ("M-v",                     spawn "watchvid")
        , ("M-x",                     spawn "setbg -d")
        , ("M-S-x",                   spawn "setbg -x")
        , ("M-'",                     spawn (myTerminal ++ " -n termfloat -f monospace:size=16 -g 50x20 -e bc -lq"))
        , ("M-<Insert>",              spawn "inserter")
        , ("M-`",                     spawn "dmenumoji")
        , ("M-<Backspace>",           spawn "systemmenu")

        , ("<XF86AudioMute>",         spawn "wpctl set-mute @DEFAULT_AUDIO_SINK@ toggle")
        , ("<XF86AudioMicMute>",      spawn "wpctl set-mute @DEFAULT_AUDIO_SOURCE@ toggle")
        , ("<XF86AudioRaiseVolume>",  spawn "wpctl set-volume @DEFAULT_AUDIO_SINK@ 5%+")
        , ("<XF86AudioLowerVolume>",  spawn "wpctl set-volume @DEFAULT_AUDIO_SINK@ 5%-")

        , ("<XF86MonBrightnessUp>",   spawn "brightnessctl s 5%+")
        , ("<XF86MonBrightnessDown>", spawn "brightnessctl s 5%-")

        , ("M-<F1>",                  spawn "readme")
        , ("M-<F2>",                  spawn "fontwizard")
        , ("M-<F3>",                  spawn (myTerminal ++ " -e pulsemixer"))
        , ("M-<F4>",                  spawn "selectdisplay")
        , ("M-<F12>",                 spawn "xmonad --restart" )
        , ("M-C-r",                   spawn "xmonad --recompile" )
        , ("<Print>",                 spawn "printscreen")
        , ("M-q",                     kill)
        ]

myManageHook :: ManageHook
myManageHook = composeAll
    [ className =? "dialog"   --> doFloat
	, className =? "download" --> doFloat
	, isFullscreen            --> doFullFloat
    ]

tall     = renamed [Replace "tall"]
           $ mySpacing 6
           $ Tall 1 (3/100) (1/2)

monocle  = renamed [Replace "monocle"]
         $ Full

threeCol = renamed [Replace "threeCol"]
           $ mySpacing 6
           $ ThreeCol 1 (3/100) (1/2)

spirals  = renamed [Replace "spirals"]
           $ mySpacing 6
           $ spiral (6/7)

myLayout = tall     |||
           spirals  |||
           threeCol |||
           noBorders monocle
