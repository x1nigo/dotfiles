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

import XMonad.Layout.NoBorders (noBorders, lessBorders, Ambiguity(OnlyScreenFloat))
import XMonad.Layout.ThreeColumns
import XMonad.Layout.Spiral
import XMonad.Layout.Grid

myTerminal :: String
myTerminal = "st"

myBrowser :: String
myBrowser = "brave"

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
     . ewmhFullscreen
     . ewmh
     . withEasySB (statusBarProp "xmobar ~/.config/xmobar/xmobarrc" (pure myXmobarPP)) toggleStrutsKey
     $ myConfig
     where
         toggleStrutsKey :: XConfig Layout -> (KeyMask, KeySym)
         toggleStrutsKey XConfig {modMask = mod4Mask} = (mod4Mask .|. shiftMask, xK_b)

myXmobarPP :: PP
myXmobarPP = def
    { ppSep             = " <fc=#575757>|</fc> "
    , ppTitle           = white . wrap " " " " . shorten 70
    , ppTitleSanitize   = xmobarStrip
    , ppCurrent         = cyan . wrap " " " " . xmobarBorder "Bottom" "#57d7f7" 3
    , ppHidden          = magenta . wrap " " " " . xmobarBorder "Top" "#5757d7" 3
    , ppHiddenNoWindows = black . wrap " " " "
    , ppLayout          = red . wrap " " " "
    , ppUrgent          = red . wrap (yellow "!") (yellow "!")
    -- , ppExtras          = [windowCount]
    , ppOrder           = \[ws,l,t] -> [ws,l,t]
    }
    where
        black, red, green, yellow, blue, magenta, cyan, white :: String -> String
        black   = xmobarColor "#373737" ""
        red     = xmobarColor "#f74747" ""
        green   = xmobarColor "#87d7a7" ""
        yellow  = xmobarColor "#ff8747" ""
        blue    = xmobarColor "#005577" ""
        magenta = xmobarColor "#5757d7" ""
        cyan    = xmobarColor "#87d7f7" ""
        white   = xmobarColor "#d7d7d7" ""

myManageHook :: ManageHook
myManageHook = composeAll
    [ className =? "dialog"    --> doFloat
	, className =? "download"  --> doFloat
    , className =? "termfloat" --> doFloat
	, isFullscreen             --> doFullFloat
    ]

myConfig = def
    { modMask            = mod4Mask
    , layoutHook         = lessBorders OnlyScreenFloat $ avoidStruts $ myLayoutHook
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
        , ("M-b",                     spawn "dm-bookmark")
        , ("M-v",                     spawn "dm-videos")
        , ("M-x",                     spawn "dm-wallpaper -d")
        , ("M-S-x",                   spawn "dm-wallpaper -x")
        , ("M-'",                     spawn (myTerminal ++ " -c termfloat -f monospace:size=16 -g 50x20 -e bc -lq"))
        , ("M-<Insert>",              spawn "dm-insert")
        , ("M-`",                     spawn "dm-emoji")
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
        , ("M-q",                     kill)
        ]

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

monocle  = renamed [Replace "monocle"]
           $ Full

myLayoutHook = tall     |||
               spirals  |||
               threeCol |||
               grid     |||
               noBorders monocle
