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
import System.Exit

import XMonad.Actions.Promote
import Data.Tree
import XMonad.Actions.TreeSelect

import XMonad.Hooks.StatusBar.PP (wrap, xmobarColor, xmobarBorder, xmobarPP, shorten, xmobarStrip, PP(..))
import XMonad.Hooks.StatusBar
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks (avoidStruts, ToggleStruts(..))
import XMonad.Hooks.ManageHelpers (isFullscreen, isDialog, doCenterFloat, doFullFloat)

import XMonad.Prompt
import XMonad.Prompt.XMonad (xmonadPrompt)
import XMonad.Prompt.Shell (shellPrompt)
import XMonad.Prompt.Man (manPrompt)

import XMonad.Layout.Spacing (spacingWithEdge, toggleWindowSpacingEnabled, toggleScreenSpacingEnabled, incScreenWindowSpacing, decScreenWindowSpacing, setScreenWindowSpacing)
import XMonad.Layout.Renamed
import XMonad.Layout.LayoutModifier
import qualified XMonad.Layout.MultiToggle as MT
import XMonad.Layout.MultiToggle (mkToggle, Toggle, EOT(EOT), (??))
import XMonad.Layout.MultiToggle.Instances (StdTransformers(NBFULL, NOBORDERS))
import XMonad.Layout.ShowWName

import XMonad.Layout.NoBorders (noBorders, lessBorders, Ambiguity(OnlyScreenFloat))
import XMonad.Layout.ThreeColumns
import XMonad.Layout.Spiral
import XMonad.Layout.Grid
import XMonad.Layout.CenteredMaster
import XMonad.Layout.CenterMainFluid
import XMonad.Layout.SimplestFloat
import XMonad.Layout.StackTile

-- =========
-- Variables
-- =========

myTerminal :: String
myTerminal = "st"

myBrowser :: String
myBrowser = "zen-browser"

myFileManager :: String
myFileManager = "lfup"

myMusicPlayer :: String
myMusicPlayer = "ncmpcpp"

myBorderWidth :: Dimension
myBorderWidth = 2

myNormalColor :: String
myNormalColor = "#282828"

myFocusedColor :: String
myFocusedColor = "#d7d7f7"

-- ==========
-- Workspaces
-- ==========

-- myWorkspaces = [" 1 ", " 2 ", " 3 ", " 4 ", " 5 ", " 6 ", " 7 ", " 8 ", " 9 "]
myWorkspaces = [" www ", " dev ", " media ", " typeW ", " art ", " sfx ", " mus ", " sys ", " null "]

-- =====
-- Hooks
-- =====

myManageHook :: ManageHook
myManageHook = composeAll
    [ className =? "termfloat" --> doCenterFloat
    , isDialog                 --> doFloat
    , isFullscreen             --> doFullFloat
    , className =? "zen"                     --> doShift ( myWorkspaces !! 0 )
    , className =? "Brave-browser"           --> doShift ( myWorkspaces !! 0 )
    , className =? "librewolf"               --> doShift ( myWorkspaces !! 0 )
    , className =? "firefox"                 --> doShift ( myWorkspaces !! 0 )
    , className =? "mpv"                     --> doShift ( myWorkspaces !! 2 )
    , className =? "Gimp"                    --> doShift ( myWorkspaces !! 5 )
    , className =? "libreoffice-startcenter" --> doShift ( myWorkspaces !! 3 )
    , className =? "Lxappearance"            --> doShift ( myWorkspaces !! 7 )
    ]

-- ====
-- Main
-- ====

main :: IO ()
main = xmonad . ewmhFullscreen . ewmh . withEasySB mySB mySK $ myConf

mySB = statusBarProp "xmobar $HOME/.config/xmobar/xmobarrc" (clickablePP myPP)

mySK :: XConfig Layout -> (KeyMask, KeySym)
mySK XConfig { modMask = m } = (m .|. shiftMask, xK_b)

myPP = def
    { ppSep             = " | "
    , ppTitle           = xmobarColor "#d7d7f7" "" . wrap " " " " . shorten 70
    , ppTitleSanitize   = xmobarStrip
    , ppCurrent         = xmobarColor "#5757f7" "" . wrap "[" "]"
    , ppHidden          = xmobarColor "#ff8747" "" . wrap " " " "
    , ppHiddenNoWindows = xmobarColor "#373737" "" . wrap " " " "
    , ppLayout          = xmobarColor "#f74747" "" . wrap " " " "
    , ppUrgent          = xmobarColor "#f78747" "" . wrap "!" "!"
    , ppOrder           = \[ws,l,t] -> [ws,l,t]
    }

-- ======================
-- Prompt (xmonad-native)
-- ======================

myXPConfig :: XPConfig
myXPConfig = def
    { font                = "xft:monospace:size=10"
    , bgColor             = "#21242b"
    , fgColor             = "#d7d7f7"
    , bgHLight            = "#8787f7"
    , fgHLight            = "#282828"
    , promptBorderWidth   = 0
    , position            = Top
    , alwaysHighlight     = True
    , height              = 24
    , historySize         = 0
    , showCompletionOnTab = False
    }

-- =========================
-- Show WS names upon switch
-- See `myLayoutHook`
-- =========================

mySWNConfig :: SWNConfig
mySWNConfig = def
    { swn_font    = "xft:monospace:bold:size=32"
    , swn_bgcolor = "#21242b"
    , swn_color   = "#d7d7f7"
    , swn_fade    = 1.5
    }

myTreeConf :: TSConfig a
myTreeConf = def
    { ts_hidechildren = True
    , ts_background   = 0xe721242b
    , ts_font         = "xft:monospace-10"
    , ts_node_width   = 200
    , ts_node_height  = 24
    , ts_node         = (0xffd7d7f7, 0xff21242b)
    , ts_nodealt      = (0xffd7d7f7, 0xff1d2024)
    , ts_highlight    = (0xff282828, 0xff5757d7)
    }

myActions =
    [ Node (TSNode "XMonad" "Execute an xmonad-specific action" (return ()))
	    [ Node (TSNode "Restart WM" "Restart xmonad" (spawn "xmonad --restart")) []
	    , Node (TSNode "Recompile WM" "Recompile xmonad" (spawn "xmonad --recompile")) []
	    , Node (TSNode "Logout/Quit" "Log (quit) out of xmonad" (io exitSuccess)) []
        ]
    , Node (TSNode "Applications" "Select the usual programs to run" (return ()))
	    [ Node (TSNode "Terminal" "Open up the terminal" (spawn myTerminal)) []
	    , Node (TSNode "Browser" "Search the internet (www)" (spawn myBrowser)) []
	    , Node (TSNode "File Manager" "Navigate the filesystem" (spawn (myTerminal ++ " -e " ++ myFileManager))) []
	    , Node (TSNode "Music Player" "Listen to some tunes" (spawn (myTerminal ++ " -e " ++ myMusicPlayer))) []
	    , Node (TSNode "Image Editor" "Run an image-editing program" (spawn "gimp")) []
	    , Node (TSNode "Office Suite" "Create/Edit documents" (spawn "libreoffice")) []
        ]
    , Node (TSNode "System" "Execute a system action" (return ()))
	    [ Node (TSNode "Shutdown" "Shuts the system down" (spawn "systemctl poweroff")) []
	    , Node (TSNode "Restart/Reboot" "Reboot the system" (spawn "systemctl reboot")) []
	    , Node (TSNode "Display" "Turns the display off" (spawn "xset dpms force off")) []
        ]
    ]

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
        , ("M-<Tab>",                 sendMessage NextLayout)
        , ("M-<Space>",               promote)
        , ("M-d",                     spawn "dmenu_run -p 'RUN:' -l 6 -g 8")
        , ("M-r",                     spawn (myTerminal ++ " -e " ++ myFileManager))
        , ("M-n",                     spawn (myTerminal ++ " -e " ++ myMusicPlayer))
        , ("M-w",                     spawn (myBrowser))
        , ("M-b",                     spawn "dm-bookmark")
        , ("M-v",                     spawn "dm-videos")
        , ("M-x",                     spawn "dm-wallpaper -d")
        , ("M-S-x",                   spawn "dm-wallpaper -x")
        , ("M-'",                     spawn (myTerminal ++ " -c termfloat -f monospace:size=16 -g 50x20 -e bc -lq"))
        , ("<Insert>",                spawn "dm-insert")
        , ("M-`",                     spawn "dm-emoji")
        , ("M-u",                     spawn "dm-unicode")
        -- , ("M-<Backspace>",           spawn "dm-system")
        , ("M-<Backspace>",           treeselectAction myTreeConf myActions)
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
        , ("M-f",                     sendMessage (MT.Toggle NBFULL) >> sendMessage ToggleStruts)
        , ("M-C-b",                   sendMessage (MT.Toggle NOBORDERS))
        , ("M-g",                     toggleWindowSpacingEnabled  >> toggleScreenSpacingEnabled)
        , ("M-S-g",                   setScreenWindowSpacing 4)
        , ("M-C-k",                   incScreenWindowSpacing 1)
        , ("M-C-j",                   decScreenWindowSpacing 1)
        , ("M-p",                     shellPrompt  myXPConfig)
        , ("M-S-p",                   xmonadPrompt myXPConfig)
        , ("M-m",                     manPrompt    myXPConfig)
        , ("M-q",                     kill)
        ]

-- =======
-- Layouts
-- =======

-- NOTE: changes to spacing, renaming, ratio, and other aspects of layouts will probably require a reboot

tall      = renamed [Replace "tall"]
            $ spacingWithEdge 4
            $ Tall 1 (3/100) (1/2)

stackT    = renamed [Replace "stackT"]
            $ spacingWithEdge 4
            $ StackTile 1 (3/100) (1/2)

fibonacci = renamed [Replace "fibonacci"]
            $ spacingWithEdge 4
            $ spiral (6/7)

threeCol  = renamed [Replace "threeCol"]
            $ spacingWithEdge 4
            $ ThreeCol 1 (3/100) (1/2)

grid      = renamed [Replace "grid"]
            $ spacingWithEdge 4
            $ GridRatio (4/3)

cmaster   = renamed [Replace "cmaster"]
            $ spacingWithEdge 4
            $ centerMaster Grid

cmasterF  = renamed [Replace "cmasterF"]
            $ spacingWithEdge 4
            $ CenterMainFluid 1 (3/100) (70/100)

monocle   = renamed [Replace "monocle"]
            $ Full

floating  = renamed [Replace "floating"]
            $ simplestFloat

myLayoutHook = showWName' mySWNConfig $ lessBorders OnlyScreenFloat $ avoidStruts $ mkToggle (NBFULL ?? NOBORDERS ?? EOT)
                                                                                  $ tall      |||
                                                                                    stackT    |||
                                                                                    fibonacci |||
                                                                                    threeCol  |||
                                                                                    grid      |||
                                                                                    cmaster   |||
                                                                                    cmasterF  |||
                                                                          noBorders monocle   |||
                                                                                    floating
