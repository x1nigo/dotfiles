import XMonad
import XMonad.Util.EZConfig (additionalKeysP)
import XMonad.Util.Loggers

import XMonad.Hooks.DynamicLog
import XMonad.Hooks.StatusBar
import XMonad.Hooks.StatusBar.PP
import XMonad.Hooks.EwmhDesktops

import XMonad.Layout.Spacing

-- main = xmonad $ xmobarProp $ myConfig
main :: IO ()
main = xmonad
	 . ewmhFullscreen
	 . ewmh
	 . withEasySB (statusBarProp "xmobar ~/.config/xmobar/xmobarrc" (pure myXmobarPP)) defToggleStrutsKey
	 $ myConfig

myXmobarPP :: PP
myXmobarPP = def
	{ ppSep             = " | "
	, ppTitleSanitize   = xmobarStrip
	, ppCurrent         = red . wrap " " "" . xmobarBorder "Bottom" "#87d7f7" 2
	, ppHidden          = blue . wrap " " ""
	, ppHiddenNoWindows = lowWhite . wrap " " ""
	, ppUrgent          = red . wrap (yellow "!") (yellow "!")
	, ppOrder           = \[ws, l, _, wins] -> [ws, l, wins]
	, ppExtras          = [logTitles formatFocused formatUnfocused]
	}
	where
		formatFocused   = magenta . ppWindow
		formatUnfocused = blue . ppWindow

		ppWindow :: String -> String
		ppWindow = xmobarRaw . (\w -> if null w then "untitled" else w) . shorten 30

		blue, lowWhite, magenta, red, white, yellow :: String -> String
		magenta  = xmobarColor "#8787f7" ""
		blue     = xmobarColor "#57d7f7" ""
		white    = xmobarColor "#d7d7d7" ""
		yellow   = xmobarColor "#ffa747" ""
		red      = xmobarColor "#f75757" ""
		lowWhite = xmobarColor "#ebdbb2" ""

myConfig = def
	{ modMask            = mod4Mask
	, layoutHook         = myLayout
	, normalBorderColor  = "#373737"
	, focusedBorderColor = "#570000"
	, borderWidth        = 2
	}
	`additionalKeysP`
		[ ("M-<Return>", spawn "st")
		, ("M-d",        spawn "dmenu_run")
		, ("M-w",        spawn "zen-browser")
		, ("M-C-r",      spawn "xmonad --recompile" )
		, ("M-S-r",      spawn "xmonad --restart" )
		]

myLayout = spacing 8 (tiled ||| Mirror tiled) ||| Full
	where
		tiled = Tall nmaster delta ratio
		nmaster = 1
		ratio = 1/2
		delta = 3/100
