import XMonad

import XMonad.Util.EZConfig
import XMonad.Util.Ungrab
import XMonad.Util.Loggers

import XMonad.Layout.ThreeColumns
import XMonad.Layout.Magnifier

import XMonad.Hooks.EwmhDesktops

import XMonad.Hooks.DynamicLog
import XMonad.Hooks.StatusBar
import XMonad.Hooks.StatusBar.PP
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers

main :: IO()
main = xmonad
       . ewmhFullscreen
       . ewmh
       . withEasySB (statusBarProp "xmobar" (pure myXmobarPP)) defToggleStrutsKey
       $ myConfig
   where
     toggleStrutsKey :: XConfig Layout -> (KeyMask, KeySym)
     toggleStrutsKey XConfig{ modMask = m } = (m, xK_b)

myXmobarPP :: PP
myXmobarPP = def
    { ppSep             = magenta " • "
    , ppTitleSanitize   = xmobarStrip
    , ppCurrent         = blue . wrap "<" ">"
    , ppHidden          = white . wrap "<" ">"
    , ppHiddenNoWindows = lowWhite . wrap " " ""
    , ppUrgent          = red . wrap (yellow "!") (yellow "!")
    , ppOrder           = \[ws, l, _, wins] -> [ws, l, wins]
    , ppExtras          = [logTitles formatFocused formatUnfocused]
    }
  where
    formatFocused   = wrap (white    "[") (white    "]") . magenta . ppWindow
    formatUnfocused = wrap (lowWhite "[") (lowWhite "]") . blue    . ppWindow

    -- | Windows should have *some* title, which should not not exceed a
    -- sane length.
    ppWindow :: String -> String
    ppWindow = xmobarRaw . (\w -> if null w then "untitled" else w) . shorten 30

    blue, lowWhite, magenta, red, white, yellow :: String -> String
    magenta  = xmobarColor "#d3869b" ""
    blue     = xmobarColor "#83a598" ""
    white    = xmobarColor "#ebdbb2" ""
    yellow   = xmobarColor "#fabd2f" ""
    red      = xmobarColor "#fb4934" ""
    lowWhite = xmobarColor "#a89984" ""

myConfig = def
  {
    modMask = mod4Mask -- rebind Mod to the Super key
    , terminal = "termonad" -- set terminal
    , layoutHook = myLayout
    , manageHook = myManageHook
    , focusedBorderColor = "#83a598"
    , normalBorderColor = "#a89984"
    
  }
  `additionalKeysP`
  [ ("M-w", spawn "firefox")
  , ("M-e", spawn "emacs")
  , ("M-C-s", unGrab *> spawn "scrot -s")
  ]

myLayout = tiled ||| Mirror tiled ||| Full ||| threeCol
  where 
    threeCol = magnifiercz' 1.3 $ ThreeColMid nmaster delta ratio
    tiled = Tall nmaster delta ratio
    nmaster = 1 -- Default number of windows in the master pane
    ratio = 1/2 -- Default proportion of screen occupied by master pane
    delta = 3/100 -- Percent of screen to increment when resizing panes

myManageHook :: ManageHook
myManageHook = composeAll
  [
    className =? "Gimp" --> doFloat
  , isDialog            --> doFloat
  ]
