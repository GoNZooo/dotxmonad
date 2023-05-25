module Main (main) where

import Control.Category ((>>>))
import XMonad
  ( Choose,
    Default (def),
    Full (..),
    KeyMask,
    KeySym,
    Mirror (..),
    Tall (Tall),
    X,
    XConfig (..),
    mod1Mask,
    shiftMask,
    spawn,
    withFocused,
    xK_F1,
    xK_F12,
    xK_F2,
    xK_F3,
    xK_b,
    xmonad,
    (.|.),
    (|||),
  )
import XMonad.Actions.NoBorders (toggleBorder)
import XMonad.Hooks.DynamicLog (statusBar, wrap, xmobarColor, xmobarPP)
import XMonad.Hooks.ManageDocks (AvoidStruts, avoidStruts)
import XMonad.Hooks.SetWMName (setWMName)
import XMonad.Hooks.StatusBar.PP (PP (..))
import XMonad.Layout.Grid (Grid (..))
import XMonad.Layout.LayoutModifier (ModifiedLayout)
import XMonad.Layout.NoBorders (SmartBorder, smartBorders)
import XMonad.Util.EZConfig (additionalKeys)

myTerminal :: String
myTerminal = "kitty"

dunstSpawn :: String
dunstSpawn = "/usr/bin/dunst"

rateSpawn :: String
rateSpawn = "/home/gonz/bin/setrate.sh"

svorakA5Spawn :: String
svorakA5Spawn = "/home/gonz/bin/svoraka5.sh"

seqwertySpawn :: String
seqwertySpawn = "/home/gonz/bin/seqwerty.sh"

bgphoneticSpawn :: String
bgphoneticSpawn = "/home/gonz/bin/bgphonetic.sh"

myLayout :: Choose Tall (Choose (Mirror Tall) (Choose Full Grid)) a
myLayout = tiled ||| Mirror tiled ||| Full ||| Grid
  where
    tiled = Tall nmaster delta ratio
    nmaster = 1
    delta = 1 / 20
    ratio = 1 / 2

myStartupHook :: X ()
myStartupHook = do
  setWMName "LG3D"
  spawn rateSpawn
  spawn svorakA5Spawn
  spawn dunstSpawn
  spawn "notify-send 'XMonad' 'XMonad restarted'"

myConfig ::
  XConfig
    ( ModifiedLayout
        AvoidStruts
        ( ModifiedLayout
            SmartBorder
            (Choose Tall (Choose (Mirror Tall) (Choose Full Grid)))
        )
    )
myConfig =
  def
    { layoutHook = avoidStruts $ smartBorders myLayout,
      borderWidth = 2,
      normalBorderColor = "#000000",
      focusedBorderColor = "#a000a0",
      terminal = myTerminal,
      startupHook = myStartupHook
    }
    `additionalKeys` [ ((mod1Mask .|. shiftMask, xK_F1), spawn svorakA5Spawn),
                       ((mod1Mask .|. shiftMask, xK_F3), spawn seqwertySpawn),
                       ((mod1Mask .|. shiftMask, xK_F2), spawn bgphoneticSpawn),
                       ((mod1Mask .|. shiftMask, xK_F12), withFocused toggleBorder)
                     ]

order :: [String] -> [String]
order (workspaces : _layout : _title : _) = [workspaces]
order _ = []

bar :: PP
bar =
  xmobarPP
    { ppCurrent = wrap "[" "]" >>> xmobarColor "#e4dfda" "#202020",
      ppTitle = xmobarColor "#e4dfda" "",
      ppOrder = order
    }

toggleStrutsKey :: XConfig t -> (KeyMask, KeySym)
toggleStrutsKey XConfig {XMonad.modMask = modMask} = (modMask, xK_b)

main :: IO ()
main = statusBar "xmobar" bar toggleStrutsKey myConfig >>= xmonad
