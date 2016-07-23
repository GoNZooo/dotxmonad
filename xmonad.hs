import XMonad
import XMonad.Hooks.ManageDocks
import XMonad.Layout.NoBorders
import XMonad.Actions.NoBorders
import XMonad.Util.EZConfig
import XMonad.Hooks.SetWMName

-- For xmobar
import XMonad.Hooks.DynamicLog

myTerminal :: String
myTerminal = "st"

rateSpawn :: String
rateSpawn = "/home/gonz/bin/setrate.sh"

svorakA5Spawn :: String
svorakA5Spawn = "/home/gonz/bin/svoraka5.sh"

seqwertySpawn :: String
seqwertySpawn = "/home/gonz/bin/seqwerty.sh"

bgphoneticSpawn :: String
bgphoneticSpawn = "/home/gonz/bin/bgphonetic.sh"

setWallpaperSpawn :: String
setWallpaperSpawn = "/home/gonz/bin/set-wallpaper.sh"

myLayout = tiled ||| Mirror tiled ||| Full
  where
        tiled = Tall nmaster delta ratio
        nmaster = 1
        delta = 1/20
        ratio = 1/2

myStartupHook :: X ()
myStartupHook = do
    setWMName "LG3D"
    spawn rateSpawn
    spawn svorakA5Spawn
    spawn setWallpaperSpawn

myConfig = defaultConfig
   { layoutHook = avoidStruts $ smartBorders $ myLayout,
      borderWidth = 2,
      normalBorderColor  = "#000000", -- black
      focusedBorderColor = "#ff3f3f",
      terminal = myTerminal,
      startupHook = myStartupHook
    } `additionalKeys`
    [((mod1Mask .|. shiftMask, xK_F1), spawn svorakA5Spawn),
     ((mod1Mask .|. shiftMask, xK_F3), spawn seqwertySpawn),
     ((mod1Mask .|. shiftMask, xK_F2), spawn bgphoneticSpawn),
     ((mod1Mask .|. shiftMask, xK_F12), withFocused toggleBorder)]

main = xmonad $ myConfig
