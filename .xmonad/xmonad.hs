import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeys)
import Graphics.X11.ExtraTypes.XF86
import System.IO

main = do
  xmproc <- spawnPipe "~/.cabal/bin/xmobar ~/.xmobarrc"
  xmonad $ def
    { modMask = mod4Mask -- right-alt so we can still emacs
    , manageHook = manageDocks <+> manageHook def -- ignore docks
    , layoutHook = avoidStruts  $  layoutHook def
    , startupHook = startup
    , logHook = dynamicLogWithPP xmobarPP
      { ppOutput =  hPutStrLn xmproc  
      , ppTitle = xmobarColor "blue" "" . shorten 0   
      , ppLayout = const "" -- to disable the layout info on xmobar
      }
    } `additionalKeys`
    [ ((mod4Mask, xK_z), spawn "slock")
    , ((mod4Mask, xK_b), sendMessage ToggleStruts)
    , ((mod4Mask, xK_q), spawn "sudo killall nm-applet trayer" >> restart "xmonad" True)
    , ((0, xF86XK_AudioRaiseVolume), spawn "pactl set-sink-volume 1 +1%; pactl set-sink-volume 2 +1%")
    , ((0, xF86XK_AudioLowerVolume), spawn "pactl set-sink-volume 1 -1%; pactl set-sink-volume 2 -1%") 
    , ((0, xF86XK_AudioMute), spawn "pactl set-sink-mute 1 toggle; pactl set-sink-mute 2 toggle")
    , ((0, xF86XK_MonBrightnessUp), spawn "xbacklight -steps 3 -inc 5")
    , ((0, xF86XK_MonBrightnessDown), spawn "xbacklight -steps 3 -dec 5")
    , ((mod4Mask, xK_0), spawn "xset dpms force off")
    ]

startup :: X()
startup = do
  spawn "trayer --edge bottom --align right --SetDockType true --SetPartialStrut true --expand true --widthtype percent --width 100 --height 25"
  spawn "nm-applet"
