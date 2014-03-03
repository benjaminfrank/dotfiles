import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ICCCMFocus
import XMonad.Hooks.SetWMName

myNormalBorderColor  = "#7a7a7a"
myFocusedBorderColor = "#000000"

-- don't forget to follow http://youtrack.jetbrains.com/issue/IDEA-101072

main = do
  xmonad =<< xmobar defaultConfig
            {
	      terminal = "urxvt",
              startupHook = setWMName "LG3D",
              logHook = takeTopFocus,
              normalBorderColor  = myNormalBorderColor,
              focusedBorderColor = myFocusedBorderColor
	    }
