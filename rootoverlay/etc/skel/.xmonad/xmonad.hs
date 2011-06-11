import XMonad
import XMonad.Hooks.DynamicLog

import XMonad.Hooks.ManageDocks
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeys)
import System.IO (Handle, hPutStrLn, hGetContents)
import qualified XMonad.StackSet as W
import Dzen
import XMonad.Hooks.DynamicLog hiding (dzen)

-- layouts
import XMonad.Layout
import XMonad.Layout.ToggleLayouts
import XMonad.Layout.ResizableTile
import Data.Ratio ((%))
import XMonad.Layout.Gaps

-- Font
myFont = "cure"

-- Colors
-- Need to simplify these
--
myBgBgColor          = "black"
myFgColor            = "white"
myBgColor            = "gray20"
myHighlightedFgColor = "white"
myHighlightedBgColor = "gray40"

myActiveBorderColor   = "gray80"
myInactiveBorderColor = "gray20"

myCurrentWsFgColor     = "white"
myCurrentWsBgColor     = "gray40"
myVisibleWsFgColor     = "gray80"
myVisibleWsBgColor     = "gray20"
myHiddenWsFgColor      = "gray80"
myHiddenEmptyWsFgColor = "gray50"
myUrgentWsBgColor      = "brown"
myTitleFgColor         = "white"

myUrgencyHintFgColor   = "white"
myUrgencyHintBgColor   = "brown"

-- layout definitions
customLayout = avoidStruts $ ResizableTall 2 (3/100) (1/2) [] ||| Full

-- Image Based Workspaces
myWorkspaces = [ wrapBitmap "arch_10x10.xbm"
	       , wrapBitmap "fox.xbm"
	       , wrapBitmap "dish.xbm"
	       , wrapBitmap "cat.xbm"
	       , wrapBitmap "empty.xbm"
	       , wrapBitmap "shroom.xbm"
	       , wrapBitmap "bug_02.xbm"
	       , wrapBitmap "eye_l.xbm"
	       , wrapBitmap "eye_r.xbm"
	       ]

wrapBitmap :: String -> String
wrapBitmap bitmap = "^p(5)^i(" ++ myBitmapsDir ++ bitmap ++ ")^p(5)"

-- Number / Label Based Workspaces
-- myWorkspaces = ["term","www"] ++ map show [3..9]

-- Window rules
myManageHook = composeAll
	[ className =? "wicd-client" --> doFloat
        , className =? "Namoroka Preferences" --> doFloat
	]

-- icons directory
myBitmapsDir = "/home/pptmstr/.dzen/"

-- main config
main :: IO ()
main = do
    d <- spawnDzen myLeftBar

    spawnToDzen "conky -c /home/pptmstr/.conkyrc" myRightBar
--    spawn "xcompmgr"  
    xmonad $ defaultConfig
        { manageHook         = myManageHook 
        , terminal           = "urxvtc"
        , workspaces         = myWorkspaces
        , borderWidth        = 0
        , normalBorderColor  = myInactiveBorderColor
        , focusedBorderColor = myActiveBorderColor
        , layoutHook         = customLayout
        , logHook            = myLogHook d
        , modMask            = mod4Mask -- Rebind Mod to the Windows key
        } `additionalKeys` myKeys

-- Extra Keys 
myKeys :: [((KeyMask, KeySym), X ())]
myKeys = [ ((controlMask, xK_Print)       , spawn "sleep 0.5; scrot -s")
         , ((0, xK_Print)                 , spawn "scrot")
	 , ((mod4Mask, xK_a)              , sendMessage MirrorShrink)
	 , ((mod4Mask, xK_z)              , sendMessage MirrorExpand)
	 ]
-- Dual screen change from greedyView to view         
	++  
	 [((m .|. mod4Mask, k), windows $ f i)
        | (i, k) <- zip (myWorkspaces) [xK_1 .. xK_9]
        , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]
	 
-- Status Bar Config
myLeftBar :: DzenConf
myLeftBar = defaultDzen
	{ width    = Just $ Percent 50
	, height   = Just 16
	, font	   = Just myFont
	, fg_color = Just myFgColor
	, bg_color = Just myBgColor
	}

myRightBar :: DzenConf
myRightBar = myLeftBar
	{ x_position = Just $ Percent 50
        , width      = Just $ Percent 50
	, alignment  = Just RightAlign
	}


-- Dzen Config
myLogHook h = dynamicLogWithPP $ defaultPP
    { ppOutput          = hPutStrLn h
    , ppSep             = wrapBg myBgBgColor "^r(1,15)"
    , ppWsSep           = ""
    , ppCurrent         = dzenColor myCurrentWsFgColor myCurrentWsBgColor
    , ppVisible         = dzenColor myVisibleWsFgColor myVisibleWsBgColor
    , ppHidden          = wrapFg myHiddenWsFgColor
    , ppHiddenNoWindows = const "" 
    , ppUrgent          = wrapBg myUrgentWsBgColor
    , ppTitle           = (" " ++) . wrapFg myTitleFgColor 
    , ppLayout          = wrapFg "" . (\x -> 
        case x of
            "ResizableTall" -> wrapBitmap "/resizableGrid.xbm"
            "Full"          -> wrapBitmap "/full.xbm"
            _               -> x)
  }
  where
    wrapFg fg = dzenColor fg ""
    wrapBg bg = dzenColor "" bg

