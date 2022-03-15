{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

import Data.Default
import qualified Data.Map as M
import Data.Maybe
import Data.Monoid
import Data.Ratio ((%))
import qualified Data.Text as T
import Graphics.X11.Xinerama (getScreenInfo)
import System.Exit
import System.IO
import Text.Read
import XMonad
import XMonad.Actions.CycleWS (shiftNextScreen, swapNextScreen)
import XMonad.Actions.GridSelect
import XMonad.Actions.OnScreen
import XMonad.Config.Dmwit (outputOf)
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Layout.Renamed
import XMonad.Layout.Spacing
import XMonad.Layout.ThreeColumns
import XMonad.ManageHook
import qualified XMonad.StackSet as W
import XMonad.Util.EZConfig (additionalKeys, mkKeymap)
import XMonad.Util.NamedScratchpad
import XMonad.Util.Run (hPutStrLn, spawnPipe)

myKeys nwindows conf@(XConfig {XMonad.modMask = modm}) =
  M.fromList $
    [ ((modm .|. shiftMask, xK_q), kill),
      ((modm, xK_p), sendMessage NextLayout),
      ((modm .|. shiftMask, xK_p), sendMessage FirstLayout),
      ((modm, xK_f), sendMessage ToggleStruts),
      ((modm, xK_j), windows W.focusDown),
      ((modm, xK_k), windows W.focusUp),
      ((modm, xK_s), windows W.swapMaster),
      ((modm .|. shiftMask, xK_j), windows W.swapDown),
      ((modm .|. shiftMask, xK_k), windows W.swapUp),
      ((modm, xK_bracketleft), sendMessage Shrink),
      ((modm, xK_t), withFocused toggleFloat),
      ((modm, xK_bracketright), sendMessage Expand),
      -- Increment the number of windows in the master area
      ((modm, xK_comma), sendMessage (IncMasterN 1)),
      -- Deincrement the number of windows in the master area
      ((modm, xK_period), sendMessage (IncMasterN (-1))),
      ((modm, xK_a), goToSelected gsconfig),
      ((modm, xK_minus), namedScratchpadAction scratchpads "mingus"),
      ( (modm, xK_q),
        spawn
          "/home/esrh/.local/bin/xmonad --recompile; /home/esrh/.local/bin/xmonad --restart"
      )
    ]
      ++ ( ( case nwindows of
               1 -> genWinKeysOne conf modm
               _ -> genWinKeys conf modm 0 ++ genWinKeys conf modm 1
           )
         )
      ++ [ ((m .|. modm, key), screenWorkspace sc >>= flip whenJust (windows . f))
           | (key, sc) <- zip [xK_l, xK_h] [0 ..],
             (f, m) <- [(W.view, 0), (W.shift, shiftMask)]
         ]

-- Map 1-10 to each workspace if there’s only one monitor.
-- Map 1-5 to monitor 1 and 6-10 to monitor 2 if there are two.

genWinKeys conf modm side =
  [ ((m .|. modm, k), windows $ f i)
    | (i, k) <-
        zip
          ( ( case side of
                1 -> take
                0 -> drop
            )
              5
              (XMonad.workspaces conf)
          )
          ( case side of
              1 -> [xK_1 .. xK_5]
              0 -> [xK_6 .. xK_9] ++ [xK_0]
          ),
      (f, m) <- [(viewOnScreen side, 0), (W.shift, shiftMask)]
  ]

genWinKeysOne conf modm =
  [ ((m .|. modm, k), windows $ f i)
    | (i, k) <-
        zip
          (XMonad.workspaces conf)
          ([xK_1 .. xK_9] ++ [xK_0]),
      (f, m) <- [(W.view, 0), (W.shift, shiftMask)]
  ]

toggleFloat w =
  windows
    ( \s ->
        if M.member w (W.floating s)
          then W.sink w s
          else
            ( W.float
                w
                (W.RationalRect (1 / 6) (1 / 6) (2 / 3) (2 / 3))
                s
            )
    )

------------------------------------------------------------------------
-- Grid Select

gsconfig =
  def
    { gs_cellheight = 100,
      gs_cellwidth = 200
    }

------------------------------------------------------------------------
-- Scratchpads
scratchpads =
  []

------------------------------------------------------------------------

myMouseBindings (XConfig {XMonad.modMask = modm}) =
  M.fromList
    -- mod-button1, Set the window to floating mode and move by dragging
    [ ( (modm, button1),
        \w ->
          focus w >> mouseMoveWindow w
            >> windows W.shiftMaster
      ),
      -- mod-button2, Raise the window to the top of the stack
      ((modm, button2), \w -> focus w >> windows W.shiftMaster),
      -- mod-button3, Set the window to floating mode and resize by dragging
      ( (modm, button3),
        \w ->
          focus w >> mouseResizeWindow w
            >> windows W.shiftMaster
      )
      -- you may also bind events to the mouse scroll wheel (button4 and button5)
    ]

------------------------------------------------------------------------

myLayout =
  renamed [CutWordsLeft 1] $ spacing gapsize $ tiled ||| Full ||| threecol
  where
    tiled = Tall nmaster delta ratio
    threecol = ThreeCol nmaster delta ratio
    gapsize = 10
    nmaster = 1
    ratio = 1 / 2
    delta = 3 / 100

myManageHook =
  composeAll
    [ className =? "MPlayer" --> doFloat,
      className =? "Gimp" --> doFloat,
      resource =? "desktop_window" --> doIgnore,
      resource =? "kdesktop" --> doIgnore
    ]

myEventHook = ewmhDesktopsEventHook

myStartupHook = return ()

replace :: String -> String -> String -> String
replace _ _ "" = ""
replace pat rep s =
  if take plen s == pat
    then rep ++ (replace pat rep (drop plen s))
    else [head s] ++ (replace pat rep (tail s))
  where
    plen = length pat

replaceList =
  [ ("Firefox Developer Edition", "firefox"),
    ("Mozilla Firefox", "firefox"),
    ("GNU Emacs at shiragiku", "gnu emacs")
  ]

replaceAll s = foldl (\acc el -> replace (fst el) (snd el) acc) s replaceList

ppTitleFunc = xmobarColor "#f4f0ec" "" . shorten 60 . replaceAll

ppFunc xmproc1 xmproc2 =
  dynamicLogWithPP
  xmobarPP
  { ppOutput = \x ->
      hPutStrLn xmproc1 x >> hPutStrLn xmproc2 x,
    ppCurrent = xmobarColor "#f4f0ec" "" . wrap "[" "]",
    ppVisible = xmobarColor "#f4f0ec" "" . wrap "(" ")",
    ppHidden = xmobarColor "#c0c0c0" "" . wrap "{" "}",
    ppHiddenNoWindows = xmobarColor "#696969" "" . wrap "(" ")",
    ppTitle = ppTitleFunc,
    ppSep = "<fc=#646464> <fn=1>/</fn> </fc>",
    ppUrgent = xmobarColor "#C45500" "" . wrap "!" "!",
    ppLayout = \layout ->
      xmobarColor
      "#f4f0ec"
      ""
      ( case layout of
          "Tall" -> "[|]"
          "Full" -> "[ ]"
          "ThreeCol" -> "[|||]"
          _ -> layout
     ),
    ppOrder = \(ws : l : t : ex) -> [ws, l] ++ ex ++ [t]
  }
  
main = do
  xmproc1 <- spawnPipe "xmobar -x 0"
  xmproc2 <- spawnPipe "xmobar -x 1"

  output <- T.pack
    <$> outputOf "xrandr --listactivemonitors 2>/dev/null | awk '{print $1 $4}'"
  let nwindows = length (T.lines output) - 1

  xmonad $
    docks $
      ewmh
        def
          { focusFollowsMouse = True,
            clickJustFocuses = False,
            borderWidth = 1,
            modMask = mod1Mask,
            workspaces = ["一", "二", "三", "四", "五", "六", "七", "八", "九", "十"],
            normalBorderColor = "#646464",
            focusedBorderColor = "#fdbcb4",
            keys = myKeys nwindows,
            mouseBindings = myMouseBindings,
            layoutHook = avoidStruts myLayout,
            logHook = ppFunc xmproc1 xmproc2,
            manageHook = myManageHook <+> manageDocks,
            handleEventHook = myEventHook,
            startupHook = myStartupHook
          }
