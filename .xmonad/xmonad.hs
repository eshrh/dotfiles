{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

import Data.Default
import qualified Data.Map as M
import Data.Maybe
import Data.Monoid
import Data.Ratio ((%))
import qualified Data.Text as T
import Data.Tree
import Graphics.X11.Xinerama (getScreenInfo)
import System.Exit
import System.IO
import Text.Read
import XMonad
import XMonad.Actions.CycleWS (shiftNextScreen, shiftPrevScreen, shiftToNext, shiftToPrev, swapNextScreen)
import XMonad.Actions.GridSelect
import XMonad.Actions.GroupNavigation
import XMonad.Actions.MouseGestures
import XMonad.Actions.OnScreen
import qualified XMonad.Actions.TreeSelect as TS
import XMonad.Config.Dmwit (outputOf)
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.WorkspaceHistory
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
    [ ((modm, xK_Tab), nextMatch Forward isOnAnyVisibleWS),
      ((modm, xK_p), sendMessage NextLayout),
      ((modm, xK_g), toggleFullscreen),
      ((modm, xK_h), windows W.focusDown),
      ((modm, xK_t), windows W.focusUp),
      ((modm, xK_q), kill),
      ((modm, xK_bracketleft), sendMessage Shrink),
      ((modm, xK_bracketright), sendMessage Expand),
      ((modm, xK_f), withFocused toggleFloat),
      ((modm, xK_period), sendMessage (IncMasterN 1)),
      ((modm, xK_comma), sendMessage (IncMasterN (-1))),
      ((modm, xK_u), TS.treeselectWorkspace tsconf myWorkspaces W.greedyView),
      ((modm, xK_a), goToSelected gsconfig),
      ((modm .|. shiftMask, xK_p), sendMessage FirstLayout),
      ((modm .|. shiftMask, xK_h), windows W.swapDown),
      ((modm .|. shiftMask, xK_t), windows W.swapUp),
      ((modm, xK_s), windows W.swapMaster),
      ( (modm .|. shiftMask, xK_q),
        spawn
          "/home/esrh/.local/bin/xmonad --recompile; /home/esrh/.local/bin/xmonad --restart"
      )
    ]
      ++ ( case nwindows of
             1 -> genWinKeysOne conf modm
             _ -> genWinKeys conf modm 0 ++ genWinKeys conf modm 1
         )
      ++ [ ((m .|. modm, key), screenWorkspace sc >>= flip whenJust (windows . f))
           | (key, sc) <- zip [xK_n, xK_d] [0 ..],
             (f, m) <- [(W.view, 0), (W.shift, shiftMask)]
         ]
      ++ spawnerKeys modm

spawnerKeys modm =
  [ ((modm, xK_Return), spawn "alacritty"),
    ((modm, xK_space), spawn "rofi -show run -matching prefix"),
    ((modm .|. shiftMask, xK_u), spawn "firefox"),
    ((modm .|. shiftMask, xK_y), spawn "thunderbird")
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
          else W.float w (W.RationalRect (1 / 6) (1 / 6) (2 / 3) (2 / 3)) s
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
-- Mouse gestures

toggleFullscreen =
  do
    sendMessage (ModifyWindowBorderEnabled not)
    sendMessage ToggleStruts

gestures =
  M.fromList
    [ ([], focus),
      ([R], \w -> focus w >> shiftNextScreen),
      ([L], \w -> focus w >> shiftPrevScreen),
      ([D], toggleFloat),
      ([U, L], const shiftToPrev),
      ([U, R], const shiftToNext),
      ([U], const toggleFullscreen)
    ]

myMouseBindings (XConfig {XMonad.modMask = modm}) =
  M.fromList
    -- mod-button1, Set the window to floating mode and move by dragging
    [ ( (modm, button1),
        \w ->
          focus w >> mouseMoveWindow w
            >> windows W.shiftMaster
      ),
      ( (modm, button2),
        \w ->
          focus w >> mouseResizeWindow w
            >> windows W.shiftMaster
      ),
      ((modm, 3), mouseGesture gestures)
    ]

myWorkspaces :: Forest String
myWorkspaces = map (`Node` []) ["一", "二", "三", "四", "五", "六", "七", "八", "九", "十"]

tsnav =
  M.fromList
    [ ((0, xK_Escape), TS.cancel),
      ((0, xK_Return), TS.select),
      ((controlMask, xK_p), TS.movePrev),
      ((controlMask, xK_n), TS.moveNext),
      ((0, xK_d), TS.moveParent),
      ((0, xK_n), TS.moveChild),
      ((0, xK_h), TS.moveNext),
      ((0, xK_t), TS.movePrev)
    ]

tsconf =
  TS.TSConfig
    { TS.ts_hidechildren = True,
      TS.ts_background = 0xc0c0c0c0,
      TS.ts_font = "xft:Sans-16",
      TS.ts_node = (0xff000000, 0xc0c0c0c0),
      TS.ts_nodealt = (0xff000000, 0xc0c0c0c0),
      TS.ts_highlight = (0xffffffff, 0xff000000),
      TS.ts_extra = 0xff000000,
      TS.ts_node_width = 40,
      TS.ts_node_height = 30,
      TS.ts_originX = 0,
      TS.ts_originY = 0,
      TS.ts_indent = 80,
      TS.ts_navigate = tsnav
    }

------------------------------------------------------------------------

myLayout =
  renamed [CutWordsLeft 1] $ spacing 10 $ tiled ||| Full ||| threecol
  where
    tiled = Tall nmaster delta ratio
    threecol = ThreeCol nmaster delta ratio
    nmaster = 1
    ratio = 1 / 2
    delta = 3 / 100

myManageHook =
  composeAll
    [ className =? "Gimp" --> doFloat,
      resource =? "desktop_window" --> doIgnore,
      resource =? "kdesktop" --> doIgnore
    ]

myEventHook = ewmhDesktopsEventHook

myStartupHook = return ()

replace :: String -> String -> String -> String
replace _ _ "" = ""
replace pat rep s =
  if take plen s == pat
    then rep ++ replace pat rep (drop plen s)
    else head s : replace pat rep (tail s)
  where
    plen = length pat

replaceList =
  [ ("Firefox Developer Edition", "firefox"),
    ("Mozilla Firefox", "firefox"),
    ("GNU Emacs", "emacs"),
    (" at ", " @ ")
  ]

replaceAll s = foldl (flip (uncurry replace)) s replaceList

textcolor = "#3c3836"

ppTitleFunc = xmobarColor textcolor "" . shorten 60 . replaceAll

ppFunc xmproc1 xmproc2 cap =
  dynamicLogWithPP
    xmobarPP
      { ppOutput = \x ->
          hPutStrLn xmproc1 x >> hPutStrLn xmproc2 x,
        ppCurrent = xmobarColor "#1d2021" "" . wrap "[" "]",
        ppVisible = xmobarColor "#1d2021" "" . wrap "(" ")",
        ppHidden = xmobarColor textcolor "" . wrap "{" "}",
        ppHiddenNoWindows = xmobarColor "#a89984" "" . wrap "(" ")",
        ppTitle = ppTitleFunc,
        ppSep = "<fc=#1d2021> <fn=1>/</fn> </fc>",
        ppUrgent = xmobarColor "#fb4934" "" . wrap "!" "!",
        ppLayout = \layout ->
          xmobarColor
            "#1d2021"
            ""
            ( case layout of
                "Tall" -> "[|]"
                "Full" -> "[ ]"
                "ThreeCol" -> "[|||]"
                _ -> layout
            ),
        ppOrder = \(ws : l : t : ex) ->
          case length capacity of
            0 -> [ws, l] ++ ex ++ [t]
            _ -> [ws, l] ++ ex ++ [t] ++ ["bat: " ++ capacity]
      }
  where
    capacity = tail (init (show cap :: String))

main = do
  xmproc1 <- spawnPipe "xmobar -x 0"
  xmproc2 <- spawnPipe "xmobar -x 1"

  output <-
    T.pack
      <$> outputOf "xrandr --listactivemonitors 2>/dev/null | awk '{print $1 $4}'"
  let nwindows = length (T.lines output) - 1

  capacity <- T.pack <$> outputOf "cat /sys/class/power_supply/BAT0/capacity"

  xmonad $
    docks $
      ewmh
        def
          { focusFollowsMouse = True,
            clickJustFocuses = False,
            borderWidth = 1,
            modMask = mod1Mask,
            workspaces = TS.toWorkspaces myWorkspaces,
            -- workspaces = ["一", "二", "三", "四", "五", "六", "七", "八", "九", "十"],
            normalBorderColor = "#646464",
            focusedBorderColor = "#fdbcb4",
            keys = myKeys nwindows,
            mouseBindings = myMouseBindings,
            layoutHook = avoidStruts myLayout,
            logHook = ppFunc xmproc1 xmproc2 capacity,
            manageHook = myManageHook <+> manageDocks,
            handleEventHook = myEventHook,
            startupHook = myStartupHook
          }
