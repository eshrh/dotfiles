{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

import Control.Monad (forM_, join)
import Data.Bifunctor
import Data.Default
import Data.Function (on)
import Data.List
import qualified Data.Map as M
import Data.Maybe
import Data.Monoid
import Data.Ratio ((%))
import qualified Data.Text as T
import Data.Char (isSpace)
import Data.Tree
import Graphics.X11.ExtraTypes.XF86 (xF86XK_AudioLowerVolume, xF86XK_AudioMute, xF86XK_AudioRaiseVolume)
import Graphics.X11.Xinerama (getScreenInfo)
import System.Exit
import System.IO
import Text.Read
import XMonad
import XMonad.Actions.CycleWS
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
import XMonad.Util.NamedWindows (getName)
import XMonad.Util.Run (hPutStrLn, safeSpawn, spawnPipe)
import XMonad.Util.EZConfig

commandKeys =
  [ ("M-<Tab>", nextMatch Forward isOnAnyVisibleWS),
    ("M-p", sendMessage NextLayout),
    ("M-g", toggleFullscreen),
    ("M-h", windows W.focusDown),
    ("M-t", windows W.focusUp),
    ("M-q", kill),
    ("M-[", sendMessage Shrink),
    ("M-]", sendMessage Expand),
    ("M-f", withFocused toggleFloat),
    ("M-.", sendMessage (IncMasterN 1)),
    ("M-,", sendMessage (IncMasterN (-1))),
    ("M-u", TS.treeselectWorkspace tsconf jpWorkspaces W.greedyView),
    ("M-a", goToSelected gsconfig),
    ("M-S-p", sendMessage FirstLayout),
    ("M-S-h", windows W.swapDown),
    ("M-S-t", windows W.swapUp),
    ("M-s", windows W.swapMaster)
  ]
  ++
  [ ("M-S-q", spawn "xmonad --recompile && xmonad --restart"),
    ("<XF86AudioLowerVolume>", spawn "pactl set-sink-volume @DEFAULT_SINK@ -10%"),
    ("<XF86AudioRaiseVolume>", spawn "pactl set-sink-volume @DEFAULT_SINK@ +10%"),
    ("M-<Return>", spawn "alacritty"),
    ("M-<Space>", spawn "rofi -show run -matching prefix"),
    ("M-S-u", spawn "firefox"),
    ("M-S-y", spawn "thunderbird"),
    ("M-S-b", spawn "ames -w"),
    ("M-S-m", spawn "ames -r"),
    ("M-<Escape>", spawn "i3lock")
  ]
  
toggleFloat w =
  windows
    ( \s ->
        if M.member w (W.floating s)
          then W.sink w s
          else W.float w (W.RationalRect (1 / 6) (1 / 6) (2 / 3) (2 / 3)) s
    )

windowKeys nwindows flipped conf@(XConfig {XMonad.modMask = modm}) =
  M.fromList $
  ( case nwindows of
             1 -> genWinKeysOne conf modm
             _ -> genWinKeys conf modm 0 flipped ++ genWinKeys conf modm 1 flipped
  )
  ++ [ ( (m .|. modm, key),
         screenWorkspace sc
         >>= flip whenJust (windows . f)
       )
     | (key, sc) <- zip (flipf [xK_d, xK_n]) [0..],
       (f, m) <- [(W.view, 0), (W.shift, shiftMask)]
     ]
  where
    flipf = if flipped then reverse else id

-- Map 1-10 to each workspace if there’s only one monitor.
-- Map 1-5 to monitor 1 and 6-10 to monitor 2 if there are two.
genWinKeys conf modm side flipped =
  [ ((m .|. modm, k), windows $ f i)
    | (i, k) <- zip (pick wksp) (pick keys),
      (f, m) <- [(viewOnScreen side, 0), (W.shift, shiftMask)]
  ]
  where
    keys = splitAt 5 ([xK_1 .. xK_9] ++ [xK_0])
    wksp = splitAt 5 (XMonad.workspaces conf)
    pick = if (side == 1) == flipped then fst else snd

genWinKeysOne conf modm =
  [ ((m .|. modm, k), windows $ f i)
    | (i, k) <-
        zip
          (XMonad.workspaces conf)
          ([xK_1 .. xK_9] ++ [xK_0]),
      (f, m) <- [(W.view, 0), (W.shift, shiftMask)]
  ]

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

mouseControls (XConfig {XMonad.modMask = modm}) =
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

jpWorkspaces :: Forest String
jpWorkspaces =
  map
    (`Node` [])
    ["一", "二", "三", "四", "五", "六", "七", "八", "九", "十"]

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

layout =
  renamed [CutWordsLeft 1] $ spacing 10 $ tiled ||| Full
  where
    tiled = Tall nmaster delta ratio
    nmaster = 1
    ratio = 1 / 2
    delta = 3 / 100

floatingWindowManageHook =
  composeAll
    [className =? "Gimp" --> doFloat]

myStartupHook = return ()

ppFunc = do
  winset <- gets windowset
  let ld = description . W.layout . W.workspace . W.current $ winset

  let pplayout = case ld of
        "Tall" -> "[|]"
        "Full" -> "[ ]"
        _ -> ld
  io $ appendFile "/tmp/.xmonad-layout-log" (pplayout ++ "\n")

trims = dropWhileEnd isSpace . dropWhile isSpace
main = do
  safeSpawn "mkfifo" ["/tmp/.xmonad-layout-log"]

  output <-
    outputOf
      "xrandr --listactivemonitors 2>/dev/null | awk '{print $1 $4}'"

  let monitors =
        map
          (Data.Bifunctor.second tail . span (/= ':'))
          ((tail . lines) output)
  forM_ monitors $
    \(id, name) -> spawnPipe ("MONITOR=" ++ name ++ " polybar mainbar0")

  hostname <- outputOf "hostname"

  let flippedkeys = case trims hostname of
        "suisen" -> True
        _ -> False

  xmonad $
    docks $
      ewmh
        def
          { focusFollowsMouse = True,
            clickJustFocuses = False,
            borderWidth = 1,
            modMask = mod1Mask,
            workspaces = TS.toWorkspaces jpWorkspaces,
            normalBorderColor = "#646464",
            focusedBorderColor = "#fdbcb4",
            keys = windowKeys (length monitors) flippedkeys,
            mouseBindings = mouseControls,
            layoutHook = avoidStruts layout,
            logHook = ppFunc,
            manageHook = floatingWindowManageHook <+> manageDocks,
            handleEventHook = ewmhDesktopsEventHook,
            startupHook = myStartupHook
          } `additionalKeysP` commandKeys
