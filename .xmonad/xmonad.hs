{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use lambda-case" #-}

import Control.Monad (ap, forM_, (<=<))
import Data.Bifunctor
import Data.Char (isSpace)
import Data.Default
import Data.List
import qualified Data.Map as M
import Data.Tree
import GHC.IO.Handle
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
import XMonad.Layout.Renamed
import XMonad.Layout.Spacing
import XMonad.Prompt
import XMonad.Prompt.Shell
import qualified XMonad.StackSet as W
import XMonad.Util.EZConfig (additionalKeysP)
import XMonad.Util.NamedScratchpad
import XMonad.Util.NamedWindows
import XMonad.Util.Run

commandKeys :: [(String, X ())]
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
    ("M-S-p", sendMessage FirstLayout),
    ("M-S-h", windows W.swapDown),
    ("M-S-t", windows W.swapUp),
    ("M-s", windows W.swapMaster),
    -- Prompts
    ("M-/", scratchpadCloseOrPrompt),
    ("M-<Space>", shellPrompt promptConf),
    -- Audio
    ("C-M-a", spawn "pactl set-sink-volume @DEFAULT_SINK@ -10%"),
    ("C-M-o", spawn "pactl set-sink-volume @DEFAULT_SINK@ +10%"),
    ("C-M-x", spawn "playerctl play-pause"),
    ("C-M-w", spawn "mpc toggle"),
    ("C-M-b", spawn "playerctl previous"),
    ("C-M-m", spawn "playerctl next"),
    -- Application commands
    ("M-S-q", spawn "xmonad --recompile && xmonad --restart"),
    ("M-<Return>", spawn "alacritty"),
    ("M-S-u", spawn "firefox"),
    ("M-S-y", spawn "thunderbird"),
    ("M-S-,", spawn "ames -w"),
    ("M-S-.", spawn "ames -r"),
    ("M-<Escape>", spawn "i3lock"),
    ("M-S-s", spawn "scrot -s")
  ]

type WindowAction = Window -> X ()

type KeyBind = (KeyMask, KeySym)

floatDimensions :: W.RationalRect
floatDimensions = W.RationalRect (1 / 6) (1 / 6) (2 / 3) (2 / 3)

toggleFloat :: WindowAction
toggleFloat w =
  windows
    ( \s ->
        if M.member w (W.floating s)
          then W.sink w s
          else W.float w floatDimensions s
    )

type StackOp i l a s sd = i -> W.StackSet i l a s sd -> W.StackSet i l a s sd

type KeyMap = [(KeyBind, X ())]

keyGen ::
  (Ord a, Eq s, Eq i) =>
  KeyMask ->
  (StackOp i l a s sd -> x -> X ()) ->
  [(KeySym, x)] ->
  StackOp i l a s sd ->
  KeyMap
keyGen modm func objects action =
  [ ((m .|. modm, k), func f i)
    | (k, i) <- objects,
      (f, m) <- [(action, 0), (W.shift, shiftMask)]
  ]

windowKeys :: Int -> Bool -> XConfig m -> M.Map KeyBind (X ())
windowKeys nwindows flipped conf@(XConfig {XMonad.modMask = modm}) =
  M.fromList $
    ( if nwindows == 1
        then genWinKeysOne conf modm
        else genWinKeys conf modm 0 flipped ++ genWinKeys conf modm 1 flipped
    )
      ++ keyGen modm applyScreenFunction objects W.view
  where
    applyScreenFunction f sc = screenWorkspace sc >>= flip whenJust (windows . f)
    objects = zip ((if flipped then id else reverse) [xK_d, xK_n]) [0, 1]

-- Map 1-10 to each workspace if there’s only one monitor.
-- Map 1-5 to monitor 1 and 6-10 to monitor 2 if there are two.
genWinKeys :: XConfig k -> KeyMask -> ScreenId -> Bool -> KeyMap
genWinKeys conf modm side flipped =
  keyGen
    modm
    (\f i -> windows $ f i)
    (zip (pick keys) (pick wksp))
    (viewOnScreen side)
  where
    keys = splitAt 5 ([xK_1 .. xK_9] ++ [xK_0])
    wksp = splitAt 5 (XMonad.workspaces conf)
    pick = if (side == 1) == flipped then snd else fst

genWinKeysOne :: XConfig x -> KeyMask -> KeyMap
genWinKeysOne conf modm =
  keyGen
    modm
    (\f i -> windows $ f i)
    (zip ([xK_1 .. xK_9] ++ [xK_0]) (XMonad.workspaces conf))
    W.view

------------------------------------------------------------------------
-- Scratchpads

scratchpadLayout :: ManageHook
scratchpadLayout =
  customFloating $
    W.RationalRect (1 / 6) (1 / 6) (2 / 3) (2 / 3)

mkScratchpadFromTerm :: String -> NamedScratchpad
mkScratchpadFromTerm name =
  NS
    name
    ("alacritty --title '" ++ name ++ "' -e " ++ name)
    (title =? name)
    scratchpadLayout

mkScratchpadFromProgram :: String -> String -> NamedScratchpad
mkScratchpadFromProgram name binary =
  NS name binary (titleContainsString name) scratchpadLayout

scratchpads :: [NamedScratchpad]
scratchpads =
  map mkScratchpadFromTerm ["htop", "ncmpcpp", "pulsemixer"]
    ++ [mkScratchpadFromProgram "qBittorrent v4.4.3.1" "qbittorrent"]

titleContainsString :: String -> Query Bool
titleContainsString s = title >>= (\x -> return (s `isInfixOf` x))

scratchpadNames :: [String]
scratchpadNames = map (\(NS n _ _ _) -> n) scratchpads

toggleScratchpad :: String -> X ()
toggleScratchpad = namedScratchpadAction scratchpads

data Scratch = Scratch

instance XPrompt Scratch where
  showXPrompt Scratch = "scratchpad: "

scratchPrompt :: XPConfig -> X ()
scratchPrompt c =
  mkXPrompt
    Scratch
    c
    (mkComplFunFromList' scratchpadNames)
    toggleScratchpad

findM :: Monad m => (a -> m Bool) -> [a] -> m (Maybe a)
findM p =
  foldr
    ( \el acc ->
        p el >>= (\x -> if x then return $ Just el else acc)
    )
    (return Nothing)

windowIsScratchpad :: Window -> X Bool
windowIsScratchpad = return . (`elem` scratchpadNames) . show <=< getName

toggleFoundScratchpad :: Window -> X ()
toggleFoundScratchpad w = getName w >>= toggleScratchpad . show

scratchpadCloseOrPrompt :: X ()
scratchpadCloseOrPrompt = do
  win <- gets (W.index . windowset)
  floating <- gets (W.floating . windowset)
  let floats = filter (`M.member` floating) win
  found <- findM windowIsScratchpad floats
  maybe (scratchPrompt promptConf) toggleFoundScratchpad found

------------------------------------------------------------------------
-- Shell prompt config

-- not quite satisfied with built ins.
xpkeymap :: (Char -> Bool) -> M.Map KeyBind (XP ())
xpkeymap p =
  M.fromList $
    map
      (first $ (,) controlMask)
      [ (xK_z, killBefore),
        (xK_k, killAfter),
        (xK_a, startOfLine),
        (xK_e, endOfLine),
        (xK_y, pasteString),
        (xK_f, moveCursor Next),
        (xK_b, moveCursor Prev),
        (xK_p, moveHistory W.focusUp'),
        (xK_n, moveHistory W.focusDown'),
        (xK_w, killWord' p Prev),
        (xK_g, quit)
      ]
      ++ map
        (first $ (,) 0)
        [ (xK_Return, setSuccess True >> setDone True),
          (xK_KP_Enter, setSuccess True >> setDone True),
          (xK_Escape, quit),
          (xK_BackSpace, deleteString Prev)
        ]

promptConf :: XPConfig
promptConf =
  def
    { font = "xft:Iosevka Meiseki Sans",
      alwaysHighlight = True,
      height = 30,
      promptBorderWidth = 0,
      historySize = 256,
      promptKeymap = xpkeymap isSpace,
      maxComplRows = Just 3,
      autoComplete = Just 0
    }

------------------------------------------------------------------------
-- Mouse gestures

toggleFullscreen :: X ()
toggleFullscreen =
  sendMessage (ModifyWindowBorderEnabled not)
    >> sendMessage ToggleStruts

gestures :: M.Map [Direction2D] WindowAction
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

mouseControls :: XConfig m -> M.Map (ButtonMask, Button) WindowAction
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

tsconf :: TS.TSConfig a
tsconf =
  def
    { TS.ts_background = 0xc0c0c0c0,
      TS.ts_node = (0xff000000, 0xc0c0c0c0),
      TS.ts_nodealt = (0xff000000, 0xc0c0c0c0),
      TS.ts_highlight = (0xffffffff, 0xff000000),
      TS.ts_extra = 0xff000000,
      TS.ts_node_width = 40,
      TS.ts_navigate = tsnav
    }

------------------------------------------------------------------------

layout =
  renamed [CutWordsLeft 1] $ spacing 10 $ tiled ||| Full
  where
    tiled = Tall 1 (3 / 100) (1 / 2)

floatingWindowManageHook :: ManageHook
floatingWindowManageHook =
  composeAll
    [className =? "Gimp" --> doFloat]

replace :: String -> String -> String -> String
replace _ _ "" = ""
replace pat rep s =
  if take plen s == pat
    then rep ++ replace pat rep (drop plen s)
    else head s : replace pat rep (tail s)
  where
    plen = length pat

replaceList :: [(String, String)]
replaceList =
  [ ("Firefox Developer Edition", "firefox"),
    ("Mozilla Firefox", "firefox"),
    ("GNU Emacs", "emacs"),
    (" at ", " @ ")
  ]

replaceAll :: String -> String
replaceAll s = foldl (flip (uncurry replace)) s replaceList

fg :: String -> (String -> String)
fg color = xmobarColor color ""

stdColor :: String -> String
stdColor = fg "#ffffff"

focusColor = stdColor

ppTitleFunc :: String -> String
ppTitleFunc = stdColor . shorten 60 . replaceAll

-- pretty print layout names
layoutDispatch :: String -> String
layoutDispatch layout = case layout of
  "Tall" -> "[|]"
  "Full" -> "[ ]"
  _ -> layout

ppFunc :: [Handle] -> X ()
ppFunc xmhandles =
  do
    (dynamicLogWithPP . namedScratchpadFilterOutWorkspacePP)
      xmobarPP
        { ppOutput = \x -> mapM_ (`hPutStrLn` x) xmhandles,
          ppCurrent = focusColor . wrap "[" "]",
          ppVisible = focusColor . wrap "(" ")",
          ppHidden = fg "#999999" . wrap "{" "}",
          ppHiddenNoWindows = fg "#666666" . wrap "(" ")",
          ppTitle = ppTitleFunc,
          ppSep = "<fc=#ffffff> <fn=1>/</fn> </fc>",
          ppUrgent = fg "#ffffff" . wrap "!" "!",
          ppLayout = xmobarColor "#ffffff" "" . layoutDispatch,
          ppOrder = \(ws : l : t : ex) -> [ws, l] ++ ex ++ [t]
        }

trims :: String -> String
trims = dropWhileEnd isSpace . dropWhile isSpace

main :: IO ()
main = do
  output <-
    outputOf
      "xrandr --listactivemonitors 2>/dev/null | awk '{print $1 $4}'"

  -- parse into [(index, name)]
  let monitors =
        map
          (Data.Bifunctor.second tail . span (/= ':'))
          ((tail . lines) output)

  -- spawn an xmobar for every screen
  xmhandles <- mapM (\(i, _) -> spawnPipe ("xmobar -x " ++ i)) monitors

  let flippedkeys = True
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
            logHook = ppFunc xmhandles,
            manageHook =
              floatingWindowManageHook
                <+> manageDocks
                <+> namedScratchpadManageHook scratchpads,
            handleEventHook = ewmhDesktopsEventHook,
            startupHook = return ()
          }
        `additionalKeysP` commandKeys
