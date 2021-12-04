{-# LANGUAGE NoMonomorphismRestriction #-}

import Data.Default
import qualified Data.Map as M
import Data.Monoid
import System.Exit
import System.IO
import XMonad
import XMonad.Actions.CycleWS (shiftNextScreen, swapNextScreen)
import XMonad.Actions.GridSelect
import XMonad.Hooks.DynamicLog 
import XMonad.Hooks.ManageDocks
import XMonad.ManageHook
import qualified XMonad.StackSet as W
import XMonad.Util.EZConfig (additionalKeys, mkKeymap)
import XMonad.Util.NamedScratchpad
import XMonad.Util.Run (hPutStrLn, spawnPipe)
import XMonad.Actions.OnScreen
myTerminal = ""

myFocusFollowsMouse :: Bool
myFocusFollowsMouse = True

myClickJustFocuses :: Bool
myClickJustFocuses = False

myBorderWidth = 1

myModMask = mod1Mask

myWorkspaces = ["一", "二", "三", "四", "五", "六", "七", "八", "九", "十"]

myFocusedBorderColor = "#f4f0ec"

myNormalBorderColor = "#646464"

------------------------------------------------------------------------

myKeys conf@(XConfig {XMonad.modMask = modm}) =
  M.fromList $
    [ ((modm .|. shiftMask, xK_q), kill),
      ((modm, xK_p), sendMessage NextLayout),
      ((modm .|. shiftMask, xK_p), setLayout $ XMonad.layoutHook conf),
      -- Resize viewed windows to the correct size
      ((modm, xK_n), refresh),
      ((modm, xK_Tab), windows W.focusDown),
      ((modm, xK_j), windows W.focusDown),
      ((modm, xK_k), windows W.focusUp),
      ((modm, xK_m), windows W.focusMaster),
      ((modm, xK_s), windows W.swapMaster),
      ((modm .|. shiftMask, xK_j), windows W.swapDown),
      ((modm .|. shiftMask, xK_k), windows W.swapUp),
      ((modm, xK_bracketleft), sendMessage Shrink),
      ((modm, xK_bracketright), sendMessage Expand),
      ((modm, xK_t), withFocused $ windows . W.sink),
      -- Increment the number of windows in the master area
      ((modm, xK_comma), sendMessage (IncMasterN 1)),
      -- Deincrement the number of windows in the master area
      ((modm, xK_period), sendMessage (IncMasterN (-1))),
      -- , ((modm              , xK_f     ), sendMessage ToggleStruts)

      ((modm, xK_o), shiftNextScreen),
      ((modm .|. shiftMask, xK_o), swapNextScreen),
      ((modm, xK_g), goToSelected gsconfig),
      ((modm, xK_minus), namedScratchpadAction scratchpads "mingus"),
      ((modm, xK_q), spawn "/home/esrh/.local/bin/xmonad --recompile; /home/esrh/.local/bin/xmonad --restart")
    ]
      ++ genKeys conf modm 0 ++ genKeys conf modm 1
      ++ [ ((m .|. modm, key), screenWorkspace sc >>= flip whenJust (windows . f))
           | (key, sc) <- zip [xK_l, xK_h] [0 ..],
             (f, m) <- [(W.view, 0), (W.shift, shiftMask)]
         ]

genKeys conf modm side = [ ((m .|. modm, k), windows $ f i)
                         | (i, k) <- zip ((case side of
                                             1 -> take
                                             0 -> drop) 5 (XMonad.workspaces conf))
                                     (case side of
                                         1 -> [xK_1 .. xK_5]
                                         0 -> [xK_6 .. xK_9] ++ [xK_0]),
                           (f, m) <- [(viewOnScreen side, 0), (W.shift, shiftMask)]
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
  [ NS
      "mingus"
      "emacsclient -c -e '(mingus)'"
      (title =? "emacs: *Mingus*" <||> title =? "emacs: *Mingus Browser*")
      defaultFloating
      --(customFloating $ W.RationalRect (1/6) (1/6) (2/3) (2/3))
  ]

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
-- Layouts:

-- You can specify and transform your layouts by modifying these values.
-- If you change layout bindings be sure to use 'mod-shift-space' after
-- restarting (with 'mod-q') to reset your layout state to the new
-- defaults, as xmonad preserves your old layout settings by default.
--
-- The available layouts.  Note that each layout is separated by |||,
-- which denotes layout choice.
--
myLayout = tiled ||| Mirror tiled ||| Full
  where
    -- default tiling algorithm partitions the screen into two panes
    tiled = Tall nmaster delta ratio

    -- The default number of windows in the master pane
    nmaster = 1

    -- Default proportion of screen occupied by master pane
    ratio = 1 / 2

    -- Percent of screen to increment by when resizing panes
    delta = 3 / 100

------------------------------------------------------------------------
-- Window rules:

-- Execute arbitrary actions and WindowSet manipulations when managing
-- a new window. You can use this to, for example, always float a
-- particular program, or have a client always appear on a particular
-- workspace.
--
-- To find the property name associated with a program, use
-- > xprop | grep WM_CLASS
-- and click on the client you're interested in.
--
-- To match on the WM_NAME, you can use 'title' in the same way that
-- 'className' and 'resource' are used below.
--
myManageHook =
  composeAll
    [ className =? "MPlayer" --> doFloat,
      className =? "Gimp" --> doFloat,
      resource =? "desktop_window" --> doIgnore,
      resource =? "kdesktop" --> doIgnore
    ]

------------------------------------------------------------------------
-- Event handling

-- * EwmhDesktops users should change this to ewmhDesktopsEventHook

--
-- Defines a custom handler function for X Events. The function should
-- return (All True) if the default handler is to be run afterwards. To
-- combine event hooks use mappend or mconcat from Data.Monoid.
--
myEventHook = mempty

------------------------------------------------------------------------
-- Status bars and logging

-- Perform an arbitrary action on each internal state change or X event.
-- See the 'XMonad.Hooks.DynamicLog' extension for examples.
--

          --         ppOutput = \x ->
          --           hPutStrLn xmproc1 x -- xmobar on monitor 1
          --             >> hPutStrLn xmproc2 x, -- xmobar on monitor 2
------------------------------------------------------------------------
-- Startup hook

-- Perform an arbitrary action each time xmonad starts or is restarted
-- with mod-q.  Used by, e.g., XMonad.Layout.PerWorkspace to initialize
-- per-workspace layout choices.
--
-- By default, do nothing.
myStartupHook = return ()

------------------------------------------------------------------------
-- Now run xmonad with all the defaults we set up.

-- Run xmonad with the settings you specify. No need to modify this.
--
main = do
  xmproc1 <- spawnPipe "xmobar -x 0"
  xmproc2 <- spawnPipe "xmobar -x 1"
  xmonad $
    docks
      def
        { -- simple stuff
          terminal = myTerminal,
          focusFollowsMouse = myFocusFollowsMouse,
          clickJustFocuses = myClickJustFocuses,
          borderWidth = myBorderWidth,
          modMask = myModMask,
          workspaces = myWorkspaces,
          normalBorderColor = myNormalBorderColor,
          focusedBorderColor = myFocusedBorderColor,
          -- key bindings
          keys = myKeys,
          mouseBindings = myMouseBindings,
          -- hooks, layouts
          layoutHook = avoidStruts myLayout,
          logHook =
            dynamicLogWithPP
              xmobarPP
                { -- the following variables beginning with 'pp' are settings for xmobar.
                  ppOutput = \x ->
                    hPutStrLn xmproc1 x -- xmobar on monitor 1
                      >> hPutStrLn xmproc2 x, -- xmobar on monitor 2
                  ppCurrent = xmobarColor "#f4f0ec" "" . wrap "[" "]", -- Current workspace
                  ppVisible = xmobarColor "#f4f0ec" "" . wrap "(" ")", -- Visible but not current workspace
                  ppHidden = xmobarColor "#c0c0c0" "" . wrap "{" "}", -- Hidden workspaces
                  ppHiddenNoWindows = xmobarColor "#696969" "" . wrap "(" ")", -- Hidden workspaces 
                  ppTitle = xmobarColor "#f4f0ec" "" . shorten 60, -- Title of active window
                  ppSep = "<fc=#646464> <fn=1>/</fn> </fc>", -- Separator character
                  ppUrgent = xmobarColor "#C45500" "" . wrap "!" "!", -- Urgent workspace
                  ppLayout = \layout -> xmobarColor "#f4f0ec" "" (case layout of
                                                                    "Tall" -> "[|]"
                                                                    "Mirror Tall" -> "[-]"
                                                                    "Full" -> "[ ]"),
                  ppOrder = \(ws : l : t : ex) -> [ws, l] ++ ex ++ [t] 
                },
          manageHook = myManageHook,
          handleEventHook = myEventHook,

         startupHook = myStartupHook
        }

