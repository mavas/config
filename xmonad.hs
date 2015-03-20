import Data.Bits ((.|.))
import Data.Monoid
import Data.Ratio ((%))
import Data.Map (fromList)
import qualified Data.Map as M
import System.Exit
import Graphics.X11.Xlib
import Graphics.X11.Xlib.Extras
import System.IO

import XMonad
import XMonad.Actions.SwapWorkspaces
import XMonad.Actions.FloatKeys
import XMonad.Hooks.DynamicLog (xmobar)
import qualified XMonad.StackSet as W
import XMonad.Hooks.ManageDocks
import XMonad.Layout.Magnifier ( maximizeVertical, MagnifyMsg(Toggle) )
import XMonad.Actions.CopyWindow ( kill1, copy )
import XMonad.Actions.DynamicWorkspaces ( withNthWorkspace, withWorkspace,
                                          selectWorkspace, renameWorkspace, removeWorkspace )
import XMonad.Layout.ToggleLayouts ( toggleLayouts, ToggleLayout(ToggleLayout) )
import XMonad.Util.Run 
import XMonad.Util.EZConfig
import XMonad.Layout
import XMonad.Operations
import XMonad.ManageHook
import qualified XMonad.StackSet as W

--main = xmonad $ defaultConfig
--	{ terminal = "mrxvt +sb -ht -fg white -bg black" }

-- make sure to edit paths to xmobar and .xmobarrc to match your system.
    -- If xmobar is in your $PATH, and its config is in ~/.xmobarrc you don't
    -- need the xmobar path or config file, use: xmproc <- spawnPipe "xmobar"
 
main = do
    --xmonad $ defaultConfig
    xmonad =<< xmobar defaultConfig
        { manageHook = manageDocks <+> manageHook defaultConfig
        , layoutHook = avoidStruts $ layoutHook defaultConfig
        --, logHook = dynamicLogWithPP xmobarPP
        --                { ppOutput = hPutStrLn xmproc
        --                , ppTitle = xmobarColor "green" "" . shorten 50
        --                }
        , terminal = "mrxvt"
        --, modMask = mod4Mask     -- Rebind Mod to the Windows key
        , keys = myKeys <+> keys defaultConfig
        --, keys = keys defaultConfig
        }
        --} `additionalKeys`
        --[ ((mod4Mask .|. shiftMask, xK_z), spawn "xscreensaver-command -lock")
        --, ((controlMask, xK_Print), spawn "sleep 0.2; scrot -s")
        --, ((0, xK_Print), spawn "scrot")
        --]


-- Key bindings. Add, modify or remove key bindings here.
--myKeys conf@(XConfig {XMonad.modMask = modMask}) = fromList []
    -- launching and killing programs
    {-++
    -- mod-[1..9] %! Switch to workspace N
    -- mod-shift-[1..9] %! Move client to workspace N
    [((m .|. modMask, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
    ++
    -- mod-{w,e,r} %! Switch to physical/Xinerama screens 1, 2, or 3
    -- mod-shift-{w,e,r} %! Move client to screen 1, 2, or 3
    [((m .|. modMask, key), screenWorkspace sc >>= flip whenJust (windows . f))
        | (key, sc) <- zip [xK_w, xK_e, xK_r] [0..]
        , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]
    -- ++
    --[((modMask .|. controlMask, k), windows $ swapWithCurrent i)
    --    | (i, k) <- zip (XMonad.workspaces conf) [xK_1 ..]]-}
    --
myKeys :: XConfig Layout -> M.Map (KeyMask, KeySym) (X ())
myKeys x = M.fromList $
    -- launching and killing programs
    [ ((modMask x .|. shiftMask, xK_Return), spawn $ XMonad.terminal x)
    , ((modMask x .|. controlMask, xK_space), sendMessage ToggleLayout)
    , ((modMask x, xK_space), sendMessage Toggle)
    , ((modMask x,               xK_p     ), spawn "dmenu_run")
    , ((modMask x .|. shiftMask, xK_p     ), spawn "gmrun") -- %! Launch gmrun
    , ((modMask x .|. shiftMask, xK_c     ), kill) -- %! Close the focused window

    , ((modMask x,               xK_space ), sendMessage NextLayout) -- %! Rotate through the available layout algorithms
    , ((modMask x .|. shiftMask, xK_space ), setLayout $ XMonad.layoutHook x) -- %!  Reset the layouts on the current workspace to default

    , ((modMask x, xK_n), refresh) -- %! Resize viewed windows to the correct size

    -- screenshot screen
    , ((modMask x, xK_Print), spawn "/home/ubuntu/bin/screenshot scr")
    -- screenshot window or area
    , ((modMask x .|. shiftMask, xK_Print), spawn "/home/ubuntu/bin/screenshot win")

    -- move focus up or down the window stack
    , ((modMask x, xK_Tab), windows W.focusDown) -- %! Move focus to the next window
    , ((modMask x .|. shiftMask, xK_Tab), windows W.focusUp  ) -- %! Move focus to the previous window
    , ((modMask x, xK_j), windows W.focusDown) -- %! Move focus to the next window
    , ((modMask x, xK_k), windows W.focusUp  ) -- %! Move focus to the previous window
    , ((modMask x, xK_m), windows W.focusMaster  ) -- %! Move focus to the master window

    -- modifying the window order
    , ((modMask x,               xK_Return), windows W.swapMaster) -- %! Swap the focused window and the master window
    , ((modMask x .|. shiftMask, xK_j     ), windows W.swapDown  ) -- %! Swap the focused window with the next window
    , ((modMask x .|. shiftMask, xK_k     ), windows W.swapUp    ) -- %! Swap the focused window with the previous window

    -- resizing the master/slave ratio
    , ((modMask x,               xK_h     ), sendMessage Shrink) -- %! Shrink the master area
    , ((modMask x,               xK_l     ), sendMessage Expand) -- %! Expand the master area

    -- floating layer support
    , ((modMask x,               xK_t     ), withFocused $ windows . W.sink) -- %! Push window back into tiling

    , ((modMask x,               xK_d     ), withFocused (keysResizeWindow (-10,-10) (1,1)))
    , ((modMask x,               xK_s     ), withFocused (keysResizeWindow (10,10) (1,1)))
    , ((modMask x .|. shiftMask, xK_d     ), withFocused (keysAbsResizeWindow (-10,-10) (1024,752)))
    , ((modMask x .|. shiftMask, xK_s     ), withFocused (keysAbsResizeWindow (10,10) (1024,752)))
    , ((modMask x,               xK_a     ), withFocused (keysMoveWindowTo (512,384) (1 % 2,1 % 2)))

    -- increase or decrease number of windows in the master area
    , ((modMask x              , xK_comma ), sendMessage (IncMasterN 1)) -- %! Increment the number of windows in the master area
    , ((modMask x              , xK_period), sendMessage (IncMasterN (-1))) -- %! Deincrement the number of windows in the master area

    -- toggle the status bar gap
    --, ((modMask              , xK_b     ), modifyGap (\i n -> let x = (XMonad.defaultGaps conf ++ repeat (0,0,0,0)) !! i in if n == x then (0,0,0,0) else x)) -- %! Toggle the status bar gap

    -- quit, or restart
    , ((modMask x .|. shiftMask, xK_q     ), io (exitWith ExitSuccess)) -- %! Quit xmonad
    , ((modMask x              , xK_q     ), spawn "if type xmonad; then xmonad --recompile && xmonad --restart; else xmessage xmonad not in \\$PATH: \"$PATH\"; fi") -- %! Restart xmonad

    ]

    ++
    zip (zip (repeat $ modMask x) [xK_F1..xK_F12]) (map (withNthWorkspace W.greedyView) [0..])
    ++
    zip (zip (repeat (modMask x .|. shiftMask)) [xK_F1..xK_F12]) (map (withNthWorkspace copy) [0..])
