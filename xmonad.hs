import XMonad hiding (Tall)
 
import XMonad.Actions.Promote
import XMonad.Actions.UpdatePointer
import XMonad.Actions.Warp
import XMonad.Actions.CycleWS
import XMonad.Actions.DynamicWorkspaces
import XMonad.Actions.GridSelect
import XMonad.Actions.RotSlaves
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.UrgencyHook
import XMonad.Layout.LayoutHints
import XMonad.Layout.HintedTile
import XMonad.Layout.NoBorders
import XMonad.Layout.Tabbed
import XMonad.Layout.Maximize
import XMonad.Layout.Dishes
import XMonad.Layout.TwoPane
import XMonad.Layout.Mosaic
import XMonad.Prompt
import XMonad.Prompt.Shell
import XMonad.Prompt.Ssh
import XMonad.Prompt.AppendFile
import XMonad.Prompt.Window
import XMonad.Util.Run (spawnPipe)
import XMonad.Util.EZConfig
import XMonad.Util.Scratchpad
 
import qualified XMonad.StackSet as W
import qualified Data.Map as M

import XMonad.Prompt.MPD
import qualified Network.MPD as MPD

import Data.Ratio
import System.IO (hPutStrLn)
import GHC.IOBase (Handle)

myWorkspaces = ["1:main ", " 2:web ", " 3:code ", " 4:logs ", " 5:ssh " ," 6:pdf ", " 7:VM ", " 8:chat ", " 9:web "] 
 
main :: IO ()
main = do
    xmobar <- spawnPipe "xmobar"
    xmonad $ withUrgencyHook NoUrgencyHook $ defaultConfig
        { normalBorderColor  = backgroundColor
        , focusedBorderColor = focusColor
        , terminal = "urxvt"
        , workspaces = myWorkspaces
        , layoutHook = avoidStruts $ myLayout
        , manageHook = manageDocks <+>  scratchpadManageHook (W.RationalRect 0.2 0.2 0.5 0.5) <+> composeOne [ isFullscreen -?> doFullFloat ] <+> (className =? "trayer" --> doIgnore) 
        , modMask = mod1Mask
        , borderWidth = 2
        , keys = \c -> myKeys c `M.union` keys defaultConfig c
        , logHook = dynamicLogWithPP (myPP xmobar)
                 >> updatePointer (Relative 1 1)
        }
        where
            myLayout = layoutHints $ avoidStruts $ smartBorders $ maximize (mosaic 2 [3,2])
                                 ||| maximize (hintedTile Tall)
                                 ||| hintedTile Wide
                                 ||| TwoPane (3/100) (1/2)
                                 ||| maximize Full
                                 ||| Dishes 2 (1/16)
                                 ||| tabbed shrinkText myTheme
            hintedTile = HintedTile nmaster delta ratio TopLeft
            nmaster = 1
            ratio   = 1/2
            delta   = 3/100
 
            myPP :: Handle -> PP
            myPP din = defaultPP
                { ppCurrent = xmobarColor lightTextColor ""
                , ppVisible = xmobarColor focusColor ""
                , ppHiddenNoWindows = xmobarColor lightBackgroundColor ""
                , ppUrgent = xmobarColor urgentColor ""
                , ppSep = " · "
                , ppWsSep = ""
                , ppTitle = xmobarColor lightTextColor ""
                , ppOutput = hPutStrLn din
                }
 
            myTheme :: Theme
            myTheme = defaultTheme
                { activeColor = lightBackgroundColor
                , inactiveColor = backgroundColor
                , urgentColor = backgroundColor
                , activeBorderColor = textColor
                , inactiveTextColor = textColor
                , urgentTextColor = textColor
                , inactiveBorderColor = lightBackgroundColor
                , urgentBorderColor = urgentColor
                , activeTextColor = lightTextColor
                , fontName = myFont
                }
 
            myXPConfig :: XPConfig
            myXPConfig = defaultXPConfig
                { font        = myFont
                , bgColor     = backgroundColor
                , fgColor     = textColor
                , fgHLight    = lightTextColor
                , bgHLight    = lightBackgroundColor
                , borderColor = lightBackgroundColor
                }
 
            myFont = "xft:DejaVu Sans:size=10"
            focusColor = "#535d6c"
            textColor = "#aaaaaa"
            lightTextColor = "#fffff0"
            backgroundColor = "#222222"
            lightBackgroundColor = "#81bef7"
            urgentColor = "#ffc000"
 
            myKeys conf@(XConfig {XMonad.modMask = modMask, workspaces = ws}) = M.fromList $
                [ ((modMask,                 xK_Return), promote)
                , ((modMask,                 xK_b), sendMessage ToggleStruts)
                , ((modMask,               xK_p), shellPrompt defaultXPConfig)
                , ((modMask .|. controlMask, xK_x), shellPrompt myXPConfig)
                , ((modMask .|. controlMask, xK_s), sshPrompt myXPConfig)
                , ((modMask,                 xK_z), warpToWindow 1 1)
                , ((modMask,                 xK_q), recompile True >> restart "xmonad" True)
                , ((modMask .|. shiftMask, xK_n), appendFilePrompt defaultXPConfig "/home/sean/.foo")
                , ((modMask .|. shiftMask, xK_s), sshPrompt defaultXPConfig)
                , ((modMask .|. controlMask, xK_x), spawn "hslock" )
                , ((modMask .|. shiftMask, xK_t), spawn "xclip -o | perl -pi -e 's:$:\n\n:g' >> ~/.foo")
                , ((modMask,               xK_Escape), toggleWS)
                , ((modMask,               xK_o), shiftNextScreen >> nextScreen)
                , ((modMask  .|. shiftMask, xK_r), scratchpadSpawnActionTerminal "urxvt -background black -foreground green +sb")
                , ((modMask, xK_g), goToSelected defaultGSConfig)
                , ((modMask, xK_x), spawnSelected defaultGSConfig ["xterm", "gvim"])
                , ((modMask, xK_a), sendMessage Taller)
                , ((modMask, xK_z), sendMessage Wider)
                , ((modMask .|. shiftMask, xK_m), addMatching MPD.withMPD defaultXPConfig [MPD.sgArtist, MPD.sgAlbum] >> return ())
                , ((modMask .|. shiftMask, xK_o), withFocused (sendMessage . maximizeRestore))

                , ((modMask .|. shiftMask, xK_g     ), windowPromptGoto defaultXPConfig { autoComplete = Just 500000 } ) 

                ] 
