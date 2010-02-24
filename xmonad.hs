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
import XMonad.Layout.Spiral
import XMonad.Layout.Tabbed
import XMonad.Layout.Maximize
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
import Data.Ratio
import System.IO (hPutStrLn)
import GHC.IOBase (Handle)

myWorkspaces = ["1:main ", " 2:web ", " 3:code ", " 4:ssh ", " 5:dls " ," 6:pdf ", " 7:video ", " 8:chat ", " 9:nine "] 
 
main :: IO ()
main = do
    xmobar <- spawnPipe "xmobar"
    xmonad $ withUrgencyHook NoUrgencyHook $ defaultConfig
        { normalBorderColor  = backgroundColor
        , focusedBorderColor = focusColor
        , terminal = "urxvt"
        , workspaces = myWorkspaces
        , layoutHook = avoidStruts $ myLayout
        , manageHook = manageDocks <+> scratchpadManageHook (W.RationalRect 0.2 0.2 0.6 0.5) <+> composeOne [ isFullscreen -?> doFullFloat ] <+> (className =? "trayer" --> doIgnore) 
        , modMask = mod1Mask
        , borderWidth = 2
        , keys = \c -> myKeys c `M.union` keys defaultConfig c
        , logHook = dynamicLogWithPP (myPP xmobar)
                 >> updatePointer (Relative 0.9 0.9)
        }
        where
            myLayout = layoutHints $ avoidStruts $ smartBorders $ maximize (hintedTile Tall)
                                 ||| mosaic 2 [3,2]
                                 ||| hintedTile Wide
                                 ||| TwoPane (3/100) (1/2)
                                 ||| maximize Full
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
                , ((modMask,                 xK_z), warpToWindow 1 1)
                , ((modMask,                 xK_q), recompile True >> restart "xmonad" True)
                , ((modMask .|. shiftMask, xK_n), appendFilePrompt defaultXPConfig "/home/sean/.foo")
                , ((modMask .|. shiftMask, xK_s), sshPrompt defaultXPConfig)
                , ((modMask,               xK_p), shellPrompt defaultXPConfig)
                , ((modMask .|. controlMask, xK_x), spawn "slock" )
                , ((modMask .|. shiftMask, xK_t), spawn "xsel | leander")
                , ((modMask,               xK_Escape), toggleWS)
                , ((modMask .|. controlMask, xK_j), nextScreen)
                , ((modMask .|. controlMask, xK_k), prevScreen)
                , ((modMask,               xK_o), shiftNextScreen >> nextScreen)
                , ((modMask  .|. shiftMask, xK_r), scratchpadSpawnActionTerminal "urxvt -tr -tint grey -foreground \'#FFFF00\' +sb")
                , ((modMask, xK_g), goToSelected defaultGSConfig)
                , ((modMask .|. shiftMask, xK_o), withFocused (sendMessage . maximizeRestore))
                , ((modMask, xK_a), sendMessage Taller)
                , ((modMask, xK_z), sendMessage Wider)
                , ((modMask, xK_r), sendMessage Reset)
                , ((modMask .|. shiftMask, xK_g     ), windowPromptGoto
                                                            defaultXPConfig { autoComplete = Just 500000 } )


                ] 
