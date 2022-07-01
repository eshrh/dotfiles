import System.Environment
import XMonad.Config.Dmwit (outputOf)
import Xmobar

main :: IO ()
main = do
  batteryOut <- outputOf "cat /sys/class/power_supply/BAT0/capacity"
  let hasBattery = not $ null batteryOut

  mpdOut <- outputOf "pidof mpd"
  let hasMPD = not $ null mpdOut

  args <- getArgs
  let screen = case args of
        ["-x", n] -> read n
        _ -> 0

  xmobar $ config hasBattery hasMPD screen

config :: Bool -> Bool -> Int -> Config
config hasBattery hasMPD screen =
  defaultConfig
    { -- appearance
      font = "xft:Iosevka Meiseki Sans:size=10:antialias=true,ipagothic:style=Regular:size=10",
      bgColor = "#000000",
      fgColor = "#999999",
      position = OnScreen screen Top,
      border = BottomB,
      borderColor = "#ffffff",
      -- layout
      sepChar = "%",
      alignSep = "}{",
      template =
        "%StdinReader% " ++ batteryText
          ++ "/ %multicpu% / %memory% }{"
          ++ mpdText
          ++ "%KATL% / %date%",
      -- general behavior
      lowerOnStart = True,
      hideOnStart = False,
      allDesktops = True,
      overrideRedirect = True,
      pickBroadest = False,
      persistent = True,
      commands =
        -- weather monitor
        [ Run $
            Weather
              "KATL"
              [ "--template", "<skyCondition> / <fc=#83a598><tempC></fc>°c"]
              36000,
          Run $
            MultiCpu
              [ "--template", "cpu: <fc=#ffffff><total></fc>%"]
              10,
          Run $
            Memory
              [ "--template", "mem: <fc=#ffffff><usedratio></fc>%"
              ]
              10,
          Run $ Date "<fc=#ffffff>%F %H:%M</fc>" "date" 10,
          Run StdinReader
        ]
          ++ [ Run $
                 BatteryP
                   ["BAT0"]
                   [ "-t", "<acstatus><watts> (<left>%)",
                     "--",
                     "-O", "<fc=green>On</fc> - ",
                     "-a", "notify-send -u critical 'battery low'",
                     "-A", "3"
                   ]
                   600
               | hasBattery
             ]
          ++ [ Run $
                 MPD
                   [ "-t", "<fc=#ffffff> <artist> - <title> (<album>) <statei> </fc>",
                     "-M", "30",
                     "--",
                     "-P", ">>",
                     "-Z", "||",
                     "-S", "<<",
                     "-p", "6600",
                     "-h", "127.0.0.1"
                   ] 10
               | hasMPD
             ]
    }
  where
    batteryText = if hasBattery then "/ %battery% " else ""
    mpdText = if hasMPD then "%mpd% / " else ""