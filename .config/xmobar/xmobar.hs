{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

import System.Environment
import XMonad.Config.Dmwit (outputOf)
import Xmobar
import GHC.Generics

import Data.Aeson

import Data.Scientific
import Text.Printf (printf)

import qualified Data.Text as T
import Data.Text (Text)

import Data.Map (Map)
import qualified Data.Map as M

import qualified Data.ByteString.Lazy as B
import Network.HTTP.Conduit (simpleHttp)

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
      font = "Iosevka Meiseki Sans 10",
      additionalFonts = ["IPAGothic 10"],
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
          ++ "/ %multicpu% / %memory% /}{"
          ++ mpdText
          ++ "%ws% / %date%",
      -- general behavior
      lowerOnStart = True,
      hideOnStart = False,
      allDesktops = True,
      overrideRedirect = True,
      pickBroadest = False,
      persistent = True,
      commands =
        -- weather monitor
        [ Run WeatherStem,
          Run $
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
          Run $ Date "<fc=#ffffff>%b-%d %H:%M</fc>" "date" 10,
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

data WeatherData = WeatherData
  { value :: Value,
    sensor_name :: T.Text
  }
  deriving (Show, Generic)

instance FromJSON WeatherData

newtype Records = Records {records :: [WeatherData]}
  deriving (Show, Generic)

instance FromJSON Records

getJSON :: IO B.ByteString
getJSON =
  simpleHttp
    "http://cdn.weatherstem.com/dashboard/data/dynamic/model/gatech/stadium/latest.json"

transformWeather' :: Records -> Map String Double
transformWeather' r =
  M.fromList $
    map
      ( \x ->
          ( (T.unpack . sensor_name) x,
            case value x of
              Number n -> toRealFloat n
              _ -> 0
          )
      )
      sensors
  where
    sensors =
      filter
        ( \x -> case value x of
            Number n -> True
            _ -> False
        )
        $ records r

transformWeather :: Maybe Records -> Map String Double
transformWeather = maybe M.empty transformWeather'

getData :: IO (Map String Double)
getData = do
  j <- getJSON
  return $ transformWeather (decode j :: Maybe Records)

tempPrinter :: Double -> String
tempPrinter f = printf "%.2g°c" ((f - 32) / 1.8)

windSpeedPrinter :: Double -> String
windSpeedPrinter x = printf "%.2g m/s" (0.44704 * x)

dataLookup :: String -> (Double -> String) -> IO String
dataLookup k f = maybe "" f . M.lookup k <$> getData

weatherStemOutput :: IO String
weatherStemOutput = do
  temp <- dataLookup "Thermometer" tempPrinter
  hum <- dataLookup "Hygrometer" show
  wind <- dataLookup "Anemometer" windSpeedPrinter
  return $ temp ++ " / " ++ hum ++ "% / " ++ wind

data WeatherStem = WeatherStem
  deriving (Read, Show)

instance Exec WeatherStem where
  alias WeatherStem = "ws"
  run WeatherStem = weatherStemOutput
  rate WeatherStem = 36000
