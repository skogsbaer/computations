module Control.Computations.Utils.Ansi (
  reset,
  bold,
  ansiReverse,
  black,
  blue,
  green,
  cyan,
  red,
  purple,
  brown,
  gray,
  darkGray,
  lightBlue,
  lightGreen,
  lightCyan,
  lightRed,
  lightPurple,
  yellow,
  white,
) where

reset :: String
reset = "\o33[0;0m"

bold :: String
bold = "\o33[1m"

ansiReverse :: String
ansiReverse = "\o33[2m"

black :: String
black = "\o33[0;30m"

blue :: String
blue = "\o33[0;34m"

green :: String
green = "\o33[0;32m"

cyan :: String
cyan = "\o33[0;36m"

red :: String
red = "\o33[0;31m"

purple :: String
purple = "\o33[0;35m"

brown :: String
brown = "\o33[0;33m"

gray :: String
gray = "\o33[0;37m"

darkGray :: String
darkGray = "\o33[1;30m"

lightBlue :: String
lightBlue = "\o33[1;34m"

lightGreen :: String
lightGreen = "\o33[1;32m"

lightCyan :: String
lightCyan = "\o33[1;36m"

lightRed :: String
lightRed = "\o33[1;31m"

lightPurple :: String
lightPurple = "\o33[1;35m"

yellow :: String
yellow = "\o33[1;33m"

white :: String
white = "\o33[1;37m"
