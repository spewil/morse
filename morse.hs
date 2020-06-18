{- stack script
 --compile
 --resolver lts-14.18
-}

module Main where

import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Builder as B
import Data.Foldable
import System.Process
import Text.Printf
import Data.List
import Control.Monad

type Pulse = Float
type Seconds = Float
type Samples = Float
type Hz = Float
type Semitones = Float
type Beats = Float

outputFilePath :: FilePath
outputFilePath = "output.bin"

volume :: Float
volume = 0.2

sampleRate :: Samples
sampleRate = 48000.0

pitchStandard :: Hz
pitchStandard = 440.0

bpm :: Beats
bpm = 80.0

beatDuration :: Seconds
beatDuration = 60.0 / bpm

-- NOTE: the formula is taken from https://pages.mtu.edu/~suits/NoteFreqCalcs.html
f :: Semitones -> Hz
f n = pitchStandard * (2 ** (1.0 / 12.0)) ** n

note :: Semitones -> Beats -> [Pulse]
note n beats = freq (f n) (beats * beatDuration)

freq :: Hz -> Seconds -> [Pulse]
freq hz duration =
  map (* volume) $ zipWith3 (\x y z -> x * y * z) release attack output
  where
    step = (hz * 2 * pi) / sampleRate

    attack :: [Pulse]
    attack = map (min 1.0) ((replicate (length output `div` 4) 0) ++ [0.0, 0.001 ..])

    release :: [Pulse]
    release = reverse $ take (length output) attack

    output :: [Pulse]
    output = map sin $ map (* step) [0.0 .. sampleRate * duration]

dot :: [Pulse]
dot = note 0 0.25

dash :: [Pulse]
dash = note 0 0.5

space :: [Pulse]
space = replicate (length dash) 0.0

save :: FilePath -> [Pulse] -> IO ()
save filePath pulses = B.writeFile filePath $ B.toLazyByteString $ fold $ map B.floatLE pulses

play :: FilePath -> IO ()
play outputFilePath = do
  _ <- runCommand $ printf "ffplay -autoexit -hide_banner -loglevel panic -showmode 1 -f f32le -ar %f %s" sampleRate outputFilePath
  return ()

playLive :: [Pulse] -> IO ()
playLive pulse = do
    save "output.bin" pulse
    play "output.bin"

incorrectInputSound :: [Pulse]
incorrectInputSound = note 5 1

morseCodes :: [(Char, [Pulse])]
morseCodes = zip alphabet (map concat
            [
              [dot, dash] -- a
            , [dash, dot, dot] -- b
            , [dash, dot, dash, dot] --c
            , [dash, dot, dot] --d
            , [dot] --e
            , [dot, dot, dash, dot] --f
            , [dash, dash, dot] --g
            , [dot, dot, dot, dot] --h
            , [dot, dot] --i
            , [dot, dash, dash, dash] --j
            , [dash, dot, dash] --k
            , [dot, dash, dot, dot] --l
            , [dash, dash] --m
            , [dash, dot] --n
            , [dash, dash, dash] --o
            , [dot, dash, dash, dot] --p
            , [dash, dash, dot, dash] --q
            , [dot, dash, dot] --r
            , [dot, dot, dot] --s
            , [dash] --t
            , [dot, dot, dash] --u
            , [dot, dot, dot, dash] --v
            , [dot, dash, dash] --w
            , [dash, dot, dot, dash] --x
            , [dash, dot, dash, dash] --y
            , [dash, dash, dot, dot] --z
            , [space] -- ' '
            ])

alphabet = ['a'..'z'] ++ [' ']

-- match input character to
charToMorse :: Char -> [Pulse]
charToMorse c
    | c `elem` alphabet = snd $ (!!0) (filter ((==c).fst) morseCodes)
    | otherwise = incorrectInputSound

-- main = interact (palindrome . filterLongLines)

stringToMorse :: [Char] -> [Pulse]
stringToMorse input = concat $ map charToMorse input

handleInput :: IO ()
handleInput = do
    input <- getLine
    when (not $ null input) $ do

        playLive $ stringToMorse input
        handleInput

main :: IO ()
main = do handleInput

-- $ ffplay -showmode 1 -f f32le -ar 48000 output.bin



