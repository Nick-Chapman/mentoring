-- Exploring the Discrete Fourier Transfor...
module Dft (main) where

import Text.Printf (printf)

newtype Number = N Double deriving (Num,Fractional,Floating,RealFloat,RealFrac,Real,Ord,Eq)

instance Show Number where
  show (N x) = printf "[%1.3f]" x

main :: IO ()
main = do
  --let samples :: [Number] = [1.0, 0, -1.0, 0]
  --let samples :: [Number] = [0.97, -0.25, -0.97, 0.25]
  let
    -- The function whose waveform is being analyzed
    func :: Number -> Number
    func x = cos (freq * x - phase)
      where
        freq = 1
        phase = -0.25

  let numSamples = 4

  let samples = [ func x
                | t <- [0::Int .. numSamples-1]
                , let frac = fromIntegral t / fromIntegral numSamples
                , let x = two_pi * frac
                ]

  dft samples

two_pi :: Number
two_pi = 2 * pi

dft :: [Number] -> IO ()
dft samples = do
  print ("samples",samples)

  let numSamples = length samples

  let
    freqs :: [Number]
    freqs = map fromIntegral $ take numSamples [ 0::Int .. ]

  print ("freqs",freqs)
  let results =
        [ mag
        | freq <- freqs
        , let (mag,_phase) = dftComponent freq samples
        ]
  print ("dft",results)
  pure ()

dftComponent :: Number -> [Number] -> (Number,Number)
dftComponent freq samples = do

  --print ("component-freq",freq)

  let indexes :: [Number] = map fromIntegral $ take (length samples) [ 0::Int .. ]

  let numSamples :: Number = fromIntegral (length samples)
  let cosAt4 = [ cos (two_pi * freq * n / numSamples) | n <- indexes ]
  let sinAt4 = [ sin (two_pi * freq * n / numSamples) | n <- indexes ]
  --print ("cosAt4",cosAt4)
  --print ("sinAt4",sinAt4)

  let dp1 = sum [ (a*b) | (a,b) <- zip samples cosAt4 ]
  let dp2 = sum [ (a*b) | (a,b) <- zip samples sinAt4 ]
  --print ("dp1",dp1)
  --print ("dp2",dp2)

  let mag = sqrt (dp1*dp1 + dp2*dp2)
  --print ("mag",mag)

  let magNorm = mag / numSamples
  --print ("magNorm",magNorm)

  let phase = atan2 dp2 dp1
  --print ("phase",phase)

  (magNorm,phase)
