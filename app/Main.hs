{-# LANGUAGE TemplateHaskell, BlockArguments #-}
module Main where

--https://hackage.haskell.org/package/base-4.20.0.1/docs/Data-Complex.html
import Data.Complex
import GHC.Float (ceilingDouble)
import Control.Monad.State
import Control.Lens
import Raylib.Core
import Raylib.Types
import Raylib.Util
import Linear
import Raylib.Util.Colors
import Raylib.Core.Shapes
import Data.Fixed

data FourierSeries = FS { coefficients :: [Complex Double], fourierFunction :: Double -> Complex Double, deltaTime :: Double }

--https://stackoverflow.com/questions/8844674/how-to-round-to-the-nearest-whole-number-in-c-sharp
--https://www.youtube.com/watch?v=r6sGWTCMz2k&t=770s
--Another link
roundUp :: Double -> Double -> Double
roundUp input interval = (fromIntegral . ceilingDouble) (input / interval) * interval

averageAdjacent :: [Complex Double] -> [Complex Double]
averageAdjacent (x : y : ys) = ((x + y) / 2) : averageAdjacent (y : ys)
averageAdjacent _ = []

euler :: (Double -> Complex Double) -> Double -> Double -> Double -> Complex Double
euler function left right deltaTime = ((* (deltaTime :+ 0)) . sum . averageAdjacent) $ map function [left, left + deltaTime..(roundUp (right - left) deltaTime + left)]

--https://hackage.haskell.org/package/base-4.20.0.1/docs/Data-Complex.html#v:exp
--https://www.google.com/search?q=coefficient&oq=coefficient&gs_lcrp=EgZjaHJvbWUqCggAEAAYsQMYgAQyCggAEAAYsQMYgAQyCggBEAAYsQMYgAQyCggCEAAYsQMYgAQyCggDEAAYsQMYgAQyCggEEAAYsQMYgAQyDQgFEAAYgwEYsQMYgAQyCggGEAAYsQMYgAQyBwgHEAAYgAQyBwgIEAAYgAQyBwgJEAAYgATSAQkxNTY3M2owajeoAgCwAgA&sourceid=chrome&ie=UTF-8
calculateCoefficient :: (Double -> Complex Double) -> Double -> Double -> Complex Double
calculateCoefficient function frequency = euler (\x -> exp (0 :+ (-(frequency * 2 * pi * x))) * function x) 0 1

--[0, 1, -1, 2, -2...]
getFrequency :: Integer -> Double
getFrequency index
    | odd index || magnitude == 0 = magnitude
    | otherwise = -magnitude
    where
        --https://stackoverflow.com/questions/18280844/converting-integer-to-double-in-haskell#:~:text=The%20usual%20way%20to%20convert,which%20Double%20is%20an%20instance.
        --https://hackage.haskell.org/package/basement-0.0.16/docs/Basement-Floating.html#v:integerToDouble
        magnitude = fromIntegral $ (fromIntegral index + 1) `div` 2

--https://www.cis.upenn.edu/~cis1940/spring13/

getIndexFromFrequency :: Double -> Integer
getIndexFromFrequency frequency
    | frequency == 0 = 0
    | frequency > 0 = intFrequency * 2 - 1
    | otherwise = intFrequency * (-2)
    where intFrequency = floor frequency

frequencies :: [Double]
frequencies = map getFrequency [0..]

calculateCoefficients :: (Double -> Complex Double) -> Double -> FourierSeries
calculateCoefficients function deltaTime = FS (map (\x -> calculateCoefficient function x deltaTime) frequencies) function deltaTime

calculateTerm :: Complex Double -> Double -> Double -> Complex Double
calculateTerm coefficient frequency time = coefficient * exp (0 :+ (2 * pi * frequency * time))

calculateFourierSeries :: FourierSeries -> Double -> Integer -> Complex Double
calculateFourierSeries series t size = (sum . take (fromIntegral size)) $ zipWith (\x y -> calculateTerm x y t) (coefficients series) frequencies

cosineDecomposition :: (Double -> Complex Double) -> Double -> [Complex Double]
cosineDecomposition function deltaTime =  map (\x -> 2 * calculateCoefficient function x deltaTime) [0, 0.5..]

step :: Double -> Complex Double
step x
    | x < 0.5 = 1
    | x == 0.5 = 0
    | otherwise = -1

oneMinusTwoX :: Double -> Complex Double
oneMinusTwoX x = 1 - 2 * (x :+ 0)

--https://hackage.haskell.org/package/linear-1.23/docs/Linear-Vector.html#v:lerp
--https://www.youtube.com/watch?v=Y0aOxj5lrKY
--https://www.youtube.com/watch?v=xGxSTzaID3k
--https://www.youtube.com/watch?v=k8FXF1KjzY0
--https://www.youtube.com/results?search_query=square+fourier+series
--https://www.youtube.com/shorts/KPhOto0hWx4
--https://hackage.haskell.org/package/linear-1.23/docs/Linear-Vector.html#v:lerp
--https://www.youtube.com/watch?v=aVwxzDHniEw
--https://www.youtube.com/watch?v=SO83KQuuZvg&t=3059s
--https://en.wikipedia.org/wiki/Linear_interpolation
complexLerp :: Double -> Complex Double -> Complex Double -> Complex Double
complexLerp t a b = (1 - complexT) * a + complexT * b
    where complexT = t :+ 0

square :: Double -> Complex Double
square t
    | (0 <= t') && (t' < 0.25) = complexLerp t'' (1 :+ 1) ((-1) :+ 1)
    | (0.25 <= t') && (t' < 0.5) = complexLerp t'' ((-1) :+ 1) ((-1) :+ (-1))
    | (0.5 <= t') && (t' < 0.75) = complexLerp t'' ((-1) :+ (-1)) (1 :+ (-1))
    | otherwise = complexLerp t'' (1 :+ (-1)) (1 :+ 1)
    where
        t' =  t `mod'` 1
        t'' = (t `mod'` 0.25) * 4


convertComplexToVector2 :: Complex Double -> V2 Float

--https://stackoverflow.com/questions/30029461/recommended-way-to-convert-double-float-in-haskell
convertComplexToVector2 (a :+ b) = V2 (realToFrac a) (realToFrac b)

drawPoint :: Complex Double -> Double -> Color -> IO ()
drawPoint position radius = drawCircleV (convertComplexToVector2 position) (realToFrac radius)

drawLines :: [Complex Double] -> Double -> Color -> IO ()
--https://www.raylib.com/cheatsheet/cheatsheet.html
--    --https://stackoverflow.com/questions/5217171/how-can-i-get-nth-element-from-a-list
drawLines (x : y : ys) width color = drawLineEx (convertComplexToVector2 x) (convertComplexToVector2 y) (realToFrac width) color >> drawLines (y : ys) width color
drawLines _ _ _ = return ()

--Based of CIS 194
second :: (a, b) -> (b -> c) -> (a, c)
second (x, y) function = (x, function y)

--https://hackage.haskell.org/package/linear-1.23/docs/Linear-V2.html
--https://hackage.haskell.org/package/base-4.20.0.1/docs/Prelude.html#v:fst
--https://hackage.haskell.org/package/lens-5.3.2/docs/Control-Lens.html
--https://hoogle.haskell.org/?hoogle=Control.Lens
--https://hackage.haskell.org/package/OpenGL-3.0.3.0/docs/Graphics-Rendering-OpenGL-GL-Tensor.html#t:Vector2
--https://hoogle.haskell.org/?hoogle=mapM
--https://hackage.haskell.org/package/base-4.20.0.1/docs/Prelude.html#v:mapM
data App = App { _fourier :: FourierSeries, _time :: Double, _size :: Integer, _playBackRate :: Double, _pointBuffer :: [Complex Double], _currentBufferTime :: Double, _maxBufferTime :: Double, _camera :: Camera2D }

makeLenses ''App

mainLoop :: App -> IO App
mainLoop = execStateT do
    liftIO beginDrawing
    liftIO $ clearBackground white
    --https://www.reddit.com/r/raylib/comments/16e3ejg/is_this_how_you_use_deltatime_in_raylib/
    --https://stackoverflow.com/questions/30029461/recommended-way-to-convert-double-float-in-haskell
    deltaTime' <- liftIO (realToFrac <$> getFrameTime :: IO Double)
    time += deltaTime'
    use time >>= liftIO . print
    use camera >>= liftIO . beginMode2D

    t <- use time
    size' <- use size
    fourierSeries <- use fourier
    rate <- use playBackRate
    buffer <- use pointBuffer
    bufferTime <- use currentBufferTime
    maxTime <- use maxBufferTime

    currentBufferTime += deltaTime' * rate
    --https://hackage.haskell.org/package/base-4.20.0.1/docs/Prelude.html#v:mapM
    --https://stackoverflow.com/questions/5217171/how-can-i-get-nth-element-from-a-list
    --https://hackage.haskell.org/package/base-4.9.0.0/docs/Data-List.html



    let values = take (fromIntegral size') $ iterate (\x -> nextElement fourierSeries x (rate * t)) (0, getCurrentValue fourierSeries 0 0)
    let conjugatedValues = map (Data.Complex.conjugate . snd) values
    let calculatedValue = snd $ last values
    pointBuffer %= (if bufferTime >= maxTime then tail else id) . (++[calculatedValue])

    --https://www.raylib.com/cheatsheet/cheatsheet.html
    --liftIO $ mapM_ (\x -> drawPoint (Data.Complex.conjugate x) 0.03125 (Color 0 255 0 255)) buffer
    --Green point buffer
    liftIO $ drawLines (map Data.Complex.conjugate buffer) 0.03125 (Color 0 255 0 255)

    --Red points
    liftIO $ mapM_ (\x -> drawPoint x 0.0625 (Color 255 0 0 255)) (take (length values - 1) conjugatedValues)

    --Red lines
    liftIO $ drawLines conjugatedValues 0.03125 (Color 255 0 0 255)

    --Blue point
    liftIO $ drawPoint (last conjugatedValues) 0.0625 (Color 0 0 255 255)

    liftIO endDrawing
    where
        nextElement :: FourierSeries -> (Integer, Complex Double) -> Double -> (Integer, Complex Double)
        nextElement fourierSeries input time = (newIndex, snd input + getCurrentValue fourierSeries newIndex time) where
            newIndex = fst input + 1

        getCurrentValue :: FourierSeries -> Integer -> Double -> Complex Double
        getCurrentValue fourierSeries coefficientIndex = calculateTerm (coefficients fourierSeries!!fromIntegral coefficientIndex) (getFrequency coefficientIndex)

startup :: IO App
startup = do
    _ <- initWindow 1280 720 "Fourier Series"
    setTargetFPS 60
    let camera' = Camera2D (V2 640 360) zero 0 100
    return $ App (calculateCoefficients square 0.01) 0 100 0.25 [] 0 0.875 camera'

shouldClose :: App -> IO Bool
shouldClose = const windowShouldClose

tearDown :: App -> IO ()
tearDown = const $ closeWindow Nothing

--main :: IO ()
--main = putStrLn "Hello World"

--https://wiki.haskell.org/Template_Haskell
$(raylibApplication 'startup 'mainLoop 'shouldClose 'tearDown)
