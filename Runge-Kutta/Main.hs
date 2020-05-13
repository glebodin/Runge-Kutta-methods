module Main where
import Graphics.Rendering.Chart.Easy
import Graphics.Rendering.Chart.Backend.Cairo

class VectorField f where
    add :: Num a => f a -> f a -> f a
    dot :: Num a => f a -> f a -> a
    mul :: Num a => a -> f a -> f a

instance VectorField [] where
    add = zipWith (+)
    dot a = sum . zipWith (*) a
    mul x = fmap (*x)

data System = System { getF :: [Double] -> [Double],
                       getT0 :: Double,
                       getY0 :: [Double]}

stepRK4 :: Double -> System -> System
stepRK4 h (System f t0 y0) = System f (t0 + h) (y0 `add` mul (h/6) g)
    where
        g1 = f y0
        g2 = 2 `mul` f (y0 `add` mul (h/2) g1)
        g3 = 2 `mul` f (y0 `add` mul (h/2) g2)
        g4 = f (y0 `add` mul h g3)
        g = foldr1 add [g1, g2, g3, g4]

system :: System
system = System f 0 (systemSolution 0.01)
    where
        x' _ = 1
        y' [x, y, h] = 1 / (4 * (x - 0.5) * (h - y))
        h' [x, y, h] = (y - h) / (x - 0.5)
        f y = ($y) <$> [x', y', h']

systemSolution :: Double -> [Double]
systemSolution t = ($t) <$> [x, y, h]
    where
        x t = t
        y t = asin (sqrt t)
        h t = sqrt (t - t ** 2) / (2 * t - 1) + asin (sqrt t)

system2 :: System
system2 = System y' 0 [0, 1]
    where
        y1' [y1,y2] = 1
        y2' [y1,y2] = y2/y1
        y' y = ($y) <$> [y1', y2']

system3 :: System
system3 = System y' 0 [0, 1]
    where
        y1' [y1,y2] = 1
        y2' [y1,y2] = y2
        y' y = ($y) <$> [y1', y2']

makeGraph :: (Double, Double) -> Int -> System -> [[Double]]
makeGraph interval nSteps system = coords
    where
        step = (snd interval - fst interval) / fromIntegral nSteps
        coords = take nSteps $ getY0 <$> iterate (stepRK4 step) system

main :: IO ()
main = toFile def "runge.png" $ do
    layoutlr_title .= "Runge-Kutta approximation"
    layoutlr_left_axis . laxis_override .= axisGridHide
    layoutlr_right_axis . laxis_override .= axisGridHide
    setColors [opaque blue, opaque red]
    let fstHalf = makeGraph (0.01, 0.49) 1000 system
    let sndHalf = makeGraph (0.51, 0.99) 1000 system {getY0 = systemSolution 0.51}
    let xs1 = head <$> fstHalf
    let xs2 = head <$> sndHalf
    let ys1 = (!!1) <$> fstHalf
    let ys2 = (!!1) <$> sndHalf
    let hs1 = (!!2) <$> fstHalf
    let hs2 = (!!2) <$> sndHalf
    plotLeft (line "y(x)" [zip xs1 ys1, zip xs2 ys2])
    plotRight (line "h(x)" [zip xs1 hs1, zip xs2 hs2])
