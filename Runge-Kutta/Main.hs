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
        g2 = f (y0 `add` mul (h/2) g1)
        g3 = f (y0 `add` mul (h/2) g2)
        g4 = f (y0 `add` mul h g3)
        g = foldr1 add [g1, g2, g3, g4]

system :: [Double] -> [Double]
system [x, y, h] = [x', y', h']
    where
        x' = 1
        y' = 1 / (4 * (x - 0.5) * (h - y))
        h' = (y - h) / (x - 0.5)

system2 :: System
system2 = System y' 0 [0, 1]
    where
        y1' [y1,y2] = 1
        y2' [y1,y2] = y2/y1
        y' y = ($y) <$> [y1', y2']

system3 :: System
system3 = System y' 0 [1, 1]
    where
        y1' [y1,y2] = 1
        y2' [y1,y2] = y2
        y' y = ($y) <$> [y1', y2']

main :: IO ()
main = toFile def "runge.png" $ do
    layout_title .= "Runge-Kutta approximation"
    setColors [opaque blue]
    let ys = (!!1) . getY0 <$> iterate (stepRK4 0.05) system3
    let xs = iterate (+0.05) (getT0 system2)
    plot (line "y(x)" [take (20 * 5) $ zip xs ys])

