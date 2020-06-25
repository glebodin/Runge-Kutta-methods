module Main where
import Graphics.Rendering.Chart.Easy
import Graphics.Rendering.Chart.Backend.Cairo

---------------------------------------------------------------------------------------------------
--We need to solve y'_{1} = f(y'_{1} .. y'_{n}) .. y'_{n} = f(y'_{1} .. y'_{n})
--Here is example of Euler's method to compare it with Runge-Kutta's one
--For example I will take y' = 1/(4(x - 0.5)(h - y)), h' = (y - h)/(x - 0.5), y_1(0) = 1, y_2(0) = 1

f1 :: Double -> [Double] -> Double
f1 x y = 1 / (4 * (x - 0.5) * (y!!1 - y!!0))

f2 :: Double -> [Double] -> Double
f2 x y = (y!!0 - y!!1) / (x - 0.5)

get :: [(Double, [Double])] -> Int -> [Double]
get [] _ = []
get a b | b >= 1 = (x!!(b - 1)) : get (tail a) b
    where (_, x) = head a
get a b | b == 0 = x : get (tail a) b
    where (x, _) = head a


update :: Double -> [Double] -> Double -> [Double]
update x y step = [(y !! 0) + step * (f1 x y), (y !! 1) + step * (f2 x y)]

solve :: Double -> [Double]-> Double -> Double -> [(Double, [Double])]
solve x y step r | x > r = []
solve x y step r = (x, y) : solve (x + step) (update x y step) step r

sol :: Double -> [Double]
sol x = [asin(sqrt(x)), sqrt(-x^2 + x) / (2 * x - 1) + asin(sqrt(x))]

main :: IO ()
main = toFile def "euler.png" $ do
    layoutlr_title .= "Euler's method"
    layoutlr_left_axis . laxis_override .= axisGridHide
    layoutlr_right_axis . laxis_override .= axisGridHide
    setColors [opaque blue, opaque red]
    let sol0 = solve 0.01 (sol 0.01) 0.001 0.49
    let sol1 = solve 0.52 (sol 0.52) 0.001 0.99
    let xs0 = get sol0 0
    let ys0 = get sol0 1
    let hs0 = get sol0 2
    let xs1 = get sol1 0
    let ys1 = get sol1 1
    let hs1 = get sol1 2
    plotLeft (line "y(x)" [zip xs0 ys0, zip xs1 ys1])
    plotRight (line "h(x)" [zip xs0 hs0, zip xs1 hs1])
