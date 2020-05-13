module Main where
import Graphics.Rendering.Chart.Easy
import Graphics.Rendering.Chart.Backend.Cairo

---------------------------------------------------------------------------------------------------
--We need to solve y'_{1} = f(y'_{1} .. y'_{n}) .. y'_{n} = f(y'_{1} .. y'_{n})
--Here is example of Euler's method to compare it with Runge-Kutta's one
--For example I will take y'_{1} = 1, y_2' = y_2 / y_1, y_1(0) = 1, y_2(0) = 1

f1 :: (Double, [Double]) -> Double
f1 (x, y) = 1

f2 :: (Double, [Double]) -> Double
f2 (x, y) = (y !! 1) / (y !! 0)

update :: (Double, [Double], Double) -> [Double]
update (x, y, step) = [(y !! 0) + step * f1(x, y), (y !! 1) + step * f2(x, y)]

solve :: (Double, [Double], Double, Double) -> [(Double, [Double])]
solve (x, y, step, r) | x > r = []
solve (x, y, step, r) = (x, y) : solve(x + step, update(x, y, step), step, r)

main :: IO ()
main = do
    step <- read <$> getLine ::IO Double
    len <- read <$> getLine ::IO Double
    print (solve(x, y, step, x + len))
    	where x = 0
    	      y = [1, 1]
    layout_title .= "Euler"
    setColors [opaque red]
    plot (line "y(x)" [take (20 * 10) $ zip xs ys])
        where [(xs : ys : other)]
