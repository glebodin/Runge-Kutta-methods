module Main where
--We need to solve y' = f(x, y), for example we take y' = x^2 - 2y
--You need to enter x, y, such as f(x) = y, step, r

f :: (Double, Double) -> Double
f (x, y) = x^2 - 2 * y

solve :: (Double, Double, Double, Double) -> [(Double, Double)]
solve (x, y, step, r) | x > r = []
solve (x, y, step, r) = (x, y) : solve(x + step, y + step * f(x, y), step, r)

main :: IO ()
main = do
    x <- read <$> getLine ::IO Double
    y <- read <$> getLine ::IO Double
    step <- read <$> getLine ::IO Double
    r <- read <$> getLine ::IO Double
    print (solve(x, y, step, r))
