module Systems
    ( System(..)
    , systemSolution
    , system
    , system2
    , system3
    )
where

data System = System { getF :: [Double] -> [Double],
                       getT0 :: Double,
                       getY0 :: [Double]}

systemSolution :: Double -> [Double]
systemSolution t = [x, y, h] <*> [t]
  where
    x t = t
    y t = asin (sqrt t)
    h t = sqrt (t - t ** 2) / (2 * t - 1) + asin (sqrt t)

system :: System
system = System f 0.05 (systemSolution 0.05)
  where
    x' _ = 1
    y' [x, y, h] = 1 / (4 * (x - 0.5) * (h - y))
    h' [x, y, h] = (y - h) / (x - 0.5)
    f l = [x', y', h'] <*> [l]

system2 :: System
system2 = System y' 0 [0, 1]
  where
    y1' [y1, y2] = 1
    y2' [y1, y2] = y2 / y1
    y' y = [y1', y2'] <*> [y]

system3 :: System
system3 = System y' 0 [1, 1]
  where
    y1' [y1, y2] = 1
    y2' [y1, y2] = y2
    y' y = [y1', y2'] <*> [y]
