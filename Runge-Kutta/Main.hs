module Main where
import           Algebra
import           Graphics.Rendering.Chart.Easy
import           Graphics.Rendering.Chart.Backend.Cairo
import           Systems
import           System.Environment
import           Text.Read

stepRK4 :: Double -> System -> System
stepRK4 h (System f t0 y0) = System f (t0 + h) (y0 `add` mul (h / 6) g)
  where
    g1 = f y0
    g2 = 2 `mul` f (y0 `add` mul (h / 2) g1)
    g3 = 2 `mul` f (y0 `add` mul (h / 2) g2)
    g4 = f (y0 `add` mul h g3)
    g  = foldr1 add [g1, g2, g3, g4]

makeGraph :: (Double, Double) -> Int -> System -> [[Double]]
makeGraph interval nSteps system = coords
  where
    step   = (snd interval - fst interval) / fromIntegral nSteps
    coords = take nSteps $ getY0 <$> iterate
        (stepRK4 step)
        system { getT0 = fst interval, getY0 = systemSolution (fst interval) }

main' :: String -> Int -> IO ()
main' "compare" nSteps = toFile def "comparison.png" $ do
    layoutlr_title .= "Approximation compared to true value, after " ++ show nSteps ++ " steps"
    layoutlr_left_axis . laxis_override .= axisGridHide
    layoutlr_right_axis . laxis_override .= axisGridHide
    let bounds = (0.01, 0.49)
    let graph  = makeGraph bounds nSteps system
    let xs1     = (!! 0) <$> graph
    let ys1    = (!! 1) <$> graph
    let hs1    = (!! 2) <$> graph
    let step   = 0.001
    let trueGraph =
            systemSolution <$> [fst bounds, fst bounds + step .. snd bounds]
    let xs2 = (!! 0) <$> trueGraph
    let ys2 = (!! 1) <$> trueGraph
    let hs2 = (!! 2) <$> trueGraph
    plotLeft (line "approx y(x)" [zip xs1 ys1])
    plotLeft (line "y(x)" [zip xs2 ys2])
    plotRight (line "approx h(x)" [zip xs1 hs1])
    plotRight (line "h(x)" [zip xs2 hs2])
main' "show" nSteps = toFile def "runge.png" $ do
    layoutlr_title .= "Runge-Kutta approximation"
    layoutlr_left_axis . laxis_override .= axisGridHide
    layoutlr_right_axis . laxis_override .= axisGridHide
    setColors [opaque blue, opaque red]
    let fstHalf = makeGraph (0.01, 0.49) nSteps system
    let sndHalf = makeGraph (0.51, 0.99)
                            nSteps
                            system { getY0 = systemSolution 0.51 }
    let xs1 = (!! 0) <$> fstHalf
    let xs2 = (!! 0) <$> sndHalf
    let ys1 = (!! 1) <$> fstHalf
    let ys2 = (!! 1) <$> sndHalf
    let hs1 = (!! 2) <$> fstHalf
    let hs2 = (!! 2) <$> sndHalf
    plotLeft (line "y(x)" [zip xs1 ys1, zip xs2 ys2])
    plotRight (line "h(x)" [zip xs1 hs1, zip xs2 hs2])

main :: IO ()
main = do
    [op, num] <- getArgs
    let (Just n) = readMaybe num
    main' op n
