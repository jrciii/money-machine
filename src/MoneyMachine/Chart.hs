module MoneyMachine.Chart where

{-import Graphics.Rendering.Chart.Easy
import Graphics.Rendering.Chart.Backend.Cairo
import qualified Data.Thyme.LocalTime as TH
import qualified Data.Time.LocalTime as TI
import qualified Data.Thyme.Time.Core as TC

chart prices srs = toRenderable layout
  where
    srPlot sr = plot_lines_values .~ [sr]
           $ plot_lines_style .~ solidLine 1.0 (opaque blue)
           $ def

    pricePlot = plot_lines_values .~ [prices]
              $ plot_lines_style .~ solidLine 1.0 (opaque orange)
              $ def

    layout = layout_title .~ "S/R"
           $ layout_plots .~ [toPlot pricePlot] ++ (map (toPlot . srPlot) srs)
           $ def

makeSRChart :: String -> [(TH.LocalTime,Double)] -> [Double] -> IO (PickFn ())
makeSRChart fileName prices sr =
  let unthymed = map (\(x,y) -> (TC.fromThyme x,y)) prices :: [(TI.LocalTime,Double)]
  in  do
       --putStrLn $ show $ makeSRLineValues unthymed sr
       renderableToFile def fileName $ chart unthymed $ makeSRLineValues unthymed sr

makeSRLineValues :: [(TI.LocalTime,Double)] -> [Double] -> [[(TI.LocalTime,Double)]]
makeSRLineValues prices sr = map (\y -> map (\x -> (fst x, y)) prices) sr

makeSRWindowChart :: String -> [(TI.LocalTime,Double)] -> [Double] -> IO ()
makeSRWindowChart fileName prices sr =
  do
       --putStrLn $ show $ makeSRLineValues unthymed sr
       () <$ (renderableToFile def fileName $ chart prices $ makeSRLineValues prices sr)
       -}