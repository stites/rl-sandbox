{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
import Graphics.Rendering.Chart.Easy
import Graphics.Rendering.Chart.Backend.Diagrams
import System.Random.MWC

import Data.Monoid
import Text.Blaze (ToMarkup(..), Markup)
import Text.Blaze.Html5 hiding (html, param, map, main)
import Text.Blaze.Html.Renderer.Text (renderHtml)
import qualified Text.Blaze.Html5 as H hiding (main)
import qualified Text.Blaze.Html5.Attributes as A
import qualified Data.Text.Lazy as T
import qualified Data.Text.IO as TIO


import Classifiers.QLearning

import Sentenai.Prelude

main :: IO ()
main = do
  gen <- createSystemRandom
  let config = simpleBanditConfig
  (history, optimal) <- simpleBandits gen config
  TIO.writeFile (target ++ "report.html") $ T.toStrict . renderHtml $
    mkReport
      [ "rewards.svg"
      , "10-trial-rolling-reward.svg"
      , "action-scatter.svg"
      -- , "action-fill.svg"
      -- TODO: include a "% off-target" chart
      , "params.svg"
      ]
  let rs = rewards history
      as = actions history

  toFile def (target ++ "rewards.svg") $ do
    layout_title .= ("10-Arm Bandit Rewards")
    setColors [opaque red]
    layout_x_axis . laxis_title .= "Trial Number"
    layout_y_axis . laxis_title .= "Reward Value"
    plot (line "rewards" $ [zip [(0 :: Int)..] rs])

  toFile def (target ++ "10-trial-rolling-reward.svg") $ do
    layout_title .= ("10-Trial Rolling Average Reward Per Action")
    setColors [opaque blue]
    layout_x_axis . laxis_title .= "Trial Number"
    let averages = take (length rs - 10) . map ((/10) . sum . take 10) . tails
    plot (line "rewards" $ [zip [(10 :: Int)..] (averages rs)])

  toFile def (target ++ "action-scatter.svg") $ do
    layout_title .= ("10-Arm Bandit Choices (Optimal:" ++ show optimal ++ ")" )
    setColors [opaque blue]
    layout_x_axis . laxis_title .= "Trial Number"
    layout_y_axis . laxis_title .= "Action Chosen"
    plot (points "arm" $ zip [(0 :: Double)..] as)

  toFile def (target ++ "params.svg") $ do
    layout_title .= "Q-learning Parameters"
    setColors [opaque red, opaque blue, opaque green]
    layout_x_axis . laxis_title .= "Trial Number"
    let lightIdx = [0 .. fromIntegral $ trials config]
        eps = (runInterval . epsilon) config
        a = (runInterval . alpha) config
        epsFn = epsilonFn config
        g = (runInterval . gamma) config

    plot (line "alpha"   $ [map (, a) lightIdx])
    plot (line "epsilon" $ [scanr (plotEps epsFn) (0, eps) lightIdx])
    plot (line "gamma"   $ [map (, g) lightIdx])


  where
    target = "out/"

    actions :: Seq (Action, Reward) -> [Double]
    actions = toList . map (fromIntegral . fst)

    rewards :: Seq (Action, Reward) -> [Double]
    rewards = toList . map (float2Double . snd)

    plotEps :: (Interval -> Integer -> Interval)
            -> Integer -> (Integer, Float) -> (Integer, Float)
    plotEps fn x (_, y') = (x, runInterval $ fn (toInterval y') x)




mkReport :: [H.AttributeValue] -> Html
mkReport elts  = H.html $ do
  H.head . H.title $ "Report"
  H.link ! A.rel "stylesheet" ! A.href bootstrapCss
  H.body $ do
    (H.div ! A.class_ "container") $ makeRows elts
  (H.script ! A.src bootstrapJs) (toHtml ' ')
  where
    bootstrapCss :: AttributeValue
    bootstrapCss = "https://maxcdn.bootstrapcdn.com/bootstrap/4.0.0-alpha.5/css/bootstrap.min.css"

    bootstrapJs :: AttributeValue
    bootstrapJs = "https://maxcdn.bootstrapcdn.com/bootstrap/4.0.0-alpha.5/js/bootstrap.min.js"

    makeRows :: [AttributeValue] -> Markup
    makeRows [] = return ()
    makeRows elts =
      let
        (row, next) = splitAt 2 elts
      in
        do
          (H.div ! A.class_ "row") $
            mapM_ ((H.div ! A.class_ "col-sm-6") . (H.img ! A.width "100%" ! A.height "auto" !) . A.src) row
          makeRows next






