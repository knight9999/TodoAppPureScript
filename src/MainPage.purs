module MainPage
  ( component
  ) where
    
import Prelude
import Data.Maybe (Maybe(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Core as HC
import Halogen.HTML.Properties as HP

data Action = Init

type State = Int

component :: forall q i o m. H.Component HH.HTML q i o m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval
      { handleAction = handleAction
      , initialize = Just Init
      }
    }

initialState :: forall i. i -> State
initialState _ = 1

render :: forall m. State -> H.ComponentHTML Action () m
render state = do
  HH.div_
    [ HH.h1
      [ HP.attr (HC.AttrName "id") "app"] 
      [ HH.text "チュートリアルのToDoリスト" ]
    ]

handleAction :: forall m o. Action -> H.HalogenM State Action () o m Unit
handleAction = case _ of
  Init -> do
    pure unit