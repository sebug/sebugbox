module Main where

import Debug.Trace

import qualified Thermite as T
import qualified Thermite.Html as T
import qualified Thermite.Html.Elements as T
import qualified Thermite.Html.Attributes as A
import qualified Thermite.Action as T
import qualified Thermite.Events as T
import qualified Thermite.Types as T

type State = { counter :: Number }

data Action = Increment | Decrement

spec :: T.Spec (T.Action _ State) State Unit Action
spec = T.simpleSpec initialState performAction render
         # T.componentWillMount Increment

render :: T.Render State Unit Action
render ctx st _ = T.div' [ T.p' [ T.text (show st.counter) ]
                         , T.button [ T.onClick ctx (const Increment) ] [ T.text "Increment" ]
                         , T.button [ T.onClick ctx (const Decrement) ] [ T.text "Decrement" ]
                         ]

performAction :: T.PerformAction Unit Action (T.Action _ State)
performAction _ Increment = T.modifyState \st -> { counter: st.counter + 1 }
performAction _ Decrement = T.modifyState \st -> { counter: st.counter - 1 }

initialState :: State
initialState = { counter: 0 }

main = do
  let cl = T.createClass spec
  T.render cl unit
  trace "Hello sailor!"
  
