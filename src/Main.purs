module Main where

import Debug.Trace

import Global

import qualified Thermite as T
import qualified Thermite.Html as T
import qualified Thermite.Html.Elements as T
import qualified Thermite.Html.Attributes as A
import qualified Thermite.Action as T
import qualified Thermite.Events as T
import qualified Thermite.Types as T

type State = { counter :: Number, incrementText :: String }

data Action = Increment | Decrement | SetEditText String

spec :: T.Spec (T.Action _ State) State Unit Action
spec = T.simpleSpec initialState performAction render
         # T.componentWillMount Increment

render :: T.Render State Unit Action
render ctx st _ = T.div' [ T.p' [ T.text (show st.counter) ]
                         , T.input [ A.value st.incrementText, T.onChange ctx handleChangeEvent ] []
                         , T.button [ T.onClick ctx (const Increment) ] [ T.text "Increment" ]
                         , T.button [ T.onClick ctx (const Decrement) ] [ T.text "Decrement" ]
                         ]


foreign import getValue
  "function getValue(e) {\
  \  return e.target.value;\
  \}" :: forall event. event -> String

handleChangeEvent :: T.FormEvent -> Action
handleChangeEvent e = SetEditText (getValue e)

performAction :: T.PerformAction Unit Action (T.Action _ State)
performAction _ Increment = T.modifyState \st ->
  let parsedIncrement = readInt 10 st.incrementText
      i = if isNaN parsedIncrement then 1 else parsedIncrement in
  { counter: st.counter + i, incrementText: st.incrementText }
performAction _ Decrement = T.modifyState \st ->
  let parsedDecrement = readInt 10 st.incrementText
      d = if isNaN parsedDecrement then 1 else parsedDecrement in
  { counter: st.counter - d, incrementText: st.incrementText }
performAction _ (SetEditText s) = T.modifyState \st ->
  { counter: st.counter, incrementText: s }

initialState :: State
initialState = { counter: 0, incrementText: "" }

main = do
  let cl = T.createClass spec
  T.render cl unit
  trace "Hello sailor!"
  
