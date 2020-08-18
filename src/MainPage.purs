module MainPage
  ( component
  ) where
    
import Prelude
import Data.Maybe (Maybe(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Core as HC
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Web.Event.Event (Event)
import Web.Event.Event as Event
import Effect.Aff.Class

import Data.Array
import Data.Int

import Effect (Effect, foreachE)
import Effect.Console (log)
import Effect.Class

import Halogen.Aff as HA
import Web.DOM.ParentNode (QuerySelector(..), querySelector)
import Web.HTML.HTMLElement
import Web.HTML.HTMLDocument as HTMLDocument
import Web.HTML.Window as Window
import Web.HTML (window)
import Web.HTML.HTMLInputElement as HTMLInputElement

data Action = Init | SelectOption Option | Submit Event | Input String | ChangeState Todo | DeleteTodo Todo

type Todo = {
  id :: Int
, comment :: String
, state :: TodoState
}

data TodoState = TodoWorking | TodoFinished
derive instance eqTodoState :: Eq TodoState

labelFromTodoState :: TodoState -> String
labelFromTodoState = case _ of
  TodoWorking -> "作業中"
  TodoFinished -> "完了"

valueFromTodoState :: TodoState -> Int
valueFromTodoState = case _ of
  TodoWorking -> 0
  TodoFinished -> 1

todoStateFromValue :: Int -> TodoState
todoStateFromValue = case _ of
  0 -> TodoWorking
  _ -> TodoFinished

data Option = OptionAll | Option TodoState
derive instance eqOption :: Eq Option

labelFromOption :: Option -> String
labelFromOption = case _ of
  OptionAll -> "すべて"
  Option x -> labelFromTodoState x

valueFromOption :: Option -> Int
valueFromOption = case _ of
  OptionAll -> -1
  Option x -> valueFromTodoState x

optionFromValue :: Int -> Option
optionFromValue = case _ of
  -1 -> OptionAll
  x -> Option (todoStateFromValue x)

type State = {
  todos :: Array Todo
, selectedOption :: Option
, options :: Array Option
, counter :: Int
}

component :: forall q i o m. (MonadAff m) => (MonadEffect m) => H.Component HH.HTML q i o m
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
initialState _ = {
  todos: []
, selectedOption: OptionAll
, options: 
  [ OptionAll
  , Option TodoWorking
  , Option TodoFinished
  ]
, counter: 0
}

render :: forall m. (MonadAff m) => (MonadEffect m) => State -> H.ComponentHTML Action () m
render state = do
  HH.div
    [ HP.attr (HC.AttrName "id") "app" ]
    $ concat
      [ [ HH.h1 [] [ HH.text "チュートリアルToDoリスト" ] ]
      , map
        ( \opt -> 
          ( HH.label
            []
            [ HH.input do
              let 
                list =
                  [ HP.attr (HC.AttrName "type") "radio"
                  , HP.attr (HC.AttrName "name") "listState"
                  , HP.attr (HC.AttrName "value") $ toStringAs decimal $ valueFromOption opt
                  , HE.onInput \_ -> Just $ SelectOption opt
                  ]
              if opt == state.selectedOption
                then
                  cons (HP.attr (HC.AttrName "checked") "checked") list
                else
                  list
            , HH.text $ labelFromOption opt
            , HH.text " "]
          )
        ) state.options
      , [ HH.text $ "（" 
          <> toStringAs decimal (length $ visibleTodos state.selectedOption state.todos) 
          <> " 件を表示）" ]
      , [ HH.table [] 
          [ HH.thead []
            [ HH.tr [] 
              [ HH.th [ HP.attr (HC.AttrName "class") "id" ] [ HH.text "ID" ]
              , HH.th [ HP.attr (HC.AttrName "class") "comment" ] [ HH.text "コメント" ]
              , HH.th [ HP.attr (HC.AttrName "class") "state" ] [ HH.text "状態" ]
              , HH.th [ HP.attr (HC.AttrName "class") "button" ] [ HH.text "-" ]
              ]
            ]
          , HH.tbody []
            $ map ( \todo ->
              ( HH.tr (if todo.state == TodoFinished then [ HP.attr (HC.AttrName "class") "done" ] else [])
                  [ HH.th [] [ HH.text $ toStringAs decimal todo.id ]
                  , HH.td [] [ HH.text todo.comment ]
                  , HH.td 
                    [ HP.attr (HC.AttrName "class") "state" ] 
                    [ HH.button [ HE.onClick \x -> Just $ ChangeState todo ] [HH.text $ labelFromTodoState todo.state] ]
                  , HH.td 
                    [ HP.attr (HC.AttrName "class") "button" ] 
                    [ HH.button [ HE.onClick \x -> Just $ DeleteTodo todo ] [HH.text "削除"] ]
                  ]
              )
            ) (visibleTodos state.selectedOption state.todos)
          ]
        ]
      , [ HH.h2 [] [ HH.text "新しい作業の追加" ]
        , HH.form 
          [ HP.attr (HC.AttrName "class") "add-form"
          , HE.onSubmit (Just <<< Submit)
          ]
          [ HH.text "コメント "
          , HH.input
            [ HP.attr (HC.AttrName "type") "text"
            , HP.attr (HC.AttrName "ref") "comment"
            , HE.onValueInput $ Just <<< Input
            ]
          , HH.text " "
          , HH.button
            [ HP.attr (HC.AttrName "type") "submit" ]
            [ HH.text "追加" ]
          ]
        ]
      ]

visibleTodos :: Option -> Array Todo -> Array Todo
visibleTodos option todos = filter check todos
  where 
    check todo = case option of
      OptionAll -> true
      Option state -> todo.state == state
  

handleAction :: forall m o. (MonadAff m) => (MonadEffect m) => Action -> H.HalogenM State Action () o m Unit
handleAction = case _ of
  Init -> do
    pure unit

  SelectOption opt -> do
    H.modify_ (\st -> st { selectedOption = opt })

  Input str -> do -- This is only log function
    H.liftEffect $ log str
    pure unit

  ChangeState todo -> do
    s <- H.get
    let 
      toggle' x
        = ( case x of
            TodoWorking -> TodoFinished
            TodoFinished -> TodoWorking
          ) :: TodoState
      todos = map (\x -> if todo == x then todo { state = (toggle' x.state ) } else x) s.todos
      --   where
      --     toggle x = case x of
      --       TodoWorking -> TodoFinished
      --       TodoFinished -> TodoWorking
    H.modify_ (\st -> st { todos = todos })

  DeleteTodo todo -> do
    s <- H.get
    let
      todos = filter (\x -> todo /= x) s.todos
    H.modify_ (\st -> st { todos = todos })

  Submit event -> do
    H.liftEffect $ Event.preventDefault event
    s <- H.get
    state <- H.liftAff do 
      mel <- liftEffect $
        ((querySelector (QuerySelector "[ref=\"comment\"]") <<< HTMLDocument.toParentNode <=< Window.document) =<< window)
      let maybeComment = HTMLInputElement.fromElement =<< mel
      case maybeComment of
        Just comment -> do
          val <- H.liftEffect do
            val <- HTMLInputElement.value comment
            log val
            HTMLInputElement.setValue "" comment
            pure val
          if val /= ""
            then
              pure $ s { todos = concat [s.todos, [ {id: s.counter, comment: val, state: TodoWorking} ]]
                       , counter = s.counter + 1 }
            else
              pure s
        Nothing -> pure s
    H.modify_ (\st -> st { todos = state.todos, counter = state.counter })
    pure unit
