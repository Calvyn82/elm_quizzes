import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import Signal exposing (Address)
import StartApp.Simple as StartApp
import String
import List

main =
  StartApp.start { model = emptyModel, view = view, update = update }

type alias Model =
  { string1 : String
  , string2 : String
  }

emptyModel = Model "" ""

type Action
    = String1 String
    | String2 String

update : Action -> Model -> Model
update action model =
  case action of
    String1 string1 ->
      { model | string1 = string1 }
    String2 string2 ->
      { model | string2 = string2 }

view : Address Action -> Model -> Html
view address model =
  let validationMessage =
    if String.length model.string1 == String.length model.string2 then
       span [style [("color", "green")]] [text "Strings are valid!"]
    else
       span [style [("color", "red")]] [text "Strings must be the same length"]

  in
     div [myStyle]
       [ field "text" address String1 "First String" model.string1
       , field "text" address String2 "Second String" model.string2
       , div [] [ text (toString (setHamming model.string1 model.string2))]
       ]

field : String -> Address Action -> (String -> Action) -> String -> String -> Html
field fieldType address toAction name content =
  div []
    [ div [myStyle] [text name]
    , input
        [ type' fieldType
        , placeholder name
        , value content
        , on "input" targetValue (\string -> Signal.message address (toAction string))
        ]
        []
    ]

myStyle =
  style
    [ ("width", "100%")
    , ("height", "40px")
    , ("padding", "10px 0")
    , ("font-size", "2em")
    , ("text-align", "center")
    ]

setHamming : String -> String -> Int
setHamming string1 string2 =
  if String.length string1 == String.length string2 then
     List.map2(,) (String.toList string1) (String.toList string2)
     |> List.filterMap isDiff
     |> List.length
  else
     0
    

isDiff : List (List String) -> List Bool
isDiff list =
  if List.head list == List.tail list then
     False
  else
     True
