import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import Signal exposing (Address, message)
import StartApp.Simple as StartApp
import String exposing (fromChar, length, toList, toUpper, join)
import Regex exposing (regex, contains)
import List exposing (map)

main =
  StartApp.start { model = "", view = view, update = update }

type Action
  = RNA String

update newStr oldStr =
  newStr

view : Address String -> String -> Html
view address string =
  let validationMessage =
    if contains (regex "^[cgatCGAT]+$") string then
       span [style [("color", "green")]] [text "This is a valid DNA string."]
    else
       span [style [("color", "red")]] [text "Please enter a valid DNA string."]

  in
     div []
       [ input
         [ placeholder "Enter RNA here"
         , value string
         , on "input" targetValue (message address)
         , myStyle
         ]
         []
       , div [ myStyle ] [ text (
         "The RNA complement is " ++ (
           complement (string)
           )
         ) ]
       , div [ myStyle ] [ validationMessage ]
       ]

myStyle =
  style
    [ ("width", "100%")
    , ("height", "40px")
    , ("padding", "5px 0")
    , ("font-size", "1em")
    , ("text-align", "center")
    ]

complement : String -> String
complement dna =
  toUpper dna
  |> toList
  |> List.map (\letter ->
         if letter == 'C' then 'G'
    else if letter == 'G' then 'C'
    else if letter == 'T' then 'A'
    else if letter == 'A' then 'U'
    else '!')
  |> map (\a -> fromChar a)
  |> join ""

