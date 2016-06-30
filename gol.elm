module Gol exposing (..)

import Html exposing (..)
import Html.App as Html
import Html.Attributes exposing(..)
import Html.Events exposing (onInput, onClick)
import Time exposing(..)
--import Debug exposing(..)

type alias Model = 
  { board : List Int
  , running : Bool
  }

type Msg =
    Toggle Int
  | PlayPause
  | Tick Time

gridSize : Int
gridSize = 100

main : Program Never
main = Html.program
  { init   = init gridSize
  , update = update
  , view   = view
  , subscriptions = subscriptions
  }

init : Int -> (Model, Cmd Msg)
init n =  
  ( { board = List.repeat (n*n) 0
    , running = False
    } 
  , Cmd.none) 

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Toggle indexTarget ->
      let 
        toggle index val =
          if (indexTarget == index) then
            (val + 1) % 2
          else
            val
      in
        ({model | board = (List.indexedMap toggle model.board)}, Cmd.none)
    PlayPause ->
      ({model | running = not model.running}, Cmd.none)
    Tick time ->
      ({model | board = updateBoard model.board}, Cmd.none)

updateBoard : List Int -> List Int
updateBoard board =
  let
    countAliveNeighbors index = 
      neighbors board (index-102) 3 +
      neighbors board (index-2) 1 +
      neighbors board index 1 +
      neighbors board (index+98) 3
    
    toggle index val =
      let 
        aliveNeighbors = countAliveNeighbors index
      in
        if (val==0)
        then 
          if (aliveNeighbors == 3)
          then 1
          else 0
        else
          if (aliveNeighbors < 2 || aliveNeighbors > 3)
          then 0
          else 1
  in
    List.indexedMap toggle board

neighbors : List Int -> Int -> Int -> Int
neighbors board startIndex range  =
  let
    endIndex = startIndex + range
  in
    if (endIndex < 0)
    then 0
    else
      List.foldr (+) 0 (List.take range (List.drop (startIndex+1) board))

view : Model -> Html Msg
view model =
  let
    buttonText = 
      if (model.running) 
      then "pause"
      else "play"
  in 
    div[]
      [ h1 [] [ text "Conway's Game of Life" ]
      , button [ onClick PlayPause ] [ text buttonText ]
      , div[ class "grid", gridDim model ] (List.indexedMap viewCell model.board) 
      ]

gridDim : Model -> Attribute a
gridDim model =
  style 
  [ ("width",  gridDimAsString model)
  , ("height", gridDimAsString model) ]

gridDimAsString model =
  (toString (toFloat (List.length model.board*10)/(toFloat gridSize)) ++ "px")

viewCell : Int -> Int -> Html Msg
viewCell index val =
  let
    color = if(val==1) then "#000" else "#fff"
  in
    div [ class "cell", style [("background", color)], onClick (Toggle index) ] []


subscriptions model =
  if (model.running)
  then Time.every (0.1*second) Tick
  else Sub.none
