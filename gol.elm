module Gol exposing (..)

import Html exposing (..)
import Html.App as Html
import Html.Attributes exposing(..)
import Html.Events exposing (onInput, onClick)
import Time exposing(..)
import String exposing(toInt)
import Array exposing(..)


type alias Model = 
  { board : Array (Array Int)
  , running : Bool
  , gridSize : Int
  }


type Msg =
    Toggle (Int, Int)
  | PlayPause
  | Tick Time
  | ChangeSize String


main : Program Never
main = Html.program
  { init   = init 10
  , update = update
  , view   = view
  , subscriptions = subscriptions
  }


init : Int -> (Model, Cmd Msg)
init n =  
  ( { board = Array.repeat n (Array.repeat n 0)
    , running = False
    , gridSize = n
    } 
  , Cmd.none) 


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    ChangeSize stSize ->
      (init (parseSize stSize))
    Toggle (iCell, iRow) ->
      let 
        row = Maybe.withDefault Array.empty <| Array.get iRow model.board  
        val = Maybe.withDefault 0 <| Array.get iCell row
        newRow = Array.set iCell ((val + 1) % 2) row
      in
        ({model | board = Array.set iRow newRow model.board}, Cmd.none)
    PlayPause ->
      ({model | running = not model.running}, Cmd.none)
    Tick time ->
      ({model | board = updateBoard model}, Cmd.none)
      

parseSize : String -> Int
parseSize stSize =
  let
    mbSize = Result.toMaybe (String.toInt stSize)
  in
    Maybe.withDefault 10 mbSize |> crop 10 100

        
crop : Int -> Int -> Int -> Int
crop min max val =
  Basics.max min (Basics.min max val) 


updateBoard : Model -> Array (Array Int)
updateBoard model =
  let
    board = model.board
    iterateRows iRow row =
      let 
        iterateCells iCell val = 
          let
            aliveNeighbors = countLivingNeighbors (iRow, iCell) board
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
        Array.indexedMap iterateCells row
  in
    Array.indexedMap iterateRows board


countLivingNeighbors : (Int, Int) -> (Array (Array Int)) -> Int
countLivingNeighbors (iRow, iCell) board =
  let
    mbRowAbove = Array.get (iRow-1) board
    mbRow = Array.get iRow board
    mbRowBelow = Array.get (iRow+1) board

    top = 
      case mbRowAbove of
        Nothing -> 0
        Just rowAbove -> count iCell rowAbove

    bottom =
      case mbRowBelow of
        Nothing -> 0
        Just rowBelow -> count iCell rowBelow

    left =
      case mbRow of
        Nothing -> 0
        Just row -> getValue (iCell-1) row

    right =
      case mbRow of
        Nothing -> 0
        Just row -> getValue (iCell+1) row
  in
    top + bottom + left + right


count : Int -> Array Int -> Int
count iCell row =
  let
    left = getValue (iCell-1) row
    mid = getValue iCell row
    right = getValue (iCell+1) row
  in
    left + mid + right


getValue : Int -> Array Int -> Int
getValue index row =
  Maybe.withDefault 0 <| Array.get index row


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
      , input [onInput ChangeSize, type' "number", placeholder "10 <= size <= 100"] [ ]
      , div[ class "grid", gridStyle model ] (Array.toList (Array.indexedMap viewRow model.board))
      ]


gridStyle : Model -> Attribute a
gridStyle model =
  style 
  [ ("width",  gridDimAsString model)
  , ("height", gridDimAsString model) ]


gridDimAsString : Model -> String
gridDimAsString model =
  (toString (Array.length model.board*10) ++ "px")


viewRow : Int -> Array Int -> Html Msg
viewRow iRow row =
  let
    viewCell iCell val =
      let
        color = if(val==1) then "#000" else "#fff"
      in
        div [ class "cell", style [("background", color)], onClick (Toggle (iCell, iRow)) ] []
  in
    div[] (Array.toList (Array.indexedMap viewCell row))
     

subscriptions : Model -> Sub Msg
subscriptions model =
  if (model.running)
  then Time.every (0.1*second) Tick
  else Sub.none


