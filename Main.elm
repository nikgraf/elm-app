module Main (..) where

import Html exposing (..)
import Json.Decode exposing (int, string, float, Decoder, (:=))
import Json.Decode.Pipeline exposing (decode, required, optional, hardcoded)
import Effects exposing (Effects, map, batch, Never)
import Http
import Task exposing (Task, andThen, onError)
import Maybe exposing (withDefault)


type alias Model =
  { nodes : Nodes
  }


type alias Textbox =
  { id : String
  , value : String
  }


type alias Wrapper =
  { id : String
  , children : List Node
  }


type Node
  = TextboxNode Textbox
  | WrapperNode Wrapper


type alias Nodes =
  List Node


init : Model
init =
  { nodes = []
  }


type Action
  = NoOp
  | SetNodes (List Node)


update : Action -> Model -> Model
update action model =
  case action of
    SetNodes nodes ->
      { nodes = nodes
      }

    NoOp ->
      model


view : Model -> Html
view model =
  let
    th' field =
      th [] [ text field ]

    tr' node =
      case node of
        WrapperNode wrapper ->
          tr
            []
            [ td [] [ text "wrapper" ]
            , td [] [ text wrapper.id ]
            ]

        TextboxNode textbox ->
          tr
            []
            [ td [] [ text "text" ]
            , td [] [ text textbox.id ]
            ]
  in
    div
      []
      [ table
          []
          [ thead
              []
              [ tr
                  []
                  (List.map
                    th'
                    [ "Kind"
                    , "Id"
                    ]
                  )
              ]
          , tbody [] (List.map tr' model.nodes)
          ]
      ]


actions : Signal.Mailbox Action
actions =
  Signal.mailbox NoOp


model : Signal Model
model =
  Signal.foldp update init actions.signal


main : Signal Html
main =
  Signal.map view model


textDecoder : Decoder Textbox
textDecoder =
  decode Textbox
    |> required "id" string
    |> required "value" string


wrapperDecoder : Decoder Wrapper
wrapperDecoder =
  decode Wrapper
    |> required "id" string
    |> required "children" (Json.Decode.list node)


node : Json.Decode.Decoder Node
node =
  Json.Decode.andThen ("kind" := Json.Decode.string) nodeDecoderBasedOnKind


nodeDecoderBasedOnKind : String -> Json.Decode.Decoder Node
nodeDecoderBasedOnKind kind =
  case kind of
    "textbox" ->
      Json.Decode.map TextboxNode textDecoder

    "wrapper" ->
      Json.Decode.map WrapperNode wrapperDecoder

    _ ->
      Json.Decode.fail ("Failed to parse")


get : Task Http.Error (List Node)
get =
  Http.get (Json.Decode.list node) "/data.json"


port runner : Task Http.Error ()
port runner =
  get `andThen` (SetNodes >> Signal.send actions.address)
