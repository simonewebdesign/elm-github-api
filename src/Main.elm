import Char
import Html exposing (..)
import Html.Attributes as Attr exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as Json exposing ((:=))
import String
import Task exposing (Task)

type alias User =
  { id : Int
  , name : String
  , avatar_url : String
  }


-- WIRING

main : Signal Html
main =
  Signal.map2 view query.signal results.signal


query : Signal.Mailbox String
query =
  Signal.mailbox ""


results : Signal.Mailbox (Result String User)
results =
  Signal.mailbox (Err "No results")


port requests : Signal (Task x ())
port requests =
  query.signal
  |> Signal.map lookupUsername
  |> Signal.map (\task -> Task.toResult task `Task.andThen` Signal.send results.address)


lookupUsername : String -> Task String User
lookupUsername query =
  let
    toUrl = Task.succeed ("https://api.github.com/users/" ++ query)
  in
    toUrl `Task.andThen` (Task.mapError (always "Not found :(") << Http.get decoder)


decoder : Json.Decoder User
decoder =
  Json.object3 User
    ("id" := Json.int)
    ("name" := Json.string)
    ("avatar_url" := Json.string)


-- VIEW

view : String -> Result String User -> Html
view string result =
  let
    field =
      input
        [ placeholder "Enter a GitHub username..."
        , value string
        , on "input" targetValue (Signal.message query.address)
        , myStyle
        ]
        []

    messages =
      case result of
        Err msg ->
          [ div [ myStyle ]
            [ text msg ]
          ]

        Ok user ->
          [ div [ myStyle ]
            [ text (toString user.id)
            , text user.name
            , text user.avatar_url
            ]
          ]
  in
    div [] (field :: messages)


myStyle : Attribute
myStyle =
  style
    [ ("width", "100%")
    , ("height", "40px")
    , ("padding", "10px 0")
    , ("font-size", "2em")
    , ("text-align", "center")
    ]
