import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import String
import Http
import Task
import Json.Decode as Json exposing ((:=))
import Navigation
import UrlParser as UrlP

main : Program Never
main =
  Navigation.program urlParser
      { init = init
      , view = view
      , update = update
      , urlUpdate = urlUpdate
      , subscriptions = always Sub.none
      }

type Page = Home | Queue | About | Page404
type alias Url = String

toPath : Page -> Url
toPath page =
    case page of
        Home -> "/"
        Queue -> "/queue"
        About -> "/about"
        Page404 -> "/404"

urlParser : Navigation.Parser (Result String Page)
urlParser = Navigation.makeParser pathParser

pathParser : Navigation.Location -> Result String Page
pathParser location =
    UrlP.parse identity pageParser (String.dropLeft 1 location.pathname)

pageParser : UrlP.Parser (Page -> a) a
pageParser = UrlP.oneOf [ UrlP.format Home (UrlP.s "")
                        , UrlP.format Queue (UrlP.s "queue")
                        , UrlP.format About (UrlP.s "about")
                        ]


urlUpdate : Result String Page -> Model -> (Model, Cmd Msg)
urlUpdate result model =
    case result of
        Ok page -> { model | currentPage = page } ! []

        Err _ -> { model | currentPage = Page404 } ! []

type alias Model = { currentPage : Page
                   , currentSearch : String
                   , searching : Bool
                   , tracks : List Track
                   , currentTrack : Maybe Track
                   , queue : List Track
                   , play : Bool
                   }

type alias Track = { id : String
                    , name : String
                    , preview_url : String
                    }

type Msg
    = NavTo Url
    | Search String
    | SearchSucceed String (List Track)
    | SearchFailed Http.Error
    | Listen Track
    | ListenFromQueue Track
    | Stop Track
    | Enqueue Track

init : Result String Page -> (Model, Cmd Msg)
init result = urlUpdate result initialModel


initialModel : Model
initialModel = { currentPage = Home
               , currentSearch = ""
               , searching = False
               , tracks = []
               , currentTrack = Nothing
               , queue = []
               , play = False
               }

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        NavTo path -> model ! [Navigation.newUrl path]

        Search name ->
            { model
                | currentSearch = name
                , searching = True } ! [searchTrack name]

        SearchSucceed search tracks ->
            if model.currentSearch == search
            then
                { model
                    | searching = False
                    , tracks = tracks } ! []
            else
                model ! []

        SearchFailed _ ->
            { model | searching = False } ! []

        Listen track ->
            { model
                | currentTrack = Just track
                , play = True } ! []

        ListenFromQueue track ->
            { model
                | queue = List.filter (.id >> (/=) track.id) model.queue
                , currentTrack = Just track
                , play = True } ! []

        Stop track ->
            { model | play = False } ! []

        Enqueue track ->
            { model | queue = List.append model.queue [track] } ! []


searchTrack : String -> Cmd Msg
searchTrack query =
    let spotifyUrl = "https://api.spotify.com/v1/search?type=track&q="
        queryUrl = spotifyUrl ++ Http.uriEncode query
    in
        Task.perform SearchFailed (SearchSucceed query) (Http.get decodeTracks queryUrl)

decodeTracks : Json.Decoder (List Track)
decodeTracks =
    Json.at ["tracks", "items"] (Json.list decodeTrack)

decodeTrack : Json.Decoder Track
decodeTrack =
    Json.object3 Track
        ("id" := Json.string)
        ("name" := Json.string)
        ("preview_url" := Json.string)

view : Model -> Html Msg
view model =
    let body = model |> case model.currentPage of
                            Home -> viewHome
                            Queue -> viewQueue
                            About -> viewAbout
                            Page404 -> view404
    in
        div []
            [ viewPlayer model
            , a (navTo (toPath Home)) [text "Search"]
            , text " - "
            , a (navTo (toPath Queue)) [text "Queue"]
            , text " - "
            , a (navTo (toPath About)) [text "About"]
            , hr [] []
            , body
            ]


viewHome : Model -> Html Msg
viewHome model =
    div [] [ input [ placeholder "Search tracks"
                   , onInput Search
                   , value model.currentSearch
                   ] []
           , ul [] (List.map viewTrack model.tracks)
           ]

viewPlayer : Model -> Html Msg
viewPlayer model =
    let emptyPlayer = p [] [ text "Current track: -"]
        normalPlayer track =
            p [] [ text "Current track: "
                 , text track.name
                 , text " - "
                 , if model.play
                   then
                       p [] [ audio [ autoplay True, src track.preview_url] []
                            , a [onClick (Stop track)] [text "stop"]]
                   else
                       a [onClick (Listen track)] [text "resume"]
                 ]
    in
        Maybe.withDefault emptyPlayer (Maybe.map normalPlayer model.currentTrack)

viewTrack : Track -> Html Msg
viewTrack track =
    li [] [ text track.name
          , text " - "
          , a [onClick (Listen track)] [text "listen"]
          , text ", "
          , a [onClick (Enqueue track)] [text "enqueue"]]

viewQueue : Model -> Html Msg
viewQueue model =
    ul [] (List.map viewQueueTrack model.queue)

viewQueueTrack : Track -> Html Msg
viewQueueTrack track =
    li [] [ text track.name
          , text " - "
          , a [onClick (ListenFromQueue track)] [text "listen"]]

viewAbout : Model -> Html Msg
viewAbout model =
    text """The path of the righteous man is beset on all sides by
          the iniquities of the selfish and the tyranny of evil men.
          Blessed is he who, in the name charity and good will, shepherds
          the weak through the valley of darkness."""


view404 : Model -> Html Msg
view404 model = text "404 not found"


navTo : Url -> List (Attribute Msg)
navTo path = [ href path
             , onWithOptions
                   "click"
                   { stopPropagation = True
                   , preventDefault = True }
                   (Json.map (always (NavTo path)) Json.value)
             ]
