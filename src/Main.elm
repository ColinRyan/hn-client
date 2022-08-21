module Main exposing (main)

-- import Json.Decode exposing (Decoder, field, int, map4, string)

import Browser
import Browser.Navigation as Nav
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (..)
import Http
import Json.Decode exposing (Decoder, field, int, string)
import Url



-- MAIN


main : Program () Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlChange = UrlChanged
        , onUrlRequest = LinkClicked
        }



-- MODEL


type alias Story =
    { id : Int
    , kids : Maybe (List Int)
    , score : Int
    , time : Int
    , descendants : Maybe Int
    , title : String
    , by : String
    , url : Maybe String
    }


type alias Comment =
    { id : Int
    , kids : Maybe (List Int)
    , score : Int
    , time : Int
    , text : String
    , by : String
    }


type Status
    = Failure
    | Loading
    | Success String


type alias Model =
    { key : Nav.Key
    , url : Url.Url
    , status : Status
    , currentPage : List Int
    , storyIds : List Int
    , stories : Dict Int Story
    , comments : Dict Int Comment
    }


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    ( Model key url Loading [] [] Dict.empty Dict.empty, getTop500Stories )



-- UPDATE


type Msg
    = LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | GetTopStoryId (Result Http.Error String)
    | GetTop500StoryIds (Result Http.Error (List Int))
    | GetStory (Result Http.Error Story)
    | GetComment (Result Http.Error Comment)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LinkClicked urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Nav.pushUrl model.key (Url.toString url) )

                Browser.External href ->
                    ( model, Nav.load href )

        UrlChanged url ->
            ( { model | url = url }
            , Cmd.none
            )

        GetTopStoryId result ->
            case result of
                Ok topStoryId ->
                    ( { model | status = Success topStoryId }, Cmd.none )

                Err _ ->
                    ( { model | status = Failure }, Cmd.none )

        GetTop500StoryIds result ->
            case result of
                Ok topStoryIds ->
                    ( { model
                        | storyIds =
                            List.drop 20
                                topStoryIds
                        , currentPage =
                            List.take 20
                                topStoryIds
                      }
                    , Cmd.batch
                        (List.map getStory
                            (List.take 20
                                topStoryIds
                            )
                        )
                    )

                Err _ ->
                    ( { model | status = Failure }, Cmd.none )

        GetStory result ->
            case result of
                Ok story ->
                    ( { model
                        | stories =
                            model.stories
                                |> Dict.insert story.id story
                      }
                    , Cmd.none
                    )

                Err _ ->
                    ( { model | status = Failure }, Cmd.none )

        GetComment result ->
            case result of
                Ok comment ->
                    ( { model
                        | comments =
                            model.comments
                                |> Dict.insert comment.id comment
                      }
                    , Cmd.batch
                        (List.map getComment
                            (Maybe.withDefault
                                []
                                comment.kids
                            )
                        )
                    )

                Err _ ->
                    ( { model | status = Failure }, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


view : Model -> Browser.Document Msg
view model =
    { title = "URL Interceptor"
    , body =
        [ ul []
            (List.map (viewStory model) model.currentPage)
        ]
    }


viewStory : Model -> Int -> Html msg
viewStory model storyId =
    let
        story =
            Dict.get storyId
                model.stories
    in
    li []
        [ div []
            [ text
                (Maybe.withDefault ""
                    (Maybe.map .title
                        story
                    )
                )
            ]
        , div []
            [ text
                (String.fromInt
                    (Maybe.withDefault 0
                        (Maybe.map .score
                            story
                        )
                    )
                )
            , text " points by"
            , text
                (Maybe.withDefault ""
                    (Maybe.map .by
                        story
                    )
                )
            ]
        ]



-- HTTP
-- Need to update this code to fetch top 500 stories and parse it then fetch
-- the info for all the stories and then all the comments


getTop500Stories : Cmd Msg
getTop500Stories =
    Http.get
        { url = "https://hacker-news.firebaseio.com/v0/topstories.json"
        , expect = Http.expectJson GetTop500StoryIds top500StoryDecoder
        }


top500StoryDecoder : Decoder (List Int)
top500StoryDecoder =
    Json.Decode.list int


getStory : Int -> Cmd Msg
getStory storyId =
    Http.get
        { url = "https://hacker-news.firebaseio.com/v0/item/" ++ String.fromInt storyId ++ ".json"
        , expect = Http.expectJson GetStory storyDecoder
        }


getComment : Int -> Cmd Msg
getComment commentId =
    Http.get
        { url = "https://hacker-news.firebaseio.com/v0/item/" ++ String.fromInt commentId ++ ".json"
        , expect = Http.expectJson GetComment commentDecoder
        }


storyDecoder : Decoder Story
storyDecoder =
    Json.Decode.map8 Story
        (field "id" int)
        (Json.Decode.maybe (field "kids" (Json.Decode.list int)))
        (field "score" int)
        (field "time" int)
        (Json.Decode.maybe (field "descendants" int))
        (field "title" string)
        (field "by" string)
        (Json.Decode.maybe (field "url" string))


commentDecoder : Decoder Comment
commentDecoder =
    Json.Decode.map6 Comment
        (field "id" int)
        (Json.Decode.maybe (field "kids" (Json.Decode.list int)))
        (field "score" int)
        (field "time" int)
        (field "text" string)
        (field "by" string)
