module Main exposing (main)

import Browser
import Browser.Dom exposing (..)
import Browser.Events exposing (..)
import Html exposing (Html, div, text)
import Html.Attributes exposing (src, style)
import Json.Decode as Decode
import Random
import String exposing (fromFloat)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Task
import Time


type alias Game =
    { vy : Float
    , bird : Pos
    , pillars : List Pos
    , flags : Flags
    , state : State
    , toggle : Bool
    , width : Float
    , height : Float
    }


type State
    = Play
    | Stop


type alias Flags =
    { birdSrc : String
    }


type alias Pos =
    { x : Float
    , y : Float
    , h : Float
    , w : Float
    }


type Msg
    = NoOp
    | Tick Float
    | Space
    | Pause
    | CreatePillar Float
    | Interval
    | GotViewport Viewport


constants =
    { jumpSpeed = 100.0
    , gravity = 300.0
    , pillarVelocity = 150
    , pillarTime = 2000
    }


defaultGame : Flags -> Game
defaultGame flags =
    { vy = 0
    , bird =
        { x = -1000
        , y = 100
        , w = 500 / 5
        , h = 350 / 5
        }
    , pillars =
        []
    , flags = flags
    , state = Stop
    , toggle = True
    , width = 800.0
    , height = 600.0
    }


init : Flags -> ( Game, Cmd Msg )
init flags =
    ( defaultGame flags, Task.perform GotViewport getViewport )


update : Msg -> Game -> ( Game, Cmd Msg )
update msg game =
    let
        m =
            if game.state == Stop && msg /= Space then
                case msg of
                    GotViewport { viewport } ->
                        let
                            width =
                                viewport.width

                            height =
                                viewport.height

                            bird =
                                game.bird
                        in
                        { game
                            | width = viewport.width
                            , height = viewport.height
                        }

                    _ ->
                        game

            else
                case msg of
                    Pause ->
                        { game
                            | state =
                                if game.state == Play then
                                    Stop

                                else
                                    Play
                        }

                    Tick dt ->
                        if hasBirdCollided game then
                            { game | state = Stop }

                        else
                            { game
                                | bird = moveBird dt game game.bird
                                , vy = game.vy - constants.gravity * dt
                                , pillars = game.pillars |> List.map (movePillar dt) |> List.filter keepPillar
                            }

                    CreatePillar n ->
                        { game
                            | pillars = createPillar n game :: game.pillars
                            , toggle = not game.toggle
                        }

                    Space ->
                        if game.state == Stop then
                            let
                                bird =
                                    game.bird
                            in
                            { game
                                | state = Play
                                , pillars = [ createPillar 0 game ]
                                , bird = { bird | x = game.width / 2 }
                            }

                        else
                            { game | vy = constants.jumpSpeed }

                    _ ->
                        game

        cmd =
            case msg of
                Interval ->
                    let
                        h =
                            game.height / 5
                    in
                    Random.generate CreatePillar (Random.float h (game.height - h))

                _ ->
                    Cmd.none
    in
    ( m, cmd )


moveBird : Float -> Game -> Pos -> Pos
moveBird dt game bird =
    { bird | y = game.bird.y - game.vy * dt }


hasBirdCollided : Game -> Bool
hasBirdCollided game =
    (game.bird.y < 0)
        || (game.bird.y + game.bird.h > game.height)
        || List.any (hasCollided game.bird) game.pillars


hasCollided : Pos -> Pos -> Bool
hasCollided rect1 rect2 =
    (rect1.x < rect2.x + rect2.w)
        && (rect1.x + rect1.w > rect2.x)
        && (rect1.y < rect2.y + rect2.h)
        && (rect1.y + rect1.h > rect2.y)


movePillar : Float -> Pos -> Pos
movePillar dt pillar =
    { pillar | x = pillar.x - constants.pillarVelocity * dt }


keepPillar : Pos -> Bool
keepPillar { x } =
    x >= 0


createPillar : Float -> Game -> Pos
createPillar h { toggle, width, height } =
    { x = width
    , y =
        if toggle then
            0

        else
            height - h
    , w = 10
    , h = h
    }


view : Game -> Html Msg
view game =
    if game.state == Stop then
        div
            [ Html.Attributes.style "display" "flex"
            , Html.Attributes.style "align-items" "center"
            , Html.Attributes.style "justify-content" "center"
            , Html.Attributes.style "height" "100vh"
            ]
            [ Html.text "Press Space to Start" ]

    else
        svg
            [ width "100%"
            , height "100%"
            , viewBox (String.join " " [ "0", "0", fromFloat game.width, fromFloat game.height ])
            ]
            [ g [] (List.map renderPillar game.pillars)
            , image
                [ game.bird.x |> fromFloat |> x
                , game.bird.y |> fromFloat |> y
                , game.bird.w |> fromFloat |> width
                , game.bird.h |> fromFloat |> height
                , xlinkHref game.flags.birdSrc
                ]
                []
            ]


renderPillar : Pos -> Svg Msg
renderPillar p =
    rect
        [ p.x |> fromFloat |> x
        , p.y |> fromFloat |> y
        , p.h |> fromFloat |> height
        , p.w |> fromFloat |> width
        ]
        []


mapKey : String -> Msg
mapKey string =
    case string of
        " " ->
            Space

        "p" ->
            Pause

        _ ->
            NoOp


subscriptions : Game -> Sub Msg
subscriptions game =
    Sub.batch
        [ onAnimationFrameDelta (\dt -> Tick (dt / 1000))
        , onKeyPress (Decode.map mapKey (Decode.field "key" Decode.string))
        , onClick (Decode.succeed Space)
        , Time.every constants.pillarTime (\_ -> Interval)
        ]


main : Program Flags Game Msg
main =
    Browser.element
        { view = view
        , init = init
        , update = update
        , subscriptions = subscriptions
        }
