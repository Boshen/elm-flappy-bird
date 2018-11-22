module Main exposing (main)

import Browser
import Browser.Dom exposing (..)
import Browser.Events exposing (..)
import Html exposing (Html, div)
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
    , backgroundX : Float
    }


type State
    = Play
    | Stop


type alias Flags =
    { birdSrc : String
    , backgroundSrc : String
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
    | CreatePillar Float
    | Interval
    | GotViewport Viewport


constants =
    { jumpSpeed = 150.0
    , gravity = 300.0
    , pillarVelocity = 150
    , pillarTime = 3000
    }


defaultBird : Pos
defaultBird =
    { x = -1000
    , y = 100
    , w = 500 / 5
    , h = 350 / 5
    }


defaultGame : Flags -> Game
defaultGame flags =
    { vy = 0
    , bird = defaultBird
    , pillars =
        []
    , flags = flags
    , state = Stop
    , toggle = True
    , width = 800.0
    , height = 600.0
    , backgroundX = 0
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
                        { game
                            | width = viewport.width
                            , height = viewport.height
                            , bird = { defaultBird | x = viewport.width / 2 }
                        }

                    _ ->
                        game

            else
                case msg of
                    Tick dt ->
                        if hasBirdCollided game then
                            { game | state = Stop }

                        else
                            { game
                                | bird = moveBird dt game game.bird
                                , vy = game.vy - constants.gravity * dt
                                , pillars = game.pillars |> List.map (movePillar dt) |> List.filter keepPillar
                                , backgroundX =
                                    if game.backgroundX < 0 then
                                        game.width

                                    else
                                        game.backgroundX - 50 * dt
                            }

                    CreatePillar n ->
                        { game
                            | pillars = createPillar n game :: game.pillars
                            , toggle = not game.toggle
                        }

                    Space ->
                        if game.state == Stop then
                            { game
                                | state = Play
                                , pillars = [ createPillar 0 game ]
                                , bird = { defaultBird | x = game.width / 2 }
                                , vy = constants.jumpSpeed
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
    svg
        [ width "100%"
        , height "100%"
        , viewBox (String.join " " [ "0", "0", fromFloat game.width, fromFloat game.height ])
        ]
        [ image
            [ width "100%"
            , height "100%"
            , preserveAspectRatio "none"
            , game.backgroundX |> fromFloat |> x
            , xlinkHref game.flags.backgroundSrc
            ]
            []
        , image
            [ width "100%"
            , height "100%"
            , preserveAspectRatio "none"
            , game.backgroundX - game.width |> fromFloat |> x
            , xlinkHref game.flags.backgroundSrc
            ]
            []
        , g [] (List.map renderPillar game.pillars)
        , image
            [ game.bird.x |> fromFloat |> x
            , game.bird.y |> fromFloat |> y
            , game.bird.w |> fromFloat |> width
            , game.bird.h |> fromFloat |> height
            , xlinkHref game.flags.birdSrc
            ]
            []
        , text_ [ game.width / 2 |> fromFloat |> x, game.height / 2 |> fromFloat |> y ]
            (if game.state == Stop then
                [ text "Press Space to Start" ]

             else
                []
            )
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
