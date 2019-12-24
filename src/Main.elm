module Main exposing (main)

import Browser
import Browser.Events as Events
import Element exposing (..)
import Element.Background as B
import Element.Border as Bd
import Element.Events as E
import Element.Font as F
import Element.Input as I
import Html exposing (Html)
import Svg as S
import Svg.Attributes as SA



-- Style


lightBlue =
    rgb255 227 243 252


purple =
    rgb255 166 125 224


veryLightGray =
    rgb255 240 240 240


lightGray =
    rgb255 180 180 180


midGray =
    rgb255 110 110 110


veryLightTurq =
    rgb255 183 215 220


lightTurq =
    rgb255 104 181 195


darkTurq =
    rgb255 66 98 106


lightBeige =
    rgb255 246 245 230


orange =
    rgb255 253 126 105


darkBlueGray =
    rgb255 70 77 87


white =
    rgb255 255 255 255


lightGreen =
    rgb255 110 180 130


monoFont =
    [ F.external
        { name = "Inconsolata"
        , url = "https://fonts.googleapis.com/css?family=Inconsolata"
        }
    ]


sansFont =
    [ F.external
        { name = "Karla"
        , url = "https://fonts.googleapis.com/css?family=Karla"
        }
    ]


iconTemplate : Int -> String -> String -> Element Msg
iconTemplate size color path =
    Element.html <|
        S.svg [ SA.viewBox "0 0 24 24", SA.height <| String.fromInt size ]
            [ S.path
                [ SA.fill color
                , SA.d path
                ]
                []
            ]


arrowDown : Int -> String -> Element Msg
arrowDown size color =
    iconTemplate size color "M7.41 8.59L12 13.17l4.59-4.58L18 10l-6 6-6-6 1.41-1.41z"


arrowUp : Int -> String -> Element Msg
arrowUp size color =
    iconTemplate size color "M7.41 15.41L12 10.83l4.59 4.58L18 14l-6-6-6 6z"



-- Model


main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


type alias Model =
    { world : World
    , balls : List Ball
    , cannon : Cannon
    , ballCount : Int
    , playState : PlayState
    }


type PlayState
    = Playing
    | Paused


type alias World =
    { width : Int
    , height : Int
    , backgroundColor : Int
    , wallThickness : Int
    , wallHeight : Int
    , gravity : Float
    , restitution : Float
    , wallColor : String
    }


type alias Ball =
    { x : Float
    , y : Float
    , vx : Float
    , vy : Float
    , radius : Float
    , mass : Float
    , color : String
    }


type alias Cannon =
    { xBase : Int
    , yBase : Int
    , length : Int
    , width : Int
    , postWidth : Int
    , angle : Int
    , speed : Float
    , angleMin : Int
    , angleMax : Int
    , angleIncrement : Int
    , speedMin : Float
    , speedMax : Float
    , speedIncrement : Float
    }


init : () -> ( Model, Cmd Msg )
init _ =
    let
        world =
            initWorld

        cannon =
            initCannon world
    in
    ( { world = world
      , cannon = cannon
      , balls = []
      , ballCount = 0
      , playState = Playing
      }
    , Cmd.none
    )


initWorld : World
initWorld =
    let
        worldWidth =
            1100

        worldHeight =
            400

        wallHeight =
            worldHeight
    in
    { width = worldWidth
    , height = worldHeight
    , backgroundColor = 0
    , wallThickness = 6
    , gravity = 0.001
    , wallHeight = wallHeight
    , restitution = 0.8
    , wallColor = "#c4c4c4"
    }


initCannon : World -> Cannon
initCannon world =
    { xBase = world.width // 10
    , yBase = world.height - world.height // 10
    , length = 18
    , width = 20
    , postWidth = 8
    , angle = 45
    , speed = 0.5
    , angleMin = -90
    , angleMax = 270
    , angleIncrement = 5
    , speedMin = 0.1
    , speedMax = 100
    , speedIncrement = 0.1
    }



-- Update


type Msg
    = NoOp
    | OnAnimationFrame Float
    | FireBall
    | PausePlay
    | StartPlay
    | Reset
    | IncreaseAngle
    | DecreaseAngle
    | IncreaseSpeed
    | DecreaseSpeed


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({ world, cannon, balls, playState } as model) =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        OnAnimationFrame dt ->
            case playState of
                Paused ->
                    ( model, Cmd.none )

                Playing ->
                    ( { model | balls = updateBalls dt world balls }, Cmd.none )

        FireBall ->
            ( { model
                | balls = newBall cannon :: balls
                , ballCount = model.ballCount + 1
              }
            , Cmd.none
            )

        PausePlay ->
            ( { model | playState = Paused }, Cmd.none )

        StartPlay ->
            ( { model | playState = Playing }, Cmd.none )

        Reset ->
            ( { model
                | balls = []
                , ballCount = 0
                , cannon = initCannon world
              }
            , Cmd.none
            )

        IncreaseAngle ->
            let
                newAngle =
                    min cannon.angleMax (cannon.angle + cannon.angleIncrement)

                newCannon =
                    { cannon | angle = newAngle }
            in
            ( { model | cannon = newCannon }, Cmd.none )

        DecreaseAngle ->
            let
                newAngle =
                    max cannon.angleMin (cannon.angle - cannon.angleIncrement)

                newCannon =
                    { cannon | angle = newAngle }
            in
            ( { model | cannon = newCannon }, Cmd.none )

        IncreaseSpeed ->
            let
                newSpeed =
                    min cannon.speedMax (cannon.speed + cannon.speedIncrement)

                newCannon =
                    { cannon | speed = newSpeed }
            in
            ( { model | cannon = newCannon }, Cmd.none )

        DecreaseSpeed ->
            let
                newSpeed =
                    max cannon.speedMin (cannon.speed - cannon.speedIncrement)

                newCannon =
                    { cannon | speed = newSpeed }
            in
            ( { model | cannon = newCannon }, Cmd.none )


newBall : Cannon -> Ball
newBall cannon =
    let
        vx =
            cannon.speed * cos (degrees (-1 * cannon.angle |> toFloat))

        vy =
            cannon.speed * sin (degrees (-1 * cannon.angle |> toFloat))
    in
    { x = cannon.xBase |> toFloat
    , y = cannon.yBase |> toFloat
    , vx = vx
    , vy = vy
    , radius = 8
    , mass = 1
    , color = "#515963"
    }


updateBalls : Float -> World -> List Ball -> List Ball
updateBalls dt world balls =
    List.map (updateBall dt world) balls


updateBall : Float -> World -> Ball -> Ball
updateBall dt world ({ x, y, vx, vy } as ball) =
    let
        collisions =
            [ floorCollision world ball
            , rightWallCollision world ball
            , leftWallCollision world ball
            , ceilingCollision world ball
            ]

        ( newVx, newVy ) =
            case collisions of
                [ True, False, False, False ] ->
                    ( vx, -1 * world.restitution * abs vy )

                [ False, True, False, False ] ->
                    ( -1 * world.restitution * abs vx, vy )

                [ False, False, True, False ] ->
                    ( world.restitution * abs vx, vy )

                [ True, True, False, False ] ->
                    if abs vy < 0.001 then
                        ( -1 * world.restitution * abs vx, vy )

                    else
                        ( vx, -1 * world.restitution * abs vy )

                [ True, False, True, False ] ->
                    if abs vy < 0.001 then
                        ( world.restitution * abs vx, vy )

                    else
                        ( vx, -1 * world.restitution * abs vy )

                [ False, False, False, True ] ->
                    ( vx, world.restitution * abs vy )

                _ ->
                    ( vx, vy + world.gravity * dt )
    in
    { ball
        | x = x + (dt * newVx)
        , y = y + (dt * newVy)
        , vx = newVx
        , vy = newVy
    }


floorCollision : World -> Ball -> Bool
floorCollision { width, height, wallThickness } { x, y, radius } =
    let
        ylimit =
            (height - wallThickness) |> toFloat
    in
    y + radius >= ylimit


ceilingCollision : World -> Ball -> Bool
ceilingCollision { width, height, wallThickness } { x, y, radius } =
    let
        ylimit =
            wallThickness |> toFloat
    in
    y - radius <= ylimit


rightWallCollision : World -> Ball -> Bool
rightWallCollision { width, height, wallHeight, wallThickness } { x, y, radius } =
    let
        xlimit =
            (width - wallThickness) |> toFloat

        ylimit =
            (height - wallHeight) |> toFloat
    in
    (x + radius >= xlimit) && (y >= ylimit)


leftWallCollision : World -> Ball -> Bool
leftWallCollision { width, height, wallHeight, wallThickness } { x, y, radius } =
    let
        xlimit =
            wallThickness |> toFloat

        ylimit =
            (height - wallHeight) |> toFloat
    in
    (x - radius <= xlimit) && (y >= ylimit)



-- Subscriptions


subscriptions : Model -> Sub Msg
subscriptions model =
    Events.onAnimationFrameDelta OnAnimationFrame



-- View


view : Model -> Html Msg
view { world, cannon, balls, playState, ballCount } =
    layout [] <|
        column
            [ width fill
            , height fill
            ]
            [ el
                [ centerX
                , paddingEach
                    { top = 20
                    , bottom = 0
                    , right = 0
                    , left = 0
                    }
                ]
              <|
                viewPlayArea world cannon balls
            , row
                [ centerX
                , spacing 50
                ]
                [ row [ spacing 20 ]
                    [ el [] <| viewAngleStepper cannon
                    , el [] <| viewSpeedStepper cannon
                    ]
                , el [ alignBottom ] <| viewFireButton playState
                ]
            , row
                [ centerX
                , spacing 20
                , padding 30
                ]
                [ viewPlayStateButton playState
                , viewResetButton
                ]
            ]


viewButtons : PlayState -> Element Msg
viewButtons playState =
    row [ spacing 30, centerX ]
        [ viewFireButton playState
        , viewPlayStateButton playState
        , viewResetButton
        ]


viewAngleStepper : Cannon -> Element Msg
viewAngleStepper cannon =
    viewStepper
        "Angle"
        (cannon.angle |> String.fromInt)
        IncreaseAngle
        DecreaseAngle


viewSpeedStepper : Cannon -> Element Msg
viewSpeedStepper cannon =
    viewStepper
        "Speed"
        (cannon.speed * 10 |> round |> String.fromInt)
        IncreaseSpeed
        DecreaseSpeed


viewStepper : String -> String -> Msg -> Msg -> Element Msg
viewStepper label valueString plusMsg minusMsg =
    let
        incrementButton icon msg =
            I.button
                [ width (px 55)
                , height (px 28)
                , B.color lightTurq
                , Bd.width 0
                , focused []
                ]
                { onPress = Just msg
                , label =
                    el
                        [ centerX
                        , centerY
                        ]
                        icon
                }

        plusButton =
            incrementButton (arrowUp 30 "white") plusMsg

        minusButton =
            incrementButton (arrowDown 30 "white") minusMsg
    in
    column
        []
        [ el
            [ paddingXY 0 5
            , F.size 20
            , F.color lightGray
            , F.family sansFont
            ]
            (text label)
        , row
            [ spacing 4
            ]
            [ el
                [ height (px 60)
                , width (px 80)
                , B.color veryLightGray
                ]
              <|
                el
                    [ padding 6
                    , alignRight
                    , centerY
                    , F.size 44
                    , F.family monoFont
                    , F.color midGray
                    ]
                    (text valueString)
            , column
                [ height fill
                ]
                [ el [ alignTop ] plusButton
                , el [ alignBottom ] minusButton
                ]
            ]
        ]


viewFireButton : PlayState -> Element Msg
viewFireButton playState =
    let
        label =
            "Fire!"

        width =
            250
    in
    case playState of
        Playing ->
            buttonC
                { defaultButtonConfig
                    | backgroundColor = lightGreen
                    , buttonWidth = width
                }
                label
                FireBall

        Paused ->
            buttonC
                { defaultButtonConfig
                    | backgroundColor = veryLightGray
                    , buttonWidth = width
                }
                label
                NoOp


viewPlayStateButton : PlayState -> Element Msg
viewPlayStateButton playState =
    let
        color =
            purple
    in
    case playState of
        Playing ->
            buttonC
                { defaultButtonConfig
                    | backgroundColor = white
                    , borderColor = color
                    , borderWidth = 2
                    , fontColor = color
                }
                "Pause"
                PausePlay

        Paused ->
            buttonC
                { defaultButtonConfig
                    | backgroundColor = white
                    , borderColor = color
                    , borderWidth = 2
                    , fontColor = color
                }
                "Play"
                StartPlay


viewResetButton : Element Msg
viewResetButton =
    buttonC
        { defaultButtonConfig
            | backgroundColor = white
            , borderColor = orange
            , borderWidth = 2
            , fontColor = orange
        }
        "Reset"
        Reset


viewPlayArea : World -> Cannon -> List Ball -> Element Msg
viewPlayArea world cannon balls =
    el
        [ centerX
        , padding 30
        ]
    <|
        html <|
            S.svg
                [ SA.width <| String.fromInt world.width
                , SA.height <| String.fromInt world.height
                , SA.style "background: #f4fbff"
                ]
            <|
                List.concat
                    [ [ viewFloor world
                      , viewCeiling world
                      , viewRightWall world
                      , viewLeftWall world
                      ]
                    , List.map viewBall balls
                    , [ viewBarrel world cannon
                      , viewBarrelBase cannon
                      , viewBarrelSupportPost world cannon
                      , viewBarrelSupportPivot cannon
                      ]
                    ]


viewBarrel : World -> Cannon -> S.Svg Msg
viewBarrel world cannon =
    let
        xRect =
            cannon.xBase

        yRect =
            cannon.yBase - cannon.width // 2

        xRotate =
            cannon.xBase

        yRotate =
            cannon.yBase

        rotateArgs =
            List.map String.fromInt [ -1 * cannon.angle, xRotate, yRotate ]

        buildRotate =
            "rotate("
                ++ String.join "," rotateArgs
                ++ ")"
    in
    S.rect
        [ SA.x <| String.fromInt xRect
        , SA.y <| String.fromInt yRect
        , SA.width <| String.fromInt cannon.length
        , SA.height <| String.fromInt cannon.width
        , SA.fill "orange"
        , SA.transform buildRotate
        ]
        []


viewBarrelBase : Cannon -> S.Svg Msg
viewBarrelBase cannon =
    S.circle
        [ SA.cx <| String.fromInt cannon.xBase
        , SA.cy <| String.fromInt cannon.yBase
        , SA.r <| String.fromInt (cannon.width // 2)
        , SA.fill "orange"
        ]
        []


viewBarrelSupportPost : World -> Cannon -> S.Svg Msg
viewBarrelSupportPost world cannon =
    let
        xRect =
            cannon.xBase - cannon.postWidth // 2

        yRect =
            cannon.yBase
    in
    S.rect
        [ SA.x <| String.fromInt xRect
        , SA.y <| String.fromInt yRect
        , SA.width <| String.fromInt cannon.postWidth
        , SA.height <| String.fromInt (world.height - cannon.yBase - world.wallThickness)
        , SA.fill "darkorange"
        ]
        []


viewBarrelSupportPivot : Cannon -> S.Svg Msg
viewBarrelSupportPivot cannon =
    S.circle
        [ SA.cx <| String.fromInt cannon.xBase
        , SA.cy <| String.fromInt cannon.yBase
        , SA.r <| String.fromInt (cannon.postWidth // 2)
        , SA.fill "darkorange"
        ]
        []


viewBall : Ball -> S.Svg Msg
viewBall { x, y, radius, color } =
    S.circle
        [ SA.cx <| (x |> round |> String.fromInt)
        , SA.cy <| (y |> round |> String.fromInt)
        , SA.r <| (radius |> round |> String.fromInt)
        , SA.fill color
        ]
        []


viewFloor : World -> S.Svg Msg
viewFloor { width, height, wallThickness, wallColor } =
    S.rect
        [ SA.x <| String.fromInt 0
        , SA.y <| String.fromInt (height - wallThickness)
        , SA.width <| String.fromInt width
        , SA.height <| String.fromInt wallThickness
        , SA.fill wallColor
        ]
        []


viewCeiling : World -> S.Svg Msg
viewCeiling { width, height, wallThickness, wallColor } =
    S.rect
        [ SA.x <| String.fromInt 0
        , SA.y <| String.fromInt 0
        , SA.width <| String.fromInt width
        , SA.height <| String.fromInt wallThickness
        , SA.fill wallColor
        ]
        []


viewRightWall : World -> S.Svg msg
viewRightWall { width, height, wallThickness, wallHeight, wallColor } =
    S.rect
        [ SA.x <| String.fromInt (width - wallThickness)
        , SA.y <| String.fromInt (height - wallHeight)
        , SA.width <| String.fromInt wallThickness
        , SA.height <| String.fromInt wallHeight
        , SA.fill wallColor
        ]
        []


viewLeftWall : World -> S.Svg msg
viewLeftWall { width, height, wallThickness, wallHeight, wallColor } =
    S.rect
        [ SA.x <| String.fromInt 0
        , SA.y <| String.fromInt (height - wallHeight)
        , SA.width <| String.fromInt wallThickness
        , SA.height <| String.fromInt wallHeight
        , SA.fill wallColor
        ]
        []


button : String -> Int -> Color -> Msg -> Element Msg
button label buttonWidth color msg =
    I.button
        [ width (px buttonWidth)
        , height (px 60)
        , B.color color
        , Bd.width 0
        , focused []
        ]
        { onPress = Just msg
        , label =
            el
                [ centerX
                , centerY
                , F.center
                , F.size 28
                , F.family sansFont
                , F.color white
                ]
                (text label)
        }


defaultButtonConfig =
    { backgroundColor = midGray
    , fontColor = white
    , borderWidth = 0
    , borderColor = midGray
    , buttonWidth = 145
    }


buttonC :
    { backgroundColor : Color
    , fontColor : Color
    , borderWidth : Int
    , borderColor : Color
    , buttonWidth : Int
    }
    -> String
    -> Msg
    -> Element Msg
buttonC config label msg =
    I.button
        [ width (px config.buttonWidth)
        , height (px 60)
        , B.color config.backgroundColor
        , Bd.width config.borderWidth
        , Bd.color config.borderColor
        , focused []
        ]
        { onPress = Just msg
        , label =
            el
                [ centerX
                , centerY
                , F.center
                , F.size 28
                , F.family sansFont
                , F.color config.fontColor
                ]
                (text label)
        }



{-
   * More interesting world and ball colors
   * Random ball colors?
   * Obstacle? Obstacle and target?
   * Pause and Reset and bordered white buttons
   * display number of balls in upper left (inFront) of play area?
   * Host this on githup pages
-}
