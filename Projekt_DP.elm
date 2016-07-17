module Main exposing (..)

import Html exposing (..)
import Html.Events exposing (..)
import Html.App as Html
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Time exposing (Time, second)
import Animation exposing (..)
import Mouse exposing (..)
import Keyboard exposing (KeyCode)
import AnimationFrame exposing (..)


main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type Key
    = Space
    | Unknown


fromCode : Int -> Key
fromCode keyCode =
    case keyCode of
        32 ->
            Space

        _ ->
            Unknown


getX : Mineral -> String
getX mineral =
    toString mineral.x


getY : Mineral -> String
getY mineral =
    toString mineral.y


getR : Mineral -> String
getR mineral =
    toString mineral.radius


getXSnitch : Snitch -> String
getXSnitch snitch =
    toString snitch.x


getYSnitch : Snitch -> String
getYSnitch snitch =
    toString snitch.y


getRSnitch : Snitch -> String
getRSnitch snitch =
    toString snitch.radius


type State
    = Play
    | Win
    | Lose


type alias Mineral =
    { radius : Float
    , x : Float
    , y : Float
    }


type alias Snitch =
    { radius : Float
    , x : Float
    , y : Float
    , xCenter : Float
    , yCenter : Float
    }


type alias Model =
    { state : State
    , score : Float
    , goal : Float
    , gold1 : Mineral
    , gold2 : Mineral
    , gold3 : Mineral
    , silver1 : Mineral
    , silver2 : Mineral
    , silver3 : Mineral
    , iron1 : Mineral
    , iron2 : Mineral
    , iron3 : Mineral
    , snitch1 : Snitch
    , snitch2 : Snitch
    , timeSnitch : Time
    , time : Time
    , timeGame : Float
    , length : Float
    , shoot : Bool
    , collision : Bool
    , level : Int
    , levelTime : Int
    }


model : Model
model =
    { state = Play
    , score = 0
    , goal = 1800
    , gold1 = { radius = 3, x = 40, y = 210 }
    , gold2 = { radius = 3, x = 160, y = 220 }
    , gold3 = { radius = 3, x = 220, y = 150 }
    , silver1 = { radius = 7, x = 70, y = 190 }
    , silver2 = { radius = 7, x = 120, y = 210 }
    , silver3 = { radius = 7, x = 30, y = 150 }
    , iron1 = { radius = 10, x = 180, y = 200 }
    , iron2 = { radius = 10, x = 60, y = 165 }
    , iron3 = { radius = 10, x = 220, y = 170 }
    , snitch1 = { radius = 5, x = -500, y = -500, xCenter = -500, yCenter = -500 }
    , snitch2 = { radius = 5, x = -500, y = -500, xCenter = -500, yCenter = -500 }
    , timeSnitch = 0
    , time = 0
    , timeGame = 0
    , length = 12
    , shoot = False
    , collision = False
    , level = 1
    , levelTime = 60
    }


init : ( Model, Cmd Msg )
init =
    ( model, Cmd.none )



-- UPDATE


shoot : Model -> Model
shoot model =
    { model | length = model.length + 2, shoot = True }


angle : Model -> Float
angle model =
    turns (Time.inMinutes model.time) * 6


angle2 : Model -> Float
angle2 model =
    turns (Time.inMinutes model.timeSnitch) * 6


handX : Model -> Float
handX model =
    (120 + model.length * cos (angle model))


handY : Model -> Float
handY model =
    (120 + model.length * sin (angle model))


isCollision : Mineral -> Model -> Bool
isCollision mineral model =
    if
        ((mineral.x - mineral.radius) <= handX model)
            && (handX model <= (mineral.x + mineral.radius))
            && ((mineral.y - mineral.radius) <= handY model)
            && (handY model <= (mineral.y + mineral.radius))
    then
        True
    else
        model.collision


isCollision2 : Snitch -> Model -> Bool
isCollision2 snitch model =
    if
        ((snitch.x - snitch.radius) <= handX model)
            && (handX model <= (snitch.x + snitch.radius))
            && ((snitch.y - snitch.radius) <= handY model)
            && (handY model <= (snitch.y + snitch.radius))
    then
        True
    else
        model.collision


isThisCollision : Mineral -> Model -> Bool
isThisCollision mineral model =
    ((mineral.x - mineral.radius) <= handX model)
        && (handX model <= (mineral.x + mineral.radius))
        && ((mineral.y - mineral.radius) <= handY model)
        && (handY model <= (mineral.y + mineral.radius))


isThisCollision2 : Snitch -> Model -> Bool
isThisCollision2 snitch model =
    ((snitch.x - snitch.radius) <= handX model)
        && (handX model <= (snitch.x + snitch.radius))
        && ((snitch.y - snitch.radius) <= handY model)
        && (handY model <= (snitch.y + snitch.radius))


setX : Model -> Mineral -> Mineral
setX model mineral =
    if isThisCollision mineral model then
        { mineral | x = -1000 }
    else
        mineral


setY : Model -> Mineral -> Mineral
setY model mineral =
    if isThisCollision mineral model then
        { mineral | y = -1000 }
    else
        mineral


setX2 : Model -> Snitch -> Snitch
setX2 model snitch =
    if isThisCollision2 snitch model then
        { snitch | xCenter = -1000 }
    else
        snitch


setY2 : Model -> Snitch -> Snitch
setY2 model snitch =
    if isThisCollision2 snitch model then
        { snitch | yCenter = -1000 }
    else
        snitch


snitchX : Snitch -> Model -> Snitch
snitchX snitch model =
    { snitch | x = (snitch.xCenter + 20 * cos (angle2 model)) }


snitchY : Snitch -> Model -> Snitch
snitchY snitch model =
    { snitch | y = (snitch.yCenter + 20 * sin (angle2 model)) }


type Msg
    = TimeUpdate Time
    | TimeUpdate2 Time
    | TimeGame Time
    | Shoot
    | KeyDown KeyCode
    | KeyUp KeyCode
    | Collide KeyCode
    | UpdateSnitchX Time
    | UpdateSnitchY Time
    | GameState Time
    | NewGame
    | NextLevel


isKeyDown : KeyCode -> Model -> Bool
isKeyDown keyCode model =
    case fromCode keyCode of
        Space ->
            True

        _ ->
            False


keyDown : KeyCode -> Model -> Model
keyDown keyCode model =
    case fromCode keyCode of
        Space ->
            shoot model

        _ ->
            model


keyUp : KeyCode -> Model -> Model
keyUp keyCode model =
    case fromCode keyCode of
        Space ->
            { model | length = 12, shoot = False, collision = False }

        _ ->
            model


update : Msg -> Model -> ( Model, Cmd Msg )
update action model =
    case action of
        TimeGame time ->
            ( { model | timeGame = model.timeGame + 1 }, Cmd.none )

        TimeUpdate newTime ->
            if model.shoot then
                ( model, Cmd.none )
            else
                ( { model | time = newTime }, Cmd.none )

        TimeUpdate2 newTime ->
            ( { model | timeSnitch = newTime }, Cmd.none )

        Shoot ->
            if model.length > 180 then
                ( { model | length = model.length + 2, shoot = True }, Cmd.none )
            else
                ( { model | collision = False, length = 12 }, Cmd.none )

        KeyDown keyCode ->
            ( keyDown keyCode model, Cmd.none )

        KeyUp keyCode ->
            ( keyUp keyCode model, Cmd.none )

        Collide keyCode ->
            if isKeyDown keyCode model then
                if isThisCollision model.iron1 model then
                    ( { model | score = model.score + 70, collision = isCollision model.iron1 model, iron1 = setX model model.iron1, iron1 = setY model model.iron1, length = 12 }, Cmd.none )
                else if isThisCollision model.iron2 model then
                    ( { model | score = model.score + 70, collision = isCollision model.iron2 model, iron2 = setX model model.iron2, iron2 = setY model model.iron2, length = 12 }, Cmd.none )
                else if isThisCollision model.iron3 model then
                    ( { model | score = model.score + 70, collision = isCollision model.iron3 model, iron3 = setX model model.iron3, iron3 = setY model model.iron3, length = 12 }, Cmd.none )
                else if isThisCollision model.silver1 model then
                    ( { model | score = model.score + 200, collision = isCollision model.silver1 model, silver1 = setX model model.silver1, silver1 = setY model model.silver1, length = 12 }, Cmd.none )
                else if isThisCollision model.silver2 model then
                    ( { model | score = model.score + 200, collision = isCollision model.silver2 model, silver2 = setX model model.silver2, silver2 = setY model model.silver2, length = 12 }, Cmd.none )
                else if isThisCollision model.silver3 model then
                    ( { model | score = model.score + 200, collision = isCollision model.silver3 model, silver3 = setX model model.silver3, silver3 = setY model model.silver3, length = 12 }, Cmd.none )
                else if isThisCollision model.gold1 model then
                    ( { model | score = model.score + 500, collision = isCollision model.gold1 model, gold1 = setX model model.gold1, gold1 = setY model model.gold1, length = 12 }, Cmd.none )
                else if isThisCollision model.gold2 model then
                    ( { model | score = model.score + 500, collision = isCollision model.gold2 model, gold2 = setX model model.gold2, gold2 = setY model model.gold2, length = 12 }, Cmd.none )
                else if isThisCollision model.gold3 model then
                    ( { model | score = model.score + 500, collision = isCollision model.gold3 model, gold3 = setX model model.gold3, gold3 = setY model model.gold3, length = 12 }, Cmd.none )
                else if isThisCollision2 model.snitch1 model then
                    ( { model | score = model.score + 700, collision = isCollision2 model.snitch1 model, snitch1 = setX2 model model.snitch1, snitch1 = setY2 model model.snitch1, length = 12 }, Cmd.none )
                else if isThisCollision2 model.snitch2 model then
                    ( { model | score = model.score + 700, collision = isCollision2 model.snitch2 model, snitch2 = setX2 model model.snitch2, snitch2 = setY2 model model.snitch2, length = 12 }, Cmd.none )
                else
                    ( model, Cmd.none )
            else
                ( model, Cmd.none )

        UpdateSnitchX time ->
            ( { model | snitch1 = snitchX model.snitch1 model, snitch2 = snitchX model.snitch2 model }, Cmd.none )

        UpdateSnitchY time ->
            ( { model | snitch1 = snitchY model.snitch1 model, snitch2 = snitchY model.snitch2 model }, Cmd.none )

        GameState time ->
            if model.score >= model.goal then
                ( { model | state = Win }, Cmd.none )
            else if (floor (model.timeGame / 60)) > (model.levelTime + 1) then
                ( { model | state = Lose }, Cmd.none )
            else
                ( model, Cmd.none )

        NextLevel ->
            ( { model
                | state = Play
                , score = 0
                , goal = 3000
                , gold1 = { radius = 3, x = 40, y = 210 }
                , gold2 = { radius = 3, x = 160, y = 220 }
                , gold3 = { radius = 3, x = 220, y = 150 }
                , silver1 = { radius = 7, x = 70, y = 190 }
                , silver2 = { radius = 7, x = 120, y = 210 }
                , silver3 = { radius = 7, x = 30, y = 150 }
                , iron1 = { radius = 10, x = 180, y = 200 }
                , iron2 = { radius = 10, x = 60, y = 165 }
                , iron3 = { radius = 10, x = 220, y = 170 }
                , snitch1 = { radius = 5, x = 90, y = 50, xCenter = 90, yCenter = 50 }
                , snitch2 = { radius = 5, x = 170, y = 80, xCenter = 170, yCenter = 80 }
                , time = 0
                , timeGame = 0
                , length = 12
                , shoot = False
                , collision = False
                , level = 2
                , levelTime = 80
              }
            , Cmd.none
            )

        NewGame ->
            if model.level == 1 then
                ( { model
                    | state = Play
                    , score = 0
                    , goal = 1800
                    , gold1 = { radius = 3, x = 40, y = 210 }
                    , gold2 = { radius = 3, x = 160, y = 220 }
                    , gold3 = { radius = 3, x = 220, y = 150 }
                    , silver1 = { radius = 7, x = 70, y = 190 }
                    , silver2 = { radius = 7, x = 120, y = 210 }
                    , silver3 = { radius = 7, x = 30, y = 150 }
                    , iron1 = { radius = 10, x = 180, y = 200 }
                    , iron2 = { radius = 10, x = 60, y = 165 }
                    , iron3 = { radius = 10, x = 220, y = 170 }
                    , snitch1 = { radius = 5, x = -500, y = -500, xCenter = -500, yCenter = -500 }
                    , snitch2 = { radius = 5, x = -500, y = -500, xCenter = -500, yCenter = -500 }
                    , timeSnitch = 0
                    , time = 0
                    , timeGame = 0
                    , length = 12
                    , shoot = False
                    , collision = False
                    , level = 1
                    , levelTime = 60
                  }
                , Cmd.none
                )
            else
                ( { model
                    | state = Play
                    , score = 0
                    , goal = 3000
                    , gold1 = { radius = 3, x = 40, y = 210 }
                    , gold2 = { radius = 3, x = 160, y = 220 }
                    , gold3 = { radius = 3, x = 220, y = 150 }
                    , silver1 = { radius = 7, x = 70, y = 190 }
                    , silver2 = { radius = 7, x = 120, y = 210 }
                    , silver3 = { radius = 7, x = 30, y = 150 }
                    , iron1 = { radius = 10, x = 180, y = 200 }
                    , iron2 = { radius = 10, x = 60, y = 165 }
                    , iron3 = { radius = 10, x = 220, y = 170 }
                    , snitch1 = { radius = 5, x = 90, y = 50, xCenter = 90, yCenter = 50 }
                    , snitch2 = { radius = 5, x = 170, y = 80, xCenter = 170, yCenter = 80 }
                    , timeSnitch = 0
                    , time = 0
                    , timeGame = 0
                    , length = 12
                    , shoot = False
                    , collision = False
                    , level = 2
                    , levelTime = 80
                  }
                , Cmd.none
                )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Keyboard.downs Collide
        , Time.every second GameState
        , Keyboard.downs KeyDown
        , Keyboard.ups KeyUp
        , AnimationFrame.times TimeUpdate
        , AnimationFrame.diffs TimeGame
        , AnimationFrame.times TimeUpdate2
        , AnimationFrame.times UpdateSnitchX
        , AnimationFrame.times UpdateSnitchY
        ]



-- VIEW


view : Model -> Html Msg
view model =
    if model.state == Play then
        div []
            [ gameWorld model
            , p [ height "500", width "500" ] [ Html.text "Punkte: " ]
            , p [] [ Html.text (toString model.score ++ "/ " ++ toString model.goal) ]
            , p [] [ Html.text "Zeit: " ]
            , p [] [ Html.text (toString ((model.levelTime - (floor (Time.inSeconds model.timeGame * 16))))) ]
            , p [] [ Html.text "Eisen = 70 Punkte ///// Silber = 200 Punkte ///// Gold = 500 Punkte ///// Goldener Snitch = 700 Punkte" ]
            , p [] [ Html.text "Leertaste gedrückt halten um zu schießen." ]
            ]
    else if model.state == Lose then
        div []
            [ p [] [ Html.text "YOU LOST" ]
            , button [ onClick NewGame ] [ Html.text "Neuer Versuch" ]
            ]
    else if model.level == 1 then
        div []
            [ p [] [ Html.text "YOU WON" ]
            , button [ onClick NextLevel ] [ Html.text "Nächstes Level" ]
            ]
    else
        div []
            [ p [] [ Html.text "YOU WON" ]
            , button [ onClick NewGame ] [ Html.text "Nochmal spielen" ]
            ]


gameWorld model =
    Svg.svg [ viewBox "0 0 241 241", width "600px" ]
        [ line [ x1 "0", y1 "120", x2 "240", y2 "120", stroke "Black" ] []
        , line [ x1 "0", y1 "0", x2 "0", y2 "240", stroke "Black" ] []
        , line [ x1 "240", y1 "0", x2 "240", y2 "240", stroke "Black" ] []
        , line [ x1 "0", y1 "241", x2 "240", y2 "240", stroke "Black" ] []
        , line [ x1 "0", y1 "0", x2 "240", y2 "0", stroke "Black" ] []
        , rect [ x "0", y "0", width "240", height "120", fill "#0CB3EB" ] []
        , rect [ x "0", y "120", width "240", height "120", fill "#4A3103" ] []
        , circle [ cx "120", cy "120", r "8", fill "Black" ] []
        , line [ x1 "120", y1 "120", x2 (toString (handX model)), y2 (toString (handY model)), stroke "Black" ] []
        , circle [ cx (getXSnitch model.snitch1), cy (getYSnitch model.snitch1), r (getRSnitch model.snitch1), fill "#E7CB2A" ] []
        , circle [ cx (getXSnitch model.snitch2), cy (getYSnitch model.snitch2), r (getRSnitch model.snitch2), fill "#E7CB2A" ] []
        , circle [ cx (getX model.gold1), cy (getY model.gold1), r (getR model.gold1), fill "#E7CB2A" ] []
        , circle [ cx (getX model.gold2), cy (getY model.gold2), r (getR model.gold2), fill "#E7CB2A" ] []
        , circle [ cx (getX model.gold3), cy (getY model.gold3), r (getR model.gold3), fill "#E7CB2A" ] []
        , circle [ cx (getX model.silver1), cy (getY model.silver1), r (getR model.silver1), fill "#EBEBE7" ] []
        , circle [ cx (getX model.silver2), cy (getY model.silver2), r (getR model.silver2), fill "#EBEBE7" ] []
        , circle [ cx (getX model.silver3), cy (getY model.silver3), r (getR model.silver3), fill "#EBEBE7" ] []
        , circle [ cx (getX model.iron1), cy (getY model.iron1), r (getR model.iron1), fill "#52514A" ] []
        , circle [ cx (getX model.iron2), cy (getY model.iron2), r (getR model.iron2), fill "#52514A" ] []
        , circle [ cx (getX model.iron3), cy (getY model.iron3), r (getR model.iron3), fill "#52514A" ] []
        ]
