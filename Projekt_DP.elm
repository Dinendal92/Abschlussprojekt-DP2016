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
import List exposing (..)
import Cmd.Extra exposing (..)


main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


getMineral : List Mineral -> Int -> Mineral
getMineral list pos =
    case (head (drop (pos - 1) list)) of
        Just y ->
            y

        Nothing ->
            { radius = 0, x = 0, y = 0 }


getSnitch : List Snitch -> Int -> Snitch
getSnitch list pos =
    case (head (drop (pos - 1) list)) of
        Just y ->
            y

        Nothing ->
            { radius = 0, x = 0, y = 0, xCenter = 0, yCenter = 0 }



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


deleteMineral : List Mineral -> Int -> List Mineral
deleteMineral list pos =
    if length ((drop (pos - 1) list)) < 2 then
        (take (pos - 1) list)
    else
        (take (pos - 1) list)
            ++ case ((tail (drop (pos - 1) list))) of
                Just y ->
                    y

                Nothing ->
                    []


deleteSnitch : List Snitch -> Int -> List Snitch
deleteSnitch list pos =
    if length ((drop (pos - 1) list)) < 2 then
        (take (pos - 1) list)
    else
        (take (pos - 1) list)
            ++ case ((tail (drop (pos - 1) list))) of
                Just y ->
                    y

                Nothing ->
                    []


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
    , gold : List Mineral
    , silver : List Mineral
    , iron : List Mineral
    , snitch : List Snitch
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
    , gold = [ { radius = 3, x = 40, y = 210 }, { radius = 3, x = 160, y = 220 }, { radius = 3, x = 220, y = 150 } ]
    , silver = [ { radius = 7, x = 70, y = 190 }, { radius = 7, x = 120, y = 210 }, { radius = 7, x = 30, y = 150 } ]
    , iron = [ { radius = 10, x = 180, y = 200 }, { radius = 10, x = 60, y = 165 }, { radius = 10, x = 220, y = 170 } ]
    , snitch = []
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


snitchX : List Snitch -> Model -> List Snitch
snitchX list model =
    if list == [] then
        []
    else
        [ { radius = (getSnitch list 1).radius
          , x = ((getSnitch list 1).xCenter + 20 * cos (angle2 model))
          , y = (getSnitch list 1).y
          , xCenter = (getSnitch list 1).xCenter
          , yCenter = (getSnitch list 1).yCenter
          }
        ]
            ++ case (tail list) of
                Just y ->
                    snitchX y model

                Nothing ->
                    []


snitchY : List Snitch -> Model -> List Snitch
snitchY list model =
    if list == [] then
        []
    else
        [ { radius = (getSnitch list 1).radius
          , x = (getSnitch list 1).x
          , y = ((getSnitch list 1).yCenter + 20 * sin (angle2 model))
          , xCenter = (getSnitch list 1).xCenter
          , yCenter = (getSnitch list 1).yCenter
          }
        ]
            ++ case (tail list) of
                Just y ->
                    snitchY y model

                Nothing ->
                    []


type Msg
    = TimeUpdate Time
    | TimeUpdate2 Time
    | TimeGame Time
    | KeyDown KeyCode
    | KeyUp KeyCode
    | Collide
    | UpdateSnitchX Time
    | UpdateSnitchY Time
    | NewGame
    | NextLevel


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
            if (floor (model.timeGame / 60)) > (model.levelTime + 1) then
                ( { model | state = Lose }, Cmd.none )
            else
                ( { model | timeGame = model.timeGame + 1 }, Cmd.none )

        TimeUpdate newTime ->
            if model.shoot then
                ( model, Cmd.none )
            else
                ( { model | time = newTime }, Cmd.none )

        TimeUpdate2 newTime ->
            ( { model | timeSnitch = newTime }, Cmd.none )

        KeyDown keyCode ->
            ( keyDown keyCode model, Cmd.Extra.message Collide )

        KeyUp keyCode ->
            ( keyUp keyCode model, Cmd.none )

        Collide ->
            if isThisCollision (getMineral model.iron 1) model then
                ( { model | score = model.score + 70, collision = isCollision (getMineral model.iron 1) model, iron = deleteMineral model.iron 1, length = 12 }, Cmd.none )
            else if isThisCollision (getMineral model.iron 2) model then
                ( { model | score = model.score + 70, collision = isCollision (getMineral model.iron 2) model, iron = deleteMineral model.iron 2, length = 12 }, Cmd.none )
            else if isThisCollision (getMineral model.iron 3) model then
                ( { model | score = model.score + 70, collision = isCollision (getMineral model.iron 3) model, iron = deleteMineral model.iron 3, length = 12 }, Cmd.none )
            else if isThisCollision (getMineral model.silver 1) model then
                ( { model | score = model.score + 200, collision = isCollision (getMineral model.silver 1) model, silver = deleteMineral model.silver 1, length = 12 }, Cmd.none )
            else if isThisCollision (getMineral model.silver 2) model then
                ( { model | score = model.score + 200, collision = isCollision (getMineral model.silver 2) model, silver = deleteMineral model.silver 2, length = 12 }, Cmd.none )
            else if isThisCollision (getMineral model.silver 3) model then
                ( { model | score = model.score + 200, collision = isCollision (getMineral model.silver 3) model, silver = deleteMineral model.silver 3, length = 12 }, Cmd.none )
            else if isThisCollision (getMineral model.gold 1) model then
                ( { model | score = model.score + 500, collision = isCollision (getMineral model.gold 1) model, gold = deleteMineral model.gold 1, length = 12 }, Cmd.none )
            else if isThisCollision (getMineral model.gold 2) model then
                ( { model | score = model.score + 500, collision = isCollision (getMineral model.gold 2) model, gold = deleteMineral model.gold 2, length = 12 }, Cmd.none )
            else if isThisCollision (getMineral model.gold 3) model then
                ( { model | score = model.score + 500, collision = isCollision (getMineral model.gold 3) model, gold = deleteMineral model.gold 3, length = 12 }, Cmd.none )
            else if isThisCollision2 (getSnitch model.snitch 1) model then
                ( { model | score = model.score + 700, collision = isCollision2 (getSnitch model.snitch 1) model, snitch = deleteSnitch model.snitch 1, length = 12 }, Cmd.none )
            else if isThisCollision2 (getSnitch model.snitch 2) model then
                ( { model | score = model.score + 700, collision = isCollision2 (getSnitch model.snitch 2) model, snitch = deleteSnitch model.snitch 2, length = 12 }, Cmd.none )
            else if model.score >= model.goal then
                ( { model | state = Win }, Cmd.none )
            else
                ( model, Cmd.none )

        UpdateSnitchX time ->
            if model.snitch /= [] then
                ( { model | snitch = snitchX model.snitch model }, Cmd.none )
            else
                ( model, Cmd.none )

        UpdateSnitchY time ->
            if model.snitch /= [] then
                ( { model | snitch = snitchY model.snitch model }, Cmd.none )
            else
                ( model, Cmd.none )

        NextLevel ->
            ( { model
                | state = Play
                , score = 0
                , goal = 3000
                , gold = [ { radius = 3, x = 40, y = 210 }, { radius = 3, x = 160, y = 220 }, { radius = 3, x = 220, y = 150 } ]
                , silver = [ { radius = 7, x = 70, y = 190 }, { radius = 7, x = 120, y = 210 }, { radius = 7, x = 30, y = 150 } ]
                , iron = [ { radius = 10, x = 180, y = 200 }, { radius = 10, x = 60, y = 165 }, { radius = 10, x = 220, y = 170 } ]
                , snitch = [ { radius = 5, x = 90, y = 50, xCenter = 90, yCenter = 50 }, { radius = 5, x = 170, y = 80, xCenter = 170, yCenter = 80 } ]
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
                    , gold = [ { radius = 3, x = 40, y = 210 }, { radius = 3, x = 160, y = 220 }, { radius = 3, x = 220, y = 150 } ]
                    , silver = [ { radius = 7, x = 70, y = 190 }, { radius = 7, x = 120, y = 210 }, { radius = 7, x = 30, y = 150 } ]
                    , iron = [ { radius = 10, x = 180, y = 200 }, { radius = 10, x = 60, y = 165 }, { radius = 10, x = 220, y = 170 } ]
                    , snitch = []
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
                    , gold = [ { radius = 3, x = 40, y = 210 }, { radius = 3, x = 160, y = 220 }, { radius = 3, x = 220, y = 150 } ]
                    , silver = [ { radius = 7, x = 70, y = 190 }, { radius = 7, x = 120, y = 210 }, { radius = 7, x = 30, y = 150 } ]
                    , iron = [ { radius = 10, x = 180, y = 200 }, { radius = 10, x = 60, y = 165 }, { radius = 10, x = 220, y = 170 } ]
                    , snitch = [ { radius = 5, x = 90, y = 50, xCenter = 90, yCenter = 50 }, { radius = 5, x = 170, y = 80, xCenter = 170, yCenter = 80 } ]
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
        [ Keyboard.downs KeyDown
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


drawIron list =
    if list /= [] then
        [ circle [ cx (getX (getMineral list 1)), cy (getY (getMineral list 1)), r (getR (getMineral list 1)), fill "#52514A" ] [] ]
            ++ drawIron
                (case (tail (list)) of
                    Just y ->
                        y

                    Nothing ->
                        []
                )
    else
        []


drawSilver list =
    if list /= [] then
        [ circle [ cx (getX (getMineral list 1)), cy (getY (getMineral list 1)), r (getR (getMineral list 1)), fill "#EBEBE7" ] [] ]
            ++ drawSilver
                (case (tail (list)) of
                    Just y ->
                        y

                    Nothing ->
                        []
                )
    else
        []


drawGold list =
    if list /= [] then
        [ circle [ cx (getX (getMineral list 1)), cy (getY (getMineral list 1)), r (getR (getMineral list 1)), fill "#E7CB2A" ] [] ]
            ++ drawGold
                (case (tail (list)) of
                    Just y ->
                        y

                    Nothing ->
                        []
                )
    else
        []


drawSnitch list =
    if list /= [] then
        [ circle [ cx (getXSnitch (getSnitch list 1)), cy (getYSnitch (getSnitch list 1)), r (getRSnitch (getSnitch list 1)), fill "#E7CB2A" ] [] ]
            ++ drawSnitch
                (case (tail (list)) of
                    Just y ->
                        y

                    Nothing ->
                        []
                )
    else
        []


gameWorld model =
    Svg.svg [ viewBox "0 0 241 241", width "600px" ]
        (concat
            [ [ line [ x1 "0", y1 "120", x2 "240", y2 "120", stroke "Black" ] []
              , line [ x1 "0", y1 "0", x2 "0", y2 "240", stroke "Black" ] []
              , line [ x1 "240", y1 "0", x2 "240", y2 "240", stroke "Black" ] []
              , line [ x1 "0", y1 "241", x2 "240", y2 "240", stroke "Black" ] []
              , line [ x1 "0", y1 "0", x2 "240", y2 "0", stroke "Black" ] []
              , rect [ x "0", y "0", width "240", height "120", fill "#0CB3EB" ] []
              , rect [ x "0", y "120", width "240", height "120", fill "#4A3103" ] []
              , circle [ cx "120", cy "120", r "8", fill "Black" ] []
              , line [ x1 "120", y1 "120", x2 (toString (handX model)), y2 (toString (handY model)), stroke "Black" ] []
              ]
            , drawIron model.iron
            , drawSilver model.silver
            , drawGold model.gold
            , drawSnitch model.snitch
            ]
        )
