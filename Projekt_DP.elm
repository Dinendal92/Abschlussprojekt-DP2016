module Main exposing (..)

import Html exposing (..)
import Html.Events exposing (..)
import Html.App as Html
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Time exposing (Time, second)
import Keyboard exposing (KeyCode)
import AnimationFrame exposing (..)
import List exposing (..)
import Cmd.Extra exposing (..)
import List.Extra exposing (..)


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
    , bludger : List Snitch
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
    level1


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


snitchPos : List Snitch -> Model -> List Snitch
snitchPos list model =
    let
        getSnitch =
            case getAt 0 list of
                Just y ->
                    y

                Nothing ->
                    { radius = 0, x = 0, y = 0, xCenter = 0, yCenter = 0 }
    in
        case list of
            [] ->
                []

            _ ->
                case getRSnitch getSnitch of
                    "4" ->
                        [ { radius = getSnitch.radius
                          , x = (getSnitch.xCenter + 20 * cos (angle2 model))
                          , y = (getSnitch.yCenter + 20 * sin (angle2 model))
                          , xCenter = getSnitch.xCenter
                          , yCenter = getSnitch.yCenter
                          }
                        ]
                            ++ snitchPos (removeAt 0 list) model

                    _ ->
                        [ { radius = getSnitch.radius
                          , x = (getSnitch.xCenter + 20 * cos (angle2 model))
                          , y = (getSnitch.yCenter + 40 * sin (angle2 model))
                          , xCenter = getSnitch.xCenter
                          , yCenter = getSnitch.yCenter
                          }
                        ]
                            ++ snitchPos (removeAt 0 list) model


type Msg
    = TimeUpdate Time
    | TimeUpdate2 Time
    | TimeGame Time
    | KeyDown KeyCode
    | KeyUp KeyCode
    | Collide
    | UpdateSnitchPos Time
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


collisionWhere : Model -> List Mineral -> List Mineral -> ( Bool, Int )
collisionWhere model minerals list =
    let
        getMineral list =
            case getAt 0 list of
                Just y ->
                    y

                Nothing ->
                    { radius = 0, x = 0, y = 0 }
    in
        case list of
            [] ->
                ( False, 0 )

            _ ->
                case isThisCollision (getMineral list) model of
                    True ->
                        ( True, ((length minerals) - (length list)) )

                    False ->
                        collisionWhere model minerals (removeAt 0 list)


collisionWhere2 : Model -> List Snitch -> List Snitch -> ( Bool, Int )
collisionWhere2 model snitches list =
    let
        getSnitch list =
            case getAt 0 list of
                Just y ->
                    y

                Nothing ->
                    { radius = 0, x = 0, y = 0, xCenter = 0, yCenter = 1 }
    in
        case list of
            [] ->
                ( False, 0 )

            _ ->
                case isThisCollision2 (getSnitch list) model of
                    True ->
                        ( True, ((length snitches) - (length list)) )

                    False ->
                        collisionWhere2 model snitches (removeAt 0 list)


update : Msg -> Model -> ( Model, Cmd Msg )
update action model =
    case action of
        TimeGame time ->
            if model.state == Lose then
                ( { model | timeGame = 0 }, Cmd.none )
            else if (floor (model.timeGame / 60)) > (model.levelTime + 1) then
                ( { model | state = Lose }, Cmd.none )
            else
                ( { model | timeGame = model.timeGame + 1 }, Cmd.Extra.message Collide )

        TimeUpdate newTime ->
            if model.shoot then
                ( model, Cmd.none )
            else
                ( { model | time = newTime }, Cmd.Extra.message Collide )

        TimeUpdate2 newTime ->
            ( { model | timeSnitch = newTime }, Cmd.Extra.message Collide )

        KeyDown keyCode ->
            ( keyDown keyCode model, Cmd.Extra.message Collide )

        KeyUp keyCode ->
            ( keyUp keyCode model, Cmd.none )

        Collide ->
            if model.score >= model.goal then
                ( { model | state = Win }, Cmd.none )
            else if fst (collisionWhere model model.iron model.iron) then
                ( { model | score = model.score + 70, collision = True, iron = removeAt (snd (collisionWhere model model.iron model.iron)) model.iron, length = 12 }, Cmd.none )
            else if fst (collisionWhere model model.silver model.silver) then
                ( { model | score = model.score + 200, collision = True, silver = removeAt (snd (collisionWhere model model.silver model.silver)) model.silver, length = 12 }, Cmd.none )
            else if fst (collisionWhere model model.gold model.gold) then
                ( { model | score = model.score + 500, collision = True, gold = removeAt (snd (collisionWhere model model.gold model.gold)) model.gold, length = 12 }, Cmd.none )
            else if fst (collisionWhere2 model model.snitch model.snitch) then
                ( { model | score = model.score + 700, collision = True, snitch = removeAt (snd (collisionWhere2 model model.snitch model.snitch)) model.snitch, length = 12 }, Cmd.none )
            else if fst (collisionWhere2 model model.bludger model.bludger) then
                ( { model | score = model.score - 100, collision = True, bludger = removeAt (snd (collisionWhere2 model model.bludger model.bludger)) model.bludger, length = 12 }, Cmd.none )
            else
                ( model, Cmd.none )

        UpdateSnitchPos time ->
            if model.snitch == [] && model.bludger == [] then
                ( model, Cmd.none )
            else
                ( { model | snitch = snitchPos model.snitch model, bludger = snitchPos model.bludger model }, Cmd.Extra.message Collide )

        NextLevel ->
            ( (case getAt model.level levels of
                Just y ->
                    y

                Nothing ->
                    model
              )
            , Cmd.none
            )

        NewGame ->
            ( (case getAt (model.level - 1) levels of
                Just y ->
                    y

                Nothing ->
                    model
              )
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
        , AnimationFrame.times UpdateSnitchPos
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
            , p [] [ Html.text "Eisen = 70 Punkte ///// Silber = 200 Punkte ///// Gold = 500 Punkte ///// Goldener Snitch = 700 Punkte ///// Bludger = - 100 Punkte  " ]
            , p [] [ Html.text "Leertaste gedrückt halten um zu schießen." ]
            ]
    else if model.state == Lose then
        div []
            [ p [] [ Html.text "Level nicht geschafft." ]
            , button [ onClick NewGame ] [ Html.text "Neuer Versuch" ]
            ]
    else if model.level == length levels then
        div []
            [ p [] [ Html.text "Alle Level geschafft." ]
            , button [ onClick NewGame ] [ Html.text "Letztes Level wiederholen" ]
            ]
    else
        div []
            [ p [] [ Html.text "Level geschafft." ]
            , button [ onClick NextLevel ] [ Html.text "Nächstes Level" ]
            ]


drawMineral list =
    let
        getMineral =
            case getAt 0 list of
                Just y ->
                    y

                Nothing ->
                    { radius = 0, x = 0, y = 0 }
    in
        case list of
            [] ->
                []

            _ ->
                [ circle
                    [ cx (getX getMineral)
                    , cy (getY getMineral)
                    , r (getR getMineral)
                    , fill
                        (case getR (getMineral) of
                            "10" ->
                                "#52514A"

                            "7" ->
                                "#EBEBE7"

                            _ ->
                                "#E7CB2A"
                        )
                    ]
                    []
                ]
                    ++ drawMineral (removeAt 0 list)


drawSnitch list =
    let
        getSnitch =
            case getAt 0 list of
                Just y ->
                    y

                Nothing ->
                    { radius = 0, x = 0, y = 0, xCenter = 0, yCenter = 0 }
    in
        case list of
            [] ->
                []

            _ ->
                [ circle
                    [ cx (getXSnitch getSnitch)
                    , cy (getYSnitch (getSnitch))
                    , r (getRSnitch (getSnitch))
                    , fill
                        (case getRSnitch (getSnitch) of
                            "4" ->
                                "#E7CB2A"

                            _ ->
                                "Black"
                        )
                    ]
                    []
                ]
                    ++ drawSnitch (removeAt 0 list)


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
            , drawMineral model.iron
            , drawMineral model.silver
            , drawMineral model.gold
            , drawSnitch model.snitch
            , drawSnitch model.bludger
            ]
        )



-- Levels


levels : List Model
levels =
    [ level1, level2, level3 ]


level1 : Model
level1 =
    { state = Play
    , score = 0
    , goal = 1800
    , gold = [ { radius = 3, x = 40, y = 210 }, { radius = 3, x = 160, y = 220 }, { radius = 3, x = 220, y = 150 } ]
    , silver = [ { radius = 7, x = 70, y = 190 }, { radius = 7, x = 120, y = 210 }, { radius = 7, x = 30, y = 150 } ]
    , iron = [ { radius = 10, x = 180, y = 200 }, { radius = 10, x = 60, y = 165 }, { radius = 10, x = 220, y = 170 } ]
    , snitch = []
    , bludger = []
    , timeSnitch = 0
    , time = 0
    , timeGame = 0
    , length = 12
    , shoot = False
    , collision = False
    , level = 1
    , levelTime = 60
    }


level2 : Model
level2 =
    { state = Play
    , score = 0
    , goal = 3000
    , gold = [ { radius = 3, x = 40, y = 210 }, { radius = 3, x = 160, y = 220 }, { radius = 3, x = 220, y = 150 } ]
    , silver = [ { radius = 7, x = 70, y = 190 }, { radius = 7, x = 120, y = 210 }, { radius = 7, x = 30, y = 150 } ]
    , iron = [ { radius = 10, x = 180, y = 200 }, { radius = 10, x = 60, y = 165 }, { radius = 10, x = 220, y = 170 } ]
    , snitch = [ { radius = 4, x = 90, y = 50, xCenter = 90, yCenter = 50 }, { radius = 4, x = 170, y = 80, xCenter = 170, yCenter = 80 } ]
    , bludger = []
    , timeSnitch = 0
    , time = 0
    , timeGame = 0
    , length = 12
    , shoot = False
    , collision = False
    , level = 2
    , levelTime = 70
    }


level3 : Model
level3 =
    { state = Play
    , score = 0
    , goal = 3000
    , gold = [ { radius = 3, x = 40, y = 210 }, { radius = 3, x = 160, y = 220 }, { radius = 3, x = 220, y = 150 } ]
    , silver = [ { radius = 7, x = 70, y = 190 }, { radius = 7, x = 120, y = 210 }, { radius = 7, x = 30, y = 150 } ]
    , iron = [ { radius = 10, x = 180, y = 200 }, { radius = 10, x = 60, y = 165 }, { radius = 10, x = 220, y = 170 } ]
    , snitch = [ { radius = 4, x = 90, y = 50, xCenter = 90, yCenter = 50 }, { radius = 4, x = 170, y = 80, xCenter = 170, yCenter = 80 } ]
    , bludger = [ { radius = 5, x = 90, y = 45, xCenter = 90, yCenter = 45 }, { radius = 5, x = 175, y = 80, xCenter = 175, yCenter = 80 } ]
    , timeSnitch = 0
    , time = 0
    , timeGame = 0
    , length = 12
    , shoot = False
    , collision = False
    , level = 3
    , levelTime = 70
    }
