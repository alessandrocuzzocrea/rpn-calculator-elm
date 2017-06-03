module RPNCalc exposing (..)

import Html exposing (text, beginnerProgram, div, button, br, input)
import Html.Events exposing (onClick)
import Html.Attributes exposing (readonly, value)


main =
    Html.beginnerProgram { model = model, view = view, update = update }


type alias Model =
    { x : Float
    , y : Float
    , z : Float
    , t : Float
    , afterEnter : Bool
    , afterOperation : Bool
    }


model : Model
model =
    Model 0 0 0 0 False False


op f { x, y, z, t, afterEnter, afterOperation } =
    let
        newX =
            f y x
    in
        Model newX z t t False True


enter { x, y, z, t, afterEnter, afterOperation } =
    Model x x y z True False


appendDigit n { x, y, z, t, afterEnter, afterOperation } =
    let
        newX =
            if afterEnter then
                n
            else
                x * 10 + n
    in
        if afterOperation then
            Model n x y z False False
        else
            Model newX y z t False False


clearX m =
    { m | x = 0 }


clearFlags m =
    { m | afterEnter = False, afterOperation = False }


type Msg
    = Operation (Float -> Float -> Float)
    | Enter
    | Clear
    | Reset
    | AppendDigit Float


update msg model =
    case msg of
        Operation f ->
            op f model

        Enter ->
            enter model

        Clear ->
            model
                |> clearX
                |> clearFlags

        Reset ->
            Model 0 0 0 0 False False

        AppendDigit n ->
            appendDigit n model


view model =
    div []
        [ text (toString model)
        , br [] []
        , input [ readonly True, value (toString model.t) ] []
        , br [] []
        , input [ readonly True, value (toString model.z) ] []
        , br [] []
        , input [ readonly True, value (toString model.y) ] []
        , br [] []
        , input [ readonly True, value (toString model.x) ] []
        , br [] []
        , button [ onClick (AppendDigit 7) ] [ text "7" ]
        , button [ onClick (AppendDigit 8) ] [ text "8" ]
        , button [ onClick (AppendDigit 9) ] [ text "9" ]
        , br [] []
        , button [ onClick (AppendDigit 4) ] [ text "4" ]
        , button [ onClick (AppendDigit 5) ] [ text "5" ]
        , button [ onClick (AppendDigit 6) ] [ text "6" ]
        , br [] []
        , button [ onClick (AppendDigit 1) ] [ text "1" ]
        , button [ onClick (AppendDigit 2) ] [ text "2" ]
        , button [ onClick (AppendDigit 3) ] [ text "3" ]
        , br [] []
        , button [ onClick (AppendDigit 0) ] [ text "0" ]
        , br [] []
        , button [ onClick (Operation (+)) ] [ text "+" ]
        , button [ onClick (Operation (-)) ] [ text "-" ]
        , button [ onClick (Operation (*)) ] [ text "*" ]
        , button [ onClick (Operation (/)) ] [ text "/" ]
        , br [] []
        , button [ onClick Enter ] [ text "Enter" ]
        , button [ onClick Clear ] [ text "Clear" ]
        , button [ onClick Reset ] [ text "Reset" ]
        ]
