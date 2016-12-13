port module Main exposing (..)

import Html exposing (Html, Attribute, programWithFlags, div, text, button, h2, i)
import Html.Events exposing (onClick)
import Html.Attributes exposing (class)


init : Flags -> (Model, Cmd Msg)
init flags =
    ( Model flags.counts, Cmd.none )


type alias Flags =
    { counts : List Int
    }


type alias Model =
    { counts : List Int
    }


type Msg
    = Increment Int
    | Decrement Int
    | AddButton
    | RemoveButton Int


view : Model -> Html Msg
view model =
    viewCounters model.counts


viewCounters : List Int -> Html Msg
viewCounters counts =
    div [ class "text-center" ]
        ( [ viewButton [ onClick AddButton ] [ text "Add Button" ] ]
        ++ ( List.indexedMap viewCounter counts )
        )


viewCounter : Int -> Int -> Html Msg
viewCounter index count =
    div []
        [ h2 [] [ text (toString count) ]
        , viewButtonGroup
            [ viewButton
                [ onClick (Decrement index) ]
                [ viewGlyphicon "minus" ]
            , viewButton
                [ onClick (Increment index) ]
                [ viewGlyphicon "plus" ]
            , viewButton
                [ onClick (RemoveButton index) ]
                [ viewGlyphicon "remove" ]
            ]
        ]


viewGlyphicon : String -> Html Msg
viewGlyphicon name =
   i [ class ("glyphicon glyphicon-" ++ name) ] []


viewButtonGroup : List (Html Msg) -> Html Msg
viewButtonGroup buttons =
    div [ class "btn-group" ] buttons


viewButton : List (Attribute Msg) -> List (Html Msg) -> Html Msg
viewButton attributes children =
    button
        ( [ class "btn btn-default" ] ++ attributes )
        children


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        Increment index ->
            saveCounts ( { model | counts = mapIndex ((+) 1) index model.counts }, Cmd.none )

        Decrement index ->
            saveCounts ( { model | counts = mapIndex ((+) -1) index model.counts }, Cmd.none )

        AddButton ->
            saveCounts ( { model | counts = model.counts ++ [0] }, Cmd.none )

        RemoveButton index ->
            saveCounts ( { model | counts = removeIndex index model.counts }, Cmd.none )


saveCounts : (Model, Cmd Msg) -> (Model, Cmd Msg)
saveCounts tuple =
    ( Tuple.first tuple, save (Tuple.first tuple).counts )


mapIndex : (a -> a) -> Int -> List a -> List a
mapIndex f index list =
    List.indexedMap
        (\i element ->
            if i == index then
                f element
            else
                element
        )
        list


removeIndex : Int -> List a -> List a
removeIndex index list =
    let
        front =
            List.take index list
        back =
            List.drop (index + 1) list
    in
        front ++ back


port save : List Int -> Cmd msg


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


main : Program Flags Model Msg
main =
    programWithFlags
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
