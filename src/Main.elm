module Main exposing (..)

import Browser
import File exposing (File)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as D

import Task

{-

TODO

- create downlaod functionality

-}

-- MAIN


main =
  Browser.element
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }



-- MODEL


type alias Model =
    { files : List File
    , sep : String
    , info : String
    }


initialModel : Model
initialModel =
    { files = []
    , sep = ","
    , info = ""
    }
    
init : () -> (Model, Cmd Msg)
init _ =
  ( initialModel, Cmd.none )



-- UPDATE


type Msg
    = GotFiles (List File)
    | ClickedConvert
    | CsvLoaded String
    | ChangedSeperator String


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        GotFiles files ->
            ( { model | files = files, info = "loaded files" }, Cmd.none )

        ChangedSeperator newSep ->
            ( { model | sep = newSep }, Cmd.none )

        ClickedConvert ->
            let
                cmd = Cmd.batch (List.map read model.files)
            in
                ( model, cmd)

        CsvLoaded content ->
            let
                converted = convertCsvMultipleChoice model.sep False content
            in
            ( { model | info = "converted:" ++ converted }, Cmd.none )

            
read : File -> Cmd Msg
read file =
    Task.perform CsvLoaded (File.toString file)


convertCsvMultipleChoice: String -> Bool -> String -> String
convertCsvMultipleChoice sep hasHeader text =
    let
        lines = String.split "\n" text
        content = if hasHeader then
                      case List.tail lines of
                          Nothing -> []
                          Just l -> l
                  else
                      lines
    in
        String.join "" (List.map (\l -> csvLineToQuestion sep l) content)

csvLineToQuestion : String -> String -> String
csvLineToQuestion sep line =
    let
        fields = String.split sep line
        result =
            case fields of
                (question :: correct :: answers) ->
                    case String.toInt correct of
                        Nothing -> ""
                        Just correctInt -> questionToXml question correctInt answers
                    
                _ -> ""
    in
        result

questionToXml : String -> Int -> List String -> String
questionToXml question correct answers =
    let
        
        answerLst = List.indexedMap (\ i a -> multipleChoiceAnswerTemplate a (i == correct-1)) answers
        answersStr = String.join "" answerLst

    in
        String.replace "{{answers}}" answersStr (multipleChoiceQuestionTemplate question)


multipleChoiceAnswerTemplate : String  -> Bool -> String
multipleChoiceAnswerTemplate text correct= """
     <answer fraction=\"""" ++ (if correct then "100" else "0" ) ++ """\">
       <text>""" ++ text ++ """</text>
       <feedback><text>""" ++ (if correct then "Correcto!" else "Incorrecto." ) ++ """</text></feedback>
     </answer>
"""
    


multipleChoiceQuestionTemplate : String -> String
multipleChoiceQuestionTemplate question = """
<question type="multichoice">
     <name>
         <text>""" ++ question ++ """</text>
     </name>
     <questiontext format="html">
         <text>""" ++ question ++ """</text>
     </questiontext>

     {{answers}}

 </question>
"""







-- SUBSCRIPTIONS
subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none



-- VIEW


view : Model -> Html Msg
view model =
  div []
    [ label [] [ text model.info ]
    , input
        [ type_ "file"
        , multiple True
        , on "change" (D.map GotFiles filesDecoder)
        ]
        []
    , div []
        [ label [] [ text "separator, e.g. , or tab" ]
        , input [ type_ "text", onInput ChangedSeperator ]  []
        ]
    , button [ onClick ClickedConvert ] [ text "convert" ]
    , div [] [ text (Debug.toString model) ]
    ]


filesDecoder : D.Decoder (List File)
filesDecoder =
  D.at ["target","files"] (D.list File.decoder)
