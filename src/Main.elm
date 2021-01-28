module Main exposing (..)

import Browser
import File exposing (File)
import File.Download as Download
import File.Select
import Json.Decode as D
import Task

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)


import Element as E
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Element.Region as Region

{-

   TODO



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


type alias Conversion =
    { file : File
    , questions : List Question
    , errors : List String
    , result : String
    }


newConversion : File -> Conversion
newConversion file =
    { file = file
    , questions = []
    , errors = []
    , result = ""
    }


type alias Model =
    { log : List String
    , files : List File
    , sep : String
    , conversions : List Conversion
    }


init : () -> ( Model, Cmd Msg )
init _ =
    let
        initialModel : Model
        initialModel =
            { log = []
            , files = []
            , sep = "\t"
            , conversions = []
            }
    in
    ( initialModel, Cmd.none )



-- UPDATE


type Msg
    = RequestFiles
    | GotFiles File (List File)
    | ClickedConvert
    | ClickedDownload
    | CsvLoaded File String
    | ChangedSeperator String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        RequestFiles ->
            ( model, requestFiles )

        GotFiles file otherFiles ->
            ( { model
                | files = file :: otherFiles
                , log = model.log ++ [ "loaded files" ]
              }
            , Cmd.none
            )

        ChangedSeperator newSep ->
            ( { model | sep = newSep, log = model.log ++ [ "changed sep to " ++ newSep ] }, Cmd.none )

        ClickedConvert ->
            let
                cmd =
                    Cmd.batch (List.map readFile model.files)
            in
            ( model, cmd )

        ClickedDownload ->
            let
                downloadCmds =
                    Cmd.batch
                        (List.indexedMap
                            (\i c -> Download.string ("questions" ++ String.fromInt i ++ ".xml") "text/xml" c.result)
                            model.conversions
                        )
            in
            ( model, downloadCmds )

        CsvLoaded file content ->
            let
                ( questions, errors ) =
                    parseCsvMultipleChoiceFile model.sep True content

                moodleXml =
                    buildMoodleXml questions
            in
            ( { model
                | conversions = model.conversions ++ [ Conversion file questions errors moodleXml ]
                , log = model.log ++ [ "converted:" ++ moodleXml ] ++ [ "\nerrors:" ++ String.join "\n" errors ]
              }
            , Cmd.none
            )


requestFiles : Cmd Msg
requestFiles =
    File.Select.files [ "text" ] GotFiles


buildMoodleXml : List Question -> String
buildMoodleXml questions =
    String.replace "{{questions}}" (String.join "" (List.map multipleChoiceQuestionTemplate questions)) quizTemplate


readFile : File -> Cmd Msg
readFile file =
    Task.perform (CsvLoaded file) (File.toString file)


parseCsvMultipleChoiceFile : String -> Bool -> String -> ( List Question, List String )
parseCsvMultipleChoiceFile sep hasHeader text =
    let
        lines =
            String.split "\n" text

        content : List String
        content =
            if hasHeader then
                case List.tail lines of
                    Nothing ->
                        []

                    Just l ->
                        l

            else
                lines

        resultQuestions : List (Result String Question)
        resultQuestions =
            List.map (\l -> parseSingleCsvLine sep l) content

        appendQuestionError : Result String Question -> ( List Question, List String ) -> ( List Question, List String )
        appendQuestionError =
            \resultQ ( qList, eList ) ->
                case resultQ of
                    Err error ->
                        ( qList, error :: eList )

                    Ok q ->
                        ( q :: qList, eList )

        res =
            List.foldr appendQuestionError ( [], [] ) resultQuestions
    in
    res


parseSingleCsvLine : String -> String -> Result String Question
parseSingleCsvLine sep line =
    let
        fields =
            String.split sep line

        result =
            case fields of
                question :: correct :: answers ->
                    case String.toInt correct of
                        Nothing ->
                            Err <| "could not convert correct answer to a number in this line [" ++ line ++ "]"

                        Just correctInt ->
                            Ok (Question question correctInt answers)

                _ ->
                    Err <| "missing fields in this line [" ++ line ++ "]"
    in
    result


type alias Question =
    { question : String
    , correct : Int
    , answers : List String
    }


multipleChoiceAnswerTemplate : String -> Bool -> String
multipleChoiceAnswerTemplate text correct =
    """
     <answer fraction=\""""
        ++ (if correct then
                "100"

            else
                "0"
           )
        ++ """">
       <text>"""
        ++ text
        ++ """</text>
       <feedback><text>"""
        ++ (if correct then
                "Correcto!"

            else
                "Incorrecto."
           )
        ++ """</text></feedback>
     </answer>
"""


multipleChoiceQuestionTemplate : Question -> String
multipleChoiceQuestionTemplate question =
    let
        answerLst =
            List.indexedMap (\i a -> multipleChoiceAnswerTemplate a (i == question.correct - 1)) question.answers

        answersStr =
            String.join "" answerLst
    in
    """
<question type="multichoice">
     <name>
         <text>""" ++ question.question ++ """</text>
     </name>
     <questiontext format="html">
         <text>""" ++ question.question ++ """</text>
     </questiontext>
""" ++ answersStr ++ """
 </question>
"""


quizTemplate : String
quizTemplate =
    """<?xml version="1.0" ?>
<quiz>
{{questions}}
</quiz>
"""



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW

blue =
    E.rgb255 238 238 238

purple =
    E.rgb255 238 238 150



view : Model -> Html Msg
view model =
    E.layout
        [ Font.size 20
        ]
        <|
        E.column
            [ E.centerX
            , E.centerY
            ]
            <|
            List.append
            [ E.el [Region.heading 1, Font.size 40] (E.text "Easy Moodle Questions")
            , uploadFilesButton
            , chooseSep model
            , E.row [ E.centerX]
                [
                 Input.button
                    [ E.centerX
                    , Background.color blue
                    , E.padding 10
                    ]
                    { onPress = Just ClickedConvert
                    , label = E.text "Convert"
                    }
                , Input.button
                    [ E.centerX
                    , Background.color blue
                    , E.padding 10
                    ]
                    { onPress = Just ClickedDownload
                    , label = E.text "Download"
                  }
                ]
            ]
            (List.map (\info -> E.text info) model.log )



uploadFilesButton : E.Element Msg
uploadFilesButton =
    Input.button
        [ Background.color blue
        , E.focused
              [ Background.color purple ]
        , E.centerX
        , E.padding 10
        ]
    { onPress = Just RequestFiles
    , label = E.text "Upload files"
    }
    
    
    
chooseSep : Model -> E.Element Msg
chooseSep model =
    Input.radio
        [ E.padding 10
        , E.spacing 20
        ]
    { onChange = ChangedSeperator
    , selected = Just model.sep
    , label = Input.labelAbove [] (E.text "Choose seperator between fields:")
    , options =
          [ Input.option "\t" (E.text "Tab")
          , Input.option "," (E.text ",")
          , Input.option "custom" (E.text "custom")
          ]
    }
    
    
view2 : Model -> Html Msg
view2 model =
    div []
        [ div [ id "log" ]
              (List.map (\info -> p [] [ text info ]) model.log ++ [ p [] [ text (Debug.toString model) ] ])
        , header
        , middle model
        , reviewView model
        , footer
        ]


sepSelector : Model -> Html Msg
sepSelector model =
    div []
        [ select [ onInput ChangedSeperator ]
            [ option [ value "," ] [ text "comma ," ]
            , option [ value "\t" ] [ text "tab " ]
            ]
        ]


reviewConvertionResult : String -> List (Html Msg)
reviewConvertionResult res =
    []


reviewConvertion : { file : File, result : String } -> Html Msg
reviewConvertion c =
    div []
        [ h2 [] [ text (File.name c.file) ]
        ]


reviewView : Model -> Html Msg
reviewView model =
    div [] []


header : Html Msg
header =
    div [ id "header" ]
        [ h1 [ id "headerTitle" ] [ text "Easy Moodle Questions" ]
        ]


middle : Model -> Html Msg
middle model =
    div [ id "content" ]
        [ button
            [ onClick RequestFiles ]
            [ text "upload files" ]

        --         , input
        --              [ type_ "file"
        --              , multiple True
        --              , on "change" (D.map GotFiles filesDecoder)
        --              ]
        --              []
        , div []
            [ label [] [ text "separator, e.g. , or tab" ]
            , input [ type_ "text", value model.sep, onInput ChangedSeperator ] []
            ]
        , button [ onClick ClickedConvert ] [ text "convert" ]
        , button [ onClick ClickedDownload ] [ text "download" ]
        ]


footer : Html Msg
footer =
    div [ id "footer" ]
        [ text "diogoaos"
        ]


filesDecoder : D.Decoder (List File)
filesDecoder =
    D.at [ "target", "files" ] (D.list File.decoder)
