module Form.View.Elements
    exposing
        ( Config
        , Model
        , State(..)
        , Validation(..)
        , idle
        , view
        )

import Element exposing (..)
import Element.Attributes
import Element.Events
import Element.Input as Input
import Form exposing (Form)
import Form.Error as Error exposing (Error)
import Form.Field.Text
import Form.Value as Value
import Html exposing (Html)
import Set exposing (Set)
import Style
import Styles


type alias Model values =
    { values : values
    , state : State
    , showAllErrors : Bool
    , showFieldError : Set String
    }


type State
    = Idle
    | Loading
    | Error String


idle : values -> Model values
idle values =
    { values = values
    , state = Idle
    , showAllErrors = False
    , showFieldError = Set.empty
    }


type alias Config values msg =
    { onChange : Model values -> msg
    , actionMessage : String
    , loadingMessage : String
    , validation : Validation

    --, styles : StylesConfig
    }


type alias StylesConfig styles =
    { column : styles
    , row : styles
    }


type Validation
    = ValidateOnSubmit
    | ValidateOnBlur


view :
    Config values msg
    -> Form values msg
    -> Model values
    -> Element Styles.Styles variation msg
view { onChange, actionMessage, loadingMessage, validation } form model =
    let
        onSubmitMsg : Maybe msg
        onSubmitMsg =
            case Form.parser form model.values of
                Ok msg ->
                    if model.state == Loading then
                        Nothing
                    else
                        Just msg

                Err _ ->
                    if model.showAllErrors then
                        Nothing
                    else
                        Just (onChange { model | showAllErrors = True })

        onSubmit =
            onSubmitMsg
                |> Maybe.map (Element.Events.onSubmit >> List.singleton)
                |> Maybe.withDefault []

        onBlur : Maybe (String -> msg)
        onBlur =
            case validation of
                ValidateOnSubmit ->
                    Nothing

                ValidateOnBlur ->
                    Just (\label -> onChange { model | showFieldError = Set.insert label model.showFieldError })

        renderField =
            viewField
                { onChange = \values -> onChange { model | values = values }
                , onBlur = onBlur
                , disabled = model.state == Loading
                , showError = \label -> model.showAllErrors || Set.member label model.showFieldError
                }

        formError =
            let
                error =
                    case model.state of
                        Error error ->
                            Just error

                        _ ->
                            Nothing
            in
            Element.whenJust error text |> el Styles.None []

        submitButton =
            Element.button Styles.None
                (if onSubmitMsg == Nothing then
                    [ Element.Attributes.attribute "disabled" "" ]
                 else
                    []
                )
                (if model.state == Loading then
                    text loadingMessage
                 else
                    text actionMessage
                )
    in
    Element.node "form" <|
        Element.column Styles.None
            onSubmit
            ((Form.fields form model.values |> List.map renderField)
                ++ [ formError
                   , submitButton
                   ]
            )



-- FIELD


type alias FieldConfig values msg =
    { onChange : values -> msg
    , onBlur : Maybe (String -> msg)
    , disabled : Bool
    , showError : String -> Bool
    }


viewField : FieldConfig values msg -> ( Form.Field values, Maybe Error ) -> Element Styles.Styles variation msg
viewField { onChange, onBlur, disabled, showError } ( field, maybeError ) =
    let
        error label value =
            if showError label then
                Maybe.map errorToString maybeError
            else
                Nothing

        whenDirty value msg =
            if Value.isDirty value then
                msg
            else
                Nothing

        fieldBlur value label =
            whenDirty value (Maybe.map (\onBlur -> onBlur label) onBlur)
    in
    case field of
        Form.Text { type_, attributes, state } ->
            let
                config =
                    { onChange = state.update >> onChange
                    , onBlur = fieldBlur state.value attributes.label
                    , disabled = disabled
                    , label = attributes.label
                    , placeholder = attributes.placeholder
                    , value = Value.raw state.value |> Maybe.withDefault ""
                    , error = error attributes.label state.value
                    }
            in
            case type_ of
                Form.Field.Text.RawText ->
                    textField config

                Form.Field.Text.Password ->
                    passwordField config

                Form.Field.Text.Email ->
                    emailField config

                Form.Field.Text.TextArea ->
                    textArea config

        Form.Checkbox { attributes, state } ->
            checkboxField Styles.None
                { checked = Value.raw state.value |> Maybe.withDefault False
                , disabled = disabled
                , onChange = state.update >> onChange
                , onBlur = fieldBlur state.value attributes.label
                , label = attributes.label
                , error = error attributes.label state.value
                }

        Form.Radio { attributes, state } ->
            radioField Styles.None
                attributes.options
                { onChange = state.update >> onChange
                , onBlur = fieldBlur state.value attributes.label
                , selected = Value.raw state.value
                , label = attributes.label
                , disabled = disabled
                , error = error attributes.label state.value
                }

        Form.Select { attributes, state } ->
            radioField Styles.None
                attributes.options
                { onChange = state.update >> onChange
                , onBlur = fieldBlur state.value attributes.label
                , selected = Value.raw state.value
                , label = attributes.label
                , disabled = disabled
                , error = error attributes.label state.value
                }

        Form.Group label fields ->
            Element.column Styles.None
                []
                [ Element.whenJust label text |> Element.el Styles.FormGroupTitle []
                , Element.row Styles.None
                    []
                    (List.map
                        (viewField
                            { onChange = onChange
                            , onBlur = onBlur
                            , disabled = disabled
                            , showError = showError
                            }
                        )
                        fields
                    )
                ]


errorToString : Error -> String
errorToString error =
    case error of
        Error.EmptyField ->
            "This field is required"

        Error.ParserError parserError ->
            parserError



-- TEXT FIELD


type alias TextFieldConfig msg =
    { onChange : String -> msg
    , onBlur : Maybe msg
    , value : String
    , label : String
    , placeholder : String
    , disabled : Bool
    , error : Maybe String
    }


textField =
    inputField Input.text Styles.None


passwordField =
    inputField Input.newPassword Styles.None


emailField =
    inputField Input.email Styles.None


textArea =
    inputField Input.multiline Styles.None


inputField input style { onChange, onBlur, disabled, value, error, label, placeholder } =
    let
        attributes =
            [] |> blurEvent onBlur
    in
    input style
        attributes
        { onChange = onChange
        , value = value
        , label =
            Input.placeholder
                { label = Input.labelLeft (el Styles.None [] (text label))
                , text = placeholder
                }
        , options = [ fieldError error ] |> setDisabled disabled
        }



-- CHECKBOX FIELD


type alias CheckboxFieldConfig msg =
    { onChange : Bool -> msg
    , onBlur : Maybe msg
    , label : String
    , checked : Bool
    , disabled : Bool
    , error : Maybe String
    }


checkboxField style { onChange, onBlur, label, checked, disabled, error } =
    let
        attributes =
            [] |> blurEvent onBlur
    in
    Input.checkbox style
        attributes
        { onChange = onChange
        , label = el Styles.None [] (text label)
        , checked = checked
        , options = [ fieldError error ] |> setDisabled disabled
        }



-- RADIO FIELD


type alias RadioFieldConfig msg =
    { onChange : String -> msg
    , onBlur : Maybe msg
    , selected : Maybe String
    , label : String
    , disabled : Bool
    , error : Maybe String
    }


radioField style options { onChange, onBlur, selected, label, disabled, error } =
    let
        attributes =
            [] |> blurEvent onBlur
    in
    Input.radio style
        attributes
        { onChange = onChange
        , selected = selected
        , label = Input.labelAbove (text label)
        , options = [ fieldError error ] |> setDisabled disabled
        , choices =
            List.map
                (\( value, name ) ->
                    Input.choice value (text name)
                )
                options
        }



-- HELPERS


fieldError : Maybe String -> Input.Option Styles.Styles variation msg
fieldError error =
    Element.whenJust error text
        |> el Styles.None []
        |> Input.errorBelow


blurEvent onBlur attrs =
    Maybe.map (Element.Events.onBlur >> flip (::) attrs) onBlur
        |> Maybe.withDefault attrs


setDisabled disabled attrs =
    if disabled then
        Input.disabled :: attrs
    else
        attrs
