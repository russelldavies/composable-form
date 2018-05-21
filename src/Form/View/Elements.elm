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


type alias Config styles values msg =
    { onChange : Model values -> msg
    , actionMessage : String
    , loadingMessage : String
    , validation : Validation
    , styles : StylesConfig styles
    }


type alias StylesConfig styles =
    { default : styles
    , button : styles
    , textField : styles
    , checkboxField : styles
    , radioField : styles
    , error : styles
    , groupTitle : styles
    }


type Validation
    = ValidateOnSubmit
    | ValidateOnBlur


view :
    Config styles values msg
    -> Form values msg
    -> Model values
    -> Element styles variation msg
view { onChange, actionMessage, loadingMessage, validation, styles } form model =
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
                , styles = styles
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
            Element.whenJust error text |> el styles.default []

        submitButton =
            Element.button styles.button
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
        Element.column styles.default
            onSubmit
            ((Form.fields form model.values |> List.map renderField)
                ++ [ formError
                   , submitButton
                   ]
            )



-- FIELD


type alias FieldConfig values msg styles =
    { onChange : values -> msg
    , onBlur : Maybe (String -> msg)
    , disabled : Bool
    , showError : String -> Bool
    , styles : StylesConfig styles
    }


viewField { onChange, onBlur, disabled, showError, styles } ( field, maybeError ) =
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
                    , styles = styles
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
            checkboxField
                { checked = Value.raw state.value |> Maybe.withDefault False
                , disabled = disabled
                , onChange = state.update >> onChange
                , onBlur = fieldBlur state.value attributes.label
                , label = attributes.label
                , error = error attributes.label state.value
                , styles = styles
                }

        Form.Radio { attributes, state } ->
            radioField
                attributes.options
                { onChange = state.update >> onChange
                , onBlur = fieldBlur state.value attributes.label
                , selected = Value.raw state.value
                , label = attributes.label
                , disabled = disabled
                , error = error attributes.label state.value
                , styles = styles
                }

        Form.Select { attributes, state } ->
            radioField
                attributes.options
                { onChange = state.update >> onChange
                , onBlur = fieldBlur state.value attributes.label
                , selected = Value.raw state.value
                , label = attributes.label
                , disabled = disabled
                , error = error attributes.label state.value
                , styles = styles
                }

        Form.Group label fields ->
            Element.column styles.default
                []
                [ Element.whenJust label text |> Element.el styles.groupTitle []
                , Element.row styles.default
                    []
                    (List.map
                        (viewField
                            { onChange = onChange
                            , onBlur = onBlur
                            , disabled = disabled
                            , showError = showError
                            , styles = styles
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


type alias TextFieldConfig styles msg =
    { onChange : String -> msg
    , onBlur : Maybe msg
    , value : String
    , label : String
    , placeholder : String
    , disabled : Bool
    , error : Maybe String
    , styles : styles
    }


textField =
    inputField Input.text


passwordField =
    inputField Input.newPassword


emailField =
    inputField Input.email


textArea =
    inputField Input.multiline


inputField input { onChange, onBlur, disabled, value, error, label, placeholder, styles } =
    let
        attributes =
            [] |> blurEvent onBlur
    in
    input styles.textField
        attributes
        { onChange = onChange
        , value = value
        , label =
            Input.placeholder
                { label = Input.labelLeft (el styles.default [] (text label))
                , text = placeholder
                }
        , options = [ fieldError styles.error error ] |> setDisabled disabled
        }



-- CHECKBOX FIELD


type alias CheckboxFieldConfig styles msg =
    { onChange : Bool -> msg
    , onBlur : Maybe msg
    , label : String
    , checked : Bool
    , disabled : Bool
    , error : Maybe String
    , styles : styles
    }


checkboxField { onChange, onBlur, label, checked, disabled, error, styles } =
    let
        attributes =
            [] |> blurEvent onBlur
    in
    Input.checkbox styles.checkboxField
        attributes
        { onChange = onChange
        , label = el styles.default [] (text label)
        , checked = checked
        , options = [ fieldError styles.error error ] |> setDisabled disabled
        }



-- RADIO FIELD


type alias RadioFieldConfig styles msg =
    { onChange : String -> msg
    , onBlur : Maybe msg
    , selected : Maybe String
    , label : String
    , disabled : Bool
    , error : Maybe String
    , styles : styles
    }


radioField options { onChange, onBlur, selected, label, disabled, error, styles } =
    let
        attributes =
            [] |> blurEvent onBlur
    in
    Input.radio styles.radioField
        attributes
        { onChange = onChange
        , selected = selected
        , label = Input.labelAbove (text label)
        , options = [ fieldError styles.error error ] |> setDisabled disabled
        , choices =
            List.map
                (\( value, name ) ->
                    Input.choice value (text name)
                )
                options
        }



-- HELPERS


fieldError : styles -> Maybe String -> Input.Option styles variation msg
fieldError style error =
    Element.whenJust error text
        |> el style []
        |> Input.errorBelow


blurEvent onBlur attrs =
    Maybe.map (Element.Events.onBlur >> flip (::) attrs) onBlur
        |> Maybe.withDefault attrs


setDisabled disabled attrs =
    if disabled then
        Input.disabled :: attrs
    else
        attrs
