module Form.View
    exposing
        ( BasicConfig
        , Model
        , State(..)
        , Validation(..)
        , basic
        , errorMessage
        , idle
        , viewField
        )

import Form exposing (Form)
import Form.Error as Error exposing (Error)
import Form.Field.Text
import Form.Value as Value
import Html exposing (Html)
import Html.Attributes as Attributes
import Html.Events as Events
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


type alias BasicConfig values msg =
    { onChange : Model values -> msg
    , action : String
    , loadingMessage : String
    , validation : Validation
    }


type Validation
    = ValidateOnSubmit
    | ValidateOnBlur


basic : BasicConfig values msg -> Form values msg -> Model values -> Html msg
basic { onChange, action, loadingMessage, validation } form model =
    let
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
                |> Maybe.map (Events.onSubmit >> List.singleton)
                |> Maybe.withDefault []

        fieldToHtml =
            viewField
                { onChange = \values -> onChange { model | values = values }
                , onBlur = onBlur
                , disabled = model.state == Loading
                , showError = showError
                }

        onBlur =
            case validation of
                ValidateOnSubmit ->
                    Nothing

                ValidateOnBlur ->
                    Just (\label -> onChange { model | showFieldError = Set.insert label model.showFieldError })

        showError label =
            model.showAllErrors || Set.member label model.showFieldError
    in
    Html.form (Attributes.class "elm-form" :: onSubmit)
        (List.concat
            [ Form.fields form model.values
                |> List.map fieldToHtml
            , [ case model.state of
                    Error error ->
                        errorMessage (Just error)

                    _ ->
                        Html.text ""
              , Html.button
                    [ Attributes.type_ "submit"
                    , Attributes.disabled (onSubmitMsg == Nothing)
                    ]
                    [ if model.state == Loading then
                        Html.text loadingMessage
                      else
                        Html.text action
                    ]
              ]
            ]
        )



-- FIELD


type alias FieldConfig values msg =
    { onChange : values -> msg
    , onBlur : Maybe (String -> msg)
    , disabled : Bool
    , showError : String -> Bool
    }


viewField : FieldConfig values msg -> ( Form.Field values, Maybe Error ) -> Html msg
viewField { onChange, onBlur, disabled, showError } ( field, maybeError ) =
    let
        error label value =
            if showError label then
                Maybe.map errorToString maybeError
            else
                Nothing

        whenDirty value x =
            if Value.isDirty value then
                x
            else
                Nothing
    in
    case field of
        Form.Text { type_, attributes, state } ->
            let
                config =
                    { onChange = state.update >> onChange
                    , onBlur = whenDirty state.value (Maybe.map (\onBlur -> onBlur attributes.label) onBlur)
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

                Form.Field.Text.TextArea ->
                    textArea config

                Form.Field.Text.Password ->
                    passwordField config

                Form.Field.Text.Email ->
                    emailField config

        Form.Checkbox { attributes, state } ->
            checkboxField
                { checked = Value.raw state.value |> Maybe.withDefault False
                , disabled = disabled
                , onChange = state.update >> onChange
                , label = attributes.label
                , error = error attributes.label state.value
                }

        Form.Radio { attributes, state } ->
            radioField attributes.options
                { onChange = state.update >> onChange
                , selected = Value.raw state.value
                , label = attributes.label
                , disabled = disabled
                , error = error attributes.label state.value
                }

        Form.Select { attributes, state } ->
            selectField attributes.options
                { onChange = state.update >> onChange
                , onBlur = whenDirty state.value (Maybe.map (\onBlur -> onBlur attributes.label) onBlur)
                , disabled = disabled
                , label = attributes.label
                , placeholder = attributes.placeholder
                , value = Value.raw state.value |> Maybe.withDefault ""
                , error = error attributes.label state.value
                }

        Form.Group label fields ->
            let
                labelView =
                    case label of
                        Just label ->
                            Html.text label

                        Nothing ->
                            Html.text ""
            in
            Html.div
                []
                (labelView
                    :: List.map
                        (viewField
                            { onChange = onChange
                            , onBlur = onBlur
                            , disabled = disabled
                            , showError = showError
                            }
                        )
                        fields
                )


errorToString : Error -> String
errorToString error =
    case error of
        Error.EmptyField ->
            "This field is required"

        Error.ParserError parserError ->
            parserError



-- TEXT FIELDS


type alias TextFieldConfig msg =
    { onChange : String -> msg
    , onBlur : Maybe msg
    , value : String
    , label : String
    , placeholder : String
    , disabled : Bool
    , error : Maybe String
    }


textField : TextFieldConfig msg -> Html msg
textField =
    inputField "text"


passwordField : TextFieldConfig msg -> Html msg
passwordField =
    inputField "password"


emailField : TextFieldConfig msg -> Html msg
emailField =
    inputField "email"


textArea : TextFieldConfig msg -> Html msg
textArea { onChange, disabled, value, error, label, placeholder } =
    Html.div [ Attributes.class "elm-form-field" ]
        [ fieldLabel label
        , Html.textarea
            [ Events.onInput onChange
            , Attributes.disabled disabled
            , Attributes.placeholder placeholder
            ]
            [ Html.text value ]
        , errorMessage error
        ]



-- CHECKBOX FIELD


type alias CheckboxFieldConfig msg =
    { onChange : Bool -> msg
    , label : String
    , checked : Bool
    , disabled : Bool
    , error : Maybe String
    }


checkboxField : CheckboxFieldConfig msg -> Html msg
checkboxField { checked, disabled, onChange, label, error } =
    Html.div
        [ Attributes.classList
            [ ( "elm-form-field", True )
            , ( "elm-form-field-error", error /= Nothing )
            ]
        ]
        [ Html.label []
            [ Html.input
                [ Events.onCheck onChange
                , Attributes.checked checked
                , Attributes.disabled disabled
                , Attributes.type_ "checkbox"
                ]
                []
            , Html.text label
            ]
        , errorMessage error
        ]



-- RADIO FIELD


type alias RadioFieldConfig msg =
    { onChange : String -> msg
    , selected : Maybe String
    , label : String
    , disabled : Bool
    , error : Maybe String
    }


radioField : List ( String, String ) -> RadioFieldConfig msg -> Html msg
radioField options { onChange, selected, label, disabled, error } =
    let
        radio value name checked =
            Html.label []
                [ Html.input
                    [ Attributes.checked checked
                    , Attributes.name label
                    , Attributes.type_ "radio"
                    , Attributes.disabled disabled
                    , Events.onClick (onChange value)
                    ]
                    []
                , Html.text name
                ]

        isSelected value =
            selected
                |> Maybe.map (\selected -> selected == value)
                |> Maybe.withDefault False
    in
    Html.div
        [ Attributes.classList
            [ ( "elm-form-field", True )
            , ( "elm-form-field-error", error /= Nothing )
            ]
        ]
        [ Html.text label
        , Html.fieldset []
            (List.map
                (\( value, name ) ->
                    radio value name (isSelected value)
                )
                options
            )
        , errorMessage error
        ]



-- SELECT FIELD


type alias SelectFieldConfig msg =
    TextFieldConfig msg


selectField : List ( String, String ) -> TextFieldConfig msg -> Html msg
selectField options { onChange, onBlur, disabled, value, error, label, placeholder } =
    let
        toOption ( key, label ) =
            Html.option
                [ Attributes.value key
                , Attributes.selected (value == key)
                ]
                [ Html.text label ]

        placeholderOption =
            Html.option
                [ Attributes.disabled True
                , Attributes.selected (value == "")
                ]
                [ Html.text ("-- " ++ placeholder ++ " --") ]

        fixedAttributes =
            [ Events.onInput onChange
            , Attributes.disabled disabled
            ]

        attributes =
            Maybe.map (Events.onBlur >> flip (::) fixedAttributes) onBlur
                |> Maybe.withDefault fixedAttributes
    in
    Html.div
        [ Attributes.classList
            [ ( "elm-form-field", True )
            , ( "elm-form-field-error", error /= Nothing )
            ]
        ]
        [ fieldLabel label
        , Html.select attributes
            (placeholderOption :: List.map toOption options)
        , errorMessage error
        ]



-- PRIVATE HELPERS


fieldLabel : String -> Html msg
fieldLabel label =
    Html.label [] [ Html.text label ]


errorMessage : Maybe String -> Html msg
errorMessage =
    Maybe.map (Html.text >> List.singleton >> Html.div [ Attributes.class "elm-form-error" ])
        >> Maybe.withDefault (Html.text "")


inputField : String -> TextFieldConfig msg -> Html msg
inputField type_ { onChange, onBlur, disabled, value, error, label, placeholder } =
    let
        fixedAttributes =
            [ Events.onInput onChange
            , Attributes.disabled disabled
            , Attributes.value value
            , Attributes.placeholder placeholder
            , Attributes.type_ type_
            ]

        attributes =
            Maybe.map (Events.onBlur >> flip (::) fixedAttributes) onBlur
                |> Maybe.withDefault fixedAttributes
    in
    Html.div
        [ Attributes.classList
            [ ( "elm-form-field", True )
            , ( "elm-form-field-error", error /= Nothing )
            ]
        ]
        [ fieldLabel label
        , Html.input attributes
            []
        , errorMessage error
        ]
