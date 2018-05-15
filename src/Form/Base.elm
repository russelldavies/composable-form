module Form.Base
    exposing
        ( FieldConfig
        , Form
        , FormParser
        , append
        , appendMeta
        , custom
        , empty
        , field
        , fields
        , optional
        , parser
        )

import Form.Error as Error exposing (Error)
import Form.Field.State exposing (State)
import Form.Value as Value exposing (Value)
import List.Nonempty exposing (Nonempty)


-- Form


{-| A set of fields that once validated result in an `output`.
-}
type Form values output field
    = Form (List (FieldBuilder values field)) (FormParser values output)


type alias FieldBuilder values field =
    values -> ( field, Maybe Error )


type alias FormParser values output =
    values -> Result (Nonempty Error) output


fields : Form values output field -> values -> List ( field, Maybe Error )
fields (Form fields _) values =
    List.map (\builder -> builder values) fields


parser : Form values output field -> FormParser values output
parser (Form _ parser) =
    parser



-- Constructors


empty : output -> Form values output custom
empty output =
    Form [] (always (Ok output))



-- Fields


type alias BuildConfig attrs values input field =
    { builder : attrs -> State input values -> field
    , isEmpty : input -> Bool
    }


type alias FieldConfig attrs input values output =
    { parser : FieldParser input output
    , value : values -> Value input
    , update : Value input -> values -> values
    , attributes : attrs
    }


type alias FieldParser input output =
    input -> Result String output


field :
    BuildConfig attrs values input field
    -> (field -> custom)
    -> FieldConfig attrs input values output
    -> Form values output custom
field { builder, isEmpty } map config =
    let
        requiredParser maybeValue =
            case maybeValue of
                Nothing ->
                    Err (List.Nonempty.fromElement Error.EmptyField)

                Just value ->
                    if isEmpty value then
                        Err (List.Nonempty.fromElement Error.EmptyField)
                    else
                        config.parser value
                            |> Result.mapError
                                (Error.ParserError
                                    >> List.Nonempty.fromElement
                                )

        parser =
            config.value >> Value.raw >> requiredParser

        update values newValue =
            let
                value =
                    config.value values

                result =
                    config.parser newValue
            in
            value
                |> Value.update newValue
                -- TODO: remove and change type to `values -> Value input -> values`?
                |> flip config.update values

        error values =
            case parser values of
                Ok _ ->
                    Nothing

                Err errors ->
                    Just (List.Nonempty.head errors)

        state values =
            { value = config.value values
            , update = update values
            }

        fieldBuilder values =
            ( builder config.attributes (state values) |> map, error values )
    in
    Form [ fieldBuilder ] parser


type alias CustomFieldConfig values output field =
    { builder : FieldBuilder values field
    , parser : FormParser values output
    }


custom : CustomFieldConfig values output custom -> Form values output custom
custom { builder, parser } =
    Form [ builder ] parser



-- Operations


{-| Make a form optional. Used in conjunction with [`append`](#append) or
[`appendMeta`](#appendMeta).

    append (optional myForm)

-}
optional : Form values output custom -> Form values (Maybe output) custom
optional (Form fields parser) =
    let
        optionalBuilder builder values =
            case builder values of
                ( field, Just Error.EmptyField ) ->
                    ( field, Nothing )

                result ->
                    result

        optionalOutput values =
            case parser values of
                Ok value ->
                    Ok (Just value)

                Err errors ->
                    if List.Nonempty.all ((==) Error.EmptyField) errors then
                        Ok Nothing
                    else
                        Err errors
    in
    Form (List.map optionalBuilder fields) optionalOutput


{-| Add a form, to a form, perform validation on it, and pass its value to the
form output.

    Form.empty (\foo -> foo) |> Form.append fooField

-}
append : Form values a custom -> Form values (a -> b) custom -> Form values b custom
append (Form newFields newParser) (Form fields parser) =
    Form (fields ++ newFields)
        (\values ->
            case parser values of
                Ok output ->
                    newParser values
                        |> Result.map output

                Err errors ->
                    case newParser values of
                        Ok _ ->
                            Err errors

                        Err newErrors ->
                            Err (List.Nonempty.append errors newErrors)
        )


{-| Add a form to a form and perform validation on it but do not pass its
value to the form output.

    Form.empty (\foo -> foo)
        |> Form.append fooField
        |> Form.appendMeta barField

Notice that the `output` function only takes and results in one argument.

-}
appendMeta : Form values a custom -> Form values b custom -> Form values b custom
appendMeta (Form newFields newParser) (Form fields parser) =
    Form (fields ++ newFields)
        (\values ->
            newParser values
                |> Result.andThen (always (parser values))
        )
