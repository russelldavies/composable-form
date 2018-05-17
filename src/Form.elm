module Form
    exposing
        ( Field(..)
        , Form
        , append
        , appendMeta
        , checkboxField
        , emailField
        , empty
        , fields
        , group
        , optional
        , parser
        , passwordField
        , radioField
        , selectField
        , textAreaField
        , textField
        )

import Form.Base as Base
import Form.Error exposing (Error)
import Form.Field.Checkbox
import Form.Field.Radio
import Form.Field.Select
import Form.Field.Text
import List.Nonempty exposing (Nonempty)


type alias Form values output =
    Base.Form values output (Field values)


fields : Form values output -> values -> List ( Field values, Maybe Error )
fields =
    Base.fields


parser : Form values output -> Base.FormParser values output
parser =
    Base.parser



-- Constructors


empty : output -> Form values output
empty =
    Base.empty



-- Operations


optional : Form values output -> Form values (Maybe output)
optional =
    Base.optional


append : Form values a -> Form values (a -> b) -> Form values b
append =
    Base.append


appendMeta : Form values a -> Form values b -> Form values b
appendMeta =
    Base.appendMeta



-- Field


type Field values
    = Text (Form.Field.Text.Field values)
    | Checkbox (Form.Field.Checkbox.Field values)
    | Radio (Form.Field.Radio.Field values)
    | Select (Form.Field.Select.Field values)
    | Group (Maybe String) (List ( Field values, Maybe Error ))


group : Maybe String -> Form values output -> Form values output
group label form =
    let
        builder values =
            let
                fields =
                    Base.fields form values
            in
            ( Group label fields, Nothing )
    in
    Base.custom { builder = builder, parser = Base.parser form }



-- Text fields


textField :
    Base.FieldConfig Form.Field.Text.Attributes String values output
    -> Form values output
textField =
    Form.Field.Text.text Text


textAreaField :
    Base.FieldConfig Form.Field.Text.Attributes String values output
    -> Form values output
textAreaField =
    Form.Field.Text.textArea Text


passwordField :
    Base.FieldConfig Form.Field.Text.Attributes String values output
    -> Form values output
passwordField =
    Form.Field.Text.password Text


emailField :
    Base.FieldConfig Form.Field.Text.Attributes String values output
    -> Form values output
emailField =
    Form.Field.Text.email Text



-- Checkbox field


checkboxField :
    Base.FieldConfig Form.Field.Checkbox.Attributes Bool values output
    -> Form values output
checkboxField =
    Form.Field.Checkbox.build Checkbox



-- Radio field


radioField :
    Base.FieldConfig Form.Field.Radio.Attributes String values output
    -> Form values output
radioField =
    Form.Field.Radio.build Radio



-- Select field


selectField :
    Base.FieldConfig Form.Field.Select.Attributes String values output
    -> Form values output
selectField =
    Form.Field.Select.build Select
