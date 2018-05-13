module Form.Field.TextField
    exposing
        ( Attributes
        , TextField
        , Type(..)
        , email
        , password
        , text
        , textArea
        )

import Form.Base as Form exposing (Form)
import Form.Field.State exposing (State)


type alias TextField values =
    { type_ : Type
    , attributes : Attributes
    , state : State String values
    }


type Type
    = RawText
    | TextArea
    | Password
    | Email


type alias Attributes =
    { label : String
    , placeholder : String
    }


text :
    (TextField values -> field)
    -> Form.FieldConfig Attributes String values output
    -> Form values output field
text =
    Form.field <| buildConfig RawText


textArea :
    (TextField values -> field)
    -> Form.FieldConfig Attributes String values output
    -> Form values output field
textArea =
    Form.field <| buildConfig TextArea


email :
    (TextField values -> field)
    -> Form.FieldConfig Attributes String values output
    -> Form values output field
email =
    Form.field <| buildConfig Email


password :
    (TextField values -> field)
    -> Form.FieldConfig Attributes String values output
    -> Form values output field
password =
    Form.field <| buildConfig Password



-- Internal


buildConfig type_ =
    { builder = TextField type_
    , isEmpty = String.isEmpty
    }
