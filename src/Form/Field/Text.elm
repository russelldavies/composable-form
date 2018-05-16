module Form.Field.Text
    exposing
        ( Attributes
        , Field
        , Type(..)
        , email
        , password
        , text
        , textArea
        )

import Form.Base as Form exposing (Form)
import Form.Field.State exposing (State)


type alias Field values =
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
    (Field values -> field)
    -> Form.FieldConfig Attributes String values output
    -> Form values output field
text =
    Form.field <| buildConfig RawText


textArea :
    (Field values -> field)
    -> Form.FieldConfig Attributes String values output
    -> Form values output field
textArea =
    Form.field <| buildConfig TextArea


email :
    (Field values -> field)
    -> Form.FieldConfig Attributes String values output
    -> Form values output field
email =
    Form.field <| buildConfig Email


password :
    (Field values -> field)
    -> Form.FieldConfig Attributes String values output
    -> Form values output field
password =
    Form.field <| buildConfig Password



-- Internal


buildConfig type_ =
    { builder = Field type_
    , isEmpty = String.isEmpty
    }
