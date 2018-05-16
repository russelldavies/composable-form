module Form.Field.Select exposing (Attributes, Field, build)

import Form.Base as Form exposing (Form)
import Form.Field.State exposing (State)


type alias Field values =
    { attributes : Attributes
    , state : State String values
    }


type alias Attributes =
    { label : String
    , placeholder : String
    , options : List ( String, String )
    }


build :
    (Field values -> field)
    -> Form.FieldConfig Attributes String values output
    -> Form values output field
build =
    Form.field { builder = Field, isEmpty = String.isEmpty }
