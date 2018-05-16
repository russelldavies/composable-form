module Form.Field.Checkbox exposing (Attributes, Field, build)

import Form.Base as Form exposing (Form)
import Form.Field.State exposing (State)
import Form.Value as Value


type alias Field values =
    { attributes : Attributes
    , state : State Bool values
    }


type alias Attributes =
    { label : String }


build :
    (Field values -> field)
    -> Form.FieldConfig Attributes Bool values output
    -> Form values output field
build toField { parser, value, update, attributes } =
    Form.field { builder = Field, isEmpty = always False }
        toField
        { parser = parser
        , value = value >> Value.withDefault False
        , update = update
        , attributes = attributes
        }
