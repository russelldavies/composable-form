module Form.Field.State exposing (State)

import Form.Value exposing (Value)


type alias State input values =
    { value : Value input
    , update : input -> values
    }
