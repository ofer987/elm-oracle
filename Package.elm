module Package exposing (parse, Package, Module)

import Json.Decode as Json exposing (..)
import List


parse : List ( String, String ) -> Package
parse input =
    List.map decode input |> List.concat


decode : ( String, String ) -> Package
decode ( _, json ) =
    case decodeString (packageDecoder) json of
        Ok package ->
            package

        Err msg ->
            []


type alias Package =
    List Module


type alias Type =
    { name : String
    , cases : List String
    }


type alias Module =
    { name : String
    , aliases : List Value
    , comment : String
    , types : List Type
    , values : List Value
    }


type alias Value =
    { name : String
    , comment : String
    , signature : String
    }


packageDecoder : Decoder Package
packageDecoder =
    let
        oneValue =
            map3 Value
                (field "name" string)
                (field "comment" string)
                (field "type" string)

        oneType =
            map2 Type
                (field "name" string)
                (field "cases" (list string))

        oneModule =
            map5 Module
                (field "name" string)
                (field "aliases" (list oneValue))
                (field "comment" string)
                (field "types" (list oneType))
                (field "values" (list oneValue))
    in
        (list oneModule)
