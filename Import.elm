module Import exposing (parse, Import, Exposed(..))

import Regex exposing (find, HowMany(..), Match, regex)
import Array
import String
import Set
import Dict


parse : String -> Dict.Dict String Import
parse source =
    search source
        |> List.map .submatches
        |> List.map process
        |> imports


imports : List RawImport -> Dict.Dict String Import
imports rawImports =
    let
        toDict list =
            Dict.union (Dict.fromList (List.map toImport list)) defaultImports
    in
        toDict rawImports


foobar : String -> Exposed -> ( String, Import )
foobar name exposed =
    ( name, Import Nothing exposed )


defaultImports : Dict.Dict String Import
defaultImports =
    Dict.fromList
        [ foobar "Basics" Every
        , foobar "Debug" None
        , foobar "List" (Some (Set.fromList [ "List", "::" ]))
        , foobar "Maybe" (Some (Set.singleton "Maybe"))
        , foobar "Result" (Some (Set.singleton "Result"))
        , foobar "Signal" (Some (Set.singleton "Signal"))
        ]


type alias RawImport =
    { name : String
    , alias : Maybe String
    , exposed : Maybe (List String)
    }


type alias Import =
    { alias : Maybe String, exposed : Exposed }


type Exposed
    = None
    | Some (Set.Set String)
    | Every


toImport : RawImport -> ( String, Import )
toImport { name, alias, exposed } =
    let
        exposedSet =
            case exposed of
                Nothing ->
                    None

                Just [ ".." ] ->
                    Every

                Just vars ->
                    Some (Set.fromList vars)
    in
        ( name, Import alias exposedSet )


join : Maybe (Maybe a) -> Maybe a
join mx =
    case mx of
        Just x ->
            x

        Nothing ->
            Nothing


exposes : String -> Maybe (List String)
exposes s =
    if s == "" then
        Nothing
    else
        String.split "," s |> List.map String.trim |> Just


process : List (Maybe String) -> RawImport
process submatches =
    let
        moreSubmatches =
            Array.fromList submatches

        name =
            Maybe.withDefault "" (join (Array.get 0 moreSubmatches))

        alias =
            join (Array.get 1 moreSubmatches)

        exposedStart =
            Maybe.withDefault "" (join (Array.get 2 moreSubmatches))

        exposedEnd =
            Maybe.withDefault "" (join (Array.get 3 moreSubmatches))

        exposed =
            (exposedStart ++ exposedEnd) |> String.trim |> exposes
    in
        { name = name, alias = alias, exposed = exposed }


search : String -> List Match
search file =
    let
        pattern =
            regex "(?:^|\\n)import\\s([\\w\\.]+)(?:\\sas\\s(\\w+))?(?:\\sexposing\\s*\\(((?:\\s*(?:\\w+|\\(.+\\))\\s*,)*)\\s*((?:\\.\\.|\\w+|\\(.+\\)))\\s*\\))?"
    in
        find All pattern file
