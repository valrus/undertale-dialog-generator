module Character (..) where


type Name
    = Toriel
    | Sans
    | Papyrus
    | Undyne
    | Alphys
    | Asgore
    | Flowey
    | Napstablook
    | Mettaton
    | Asriel
    | Temmie


type alias StyleList =
    List ( String, String )


moodCount : Bool -> Name -> Int
moodCount exmode c =
    case c of
        Toriel ->
            40

        Sans ->
            if exmode then 14 else 6

        Papyrus ->
            19

        Undyne ->
            if exmode then 39 else 24

        Alphys ->
            22

        Asgore ->
            21

        Napstablook ->
            2

        Mettaton ->
            if exmode then 29 else 22

        Flowey ->
            21

        Asriel ->
            if exmode then 26 else 19

        Temmie ->
            3


portraitSize : Name -> ( Int, Int )
portraitSize c =
    case c of
        Napstablook ->
            ( 60, 66 )

        _ ->
            ( 60, 60 )


portraitOffset : Name -> ( Float, Float )
portraitOffset c =
    case c of
        Napstablook ->
            ( 0, -4 )

        _ ->
            ( 0, 0 )


fontStyles : Name -> StyleList
fontStyles c =
    case c of
        Papyrus ->
            [ ( "font-family", "Smooth_Papyrus, Papyrus" )
            , ( "font-size", "26px" )
            , ( "font-weight", "bold" )
            , ( "text-transform", "uppercase" )
            ]

        Sans ->
            [ ( "font-family", "Comic Sans, Comic Sans MS Regular, Comic Sans MS" )
            , ( "font-size", "30px" )
            , ( "font-weight", "bold" )
            , ( "letter-spacing", "1px" )
            , ( "text-transform", "lowercase" )
            ]

        _ ->
            [ ( "font-family", "determination_monoregular" )
            , ( "font-size", "26px" )
            ]


textboxStyles : Name -> StyleList
textboxStyles c =
    case c of
        Papyrus ->
            [ ( "width", "416px" )
            , ( "left", "150px" )
            ]

        Sans ->
            [ ( "width", "382px" )
            , ( "left", "184px" )
            ]

        _ ->
            [ ( "width", "382px" )
            , ( "left", "184px" )
            ]


dialogAsterisk : Name -> String
dialogAsterisk c =
    case c of
        Papyrus ->
            ""

        _ ->
            "*"
