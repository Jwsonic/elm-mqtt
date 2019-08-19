module PacketTest exposing (suite)

import Bytes exposing (Bytes)
import Bytes.Encode as Encode
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Packet exposing (..)
import Test exposing (..)


intsToBytes : List Int -> Bytes
intsToBytes ints =
    let
        encoder =
            Encode.sequence <| List.map Encode.unsignedInt8 ints
    in
    Encode.encode encoder


suite : Test
suite =
    describe "parser"
        [ test "It parses connect with no clientId in 3.1.1" <|
            \_ ->
                Expect.equal (Just CONNECT)
                    (decode <|
                        intsToBytes
                            [ 16
                            , 18 -- Header
                            , 0
                            , 6 -- Protocol ID length
                            , 77
                            , 81
                            , 73
                            , 115
                            , 100
                            , 112 -- Protocol ID
                            , 3 -- Protocol version
                            , 0 -- Connect flags
                            , 0
                            , 30 -- Keepalive
                            , 0
                            , 4 -- Client ID length
                            , 116
                            , 101
                            , 115
                            , 116 -- Client ID
                            ]
                    )
        ]
