module ParserTest exposing (suite)

import Expect exposing (Expectation)
import Fixtures exposing (..)
import Fuzz exposing (Fuzzer, intRange, list, string)
import Packet exposing (..)
import Parser exposing (..)
import Test exposing (..)


suite : Test
suite =
    describe "parser"
        [ describe "connAck" connAckTests
        , describe "publish" publishTests
        , describe "pubAck" pubAckTests
        ]


connAckTests =
    [ test "It parses return of zero" <|
        \_ ->
            Expect.equal (Just <| ConnAck { sessionPresent = False, returnCode = Accepted })
                (decode connAckWithReturnZero)
    ]


publishTests =
    [ test "It parses the minimal publish" <|
        \_ ->
            Expect.equal (Just <| Publish { dup = False, qos = Zero, retain = False })
                (decode publishMinimal)
    ]


pubAckTests =
    [ fuzz (intRange 0 65535) "It parses a puback" <|
        \fuzzInt ->
            Expect.equal (Just <| PubAck { packetId = fuzzInt })
                (decode <| pubAckBuilder fuzzInt)
    ]
