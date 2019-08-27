module ParserTest exposing (suite)

import Debug
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
        , describe "pubRec" pubRecTests
        , describe "pubRel" pubRelTests
        , describe "pubComp" pubCompTests
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
            Expect.equal (Just <| PubAck fuzzInt)
                (decode <| pubAckBuilder fuzzInt)
    ]


pubRecTests =
    [ fuzz (intRange 0 65535) "It parses a pubrec" <|
        \fuzzInt ->
            Expect.equal (Just <| PubRec fuzzInt)
                (decode <| pubRecBuilder fuzzInt)
    ]


pubRelTests =
    [ fuzz (intRange 0 65535) "It parses a pubrel" <|
        \fuzzInt ->
            Expect.equal (Just <| PubRel fuzzInt)
                (decode <| pubRelBuilder fuzzInt)
    ]


pubCompTests =
    [ fuzz (intRange 0 65535) "It parses a pubcomp" <|
        \fuzzInt ->
            Expect.equal (Just <| PubComp fuzzInt)
                (decode <| pubCompBuilder fuzzInt)
    ]
