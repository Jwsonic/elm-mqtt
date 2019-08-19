module PacketTest exposing (suite)

import Expect exposing (Expectation)
import Fixtures exposing (..)
import Fuzz exposing (Fuzzer, int, list, string)
import Packet exposing (..)
import Test exposing (..)


suite : Test
suite =
    describe "parser"
        [ describe "connect" connectTests
        , describe "connAck" connAckTests
        ]


connectTests =
    [ test "It parses default connect" <|
        \_ ->
            Expect.equal (Just Connect)
                (decode defaultConnect)
    , test "It parses empty will payload" <|
        \_ ->
            Expect.equal (Just Connect)
                (decode emptyWillPayload)
    ]


connAckTests =
    [ test "It parses return of zero" <|
        \_ ->
            Expect.equal (Just <| ConnAck { sessionPresent = False, returnCode = Accepted })
                (decode connAckWithReturnZero)
    ]
