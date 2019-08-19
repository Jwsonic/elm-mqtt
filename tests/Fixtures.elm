module Fixtures exposing (connAckWithReturnZero, defaultConnect, emptyWillPayload, intsToBytes)

import Bytes exposing (Bytes)
import Bytes.Encode as Encode


intsToBytes : List Int -> Bytes
intsToBytes ints =
    let
        encoder =
            Encode.sequence <| List.map Encode.unsignedInt8 ints
    in
    Encode.encode encoder



-- Some test fixures pulled from https://github.com/mqttjs/mqtt-packet/blob/1f1bb6564244215f5a42b933308a48f769d3b357/test.js


defaultConnect =
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


emptyWillPayload =
    intsToBytes
        [ 16
        , 47 -- Header
        , 0
        , 6 -- Protocol ID length
        , 77
        , 81
        , 73
        , 115
        , 100
        , 112 -- Protocol ID
        , 3 -- Protocol version
        , 246 -- Connect flags
        , 0
        , 30 -- Keepalive
        , 0
        , 4 -- Client ID length
        , 116
        , 101
        , 115
        , 116 -- Client ID
        , 0
        , 5 -- Will topic length
        , 116
        , 111
        , 112
        , 105
        , 99 -- Will topic
        , 0
        , 0 -- Will payload length
        , 0 -- Will payload
        , 8 -- Username length
        , 117
        , 115
        , 101
        , 114
        , 110
        , 97
        , 109
        , 101 -- Username
        , 0
        , 8 -- Password length
        , 112
        , 97
        , 115
        , 115
        , 119
        , 111
        , 114
        , 100 -- Password
        ]


connAckWithReturnZero =
    intsToBytes
        [ 32
        , 2
        , 0
        , 0
        ]
