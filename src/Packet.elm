module Packet exposing (ControlPacket(..), decode)

import Bitwise
import Bytes exposing (Bytes)
import Bytes.Decode as Decode
import Bytes.Encode as Encode
import Debug


type ControlPacket
    = RESERVED
    | CONNECT
    | CONNACK
    | PUBLISH
    | PUBACK
    | PUBREC
    | PUBREL
    | PUBCOMP
    | SUBSCRIBE
    | SUBACK
    | UNSUBSCRIBE
    | UNSUBACK
    | PINGREQ
    | PINGRESP
    | DISCONNECT


headerToPacket : Int -> Int -> ControlPacket
headerToPacket header length =
    let
        fixedHeaderType =
            Bitwise.shiftRightZfBy 4 header

        fixedHeaderFlags =
            Bitwise.and 15 header
    in
    case ( fixedHeaderType, fixedHeaderFlags ) of
        ( 0, _ ) ->
            RESERVED

        ( 1, 0 ) ->
            CONNECT

        _ ->
            RESERVED


packetDecoder : Decode.Decoder ControlPacket
packetDecoder =
    Decode.map2 headerToPacket Decode.unsignedInt8 Decode.unsignedInt8


decode : Bytes -> Maybe ControlPacket
decode bytes =
    Decode.decode packetDecoder bytes
