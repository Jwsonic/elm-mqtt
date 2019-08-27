module Parser exposing (decode)

import Bitwise
import Bytes exposing (Bytes)
import Bytes.Decode as Decode
import Bytes.Encode as Encode
import Debug
import Packet exposing (ConnAckReturnCode(..), Packet(..), QoS(..))


decode : Bytes -> Maybe Packet
decode bytes =
    Decode.decode packetDecoder bytes


packetDecoder : Decode.Decoder Packet
packetDecoder =
    decodeTwoBytes
        |> Decode.andThen decodeFixedHeader


decodeTwoBytes : Decode.Decoder ( Int, Int )
decodeTwoBytes =
    Decode.map2 Tuple.pair Decode.unsignedInt8 Decode.unsignedInt8


decodeFixedHeader : ( Int, Int ) -> Decode.Decoder Packet
decodeFixedHeader ( header, length ) =
    let
        type_ =
            Bitwise.shiftRightBy 4 header

        flags =
            Bitwise.and 15 header
    in
    case ( type_, flags, length ) of
        ( 1, 0, _ ) ->
            connectDecoder

        ( 2, 0, 2 ) ->
            connAckDecoder

        ( 3, _, _ ) ->
            publishDecoder flags length

        ( 4, 0, 2 ) ->
            pubAckDecoder

        ( 5, 0, 2 ) ->
            pubRecDecoder

        ( 6, 2, 2 ) ->
            pubRelDecoder

        ( 7, 0, 2 ) ->
            pubCompDecoder

        _ ->
            Decode.fail


connectDecoder : Decode.Decoder Packet
connectDecoder =
    Decode.succeed Disconnect


connAckDecoder : Decode.Decoder Packet
connAckDecoder =
    let
        decodeSessionPresesent =
            Decode.unsignedInt8
                |> Decode.andThen
                    (\flags ->
                        case flags of
                            0 ->
                                Decode.succeed False

                            1 ->
                                Decode.succeed True

                            _ ->
                                Decode.fail
                    )

        decodeReturnCode =
            Decode.unsignedInt8
                |> Decode.andThen
                    (\returnCode ->
                        case returnCode of
                            0 ->
                                Decode.succeed Accepted

                            1 ->
                                Decode.succeed UnnacpetableProtocolVersion

                            2 ->
                                Decode.succeed IdentifierRejected

                            3 ->
                                Decode.succeed ServerUnavailable

                            4 ->
                                Decode.succeed BadUsernameOrPassword

                            5 ->
                                Decode.succeed NotAuthorized

                            _ ->
                                Decode.fail
                    )

        mapper sessionPresent returnCode =
            ConnAck { sessionPresent = sessionPresent, returnCode = returnCode }
    in
    Decode.map2 mapper decodeSessionPresesent decodeReturnCode


publishDecoder : Int -> Int -> Decode.Decoder Packet
publishDecoder flags length =
    let
        decodeDup =
            Decode.succeed <| Bitwise.and 8 flags == 8

        decodeRetain =
            Decode.succeed <| Bitwise.and 1 flags == 1

        decodeQos =
            case Bitwise.shiftRightBy 1 flags |> Bitwise.and 3 of
                0 ->
                    Decode.succeed Zero

                1 ->
                    Decode.succeed One

                2 ->
                    Decode.succeed Two

                _ ->
                    Decode.fail

        mapper dup qos retain =
            Publish { dup = dup, qos = qos, retain = retain }
    in
    Decode.map3 mapper decodeDup decodeQos decodeRetain


pubAckDecoder : Decode.Decoder Packet
pubAckDecoder =
    Decode.map PubAck <| Decode.unsignedInt16 Bytes.BE


pubRecDecoder : Decode.Decoder Packet
pubRecDecoder =
    Decode.map PubRec <| Decode.unsignedInt16 Bytes.BE


pubRelDecoder : Decode.Decoder Packet
pubRelDecoder =
    Decode.map PubRel <| Decode.unsignedInt16 Bytes.BE


pubCompDecoder : Decode.Decoder Packet
pubCompDecoder =
    Decode.map PubComp <| Decode.unsignedInt16 Bytes.BE
