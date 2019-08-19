module Packet exposing (ConnAckReturnCode(..), Packet(..), decode, packetDecoder)

import Bitwise
import Bytes exposing (Bytes)
import Bytes.Decode as Decode
import Bytes.Encode as Encode
import Debug


type Packet
    = Connect
    | ConnAck { sessionPresent : Bool, returnCode : ConnAckReturnCode }
    | Publish
    | PubAck
    | PubRec
    | PubRel
    | PubComp
    | Subscribe
    | Suback
    | UnSubscribe
    | UnSubAck
    | PingReq
    | PingResp
    | Disconnect


type ConnAckReturnCode
    = Accepted
    | UnnacpetableProtocolVersion
    | IdentifierRejected
    | ServerUnavailable
    | BadUsernameOrPassword
    | NotAuthorized


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
        fixedHeaderType =
            Bitwise.shiftRightBy 4 header

        fixedHeaderFlags =
            Bitwise.and 15 header
    in
    case ( fixedHeaderType, fixedHeaderFlags, length ) of
        ( 1, 0, _ ) ->
            connectDecoder

        ( 2, 0, 2 ) ->
            connAckDecoder

        _ ->
            Decode.fail


connectDecoder : Decode.Decoder Packet
connectDecoder =
    Decode.succeed Connect


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

        builder sessionPresent returnCode =
            ConnAck { sessionPresent = sessionPresent, returnCode = returnCode }
    in
    Decode.map2 builder decodeSessionPresesent decodeReturnCode
