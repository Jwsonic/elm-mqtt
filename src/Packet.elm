module Packet exposing (ConnAckReturnCode(..), Packet(..), QoS(..))

-- Currently we only support packets received by the client


type Packet
    = ConnAck { sessionPresent : Bool, returnCode : ConnAckReturnCode }
    | Publish { dup : Bool, qos : QoS, retain : Bool }
    | PubAck { packetId : Int }
    | PubRec
    | PubRel
    | PubComp
    | Suback
    | UnSubAck
    | PingResp
    | Disconnect



-- Packets that can be received by either the client and the server


type ConnAckReturnCode
    = Accepted
    | UnnacpetableProtocolVersion
    | IdentifierRejected
    | ServerUnavailable
    | BadUsernameOrPassword
    | NotAuthorized


type QoS
    = Zero
    | One
    | Two
