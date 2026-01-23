open! Ctypes

module C (F : Cstubs.FOREIGN) = struct
  open! F

  let max_supported_protocol_version =
    foreign "daveMaxSupportedProtocolVersion" (void @-> returning int)
  ;;

  module Key_ratchet = struct
    type t = unit ptr

    let t : t typ = ptr void
    let destroy = foreign "daveKeyRatchetDestroy" (t @-> returning void)
  end

  module Commit_result = struct
    type t = unit ptr

    let t : t typ = ptr void
    let destroy = foreign "daveCommitResultDestroy" (t @-> returning void)
    let is_failed = foreign "daveCommitResultIsFailed" (t @-> returning bool)
    let is_ignored = foreign "daveCommitResultIsIgnored" (t @-> returning bool)

    let get_roster_member_ids =
      foreign
        "daveCommitResultGetRosterMemberIds"
        (t
         @-> ptr (ptr uint64_t) (* rosterIds *)
         @-> ptr size_t (* rosterIdsLength *)
         @-> returning void)
    ;;

    let get_roster_member_signature =
      foreign
        "daveCommitResultGetRosterMemberSignature"
        (t
         @-> uint64_t (* rosterId *)
         @-> ptr (ptr uint8_t) (* signature *)
         @-> ptr size_t (* signatureLength *)
         @-> returning void)
    ;;
  end

  module Welcome_result = struct
    type t = unit ptr

    let t : t typ = ptr void
    let destroy = foreign "daveWelcomeResultDestroy" (t @-> returning void)

    let get_roster_member_ids =
      foreign
        "daveWelcomeResultGetRosterMemberIds"
        (t
         @-> ptr (ptr uint64_t) (* rosterIds *)
         @-> ptr size_t (* rosterIdsLength *)
         @-> returning void)
    ;;

    let get_roster_member_signature =
      foreign
        "daveWelcomeResultGetRosterMemberSignature"
        (t
         @-> uint64_t (* rosterId *)
         @-> ptr (ptr uint8_t) (* signature *)
         @-> ptr size_t (* signatureLength *)
         @-> returning void)
    ;;
  end

  module Session = struct
    type t = unit ptr

    let t : t typ = ptr void

    let create =
      foreign
        "daveSessionCreate"
        (ptr void (* context *)
         @-> ocaml_bytes (* authSessionId *)
         @-> static_funptr (* callback *)
               Ctypes.(
                 ocaml_bytes (* source *)
                 @-> ocaml_bytes (* reason *)
                 @-> ptr void (* userData *)
                 @-> returning void)
         @-> ptr void (* userData *)
         @-> returning t)
    ;;

    let destroy = foreign "daveSessionDestroy" (t @-> returning void)

    let init =
      foreign
        "daveSessionInit"
        (t
         @-> uint16_t (* version *)
         @-> uint64_t (* groupId *)
         @-> ocaml_bytes (* selfUserId *)
         @-> returning void)
    ;;

    let reset = foreign "daveSessionReset" (t @-> returning void)

    let set_protocol_version =
      foreign
        "daveSessionSetProtocolVersion"
        (t @-> uint64_t (* version *) @-> returning void)
    ;;

    let get_protocol_version =
      foreign "daveSessionGetProtocolVersion" (t @-> returning uint64_t)
    ;;

    let get_last_epoch_authenticator =
      foreign
        "daveSessionGetLastEpochAuthenticator"
        (t
         @-> ptr (ptr uint8_t) (* authenticator *)
         @-> ptr size_t (* length *)
         @-> returning void)
    ;;

    let set_external_sender =
      foreign
        "daveSessionSetExternalSender"
        (t
         @-> ptr (const uint8_t) (* externalSender *)
         @-> size_t (* length *)
         @-> returning void)
    ;;

    let process_proposals =
      foreign
        "daveSessionProcessProposals"
        (t
         @-> ptr (const uint8_t) (* proposals *)
         @-> size_t (* length *)
         @-> ptr ocaml_bytes (* recognizedUserIds *)
         @-> size_t (* recognizedUserIdsLength *)
         @-> ptr (ptr uint8_t) (* commitWelcomeBytes *)
         @-> ptr size_t (* commitWelcomeBytesLength *)
         @-> returning void)
    ;;

    let process_commit =
      foreign
        "daveSessionProcessCommit"
        (t
         @-> ptr (const uint8_t) (* commit *)
         @-> size_t (* length *)
         @-> returning Commit_result.t)
    ;;

    let process_welcome =
      foreign
        "daveSessionProcessWelcome"
        (t
         @-> ptr (const uint8_t) (* welcome *)
         @-> size_t (* length *)
         @-> ptr ocaml_bytes (* recognizedUserIds *)
         @-> size_t (* recognizedUserIdsLength *)
         @-> returning Welcome_result.t)
    ;;

    let get_marshalled_key_package =
      foreign
        "daveSessionGetMarshalledKeyPackage"
        (t
         @-> ptr (ptr uint8_t) (* keyPackage *)
         @-> ptr size_t (* length *)
         @-> returning void)
    ;;

    let get_key_ratchet =
      foreign
        "daveSessionGetKeyRatchet"
        (t @-> ptr (const uint8_t) (* userId *) @-> returning Key_ratchet.t)
    ;;

    let get_pairwise_fingerprint =
      foreign
        "daveSessionGetPairwiseFingerprint"
        (t
         @-> uint16_t (* version *)
         @-> ocaml_bytes (* userId *)
         @-> static_funptr (* callback *)
               Ctypes.(
                 ptr (const uint8_t) (* fingerprint *)
                 @-> size_t (* length *)
                 @-> ptr void (* userData *)
                 @-> returning void)
         @-> ptr void (* userData *)
         @-> returning void)
    ;;
  end

  module Encryptor = struct
    type t = unit ptr

    let t : t typ = ptr void
    let create = foreign "daveEncryptorCreate" (void @-> returning t)
    let destroy = foreign "daveEncryptorDestroy" (t @-> returning void)
  end
end
