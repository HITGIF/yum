open! Core
open! Ctypes
open! Foreign

module C (F : Cstubs.FOREIGN) = struct
  open! F

  module Make_enum (Arg : sig
      type t [@@deriving enumerate]

      module Variants : sig
        val to_rank : t -> int
      end
    end) =
  struct
    let to_int = Arg.Variants.to_rank
    let t_to_int = Arg.all |> List.map ~f:(fun t -> to_int t, t) |> Int.Map.of_alist_exn
    let of_int = Map.find_exn t_to_int
    let t = view ~read:of_int ~write:to_int int
  end

  module Codec = struct
    type t =
      | Unknown
      | Opus
      | Vp8
      | Vp9
      | H264
      | H265
      | Av1
    [@@deriving enumerate, variants]

    include functor Make_enum
  end

  module Media_type = struct
    type t =
      | Audio
      | Video
    [@@deriving enumerate, variants]

    include functor Make_enum
  end

  module Encryptor_result_code = struct
    type t =
      | Success
      | Encryption_failure
      | Missing_key_ratchet
      | Missing_cryptor
      | Too_many_attempts
    [@@deriving enumerate, variants]

    include functor Make_enum
  end

  module Decryptor_result_code = struct
    type t =
      | Success
      | Decryption_failure
      | Missing_key_ratchet
      | Invalid_nonce
      | Missing_cryptor
    [@@deriving enumerate, variants]

    include functor Make_enum
  end

  module Encryptor_stats = struct
    type t

    let t : t structure typ = structure [%module_name]
    let passthrough_count = field t [%var_name] uint64_t
    let encrypt_success_count = field t [%var_name] uint64_t
    let encrypt_failure_count = field t [%var_name] uint64_t
    let encrypt_duration = field t [%var_name] uint64_t
    let encrypt_attempts = field t [%var_name] uint64_t
    let encrypt_max_attempts = field t [%var_name] uint64_t
    let encrypt_missing_key_count = field t [%var_name] uint64_t
    let () = seal t
  end

  module Decryptor_stats = struct
    type t

    let t : t structure typ = structure [%module_name]
    let passthrough_count = field t [%var_name] uint64_t
    let decrypt_success_count = field t [%var_name] uint64_t
    let decrypt_failure_count = field t [%var_name] uint64_t
    let decrypt_duration = field t [%var_name] uint64_t
    let decrypt_attempts = field t [%var_name] uint64_t
    let decrypt_missing_key_count = field t [%var_name] uint64_t
    let decrypt_invalid_nonce_count = field t [%var_name] uint64_t
    let () = seal t
  end

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
         @-> ptr void (* authSessionId *)
         @-> funptr (* callback *)
               Ctypes.(
                 string (* source *)
                 @-> string (* reason *)
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
         @-> string (* selfUserId *)
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
         @-> ptr ocaml_string (* recognizedUserIds *)
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
         @-> string (* userId *)
         @-> funptr (* callback *)
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

    let set_key_ratchet =
      foreign "daveEncryptorSetKeyRatchet" (t @-> Key_ratchet.t @-> returning void)
    ;;

    let set_passthrough_mode =
      foreign
        "daveEncryptorSetPassthroughMode"
        (t @-> bool (* passthroughMode *) @-> returning void)
    ;;

    let assign_ssrc_to_codec =
      foreign
        "daveEncryptorAssignSsrcToCodec"
        (t @-> uint32_t (* ssrc *) @-> Codec.t @-> returning void)
    ;;

    let get_protocol_version =
      foreign "daveEncryptorGetProtocolVersion" (t @-> returning uint16_t)
    ;;

    let get_max_ciphertext_byte_size =
      foreign
        "daveEncryptorGetMaxCiphertextByteSize"
        (t @-> Media_type.t @-> size_t (* frameSize *) @-> returning size_t)
    ;;

    let has_key_ratchet = foreign "daveEncryptorHasKeyRatchet" (t @-> returning bool)

    let is_passthrough_mode =
      foreign "daveEncryptorIsPassthroughMode" (t @-> returning bool)
    ;;

    let encrypt =
      foreign
        "daveEncryptorEncrypt"
        (t
         @-> Media_type.t
         @-> uint32_t (* ssrc *)
         @-> ptr uint8_t (* frame *)
         @-> size_t (* frameLength *)
         @-> ptr uint8_t (* encryptedFrame *)
         @-> size_t (* encryptedFrameCapacity *)
         @-> ptr size_t (* bytesWritten *)
         @-> returning Encryptor_result_code.t)
    ;;

    let set_protocol_version_changes_callback =
      foreign
        "daveEncryptorSetProtocolVersionChangedCallback"
        (t
         @-> funptr (* callback *) Ctypes.(ptr void (* userData *) @-> returning void)
         @-> ptr void (* userData *)
         @-> returning void)
    ;;

    let get_stats =
      foreign
        "daveEncryptorGetStats"
        (t @-> Media_type.t @-> ptr Encryptor_stats.t @-> returning void)
    ;;
  end

  module Decryptor = struct
    type t = unit ptr

    let t : t typ = ptr void
    let create = foreign "daveDecryptorCreate" (void @-> returning t)
    let destroy = foreign "daveDecryptorDestroy" (t @-> returning void)

    let transition_to_key_ratchet =
      foreign
        "daveDecryptorTransitionToKeyRatchet"
        (t @-> Key_ratchet.t @-> returning void)
    ;;

    let set_passthrough_mode =
      foreign
        "daveDecryptorTransitionToPassthroughMode"
        (t @-> bool (* passthroughMode *) @-> returning void)
    ;;

    let decrypt =
      foreign
        "daveDecryptorDecrypt"
        (t
         @-> Media_type.t
         @-> ptr uint8_t (* encryptedFrame *)
         @-> size_t (* encryptedFrameCapacity *)
         @-> ptr uint8_t (* frame *)
         @-> size_t (* frameCapacity *)
         @-> ptr size_t (* bytesWritten *)
         @-> returning Decryptor_result_code.t)
    ;;

    let get_max_plaintext_byte_size =
      foreign
        "daveDecryptorGetMaxPlaintextByteSize"
        (t @-> Media_type.t @-> size_t (* encryptedFrameSize *) @-> returning size_t)
    ;;

    let get_stats =
      foreign
        "daveDecryptorGetStats"
        (t @-> Media_type.t @-> ptr Decryptor_stats.t @-> returning void)
    ;;
  end
end
