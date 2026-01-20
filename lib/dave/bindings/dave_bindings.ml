open! Ctypes

module C (F : Cstubs.FOREIGN) = struct
  open! F

  let max_supported_protocol_version =
    foreign "daveMaxSupportedProtocolVersion" (void @-> returning int)
  ;;
end
