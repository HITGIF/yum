open! Core
open! Ctypes
open! Bindings_core
module Binding = Dave_bindings.C (Dave_generated)

let max_supported_protocol_version = Binding.max_supported_protocol_version ()
