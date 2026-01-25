(lang dune 3.20)

(name yum)

(generate_opam_files
 ; cohttp_async_websocket is pinned
 false)

(source
 (github hitgif/yum))

(authors "HITGIF <me@anhai.wang>")

(maintainers "HITGIF <me@anhai.wang>")

(license MIT)

(package
 (name yum)
 (synopsis "A Discord music player bot")
 (depends
  async
  async_ssl
  async_udp
  camlzip
  core
  core_unix
  cohttp-async
  cohttp_async_websocket
  ctypes
  ctypes-foreign
  conf-libsodium
  dedent
  file_path
  memtrace
  (ocaml
   (= 5.2.0))
  ppx_jane
  ppx_with
  ppx_quick_test
  yojson))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
