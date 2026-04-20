(lang dune 3.22)
(name ohspeed)
(generate_opam_files true)

(package
 (name ohspeed)
 (synopsis "Speedtest-grade OCaml 5 CLI with an Eio-powered live terminal dashboard")
 (description
  "A fast OCaml 5 CLI that measures idle latency, jitter, concurrent download and upload throughput against configurable endpoints, using an effects-based Eio engine with a high-performance live TUI and saved history.")
 (authors "Raaaaaymond")
 (maintainers "Raaaaaymond <wbc20230610@outlook.com>")
 (license "MIT")
 (homepage "https://github.com/L0stInFades/ohspeed")
 (bug_reports "https://github.com/L0stInFades/ohspeed/issues")
 (source (github L0stInFades/ohspeed))
 (depends
  (ocaml (>= 5.4))
  dune
  cmdliner
  cohttp-eio
  eio
  eio_main
  fmt
  terminal_size
  tls-eio
  ca-certs
  mirage-crypto-rng
  yojson
  alcotest))
