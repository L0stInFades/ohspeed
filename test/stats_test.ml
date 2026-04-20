let percentile_test () =
  let actual = Ohspeed.Stats.percentile [ 10.; 20.; 30.; 40.; 50. ] 0.5 in
  Alcotest.(check (option (float 0.0001))) "p50" (Some 30.) actual

let jitter_test () =
  let actual = Ohspeed.Stats.jitter [ 10.; 14.; 13.; 20. ] in
  Alcotest.(check (option (float 0.0001))) "jitter" (Some 4.) actual

let parse_csv_test () =
  let actual = Ohspeed.Model.parse_int_csv ~name:"sizes" "100, 200,300" in
  Alcotest.(check (result (list int) string)) "sizes" (Ok [ 100; 200; 300 ]) actual

let sparkline_test () =
  let actual = Ohspeed.Tui.sparkline ~width:8 [ 1.; 2.; 3.; 4.; 5. ] in
  Alcotest.(check string) "sparkline" "▁▃▅▆█   " actual

let history_json_roundtrip_test () =
  let report =
    Ohspeed.Model.
      {
        generated_at = 1234.;
        preset = Quick;
        server =
          {
            colo = Some "LAX";
            city = Some "Los Angeles";
            country = Some "US";
            asn = Some "906";
            ip = Some "127.0.0.1";
          };
        idle_latency_ms = Some 12.3;
        idle_jitter_ms = Some 1.5;
        idle_points = [ 11.; 12.; 13. ];
        download =
          Some
            {
              bandwidth_bps = Some 1_000_000.;
              samples = [];
              qualified_samples = 0;
              sets = [];
              loaded_latency_ms = Some 18.;
              loaded_jitter_ms = Some 2.;
              loaded_points = [];
              selected_request_bytes = Some 250_000;
            };
        upload =
          Some
            {
              bandwidth_bps = Some 2_000_000.;
              samples = [];
              qualified_samples = 0;
              sets = [];
              loaded_latency_ms = Some 19.;
              loaded_jitter_ms = Some 2.2;
              loaded_points = [];
              selected_request_bytes = Some 250_000;
            };
      }
  in
  let entry = Ohspeed.History.of_report report in
  let json = Ohspeed.History.entry_to_yojson entry in
  let roundtrip = Ohspeed.History.entry_of_yojson json in
  Alcotest.(check bool) "history roundtrip" true (Option.is_some roundtrip)

let () =
  Alcotest.run "ohspeed"
    [
      ("stats", [ Alcotest.test_case "percentile" `Quick percentile_test ]);
      ("jitter", [ Alcotest.test_case "jitter" `Quick jitter_test ]);
      ("parse", [ Alcotest.test_case "csv" `Quick parse_csv_test ]);
      ("tui", [ Alcotest.test_case "sparkline" `Quick sparkline_test ]);
      ( "history",
        [ Alcotest.test_case "json roundtrip" `Quick history_json_roundtrip_test ] );
    ]
