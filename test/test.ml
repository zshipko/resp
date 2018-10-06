module Value = Resp.String

let test_roundtrip () =
  let v = `Array [| `String "x"; `Integer 123L; `Bulk (`String "abc") |] in
  let output = ref "" in
  Value.write output v;
  let v' = Value.read output in
  Alcotest.(check bool) "rountrip" (v = v') true

let () = Alcotest.run "Resp" [
  "encoding", [
    "Rondtrip", `Quick, test_roundtrip
  ]
]
