open Ostap
open Ostap.Matcher
open Ostap.Util
open Ostap.Combinators
open Str

open Lexer
open Printf
open Yojson.Basic

(* if 1st argument is passed ti executes only specific tests
   otherwise it executes all tests *)

let tests =
  [ ("mini.yaml" , Some (`Assoc [("name", `String "value")]))
  ; ("mini2.yaml", Some (`Assoc [("name", `String "value");
                                 ("name2", `String "value2")]
                        )
  )
  ; ("mini2.yaml", Some (`Assoc [("name", `String "value");
                                 ("name2",`String "value2")]
                        )
  )
  ; ("mini3.yaml", Some (`Assoc [("name", `String "value");
                                 ("name2",`List [ `Assoc["ko", `String "ku"];
                                                  `Assoc["ro", `String "ru"]])]
                        )
  )
  ; ("mini3.5.yaml", None)
  ; ("mini4.yaml", Some (`Assoc [
                                 ("name2",`List [ `Assoc["ko", `List [`Assoc["k1",`String "z1"]
                                                                     ;`Assoc["k2",`String "z2"]
                                                                     ]]
                                                ;`Assoc["ro", `List [`Assoc["r1",`String "u1"]
                                                                    ;`Assoc["r2",`String "u2"]
                                                                    ]]])]
                        )
  )
  ; ("mini5.yaml", Some (`Assoc [("name", `String "value");
                                 ("name2",`List [ `Assoc["ko", `Assoc[("k1",`String "z1")
                                                                     ;("k2",`String "z2")
                                                                     ]]
                                                ;`Assoc["ro", `List [`Assoc["r1",`String "u1"]
                                                                    ;`Assoc["r2",`String "u2"]
                                                                    ]]])]
                        )
  )
  ; ("mini6.yaml", Some (`Assoc [("root", `List [`Assoc [("a",`String "b")
                                                        ;("e",`String "f")
                                                        ;("c",`String "d")
                                                        ]
                                                ])
                                ])
  )
  ; ("mini6.1.yaml", Some (`Assoc [("root", `List [`Assoc [("a",`String "b")
                                                          ;("e",`String "f")
                                                          ]
                                                  ])
                                   ;("c",`String "d")
                                ])
  )
  ; ("input.yaml", None)
  ]



let () =
  if Array.length Sys.argv > 1 then begin
    let filename = Sys.argv.(1) in
    let l = new lexer (Ostap.Util.read ("yamls/"^filename)) in
    printf "Test '%s': " filename;
    let expected = List.assoc filename tests in
    let () = match (Parser.start l,expected) with
    | (Parsed((dans,_),_),Some exp) when dans = exp ->
      print_endline "Passed"
    | (Parsed((got,_),_),Some exp) ->
      print_endline "Failed (while comparison to expected value)";
      print_endline "EXPECTED:";
      Yojson.Basic.pretty_to_channel stdout exp;
      print_newline ();
      print_endline "GOTTEN:";
      Yojson.Basic.pretty_to_channel stdout got;
      print_newline ()
    | (Parsed((got,_),_),_) ->
      print_endline "Parsed";
      Yojson.Basic.pretty_to_channel stdout got;
      print_newline ()

    | (Failed r,_) ->
      print_endline "Failed";
      print_endline (Ostap.Reason.toString `All `Desc r)
    in
    exit 0
  end

let () =
  List.iter (fun (testname,res) ->
    let input = read ("yamls/"^testname) in
    let l = new lexer input in
    printf "Test '%s': " testname;
    let ans = Parser.start l in

    match ans,res with
    | (Parsed((dans,_),_), Some r)  when dans = r -> print_endline "Passed"
    | (Parsed((got,_),_), Some exp) ->
      print_endline "Failed (while comparison to expected value)";
      print_endline "EXPECTED:";
      Yojson.Basic.pretty_to_channel stdout exp;
      print_newline();
      print_endline "GOTTEN:";
      Yojson.Basic.pretty_to_channel stdout got;
      print_newline()

    | (Parsed _, None)     -> print_endline "Passed"
    | Failed r, _ ->
      print_endline "Failed";
      print_endline (Ostap.Reason.toString `All `Desc r)


  ) tests
