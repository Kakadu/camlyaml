open Printf

type json =
        [ `Assoc of (string * json) list
        | `Bool of bool
        | `Float of float
        | `Int of int
        | `List of json list
        | `Null
        | `String of string ]

open Ostap
let repr = Matcher.Token.repr
let make_reason msg l = new Reason.t (Msg.make msg [||] (Matcher.Token.loc l))


let start : _ -> (_, json, _) Combinators.result =
  let ostap (
    mapping_item[pos]:
      (* name: <float number> *)
      <key,loc> :IDENT => {snd loc >= pos} =>
      ":" <(v,_)> : FLOAT
                  { (key, `Float (float_of_string v)) }
    | (* name: <integer number> *)
      <key,loc> :IDENT => {snd loc >= pos} =>
      ":" <(v,_)> : LITERAL
                  { (key, `Int (int_of_string v)) }
      (* name: string *)
    | <key,loc> :IDENT => {snd loc >= pos} =>
      ":" <(v,loc2)> : TEXTLINE => { let a = fst loc and b = fst loc2 in
                                     (*printf "Testig that `fst loc1`=`fst loc2`: %d ? %d\n%!" a b;*)
                                     a=b } =>
                  { (key, `String v) }
      (* name:
           something *)
    | <(key,loc)> :IDENT ":" xs:blockList1[pos+1]
                  { (key, xs) }
    | <(key,loc)> :IDENT ":" m:mapping1[pos+1]
                  { (key, m) }
    ;
    mapping[pos]: xs: (mapping_item[pos])*  { `Assoc xs }
    ;
    mapping1[pos]: xs: (mapping_item[pos])+  { `Assoc xs }
    ;
    blockListItem[pos]: <(_k,loc)> :"-" => {snd loc >= pos} =>
                                       r:mapping[snd loc+1] { r }
    ;(*
    blockList[pos]: xs: (blockListItem[pos])* { `List xs }
    ; *)
    blockList1[pos]: xs: (blockListItem[pos])+ { `List xs }
    ;

    root_elem[pos]: mapping_item[pos]
    ;
    start: all: (root_elem[0])*  { `Assoc all }
  ) in
  start
