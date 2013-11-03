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

let inline_value : _ -> _ -> (_, json, _) Combinators.result =
  let ostap (
    inline_kvp[loc]:    <(k,_)> : IDENT ":" v : inline_value[loc] { (k,v) }
    ;
    inline_object[loc]:
      <_,(l1,pos1)> : "{" => {l1=fst loc && pos1>=snd loc} =>
      xs: !(Util.list0)[inline_kvp loc]
      <_,(l2,_)> : "}" => {l1=l2} =>
      { let (_: (string * json) list ) = xs in `Assoc xs   }
    ;
    inline_value[loc]:
      <(_,(line1,loc1))> :"[" => {snd loc <= loc1 && fst loc=line1} =>
      xs: !(Util.list0)[inline_value loc]
      <(_,(line2,_))> :"]" => { line1=line2 } =>
      { let (_:json list) = xs in `List xs }
    | <f,_> : FLOAT     { `Float (float_of_string f) }
    | <x,_> : LITERAL   { `Int (int_of_string x) }
    | "\"" <s,_> : TEXTLINE "\""  { `String s }
    | <s,_> : TEXTLINE  { `String s }
    | inline_object[loc]

  ) in
  inline_value

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
    | <key,loc1> :IDENT => { snd loc1>=pos } =>
      <_,loc>    :":"
      x          :inline_value[loc] { (key, x) }
    ;
    mapping[pos]: xs: (mapping_item[pos])*  { `Assoc xs }
    ;
    mapping1[pos]: xs: (mapping_item[pos])+  { `Assoc xs }
    ;
    blockListItem[pos]: <(_k,loc)> :"-" => {snd loc >= pos} =>
                                       r:mapping[snd loc+1] { r }
    ;
    blockList1[pos]: xs: (blockListItem[pos])+ { `List xs }
    ;

    root_elem[pos]: mapping_item[pos]
    ;
    start: all: (root_elem[0])*  { `Assoc all }
  ) in
  start
