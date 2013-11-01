open Ostap
open Ostap.Matcher
open Str

let identRegexp = "[a-zA-Z]\\([a-zA-Z0-9]\\)*\\b"

class ['ans, 'arg] lexer s =
  let skip    = Skip.create [Skip.whitespaces " \n\t\r"; Skip.nestedComment "(*" "*)"] in
  let ident   = regexp identRegexp in
  let literal = regexp "[0-9]+" in
  let float   = regexp "[-+]?[0-9]*\\.[0-9]+" in
  let textline= regexp "[a-zA-Z0-9 ?]+" in

  object (self)
    inherit Matcher.t s
    method skip p coord = skip s p coord

    method getIDENT     = self#get "identifier" ident
    method getLITERAL   = self#get "literal"  literal
    method getFLOAT     = self#get "float"    float
    method getTEXTLINE  = self#get "textline" textline

  end
