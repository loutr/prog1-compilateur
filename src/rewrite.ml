
open Lib
open Tast

exception Error of string

let debug = ref false

(* no rewrite rule is currently applied *)
let decl e = e

let file ?debug:(b=false) dl =
  debug := b;
  List.map decl dl
