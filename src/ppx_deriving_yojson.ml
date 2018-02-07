open Longident
open Location
open Asttypes
open Parsetree
open Ast_helper
open Ast_convenience
module P = Ppx_deriving_json

module Yojson_deriver : P.Json_deriver = struct
  let name = "yojson"
  let suffix_to = "to_yojson"
  let suffix_of = "of_yojson"
  let value_type = [%type: Yojson.Safe.json]
  let is_value_type = function
    | [%type: Yojson.Safe.json] -> true
    | _ -> false
  let runtime_module = "Ppx_deriving_json_yojson_runtime"
  let fields_module = "Yojson_fields"
  let encode_float_function _attrs =
    [%expr fun x -> `Float x]
  let encode_integer_according_to_attributes to_string attrs =
    [%expr fun x ->
      [%e Exp.variant (P.attr_int_encoding_as_string ~deriver:name attrs)
            (Some [%expr ([%e to_string] x)])]]
  let encode_int_function _attrs =
    [%expr fun x -> `Int x]
  let encode_int32_function _attrs =
    (* XXX why is it different from int64/nativeint?
       (because it would always be representatble as a float?) *)
    [%expr fun x -> `Intlit (Int32.to_string x)]
  let encode_int64_function attrs =
    encode_integer_according_to_attributes [%expr Int64.to_string] attrs
  let encode_nativeint_function attrs =
    encode_integer_according_to_attributes [%expr Nativeint.to_string] attrs
  let decode_float_function _attrs =
    [[%pat? `Int x],    [%expr Result.Ok (float_of_int x)];
     [%pat? `Intlit x], [%expr Result.Ok (float_of_string x)];
     [%pat? `Float x],  [%expr Result.Ok x]]
  let decode_integer_according_to_attributes of_string of_int attrs =
    match P.attr_int_encoding ~deriver:name attrs with
      | `String ->
        [[%pat? `String x], [%expr Result.Ok ([%e of_string] x)]]
      | `Int ->
        [[%pat? `Int x],    [%expr Result.Ok ([%e of_int] x)];
         [%pat? `Intlit x], [%expr Result.Ok ([%e of_string] x)]]
  let decode_int_function _attrs =
    [[%pat? `Int x], [%expr Result.Ok x]]
  let decode_int32_function _attrs =
    (* XXX why is it different from int64/nativeint?
       (because it would always be representatble as a float?) *)
    [[%pat? `Int x],    [%expr Result.Ok (Int32.of_int x)];
     [%pat? `Intlit x], [%expr Result.Ok (Int32.of_string x)]]
  let decode_int64_function attrs =
    decode_integer_according_to_attributes
      [%expr Int64.of_string]
      [%expr Int64.of_int]
      attrs
  let decode_nativeint_function attrs =
    decode_integer_according_to_attributes
      [%expr Nativeint.of_string]
      [%expr Nativeint.of_int]
      attrs
end

include P.Register (Yojson_deriver)
