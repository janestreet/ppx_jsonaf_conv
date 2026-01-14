open Base
open! Jsonaf_kernel
open! Jsonaf
open Expect_test_helpers_core
open Async_log_kernel.Ppx_log_syntax

(* Module names below are used in error messages being tested. *)
open Jsonaf.Export

[@@@warning "-unused-module"]

include struct
  [@@@ocaml.warning "-32"]

  let ( ! ) _x = `Shadowed
  let ignore _x = `Shadowed
  let ( = ) _ _ = `Shadowed
end

module Fields = struct
  type ty =
    { x : int
    ; y : int [@key "some"]
    ; z : int [@key "some"]
    }
  [@@deriving jsonaf_fields ~capitalize:"PascalCase"]

  let%expect_test _ =
    print_s [%sexp (jsonaf_fields_of_ty : string list)];
    [%expect {| (X some some) |}]
  ;;
end

module Option = struct
  type ty =
    { x : int option option
    ; y : int option option
    }
  [@@deriving jsonaf]

  let%expect_test _ =
    let open Poly in
    let a = { x = None; y = Some None } in
    let b = jsonaf_of_ty a in
    let c = ty_of_jsonaf b in
    if None = c.x then Stdio.print_endline "x = None";
    if None = c.y then Stdio.print_endline "y = None";
    [%expect
      {|
      x = None
      y = None
      |}]
  ;;
end

module Default_omit = struct
  type ty =
    { x : int option
    ; y : int option [@default None] [@jsonaf_drop_default.equal]
    ; z : int [@default 0] [@jsonaf_drop_default.equal]
    ; b : int [@default 0] [@jsonaf_drop_default.equal]
    }
  [@@deriving jsonaf, equal]

  let ( = ) = equal_ty

  let%expect_test _ =
    let value = { x = None; y = None; z = 0; b = 1 } in
    let jsonaf = jsonaf_of_ty value in
    let jsonaf' =
      `Object [ "x", `Null; "y", `Null; "z", `Number "0"; "b", `Number "1" ]
    in
    print_s (Jsonaf.sexp_of_t jsonaf);
    require (ty_of_jsonaf jsonaf = value);
    require (ty_of_jsonaf jsonaf' = value);
    [%expect {| (Object ((x Null) (b (Number 1)))) |}]
  ;;
end

module Tuple = struct
  type poly = int * float * string [@@deriving jsonaf, equal]

  let ( = ) = equal_poly

  let%expect_test _ =
    List.iter
      ~f:(fun value ->
        let jsonaf = jsonaf_of_poly value in
        print_s (Jsonaf.sexp_of_t jsonaf);
        require (poly_of_jsonaf jsonaf = value))
      [ 1, 1., "string"; 1, 2., "example" ];
    [%expect
      {|
      (Array (
        (Number 1)
        (Number 1)
        (String string)))
      (Array (
        (Number 1)
        (Number 2)
        (String example)))
      |}]
  ;;
end

module Types = struct
  type t = int * int32 * int64 * bool * int ref * nativeint * bytes * char * unit * float
  [@@deriving jsonaf, equal]

  let ( = ) = equal

  let%expect_test _ =
    List.iter
      ~f:(fun value ->
        let jsonaf = jsonaf_of_t value in
        print_s (Jsonaf.sexp_of_t jsonaf);
        require (t_of_jsonaf jsonaf = value))
      [ ( 1
        , Int32.of_int_exn 1
        , Int64.of_int 1
        , true
        , ref 1
        , Nativeint.of_int 1
        , Bytes.of_string "baddecaf"
        , 'c'
        , ()
        , 1. )
      ];
    [%expect
      {|
      (Array (
        (Number 1)
        (Number 1)
        (Number 1)
        True
        (Number 1)
        (Number 1)
        (String baddecaf)
        (String c)
        Null
        (Number 1)))
      |}]
  ;;

  type lt = int lazy_t [@@deriving jsonaf]

  let%expect_test _ =
    List.iter
      ~f:(fun value ->
        let jsonaf = jsonaf_of_lt value in
        print_s (Jsonaf.sexp_of_t jsonaf);
        require (Jsonaf.exactly_equal (jsonaf_of_lt value) jsonaf);
        require Poly.(lt_of_jsonaf jsonaf = value))
      [ lazy 1 ];
    [%expect {| (Number 1) |}]
  ;;

  type opt = int option [@@deriving jsonaf, equal]

  let ( = ) = equal_opt

  let%expect_test _ =
    List.iter
      ~f:(fun value ->
        let jsonaf = jsonaf_of_opt value in
        print_s (Jsonaf.sexp_of_t jsonaf);
        require (Jsonaf.exactly_equal (jsonaf_of_opt value) jsonaf);
        require (opt_of_jsonaf jsonaf = value))
      [ Some 1; None ];
    [%expect
      {|
      (Number 1)
      Null
      |}]
  ;;

  let%expect_test _ =
    List.iter
      ~f:(fun value ->
        let jsonaf = [%jsonaf_of: int list] value in
        print_s (Jsonaf.sexp_of_t jsonaf);
        require Poly.([%jsonaf_of: int list] value = jsonaf);
        require Poly.([%of_jsonaf: int list] jsonaf = value))
      [ []; [ 1 ]; [ 1; 2 ] ];
    [%expect
      {|
      (Array ())
      (Array ((Number 1)))
      (Array (
        (Number 1)
        (Number 2)))
      |}]
  ;;

  let%expect_test _ =
    List.iter
      ~f:(fun value ->
        let jsonaf = [%jsonaf_of: int array] value in
        print_s (Jsonaf.sexp_of_t jsonaf);
        require Poly.([%of_jsonaf: int array] jsonaf = value))
      [ [||]; [| 1 |]; [| 1; 2 |] ];
    [%expect
      {|
      (Array ())
      (Array ((Number 1)))
      (Array (
        (Number 1)
        (Number 2)))
      |}]
  ;;

  let%expect_test _ =
    List.iter
      ~f:(fun value ->
        let jsonaf = [%jsonaf_of: float] value in
        print_s (Jsonaf.sexp_of_t jsonaf);
        require_equal (module Float) ([%of_jsonaf: float] jsonaf) value)
      [ 0.
      ; 1.
      ; 1.23
      ; -1.
      ; -1.23
      ; 0.12345678901234567890123345
      ; 12345678901234567890123345.6789
      ; 123456789012345678.
      ];
    [%expect
      {|
      (Number 0)
      (Number 1)
      (Number 1.23)
      (Number -1)
      (Number -1.23)
      (Number 0.123456789012345677)
      (Number 1.23456789012345682e+25)
      (Number 123456789012345680)
      |}]
  ;;

  let%expect_test _ =
    List.iter
      ~f:(fun value -> require_does_raise (fun () -> [%jsonaf_of: float] value))
      [ Float.nan; Float.infinity; Float.neg_infinity ];
    [%expect
      {|
      (Failure "Cannot represent non-finite float as JSON: nan")
      (Failure "Cannot represent non-finite float as JSON: inf")
      (Failure "Cannot represent non-finite float as JSON: -inf")
      |}]
  ;;

  let%expect_test _ =
    List.iter
      ~f:(fun jsonaf ->
        let value = [%of_jsonaf: float] jsonaf in
        print_s ([%sexp_of: float] value);
        require Poly.([%of_jsonaf: float] jsonaf = value))
      [ `Number "1."; `Number "1"; `Number "1" ];
    [%expect
      {|
      1
      1
      1
      |}]
  ;;

  let%expect_test _ =
    List.iter
      ~f:(fun jsonaf ->
        let value = [%of_jsonaf: int32] jsonaf in
        print_s ([%sexp_of: int32] value);
        require Poly.([%of_jsonaf: int32] jsonaf = value))
      [ `Number "1"; `Number "1" ];
    [%expect
      {|
      1
      1
      |}]
  ;;

  let%expect_test _ =
    List.iter
      ~f:(fun jsonaf ->
        let value = [%of_jsonaf: int64] jsonaf in
        print_s ([%sexp_of: int64] value);
        require Poly.([%of_jsonaf: int64] jsonaf = value))
      [ `Number "1"; `Number "1" ];
    [%expect
      {|
      1
      1
      |}]
  ;;

  let%expect_test _ =
    List.iter
      ~f:(fun jsonaf ->
        let value = [%of_jsonaf: nativeint] jsonaf in
        print_s ([%sexp_of: nativeint] value);
        require Poly.([%of_jsonaf: nativeint] jsonaf = value))
      [ `Number "1"; `Number "1" ];
    [%expect
      {|
      1
      1
      |}]
  ;;

  let%expect_test _ =
    let open Stdlib in
    let tbl = Hashtbl.create 10 in
    let _ = Hashtbl.add tbl "key_1" "value_1" in
    let _ = Hashtbl.add tbl "key_2" "value_2" in
    let _ = Hashtbl.add tbl "key_3" "value_3" in
    let jsonaf = [%jsonaf_of: (string, string) hashtbl] tbl in
    print_s (Jsonaf.sexp_of_t jsonaf);
    require ([%of_jsonaf: (string, string) hashtbl] jsonaf = tbl);
    [%expect
      {|
      (Array (
        (Array ((String key_1) (String value_1)))
        (Array ((String key_2) (String value_2)))
        (Array ((String key_3) (String value_3)))))
      |}]
  ;;
end

module Sum_and_polymorphic_variants = struct
  type poly =
    [ `No_arg
    | `No_arg_with_renaming [@name "zero_arg"]
    | `One_arg of int
    | `One_arg_with_renaming of int [@name "one_arg"]
    | `One_tuple of int * string
    | `Two_args of int * string
    ]
  [@@deriving jsonaf, equal]

  let ( = ) = equal_poly

  let%expect_test _ =
    List.iter
      ~f:(fun value ->
        let jsonaf = jsonaf_of_poly value in
        print_s (Jsonaf.sexp_of_t jsonaf);
        require (poly_of_jsonaf jsonaf = value))
      [ `No_arg
      ; `No_arg_with_renaming
      ; `One_arg 1
      ; `One_arg_with_renaming 1
      ; `One_tuple (1, "a")
      ; `Two_args (1, "a")
      ];
    [%expect
      {|
      (Array ((String No_arg)))
      (Array ((String zero_arg)))
      (Array (
        (String One_arg)
        (Number 1)))
      (Array (
        (String one_arg)
        (Number 1)))
      (Array (
        (String One_tuple)
        (Number 1)
        (String a)))
      (Array (
        (String Two_args)
        (Number 1)
        (String a)))
      |}]
  ;;

  type nominal =
    | No_arg
    | One_arg of int
    | One_tuple of (int * string)
    | Two_args of int * string
  [@@deriving jsonaf, equal]

  let ( = ) = equal_nominal

  let%expect_test _ =
    List.iter
      ~f:(fun value ->
        let jsonaf = jsonaf_of_nominal value in
        print_s (Jsonaf.sexp_of_t jsonaf);
        require (nominal_of_jsonaf jsonaf = value))
      [ No_arg; One_arg 1; One_tuple (1, "a"); Two_args (1, "a") ];
    [%expect
      {|
      (Array ((String No_arg)))
      (Array (
        (String One_arg)
        (Number 1)))
      (Array (
        (String One_tuple)
        (Array (
          (Number 1)
          (String a)))))
      (Array (
        (String Two_args)
        (Number 1)
        (String a)))
      |}]
  ;;
end

module Name = struct
  type nominal =
    | Con_1 [@name "Name_1"]
    | Con_2 of int [@name "Name_2"]
    | Con_3 of (int * string) [@name "Name_3"]
    | Con_4 of int * string [@name "Name_4"]
    | Con_5 of { a : int } [@name "name_5"]
    | Con_6 of { b : int } [@name ""]
  [@@deriving jsonaf, equal]

  let ( = ) = equal_nominal

  let%expect_test _ =
    List.iter
      ~f:(fun value ->
        let jsonaf = jsonaf_of_nominal value in
        print_s (Jsonaf.sexp_of_t jsonaf);
        require (nominal_of_jsonaf jsonaf = value))
      [ Con_1; Con_2 2; Con_3 (1, "a"); Con_4 (1, "a"); Con_5 { a = 1 }; Con_6 { b = 1 } ];
    [%expect
      {|
      (Array ((String Name_1)))
      (Array (
        (String Name_2)
        (Number 2)))
      (Array (
        (String Name_3)
        (Array (
          (Number 1)
          (String a)))))
      (Array (
        (String Name_4)
        (Number 1)
        (String a)))
      (Array ((String name_5) (Object ((a (Number 1))))))
      (Array ((String "") (Object ((b (Number 1))))))
      |}]
  ;;
end

module Capitalize_variants = struct
  type nominal =
    | Con_1
    | Con_2 of int
    | Con_3 of (int * string)
    | Con_4 of int * string
    | Con_5 of { a : int }
    | Con_6 of { b : int } [@name "something-custom"]
  [@@deriving jsonaf ~capitalize:"kebab-case", equal]

  let ( = ) = equal_nominal

  let%expect_test _ =
    List.iter
      ~f:(fun value ->
        let jsonaf = jsonaf_of_nominal value in
        print_s (Jsonaf.sexp_of_t jsonaf);
        require (nominal_of_jsonaf jsonaf = value))
      [ Con_1; Con_2 2; Con_3 (1, "a"); Con_4 (1, "a"); Con_5 { a = 1 }; Con_6 { b = 1 } ];
    [%expect
      {|
      (Array ((String con-1)))
      (Array (
        (String con-2)
        (Number 2)))
      (Array (
        (String con-3)
        (Array (
          (Number 1)
          (String a)))))
      (Array (
        (String con-4)
        (Number 1)
        (String a)))
      (Array ((String con-5) (Object ((a (Number 1))))))
      (Array ((String something-custom) (Object ((b (Number 1))))))
      |}]
  ;;
end

module Records = struct
  type t =
    { a : int
    ; b : (float * string) list option
    }
  [@@deriving jsonaf, equal]

  let ( = ) = equal

  let%expect_test _ =
    let t = { a = 2; b = Some [ 1., "a"; 2.3, "b" ] } in
    let jsonaf = jsonaf_of_t t in
    print_s (Jsonaf.sexp_of_t jsonaf);
    require (t_of_jsonaf jsonaf = t);
    [%expect
      {|
      (Object (
        (a (Number 2))
        (b (
          Array (
            (Array ((Number 1)   (String a)))
            (Array ((Number 2.3) (String b))))))))
      |}]
  ;;
end

module Keys = struct
  type t =
    { name_a : int [@key "key_a"]
    ; name_b : int option [@key "key_b"]
    ; name_c : int option [@key "key_c"] [@jsonaf.option]
    ; name_d : int option [@key "key_d"] [@default None] [@jsonaf_drop_default.equal]
    ; name_e : int [@key ""]
    }
  [@@deriving jsonaf, equal]

  let ( = ) = equal

  let%expect_test _ =
    let t =
      { name_a = 1; name_b = Some 2; name_c = Some 3; name_d = Some 4; name_e = 5 }
    in
    let jsonaf = jsonaf_of_t t in
    print_s (Jsonaf.sexp_of_t jsonaf);
    require (t_of_jsonaf jsonaf = t);
    [%expect
      {|
      (Object (
        (key_a (Number 1))
        (key_b (Number 2))
        (key_c (Number 3))
        (key_d (Number 4))
        (""    (Number 5))))
      |}]
  ;;
end

module Capitalize_arg = struct
  type t =
    { name_a : int
    ; name_b : int option [@key "otherName"]
    }
  [@@deriving jsonaf ~capitalize:"camelCase", equal]

  let ( = ) = equal

  let%expect_test _ =
    let t = { name_a = 1; name_b = Some 2 } in
    let jsonaf = jsonaf_of_t t in
    print_s (Jsonaf.sexp_of_t jsonaf);
    require (t_of_jsonaf jsonaf = t);
    [%expect
      {|
      (Object (
        (nameA     (Number 1))
        (otherName (Number 2))))
      |}]
  ;;
end

module Inline_records = struct
  type t =
    | A of
        { a : int
        ; b : (float * string) list option
        }
    | B of int
  [@@deriving jsonaf, equal]

  let ( = ) = equal

  let%expect_test _ =
    let t = A { a = 2; b = Some [ 1., "a"; 2.3, "b" ] } in
    let jsonaf = jsonaf_of_t t in
    print_s (Jsonaf.sexp_of_t jsonaf);
    require (t_of_jsonaf jsonaf = t);
    [%expect
      {|
      (Array (
        (String A)
        (Object (
          (a (Number 2))
          (b (
            Array (
              (Array ((Number 1)   (String a)))
              (Array ((Number 2.3) (String b))))))))))
      |}]
  ;;

  let%expect_test _ =
    let t = B 100 in
    let jsonaf = jsonaf_of_t t in
    print_s (Jsonaf.sexp_of_t jsonaf);
    require (t_of_jsonaf jsonaf = t);
    [%expect
      {|
      (Array (
        (String B)
        (Number 100)))
      |}]
  ;;
end

module User_specified_conversion = struct
  type my_float = float

  let jsonaf_of_my_float n = `Number (Float.to_string n)
  let my_float_of_jsonaf = Export.float_of_jsonaf

  let%expect_test _ =
    let my_float : my_float = 1.2 in
    let jsonaf = jsonaf_of_my_float my_float in
    print_s (Jsonaf.sexp_of_t jsonaf);
    require Float.(my_float_of_jsonaf jsonaf = my_float);
    [%expect {| (Number 1.2) |}]
  ;;
end

module Abstract_types_are_allowed_in_structures : sig
  type t [@@deriving jsonaf]
end = struct
  type t [@@deriving jsonaf]
end

module Manifest_types = struct
  type a = { t : int }
  type b = a = { t : int } [@@deriving jsonaf]
end

module Function_types : sig
  type t1 = int -> unit [@@deriving jsonaf]
  type t2 = label:int -> ?optional:int -> unit -> unit [@@deriving jsonaf]
end = struct
  type t1 = int -> unit [@@deriving jsonaf]
  type t2 = label:int -> ?optional:int -> unit -> unit [@@deriving jsonaf]
end

module No_unused_rec = struct
  type r = { r : int } [@@deriving jsonaf]
end

module Field_name_should_not_be_rewritten = struct
  open No_unused_rec

  type nonrec r = { r : r }

  let _ = fun (r : r) -> r.r
end

module Polymorphic_variant_inclusion = struct
  type sub1 =
    [ `C1
    | `C2
    ]
  [@@deriving jsonaf, equal]

  type 'b sub2 =
    [ `C4
    | `C5 of 'b
    ]
  [@@deriving jsonaf, equal]

  type ('a, 'b) t = [ sub1 | `C3 of [ `Nested of 'a ] | 'b sub2 | `C6 ] option
  [@@deriving jsonaf, equal]

  let%expect_test _ =
    let cases : (string * string, float) t list =
      [ None
      ; Some `C1
      ; Some `C2
      ; Some (`C3 (`Nested ("a", "b")))
      ; Some `C4
      ; Some (`C5 1.5)
      ; Some `C6
      ]
    in
    List.iter
      ~f:(fun t ->
        let jsonaf = [%jsonaf_of: (string * string, float) t] t in
        print_s (Jsonaf.sexp_of_t jsonaf);
        require
          ([%equal: (string * string, float) t]
             ([%of_jsonaf: (string * string, float) t] jsonaf)
             t))
      cases;
    [%expect
      {|
      Null
      (Array ((String C1)))
      (Array ((String C2)))
      (Array (
        (String C3)
        (Array (
          (String Nested)
          (Array (
            (String a)
            (String b)))))))
      (Array ((String C4)))
      (Array (
        (String C5)
        (Number 1.5)))
      (Array ((String C6)))
      |}]
  ;;

  type sub1_alias = sub1 [@@deriving jsonaf_poly, equal]

  type u =
    [ `A
    | sub1_alias
    | `D
    ]
  [@@deriving jsonaf, equal]

  let ( = ) = equal_u

  let%expect_test _ =
    let cases : u list = [ `A; `C1; `C2; `D ] in
    List.iter
      ~f:(fun u ->
        let jsonaf = [%jsonaf_of: u] u in
        print_s (Jsonaf.sexp_of_t jsonaf);
        require ([%of_jsonaf: u] jsonaf = u))
      cases;
    [%expect
      {|
      (Array ((String A)))
      (Array ((String C1)))
      (Array ((String C2)))
      (Array ((String D)))
      |}]
  ;;
end

module Polymorphic_record_field = struct
  type 'x t =
    { poly : 'a 'b. 'a list
    ; maybe_x : 'x option
    }
  [@@deriving jsonaf]

  let%expect_test _ =
    let t x = { poly = []; maybe_x = Some x } in
    let jsonaf = jsonaf_of_t Export.jsonaf_of_int (t 1) in
    print_s (Jsonaf.sexp_of_t jsonaf);
    require Poly.(t_of_jsonaf Export.int_of_jsonaf jsonaf = t 1);
    require Poly.(jsonaf_of_t Export.jsonaf_of_int (t 1) = jsonaf);
    [%expect {| (Object ((poly (Array ())) (maybe_x (Number 1)))) |}]
  ;;
end

module No_unused_value_warnings : sig end = struct
  module No_warning : sig
    type t = [ `A ] [@@deriving jsonaf]
  end = struct
    type t = [ `A ] [@@deriving jsonaf]
  end

  include struct
    [@@@warning "-unused-module"]

    module Empty = struct end
  end

  module No_warning2 (_ : sig
      type t [@@deriving jsonaf]
    end) =
  struct end

  (* this one can't be handled (what if Empty was a functor, huh?) *)
  (* module No_warning3(X : sig type t with jsonaf end) = Empty *)
  module type S = sig
    type t = [ `A ] [@@deriving jsonaf]
  end

  module No_warning4 : S = struct
    type t = [ `A ] [@@deriving jsonaf]
  end

  module No_warning5 : S = (
    (
    struct
      type t = [ `A ] [@@deriving jsonaf]
    end :
      S) :
      S)

  module Nested_functors
      (_ : sig
         type t [@@deriving jsonaf]
       end)
      (_ : sig
         type t [@@deriving jsonaf]
       end) =
  struct end

  let () =
    let module M : sig
      type t [@@deriving jsonaf]
    end = struct
      type t [@@deriving jsonaf]
    end
    in
    ()
  ;;

  module Include = struct
    include (
    struct
      type t = int [@@deriving jsonaf]
    end :
      sig
        type t [@@deriving jsonaf]
      end
      with type t := int)
  end
end

module Default = struct
  type t = { a : int [@default 2] [@jsonaf_drop_default.equal] }
  [@@deriving jsonaf, equal]

  let ( = ) = equal

  let%expect_test _ =
    let jsonaf = jsonaf_of_t { a = 1 } in
    print_s (Jsonaf.sexp_of_t jsonaf);
    require (t_of_jsonaf jsonaf = { a = 1 });
    [%expect {| (Object ((a (Number 1)))) |}]
  ;;

  let%expect_test _ =
    let jsonaf = jsonaf_of_t { a = 2 } in
    print_s (Jsonaf.sexp_of_t jsonaf);
    require (t_of_jsonaf jsonaf = { a = 2 });
    require (t_of_jsonaf (`Object [ "a", `Number "2" ]) = { a = 2 });
    [%expect {| (Object ()) |}]
  ;;
end

module Type_alias = struct
  (* checking that the [as 'a] is supported and ignored in signatures, that it still
     exports the jsonaf_of_t__ when needed *)
  module B : sig
    type a = [ `A ]
    type t = [ `A ] as 'a constraint 'a = a [@@deriving jsonaf, equal]
  end = struct
    type a = [ `A ] [@@deriving jsonaf, equal]
    type t = [ `A ] [@@deriving jsonaf, equal]
  end

  let ( = ) = B.equal

  let%expect_test _ =
    let jsonaf = B.jsonaf_of_t `A in
    print_s (Jsonaf.sexp_of_t jsonaf);
    require (`A = B.t_of_jsonaf jsonaf);
    [%expect {| (Array ((String A))) |}]
  ;;

  module B2 = struct
    type t =
      [ B.t
      | `B
      ]
    [@@deriving jsonaf]
  end

  module C : sig
    type t = int as 'a [@@deriving jsonaf]
  end = struct
    type t = int [@@deriving jsonaf]
  end

  module D : sig
    type t = 'a constraint 'a = int [@@deriving jsonaf]
  end = struct
    type t = int [@@deriving jsonaf]
  end
end

module Tricky_variants = struct
  (* Checking that the generated code compiles (there used to be a problem with subtyping
     constraints preventing proper generalization). *)
  type t = [ `a ] [@@deriving jsonaf]
  type 'a u = [ t | `b of 'a ] * int [@@deriving jsonaf]
end

module Drop_default = struct
  open! Base
  open Expect_test_helpers_core

  type t = { a : int } [@@deriving equal]

  let test ?cr t_of_jsonaf jsonaf_of_t =
    let ( = ) : Jsonaf_kernel.t -> Jsonaf_kernel.t -> bool = Jsonaf.exactly_equal in
    require ?cr ((`Object [ "a", `Number "1" ] : Jsonaf_kernel.t) = jsonaf_of_t { a = 1 });
    require ?cr ((`Object [] : Jsonaf_kernel.t) = jsonaf_of_t { a = 2 });
    let ( = ) = equal in
    require ?cr (t_of_jsonaf (`Object [ "a", `Number "1" ] : Jsonaf_kernel.t) = { a = 1 });
    require ?cr (t_of_jsonaf (`Object [] : Jsonaf_kernel.t) = { a = 2 })
  ;;

  type my_int = int [@@deriving jsonaf]

  module Poly = struct
    type nonrec t = t = { a : my_int [@default 2] [@jsonaf_drop_default ( = )] }
    [@@deriving jsonaf]

    let%expect_test _ =
      test t_of_jsonaf jsonaf_of_t;
      [%expect {| |}]
    ;;
  end

  module Equal = struct
    let equal_my_int = equal_int

    type nonrec t = t = { a : my_int [@default 2] [@jsonaf_drop_default.equal] }
    [@@deriving jsonaf]

    let%expect_test _ =
      test t_of_jsonaf jsonaf_of_t;
      [%expect {| |}]
    ;;
  end

  module Compare = struct
    let compare_my_int = compare_int

    type nonrec t = t = { a : my_int [@default 2] [@jsonaf_drop_default.compare] }
    [@@deriving jsonaf]

    let%expect_test _ =
      test t_of_jsonaf jsonaf_of_t;
      [%expect {| |}]
    ;;
  end

  module Jsonaf_kernel = struct
    type nonrec t = t = { a : my_int [@default 2] [@jsonaf_drop_default.jsonaf] }
    [@@deriving jsonaf]

    let%expect_test _ =
      test t_of_jsonaf jsonaf_of_t;
      [%expect {| |}]
    ;;
  end
end

module Drop_if = struct
  type t = { a : int [@default 2] [@jsonaf_drop_if fun x -> Int.(x % 2 = 0)] }
  [@@deriving jsonaf, equal]

  let ( = ) = equal

  let%expect_test _ =
    let value = { a = 2 } in
    let jsonaf = jsonaf_of_t value in
    print_s (Jsonaf.sexp_of_t jsonaf);
    require (t_of_jsonaf jsonaf = value);
    require (t_of_jsonaf (`Object [ "a", `Number "2" ]) = value);
    [%expect {| (Object ()) |}]
  ;;

  let%expect_test _ =
    let value = { a = 1 } in
    let jsonaf = jsonaf_of_t value in
    print_s (Jsonaf.sexp_of_t jsonaf);
    require (t_of_jsonaf jsonaf = value);
    [%expect {| (Object ((a (Number 1)))) |}]
  ;;

  type u =
    { a : int
         [@jsonaf_drop_if
           fun x ->
             (* pa_type_conv used to drop parens altogether, causing type errors in the
                following code *)
             let pair = x, 2 in
             match Some pair with
             | None -> true
             | Some (x, y) -> Poly.(x = y)]
    }
  [@@deriving jsonaf]
end

module Omit_nil = struct
  type natural_option = int [@@deriving equal]

  let jsonaf_of_natural_option i = if i >= 0 then Export.jsonaf_of_int i else `Null

  let natural_option_of_jsonaf = function
    | `Null -> -1
    | jsonaf -> Export.int_of_jsonaf jsonaf
  ;;

  type t = { a : natural_option [@default -1] [@jsonaf_drop_default.equal] }
  [@@deriving jsonaf, equal]

  let ( = ) = equal

  let%expect_test _ =
    let value = { a = 1 } in
    let jsonaf = jsonaf_of_t value in
    print_s (Jsonaf.sexp_of_t jsonaf);
    require (t_of_jsonaf jsonaf = value);
    [%expect {| (Object ((a (Number 1)))) |}]
  ;;

  let%expect_test _ =
    let value = { a = -1 } in
    let jsonaf = jsonaf_of_t value in
    let jsonaf' = `Object [] in
    print_s (Jsonaf.sexp_of_t jsonaf);
    require (t_of_jsonaf jsonaf = value);
    require (t_of_jsonaf jsonaf' = value);
    [%expect {| (Object ()) |}]
  ;;

  type t2 = A of { a : int option [@jsonaf.option] } [@@deriving jsonaf, equal]

  let ( = ) = equal_t2

  let%expect_test _ =
    let value = A { a = None } in
    let jsonaf = jsonaf_of_t2 value in
    let jsonaf' = `Array [ `String "A"; `Object [] ] in
    print_s (Jsonaf.sexp_of_t jsonaf);
    require (t2_of_jsonaf jsonaf = value);
    require (t2_of_jsonaf jsonaf' = value);
    [%expect {| (Array ((String A) (Object ()))) |}]
  ;;

  let%expect_test _ =
    let value = A { a = Some 1 } in
    let jsonaf = jsonaf_of_t2 value in
    print_s (Jsonaf.sexp_of_t jsonaf);
    require (t2_of_jsonaf jsonaf = value);
    [%expect {| (Array ((String A) (Object ((a (Number 1)))))) |}]
  ;;
end

module No_unused_rec_warning = struct
  type r = { field : r -> unit } [@@deriving jsonaf_of]
end

module True_and_false = struct
  type t =
    | True
    | False
  [@@deriving jsonaf, equal]

  let ( = ) = equal

  let%expect_test _ =
    let value = True in
    let jsonaf = jsonaf_of_t value in
    print_s (Jsonaf.sexp_of_t jsonaf);
    require (t_of_jsonaf jsonaf = value);
    [%expect {| (Array ((String True))) |}]
  ;;

  let%expect_test _ =
    let value = False in
    let jsonaf = jsonaf_of_t value in
    print_s (Jsonaf.sexp_of_t jsonaf);
    require (t_of_jsonaf jsonaf = value);
    [%expect {| (Array ((String False))) |}]
  ;;

  type u =
    | True of int
    | False of int
  [@@deriving jsonaf, equal]

  let ( = ) = equal_u

  let%expect_test _ =
    let value = True 1 in
    let jsonaf = jsonaf_of_u value in
    print_s (Jsonaf.sexp_of_t jsonaf);
    require (u_of_jsonaf jsonaf = value);
    [%expect
      {|
      (Array (
        (String True)
        (Number 1)))
      |}]
  ;;

  let%expect_test _ =
    let value = False 0 in
    let jsonaf = jsonaf_of_u value in
    print_s (Jsonaf.sexp_of_t jsonaf);
    require (u_of_jsonaf jsonaf = value);
    [%expect
      {|
      (Array (
        (String False)
        (Number 0)))
      |}]
  ;;

  type v =
    [ `True
    | `False of int
    ]
  [@@deriving jsonaf, equal]

  let ( = ) = equal_v

  let%expect_test _ =
    let value = `True in
    let jsonaf = jsonaf_of_v value in
    print_s (Jsonaf.sexp_of_t jsonaf);
    require (v_of_jsonaf jsonaf = value);
    [%expect {| (Array ((String True))) |}]
  ;;

  let%expect_test _ =
    let value = `False 0 in
    let jsonaf = jsonaf_of_v value in
    print_s (Jsonaf.sexp_of_t jsonaf);
    require (v_of_jsonaf jsonaf = value);
    [%expect
      {|
      (Array (
        (String False)
        (Number 0)))
      |}]
  ;;
end

module Gadt = struct
  (* plain type without argument *)
  type 'a s = Packed : 'a s [@@deriving jsonaf_of]

  let%expect_test _ =
    let jsonaf = [%jsonaf_of: int s] Packed in
    print_s (Jsonaf.sexp_of_t jsonaf);
    [%expect {| (Array ((String Packed))) |}]
  ;;

  (* two kind of existential variables *)
  type 'a t = Packed : 'a * _ * ('b[@jsonaf.opaque]) -> 'a t [@warning "-3"]
  [@@deriving jsonaf_of]

  let%expect_test _ =
    let jsonaf = [%jsonaf_of: int t] (Packed (2, "asd", 1.)) in
    print_s (Jsonaf.sexp_of_t jsonaf);
    [%expect
      {|
      (Array (
        (String Packed)
        (Number 2)
        (String _)
        (String <opaque>)))
      |}]
  ;;

  (* to_channel stderr ([%jsonaf_of: int t] (Packed (2, "asd", 1.))) *)
  (* plain type with argument *)
  type 'a u = A : 'a -> 'a u [@@deriving jsonaf_of]

  let%expect_test _ =
    let jsonaf = [%jsonaf_of: int u] (A 2) in
    print_s (Jsonaf.sexp_of_t jsonaf);
    [%expect
      {|
      (Array (
        (String A)
        (Number 2)))
      |}]
  ;;

  (* recursive *)
  type v = A : v option -> v [@@deriving jsonaf_of]

  let%expect_test _ =
    let jsonaf = [%jsonaf_of: v] (A (Some (A None))) in
    print_s (Jsonaf.sexp_of_t jsonaf);
    [%expect {| (Array ((String A) (Array ((String A) Null)))) |}]
  ;;

  (* implicit existential variable *)
  type w = A : 'a * int * ('a -> string) -> w [@@deriving jsonaf_of]

  let%expect_test _ =
    let jsonaf = [%jsonaf_of: w] (A (1., 2, Float.to_string)) in
    print_s (Jsonaf.sexp_of_t jsonaf);
    [%expect
      {|
      (Array (
        (String A)
        (String _)
        (Number 2)
        (String <fun>)))
      |}]
  ;;

  (* tricky variable naming *)
  type 'a x = A : 'a -> 'b x [@@deriving jsonaf_of]

  let%expect_test _ =
    let jsonaf = [%jsonaf_of: int x] (A 1.) in
    print_s (Jsonaf.sexp_of_t jsonaf);
    [%expect
      {|
      (Array (
        (String A)
        (String _)))
      |}]
  ;;

  (* interaction with inline record *)
  type _ x2 = A : { x : 'c } -> 'c x2 [@@deriving jsonaf_of]

  let%expect_test _ =
    let jsonaf = [%jsonaf_of: int x2] (A { x = 1 }) in
    print_s (Jsonaf.sexp_of_t jsonaf);
    [%expect {| (Array ((String A) (Object ((x (Number 1)))))) |}]
  ;;

  (* unused but colliding variables *)
  type (_, _) y = A : ('a, 'a) y [@@deriving jsonaf_of]

  let%expect_test _ =
    let jsonaf = [%jsonaf_of: (int, int) y] A in
    print_s (Jsonaf.sexp_of_t jsonaf);
    [%expect {| (Array ((String A))) |}]
  ;;

  (* making sure we're not reversing parameters *)
  type (_, _) z = A : ('a * 'b) -> ('a, 'b) z [@@deriving jsonaf_of]

  let%expect_test _ =
    let jsonaf = [%jsonaf_of: (int, string) z] (A (1, "a")) in
    print_s (Jsonaf.sexp_of_t jsonaf);
    [%expect
      {|
      (Array (
        (String A)
        (Array (
          (Number 1)
          (String a)))))
      |}]
  ;;

  (* interaction with universal quantifiers *)
  type _ z2 = A : { x : 'c. 'c option } -> 'c z2 [@@deriving jsonaf_of]

  let%expect_test _ =
    let jsonaf = [%jsonaf_of: unit z2] (A { x = None }) in
    print_s (Jsonaf.sexp_of_t jsonaf);
    [%expect {| (Array ((String A) (Object ((x Null))))) |}]
  ;;
end

module Anonymous_variable = struct
  type _ t = int [@@deriving jsonaf]

  let%expect_test _ =
    let jsonaf = [%jsonaf_of: _ t] 2 in
    print_s (Jsonaf.sexp_of_t jsonaf);
    require Poly.([%of_jsonaf: _ t] jsonaf = 2);
    [%expect {| (Number 2) |}]
  ;;

  (* making sure we don't generate signatures like (_ -> t) -> _ t -> t which are too
     general *)
  module M : sig
    type _ t [@@deriving jsonaf]
  end = struct
    type 'a t = 'a [@@deriving jsonaf]
  end
end

module Record_field_disambiguation = struct
  type a =
    { fl : float
    ; b : b
    }

  and b = { fl : int } [@@deriving jsonaf]
end

module Private = struct
  type t = private int [@@deriving jsonaf_of]
  type ('a, 'b) u = private t [@@deriving jsonaf_of]
  type ('a, 'b, 'c) v = private ('a, 'b) u [@@deriving jsonaf_of]
end

module Nonregular_types = struct
  type 'a nonregular =
    | Leaf of 'a
    | Branch of ('a * 'a) nonregular
  [@@deriving jsonaf]

  type 'a variant = [ `A of 'a ] [@@deriving jsonaf]

  type ('a, 'b) nonregular_with_variant =
    | Branch of ([ | 'a list variant ], 'b) nonregular_with_variant
  [@@deriving jsonaf]
end

module Opaque = struct
  type t = (int[@jsonaf.opaque]) list [@@deriving jsonaf]

  let%expect_test _ =
    let value = [ 1; 2 ] in
    let jsonaf = jsonaf_of_t value in
    print_s (Jsonaf.sexp_of_t jsonaf);
    require_does_raise (fun () -> t_of_jsonaf jsonaf);
    [%expect
      {|
      (Array (
        (String <opaque>)
        (String <opaque>)))
      (Of_jsonaf_error
       "opaque_of_jsonaf: cannot convert opaque values"
       (String <opaque>))
      |}]
  ;;

  type u = ([ `A of int ][@jsonaf.opaque]) [@@deriving jsonaf]

  let%expect_test _ =
    let value = `A 1 in
    let jsonaf = jsonaf_of_u value in
    print_s (Jsonaf.sexp_of_t jsonaf);
    require_does_raise (fun () -> u_of_jsonaf jsonaf);
    [%expect
      {|
      (String <opaque>)
      (Of_jsonaf_error
       "opaque_of_jsonaf: cannot convert opaque values"
       (String <opaque>))
      |}]
  ;;
end

module Optional = struct
  type t = { optional : int option [@jsonaf.option] } [@@deriving jsonaf, equal]

  let ( = ) = equal

  let%expect_test _ =
    let value = { optional = None } in
    let jsonaf = jsonaf_of_t value in
    print_s (Jsonaf.sexp_of_t jsonaf);
    require (t_of_jsonaf jsonaf = value);
    [%expect {| (Object ()) |}]
  ;;

  let%expect_test _ =
    let value = { optional = Some 5 } in
    let jsonaf = jsonaf_of_t value in
    print_s (Jsonaf.sexp_of_t jsonaf);
    require (t_of_jsonaf jsonaf = value);
    [%expect {| (Object ((optional (Number 5)))) |}]
  ;;
end

module Listed = struct
  type t =
    { listed : int list [@jsonaf.list]
    ; unlisted : int list
    }
  [@@deriving jsonaf, equal]

  let ( = ) = equal

  let%expect_test _ =
    let value = { listed = []; unlisted = [] } in
    let jsonaf = jsonaf_of_t value in
    print_s (Jsonaf.sexp_of_t jsonaf);
    [%expect {| (Object ((unlisted (Array ())))) |}];
    require (t_of_jsonaf jsonaf = value);
    let json_string = Jsonaf.to_string_hum jsonaf in
    print_endline json_string;
    [%expect
      {|
      {
        "unlisted": []
      }
      |}];
    require (Jsonaf.of_string json_string |> t_of_jsonaf = value)
  ;;

  let%expect_test _ =
    let value = { listed = [ 5 ]; unlisted = [ 5 ] } in
    let jsonaf = jsonaf_of_t value in
    print_s (Jsonaf.sexp_of_t jsonaf);
    [%expect
      {| (Object ((listed (Array ((Number 5)))) (unlisted (Array ((Number 5)))))) |}];
    require (t_of_jsonaf jsonaf = value);
    let json_string = Jsonaf.to_string_hum jsonaf in
    print_endline json_string;
    [%expect
      {|
      {
        "listed": [
          5
        ],
        "unlisted": [
          5
        ]
      }
      |}];
    require (Jsonaf.of_string json_string |> t_of_jsonaf = value)
  ;;
end

module Variance = struct
  type (+'a, -'b, 'c, +_, -_, _) t [@@deriving jsonaf]
end

module Clash = struct
  (* Same name for type-var and type-name; must be careful when introducing rigid type
     names. *)
  type 'hey hey = Hey of 'hey [@@deriving jsonaf]
  type 'hey rigid_hey = Hey of 'hey [@@deriving jsonaf]
  type ('foo, 'rigid_foo) foo = Foo of 'foo [@@deriving jsonaf]
  type 'rigid_bar rigid_rigid_bar = Bar [@@deriving jsonaf]
end

module Applicative_functor_types = struct
  module Bidirectional_map = struct
    type ('k1, 'k2) t

    module S
        (K1 : sig
           type t
         end)
        (K2 : sig
           type t
         end) =
    struct
      type nonrec t = (K1.t, K2.t) t
    end

    module type Of_jsonafable = sig
      type t [@@deriving of_jsonaf]
    end

    let s__t_of_jsonaf
      (type k1 k2)
      (module _ : Of_jsonafable with type t = k1)
      (module _ : Of_jsonafable with type t = k2)
      (_ : Jsonaf_kernel.t)
      : (k1, k2) t
      =
      assert false
    ;;
  end

  module Int = struct
    type t = int [@@deriving of_jsonaf]
  end

  module String = struct
    type t = string [@@deriving of_jsonaf]
  end

  module M : sig
    type t = Bidirectional_map.S(String)(Int).t [@@deriving of_jsonaf]
  end = struct
    type t = Bidirectional_map.S(String)(Int).t [@@deriving of_jsonaf]
  end
end

module Type_extensions = struct
  let (_ : [%jsonaf_of: int]) = [%jsonaf_of: int]
  let (_ : [%of_jsonaf: int]) = [%of_jsonaf: int]
end

module Allow_extra_fields = struct
  module M1 = struct
    type t1 = { a : int } [@@deriving jsonaf, equal]
    type t2 = t1 = { a : int } [@@deriving jsonaf, equal] [@@jsonaf.allow_extra_fields]

    let ( = ) = equal_t2

    let%expect_test _ =
      let jsonaf = Jsonaf.of_string {|{"a":1}|} in
      let jsonaf_extra = Jsonaf.of_string {|{"a":1,"b":2}|} in
      require (t2_of_jsonaf jsonaf = t2_of_jsonaf jsonaf_extra);
      require (t1_of_jsonaf jsonaf = t2_of_jsonaf jsonaf);
      require_does_raise (fun () -> t1_of_jsonaf jsonaf_extra);
      [%expect
        {|
        (Of_jsonaf_error
         "ppx_jsonaf_test.ml.Allow_extra_fields.M1.t1_of_jsonaf: extra fields: b"
         (Object (
           (a (Number 1))
           (b (Number 2)))))
        |}]
    ;;
  end

  module M2 = struct
    type t1 = A of { a : int list } [@@deriving jsonaf, equal]

    type t2 = t1 = A of { a : int list } [@jsonaf.allow_extra_fields]
    [@@deriving jsonaf, equal]

    let ( = ) = equal_t2

    let%expect_test _ =
      let jsonaf = Jsonaf.of_string {|["A",{"a":[0]}]|} in
      let jsonaf_extra = Jsonaf.of_string {|["A",{"a":[0],"b":"1"}]|} in
      require (t2_of_jsonaf jsonaf = t2_of_jsonaf jsonaf_extra);
      require (t1_of_jsonaf jsonaf = t2_of_jsonaf jsonaf);
      require_does_raise (fun () -> t1_of_jsonaf jsonaf_extra);
      [%expect
        {|
        (Of_jsonaf_error
         "ppx_jsonaf_test.ml.Allow_extra_fields.M2.t1_of_jsonaf: extra fields: b"
         (Array ((String A) (Object ((a (Array ((Number 0)))) (b (String 1)))))))
        |}]
    ;;
  end
end

module Allow_extra_fields_log = struct
  module M1 = struct
    type t1 = { a : int } [@@deriving jsonaf, equal]

    type t2 = t1 = { a : int }
    [@@deriving jsonaf, equal] [@@jsonaf.allow_extra_fields.log]

    let ( = ) = equal_t2

    let%expect_test _ =
      let jsonaf = Jsonaf.of_string {|{"a":1}|} in
      let jsonaf_extra = Jsonaf.of_string {|{"a":1,"b":2}|} in
      require (t2_of_jsonaf jsonaf = t2_of_jsonaf jsonaf_extra);
      [%expect
        {| 1969-12-31 19:00:00.000000-05:00 Error "ppx_jsonaf_test.ml.Allow_extra_fields_log.M1.t2_of_jsonaf: extra fields: b" |}];
      require (t1_of_jsonaf jsonaf = t2_of_jsonaf jsonaf_extra);
      [%expect
        {| 1969-12-31 19:00:00.000000-05:00 Error "ppx_jsonaf_test.ml.Allow_extra_fields_log.M1.t2_of_jsonaf: extra fields: b" |}]
    ;;
  end
end

module Exceptions = struct
  module Variant = struct
    type t =
      | A [@name "AA"]
      | B of int
      | C of { a : int }
      | D of int * string
    [@@deriving jsonaf]

    let%expect_test _ =
      let wrong_constr_name = `Array [ `String "Z" ] in
      require_does_raise (fun () -> t_of_jsonaf wrong_constr_name);
      [%expect
        {|
        (Of_jsonaf_error
         "ppx_jsonaf_test.ml.Exceptions.Variant.t_of_jsonaf: unexpected variant constructor"
         (Array ((String Z))))
        |}]
    ;;

    let%expect_test _ =
      let wrong_constr_name = `Array [ `String "A" ] in
      require_does_raise (fun () -> t_of_jsonaf wrong_constr_name);
      [%expect
        {|
        (Of_jsonaf_error
         "ppx_jsonaf_test.ml.Exceptions.Variant.t_of_jsonaf: unexpected variant constructor"
         (Array ((String A))))
        |}]
    ;;

    let%expect_test _ =
      let wrong_constr_name = `Array [ `String "AA" ] in
      require_does_not_raise (fun () -> Base.ignore (t_of_jsonaf wrong_constr_name));
      [%expect {| |}]
    ;;

    let%expect_test _ =
      let wrong_arg_type = `Array [ `String "B"; `Number "1." ] in
      require_does_raise (fun () -> t_of_jsonaf wrong_arg_type);
      [%expect {| (Of_jsonaf_error "int_of_jsonaf: integer needed" (Number 1.)) |}]
    ;;

    let%expect_test _ =
      let wrong_arg_type = `Array [ `String "C"; `String "string" ] in
      require_does_raise (fun () -> t_of_jsonaf wrong_arg_type);
      [%expect
        {|
        (Of_jsonaf_error
         "ppx_jsonaf_test.ml.Exceptions.Variant.t_of_jsonaf: unexpected variant constructor"
         (Array (
           (String C)
           (String string))))
        |}]
    ;;

    let%expect_test _ =
      let wrong_arg_type = `Array [ `String "C"; `Object [ "b", `Number "1" ] ] in
      require_does_raise (fun () -> t_of_jsonaf wrong_arg_type);
      [%expect
        {|
        (Of_jsonaf_error
         "ppx_jsonaf_test.ml.Exceptions.Variant.t_of_jsonaf: extra fields: b"
         (Array ((String C) (Object ((b (Number 1)))))))
        |}]
    ;;

    let%expect_test _ =
      let wrong_arg_type = `Array [ `String "D"; `Number "1"; `Number "1." ] in
      require_does_raise (fun () -> t_of_jsonaf wrong_arg_type);
      [%expect {| (Of_jsonaf_error "string_of_jsonaf: string needed" (Number 1.)) |}]
    ;;

    let%expect_test _ =
      let wrong_arg_num = `Array [ `String "D"; `Array [ `Number "1"; `Number "1." ] ] in
      require_does_raise (fun () -> t_of_jsonaf wrong_arg_num);
      [%expect
        {|
        (Of_jsonaf_error
         "ppx_jsonaf_test.ml.Exceptions.Variant.t_of_jsonaf: sum tag \"D\" has incorrect number of arguments"
         (Array (
           (String D)
           (Array (
             (Number 1)
             (Number 1.))))))
        |}]
    ;;

    let%expect_test _ =
      let wrong_arg_num =
        `Array [ `String "D"; `Number "1"; `Number "1."; `String "str" ]
      in
      require_does_raise (fun () -> t_of_jsonaf wrong_arg_num);
      [%expect
        {|
        (Of_jsonaf_error
         "ppx_jsonaf_test.ml.Exceptions.Variant.t_of_jsonaf: sum tag \"D\" has incorrect number of arguments"
         (Array (
           (String D)
           (Number 1)
           (Number 1.)
           (String str))))
        |}]
    ;;
  end

  module Sum = struct
    type t =
      [ `A
      | `B of int
      | `D of int * string
      ]
    [@@deriving jsonaf]

    let%expect_test _ =
      let wrong_constr_name = `Array [ `String "Z" ] in
      require_does_raise (fun () -> t_of_jsonaf wrong_constr_name);
      [%expect
        {|
        (Of_jsonaf_error
         "ppx_jsonaf_test.ml.Exceptions.Sum.t_of_jsonaf: no matching variant found"
         (Array ((String Z))))
        |}]
    ;;

    let%expect_test _ =
      let wrong_arg_type = `Array [ `String "B"; `Number "1." ] in
      require_does_raise (fun () -> t_of_jsonaf wrong_arg_type);
      [%expect {| (Of_jsonaf_error "int_of_jsonaf: integer needed" (Number 1.)) |}]
    ;;

    let%expect_test _ =
      let wrong_arg_type = `Array [ `String "D"; `Number "1"; `Number "1." ] in
      require_does_raise (fun () -> t_of_jsonaf wrong_arg_type);
      [%expect {| (Of_jsonaf_error "string_of_jsonaf: string needed" (Number 1.)) |}]
    ;;

    let%expect_test _ =
      let wrong_arg_num = `Array [ `String "D"; `Array [ `Number "1"; `Number "1." ] ] in
      require_does_raise (fun () -> t_of_jsonaf wrong_arg_num);
      [%expect
        {|
        (Of_jsonaf_error
         "ppx_jsonaf_test.ml.Exceptions.Sum.t_of_jsonaf: polymorphic variant tag \"D\" has incorrect number of arguments"
         (Array (
           (String D)
           (Array (
             (Number 1)
             (Number 1.))))))
        |}]
    ;;

    let%expect_test _ =
      let wrong_arg_num =
        `Array [ `String "D"; `Number "1"; `Number "1."; `String "str" ]
      in
      require_does_raise (fun () -> t_of_jsonaf wrong_arg_num);
      [%expect
        {|
        (Of_jsonaf_error
         "ppx_jsonaf_test.ml.Exceptions.Sum.t_of_jsonaf: polymorphic variant tag \"D\" has incorrect number of arguments"
         (Array (
           (String D)
           (Number 1)
           (Number 1.)
           (String str))))
        |}]
    ;;
  end

  module Record = struct
    type t =
      { a : int [@key "A"]
      ; b : string
      ; c : float
      ; d : int option
      ; e : int option [@default None]
      ; f : int [@default 0]
      }
    [@@deriving jsonaf]

    let%expect_test _ =
      let wrong_field_name =
        `Object
          [ "a", `Number "1"
          ; "b", `String "str"
          ; "c", `Number "1."
          ; "d", `Null
          ; "e", `Number "1"
          ; "f", `Number "1"
          ]
      in
      require_does_raise (fun () -> t_of_jsonaf wrong_field_name);
      [%expect
        {|
        (Of_jsonaf_error
         "ppx_jsonaf_test.ml.Exceptions.Record.t_of_jsonaf: extra fields: a"
         (Object (
           (a (Number 1))
           (b (String str))
           (c (Number 1.))
           (d Null)
           (e (Number 1))
           (f (Number 1)))))
        |}]
    ;;

    let%expect_test _ =
      let wrong_field_type =
        `Object
          [ "A", `String "A"
          ; "b", `String "str"
          ; "c", `Number "1."
          ; "d", `Null
          ; "e", `Number "1"
          ; "f", `Number "1"
          ]
      in
      require_does_raise (fun () -> t_of_jsonaf wrong_field_type);
      [%expect {| (Of_jsonaf_error "int_of_jsonaf: integer needed" (String A)) |}]
    ;;

    let%expect_test _ =
      let wrong_field_number =
        `Object [ "A", `Number "1"; "b", `String "str"; "c", `Number "1." ]
      in
      require_does_raise (fun () -> t_of_jsonaf wrong_field_number);
      [%expect
        {|
        (Of_jsonaf_error
         "ppx_jsonaf_test.ml.Exceptions.Record.t_of_jsonaf: the following record elements were undefined: d"
         (Object (
           (A (Number 1))
           (b (String str))
           (c (Number 1.)))))
        |}]
    ;;

    let%expect_test _ =
      let duplicate_fields =
        `Object
          [ "A", `Number "1"
          ; "b", `String "str"
          ; "c", `Number "1."
          ; "d", `Null
          ; "e", `Number "1"
          ; "f", `Number "1"
          ; "f", `Number "1"
          ]
      in
      require_does_raise (fun () -> t_of_jsonaf duplicate_fields);
      [%expect
        {|
        (Of_jsonaf_error
         "ppx_jsonaf_test.ml.Exceptions.Record.t_of_jsonaf: duplicate fields: f"
         (Object (
           (A (Number 1))
           (b (String str))
           (c (Number 1.))
           (d Null)
           (e (Number 1))
           (f (Number 1))
           (f (Number 1)))))
        |}]
    ;;

    let%expect_test _ =
      let extra_fields =
        `Object
          [ "A", `Number "1"
          ; "b", `String "str"
          ; "c", `Number "1."
          ; "d", `Null
          ; "e", `Number "1"
          ; "f", `Number "1"
          ; "g", `Number "1"
          ]
      in
      require_does_raise (fun () -> t_of_jsonaf extra_fields);
      [%expect
        {|
        (Of_jsonaf_error
         "ppx_jsonaf_test.ml.Exceptions.Record.t_of_jsonaf: extra fields: g"
         (Object (
           (A (Number 1))
           (b (String str))
           (c (Number 1.))
           (d Null)
           (e (Number 1))
           (f (Number 1))
           (g (Number 1)))))
        |}]
    ;;
  end
end

module Bignum = struct
  module Bignum = struct
    include Bignum

    let jsonaf_of_t t =
      if Bignum.equal (Bignum.den t) Bignum.zero
      then `String (Bignum.to_string_hum t)
      else (
        match Bignum.to_string_decimal_accurate t with
        | Ok s -> `Number s
        | Error _ -> `Number (Bignum.to_string_hum t))
    ;;

    let t_of_jsonaf = Jsonaf_bignum.bignum_of_number_or_string_exn
  end

  let%expect_test "roundtrip" =
    let f value =
      let jsonaf = Bignum.jsonaf_of_t value in
      print_s (Jsonaf.sexp_of_t jsonaf);
      require (Bignum.equal (Bignum.t_of_jsonaf jsonaf) value)
    in
    f (Bignum.of_string "123456789012345.123456789012345");
    [%expect {| (Number 123456789012345.123456789012345) |}];
    f (Bignum.of_string "nan");
    [%expect {| (String nan) |}];
    f (Bignum.of_string "inf");
    [%expect {| (String inf) |}];
    f (Bignum.of_string "-inf");
    [%expect {| (String -inf) |}]
  ;;

  let%expect_test "don't roundtrip" =
    let f value =
      let jsonaf = Bignum.jsonaf_of_t value in
      let value' = Bignum.t_of_jsonaf jsonaf in
      print_s (Jsonaf.sexp_of_t jsonaf);
      if Bignum.equal value' value
      then ()
      else print_s [%sexp { original = (value : Bignum.t); parsed = (value' : Bignum.t) }]
    in
    f (Bignum.of_string "1/3");
    [%expect
      {|
      (Number 0.333333333)
      ((original (0.333333333 + 1/3000000000)) (parsed 0.333333333))
      |}]
  ;;
end
