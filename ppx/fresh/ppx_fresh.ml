(*
  rewrites
  fresh (a b ... z) ... to

  Fresh.one (fun a -> Fresh.one (fun b -> ... Fresh.one (fun z -> ... )))
*)
open Base
open Ppxlib
open Ppxlib.Ast_helper

let is_fresh e =
  match e.pexp_desc with
  | Pexp_ident { txt = Lident "fresh"; _ } -> true
  | _ -> false
;;

let reconstruct_args e =
  match e.pexp_desc with
  (* f x ... z ~~> [f; x; ...; z] *)
  | Pexp_apply
      ( ({ pexp_desc =
             Pexp_ident { txt = Longident.Lident _arg1; _ }
         ; _
         } as f)
      , ys ) -> f :: List.map ~f:snd ys
  (* f  ~~> [f] *)
  | Pexp_ident { txt = Lident _arg1; _ } -> [ e ]
  | _ -> failwith "error"
;;

let my_list ~loc es =
  List.fold_right ~init:[%expr []] es ~f:(fun x acc ->
      [%expr [%e x] :: [%e acc]])
;;

let mapper =
  object (self)
    inherit Ast_traverse.map as super

    method! expression e =
      let loc = e.pexp_loc in
      match e.pexp_desc with
      | Pexp_apply (efresh, (Nolabel, args) :: body)
        when is_fresh efresh ->
        assert (List.length body > 0);
        let new_body =
          body
          |> List.map ~f:snd
          |> List.map ~f:self#expression
          |> function
          | [ h ] -> h
          | xs -> [%expr ?&[%e my_list ~loc xs]]
        in
        let names =
          List.map (reconstruct_args args) ~f:(fun e ->
              match e.pexp_desc with
              | Pexp_ident { txt = Lident lident; _ } ->
                e.pexp_loc, lident
              | _ -> assert false)
        in
        List.fold_right
          names
          ~init:new_body
          ~f:(fun (vloc, lident) acc ->
            let pat =
              Pat.var
                ~loc:vloc
                  (* we declare a pattern with location of original identifier *)
                (Ast_builder.Default.Located.mk lident ~loc)
            in
            let loc = efresh.pexp_loc in
            [%expr Fresh.one (fun [%p pat] -> [%e acc])])
      | _ -> super#expression e
  end
;;

let () =
  Ppxlib.Driver.register_transformation
    ~impl:mapper#structure
    "somename"
;;
