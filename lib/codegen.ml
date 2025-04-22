open Ast

let rec visit_query = function
  | SelectQuery
      { result_alias
      ; result
      ; where
      ; orderby
      ; offset
      ; limit
      ; rptr_passthrough = _
      ; implicit
      ; aliases
      } ->
      visit_select_query result_alias result where orderby offset limit implicit
        aliases
  | InsertQuery { subject; shape; unless_conflict; aliases } ->
      visit_insert_query subject shape unless_conflict aliases
  | UpdateQuery { subject; shape; where; sql_mode_link_only = _; aliases } ->
      visit_update_query subject shape where aliases
  | DeleteQuery { subject; where; orderby; offset; limit; aliases } ->
      visit_delete_query subject where orderby offset limit aliases
  | ForQuery
      { from_desugaring = _
      ; has_union
      ; optional = _
      ; iterator
      ; iterator_alias
      ; result_alias = _
      ; result
      ; aliases
      } ->
      visit_for_query has_union iterator iterator_alias result aliases

and visit_aliases (aliases : alias list option) : string =
  match aliases with
  | None | Some [] -> ""
  | Some aliases ->
      let process_alias = function
        | AliasedExpr { alias; expr } ->
            Format.sprintf "%s := %s" alias (visit_expr expr)
        | ModuleAliasDecl { module_; alias = None } ->
            Format.sprintf "module %s" module_
        | ModuleAliasDecl { module_; alias = Some a } ->
            Format.sprintf "%s as module %s" a module_
      in
      let alias_strings = List.map process_alias aliases in
      "with " ^ String.concat ", " alias_strings ^ " "

and visit_set (elements : expr list) : string =
  match elements with
  | elements ->
      let visit_element idx elem =
        let delim = if idx = 0 then "" else ", " in
        delim ^ visit_expr elem
      in
      "{" ^ (elements |> List.mapi visit_element |> String.concat "") ^ "}"

and visit_constant (c : constant) : string =
  match c with
  | String s -> "'" ^ s ^ "'"
  | Boolean b -> if b then "true" else "false"
  | Integer i -> i
  | Float f -> f
  | Bigint bi -> bi
  | Decimal d -> d
  | _ -> failwith "Unsupported constant type"

and visit_path (p : path) : string =
  let rec visit_step ret i = function
    | [] -> ret
    | step :: rest ->
        let prefix =
          if
            i > 0
            || p.partial
               &&
               match step with
               | TypeIntersection _ -> false
               | Ptr { type_ = Some "property"; _ } -> false
               | _ -> true
          then ret ^ "."
          else ret
        in
        let ret =
          match step with
          | ObjectRef _ | Splat _ | Ptr _
          | Expr (Set _ | Tuple _ | NamedTuple _ | Parameter _)
          | TypeIntersection _ ->
              prefix ^ visit_path_element step
          | _ when i = 0 -> prefix ^ "(" ^ visit_path_element step ^ ")"
          | _ -> prefix ^ visit_path_element step
        in
        visit_step ret (i + 1) rest
  in
  visit_step "" 0 p.steps

and visit_path_element (el : path_element) : string =
  match el with
  | Expr e -> visit_expr e
  | ObjectRef o -> visit_object_ref o
  | Ptr p -> visit_ptr p
  | _ -> failwith "Unsupported path element type"

and visit_object_ref (o : object_ref) : string =
  match o.module_ with Some m -> m ^ "::" ^ o.name | None -> o.name

and visit_ptr (p : ptr) : string =
  let prefix =
    match p.type_ with
    | Some "property" -> "@"
    | _ -> ( match p.direction with Some d when d <> ">" -> d | _ -> "")
  in
  prefix ^ p.name

and visit_shape (elements : shape_element list) : string =
  match elements with
  | [] -> "{}"
  | _ ->
      let content =
        elements |> List.map visit_shape_element |> String.concat ", "
      in
      "{" ^ content ^ "}"

and visit_shape_element (el : shape_element) : string =
  let quals =
    (match el.required with
    | Some true -> [ "required" ]
    | Some false -> [ "optional" ]
    | None -> [])
    @
    match el.cardinality with
    | Some One -> [ "single" ]
    | Some Many -> [ "multi" ]
    | None -> []
  in
  let qual_str = if quals = [] then "" else String.concat " " quals in

  let steps = el.expr.steps in
  let path_str =
    match steps with
    | [ _ ] -> visit_path el.expr
    | x :: y :: rest ->
        let dot = match y with TypeIntersection _ -> "" | _ -> "." in
        let x_str = visit_path_element x in
        let y_str = visit_path_element y in
        let z_str =
          match rest with z :: _ -> visit_path_element z | [] -> ""
        in
        x_str ^ dot ^ y_str ^ z_str
    | _ -> ""
  in

  let elements_str =
    match (el.compexpr, el.elements) with
    | None, Some elems -> ": " ^ visit_shape elems
    | Some expr, _ ->
        (match el.operation with
        | Assign -> " := "
        | Append -> " += "
        | Subtract -> " -= "
        | _ -> failwith "unexpected shape operation")
        ^ "(" ^ visit_expr expr ^ ")"
    | _ -> ""
  in

  qual_str ^ path_str ^ elements_str

and visit_expr = function
  | Constant c -> visit_constant c
  | Set elements ->
      let visit_element idx elem =
        let delim = if idx = 0 then "" else ", " in
        delim ^ visit_expr elem
      in
      "{" ^ (elements |> List.mapi visit_element |> String.concat "") ^ "}"
  | Path p -> visit_path p
  | Shape { expr; elements; allow_factoring = _ } ->
      let expr_str =
        match expr with Some e -> visit_expr e ^ " " | None -> ""
      in
      let elements_str = visit_shape elements in
      expr_str ^ elements_str
  | BinOp { left : expr; op : string; right : expr; _ } ->
      let left_str = visit_expr left in
      let right_str = visit_expr right in
      left_str ^ " " ^ op ^ " " ^ right_str
  | Query q -> visit_query q
  | _ -> failwith "Unsupported expr type "

and visit_filter (where : expr option) : string =
  match where with Some expr -> visit_expr expr | None -> ""

and visit_sort_expr (expr : sort_expr) : string =
  let path_str = visit_expr expr.path in

  let dir_str =
    match expr.direction with
    | Some Asc -> " asc"
    | Some Desc -> " desc"
    | None -> ""
  in

  let nulls_str =
    match expr.nones_order with
    | Some First -> " empty first"
    | Some Last -> " empty last"
    | None -> ""
  in

  path_str ^ dir_str ^ nulls_str

and visit_select_query (result_alias : string option) (result : expr)
    (where : expr option) (orderby : sort_expr list option)
    (offset : expr option) (limit : expr option) (implicit : bool)
    (aliases : alias list option) : string =
  let with_clause = visit_aliases aliases in
  let parenthesise = (not implicit) || Option.is_some aliases in

  let select_kw = if parenthesise then "select " else "(select " in

  let select_body = visit_expr result in

  let where_str =
    match where with None -> "" | Some e -> " filter " ^ visit_expr e
  in

  let order_str =
    match orderby with
    | Some ob_exprs when ob_exprs <> [] ->
        " order by "
        ^ (ob_exprs |> List.map visit_sort_expr |> String.concat " then")
    | _ -> ""
  in

  let offset_str =
    match offset with Some o -> " offset " ^ visit_expr o | None -> ""
  in

  let limit_str =
    match limit with Some l -> " limit " ^ visit_expr l | None -> ""
  in

  with_clause ^ select_kw
  ^ (match result_alias with
    | None -> select_body
    | Some alias -> alias ^ " := " ^ select_body)
  ^ where_str ^ order_str ^ offset_str ^ limit_str
  ^ if parenthesise then "" else ")"

and visit_insert_query (subject : object_ref) (shape : shape_element list)
    (unless_conflict : (expr option * expr option) option)
    (aliases : alias list option) : string =
  let with_clause = visit_aliases aliases in
  let subject_str = visit_object_ref subject in
  let shape_str = match shape with [] -> "" | _ -> visit_shape shape in

  let conflict_str =
    match unless_conflict with
    | Some (on_expr, else_expr) ->
        let on_str =
          match on_expr with Some e -> " on " ^ visit_expr e | None -> ""
        in
        let else_str =
          match else_expr with Some e -> " else " ^ visit_expr e | None -> ""
        in
        "unless conflict" ^ on_str ^ else_str
    | None -> ""
  in

  with_clause ^ " insert " ^ subject_str ^ " " ^ shape_str ^ conflict_str

and visit_update_query (subject : expr) (shape : shape_element list)
    (where : expr option) (aliases : alias list option) : string =
  let where_str =
    match where with None -> "" | Some e -> " filter " ^ visit_expr e
  in
  let shape_str = match shape with [] -> "" | _ -> visit_shape shape in

  visit_aliases aliases ^ "update " ^ visit_expr subject ^ where_str ^ " set "
  ^ shape_str

and visit_delete_query (subject : expr) (where : expr option)
    (orderby : sort_expr list option) (offset : expr option)
    (limit : expr option) (aliases : alias list option) : string =
  let where_str =
    match where with None -> "" | Some e -> " filter " ^ visit_expr e
  in

  let order_str =
    match orderby with
    | Some ob_exprs when ob_exprs <> [] ->
        " order by "
        ^ (ob_exprs |> List.map visit_sort_expr |> String.concat " then")
    | _ -> ""
  in

  let offset_str =
    match offset with Some o -> " offset " ^ visit_expr o | None -> ""
  in

  let limit_str =
    match limit with Some l -> " limit " ^ visit_expr l | None -> ""
  in

  visit_aliases aliases ^ "delete " ^ visit_expr subject ^ where_str ^ order_str
  ^ offset_str ^ limit_str

and visit_for_query (has_union : bool) (iterator : expr)
    (iterator_alias : string) (result : expr) (aliases : alias list option) :
    string =
  let result_str =
    if has_union then " union (" ^ visit_expr result ^ ")" else visit_expr result
  in
  visit_aliases aliases ^ "for " ^ iterator_alias ^ " in " ^ visit_expr iterator
  ^ result_str
