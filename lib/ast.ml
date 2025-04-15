type sort_order =
  | Asc
  | Desc

and nones_order =
  | First
  | Last

and cardinality_modifier =
  | Optional
  | Required

and sort_expr =
  { path : expr
  ; direction : sort_order option
  ; nones_order : nones_order option
  }

and window_spec =
  { order_by : sort_expr list
  ; partition : expr list
  }

and str_interp_fragment =
  { expr : expr
  ; suffix : string
  }

and ptr =
  { name : string
  ; direction : string option
  ; type_ : string option
  }

and name =
  | UnqualName of string
  | QualName of string * string

and base_object_ref =
  | ObjectRef of object_ref
  | PseudoObjectRef of string

and object_ref =
  { name : string
  ; module_ : string option
  }

and expr =
  | DetachedExpr of
      { expr : expr
      ; preserve_path_prefix : bool
      }
  | BinOp of
      { left : expr
      ; op : string
      ; right : expr
      ; rebalanced : bool
      ; set_constructor : bool
      }
  | FunctionCall of
      { func : name
      ; args : expr list
      ; kwargs : (string * expr) list
      ; window : window_spec option
      }
  | StrInterp of
      { prefix : string
      ; interpolations : str_interp_fragment list
      }
  | Parameter of string
  | UnaryOp of string * expr
  | IsOp of expr * string * type_expr
  | TypeCast of expr * type_expr * cardinality_modifier option
  | IfElse of
      { condition : expr
      ; if_expr : expr
      ; else_expr : expr
      }
  | NamedTuple of (ptr * expr) list
  | Tuple of expr list
  | Array of expr list
  | Set of expr list
  | ShapeElement of shape_element
  | Shape of
      { expr : expr option
      ; elements : shape_element list
      ; allow_factoring : bool
      }
  | Path of path
  | Query of query

and type_expr =
  | TypeOf of expr
  | TypeExprLiteral of constant
  | TypeName of
      { name : string
      ; main_type : base_object_ref
      ; sub_types : type_expr list option
      ; dimensions : int list option
      }
  | TypeOp of type_expr * string * type_expr

and constant =
  | String of string
  | Bytes of bytes
  | Boolean of bool
  | Integer of string
  | Float of string
  | Bigint of string
  | Decimal of string

and type_intersection = type_expr

and path_element =
  | Expr of expr
  | Ptr of ptr
  | TypeIntersection of type_intersection
  | ObjectRef of object_ref
  | Splat of
      { depth : int
      ; type_ : type_expr option
      ; intersection : type_intersection option
      }

and path =
  { steps : path_element list
  ; partial : bool
  ; allow_factoring : bool
  }

and shape_op =
  | Append
  | Subtract
  | Assign
  | Materialize

and shape_origin =
  | Explicit
  | Default
  | SplatExpansion
  | Materialization

and shape_element =
  { expr : path
  ; elements : shape_element list option
  ; compexpr : expr option
  ; cardinality : cardinality_modifier option
  ; required : bool option
  ; operation : shape_op
  ; origin : shape_origin
  ; where : expr option
  ; orderby : sort_expr list option
  ; offset : expr option
  ; limit : expr option
  }

and alias =
  | AliasedExpr of
      { alias : string
      ; expr : expr
      }
  | ModuleAliasDecl of
      { module_ : string
      ; alias : string option
      }

and grouping_atom =
  | GroupingObjectRef of object_ref
  | GroupingPath of path
  | GroupingIdentList of grouping_atom list

and grouping_element =
  | GroupingSimple of grouping_atom
  | GroupingSets of grouping_element list
  | GroupingOperation of
      { oper : string
      ; elements : grouping_atom list
      }

and query =
  | SelectQuery of
      { result_alias : string option
      ; result : expr
      ; where : expr option
      ; orderby : sort_expr list option
      ; offset : expr option
      ; limit : expr option
      ; rptr_passthrough : bool
      ; implicit : bool
      ; aliases : alias list option
      }
  | InsertQuery of
      { subject : object_ref
      ; shape : shape_element list
      ; unless_conflict : (expr option * expr option) option
      ; aliases : alias list option
      }
  | UpdateQuery of
      { shape : shape_element list
      ; subject : expr
      ; where : expr option
      ; sql_mode_link_only : bool
      ; aliases : alias list option
      }
  | DeleteQuery of
      { subject : expr
      ; where : expr option
      ; orderby : sort_expr list option
      ; offset : expr option
      ; limit : expr option
      ; aliases : alias list option
      }
  | ForQuery of
      { from_desugaring : bool
      ; has_union : bool
      ; optional : bool
      ; iterator : expr
      ; iterator_alias : string
      ; result_alias : string option
      ; result : expr
      ; aliases : alias list option
      }
