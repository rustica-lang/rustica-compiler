open Ast
open Codegen

let%expect_test "test for aliases" =
  let test_aliases =
    Some
      [ ModuleAliasDecl { module_ = "default"; alias = None }
      ; AliasedExpr { alias = "my_str"; expr = Constant (String "hello world") }
      ; ModuleAliasDecl { module_ = "auth"; alias = Some "a" }
      ]
  in
  print_endline (visit_aliases test_aliases);
  [%expect {| with module default, my_str := 'hello world', a as module auth |}]

let%expect_test "test for inline aliases" =
  let ast =
    SelectQuery
      { result_alias = Some "x"
      ; result =
          Set
            [ Constant (Integer "1")
            ; Constant (Integer "2")
            ; Constant (Integer "3")
            ]
      ; where = None
      ; orderby = Some []
      ; offset = None
      ; limit = None
      ; rptr_passthrough = false
      ; implicit = false
      ; aliases = None
      }
  in
  visit_query ast |> print_endline;
  [%expect {| select x := {1, 2, 3} |}]

let%expect_test "test for select query" =
  let ast =
    SelectQuery
      { result_alias = None
      ; result =
          Shape
            { expr =
                Some
                  (Path
                     { steps =
                         [ ObjectRef { name = "User"; module_ = Some "default" }
                         ]
                     ; allow_factoring = false
                     ; partial = false
                     })
            ; allow_factoring = false
            ; elements =
                [ { expr =
                      { steps =
                          [ Ptr { name = "id"; direction = None; type_ = None }
                          ]
                      ; allow_factoring = false
                      ; partial = false
                      }
                  ; elements = None
                  ; compexpr = None
                  ; cardinality = None
                  ; required = None
                  ; operation = Assign
                  ; origin = Explicit
                  ; where = None
                  ; orderby = None
                  ; offset = None
                  ; limit = None
                  }
                ; { expr =
                      { steps =
                          [ Ptr
                              { name = "name"; direction = None; type_ = None }
                          ]
                      ; allow_factoring = false
                      ; partial = false
                      }
                  ; elements = None
                  ; compexpr = None
                  ; cardinality = None
                  ; required = None
                  ; operation = Assign
                  ; origin = Explicit
                  ; where = None
                  ; orderby = None
                  ; offset = None
                  ; limit = None
                  }
                ; { expr =
                      { steps =
                          [ Ptr
                              { name = "nemesis_name"
                              ; direction = Some ">"
                              ; type_ = None
                              }
                          ]
                      ; allow_factoring = false
                      ; partial = false
                      }
                  ; elements = None
                  ; compexpr =
                      Some
                        (Path
                           { steps =
                               [ Ptr
                                   { name = "nemesis"
                                   ; direction = Some ">"
                                   ; type_ = None
                                   }
                               ; Ptr
                                   { name = "name"
                                   ; direction = Some ">"
                                   ; type_ = None
                                   }
                               ]
                           ; allow_factoring = false
                           ; partial = true
                           })
                  ; cardinality = None
                  ; required = None
                  ; operation = Assign
                  ; origin = Explicit
                  ; where = None
                  ; orderby = None
                  ; offset = None
                  ; limit = None
                  }
                ]
            }
      ; where =
          Some
            (BinOp
               { left =
                   Path
                     { steps =
                         [ Ptr { name = "name"; direction = None; type_ = None }
                         ]
                     ; allow_factoring = false
                     ; partial = true
                     }
               ; op = "="
               ; right = Constant (String "Spider-Man")
               ; rebalanced = false
               ; set_constructor = false
               })
      ; orderby =
          Some
            [ { path =
                  Path
                    { steps =
                        [ Ptr { name = "name"; direction = None; type_ = None }
                        ]
                    ; allow_factoring = false
                    ; partial = true
                    }
              ; direction = Some Asc
              ; nones_order = None
              }
            ]
      ; offset = Some (Constant (Integer "1"))
      ; limit = Some (Constant (Integer "2"))
      ; rptr_passthrough = false
      ; implicit = false
      ; aliases = Some [ ModuleAliasDecl { module_ = "default"; alias = None } ]
      }
  in
  visit_query ast |> print_endline;
  [%expect
    {| with module default select default::User {id, name, nemesis_name := (.nemesis.name)} filter .name = 'Spider-Man' order by .name asc offset 1 limit 2 |}]

let%expect_test "test for insert query" =
  let ast =
    InsertQuery
      { subject = { name = "Movie"; module_ = None }
      ; shape =
          [ { expr =
                { steps =
                    [ Ptr { name = "title"; direction = Some ">"; type_ = None }
                    ]
                ; allow_factoring = false
                ; partial = false
                }
            ; elements = None
            ; compexpr = Some (Constant (String "Spider-Man: No Way Home"))
            ; cardinality = None
            ; required = None
            ; operation = Assign
            ; origin = Explicit
            ; where = None
            ; orderby = None
            ; offset = None
            ; limit = None
            }
          ; { expr =
                { steps =
                    [ Ptr
                        { name = "release_year"
                        ; direction = Some ">"
                        ; type_ = None
                        }
                    ]
                ; allow_factoring = false
                ; partial = false
                }
            ; elements = None
            ; compexpr = Some (Constant (Integer "2021"))
            ; cardinality = None
            ; required = None
            ; operation = Assign
            ; origin = Explicit
            ; where = None
            ; orderby = None
            ; offset = None
            ; limit = None
            }
          ; { expr =
                { steps =
                    [ Ptr
                        { name = "characters"
                        ; direction = Some ">"
                        ; type_ = None
                        }
                    ]
                ; allow_factoring = false
                ; partial = false
                }
            ; elements = None
            ; compexpr =
                Some
                  (Query
                     (SelectQuery
                        { result_alias = None
                        ; result =
                            Path
                              { steps =
                                  [ ObjectRef
                                      { name = "Person"; module_ = None }
                                  ]
                              ; allow_factoring = false
                              ; partial = false
                              }
                        ; where =
                            Some
                              (BinOp
                                 { left =
                                     Path
                                       { steps =
                                           [ Ptr
                                               { name = "name"
                                               ; direction = None
                                               ; type_ = None
                                               }
                                           ]
                                       ; allow_factoring = false
                                       ; partial = true
                                       }
                                 ; op = "in"
                                 ; right =
                                     Set
                                       [ Constant (String "Spider-Man")
                                       ; Constant (String "Doctor Strange")
                                       ; Constant (String "Doc Ock")
                                       ; Constant (String "Green Goblin")
                                       ]
                                 ; rebalanced = false
                                 ; set_constructor = false
                                 })
                        ; orderby = Some []
                        ; offset = None
                        ; limit = None
                        ; rptr_passthrough = false
                        ; implicit = false
                        ; aliases = None
                        }))
            ; cardinality = None
            ; required = None
            ; operation = Assign
            ; origin = Explicit
            ; where = None
            ; orderby = None
            ; offset = None
            ; limit = None
            }
          ]
      ; unless_conflict = None
      ; aliases = None
      }
  in
  visit_query ast |> print_endline;
  [%expect
    {| insert Movie {title := ('Spider-Man: No Way Home'), release_year := (2021), characters := (select Person filter .name in {'Spider-Man', 'Doctor Strange', 'Doc Ock', 'Green Goblin'})} |}]

let%expect_test "test for update query" =
  let ast =
    UpdateQuery
      { subject =
          Path
            { steps = [ ObjectRef { name = "Hero"; module_ = None } ]
            ; allow_factoring = false
            ; partial = false
            }
      ; shape =
          [ { expr =
                { steps =
                    [ Ptr { name = "name"; direction = Some ">"; type_ = None }
                    ]
                ; allow_factoring = false
                ; partial = false
                }
            ; elements = None
            ; compexpr = Some (Constant (String "Ronin"))
            ; cardinality = None
            ; required = None
            ; operation = Assign
            ; origin = Explicit
            ; where = None
            ; orderby = None
            ; offset = None
            ; limit = None
            }
          ]
      ; where =
          Some
            (BinOp
               { left =
                   Path
                     { steps =
                         [ Ptr
                             { name = "name"
                             ; direction = Some ">"
                             ; type_ = None
                             }
                         ]
                     ; allow_factoring = false
                     ; partial = true
                     }
               ; op = "="
               ; right = Constant (String "Hawkeye")
               ; rebalanced = false
               ; set_constructor = false
               })
      ; sql_mode_link_only = false
      ; aliases = None
      }
  in
  visit_query ast |> print_endline;
  [%expect {| update Hero filter .name = 'Hawkeye' set {name := ('Ronin')} |}]

let%expect_test "test for delete query" =
  let ast =
    DeleteQuery
      { subject =
          Path
            { steps = [ ObjectRef { name = "Hero"; module_ = Some "default" } ]
            ; allow_factoring = false
            ; partial = false
            }
      ; where =
          Some
            (BinOp
               { left =
                   Path
                     { steps =
                         [ Ptr
                             { name = "name"
                             ; direction = Some ">"
                             ; type_ = None
                             }
                         ]
                     ; allow_factoring = false
                     ; partial = true
                     }
               ; op = "ilike"
               ; right = Constant (String "the %")
               ; rebalanced = false
               ; set_constructor = false
               })
      ; orderby =
          Some
            [ { path =
                  Path
                    { steps =
                        [ Ptr
                            { name = "name"
                            ; direction = Some ">"
                            ; type_ = None
                            }
                        ]
                    ; allow_factoring = false
                    ; partial = true
                    }
              ; direction = Some Asc
              ; nones_order = None
              }
            ]
      ; offset = Some (Constant (Integer "10"))
      ; limit = Some (Constant (Integer "5"))
      ; aliases = None
      }
  in
  visit_query ast |> print_endline;
  [%expect
    {| delete default::Hero filter .name ilike 'the %' order by .name asc offset 10 limit 5 |}]

let%expect_test "test for for query" =
  let ast =
    ForQuery
      { result =
          Query
            (SelectQuery
               { result_alias = None
               ; result =
                   Set
                     [ Path
                         { steps =
                             [ ObjectRef { name = "number"; module_ = None } ]
                         ; allow_factoring = false
                         ; partial = false
                         }
                     ; BinOp
                         { left =
                             Path
                               { steps =
                                   [ ObjectRef
                                       { name = "number"; module_ = None }
                                   ]
                               ; allow_factoring = false
                               ; partial = false
                               }
                         ; op = "+"
                         ; right = Constant (Float "0.5")
                         ; rebalanced = false
                         ; set_constructor = false
                         }
                     ]
               ; where = None
               ; orderby = Some []
               ; offset = None
               ; limit = None
               ; rptr_passthrough = false
               ; implicit = false
               ; aliases = None
               })
      ; result_alias = None
      ; optional = false
      ; iterator_alias = "number"
      ; iterator =
          Set
            [ Constant (Integer "0")
            ; Constant (Integer "1")
            ; Constant (Integer "2")
            ; Constant (Integer "3")
            ]
      ; from_desugaring = false
      ; has_union = true
      ; aliases = None
      }
  in
  visit_query ast |> print_endline;
  [%expect
    {| for number in {0, 1, 2, 3} union (select {number, number + 0.5}) |}]
