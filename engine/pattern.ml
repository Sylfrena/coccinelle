open Common open Commonop

module A = Ast_cocci
module B = Ast_c
module F = Control_flow_c

(*****************************************************************************)
type sequence_processing_style = Ordered | Unordered

(* put in semantic_c.ml ? *)
type semantic_info_ident = 
  | Function 
  | LocalFunction (* entails Function *)
  | DontKnow

let term ((s,_,_) : 'a Ast_cocci.mcode) = s

(*****************************************************************************)
(*
 * version0: 
 *   (Ast_cocci.rule_elem -> Control_flow_c.node -> bool)
 *   type ('a, 'b) matcher = 'a -> 'b -> bool
 * 
 * version1: same but with a global variable holding the current binding
 *  BUT bug
 *   - can have multiple possibilities
 *   - globals sux
 *   - sometimes have to undo, cos if start match, then it binds, and if later
 *      it does not match, then must undo the first binds.
 *     ex: when match parameters, can  try to match, but then we found far 
 *     later that the last argument of a function does not match
 *      => have to undo the binding !!!
 *      (can handle that too with a global, by saving the global, ... but sux)
 *   => better not use global
 * 
 * version2: 
 *  (binding -> Ast_cocci.rule_elem -> Control_flow_c.node -> binding list)
 *   type ('a, 'b) matcher = binding -> 'a -> 'b -> binding list
 *  Empty list mean failure (let matchfailure = []).
 *  To be able to have pretty code, have to use partial application powa, and 
 *  so the type is in fact
 *    type ('a, 'b) matcher =  'a -> 'b -> binding -> binding list
 *  Then by defining the correct combinators, can have quite pretty code (that 
 *   looks like the clean code of version0).
 * 
 * opti: return a lazy list of possible matchs ?
 *)

type ('a, 'b) matcher = 
    'a -> 'b -> Lib_engine.metavars_binding -> Lib_engine.metavars_binding list

(* monad like stuff
 * src: papers on parser combinators in haskell (cf a pearl by meijer in ICFP)
 *)

let (>&&>) m1 m2 = fun binding ->
  let xs = m1 binding in
  let xxs = xs +> List.map (fun binding -> m2 binding) in
  List.flatten xxs

let (>||>) m1 m2 = fun binding ->
  m1 binding ++  m2 binding

(* An exclusive or (xor). *)
let (>|+|>) m1 m2 = fun binding -> 
  let xs = m1 binding in
  if null xs
  then m2 binding
  else xs

let return res = fun binding -> 
  match res with
  | false -> []
  | true -> [binding]

(*****************************************************************************)

(* old: semiglobal for metavar binding (logic vars) *)
(* old:
let _sg_metavars_binding = ref empty_metavar_binding

let check_add_metavars_binding = function
  | MetaId (s1, s2) -> 
      let (good, binding) = check_add (!_sg_metavars_binding.metaId) s1 s2 in
      !_sg_metavars_binding.metaId <- binding;
      good
          
  | MetaFunc (s1, s2) -> 
      let (good, binding) = check_add (!_sg_metavars_binding.metaFunc) s1 s2 in
      !_sg_metavars_binding.metaFunc <- binding;
      good

let with_metaalvars_binding binding f = 
  let oldbinding = !_sg_metavars_binding in
  let _ = _sg_metavars_binding := binding  in
  let res = f _sg_metavars_binding in
  let _ = _sg_metavars_binding := oldbinding in
  res

*)

(* old:
let check_add k valu (===) anassoc = 
  (match optionise (fun () -> 
   (anassoc +> List.find (function (k', _) -> k' = k))) with
      | Some (k', valu') ->
          assert (k = k');
          (valu === valu',  anassoc)
      | None -> 
          (true, anassoc +> insert_assoc (k, valu))
  )

*)

let _MatchFailure = []
let _GoodMatch binding = [binding]

(* pre: if have declared a new metavar that hide another one, then must be 
   passed with a binding that deleted this metavar *)
let check_add_metavars_binding = fun (k, valu) binding -> 
  (match optionise (fun () -> binding +> List.assoc k) with
  | Some (valu') ->
      if
        (match valu, valu' with
        | Ast_c.MetaIdVal a, Ast_c.MetaIdVal b -> a =$= b
        | Ast_c.MetaFuncVal a, Ast_c.MetaFuncVal b -> a =$= b
        | Ast_c.MetaLocalFuncVal a, Ast_c.MetaLocalFuncVal b -> 
            (* do something more ? *)
            a =$= b

          (* al_expr  before comparing !!! and accept when they match.
             Note that here we have Astc._expression, so it is a match modulo 
             isomorphism (there is no metavariable involved here, just 
             isomorphisms).
             => TODO call isomorphism_c_c instead of =*= *)
        | Ast_c.MetaExprVal a, Ast_c.MetaExprVal b -> 
            Abstract_line_c.al_expr a =*= Abstract_line_c.al_expr b
        | Ast_c.MetaStmtVal a, Ast_c.MetaStmtVal b -> 
            Abstract_line_c.al_statement a =*= Abstract_line_c.al_statement b
        | Ast_c.MetaTypeVal a, Ast_c.MetaTypeVal b -> 
            Abstract_line_c.al_type a =*= Abstract_line_c.al_type b

        | Ast_c.MetaExprListVal a, Ast_c.MetaExprListVal b -> 
            failwith "not handling MetaExprListVal"
        | Ast_c.MetaParamVal a, Ast_c.MetaParamVal b -> 
            failwith "not handling MetaParamVal"
        | Ast_c.MetaParamListVal a, Ast_c.MetaParamListVal b -> 
            failwith "not handling MetaParamListVal"
        | _ -> raise Impossible
        ) 
      then _GoodMatch binding
      else _MatchFailure

  | None -> 
     let valu' = 
      (match valu with
      | Ast_c.MetaIdVal a        -> Ast_c.MetaIdVal a
      | Ast_c.MetaFuncVal a      -> Ast_c.MetaFuncVal a
      | Ast_c.MetaLocalFuncVal a -> Ast_c.MetaLocalFuncVal a (* more ? *)
      | Ast_c.MetaExprVal a -> Ast_c.MetaExprVal (Abstract_line_c.al_expr a)
      | Ast_c.MetaStmtVal a -> Ast_c.MetaStmtVal (Abstract_line_c.al_statement a)
      | Ast_c.MetaTypeVal a -> Ast_c.MetaTypeVal (Abstract_line_c.al_type a)
      | Ast_c.MetaExprListVal a ->  failwith "not handling MetaExprListVal"
      | Ast_c.MetaParamVal a ->     failwith "not handling MetaParamVal"
      | Ast_c.MetaParamListVal a -> failwith "not handling MetaParamListVal"

      ) 
     in
     _GoodMatch   (binding +> insert_assoc (k, valu'))
  )
  
(*****************************************************************************)
let rec (match_re_node: (Ast_cocci.rule_elem, Control_flow_c.node) matcher) = 
 fun re node -> 
  match A.unwrap re, F.unwrap node with

  (* note: the order of the clauses is important. *)

  | _, F.Enter | _, F.Exit | _, F.ErrorExit -> return false

  (* the metaRuleElem contains just '-' information. We dont need to add
   * stuff in the environment. If we need stuff in environment, because
   * there is a + S somewhere, then this will be done via MetaStatement, not
   * via MetaRuleElem. 
   * Can match TrueNode/FalseNode/... so must be placed before those cases.
   *)
  | A.MetaRuleElem _, _ -> return true

  | _, F.Fake  | _, F.CaseNode _ 
  | _, F.TrueNode | _, F.FalseNode | _, F.AfterNode | _, F.FallThroughNode 
    -> return false

  (* cas general: a Meta can match everything *)
  | A.MetaStmt (ida),  _unwrap_node -> 
     (* match only "header"-statement *)
     (match Control_flow_c.extract_fullstatement node with
     | Some stb -> check_add_metavars_binding (term ida, Ast_c.MetaStmtVal stb)
     | None -> return false
     )

  (* not me?: *)
  | A.MetaStmtList _, _ -> failwith "not handling MetaStmtList"

  | A.Exp expr, nodeb -> 
     (* Now keep fullstatement inside the control flow node, 
      * so that can then get in a MetaStmtVar the fullstatement to later
      * pp back when the S is in a +. But that means that 
      * Exp will match an Ifnode even if there is no such exp
      * inside the condition of the Ifnode (because the exp may
      * be deeper, in the then branch). So have to not visit
      * all inside a node anymore.
      * 
      * update: j'ai choisi d'accrocher au noeud du CFG � la
      * fois le fullstatement et le partialstatement et appeler le 
      * visiteur que sur le partialstatement.
      *)

      (* julia's code *)
      (function binding ->
        let globals = ref [] in
        let bigf = { Visitor_c.default_visitor_c with Visitor_c.kexpr = 
            (fun (k, bigf) e ->
	      match match_e_e expr e binding with
		[] -> (* failed *) k e
	      |	b -> globals := b @ !globals);
              }
        in

      (* let all_exprs =  
          ...
        let bigf = { Visitor_c.default_visitor_c with Visitor_c.kexpr = 
              (fun (k, bigf) expr -> push2 expr globals;  k expr );
           } 
        in
       *)
        let visitor_e = Visitor_c.visitor_expr_k bigf in

        (match nodeb with 

        | F.Decl decl -> Visitor_c.visitor_decl_k bigf decl 
        | F.ExprStatement (_, (eopt, _)) ->  eopt +> do_option visitor_e

        | F.IfHeader (_, (e,_)) 
        | F.SwitchHeader (_, (e,_))
        | F.WhileHeader (_, (e,_))
        | F.DoWhileTail (e,_) 
          -> visitor_e e

        | F.ForHeader (_, (((e1opt,i1), (e2opt,i2), (e3opt,i3)), _)) -> 
            e1opt +> do_option visitor_e;
            e2opt +> do_option visitor_e;
            e3opt +> do_option visitor_e;
            
        | F.ReturnExpr (_, (e,_)) -> visitor_e e

        | F.Case  (_, (e,_)) -> visitor_e e
        | F.CaseRange (_, ((e1, e2),_)) -> visitor_e e1; visitor_e e2

        | _ -> ()
        );
        !globals
      )
     (*
      in
      all_exprs +> List.fold_left (fun acc e -> acc >||> match_e_e expr e) 
        (return false)
       *)
  



  | A.FunHeader (stoa, ida, _, paramsa, _), 
    F.FunHeader ((idb, (retb, (paramsb, (isvaargs,_))), stob), _) -> 


      match_ident LocalFunction ida idb >&&>
      
      (* todo: stoa vs stob 
       * todo: isvaargs ? retb ?
       * "iso-by-absence" for storage, and return type.
       *)
      (
       (* for the pattern phase, no need the EComma *)
       let paramsa' = A.undots paramsa +> List.filter(function x -> 
         match A.unwrap x with A.PComma _ -> false | _ -> true)
       in
       match_params
        (match A.unwrap paramsa with 
        | A.DOTS _ -> Ordered 
        | A.CIRCLES _ -> Unordered 
        | A.STARS _ -> failwith "not yet handling stars (interprocedural stuff)"
        )
         paramsa' paramsb
      )

  | A.Decl decla, F.Decl declb -> match_re_decl decla declb

  | A.SeqStart _, F.SeqStart _ -> return true
  | A.SeqEnd _,   F.SeqEnd   _ -> return true

  | A.ExprStatement (ea, _), F.ExprStatement (_, (Some eb, ii)) -> 
      match_e_e ea eb

  | A.IfHeader (_,_, ea, _), F.IfHeader (_, (eb,ii)) -> match_e_e ea eb
  | A.Else _,                F.Else _                -> return true
  | A.WhileHeader (_, _, ea, _), F.WhileHeader (_, (eb,ii)) -> match_e_e ea eb
  | A.DoHeader _,             F.DoHeader _          -> return true
  | A.WhileTail (_,_,ea,_,_), F.DoWhileTail (eb,ii) -> match_e_e ea eb

  | A.ForHeader (_, _, ea1opt, _, ea2opt, _, ea3opt, _), 
    F.ForHeader (_, (((eb1opt,_), (eb2opt,_), (eb3opt,_)), ii)) -> 
      match_opt match_e_e ea1opt eb1opt >&&>
      match_opt match_e_e ea2opt eb2opt >&&>
      match_opt match_e_e ea3opt eb3opt >&&>
      return true
      

  | A.Return _,              F.Return (_, ((),ii))     -> return true
  | A.ReturnExpr (_, ea, _), F.ReturnExpr (_, (eb,ii)) -> match_e_e ea eb

  | _, F.ExprStatement (_, (None, ii)) -> return false (* happen ? *)

  (* have not a counter part in coccinelle, for the moment *)
  (* todo?: print a warning at least ? *)
  | _, F.SwitchHeader _ 
  | _, F.Label _
  | _, F.Case _  | _, F.CaseRange _  | _, F.Default _
  | _, F.Goto _ | _, F.Break _ | _, F.Continue _ 
  | _, F.Asm
    -> return false

  | _, _ -> return false


(*-------------------------------------------------------------------------- *)

and (match_re_decl: (Ast_cocci.declaration, Ast_c.declaration) matcher) = 
 fun decla (B.DeclList (xs, _)) -> 
   xs +> List.fold_left (fun acc var -> acc >||> match_re_onedecl decla var)
     (return false)

and match_re_onedecl = fun decla declb -> 
  match A.unwrap decla, declb with
    (* could handle iso here but handled in standard.iso *)
    (* todo, use sto? lack of sto in Ast_cocci *)
  | A.UnInit (typa, sa, _), ((Some ((sb, None),_), typb, sto), _) ->
      match_ft_ft typa typb >&&>
      match_ident DontKnow sa sb
  | A.Init (typa, sa, _, expa, _), ((Some ((sb, Some ini),_), typb, sto), _) ->
      match_ft_ft typa typb >&&>
      match_ident DontKnow sa sb >&&>
      (match ini with
      | B.InitExpr expb, _ -> match_e_e expa expb
      | _ -> 
          pr2 "warning: complex initializer, cocci does not handle that";
          return false
      )
  | _, ((None, typb, sto), _) -> 
      failwith "no variable in this declaration, wierd"
      
  | A.DisjDecl xs, _ -> 
      xs +> List.fold_left (fun acc decla -> 
        acc >|+|> match_re_onedecl decla declb
        ) (return false)
  | A.OptDecl _, _ | A.UniqueDecl _, _ | A.MultiDecl _, _ -> 
      failwith "not handling Opt/Unique/Multi Decl"
  | _, _ -> return false



(* ------------------------------------------------------------------------- *)

and (match_e_e: (Ast_cocci.expression,Ast_c.expression) matcher) = fun ep ec ->
  match A.unwrap ep, ec with
  
  (* cas general: a MetaExpr can match everything *)
  | A.MetaExpr (ida, opttypa),  (((expr, opttypb), ii) as expb) -> 
      (match opttypa, opttypb with
      | None, _ -> return true
      | Some (tas : Type_cocci.typeC list), Some tb -> 
	  failwith "type matching not supported"
	  (*
          tas +> List.fold_left (fun acc ta -> acc >||>  match_ft_ft ta tb) 
            (return false)
	     *)
      | Some _, None -> 
          failwith ("I have not the type information. Certainly a pb in " ^
                    "annotate_typer.ml")
      ) >&&>
      check_add_metavars_binding (term ida, Ast_c.MetaExprVal (expb))


  (* old: | A.Edots _, _ -> raise Impossible
     In fact now can also have the Edots inside normal expression, 
     not just in arg lists.
     in 'x[...];'  
     less: in if(<... x ... y ...>) *)
  | A.Edots (_, None), _    -> return true
  | A.Edots (_, Some expr), _    -> failwith "not handling when on Edots"


  | A.MetaConst _, _ -> failwith "not handling MetaConst"
  | A.MetaErr _, _ -> failwith "not handling MetaErr"

  | A.Ident ida,                (((B.Ident idb) , typ), ii) ->
      match_ident DontKnow ida idb

 (* todo: handle some isomorphisms in int/float ? can have different format :
  *   1l can match a 1.
  * TODO: normally string can contain some metavar too, so should recurse on 
  *  the string 
  *)
  | A.Constant (A.String sa,_,_),  ((B.Constant (B.String (sb, _)), typ),ii)  
    when sa =$= sb -> return true
  | A.Constant (A.Char sa,_,_),    ((B.Constant (B.Char   (sb, _)), typ),ii)
    when sa =$= sb -> return true
  | A.Constant (A.Int sa,_,_),     ((B.Constant (B.Int    (sb)), typ),ii)
    when sa =$= sb -> return true
  | A.Constant (A.Float sa,_,_),   ((B.Constant (B.Float  (sb, ftyp)), typ),ii)
    when sa =$= sb -> return true

  | A.FunCall (ea1, _, eas, _), ((B.FunCall (eb1, ebs), typ),ii) -> 
     (* todo: do special case to allow IdMetaFunc, cos doing the recursive call
        will be too late, match_ident will not have the info whether it  was a 
        function.
        todo: but how detect when do x.field = f;  how know that f is a Func ?
        by having computed some information before the matching *)

      match_e_e ea1 eb1  >&&> (

      (* for the pattern phase, no need the EComma *)
      let eas' =
	A.undots eas +>
	List.filter (function x -> 
          match A.unwrap x with A.EComma _ -> false | _ -> true) 
      in
      let ebs' = ebs +> List.map fst +> List.map (function
        | Left e -> e
        | Right typ -> failwith "not handling type in funcall"
        ) in
      match_arguments 
        (match A.unwrap eas with 
        | A.DOTS _ -> Ordered 
        | A.CIRCLES _ -> Unordered 
        | A.STARS _ -> failwith "not handling stars"
        )
        eas' ebs'
     )

  | A.Assignment (ea1, opa, ea2),   ((B.Assignment (eb1, opb, eb2), typ),ii) ->
      return (equal_assignOp (term opa)  opb) >&&>
      (match_e_e ea1 eb1 >&&>  match_e_e ea2 eb2) 


  | A.CondExpr (ea1,_,ea2opt,_,ea3), ((B.CondExpr (eb1, eb2opt, eb3), typ),ii) 
    ->
      match_e_e ea1 eb1 >&&>
      match_opt match_e_e ea2opt eb2opt >&&>
      match_e_e ea3 eb3
   
  (* todo?: handle some isomorphisms here ? *)

  | A.Postfix (ea, opa), ((B.Postfix (eb, opb), typ),ii) -> 
      return (equal_fixOp (term opa) opb) >&&>
      match_e_e ea eb

  | A.Infix (ea, opa), ((B.Infix (eb, opb), typ),ii) -> 
      return (equal_fixOp (term opa) opb) >&&>
      match_e_e ea eb

  | A.Unary (ea, opa), ((B.Unary (eb, opb), typ),ii) -> 
      return (equal_unaryOp (term opa) opb) >&&>
      match_e_e ea eb

  | A.Binary (ea1, opa, ea2), ((B.Binary (eb1, opb, eb2), typ),ii) -> 
      return (equal_binaryOp (term opa) opb) >&&>
      match_e_e ea1 eb1 >&&> 
      match_e_e ea2 eb2

        
  (* todo?: handle some isomorphisms here ?  (with pointers = Unary Deref) *)

  | A.ArrayAccess (ea1, _, ea2, _), ((B.ArrayAccess (eb1, eb2), typ),ii) -> 
      match_e_e ea1 eb1 >&&>
      match_e_e ea2 eb2


  (* todo?: handle some isomorphisms here ? *)

  | A.RecordAccess (ea, _, ida), ((B.RecordAccess (eb, idb), typ),ii) ->
      match_e_e ea eb >&&>
      match_ident DontKnow ida idb

  | A.RecordPtAccess (ea, _, ida), ((B.RecordPtAccess (eb, idb), typ),ii) ->
      match_e_e ea eb >&&>
      match_ident DontKnow ida idb

  (* todo?: handle some isomorphisms here ? *)

  | A.Cast (_, typa, _, ea), ((B.Cast (typb, eb), typ),ii) ->
      match_ft_ft typa typb >&&>
      match_e_e ea eb

  | A.SizeOfExpr (_, ea), ((B.SizeOfExpr (eb), typ),ii) ->
      match_e_e ea eb

  | A.SizeOfType (_, _, typa, _), ((B.SizeOfType (typb), typ),ii) ->
      match_ft_ft typa typb

  (* todo? iso ? allow all the combinations ? *)
  | A.Paren (_, ea, _), ((B.ParenExpr (eb), typ),ii) -> 
      match_e_e ea eb

  | A.NestExpr _, _ -> failwith "not my job to handle NestExpr"


  | A.MetaExprList _, _   -> raise Impossible (* only in arg lists *)

  | A.EComma _, _   -> raise Impossible (* can have EComma only in arg lists *)

  | A.Ecircles _, _ -> raise Impossible (* can have EComma only in arg lists *)
  | A.Estars _, _   -> raise Impossible (* can have EComma only in arg lists *)


  | A.DisjExpr eas, eb -> 
      eas +> List.fold_left (fun acc ea -> acc >|+|>  match_e_e ea eb) 
        (return false)


  | A.MultiExp _, _ | A.UniqueExp _,_ | A.OptExp _,_ -> 
      failwith "not handling Opt/Unique/Multi on expr"


  (* have not a counter part in coccinelle, for the moment *)
  | _, ((B.Sequence _,_),_) 

  | _, ((B.StatementExpr _,_),_) 
  | _, ((B.Constructor,_),_) 
  | _, ((B.MacroCall _,_),_) 
  | _, ((B.MacroCall2 _,_),_)
    -> return false

  | _, _ -> return false

  
(*-------------------------------------------------------------------------- *)

and (match_arguments: 
       sequence_processing_style -> 
         (Ast_cocci.expression list, Ast_c.expression list) matcher) = 
 fun seqstyle eas ebs ->
 (* old:
    if List.length eas = List.length ebs
    then
      (zip eas ebs +> List.fold_left (fun acc (ea, eb) -> 
           acc >&&> match_e_e ea eb) (return true))
    else return false
 *)
  match seqstyle with
  | Ordered -> 
      (match eas, ebs with
      | [], [] -> return true
      | [], y::ys -> return false
      | x::xs, ys -> 
          (match A.unwrap x, ys with
          | A.Edots (_, optexpr), ys -> 
              (* todo: if optexpr, then a WHEN and so may have to filter yys *)
              (* '...' can take more or less the beginnings of the arguments *)
              let yys = Common.tails ys in 
              yys +> List.fold_left (fun acc ys -> 
                acc >||>  match_arguments seqstyle xs ys
                  ) (return false)

          | A.Ecircles (_,_), ys -> raise Impossible (* in Ordered mode *)
          | A.Estars (_,_), ys   -> raise Impossible (* in Ordered mode *)

           (* filtered by the caller, in the case for FunCall *)
          | A.EComma (_), ys -> raise Impossible 

          | A.MetaExprList ida, ys -> 
              let startendxs = (Common.zip (Common.inits ys) (Common.tails ys))
              in
              startendxs +> List.fold_left (fun acc (startxs, endxs) -> 
                acc >||> (
                check_add_metavars_binding 
                  (term ida, Ast_c.MetaExprListVal (startxs)) >&&>
                match_arguments seqstyle xs endxs
             )) (return false)

          | A.MultiExp _, _ | A.UniqueExp _,_ | A.OptExp _,_ -> 
              failwith "not handling Opt/Unique/Multi on expr"
              

          | _, y::ys -> 
              match_e_e x y >&&> 
              match_arguments seqstyle xs ys
          | x, [] -> return false
          )
      )
  | Unordered -> failwith "not handling ooo"

(* ------------------------------------------------------------------------- *)

and (match_ft_ft: (Ast_cocci.fullType, Ast_c.fullType) matcher) =
  fun typa typb ->
    match (A.unwrap typa,typb) with
      (A.Type(cv,ty1),((qu,il),ty2)) ->
	(* Drop out the const/volatile part that has been matched.
         * This is because a SP can contain  const T v; in which case
         * later in match_t_t when we encounter a T, we must not add in
         * the environment the whole type.
         *)
	let new_il todrop =
	  List.filter (function (pi,_) -> not(pi.Common.str = todrop)) in

        if qu.B.const && qu.B.volatile 
        then pr2 "warning: the type is both const & volatile but cocci does not handle that";

	(match cv with
          (* "iso-by-absence" *)
	  None -> match_t_t ty1 typb
	| Some(A.Const,_,_) ->
	    if qu.B.const
	    then
	      match_t_t ty1
		(({qu with B.const = false},new_il "const" il),ty2)
	    else return false
	| Some(A.Volatile,_,_) ->
	    if qu.B.volatile
	    then
	      match_t_t ty1
		(({qu with B.volatile = false},new_il "volatile" il),ty2)
	    else return false)
    | (A.OptType(ty),typb) ->
	pr2 "warning: ignoring ? arity on type";
	match_ft_ft ty typb
    | (A.UniqueType(ty),typb) ->
	pr2 "warning: ignoring ! arity on type";
	match_ft_ft ty typb
    | (A.MultiType(ty),typb) ->
	pr2 "warning: ignoring + arity on type";
	match_ft_ft ty typb

and (match_t_t: (Ast_cocci.typeC, Ast_c.fullType) matcher) =
  fun typa typb -> 
    match A.unwrap typa, typb with

      (* cas general *)
      A.MetaType ida,  typb -> 
	check_add_metavars_binding (term ida, B.MetaTypeVal typb)

    | A.BaseType (basea, signaopt),   (qu, (B.BaseType baseb, iib)) -> 
	let match_sign signa signb = 
          (match signa, signb with
            (* todo: iso on sign, if not mentioned then free.  tochange? 
             * but that require to know if signed int because explicit
             * signed int,  or because implicit signed int.
             *)
          | None, _ -> return true
          | Some a, b -> return (equal_sign (term a) b)) in
	
	
      (* handle some iso on type ? (cf complex C rule for possible implicit
	 casting) *)
	(match term basea, baseb with
	| A.VoidType,  B.Void -> assert (signaopt = None); return true
	| A.CharType,  B.IntType B.CChar when signaopt = None -> 
            return true


          (* todo?: also match signed CChar2 ? *)

	| A.ShortType, B.IntType (B.Si (signb, B.CShort)) ->
	    match_sign signaopt signb
	| A.IntType,   B.IntType (B.Si (signb, B.CInt))   ->
	    match_sign signaopt signb
	| A.LongType,  B.IntType (B.Si (signb, B.CLong))  ->
	    match_sign signaopt signb

	| A.FloatType, B.FloatType (B.CFloat) -> 
            assert (signaopt = None); (* no sign on float in C *)
            return true
	| A.DoubleType, B.FloatType (B.CDouble) -> 
            assert (signaopt = None); (* no sign on float in C *)
            return true
	| x, y -> return false)
	  
  (* todo? iso with array *)
    | A.Pointer (typa, _),            (qu, (B.Pointer typb, _)) -> 
	match_ft_ft typa typb
	  
    | A.Array (typa, _, eaopt, _), (qu, (B.Array (ebopt, typb), _)) -> 
	match_ft_ft typa typb >&&>
        match_opt match_e_e  eaopt ebopt
       (* todo: handle the iso on optionnal size specifification ? *)
	  
    | A.StructUnionName(sa, sua),
	(qu, (B.StructUnionName (sb, sub), _)) -> 
     (* todo: could also match a Struct that has provided a name *)
	return (equal_structUnion (term sua) sub && (term sa) =$= sb)

   (* todo? handle isomorphisms ? because Unsigned Int can be match on a 
      uint in the C code. But some CEs consists in renaming some types,
      so we don't want apply isomorphisms every time. *) 
    | A.TypeName sa,  (qu, (B.TypeName sb, _)) ->
	return ((term sa) =$= sb)
    | (_,_) -> return false (* incompatible constructors *)

(*-------------------------------------------------------------------------- *)

and (match_params: 
       sequence_processing_style -> 
         (Ast_cocci.parameterTypeDef list, 
          ((Ast_c.parameterType * Ast_c.il) list)) 
           matcher) = 
 fun seqstyle pas pbs ->
 (* todo: if contain metavar ? => recurse on two list and consomme *)
 (* old:
  let pas' = pas +> List.filter (function A.Param (x,y,z) -> true | _ -> false)
   in
  if (List.length pas' = List.length pbs) 
  then
  (zip pas' pbs +> List.fold_left (fun acc param -> 
   match param with
    | A.Param (ida, qua, typa), ((hasreg, idb, typb, _), ii) -> 
        acc >&&>
        match_ft_ft typa typb >&&>
        match_ident ida idb  
    | x -> error_cant_have x
    ) (return true)
  )
  else return false
  *)

  match seqstyle with
  | Ordered -> 
      (match pas, pbs with
      | [], [] -> return true
      | [], y::ys -> return false
      | x::xs, ys -> 
          (match A.unwrap x, ys with
          | A.Pdots (_), ys -> 

              (* '...' can take more or less the beginnings of the arguments *)
              let yys = Common.tails ys in 
              yys +> List.fold_left (fun acc ys -> 
                acc >||>  match_params seqstyle xs ys
                  ) (return false)


          | A.MetaParamList ida, ys -> 
              let startendxs = (Common.zip (Common.inits ys) (Common.tails ys))
              in
              startendxs +> List.fold_left (fun acc (startxs, endxs) -> 
                acc >||> (
                check_add_metavars_binding
		  (term ida, Ast_c.MetaParamListVal (startxs)) >&&>
                match_params seqstyle xs endxs
             )) (return false)


          | A.Pcircles (_), ys -> raise Impossible (* in Ordered mode *)

          (* filtered by the caller, in the case for FunDecl *)
          | A.PComma (_), ys -> raise Impossible 

          | A.MetaParam (ida), y::ys -> 
             (* todo: use quaopt, hasreg ? *)
             check_add_metavars_binding (term ida, Ast_c.MetaParamVal (y)) >&&>
             match_params seqstyle xs ys

          | A.Param (ida, typa), (((hasreg, idb, typb), _), _)::ys -> 
              (match idb with
              | Some idb -> 
                  (* todo: use quaopt, hasreg ? *)
                  (match_ft_ft typa typb >&&>
                   match_ident DontKnow ida idb
                  ) >&&> 
                  match_params seqstyle xs ys
              | None -> 
                  assert (null ys);
                  assert (
                    match typb with 
                    | (_qua, (B.BaseType B.Void,_)) -> true
                    | _ -> false
                          );
   
                  return false
              )

          | x, [] -> return false

          | A.VoidParam _, _ -> failwith "handling VoidParam"
          | (A.OptParam _ | A.UniqueParam _), _ -> 
              failwith "handling Opt/Unique/Multi for Param"
                                
          )
      )

  | Unordered -> failwith "handling ooo"


(* ------------------------------------------------------------------------- *)

and (match_ident: semantic_info_ident -> (Ast_cocci.ident, string) matcher) = 
fun seminfo_idb ida idb -> 
 match A.unwrap ida with
 | A.Id ida -> return ((term ida) =$= idb)
 | A.MetaId ida -> check_add_metavars_binding (term ida, Ast_c.MetaIdVal (idb))

 | A.MetaFunc ida -> 
     (match seminfo_idb with
     | LocalFunction | Function -> 
	 check_add_metavars_binding (term ida, (Ast_c.MetaFuncVal idb))
     | DontKnow -> 
         failwith "MetaFunc and MetaLocalFunc, need semantic info about id"
     )

 | A.MetaLocalFunc ida -> 
     (match seminfo_idb with
     | LocalFunction -> 
	  check_add_metavars_binding (term ida, (Ast_c.MetaLocalFuncVal idb))
     | Function -> return false
     | DontKnow -> 
         failwith "MetaFunc and MetaLocalFunc, need semantic info about id"
     )

 | A.OptIdent _ | A.UniqueIdent _ | A.MultiIdent _ -> 
     failwith "not handling Opt/Unique/Multi for ident"

(* ------------------------------------------------------------------------- *)
and match_opt f eaopt ebopt =
  match eaopt, ebopt with
  | None, None -> return true
  | Some ea, Some eb -> f ea eb
  | _, _ -> return false


(*****************************************************************************)
(* Normally Ast_cocci  should reuse some types of Ast_c, so those functions
 * should not exist.
 * update: but now Ast_c depends on Ast_cocci, so can't make too Ast_cocci
 * depends on Ast_c, so have to stay with those equal_xxx functions. *)
(*****************************************************************************)

and equal_unaryOp a b = 
  match a, b with
  | A.GetRef   , B.GetRef  -> true
  | A.DeRef    , B.DeRef   -> true
  | A.UnPlus   , B.UnPlus  -> true
  | A.UnMinus  , B.UnMinus -> true
  | A.Tilde    , B.Tilde   -> true
  | A.Not      , B.Not     -> true
  | _, _ -> false


and equal_assignOp a b = 
  match a, b with
  | A.SimpleAssign, B.SimpleAssign -> true
  | A.OpAssign a,   B.OpAssign b -> equal_arithOp a b
  | _ -> false


and equal_fixOp a b = 
  match a, b with
  | A.Dec, B.Dec -> true
  | A.Inc, B.Inc -> true
  | _ -> false

and equal_binaryOp a b = 
  match a, b with
  | A.Arith a,    B.Arith b ->   equal_arithOp a b
  | A.Logical a,  B.Logical b -> equal_logicalOp a b
  | _ -> false

and equal_arithOp a b = 
  match a, b with
  | A.Plus     , B.Plus     -> true
  | A.Minus    , B.Minus    -> true
  | A.Mul      , B.Mul      -> true
  | A.Div      , B.Div      -> true
  | A.Mod      , B.Mod      -> true
  | A.DecLeft  , B.DecLeft  -> true
  | A.DecRight , B.DecRight -> true
  | A.And      , B.And      -> true
  | A.Or       , B.Or       -> true
  | A.Xor      , B.Xor      -> true
  | _          , _          -> false

and equal_logicalOp a b = 
  match a, b with
  | A.Inf    , B.Inf    -> true
  | A.Sup    , B.Sup    -> true
  | A.InfEq  , B.InfEq  -> true
  | A.SupEq  , B.SupEq  -> true
  | A.Eq     , B.Eq     -> true
  | A.NotEq  , B.NotEq  -> true
  | A.AndLog , B.AndLog -> true
  | A.OrLog  , B.OrLog  -> true
  | _          , _          -> false
  


and equal_structUnion a b = 
  match a, b with
  | A.Struct, B.Struct -> true
  | A.Union,  B.Union -> true
  | _, _ -> false


and equal_sign a b = 
  match a, b with
  | A.Signed,    B.Signed   -> true
  | A.Unsigned,  B.UnSigned -> true
  | _, _ -> false
