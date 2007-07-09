open Common open Commonop

open Ast_c
open Control_flow_c

open Ograph_extended
open Oassoc
open Oassocb


(*****************************************************************************)
(* todo?: compute target level with goto (but rare that different I think)
 * ver1: just do init, ver2: compute depth of label (easy, intercept
 * compound in the visitor)
 * 
 * todo: to generate less exception with the breakInsideLoop, analyse
 * correctly the loop deguisé comme list_for_each. Add a case ForMacro
 * in ast_c (and in lexer/parser), and then do code that imitates the
 * code for the For. 
 * update: the list_for_each was previously converted
 * into Tif by the lexer, now they are returned as Twhile so less pbs.
 * But not perfect solution.
 * 
 * checktodo: after a switch, need check that all the st in the
 * compound start with a case: ?
 * 
 * checktodo: how ensure that when we call aux_statement recursivly, we
 * pass it auxinfo_label and not just auxinfo ? how enforce that ?
 * 
 * todo: can have code (and so nodes) in many places, in the size of an
 * array, in the init of initializer, but also in StatementExpr, ...
 * 
 * todo?: steal code from CIL ? (but seems complicated ... again) *)
(*****************************************************************************)

type error = 
  | DeadCode          of Common.parse_info option
  | CaseNoSwitch      of Common.parse_info
  | OnlyBreakInSwitch of Common.parse_info
  | NoEnclosingLoop   of Common.parse_info
  | GotoCantFindLabel of string * Common.parse_info
  | NoExit of Common.parse_info
  | DuplicatedLabel of string
  | NestedFunc
  | ComputedGoto

exception Error of error

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let mk_node node labels nodestr =
  let nodestr = 
    if !Flag_parsing_c.show_flow_labels
    then nodestr ^ ("[" ^ (labels +> List.map i_to_s +> join ",") ^ "]")
    else nodestr
  in
  ((node, labels), nodestr)

let add_node node labels nodestr g = 
   g#add_node (mk_node node labels nodestr)

let add_arc_opt (starti, nodei) g = 
  starti +> do_option (fun starti -> g#add_arc ((starti, nodei), Direct))


let lbl_empty = [] 

let pinfo_of_ii ii = (List.hd ii).Ast_c.pinfo



(*****************************************************************************)
(* Contextual information passed in aux_statement *)
(*****************************************************************************)

(* Information used internally in ast_to_flow and passed recursively. *) 
type additionnal_info =  { 

  ctx: context_info; (* cf below *)
  ctx_stack: context_info list;

  (* are we under a ifthen[noelse]. Used for ErrorExit *)
  under_ifthen: bool; 

  (* does not change recursively *)
  labels_assoc: (string, nodei) oassoc; 
  exiti: nodei option;
  errorexiti: nodei option;

  (* ctl_braces: the nodei list is to handle current imbrication depth.
   * It contains the must-close '}'. 
   * update: now it is instead a node list. 
   *)
  braces: node list;

  (* ctl: *)
  labels: int list; 
  }

 (* Sometimes have a continue/break and we must know where we must jump.
  *    
  * ctl_brace: The node list in context_info record the number of '}' at the 
  * context point, for instance at the switch point. So that when deeper,
  * we can compute the difference between the number of '}' from root to
  * the context point to close the good number of '}' . For instance 
  * where there is a 'continue', we must close only until the for.
  *)
  and context_info =
      | NoInfo 
      | LoopInfo   of nodei * nodei (* start, end *) * node list    
      | SwitchInfo of nodei * nodei (* start, end *) * node list


let initial_info = {
  ctx = NoInfo; 
  ctx_stack = [];
  under_ifthen = false;
  braces = [];
  labels = []; 

  labels_assoc = new oassocb [];
  exiti = None;
  errorexiti = None;
} 


(*****************************************************************************)
(* (Semi) Globals, Julia's style. *)
(*****************************************************************************)
(* global graph *)
let g = ref (new ograph_mutable) 

(* For switch, use compteur (or pass int ref) too cos need know order of the
 *  case if then later want to  go from CFG to (original) AST. 
 *)
let counter_for_labels = ref 0
let counter_for_braces = ref 0
let counter_for_switch = ref 0


(*****************************************************************************)
(* helpers *)
(*****************************************************************************)

(* alt: do via a todo list, so can do all in one pass (but more complex) 
 * todo: can also count the depth level and associate it to the node, for 
 * the ctl_braces: 
 *)
let compute_labels_and_create_them statement = 

  (* map C label to index number in graph *)
  let (h: (string, nodei) oassoc ref) = ref (new oassocb []) in

  begin
    statement +> Visitor_c.vk_statement { Visitor_c.default_visitor_c with 
      Visitor_c.kstatement = (fun (k, bigf) statement -> 
        match statement with
        | Labeled (Ast_c.Label (s, st)),ii -> 
            (* at this point I put a lbl_empty, but later
             * I will put the good labels. 
             *)
            let newi = !g +> add_node (Label (statement,(s,ii))) lbl_empty  (s^":")
            in
            begin
              (* the C label already exist ? *)
              if (!h#haskey s) then raise (Error (DuplicatedLabel s));
              h := !h#add (s, newi);
              (* not k st !!! otherwise in lbl1: lbl2: i++; we miss lbl2 *)
              k statement; 
            end
        | st -> k st
      )
    };
    !h;
  end


(* ctl_braces: *)
let insert_all_braces xs starti = 
  xs  +> List.fold_left (fun acc e -> 
    (* Have to build a new node (clone), cos cant share it. 
     * update: This is now done by the caller. The clones are in xs.
     *)
    let node = e in
    let newi = !g#add_node node in
    !g#add_arc ((acc, newi), Direct);
    newi
  ) starti

(*****************************************************************************)
(* Statement *)
(*****************************************************************************)

(* Take in a (optional) start node, return an (optional) end node.
 * 
 * old: old code was returning an nodei, but goto has no end, so
 * aux_statement should return nodei option.
 * 
 * old: old code was taking a nodei, but should also take nodei
 * option.
 * 
 * note: deadCode detection. What is dead code ? When there is no
 * starti to start from ? So make starti an option too ? Si on arrive
 * sur un label: au moment d'un deadCode, on peut verifier les
 * predecesseurs de ce label, auquel cas si y'en a, ca veut dire
 * qu'en fait c'est pas du deadCode et que donc on peut se permettre
 * de partir d'un starti à None. Mais si on a xx; goto far:; near:
 * yy; zz; far: goto near:. Bon ca doit etre un cas tres tres rare,
 * mais a cause de notre parcours, on va rejeter ce programme car au
 * moment d'arriver sur near: on n'a pas encore de predecesseurs pour
 * ce label. De meme, meme le cas simple ou la derniere instruction
 * c'est un return, alors ca va generer un DeadCode :(
 * 
 * So make a first pass where dont launch exn at all. Create nodes,
 * if starti is None then dont add arc. Then make a second pass that
 * just checks that all nodes (except enter) have predecessors.
 * (todo: if the pb is at a fake node, then try first successos that
 * is non fake). So make starti an option too. So type is now
 * 
 * nodei option -> statement -> nodei option.
 * 
 * Because of special needs of coccinelle, need pass more info, cf
 * type additionnal_info defined above.
 * 
 * - to complete (break, continue (and enclosing loop), switch (and
 * associated case, casedefault)) we need to pass additionnal info.
 * The start/exit when enter in a loop, to know the current 'for'.
 * 
 * - to handle the braces, need again pass additionnal info.
 * 
 * - need pass the labels.
 * 
 *)

let rec (aux_statement: 
            (nodei option * additionnal_info) -> statement -> nodei option) 
 = fun (starti, auxinfo) stmt ->

  if not !Flag_parsing_c.label_strategy_2
  then incr counter_for_labels;
    
  let lbl = 
    if not !Flag_parsing_c.label_strategy_2 
    then auxinfo.labels @ [!counter_for_labels]
    else auxinfo.labels 
  in

  (* Normally the new auxinfo to pass recursively to the next aux_statement.
   * But in some cases we add additionnal stuff. 
   *)
  let auxinfo_label = 
    if not !Flag_parsing_c.label_strategy_2
    then { auxinfo with labels = auxinfo.labels @ [ !counter_for_labels ]; } 
    else auxinfo
  in
      
  (* ------------------------- *)        
  match stmt with

  | Ast_c.Compound statxs, ii -> 
      (* flow_to_ast: *)
      let (i1, i2) = tuple_of_list2 ii in

      (* ctl_braces: *)
      incr counter_for_braces;
      let brace = !counter_for_braces in

      let o_info  = "{" ^ i_to_s brace in
      let c_info = "}" ^ i_to_s brace in
 
      let newi =    !g +> add_node (SeqStart (stmt, brace, i1)) lbl o_info in
      let endnode = mk_node    (SeqEnd (brace, i2))         lbl c_info in
      let _endnode_dup = 
        mk_node (SeqEnd (brace, Ast_c.fakeInfo())) lbl c_info 
      in

      let newauxinfo = 
        { auxinfo_label with braces = endnode(*_dup*):: auxinfo_label.braces }
      in
     

      !g +> add_arc_opt (starti, newi);
      let starti = Some newi in

      statxs +> List.fold_left (fun starti statement ->
        if !Flag_parsing_c.label_strategy_2
        then incr counter_for_labels;

        let newauxinfo' = 
          if !Flag_parsing_c.label_strategy_2
          then 
            { newauxinfo with 
              labels = auxinfo.labels @ [ !counter_for_labels ] 
            } 
          else newauxinfo
        in
        aux_statement (starti, newauxinfo') statement
      ) starti

      (* braces: *)
      +> fmap (fun starti -> 
            (* subtil: not always return a Some.
             * Note that if starti is None, alors forcement ca veut dire
             * qu'il y'a eu un return (ou goto), et donc forcement les 
             * braces auront au moins ete crée une fois, et donc flow_to_ast
             * marchera.
             * Sauf si le goto revient en arriere ? mais dans ce cas
             * ca veut dire que le programme boucle. Pour qu'il boucle pas
             * il faut forcement au moins un return.
             *)
            let endi = !g#add_node endnode in
            !g#add_arc ((starti, endi), Direct);
            endi 
           ) 


   (* ------------------------- *)        
  | Labeled (Ast_c.Label (s, st)), ii -> 
      let ilabel = auxinfo.labels_assoc#find s in
      let node = mk_node (unwrap (!g#nodes#find ilabel)) lbl (s ^ ":") in
      !g#replace_node (ilabel, node);
      !g +> add_arc_opt (starti, ilabel);
      aux_statement (Some ilabel, auxinfo_label) st


  | Jump (Ast_c.Goto s), ii -> 
     (* special_cfg_ast: *)
     let newi = !g +> add_node (Goto (stmt, (s,ii))) lbl ("goto " ^ s ^ ":") in
     !g +> add_arc_opt (starti, newi);

     let ilabel = 
       try auxinfo.labels_assoc#find s 
       with Not_found -> 
         (* jump vers ErrorExit a la place ? 
          * pourquoi tant de "cant jump" ? pas detecté par gcc ? 
          *)
         raise (Error (GotoCantFindLabel (s, pinfo_of_ii ii)))
     in
     (* !g +> add_arc_opt (starti, ilabel); 
      * todo: special_case: suppose that always goto to toplevel of function,
      * hence the Common.init 
      * todo?: can perhaps report when a goto is not a classic error_goto ? 
      * that is when it does not jump to the toplevel of the function.
      *)
     let newi = insert_all_braces (Common.list_init auxinfo.braces) newi in
     !g#add_arc ((newi, ilabel), Direct);
     None
      
  | Jump (Ast_c.GotoComputed e), ii -> 
      raise (Error (ComputedGoto))
      
   (* ------------------------- *)        
  | Ast_c.ExprStatement opte, ii -> 
      (* flow_to_ast:   old: when opte = None, then do not add in CFG. *)
      let s = 
        match opte with
        | None -> "empty;"
        | Some e -> 
            let ((unwrap_e, typ), ii) = e in
            (match unwrap_e with
            | FunCall (((Ident f, _typ), _ii), _args) -> 
                f ^ "(...)"
            | Assignment (((Ident var, _typ), _ii), SimpleAssign, e) -> 
                var ^ " = ... ;"
            | Assignment 
                (((RecordAccess (((Ident var, _typ), _ii), field), _typ2), 
                  _ii2),
                 SimpleAssign, 
                 e) -> 
                   var ^ "." ^ field ^ " = ... ;"
                   
            | _ -> "statement"
        )
      in
      let newi = !g +> add_node (ExprStatement (stmt, (opte, ii))) lbl s in
      !g +> add_arc_opt (starti, newi);
      Some newi
      

   (* ------------------------- *)        
  | Selection  (Ast_c.If (e, st1, (Ast_c.ExprStatement (None), []))), ii ->
      (* sometome can have ExprStatement None but it is a if-then-else,
       * because something like   if() xx else ;
       * so must force to have [] in the ii associated with ExprStatement 
       *)
      
      let (i1,i2,i3, iifakeend) = tuple_of_list4 ii in
      let ii = [i1;i2;i3] in
     (* starti -> newi --->   newfakethen -> ... -> finalthen --> lasti
      *                  |                                      |
      *                  |->   newfakeelse -> ... -> finalelse -|
      * update: there is now also a link directly to lasti.
      *  
      * because of CTL, now do different things if we are in a ifthen or
      * ifthenelse.
      *)
      let newi = !g +> add_node (IfHeader (stmt, (e, ii))) lbl ("if") in
      !g +> add_arc_opt (starti, newi);
      let newfakethen = !g +> add_node TrueNode        lbl "[then]" in
      let newfakeelse = !g +> add_node FallThroughNode lbl "[fallthrough]" in
      let afteri = !g +> add_node AfterNode lbl "[after]" in
      let lasti  = !g +> add_node (EndStatement (Some iifakeend)) lbl "[endif]" 
      in

      (* for ErrorExit heuristic *)
      let newauxinfo = { auxinfo_label with  under_ifthen = true; } in

      !g#add_arc ((newi, newfakethen), Direct);
      !g#add_arc ((newi, newfakeelse), Direct);
      !g#add_arc ((newi, afteri), Direct);
      !g#add_arc ((afteri, lasti), Direct);
      !g#add_arc ((newfakeelse, lasti), Direct);

      let finalthen = aux_statement (Some newfakethen, newauxinfo) st1 in
      !g +> add_arc_opt (finalthen, lasti);
      Some lasti

      
  | Selection  (Ast_c.If (e, st1, st2)), ii -> 
     (* starti -> newi --->   newfakethen -> ... -> finalthen --> lasti
      *                 |                                      |
      *                 |->   newfakeelse -> ... -> finalelse -|
      * update: there is now also a link directly to lasti.
      *)
      let (iiheader, iielse, iifakeend) = 
        match ii with
        | [i1;i2;i3;i4;i5] -> [i1;i2;i3], i4, i5
        | _ -> raise Impossible
      in
      let newi = !g +> add_node (IfHeader (stmt, (e, iiheader))) lbl "if" in
      !g +> add_arc_opt (starti, newi);
      let newfakethen = !g +> add_node TrueNode  lbl "[then]" in
      let newfakeelse = !g +> add_node FalseNode lbl "[else]" in
      let elsenode = !g +> add_node (Else iielse) lbl "else" in


      !g#add_arc ((newi, newfakethen), Direct);
      !g#add_arc ((newi, newfakeelse), Direct);

      !g#add_arc ((newfakeelse, elsenode), Direct);

      let finalthen = aux_statement (Some newfakethen, auxinfo_label) st1 in
      let finalelse = aux_statement (Some elsenode, auxinfo_label) st2 in

      (match finalthen, finalelse with 
        | (None, None) -> None
        | _ -> 
            let lasti = !g +> add_node (EndStatement(Some iifakeend)) lbl "[endif]" in
            let afteri = !g +> add_node AfterNode lbl "[after]" in
            !g#add_arc ((newi, afteri),  Direct);
            !g#add_arc ((afteri, lasti), Direct);
            begin
              !g +> add_arc_opt (finalthen, lasti);
              !g +> add_arc_opt (finalelse, lasti);
              Some lasti
           end)
        
      
  | Selection  (Ast_c.Ifdef (st1s, st2s)), ii -> 
      let (ii,iifakeend) = 
        match ii with
        | [i1;i2;i3;i4] -> [i1;i2;i3], i4
        | [i1;i2;i3] -> [i1;i2], i3
        | _ -> raise Impossible
      in

      let newi = !g +> add_node (Ifdef (stmt, ((), ii))) lbl "ifcpp" in
      !g +> add_arc_opt (starti, newi);
      let newfakethen = !g +> add_node TrueNode  lbl "[then]" in
      let newfakeelse = !g +> add_node FalseNode lbl "[else]" in

      !g#add_arc ((newi, newfakethen), Direct);
      !g#add_arc ((newi, newfakeelse), Direct);

      let aux_statement_list (starti, newauxinfo) statxs =
      statxs +> List.fold_left (fun starti statement ->
        aux_statement (starti, newauxinfo) statement
      ) starti
      in


      let finalthen = aux_statement_list (Some newfakethen, auxinfo_label) st1s in
      let finalelse = aux_statement_list (Some newfakeelse, auxinfo_label) st2s in

      (match finalthen, finalelse with 
        | (None, None) -> None
        | _ -> 
            let lasti =  !g +> add_node (EndStatement (Some iifakeend)) lbl "[endifcpp]" in
            begin
              !g +> add_arc_opt (finalthen, lasti);
              !g +> add_arc_opt (finalelse, lasti);
              Some lasti
           end
      )
      

   (* ------------------------- *)        
  | Selection  (Ast_c.Switch (e, st)), ii -> 
      let (i1,i2,i3, iifakeend) = tuple_of_list4 ii in
      let ii = [i1;i2;i3] in


      let newswitchi = !g +> add_node (SwitchHeader (stmt, (e,ii))) lbl "switch" 
      in
      !g +> add_arc_opt (starti, newswitchi);

      let newendswitch = !g +> add_node (EndStatement (Some iifakeend)) lbl "[endswitch]" in

  
      (* The newswitchi is for the labels to know where to attach.
       * The newendswitch (endi) is for the 'break'. *)

      (* let finalthen = aux_statement (None, newauxinfo) st in *)

      (* Prepare var to be able to copy paste  *)
       let starti = None in
       (* let auxinfo = newauxinfo in *)
       let stmt = st in

       (* COPY PASTE of compound case.
        * Why copy paste ? why can't call directly compound case ? 
        * because we need to build a context_info that need some of the
        * information build inside the compound case: the nodei of {
        *)

       let finalthen = 
           match stmt with
       
           | Ast_c.Compound statxs, ii -> 
               let (i1, i2) = 
                 match ii with 
                 | [i1; i2] -> (i1, i2) 
                 | _ -> raise Impossible
               in

               incr counter_for_braces;
               let brace = !counter_for_braces in

               let o_info  = "{" ^ i_to_s brace in
               let c_info = "}" ^ i_to_s brace in

               (* TODO, we should not allow to match a stmt that corresponds
                * to a compound of a switch, so really SeqStart (stmt, ...)
                * here ? 
                *)
               let newi = !g +> add_node (SeqStart (stmt,brace,i1)) lbl o_info in
               let endnode = mk_node (SeqEnd (brace, i2))    lbl c_info in
               let _endnode_dup = 
                 mk_node (SeqEnd (brace, Ast_c.fakeInfo())) lbl c_info 
               in

               let newauxinfo = 
                { auxinfo_label with 
                  braces = endnode(*_dup*):: auxinfo_label.braces}
               in

               (* new: cos of switch *)
               let newauxinfo = { newauxinfo with 
                     ctx = SwitchInfo (newi, newendswitch, auxinfo.braces);
                     ctx_stack = newauxinfo.ctx::newauxinfo.ctx_stack
                 }
               in
               !g#add_arc ((newswitchi, newi), Direct); 
               (* new: if have not a default case, then must add an edge 
                * between start to end.
                * todo? except if the case[range] coverthe whole spectrum 
                *)

               if not (statxs +> List.exists (function 
                 | (Labeled (Ast_c.Default _), _) -> true
                 | _ -> false
                  ))
               then begin
                 (* when there is no default, then a valid path is 
                  * from the switchheader to the end. In between we
                  * add a Fallthrough.
                 *)

                 let newafter = !g +> add_node FallThroughNode lbl "[switchfall]"
                 in
                 !g#add_arc ((newafter, newendswitch), Direct);
                 !g#add_arc ((newswitchi, newafter), Direct);
                 (* old:
                 !g#add_arc ((newswitchi, newendswitch), Direct) +> adjust_g;
                 *)
               end;

               !g +> add_arc_opt (starti, newi);
               let starti = Some newi in
       
               statxs +> List.fold_left (fun starti stat ->
                 aux_statement (starti, newauxinfo) stat
               ) starti
       
       
               (* braces: *)
               +> fmap (fun starti -> 
                     let endi = !g#add_node endnode  in
                     !g#add_arc ((starti, endi), Direct);
                     endi 
                       )
           | x -> raise Impossible
       in
       !g +> add_arc_opt (finalthen, newendswitch);


       (* what if has only returns inside. We must  try to see if the
        * newendswitch has been used via a 'break;'  or because no 
        * 'default:')
        *)
       let res = 
         (match finalthen with
         | Some finalthen -> 
             !g#add_arc ((finalthen, newendswitch), Direct);
             Some newendswitch
         | None -> 
             if (!g#predecessors newendswitch)#null
             then 
               begin
                 assert ((!g#successors newendswitch)#null);
                 !g#del_node newendswitch;
                 None
               end
             else 
               Some newendswitch
                 
         )
       in
       res
       

  | Labeled (Ast_c.Case  (_, _)), ii
  | Labeled (Ast_c.CaseRange  (_, _, _)), ii -> 

      incr counter_for_switch;
      let switchrank = !counter_for_switch in
      let node, st = 
        match stmt with 
        | Labeled (Ast_c.Case  (e, st)), ii -> 
            (Case (stmt, (e, ii))),  st
        | Labeled (Ast_c.CaseRange  (e, e2, st)), ii -> 
            (CaseRange (stmt, ((e, e2), ii))), st
        | _ -> raise Impossible
      in

      let newi = !g +> add_node node  lbl "case:" in

      (match auxinfo.ctx with
      | SwitchInfo (startbrace, switchendi, _braces) -> 
          (* no need to attach to previous for the first case, cos would be
           * redundant. *)
          starti +> do_option (fun starti -> 
            if starti <> startbrace
            then !g +> add_arc_opt (Some starti, newi); 
            );

          let newcasenodei = 
            !g +> add_node (CaseNode switchrank) 
              lbl ("[casenode] " ^ i_to_s switchrank) 
          in
          !g#add_arc ((startbrace, newcasenodei), Direct);
          !g#add_arc ((newcasenodei, newi), Direct);
      | _ -> raise (Error (CaseNoSwitch (pinfo_of_ii ii)))
      );
      aux_statement (Some newi, auxinfo_label) st
      

  | Labeled (Ast_c.Default st), ii -> 
      incr counter_for_switch;
      let switchrank = !counter_for_switch in

      let newi = !g +> add_node (Default (stmt, ((),ii))) lbl "case default:" in
      !g +> add_arc_opt (starti, newi);

      (match auxinfo.ctx with
      | SwitchInfo (startbrace, switchendi, _braces) -> 
           let newcasenodei = 
             !g +> add_node (CaseNode switchrank) 
               lbl ("[casenode] " ^ i_to_s switchrank) 
           in
           !g#add_arc ((startbrace, newcasenodei), Direct);
           !g#add_arc ((newcasenodei, newi), Direct);
      | _ -> raise (Error (CaseNoSwitch (pinfo_of_ii ii)))
      );
      aux_statement (Some newi, auxinfo_label) st






   (* ------------------------- *)        
  | Iteration  (Ast_c.While (e, st)), ii -> 
     (* starti -> newi ---> newfakethen -> ... -> finalthen -
      *             |---|-----------------------------------|
      *                 |-> newfakelse 
      *)

      let (i1,i2,i3, iifakeend) = tuple_of_list4 ii in
      let ii = [i1;i2;i3] in

      let newi = !g +> add_node (WhileHeader (stmt, (e,ii))) lbl "while" in
      !g +> add_arc_opt (starti, newi);
      let newfakethen = !g +> add_node TrueNode  lbl "[whiletrue]" in
      (* let newfakeelse = !g +> add_node FalseNode lbl "[endwhile]" in *)
      let newafter = !g +> add_node FallThroughNode lbl "[whilefall]" in
      let newfakeelse = !g +> add_node (EndStatement (Some iifakeend)) lbl "[endwhile]" in

      let newauxinfo = { auxinfo_label with
         ctx = LoopInfo (newi, newfakeelse,  auxinfo_label.braces);
         ctx_stack = auxinfo_label.ctx::auxinfo_label.ctx_stack
        }
      in

      !g#add_arc ((newi, newfakethen), Direct);
      !g#add_arc ((newafter, newfakeelse), Direct);
      !g#add_arc ((newi, newafter), Direct);
      let finalthen = aux_statement (Some newfakethen, newauxinfo) st in
      !g +> add_arc_opt (finalthen, newi);
      Some newfakeelse

      
  (* This time, may return None, for instance if goto in body of dowhile
   * (whereas While cant return None). But if return None, certainly 
   * some deadcode.
   *)
  | Iteration  (Ast_c.DoWhile (st, e)), ii -> 
     (* starti -> doi ---> ... ---> finalthen (opt) ---> whiletaili
      *             |--------- newfakethen ---------------|  |---> newfakelse
      *)

      let (iido, iiwhiletail, iifakeend) = 
        match ii with
        | [i1;i2;i3;i4;i5;i6] -> i1, [i2;i3;i4;i5], i6
        | _ -> raise Impossible
      in
      let doi = !g +> add_node (DoHeader (stmt, iido))  lbl "do" in
      !g +> add_arc_opt (starti, doi);
      let taili = !g +> add_node (DoWhileTail (e, iiwhiletail)) lbl "whiletail" 
      in


      let newfakethen = !g +> add_node TrueNode lbl "[dowhiletrue]" in
      (*let newfakeelse = !g +> add_node FalseNode lbl "[enddowhile]" in *)
      let newafter = !g +> add_node FallThroughNode lbl "[dowhilefall]" in
      let newfakeelse = !g +> add_node (EndStatement (Some iifakeend)) lbl "[enddowhile]" in

      let newauxinfo = { auxinfo_label with
         ctx = LoopInfo (taili, newfakeelse, auxinfo_label.braces);
         ctx_stack = auxinfo_label.ctx::auxinfo_label.ctx_stack
        }
      in

      !g#add_arc ((taili, newfakethen), Direct); 
      !g#add_arc ((newafter, newfakeelse), Direct);
      !g#add_arc ((taili, newafter), Direct);

      !g#add_arc ((newfakethen, doi), Direct); 

      let finalthen = aux_statement (Some doi, newauxinfo) st in 
      (match finalthen with
      | None -> 
          if (!g#predecessors taili)#null
          then raise (Error (DeadCode (Some (pinfo_of_ii ii))))
          else Some newfakeelse
      | Some finali -> 
          !g#add_arc ((finali, taili), Direct);
          Some newfakeelse
      )
          


  | Iteration  (Ast_c.For (e1opt, e2opt, e3opt, st)), ii -> 
      let (i1,i2,i3, iifakeend) = tuple_of_list4 ii in
      let ii = [i1;i2;i3] in

      let newi = 
        !g +> add_node (ForHeader (stmt, ((e1opt, e2opt, e3opt), ii))) lbl "for" 
      in
      !g +> add_arc_opt (starti, newi);
      let newfakethen = !g +> add_node TrueNode  lbl "[fortrue]" in
      (*let newfakeelse = !g +> add_node FalseNode lbl "[endfor]" in*)
      let newafter = !g +> add_node FallThroughNode lbl "[forfall]" in
      let newfakeelse = !g +> add_node (EndStatement (Some iifakeend)) lbl "[endfor]" in

      let newauxinfo = { auxinfo_label with
           ctx = LoopInfo (newi, newfakeelse, auxinfo_label.braces); 
           ctx_stack = auxinfo_label.ctx::auxinfo_label.ctx_stack
        }
      in

      !g#add_arc ((newi, newfakethen), Direct);
      !g#add_arc ((newafter, newfakeelse), Direct);
      !g#add_arc ((newi, newafter), Direct);
      let finalthen = aux_statement (Some newfakethen, newauxinfo) st in
      !g +> add_arc_opt (finalthen, newi);
      Some newfakeelse


   (* ------------------------- *)        
  | Jump ((Ast_c.Continue|Ast_c.Break) as x),ii ->  
      (* flow_to_ast: *)
      let newi = 
        !g +> add_node 
          (match x with
          | Ast_c.Continue -> Continue (stmt, ((), ii))
          | Ast_c.Break    -> Break    (stmt, ((), ii))
          | _ -> raise Impossible
          )
          lbl "continue_break;"
      in
      !g +> add_arc_opt (starti, newi);

      (* let newi = some starti in *)

      (match auxinfo.ctx with
      | LoopInfo (loopstarti, loopendi, braces) -> 
          let desti = 
            (match x with 
            | Ast_c.Break -> loopendi 
            | Ast_c.Continue -> loopstarti 
            | x -> raise Impossible
            ) in
          let difference = List.length auxinfo.braces - List.length braces in
          assert (difference >= 0);
          let toend = take difference auxinfo.braces in
          let newi = insert_all_braces toend newi in
          !g#add_arc ((newi, desti), Direct);
          None

      | SwitchInfo (startbrace, loopendi, braces) -> 
          if x = Ast_c.Break then
            begin
              let difference = 
                List.length auxinfo.braces - List.length braces
              in
              assert (difference >= 0);
              let toend = take difference auxinfo.braces in
              let newi = insert_all_braces toend newi in
              !g#add_arc ((newi, loopendi), Direct);
              None
            end
          else 
            (* old: raise (OnlyBreakInSwitch (fst (List.hd ii)))
             * in fact can have a continue, 
             *)
           if x = Ast_c.Continue then
             (try 
               let (loopstarti, loopendi, braces) = 
                 auxinfo.ctx_stack +> find_some (function 
                   | LoopInfo (loopstarti, loopendi, braces) -> 
                       Some (loopstarti, loopendi, braces)
                   | _ -> None
                                                ) in
               let desti = loopstarti in
               let difference = 
                 List.length auxinfo.braces - List.length braces in
               assert (difference >= 0);
               let toend = take difference auxinfo.braces in
               let newi = insert_all_braces toend newi in
               !g#add_arc ((newi, desti), Direct);
               None
               
               with Not_found -> 
                 raise (Error (OnlyBreakInSwitch (pinfo_of_ii ii)))
             )
           else raise Impossible
      | NoInfo -> raise (Error (NoEnclosingLoop (pinfo_of_ii ii)))
      )        





  | Jump ((Ast_c.Return | Ast_c.ReturnExpr _) as kind), ii -> 
     (match auxinfo.exiti, auxinfo.errorexiti with
     | None, None -> 
         raise (Error (NoExit (pinfo_of_ii ii)))
     | Some exiti, Some errorexiti -> 

      (* flow_to_ast: *)
      let info = 
        match kind with
        | Ast_c.Return -> "return"
        | Ast_c.ReturnExpr _ -> "return ..."
        | _ -> raise Impossible
      in
      let newi = 
        !g +> add_node 
          (match kind with
          | Ast_c.Return ->       Return (stmt, ((),ii))
          | Ast_c.ReturnExpr e -> ReturnExpr (stmt, (e, ii))
          | _ -> raise Impossible
          )
          lbl info 
      in
      !g +> add_arc_opt (starti, newi);
      let newi = insert_all_braces auxinfo.braces newi in

      if auxinfo.under_ifthen
      then !g#add_arc ((newi, errorexiti), Direct)
      else !g#add_arc ((newi, exiti), Direct)
      ;
      None
     | _ -> raise Impossible
     )


  (* ------------------------- *)        
  | Ast_c.Decl decl, ii -> 
     let s = 
       (match decl with
       | (Ast_c.DeclList ([(Some ((s, _),_), typ, sto), _], _)) -> 
           "decl:" ^ s
       | _ -> "decl_novar_or_multivar"
       ) in
            
     let newi = !g +> add_node (Decl (decl)) lbl s in
     !g +> add_arc_opt (starti, newi);
     Some newi
      
  (* ------------------------- *)        
  | Ast_c.Asm body, ii -> 
      let newi = !g +> add_node (Asm (stmt, ((body,ii)))) lbl "asm;" in
      !g +> add_arc_opt (starti, newi);
      Some newi

  | Ast_c.MacroStmt, ii -> 
      let newi = !g +> add_node (Macro (stmt, ((),ii))) lbl "macro;" in
      !g +> add_arc_opt (starti, newi);
      Some newi


  (* ------------------------- *)        
  | Ast_c.NestedFunc def, ii -> 
      raise (Error NestedFunc)
      



(*****************************************************************************)
(* Definition of function *)
(*****************************************************************************)

let (aux_definition: nodei -> definition -> unit) = fun topi funcdef ->

  let lbl_start = [!counter_for_labels] in

  let ((funcs, functype, sto, compound), ii) = funcdef in
  let iifunheader, iicompound = 
    (match ii with 
    | is::ioparen::icparen::iobrace::icbrace::iifake::isto -> 
        is::ioparen::icparen::iifake::isto,     [iobrace;icbrace]
    | _ -> raise Impossible
    )
  in

  let topstatement = Ast_c.Compound compound, iicompound in

  let headi = !g +> add_node (FunHeader ((funcs, functype, sto), iifunheader))
                         lbl_start ("function " ^ funcs) in
  let enteri     = !g +> add_node Enter     lbl_empty "[enter]"     in
  let exiti      = !g +> add_node Exit      lbl_empty "[exit]"      in
  let errorexiti = !g +> add_node ErrorExit lbl_empty "[errorexit]" in

  !g#add_arc ((topi, headi), Direct);
  !g#add_arc ((headi, enteri), Direct);

  (* ---------------------------------------------------------------- *)
  (* todocheck: assert ? such as we have "consommer" tous les labels  *)
  let info = 
    { initial_info with 
      labels = lbl_start;
      labels_assoc = compute_labels_and_create_them topstatement;
      exiti      = Some exiti;
      errorexiti = Some errorexiti;
    } 
  in

  let lasti = aux_statement (Some enteri, info) topstatement in
  !g +> add_arc_opt (lasti, exiti)

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

(* Helpers for SpecialDeclMacro *)
let specialdeclmacro_to_stmt (s, args, ii) =
  let (iis, iiopar, iicpar, iiptvirg) = tuple_of_list4 ii in
  let ident = (Ast_c.Ident s, Ast_c.noType()), [iis] in
  let f = (Ast_c.FunCall (ident, args), Ast_c.noType()), [iiopar;iicpar] in
  let stmt = Ast_c.ExprStatement (Some f), [iiptvirg] in
  stmt,  (f, [iiptvirg])



let ast_to_control_flow e = 

  (* globals (re)initialialisation *) 
  g := (new ograph_mutable);
  counter_for_labels := 1;
  counter_for_braces := 0;
  counter_for_switch := 0;

  let topi = !g +> add_node TopNode lbl_empty "[top]" in

  match e with 
  | Ast_c.Definition (((funcs, _, _, c),_) as def) -> 
      (* if !Flag.show_misc then pr2 ("build info function " ^ funcs); *)
      aux_definition topi def;
      Some !g

  | Ast_c.Declaration _ 
  | Ast_c.Include _ 
  | Ast_c.MacroTop _
    -> 
      let (elem, str) = 
        match e with 
        | Ast_c.Declaration decl -> (Control_flow_c.Decl decl),  "decl"
        | Ast_c.Include (a,b) -> (Control_flow_c.Include (a,b)), "#include"
        (* todo? still useful ? could consider as Decl instead *)
        | Ast_c.MacroTop (s, args, ii) -> 
            let (st, (e, ii)) = specialdeclmacro_to_stmt (s, args, ii) in
            (Control_flow_c.ExprStatement (st, (Some e, ii))), "macrotoplevel"

        | _ -> raise Impossible
      in
      let xi =   !g +> add_node elem    lbl_empty str in
      let endi = !g +> add_node EndNode lbl_empty "[end]" in

      !g#add_arc ((topi, xi),Direct);
      !g#add_arc ((xi, endi),Direct);
      Some !g

  | Ast_c.Define ((s,ii), (defkind, defval))  -> 
      let headeri = 
        !g +> add_node 
          (DefineHeader ((s, ii), defkind)) lbl_empty ("#define " ^ s)
      in
      !g#add_arc ((topi, headeri),Direct);

      (match defval with
      | Ast_c.DefineExpr e -> 
          let xi = !g +> add_node (DefineExpr e) lbl_empty "defexpr" in
          let endi = !g +> add_node EndNode lbl_empty "[end]" in
          !g#add_arc ((headeri, xi) ,Direct);
          !g#add_arc ((xi, endi) ,Direct);
          
      | Ast_c.DefineType ft -> 
          let xi = !g +> add_node (DefineType ft) lbl_empty "deftyp" in
          let endi = !g +> add_node EndNode lbl_empty "[end]" in
          !g#add_arc ((headeri, xi) ,Direct);
          !g#add_arc ((xi, endi) ,Direct);

      | Ast_c.DefineStmt st -> 
          let info = initial_info in
          let lasti = aux_statement (Some headeri , info) st in
          lasti +> do_option (fun lasti -> 
            let endi = !g +> add_node EndNode lbl_empty "[end]" in
            !g#add_arc ((lasti, endi), Direct)
          )
          

      | Ast_c.DefineDoWhileZero (st, ii) -> 
          let headerdoi = 
            !g +> add_node (DefineDoWhileZeroHeader ((),ii)) lbl_empty "do0" 
          in
          !g#add_arc ((headeri, headerdoi), Direct);
          let info = initial_info in
          let lasti = aux_statement (Some headerdoi , info) st in
          lasti +> do_option (fun lasti -> 
            let endi = !g +> add_node EndNode lbl_empty "[end]" in
            !g#add_arc ((lasti, endi), Direct)
          )

      | Ast_c.DefineFunction def -> 
          aux_definition headeri def;

      | Ast_c.DefineText (s, ii) -> 
          raise Todo
      | Ast_c.DefineEmpty -> 
          let endi = !g +> add_node EndNode lbl_empty "[end]" in
          !g#add_arc ((headeri, endi),Direct);
      );

      Some !g
      

  | _ -> None


(*****************************************************************************)
(* CFG checks *)
(*****************************************************************************)

(* the second phase, deadcode detection. Old code was raising DeadCode if
 * lasti = None, but maybe not. In fact if have 2 return in the then
 * and else of an if ? alt: but can assert that at least there exist
 * a node to exiti, just check #pred of exiti 
 * 
 * old: I think that DeadCode is too aggressive, what if have both
 * return in else/then ? 
 * 
 * Why so many deadcode in Linux ? Ptet que le label est utilisé 
 * mais dans le corps d'une macro et donc on le voit pas :(
 * 
 *)
let deadcode_detection g = 

  g#nodes#iter (fun (k, node) -> 
    let pred = g#predecessors k in
    if pred#null then 
      (match unwrap node with
      (* old: 
       * | Enter -> ()
       * | EndStatement _ -> pr2 "deadcode sur fake node, pas grave"; 
       *)
      | TopNode -> ()
      | FunHeader _ -> ()
      | ErrorExit -> ()
      | Exit -> ()     (* if have 'loop: if(x) return; i++; goto loop' *)
      | SeqEnd _ -> () (* todo?: certaines '}' deviennent orphelins *)
      | x -> 
          (match Control_flow_c.extract_fullstatement node with
          | Some (st, ii) -> raise (Error (DeadCode (Some (pinfo_of_ii ii))))
          | _ -> 
             pr2 "control_flow: orphelin nodes, maybe something wierd happened"
          )
      )
  )

(*------------------------------------------------------------------------*)
(* special_cfg_braces: the check are really specific to the way we
 * have build our control_flow, with the { } in the graph so normally
 * all those checks here are useless.
 * 
 * evo: to better error reporting, to report earlier the message, pass
 * the list of '{' (containing morover a brace_identifier) instead of
 * just the depth. *)

let (check_control_flow: cflow -> unit) = fun g ->

  let nodes = g#nodes  in
  let starti = get_first_node g in

  let visited = ref (new oassocb []) in

  let print_trace_error xs =  pr2 "PB with flow:";  pr2 (Dumper.dump xs); in

  let rec dfs (nodei, (* Depth depth,*) startbraces,  trace)  = 
    let trace2 = nodei::trace in
    if !visited#haskey nodei 
    then 
      (* if loop back, just check that go back to a state where have same depth
         number *)
      let (*(Depth depth2)*) startbraces2 = !visited#find nodei in
      if  (*(depth = depth2)*) startbraces <> startbraces2
      then  
        begin 
          pr2 (sprintf "PB with flow: the node %d has not same braces count" 
                 nodei);  
          print_trace_error trace2  
        end
    else 
      let children = g#successors nodei in
      let _ = visited := !visited#add (nodei, (* Depth depth*) startbraces) in

      (* old: good, but detect a missing } too late, only at the end
      let newdepth = 
        (match fst (nodes#find nodei) with
        | StartBrace i -> Depth (depth + 1)
        | EndBrace i   -> Depth (depth - 1)
        | _ -> Depth depth
        ) 
      in
      *)
      let newdepth = 
        (match unwrap (nodes#find nodei),  startbraces with
        | SeqStart (_,i,_), xs  -> i::xs
        | SeqEnd (i,_), j::xs -> 
            if i = j 
            then xs
            else 
              begin 
                pr2 (sprintf ("PB with flow: not corresponding match between }%d and excpeted }%d at node %d") i j nodei); 
                print_trace_error trace2; 
                xs 
              end
        | SeqEnd (i,_), [] -> 
            pr2 (sprintf "PB with flow: too much } at }%d " i);
            print_trace_error trace2; 
            []
        | _, xs ->  xs
        ) 
      in

   
      if children#tolist = [] 
      then 
        if (* (depth = 0) *) startbraces <> []
        then print_trace_error trace2
      else 
        children#tolist +> List.iter (fun (nodei,_) -> 
          dfs (nodei, newdepth, trace2)
        )
    in

  dfs (starti, (* Depth 0*) [], [])

(*****************************************************************************)
(* Error report *)
(*****************************************************************************)

let report_error error = 
  let error_from_info info = 
    (Common.error_message_short info.file ("", info.charpos))
  in
  match error with
  | DeadCode          infoopt -> 
      (match infoopt with
      | None ->   pr2 "FLOW: deadcode detected, but cant trace back the place"
      | Some info -> pr2 ("FLOW: deadcode detected: " ^ error_from_info info)
      )
  | CaseNoSwitch      info -> 
      pr2 ("FLOW: case without corresponding switch: " ^ error_from_info info)
  | OnlyBreakInSwitch info -> 
      pr2 ("FLOW: only break are allowed in switch: " ^ error_from_info info)
  | NoEnclosingLoop   (info) -> 
      pr2 ("FLOW: can't find enclosing loop: " ^ error_from_info info)
  | GotoCantFindLabel (s, info) ->
      pr2 ("FLOW: cant jump to " ^ s ^ ": because we can't find this label")
  | NoExit info -> 
      pr2 ("FLOW: can't find exit or error exit: " ^ error_from_info info)
  | DuplicatedLabel s -> 
      pr2 ("FLOW: duplicate label" ^ s)
  | NestedFunc  -> 
      pr2 ("FLOW: not handling yet nested function")
  | ComputedGoto -> 
      pr2 ("FLOW: not handling computed goto yet")
