(****************** Matematica - corso di LPL: Laboratorio 2021- ****************)
(* ******************************************************************************)
(* Definizione del Linguaggio Small21: Nucleo di C modificato ed esteso         *)
(* Realizzazione di un Esecutore di Small21 utilizzando OCaml                   *)
(*                + Laboratorio del 12/04/21  			                        *)
(*                + Laboratorio del 13/04/21  			                        *)
(*                + Laboratorio del 20/04/21  			                        *)
(*                + Laboratorio del 27/04/21  			                        *)
(*                + Laboratorio del 04/05/21  			                        *)
(*                + Laboratorio del 11/05/21  			                        *)
(*                + Laboratorio del 18/05/21  			                        *)
(*                + Laboratorio del 25/05/21  			                        *)
(********************************************************************************)

(* ============================================================================ *)
(* Import *)

open String;;
open List;;
open Printf;; 

(* ============================================================================ *)
(* Parametri di Programma e Globali *)

let storeSize = 1000;;   (* store size in value cells *)
let bTab = 3;;           (* indentation spaces for in_Line blocks *)
let mTab = 5;;			 (* indentation spaces for the Program block *)
let tracer = true;;		 (* on/off for the computation trace *)

(* =========================================================================== *)
(* The Token, i.e. Lexical Categories *)

type ide = string;;
type lab = string;;
type num = int;;
type bool = True | False;;
type unit = Null


(* ============================================================================ *)
(* The Syntactic Categories *)
(* Vedi Laboratorio1 (slides) del 12/04/21 *)

type tye = 
	Int
	| Bool
	| Lab
	| Void
	| Mut of tye
	| Arr of tye * num
	| Terr
	| Abs of tye * tyeSeq
	| Unit
	and

tyeSeq = tye list
	and	
	
dcl = 
	Var of tye * ide * exp
	| Const of tye * ide * exp
	| Array of tye * ide
	| SeqD of dcl * dcl
	| Pcd of tye * ide * fpars * blockP
	| ED
	and 

fpars = 
	EFP
	| FP of ppf * tye * ide
	and

ppf = 
	Value
	| Ref
	| Constant
	and

blockP = BlockP of dcl * stm (*Blocco procedura*)
	and
	
exp = 
	Val of ide
	| Arrow1 of ide * exp
	| N of num
	| Plus of exp * exp
	| Minus of exp * exp				(************* added 13.05.21 *****)
	| LT of exp * exp					(************* added **************)
	| GT of exp * exp					(************* added **************)
	| Eq of exp * exp					
	| B of bool
	| Or of exp * exp
	| Apply of ide * apars
	| EE
    and

apars = 
	EAP
	| AP of exp
	and

cmd = 
	LabC of lab * stm
	| UnL of stm
	| SeqC of cmd * cmd
	and 
	
stm = 
	Upd of exp * exp
	| IfT of exp * stm
	| Goto of lab
	| SeqS of stm * stm
	| BlockS of dcl * stm
	| Call of ide * apars
	| Return of exp
	| ES
	and			

prog = Prog of ide * block
	and
	
block = Block of dcl * cmd
    ;;

(* Operations for the AST presentation in the given Concrete Syntax *)
(* Vedi Laboratorio1 (slides) del 13/04/21 *)

exception SyntaxError of int * string;;

let toStringI (i:ide) = sprintf "%s" i;;

let toStringN (n:num) = sprintf "%d" n;;

let toStringB = function
		| True -> "true"
		| False -> "false"
		;;

let rec indent tab = 
		if tab < 0 then ""
		else if tab = 0 then ""
			 else " " ^ (indent (tab-1))
		;;

(* modificata per includere esempio array multidimensionale *)
(* ulteriormente modificata per includere errori con codici differenti *) 
let rec toStringTye = (function 
		| Int -> "int"
		| Bool -> "bool"
	    | Lab -> "lab"
	    | Void -> "void"
		| Arr(ty,n) -> (
			match sepArr ty with
				| (tyB,dim) 
					when isSimple tyB -> (toStringTye tyB) ^ "[" 
											^ (toStringN n) ^ "]" ^ dim
				|_ -> let msg = "toStringTye: AST Hidden types" in
					raise(SyntaxError(1,msg)))
	    |_ ->  let msg = "toStringTye: AST Hidden types" in
	    	   raise (SyntaxError(2,msg)))	
       and

isSimple = (function
		| Int -> true
		| Bool -> true
		| _ -> false)
	   and

isArr = (function
		| Arr(t,n) 
			when (isSimple t) && (n>0) -> true
		| _ -> false)
	   and

sepArr = (function
		| Arr(ty,n) -> let (tyB,dim) = sepArr ty in
					   (tyB,"[" ^ (toStringN n) ^ "]" ^ dim)
	    | ty -> (ty,""))
	   and	   
				
toStringTseq t = (
		let msg = "toStringTye: AST Hidden types" in
	    raise (SyntaxError (3,msg)))
		and 
(* Fine: modifiche apportate attivita' del 13/04/2021 *)


(* modificata per includere variabile non inizializzata *)
(* modificata per correggere scambio tra trattamento Const e Var *)
toStringDcl tab d = (
		match d with
		  | Var(tye,ide,EE) ->  ((indent tab) ^ (toStringTye tye) ^ " " 
								 ^ (toStringI ide) ^ ";")
		  | Var(tye,ide,exp) ->  ((indent tab) ^ (toStringTye tye) ^ " " 
								 ^ (toStringI ide) ^ " = " ^ (toStringExp exp) ^ ";")
		  | Const(tye,ide,exp) -> ((indent tab) ^ "const " ^ (toStringTye tye) ^ " " 
								  ^ (toStringI ide) ^ " = " ^ (toStringExp exp) ^ ";")
	      | Array(tye,ide) -> ((indent tab) 
	      						^ (toStringTye tye) ^ " " ^ (toStringI ide) ^ ";")
	      | SeqD(d1,d2) -> ((flattenD tab d1) ^ (flattenD tab d2))
	      | Pcd(tye,ide,fpars,blockP) ->(
	      				(indent tab) ^ (toStringTye tye) ^ " " 
	      				^ (toStringI ide) ^ "(" ^ (toStringFPars fpars) ^ "){\n" 
	      				^ (toStringBlockP (tab+7) blockP))	      				   
	      | ED -> ""
		)
		and
(* Fine: modifiche apportate attivita' del 13/04/2021 *)

flattenD tab = (function
		| SeqD(d1,d2) -> (flattenD tab d1) ^ (flattenD tab d2)
		| ED -> "" 
		| x -> (toStringDcl tab x) ^ "\n"
		)
		and

toStringFPars = (function
		| EFP -> ""
		| FP(ppf,tye,ide) -> ((toStringPPF ppf) ^ " " 
							 ^ (toStringTye tye) ^ " " ^ (toStringI ide))
		)
		and
		
toStringPPF = (function
		| Value -> "value"
		| Ref -> "ref"
		| Constant -> "constant"	
		)
		and
		
toStringBlockP tab (BlockP(dcl,stm)) = (
		let ddString = toStringDcl tab dcl in
		if String.length ddString = tab 
		then (toStringStm tab stm) ^ (indent tab) ^ "}"
		else ddString ^ (toStringStm tab stm) ^ "\n" ^ (indent tab) ^ "}"
		)
		and

toStringExp = (function
		| Val ide -> toStringI ide
		| Arrow1(ide,exp) -> (toStringI ide) ^ "[" ^ (toStringExp exp) ^ "]"
		| N n -> toStringN n 
		| Plus(exp1,exp2) -> "(" ^ (toStringExp exp1) ^ " + "
								 ^ (toStringExp exp2) ^ ")"
		| Minus(exp1,exp2) ->                 (************* added 13.05.21 *****)
								"(" ^ (toStringExp exp1) ^ " - " 
								 ^ (toStringExp exp2) ^ ")"
		| Eq(exp1,exp2) -> "(" ^ (toStringExp exp1) ^ " == "
								 ^ (toStringExp exp2) ^ ")"
		| LT(exp1,exp2) -> "(" ^ (toStringExp exp1) ^ " < "
								 ^ (toStringExp exp2) ^ ")"
		| GT(exp1,exp2) -> "(" ^ (toStringExp exp1) ^ " > "
								 ^ (toStringExp exp2) ^ ")"
		| B b -> toStringB b 
		| Or(exp1,exp2) -> "(" ^ (toStringExp exp1) ^ " or "
								 ^ (toStringExp exp2) ^ ")"
		| Apply(ide,apars) -> (toStringI ide) ^ "(" 
							  ^ (toStringAPar apars) ^ ")"
        | EE -> ""
		)
		and
		
toStringAPar = (function
		| EAP -> ""
		| AP exp -> toStringExp exp	
		)
		and
				
toStringCmd tab = (function
		| LabC(lab,stm) -> (indent tab) ^ (toStringI lab) ^ ": " 
						   ^ (toStringStm 0 stm)
		| UnL stm -> toStringStm tab stm
		| SeqC(cmd1,cmd2) -> (toStringCmd tab cmd1) ^ "\n" 
							 ^ (toStringCmd tab cmd2)

		)
		and
		
toStringStm tab = function
        | Upd(exp1,exp2) -> ((indent tab) ^ (toStringExp exp1) ^ " = " 
							^ (toStringExp exp2) ^ ";")
		| IfT(exp,stm) ->(  
				let expString = 
						 (if isAtomic exp then "(" ^ (toStringExp exp) ^ ")"
						  else toStringExp exp) 
				     and stmString = (toStringStm 1 stm) 
				     in
				(indent tab) ^ "if " ^ expString ^ stmString )
		| Goto lab -> (indent tab) ^ "goto " ^ (toStringI lab) ^ ";"
		| SeqS(stm1,stm2) -> (flattenC tab stm1) ^ (flattenC tab stm2)
		| BlockS(dcl,stm) -> (sprintf "%s{\n%s\n%s\n%s}" 
									  (indent(tab+bTab))
									  (toStringDcl (tab+bTab+1) dcl)
								  	  (toStringStm (tab+bTab+1) stm)
								  	  (indent (tab+bTab)))
		| Call(ide,apars) -> (sprintf "%s%s(%s);" 
									 (indent tab)(toStringI ide)
									 (toStringAPar apars))
		| Return exp -> (sprintf "%sreturn %s;"
								(indent tab)(toStringExp exp))
		| ES -> ""
		
		and
		
isAtomic = (function
		| Val _ -> true 
		| N _ -> true 
		| B _ -> true 
		| Arrow1 _ -> true 
		| Apply _ -> true 
		| _ -> false
		)
	    and

flattenC tab = (function
		| SeqS(m1,m2) -> (flattenC tab m1) ^ (flattenC tab m2)
		| ES -> "" 
		| stm -> (toStringStm tab stm) ^ "\n"
		)
		and

toStringProg (Prog (ide,block)) = 
		"Program " ^ (toStringI ide) ^ "{\n"
		^ (toStringBlock mTab block)
	   and
	
toStringBlock tab (Block(dcl,cmd)) = (
		let ddString = toStringDcl tab dcl 
			and cmdString = (toStringCmd tab cmd) 
							 ^ (indent tab) ^ "}" in
		if String.length ddString = tab then cmdString
		else ddString ^ cmdString
		)
       ;;

let printDcl tab dcl = printf "\n%s\n\n" (toStringDcl tab dcl);;
let printExp exp = printf "\n%s\n\n" (toStringExp exp);;
let printCmd tab cmd = printf "\n%s\n\n" (toStringCmd tab cmd);;
let printStm tab stm = printf "\n%s\n\n" (toStringStm tab stm);;
let printProg prog = printf "\n%s\n\n" (toStringProg prog);;


(* ================================================================================ *)
(* The Semantic Structure: Store, Frame, Activation Record, Stack                   *)	
(* ================================================================================ *)
(* Store: Definition *)

type loc = Loc of int 
	and                       

mval = MvalI of num      (* corretto num invece dell'errato int *)
	| MvalB of bool
	| Undef
	and

store = Store of int * (loc -> mval);;

(* Funzione di Astrazione ed Invariante di Rappresentazione per valori c di tipo store: 
1) Presentazione (o forma dei valori): [l1<-Mv1,...,lk<-Mvk] per k>=0
2) AF(c) = [] if c=Store(0,g) 
   AF(c) = [l1<-Mv1,...,lk<-Mvk] 
   		  if c=Store(k,g) and (perogni i\in[1..k], li=Loc(i-1) and (g li)= Mvi).
3) I(Store(n,g)) = 0≤n<=storeSize and 
				  (perogni i\in[n+1..storeSize], li=Loc(i-1) and (g li) = Undef)
*) 

(* Store: Operations and Exceptions *)
exception CodeError of string * string;;
exception IllegalStoreAddress of string * loc;;
exception UndefinedLoc of string * loc;;
exception StoreSizeExceeded of string * int * int;;

(*Crea uno store vuoto*)
let emptyStore () =  
		Store (0, fun ((Loc n)as x) -> 
					if (n >= 0) && (n < storeSize) 
					then Undef
					else raise(IllegalStoreAddress("emptyStore",x)))
		;; (* uso di patterns come parametri e di "as" per nominarli *)
		
let allocate (Store(d,g)) (k:int) = (*Alloca, in uno store mu, k locazioni libere in sequenza 
									da d, modifica mu in mu1 e restituisce (d,mu1)*)
		if (d+k<=storeSize)
		then ((Loc d),Store(d+k,g))
		else raise (StoreSizeExceeded ("allocate",d,k))
		;; (* uso di pattern come parametri e di parametri con tipo *)
	  		 
let upd (Store(d,g)) (loc:loc) (mv:mval) = (*Modifica il valore della locazione loc con 
											mv, con loc che deve essere già allocata*)
		let checkLoc = fun (Loc x) -> x >= 0 && x < d 
					(* dichiarare esternamente come ausiliaria quando 
					   utilizzata anche in altre operazioni *)
		in	if checkLoc loc
			then Store(d, fun l -> if l = loc then mv else g l)
			else raise (IllegalStoreAddress("upd",loc))
			;; (* uso di pattern come parametri e di parametri con tipo *)

let checkLoc = fun (Loc x) d -> x >= 0 && x < d ;; 	(* operazione locale *)	

let getStore (Store(d,g)) (loc:loc) = (*Restituisce il valore della locazione loc, con 
										loc che deve essere già allocata e definita*)
		if not(checkLoc loc d) 
		then raise (IllegalStoreAddress("getStore",loc))
		else match g loc with
				Undef -> raise (UndefinedLoc ("getStore",loc))
				| ret -> ret
				;;(* uso di pattern come parametri e di parametri con tipo *)
				
(* ================================================================================ *)
(* Store: Operation for store presentation *)

let toStringMval = function
		 | MvalI n -> toStringN n
		 | MvalB b -> toStringB b
		 | _ -> "Undef";;

let toStringLoc (Loc n) = sprintf "L%d" n;; 

let toStringStore (Store(d,g)) = (* una presentazione dello store *)
		if d = 0 then "[]"
		else let pair n h = 
						(let loc = Loc n in 
						 let v = toStringMval(h loc) and l = toStringLoc loc in
						 l^"<-"^v) in
		     let rec range n m h = (* requires n<m *)
						(if n = m-1 
						then pair n h
						else (pair n h)^","^(range (n+1) m h)) in	
			"["^(range 0 d g)^"]"							
		;;
let storeDump store = 			 (* dump dello store *)
		printf "\n%s\n\n" (toStringStore store);;


(* ================================================================================ *)
(* Frame and  Denotable Values *)
(* ================================================================================ *)

type arLink = int
   and

aval = (* Atomic VALues: Corretto Bval of bool invece di
		   True e False che sono overtyped con bool *)				
	Ival of num
	| Bval of bool
	| Null
	and

cnt = cmd list    						(***** anticipata 11.05.21 ******)
	and

dval = (*Valore denotabili*)
	DConst of tye * aval
	| DVar of tye * loc
	| DArry of tye * loc				(************* added **************)
	| DAbs of tye * closure	
	| DLab of tye * cmd					(************* added 11.05.21 *****)
	| Unbound
	and

closure = (* procedure e funzioni procedurali *)
	ClosT of ide * tye * fpars * dcl * stm * arLink      (****** modified*******)
	(* Ulteriori per parametri per nome, o per funzioni come valori, o per ...*)
   and  
 
env = Env of ide list * (ide -> dval)
   ;;

(* Funzione di Astrazione ed Invariante di Rappresentazione per env:
   Include anche il dominio come valore di tipo ide list.
1) Presentazione (o forma dei valori c): [id1/D1,...,idk/Dk] per k>=0
2) AF(c) = [] if c = Env(l,g) and (perogni id, g(id) = Unbound)
   AF(c) = [id1/D1,...,idk/Dk] 
   		 if c = Env(l,g) and (g(idi) = Di per i\in[1..k], 
   		                      g(x) = Unbound altrimenti).
3) I(c) = (c = Env(l,g) => (List.mem x l) iff (g(x) != Umbound)
*)

(* ================================================================================ *)
(* Environment: Operations and Exceptions *)
exception UnboundIde of string * ide;;

let emptyEnv () = (*Crea un environment vuoto*) 
		Env ([] , fun ide -> Unbound)
		;;

let bind (Env(l,r)) ide dval = (*Aggiunge il binding [ide/dval] all'ambiente*)
		Env(ide::l , fun i -> if i = ide then dval else r i)
		(* Cosa se esiste gia' una locale ide' e ide = ide'? Risposta: ide e ide' sono entrambi
		presenti in l, ma il valore della funzione è quello di ide *)
		;;	
let defined (Env(l,r)) ide = (*Dice se ide è definito nell'environment Env(l,r)*)
		mem ide l
		;;
let getEnv (Env(l,r)) ide = (*Se ide è definito nell'environment Env(l,r), restituisce il valore 
							denotabile associato a ide*)		
		if mem ide l then r ide
		else raise (UnboundIde ("getEnv",ide))
		;;
		
(* ================================================================================ *)
(* Frame: Operation for Frame presentation *)

(* for internal use only *)
let rec toTyeInner = function  
		| Int -> "int"
		| Bool -> "bool"
	    | Lab -> "lab"
	    | Void -> "void"
	    | Unit -> "Unit"  
	    | Mut ty -> "M" ^ (toTyeInner ty) 
	    | Arr(ty,n) -> (let (tyB,dim) = sepArr ty in
	    	 ":" ^ (toTyeInner tyB) ^ "[" ^ (toStringN n) ^ "]" ^ dim)
	    | Terr -> "E"
		| Abs(ty,tyS) -> 
			 let g = (fun x a -> 
			 			let check = ((get a 0) = ']') in
        			    if check then (x^a) else (x^","^a)) in
            "[" ^ (toTyeInner ty) ^ "::" 
            ^ (fold_right g (map toTyeInner tyS) "]")
       and

toStringClos (ClosT(ide,ty,fpar,dcl,cmd,ark)) = 
		 "$" ^ (toStringI ide) ^ ","
		  ^ (toTyeInner ty) ^ ",:fpar:,:cmd:,"
		  ^ (string_of_int ark) ^ "$"
		and

toStringAval = function			
		| Ival num -> toStringN num
		| Bval True -> "true"
		| Bval False -> "false"
	    | Null -> "Null"
		and

toStringCnt cnt =
		"cnt" 								(************* added 11.05.21 ********)
		and

toStringDval = function
		DConst(ty,aval) -> sprintf "(%s,%s)" 
					(toTyeInner ty)(toStringAval aval)
		| DVar(ty,loc) -> sprintf "(%s,%s)" 
					(toTyeInner ty)(toStringLoc loc)
		| DArry(ty,loc) -> sprintf "(%s,%s)" 
					(toTyeInner ty)(toStringLoc loc)
		| DAbs(ty,clos) -> sprintf "(%s,%s)" 
					(toTyeInner ty)(toStringClos clos)
		| DLab(ty,cnt)	->	sprintf "(%s,%s)"			(*** added 11.05.21 **)
					(toTyeInner ty)":cmd:"
		| _ -> "Unbound"	
		and
		
toStringEnv (Env (ideList, g)) = (* una presentazione di Env *)
		let pair h i = (let ide = toStringI i and 
		    den = toStringDval (h i) in ide^"/"^den) in
        let pList = List.map (pair g) ideList in
        let gTemp = (fun x a -> let check = ((get a 0) = ']') in
        			            if check then x^a else (x^";\n "^a)) in
        "[" ^ (fold_right gTemp pList "]")
		and

envDump env = (* dump di env *)
		printf "\n%s\n\n" (toStringEnv env);;


(* ================================================================================ *)
(* Activation Records *)
(* ================================================================================ *)

type head = Name of ide | Exp | Cmd | NoneH | IPB;;  (***** 19.05.21 *****)
type stch = int;;
type frame = env;;
(* type cnt = cmd list;;     anticipata: valore denotabile 11.05.21 ******)
type res = aval option;;

type ar = AR of (head * stch * frame * cnt * res);;

(* Funzione di Astrazione ed Invariante di Rappresentazione per ar:
1) Presentazione (o forma dei valori): 
		{t,k,[id1/D1,...,idn/Dn],C,v} per k>=0
2) AF(c) = {AF(t),k,AF(e),AF(C),AF(v)} if c = AR(t,k,e,C,v) 
3) I(AR(t,k,e,C,v)) = k>=0
*)

exception NegativeAddrNotAdmittedinAR of int;;

let mkAR5 h k e cl r = 
	if k>=0 then AR(h,k,e,cl,r) else raise (NegativeAddrNotAdmittedinAR(k));;

(* ================================================================================ *)
(* AR: Operations for presentation and Components Selection *)
let toStringAR (AR(h,s,f,c,r)) = 			(* una presentazione di AR *)
			let hString = (match h with 
							 Name ide -> toStringI ide 
							 |Cmd -> "[cmd]"
							 |Exp-> "[exp]"
							 |NoneH -> "[N]"
							 |IPB -> "[IB]"
							 ) and 
				rString = (match r with 
							Some(aval) -> toStringAval aval 
							| _ -> "[N]")
							in 
			"{" ^ hString ^ "," ^ (string_of_int s) ^ "," ^ (toStringEnv f) ^
			   "," ^ ":cmdNext:" ^ "," ^ rString ^ "}"
			;;

let head (AR(h,_,_,_,_)) = h;;
let frame (AR(_,_,e,_,_)) = e;; 
let cnt (AR(_,_,_,c,_)) = c;;
let res (AR(_,_,_,_,v)) = v;;



(* ================================================================================ *)
(* AR Stack *)
(* ================================================================================ *)

type stack = Stack of ar list;;
exception EmptyStack;;
exception InvalidUseOfStack of string * stack * int;;
exception InvalidInvocationReturnedValue;;

(* ================================================================================ *)
(* Funzione di Astrazione ed Invariante di Rappresentazione per Stack:
1) Presentazione (o forma dei valori): >ar1,...,ark] per k>=0
2) AF(c) = >] if c = Stack [] 
   AF(c) = >ar1,...,ark] 
   		 iff c = Stack ars and length ars = k and k>0 and
   		    (hd ars) = ar1 and AF(pop(c)) = >ar2,...,ark] 
3) I(c) = true *)
(* ================================================================================ *)

let emptyStack () = (*Crea uno stack vuoto*)
		Stack []
		;;
let push (Stack ars) ar = (*Aggiunge un AR al top dello stack*)
		Stack(ar::ars)
        ;;
let top (Stack ars) = (*Restistuisce il top dello stack, se esite*)
		match ars with
			| [] -> raise EmptyStack
			| ar::_ -> ar
		;;
let pop (Stack ars) = (*Rimuove il top dello stack, se esite*)
		match ars with 
			| [] -> raise EmptyStack
			| _::rest -> Stack rest
		;;
let sizeS (Stack ars) = (*Calcola il numero di AR presenti*)
		List.length ars
		;;
let rec popk ((Stack ars)as sk) k = (*Rimuove i k AR top dello stack*)
		match (ars,k) with 
			|(_::_,0) -> sk
			|(_::rest,n) when n>0 -> popk(Stack rest) (n-1)
			|_ -> raise (InvalidUseOfStack("popk: ", sk, k))
		;;	
let bindS (Stack ars) ide dval = (*Aggiunge il binding [ide/dval] all'AR top dello stack*) 
		match ars with
			| AR(t,k,env,cl,r)::rest -> Stack (AR (t,k,bind env ide dval,cl,r)::rest)
			| _ -> raise EmptyStack
		;;

(* Scope Statico/Dinamico a controllo valore parametro static ====================== *)
let rec getS ((Stack ars)as sk) ide = (*Restituisce il binding in accordo alla catina statica*)
    match ars with
		| AR(_,_,env,_,_)::rest
		    when (defined env ide) -> getEnv env ide
		| AR(_,k,_,_,_)::rest 
		    when k>0 -> 
		    let sk1 = popk sk k in getS sk1 ide
		| _ -> raise (UnboundIde ("staticGetS",ide))
		;;
let getR (Stack ars) = (*Restituisce il valore val dello AR top dello stack, se possibile*) 
		match ars with
			| AR(_,_,_,_,Some v)::rest -> v
			| _ -> raise (InvalidInvocationReturnedValue)
		;;
let resetC((Stack ars)as sk) cl = (*Reset della componente cont, nell'AR top dello stack, con
									continuazione fornita come parametro*)
		match ars with
			| AR(t,k,env,_,retV)::rest -> Stack(AR(t,k,env,cl,retV)::rest)
			| _ -> raise (InvalidUseOfStack("resetC: ", sk, 0))
		;;
let resetR((Stack ars)as sk) retV = (*Reset della componente val, nell'AR top dello stack, con
									valore fornito come parametro*)
		match ars with
			| AR(t,k,env,cl,_)::rest -> Stack(AR(t,k,env,cl,retV)::rest)
			| _ -> raise (InvalidUseOfStack("resetR: ", sk, 0))
		;;
let resetCR((Stack ars)as sk) cl retV = (*Combinazione di resetC e resetR*)
		match ars with
			| AR(t,k,env,_,_)::rest -> Stack(AR(t,k,env,cl,retV)::rest)
			| _ -> raise (InvalidUseOfStack("resetCR: ", sk, 0))
		;;
let getCode((Stack ars)as sk) = (*Restituisce la continuation dall'AR top dello stack, se possibile*) 
		match ars with    (******** added 11.05.21 ********)
			| AR(_,_,_,cnt,_)::rest -> cnt					
			| _ -> raise (InvalidUseOfStack("getCode: ", sk, 0))
		;;
let getH((Stack ars)as sk) = (*Restituisce l'head dall'AR top dello stack, se possibile*)
		match ars with       (******** added 21.05.21 ********)
			| AR(h,_,_,_,_)::rest -> h					
			| _ -> raise (InvalidUseOfStack("getHead: ", sk, 0))
		;;
let addCode((Stack ars)as sk) cmd = (*Aggiunge cmd in testa alla continuation dall'AR top dello stack, se possibile*)
		match ars with
			| AR(t,k,env,cl,r)::rest -> Stack(AR(t,k,env,cmd::cl,r)::rest)
			| _ -> raise (InvalidUseOfStack("addCode: ", sk, 0))
		;;

(* Operations for Stack presentation *)

let toStringStack(Stack ars) = 		(* una presentazione di AR *)
		let tabFun = fun x ac -> x ^ "\n" ^ ac in
		">" ^ (fold_right tabFun (List.map (toStringAR) ars) "]")
		;;

let printStack(stack) = 		        (* dump di Stack *)
		printf "\n%s\n\n" (toStringStack stack)
		;;


(*********** State: A pair (stack,store) -- Operation for state presentation *)

let toStringState (stack,store) =       (* una presentazione dello Stato *)
		let s1 = "\nStack: " ^ (toStringStack stack) in
		let s2 = "\nStore: " ^ (toStringStore store) in
		s1 ^ s2 ^ "\n==================================================="
		;;

let showState (stack,store) =           (* dump dello stato *)
		printf "\nStack:\n%s\nStore:\n%s\n\n" (toStringStack stack) 
											  (toStringStore store) 
		;;

let sigma0 = 
	let ar0 = mkAR5 NoneH 0 (emptyEnv()) [] None in 
	let sk0 = push (emptyStack()) ar0 in
	let mu0 = emptyStore() in
	(sk0,mu0)
		;;

(* ============================================================================ *)
(* The Semantic Function: dclSem, expSem, cmdSem, progSem *)
exception TypeErrorI of string * ide;;
exception SystemErrorE of string;; 
exception SystemErrorM of string;;
exception SystemError of string;;
exception SystemErrorC of string * cmd;;		(******** added 11.05.21 ********)
exception TypeErrorC of string * cmd;;			(******** added 11.05.21 ********)
exception SystemErrorS of string * stm;;		(******** added 11.05.21 ********)
exception TypeErrorS of string * stm;;			(******** added 11.05.21 ********)
exception TypeErrorE of string * exp;;		    (******** added 11.05.21 ********)
exception StaticErrorE of string * exp;;		(******** added 11.05.21 ********)
exception TypeErrorP of string;;		        (******** added 13.05.21 ********)
exception StaticErrorS of string * stm;;		(******** added 21.05.21 ********)
exception TypeErrorT of string;;		        (******** added 21.05.21 ********)


let declared sk ide = (*Dice se ide è definito nel frame top di sk*)
	let frame0 = frame(top sk)
	in defined frame0 ide
	;;

let ysame t1 t2 = t1 = t2
    ;;
    
let eTOm = function (*Convertitore da aval in mval******** added 04.05.21 ********)
	Ival n -> MvalI n
	| Bval b -> MvalB b
	| _ -> raise(SystemErrorE("eTOm"))
	;;
	
let mTOe = function	(*Convertitore da mval in aval******** added 04.05.21 ********)
	MvalI n -> Ival n
	| MvalB b -> Bval b
	| _ -> raise(SystemErrorM("mTOe"))
	;;

let aTOe = function	(*Convertitore da aval in exp******** added 21.05.21 ********)
	Ival n -> N n
	| Bval b -> B b
	| Null  -> EE
	;;
	
let eTOa = function	(*Convertitore da exp a aval******** added 21.05.21 ********)
	N n -> Ival n
	| B b -> Bval b
	| EE -> Null
	| _ -> raise(SystemErrorM("eTOa"))
	;;

let rec getSize	= function					(******** added 04.05.21 ********)
	Int -> 1
	| Bool -> 1
	| Lab -> 1
	| Void -> 0
	| Mut tye -> getSize tye
	| Arr (tye,num) -> num
	| Terr -> 0
	| Abs (tye,tyeSeq) -> 0
	| Unit -> 0
	;;


(* ============================================================================ *)
(* The Semantic Function: dclSem *)


let rec dclSem dcl (sk,(Store(d,g)as mu)) =
	match dcl with
	  |Const(ty,ide,exp) -> 
		    (match expSem exp (sk,mu) with
			   |(te,v,(sk1,mu1)) 
			   	  	  when (isSimple ty) && (ysame ty te) && 
			   	  	  		not(declared sk ide)
			   	  	  -> (let den = DConst(te,v) in
			   	  	      let sk2 = bindS sk1 ide den in
			   	  	      let st2 = (sk2,mu1) in
			   	  	      (Void,st2))
			   	|(_,_,(sk1,_))
			   		   when (declared sk1 ide) 
			   		   -> raise(TypeErrorI("E3: dclSem",ide))
			   	|(te,_,_)
			   		   when (isSimple te) 
			   		   -> raise(TypeErrorI("E2: dclSem",ide))
			   	|_ -> raise(TypeErrorI("E1: dclSem",ide)))
		| Var(ty,ide,exp) -> 
		    (match expSem exp (sk,mu) with
			   |(te,v,(sk,mue)) 
			   	  	  when (isSimple ty) &&        (***** checked 10.05.21 ******)
			   	  	  	   (te = Unit || ysame ty te) && 
			   	  	  	   not(declared sk ide)
			   	  	  -> (let (loca,mua) = allocate mue 1 in
			   	  	      let den = DVar(Mut ty,loca) in
			   	  	      let skF = bindS sk ide den in
			   	  	      let muF = if(v = Null) then mua   (** checked 23.05.21 **)
			   	  	      	  		else upd mua loca (eTOm v) in
			   	  	      (Void,(skF,muF)))
			   	|_ 	  when (declared sk ide) 
			   	  	  -> raise(TypeErrorI("E6: dclSem",ide))
			   	|(te,_,_)
			   		  when (te != Unit) 
			   	  	  -> raise(TypeErrorI("E4: dclSem",ide))
			   	|_ -> raise(TypeErrorI("E5: dclSem",ide)))
		| Array(ty,ide) ->
		    (match ty with
		    	| Arr(t1,num)		    
						when (isSimple t1) && (num > 0) && not(declared sk ide)
	    	   			-> (let (loca,muF) = allocate mu num in
	    	   	   			let den = DArry(Arr(Mut t1,num),loca) in
	    		   			let skF = bindS sk ide den in
	    		   			(Void,(skF,muF)))
				| Arr(t1,_) 
				       when not(isSimple t1)
				       -> raise(TypeErrorI("E7: dclSem",ide))
				| Arr(_,num) 
				       when (num < 0 || num = 0)
				       -> raise(TypeErrorI("E8: dclSem",ide))
				| Arr(_,_) 
				       when declared sk ide
				       -> raise(TypeErrorI("E9: dclSem",ide))
				| _ -> raise(SystemError("MultidimensionArray?: dclSem")))
		| SeqD(d1,d2) -> 
			(let (t1,sg1) = dclSem d1 (sk,mu) in
			 dclSem d2 sg1)
		| Pcd(ty,ide,fpars,blockP) ->
			(match (declared sk ide,fpars,blockP) with
				| (_,_,_)
				       when not(isSimple ty) && (ty <> Void)
					   -> raise(TypeErrorI("E12: dclSem",ide))
				| (true,_,_) -> raise(TypeErrorI("E14: dclSem",ide))
				| (_,FP(pf,t,ide),_)
						when not(isConstant pf) && not(isSimple t)
						-> raise(TypeErrorI("E13: dclSem",ide))
				| (_,FP(pf,t,ide),_)
						when isConstant pf && not(isSimple t || isArr t)
						-> (match t with
								| Arr(t1,_)
									when not(isSimple t1)
									-> raise(TypeErrorI("E13.2: dclSem",ide))
								| Arr(_,_)
									-> raise(TypeErrorI("E13.3: dclSem",ide))
								| _ -> raise(TypeErrorI("E13.1: dclSem",ide)) 							
							)
				| (_,FP(pf,t,ide),_)
						when isConstant pf && not(isSimple t || isArr t)
						-> raise(TypeErrorI("E13.1: dclSem",ide))				
				| (_,FP(pf,t,_),BlockP(d,s))				(* checked 21.05.21 *)
						-> (let tr = Abs(ty,[t]) in
						    let clos = ClosT(ide,tr,fpars,d,s,sizeS sk) in
						    let den = DAbs(tr,clos) in
						    let skF = bindS sk ide den in
						    (Void,(skF,mu)))
				| (_,EFP,BlockP(d,s))
						-> (let tr = Abs(ty,[]) in
						    let clos = ClosT(ide,ty,EFP,d,s,sizeS sk) in
						    let den = DAbs(tr,clos) in
						    let skF = bindS sk ide den in
						    (Void,(skF,mu))))
		| ED -> (Void,(sk,mu))
		and 

isConstant = (function
		| Constant -> true
		| _ -> false)
	   and

(* ============================================================================ *)
(* The Semantic Function: expSem *)

expSem exp (sk,(Store(d,g)as mu)) = 
	match exp with
	  | N n -> 
	 	 (Int,Ival n,(sk,mu))
	  | B b -> 
	 	 (Bool,Bval b,(sk,mu))
	  | EE -> 
	 	 (Unit,Null,(sk,mu))         (**** corretto 21.05.21  ****)
	  | Val ide -> 
	  	 (match getS sk ide with
		    | DVar(Mut t,loct) when (isSimple t) 
			    -> let vt = mTOe(getStore mu loct) in
			   	        (t,vt,(sk,mu))
		    | DVar(Mut t,loct)
		       	-> raise(TypeErrorE("E16: expSem - ", exp))
		    | DConst(t,v) -> 
		   		(t,v,(sk,mu))
		   	|_ -> 
		   		let msg = "UnExpected binding in Env" in
		   	    raise(SystemErrorE("expSem: "^msg)))
	  | Plus(e1,e2) -> 
	 	 (let (t1,v1,sg1) = expSem e1 (sk,mu) in 
	 	  let (t2,v2,sg2) = expSem e2 sg1 in
	 	  match (t1,t2,v1,v2) with
	 	  	| (Int,Int,Ival n1,Ival n2) 
	 	  		  -> (Int,Ival(n1+n2),sg2)
	 	  	| (_,Int,_,Ival n2)
	 	  		  -> raise(TypeErrorE("E25: expSem - ",exp))
	 	  	| _   -> raise(TypeErrorE("E26: expSem - ",exp))
	 	 	)
	  | Minus(e1,e2) -> 
	 	 (let (t1,v1,sg1) = expSem e1 (sk,mu) in 
	 	  let (t2,v2,sg2) = expSem e2 sg1 in
	 	  match (t1,t2,v1,v2) with
	 	  	| (Int,Int,Ival n1,Ival n2) 
	 	  		  -> (Int,Ival(n1-n2),sg2)
	 	  	| (_,Int,_,_)
	 	  		  -> raise(TypeErrorE("E25: expSem - ",exp))
	 	  	| _   -> raise(TypeErrorE("E26: expSem - ",exp))
	 	 	)
	  | Eq(e1,e2) -> 
	 	 (let (t1,v1,sg1) = expSem e1 (sk,mu) in 
	 	  let (t2,v2,sg2) = expSem e2 sg1 in
	 	  match (t1,t2,v1,v2) with
	 	  	| _   when (ysame t1 t2) && (isSimple t1)
	 	  		  -> (Bool,compA (=) v1 v2,sg2)
	 	  	| _   when (isSimple t1)
	 	  		  -> raise(TypeErrorE("E26: expSem - ",exp))
	 	  	| _   -> raise(TypeErrorE("E25: expSem - ",exp)) 
	 	 	)
	  | LT(e1,e2) -> 
	 	 (let (t1,v1,sg1) = expSem e1 (sk,mu) in 
	 	  let (t2,v2,sg2) = expSem e2 sg1 in
	 	  match (t1,t2,v1,v2) with
	 	  	| (_,_,Ival n1,Ival n2) 
	 	  		  -> (Bool,compI (<) n1 n2,sg2)
	 	  	| (Int,_,_,_)
	 	  		  -> raise(TypeErrorE("E26: expSem - ",exp))
	 	  	| _   -> raise(TypeErrorE("E25: expSem - ",exp))
	 	 	)
	  | GT(e1,e2) -> 
	 	 (let (t1,v1,sg1) = expSem e1 (sk,mu) in 
	 	  let (t2,v2,sg2) = expSem e2 sg1 in
	 	  match (t1,t2,v1,v2) with
	 	  	| (_,_,Ival n1,Ival n2) 
	 	  		  -> (Bool,compI (>) n1 n2,sg2)
	 	  	| (Int,_,_,_)
	 	  		  -> raise(TypeErrorE("E26: expSem - ",exp))
	 	  	| _   -> raise(TypeErrorE("E25: expSem - ",exp))
	 	 	)
	  | Or(e1,e2) -> 
	 	 (let (t1,v1,sg1) = expSem e1 (sk,mu) in 
	 	  let (t2,v2,sg2) = expSem e2 sg1 in
	 	  match (t1,t2,v1,v2) with
	 	  	| (_,_,Bval b1,Bval b2) 
	 	  		  -> (Bool,compB (||) (toB b1) (toB b2),sg2)
	 	  	| (Bool,_,_,_)
	 	  		  -> raise(TypeErrorE("E26: expSem - ",exp))
	 	  	| _   -> raise(TypeErrorE("E25: expSem - ",exp))
	 	 	)
      | Apply(ide,aps)
          ->(match getS sk ide with
                 (* include check for E33 *)
               | DAbs(Abs(tr,aa),ClosT(_,_,fps,dcl,sts,k)) (* no overloading *)
               		when (isSimple tr)
                    ->(let ar = mkAR5 (Name ide) ((sizeS sk)-k+1) 
                                      (emptyEnv()) [] None in
                       let skc = push sk ar in 
                       let (sgR,epiR) = tr1Fun fps aps sk skc mu in
                                    (* include check for E34 *)
                       let (_,(ar2sk2,mu2)) = dclSem dcl sgR in
                       let ar3sk2 = resetC ar2sk2 [UnL sts] in 
                       let (_,(sk4,mu4)) = nextCmd(ar3sk2,mu2) in
                       let sgF = (pop sk4,mu4) in
                       (tr,getR sk4,sgF))
               | DAbs(Abs(tr,aa),ClosT(_,_,fps,dcl,sts,k)) 
               	    -> raise(TypeErrorE("E34.1: expSem",Apply(ide,aps)))
               |  _ -> raise(TypeErrorE("E32: expSem",Apply(ide,aps))))      
	  | Arrow1(ide,exp) -> 
	   	  (match getS sk ide with
		    | DArry(Arr(Mut tr,n),u) 
		       	when isSimple tr  
		       	-> (match expSem exp (sk,mu) with
		       	     |(Int,Ival n1,sgr)
		       	           when (n1 >= 0) && (n1 < n)
		       	           -> (let Loc k = u in
		       	               let loc1 = Loc(k+n1) in
		       	               let (skr,mur) = sgr in
		       	               let vr = mTOe(getStore mur loc1) in
		       	               (tr,vr,sgr))
                     |(t,n1,sgr)
		       	           when not(ysame t Int)
		       	           -> raise(TypeErrorE("E30: expSem - ",exp))
                     |_ -> raise(TypeErrorE("E31: expSem - ",exp)))
		    | DArry(Arr(Mut tr,n),u) 
		       	-> raise(TypeErrorE("E30: expSem: Simple Type? - ",exp)) 
		    | _ 
		       	-> raise(TypeErrorE("E28: expSem: - ",exp))) 
	and

compA g v1 v2 =
	if g v1 v2 then Bval True
	else Bval False
   
   and
compI g v1 v2 = 
	if g v1 v2 then Bval True
	else Bval False
   
   and
compB g v1 v2 = 
	if g v1 v2 then Bval True
	else Bval False
   
   and

toB = function True -> true | _ -> false   
   and
   
dexpSem dexp (sk,(Store(d,g)as mu)) =
	match dexp with
	  | Val ide -> (
	   	  match getS sk ide with
		    | DVar(Mut t,loct) 
		       	when isSimple t 
		       	-> (Mut t,loct,(sk,mu))		
            | DVar(Mut t,loct)
		       	-> raise(TypeErrorE("E16: dexpSem - ",dexp))
			| DArry(Arr(Mut tr,n), loct)
				when (isSimple tr) && (n>0)
				-> (Arr(Mut tr,n), loct, (sk,mu))
			| DArry(Arr(Mut tr,n), loct)
				when not(isSimple tr)
				-> raise(TypeErrorE("E16.1: dexpSem - ",dexp))
			| DArry(Arr(Mut tr,n), loct)
				-> raise(TypeErrorE("E16.2: dexpSem - ",dexp))
		    | _  
		    	-> raise(TypeErrorE("E17: dexpSem - ",dexp)))
	  | Arrow1(ide,exp) -> 
	   	  (match getS sk ide with
		    | DArry(Arr(Mut tr,n),u) 
		       	when isSimple tr  
		       	-> (match expSem exp (sk,mu)	with
		       	     |(Int,Ival n1,sgr)
		       	           when (n1 >= 0) && (n1 < n)
		       	           -> let Loc k = u in
		       	              let locr = Loc(k+n1) in
		       	              (Mut tr,locr,sgr)
                     |(t,n1,sgr)
		       	           when not(ysame t Int)
		       	           -> raise(TypeErrorE("E30.1: dexpSem - ",dexp))
                     |_ -> raise(TypeErrorE("E31.1: dexpSem - ",dexp)))
		    | DArry(Arr(Mut tr,n),u) 
		       	-> raise(TypeErrorE("E30.1: dexpSem: Simple Type? - ",dexp)) 
		    | _ 
		       	-> raise(TypeErrorE("E28.1: dexpSem: - ",dexp))) 
	  |_ -> 
	  		let msg = (toStringExp dexp) ^ " is Mutable? " in 
	        raise(SystemErrorE("dexp: " ^ msg))
	and



(* ============================================================================ *)
(* The Semantic Function: cmdSem *)

cmdSem cmd (sk,(Store(d,g)as mu)) = 
	match cmd with
	  | LabC(lab,stm) ->( 
	  	  match(top sk) with
	  	  	| _ when (sizeS sk = 1) && 
	  	  			 not(declared sk lab)
	  	  		-> let cnt = getCode sk in
	  	  		   let c1 = pack (UnL stm) cnt in        (*** checked15.05.21 ***)
	  	  		   let den = DLab(Lab,c1) in
	  	  		   let sk1 = bindS sk lab den in
	  	  		   let sg1 = (sk1,mu) in 
	  	  		   stmSem stm sg1
	  	  	| _ when (sizeS sk != 1)
	  	  	    -> raise(TypeErrorC("E35: cmdSem",cmd))
	  	  	| _ when (declared sk lab)
	  	  	    -> stmSem stm (sk,mu)  					 (*** checked15.05.21 ***)
	  	  	| _ -> let msg = "cmdSem: Configurazione Inattesa" in
	  	  	       raise(SystemError msg))
	  | UnL stm -> 
	  		stmSem stm (sk,mu)
	  | SeqC(cmd1,cmd2) ->(					
	  	  match(top sk) with
	  	  	| _  when (sizeS sk = 1)
	  	  		 -> let sk1 = addCode sk cmd2 in
	  	  		    let sg1 = (sk1,mu) in 
	  	  		    cmdSem cmd1 sg1
	  	  	| _ when (sizeS sk != 1)
	  	  	    -> raise(TypeErrorC("E56: cmdSem",cmd))
	  	  	| _ -> let msg = "cmdSem: Configurazione Inattesa" in
	  	  	       raise(SystemError msg))
	  and

stmSem stm (sk,(Store(d,g)as mu)) =
	match stm with
	  | Goto lab when (sizeS sk = 1) ->					 (*** checked15.05.21 ***)
	  	  (match (top sk) with
	  	    | AR(_,_,env,_,_) 
	  	    	when defined env lab 
	  	    	-> (match getEnv env lab with 
	  	    	     | DLab(Lab,cmd) ->(
	  	    	    	  let sk1 = resetC sk [] in
	  	    	    	  cmdSem cmd (sk1,mu))
	  	    	    | _ -> raise(TypeErrorS("E43: stmSem",stm)))
	  	   | _ -> (match jac lab (getCode sk) with
	  	   			| (Some (LabC(_,stm)),cnt) ->(
	  	    	    	  let sk1 = resetC sk cnt in
	  	  		          let c1 = pack (UnL stm) cnt in    
	  	  		          let den = DLab(Lab,c1) in
	  	  		          let sk2 = bindS sk1 lab den in
	  	   				  stmSem stm (sk2,mu))
	  	   			|_ -> raise(TypeErrorS("E43: stmSem",stm))))
	  | Goto lab -> raise(TypeErrorS("E45: stmSem",stm))    
	  | IfT(exp,stm) ->
		 (match expSem exp (sk,mu) with
			|(Bool,Bval True,sg1) -> stmSem stm sg1
			|(Bool,_,sg1) -> (Void,sg1)
			|_ -> raise(TypeErrorS("E41: stmSem",stm)))
	  | Upd(dexp,exp) ->
		 (let (tr,vr,sgr) = expSem exp (sk,mu) in
		  let (tl,locl,(skl,mul)) = dexpSem dexp sgr in
		 	(match tl with
				| (Mut t) when (isSimple t) && (ysame t tr)
					  	  -> let muF = upd mul locl (eTOm vr) in     (** 23.05.21 **)
					         (Void,(skl,muF))
			    | (Mut t) when (t != tr)
                          -> raise(TypeErrorS("E39: stmSem",stm))
			    | (Mut t) (* not(isSimple t) *)
                          -> raise(TypeErrorS("E39: stmSem",stm))
			    | _  -> raise(TypeErrorS("E38: stmSem",stm))))			
	  | ES -> (Void,(sk,mu))
	  | BlockS (dcl,stm) 
	      -> (let g = (function        (*** checked 21.05.21 ***)
	      			|Name _ -> IPB
	      			|IPB ->  IPB
	      			|_ -> NoneH) in
	      	  let h = getH sk in
	          let artop = mkAR5 (g h) 1 (emptyEnv()) [] None in 
	          let sk1 = push sk artop in 
	          match dclSem dcl (sk1,mu) with
	            | (Void,(sk2,mu2)) 
	                ->(let sk3  = resetC sk2 [UnL stm] in
	                   try(let(_,(sk4,mu4)) = nextCmd(sk3,mu2) in
	                       let sgF = (pop sk4,mu4) in
	                       (Void,sgF))
	                   with | TypeErrorC("104:nextCmd",_)
	                            -> raise(TypeErrorS("E52: stmSem",stm)))
	            |_ -> raise(TypeErrorS("E51: stmSem",stm)))
      | Call (ide,aps)
          ->( match getS sk ide with
                 (* include check for E46 *)
               | DAbs(Abs(Void,aa),ClosT(_,_,fps,dcl,sts,k)) (* no overloading *)
                    ->(let ar = mkAR5 (Name ide) ((sizeS sk)-k+1) 
                                      (emptyEnv()) [] None in
                       let skc = push sk ar in 
                       let (sgR,epiR) = tr1Fun fps aps sk skc mu in
                                    (* include check for E47 *)
                       let (_,(ar2sk2,mu2)) = dclSem dcl sgR in
                       let ar3sk2 = resetC ar2sk2 [UnL sts] in 
                       let (_,(sk4,mu4)) = nextCmd(ar3sk2,mu2) in
                       let sgF = (pop sk4,mu4) in
                       (Void,sgF))
               |  _ -> raise(TypeErrorS("E45: stmSem",stm)))      
      | Return exp
      	  ->(match getH sk with
               | Name ide    (* include check for E64 *)
                  ->(match (getS sk ide , expSem exp (sk,mu)) with  
                       | (DAbs(Abs(t,ts),_),(te,ve,(ske,mue)))  
                             when (t = te) || (t = Void && te = Unit)             
               		         -> let skF = resetCR ske [] (Some ve) in
                                (Void,(skF,mue)) 
                       | _ -> raise(TypeErrorS("E65: stmSem",stm)))                       
               | IPB 
          		 ->(let (te,ve,(ske,mue)) = expSem exp (sk,mu) in  
                    let ar1 = top (resetC ske []) in    (* include check for E64 *)
                    let cmd = UnL(Return (aTOe ve)) in
                    let ar2 = top (resetC (pop ske) [cmd])  in
                    let skr1 = pop(pop ske) in
                    let sgF = (push (push skr1 ar2) ar1,mue) in
                    (Void,sgF))
               | _  
                 -> raise(StaticErrorS("Wrong Use of Return: stmSem",stm)))       
	  | SeqS(stm1,stm2) 
          ->(let sk1 = addCode sk (UnL stm2) in     (* include check for E67 *)
	  	  	 let sg1 = (sk1,mu) in 
	  	  	 stmSem stm1 sg1)			(* include check for E66 *)
	  and
 
tr1Fun fps aps sk skc mu =
   match (fps,aps) with
     | (FP(Value,t,ide),AP exp) 
          when (isSimple t) && not(declared skc ide)
          -> (match expSem exp (sk,mu) with
               | (ta,va,(sk1,mu1))
            	    when ysame t ta 
             	    -> (let (loct,mu2) = allocate mu1 1 in
                        let mu3 = upd mu2 loct (eTOm va) in
                        let den = DVar(Mut t,loct) in
                        let skcF = bindS skc ide den in
                        let sgr = (skcF,mu3) and epir = [] in
                        (sgr,epir))
               | _ -> raise(TypeErrorT("E61.1: Trasmission: Type not expected"))) 
     | (FP(Value,_,ide),AP _) 
          when declared skc ide
          -> raise(TypeErrorT("E61: Trasmission: Declared ide"))
     | (FP(Value,t,_),AP _) 
          -> raise(TypeErrorT("E61.2: Trasmission: Type not expected"))
     | (FP(Ref,t,ide),AP exp)
          when (isSimple t) && not(declared skc ide)
          -> (match dexpSem exp (sk,mu) with
               | (Mut ta,loca,(sk1,mu1))
            	    when ysame t ta 
             	    -> (let den = DVar(Mut ta,loca) in
                        let skcF = bindS skc ide den in
                        let sgr = (skcF,mu1) and epir = [] in
                        (sgr,epir))
               | _ -> raise(TypeErrorT("E61.1: Trasmission: Types Mismatch"))) 
     | (FP(Ref,_,ide),AP _) 
          when declared skc ide
          -> raise(TypeErrorT("E61: Trasmission: Declared ide"))
     | (FP(Ref,t,_),AP _)
          -> raise(TypeErrorT("E61.2: Trasmission: Type not expected"))
	 | (FP(Constant,t,ide),AP exp) 
          when (isSimple t) && not(declared skc ide)
          -> (match expSem exp (sk,mu) with
               | (ta,va,(sk1,mu1))
            	    when ysame t ta 
             	    -> (let den = DConst(t,va) in
                        let skcF = bindS skc ide den in
                        let sgr = (skcF,mu1) and epir = [] in
                        (sgr,epir))
               | _ -> raise(TypeErrorT("E61.1: Trasmission: Types Mismatch")))
	 | (FP(Constant,t,ide),AP exp) 
          when (isArr t) && not(declared skc ide)
          -> (match dexpSem exp (sk,mu) with
               | (Arr(Mut tr,n),loca,(sk1,mu1))
            	    when ysame t (Arr(tr,n))
             	    -> (let den = DArry(Arr(Mut tr,n),loca) in
                        let skcF = bindS skc ide den in
                        let sgr = (skcF,mu1) and epir = [] in
                        (sgr,epir))
               | _ -> raise(TypeErrorT("E61.1: Trasmission: Types Mismatch"))) 
	 | (FP(Constant,_,ide),AP _) 
          when declared skc ide
          -> raise(TypeErrorT("E61: Trasmission: Declared ide"))
     | (FP(Constant,t,_),AP _)
          -> raise(TypeErrorT("E61.3: Trasmission: Type not expected"))	  
     | (EFP,EAP) 
          -> let sgr =(skc,mu) and epir = [] in
             (sgr,epir) 
     | (EFP,_) 
          -> raise(TypeErrorT("E62: Trasmission: Too Many Params"))
     | (_,EAP) 
     	  -> raise(TypeErrorT("E63: Trasmission: Too Few Params"))     
      and	  
	       
jac lab cnt = 
	let rec find lab cmd = 
	   match cmd with
		 | LabC(lab1,cmd1) 
		 		when (lab = lab1) 
		 		-> (Some cmd,[])
		 | LabC(lab1,cmd1) 
		 		-> (None,[])
		 | UnL stm
		 		-> (None,[])
		 | SeqC(cmd1,cmd2)
		 		-> (match find lab cmd1 with
		 			 |(None,_) -> find lab cmd2
		 			 |(cmdO,[]) -> (cmdO,[cmd2])
		 			 |(cmdO,[cmd]) -> (cmdO,[cmd;cmd2])
		 			 |_ ->  raise(SystemError 
		 			 		"find: check for errors in find"))
		in
   match cnt with		 		 
	 | [] -> (None,[])
	 | cmd::cmds -> (
		 match find lab cmd with
		   |(None,_) -> jac lab cmds
		   |(Some cmd1,cmdR) -> (Some cmd1,cmdR @ cmds))
	  and
  
flat cnt = (* applicabile solo se cnt non vuoto *)
   match cnt with
	 | cmd::[] -> cmd
	 | cmd::cmds -> SeqC(cmd,flat cmds)
	 | _ ->  raise(SystemError "flat: wrong use of flat")
	 and

pack cmd = function
	| []-> cmd
	| cnt -> SeqC(cmd,flat cnt)
	and

(* ============================================================================ *)
(* The Semantic Function: progSem *)

progSem (Prog(ide,Block(dcl,cmd))) =
		let (_,(sk1,mu1)) = dclSem dcl sigma0 in
		let sk2 = initNCR sk1 (Name ide) [cmd] None in
		startAndstop nextCmd (sk2,mu1)
	and		

initNCR ((Stack ars)as sk) nm cl retV = 
   match ars with
	 | AR(_,k,env,_,_)::rest -> Stack(AR(nm,k,env,cl,retV)::rest)
     | _ -> raise (InvalidUseOfStack("InitNCR: ", sk, 0))

	and

(* ================================================================================ *)
(* Continuation Selection and Execution for Commands                                *)
(* ================================================================================ *)
		
nextCmd (Stack ars,mu) =( 
	match ars with
	  | AR(_,_,_,[],_)::rest 
	  		   -> (Void,(Stack ars,mu))
	  | AR(t,k,env,c::cc,r)::rest -> 
		  let skN = Stack(AR(t,k,env,cc,r)::rest) in 
		  let stN = (skN,mu) in 
		  (match (cmdSem c stN) with
		  	  |(Void,stM) -> trace nextCmd stM
			  |_ -> raise(TypeErrorC("104:nextCmd",c)))
	  | _ -> 
	      raise (InvalidUseOfStack("nextCmd: Stack is empty", Stack ars, 0))) 
    and

trace g st =
	let h () = g st in
	if tracer then h (showState st)
	else g st
	and

startAndstop g st =
	let (_,stF) = trace g st and
	    h() = printf "\n%s\n" "SUCCESSFUL_TERMINATION" in  
	if tracer then h()
	else h(showState stF)
	;;

(* ============================================================================ *)
(* Tests*)

(*Test di double*)

let d1 = Var(Int, "a", EE);;
let dpcd = Pcd(Int, "double", (FP(Constant, Int, "x")),BlockP(ED,Return (Plus (Val "x", Val "x"))));;
let dclseq = SeqD(d1,dpcd);;
let c1 = UnL(Upd(Val "a", N 10));;
let c2 = UnL(Upd(Val "a", Apply("double",AP (Val "a"))));;
let cmdseq = SeqC(c1,c2);;
let doubleTest = Prog("doubleTest",Block(dclseq, cmdseq));;


(*Test di arraySwap*)

let d1 = Array(Arr (Int, 2),"a");;
let d2 = SeqD(Var(Int, "temp", EE),ED);;
let stmpcd1 = Upd(Val "temp" , Arrow1("v", N 0));;
let stmpcd2 = Upd(Arrow1("v", N 0), Arrow1("v", N 1));;
let stmpcd3 = Upd(Arrow1("v", N 1), Val "temp");;
let stmpcdseq = SeqS(SeqS(stmpcd1,stmpcd2),stmpcd3);;
let dpcd = Pcd(Void, "swap", (FP(Constant, Arr (Int, 2), "v")), BlockP(d2,stmpcdseq));;
let dclseq = SeqD(d1,dpcd);;
let c1 = UnL(Upd(Arrow1("a", N 0), N 1));;
let c2 = UnL(Upd(Arrow1("a", N 1), N 2));;
let c3 = UnL(Call ("swap", AP (Val "a")));;
let cmdseq = SeqC(SeqC(c1,c2),c3);;
let arraySwap = Prog("arraySwap",Block(dclseq,cmdseq));;


(*Test di sort2*)

let d0 = Array(Arr (Int, 2),"a");;
let d1 = Array(Arr (Int, 2),"b");;
let d2 = SeqD(Var(Int, "temp", EE),ED);;
let stmpcd1 = Upd(Val "temp" , Arrow1("v", N 0));;
let stmpcd2 = Upd(Arrow1("v", N 0), Arrow1("v", N 1));;
let stmpcd3 = Upd(Arrow1("v", N 1), Val "temp");;
let stmpcdseq1 = SeqS(SeqS(stmpcd1,stmpcd2),stmpcd3);;
let dpcd1 = Pcd(Void, "swap", (FP(Constant, Arr (Int, 2), "v")), BlockP(d2,stmpcdseq1));;
let stmpcd4 = IfT(GT(Arrow1("w", N 0),Arrow1("w", N 1)),Call("swap",AP(Val "w")));;
let dpcd2 = Pcd(Void, "sort2", (FP(Constant, Arr (Int, 2), "w")), BlockP(ED,stmpcd4));;
let dclseq = SeqD(SeqD(SeqD(d0,d1),dpcd1),dpcd2);;
let c1 = UnL(Upd(Arrow1("a", N 0), N 1));;
let c2 = UnL(Upd(Arrow1("a", N 1), N 2));;
let c3 = UnL(Call ("sort2", AP (Val "a")));;
let c4 = UnL(Upd(Arrow1("b", N 0), N 7));;
let c5 = UnL(Upd(Arrow1("b", N 1), N 5));;
let c6 = UnL(Call ("sort2", AP (Val "b")));;
let cmdseq = SeqC(SeqC(SeqC(SeqC(SeqC(c1,c2),c3),c4),c5),c6);;
let sort2Test = Prog("sort2Test",Block(dclseq,cmdseq));;


(*Emulazione della trasmissione di più parametri omogenei*)

let d1 = Var(Int,"x",N 23);;
let d2 = Var(Int,"y",N 15);;
let d3 = Var(Int,"min",EE);;
let stmpcd1 = IfT(LT(Arrow1("v", N 0),Arrow1("v", N 1)),Return(Arrow1("v", N 0)));;
let stmpcd2 = Return(Arrow1("v", N 1));;
let dpcd = Pcd(Int, "minArray", (FP(Constant, Arr (Int, 2), "v")), BlockP(ED,SeqS(stmpcd1,stmpcd2)));;
let d4 = Array(Arr (Int, 2),"aux");;
let dclseq = SeqD(SeqD(SeqD(SeqD(d1,d2),d3),dpcd),d4);;
let c1 = UnL(Upd(Arrow1("aux", N 0), Val "x"));;
let c2 = UnL(Upd(Arrow1("aux", N 1), Val "y"));;
let c3 = UnL(Upd(Val "min",Apply("minArray",AP(Val "aux"))));;
let cmdseq = SeqC(SeqC(c1,c2),c3);;
let minTest = Prog("minTest",Block(dclseq,cmdseq));;


(*Esempio di modifica diretta del parametro formale nella trasmissione per costante*)

let d1 = Var(Int, "a", EE);;
let stmpcd1 = Upd(Val "x", Plus(Val "x", Val "x"));;
let stmpcd2 = Return (Val "x");;
let stmpcdseq = SeqS(stmpcd1,stmpcd2);;
let dpcd = Pcd(Int, "double", (FP(Constant, Int, "x")),BlockP(ED,stmpcdseq));;
let dclseq = SeqD(d1,dpcd);;
let c1 = UnL(Upd(Val "a", N 10));;
let c2 = UnL(Upd(Val "a", Apply("double",AP (Val "a"))));;
let cmdseq = SeqC(c1,c2);;
let doubleERRTest = Prog("doubleERRTest",Block(dclseq, cmdseq));;


(*Esempio di modifica indiretta del parametro formale nella trasmissione per costante*)

let d1 = Var(Int, "a", EE);;
let stmpcd1 = Upd(Val "z", Plus(Val "z", N 1));; 
let g = Pcd(Int, "g", (FP(Ref, Int, "z")),BlockP(ED,SeqS(stmpcd1,Return (Val "z"))));;
let dclpcd = Var(Int, "y",EE);;
let stmpcd2 = Upd(Val "y", Apply("g", AP(Val "x")));;
let stmpcd3 = Return(Plus(Val "y", Val "y"));;
let doublev2 = Pcd(Int, "doublev2", (FP(Constant, Int, "x")),BlockP(SeqD(dclpcd,ED),SeqS(stmpcd2,stmpcd3)));;
let dclseq = SeqD(SeqD(d1,g),doublev2);;
let c1 = UnL(Upd(Val "a", N 10));;
let c2 = UnL(Upd(Val "a", Apply("doublev2",AP (Val "a"))));;
let cmdseq = SeqC(c1,c2);;
let doublev2ERR = Prog("doublev2ERR",Block(dclseq, cmdseq));;


(*Esempio di errore E13.1*)

let dpcd = Pcd(Int, "double", (FP(Constant, Arr(Int, 0), "x")),BlockP(ED,Return (Val "x")));;
let err133 = Prog("err131", Block(dpcd, UnL(ES)));;


(*Esempio di errore E17*)

let d1 = Var(Int, "x", N 1);;
let d2 = Const(Int, "y", N 2);;
let stm1 = Upd(Val "y", Val "x");;
let err17 = Prog("err17", Block(SeqD(d1,d2), UnL(stm1)));;


