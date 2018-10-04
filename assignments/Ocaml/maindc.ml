include Scanner
include Bigint

open Bigint
open Printf
open Scanner

type stack_t = Bigint.bigint Stack.t
let push = Stack.push
let pop = Stack.pop

let ord thechar = int_of_char thechar
type binop_t = bigint -> bigint -> bigint

let rec print_rec number1 =
	let xlen = (String.length number1) in
	if xlen > 70 then
		(printf "%s%s\n%!" (String.sub number1 0 69) "\\";
		print_rec (String.sub number1 69 (xlen - 69)))
	else printf "%s\n%!" number1

let print_number number = print_rec (string_of_bigint number)

let print_stackempty () = eprintf "dc: stack empty\n%!"

let symbol_list = Array.make 1000 Bigint.zero
let executereg (thestack: stack_t) (oper: char) (reg: int) =
	try match oper with
	| 'l' -> push symbol_list.(reg) thestack
	| 's' -> symbol_list.(reg) <- pop thestack
	| _   -> printf "0%o 0%o is unimplemented\n%!" (ord oper) reg
	with Stack.Empty -> print_stackempty()

let executebinop (thestack: stack_t) (oper: binop_t) =
	try let right = pop thestack
		in  try let left = pop thestack
			in  push (oper left right) thestack
			with Stack.Empty -> (print_stackempty ();
								push right thestack)
	with Stack.Empty -> print_stackempty ()

let execute (thestack: stack_t) (oper: char) =
	try match oper with
		| '+'  -> executebinop thestack Bigint.add
		| '-'  -> executebinop thestack Bigint.sub
		| '*'  -> executebinop thestack Bigint.mul
		| '/'  -> executebinop thestack Bigint.div
		| '%'  -> executebinop thestack Bigint.rem
		| '^'  -> executebinop thestack Bigint.pow
		| 'c'  -> Stack.clear thestack
		| 'd'  -> push (Stack.top thestack) thestack
		| 'f'  -> Stack.iter print_number thestack
		| 'l'  -> failwith "operator l scanned with no register"
		| 'p'  -> print_number (Stack.top thestack)
		| 's'  -> failwith "operator s scanned with no register"
		| '\n' -> ()
		| ' '  -> ()
		| _    -> printf "0%o is unimplemented\n%!" (ord oper)
	with Stack.Empty -> print_stackempty()

	let toploop (thestack: stack_t) inputchannel =
		let scanbuf = Lexing.from_channel inputchannel in
		let rec toploop () = 
			try  let nexttoken = Scanner.scanner scanbuf
				 in  (match nexttoken with
					| Number number       -> push number thestack
					| Regoper (oper, reg) -> executereg thestack oper reg
					| Operator oper       -> execute thestack oper
					);
				 toploop ()
			with End_of_file -> ();
		in  toploop ()

let readfiles () =
	let thestack : bigint Stack.t = Stack.create ()
	in  ((if Array.length Sys.argv > 1 
		then try  let thefile = open_in Sys.argv.(1)
			in  toploop thestack thefile
			with Sys_error message -> (
			   printf "%s: %s\n%!" Sys.argv.(0) message;
			   exit 1));
		toploop thestack stdin)

let interact () =
	let thestack : bigint Stack.t = Stack.create ()
	in  toploop thestack stdin

let _ = if not !Sys.interactive then readfiles ()
