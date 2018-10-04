(* Author: Tian Qiu, tqiu1@ucsc.edu *)
(* $Id: bigint.ml,v 1.5 2014-11-11 15:06:24-08 - - $ *)

open Printf

module Bigint = struct

type sign     = Pos | Neg
type bigint   = Bigint of sign * int list
let  radix    = 10
let  radixlen =  1

let car       = List.hd
let cdr       = List.tl
let map       = List.map
let reverse   = List.rev
let strcat    = String.concat
let strlen    = String.length
let strsub    = String.sub
let zero      = Bigint (Pos, [])
let len       = List.length

let charlist_of_string str = 
	let last = strlen str - 1
	in let rec charlist pos result =
		if pos < 0
		then result
		else charlist (pos - 1) (str.[pos] :: result)
in charlist last []

let bigint_of_string str =
	let len = strlen str
	in let to_intlist first =
	let substr = strsub str first (len - first) in
	let digit char = int_of_char char - int_of_char '0' in
	map digit (reverse (charlist_of_string substr))
	in if len = 0
		then zero
		else if str.[0] = '_'
			then Bigint (Neg, to_intlist 1)
			else Bigint (Pos, to_intlist 0)

let string_of_bigint (Bigint (sign, value)) =
	match value with
	| []    -> "0"
	| value -> let reversed = reverse value
		in strcat "" ((if sign = Pos then "" else "-") ::
		(map string_of_int reversed))


let cmp list1 list2 =
	let rec cmp' list1 list2 =
		if list1 = [] then 0
		else if (car list1) = (car list2)
			then cmp' (cdr list1) (cdr list2)
			else if (car list1) > (car list2)
				then 1
				else (-1)
	in if (len list1) = (len list2)
	then cmp' (reverse list1) (reverse list2)
	else if (len list1) > (len list2)
		then 1
		else (-1)

let rec calize value_list = if value_list = []
	then [0]
	else if (car value_list) = 0
		then calize (cdr value_list)
		else (reverse value_list)

let rec add' list1 list2 carry = match (list1, list2, carry) with
	| list1, [], 0       -> list1
	| [], list2, 0       -> list2
	| list1, [], carry   -> add' list1 [carry] 0
	| [], list2, carry   -> add' [carry] list2 0
	| car1::cdr1, car2::cdr2, carry ->
		let sum = car1 + car2 + carry
		in  sum mod radix :: add' cdr1 cdr2 (sum / radix)

let rec sub' list1 list2 carry = match (list1, list2, carry) with
	| list1, [], 0       -> list1
	| [], list2, 0       -> list2
	| list1, [], carry   -> sub' list1 [carry] 0
	| [], list2, carry   -> sub' [carry] list2 0
	| car1::cdr1, car2::cdr2, carry ->
		if (car1 - carry) < car2 
		then (car1 - carry + radix - car2) :: sub' cdr1 cdr2 1
		else (car1 - carry - car2) :: sub' cdr1 cdr2 0

let rec mul' list1 list2 value_list =
	let rec submul mlist value carry =
		if mlist = [] then if carry = 0 then [] else [carry]
		else let result = ((car mlist) * value + carry)
		in result mod radix :: submul (cdr mlist) value (result / radix)
	in if list2 = [] then value_list
	else mul' (0 :: list1) (cdr list2) 
		(add' value_list (submul list1 (car list2) 0) 0)

let rec div' nlist dlist len =
	let rec subdiv nlist dlist len times value_list =
		if len < 0 then [value_list; nlist]
		else if (cmp nlist dlist) >= 0
			then subdiv (calize (reverse (sub' nlist dlist 0))) 
				dlist len (times + 1) value_list
			else subdiv nlist (cdr dlist) (len - 1) 0 (times :: value_list)
	in if (cmp nlist dlist) >= 0
	then div' nlist (0 :: dlist) (len + 1)
	else subdiv nlist (cdr dlist) (len - 1) 0 []

let rec pow' mlist rlist value_list times =
	match (mlist, rlist, value_list, times) with
	| [], rlist, value_list, 0          -> rlist
	| car1::cdr1, rlist, value_list, 0  ->
		pow' cdr1 rlist (pow' [] [1] value_list radix) car1
	| mlist, rlist, value_list, times   ->
		pow' mlist (mul' rlist value_list []) value_list (times - 1)

let add (Bigint (neg1, value1)) (Bigint (neg2, value2)) =
	if neg1 = neg2
	then Bigint (neg1, add' value1 value2 0)
	else if (cmp value1 value2) > 0
		then Bigint (neg1, calize (reverse (sub' value1 value2 0)))
		else Bigint ((if neg1 = Pos then Neg else Pos), 
		calize (reverse (sub' value2 value1 0)))

let sub (Bigint (neg1, value1)) (Bigint (neg2, value2)) =
	if neg1 = neg2
	then if (cmp value1 value2) > 0
		then Bigint (neg1, calize (reverse (sub' value1 value2 0)))
		else Bigint ((if neg1 = Pos then Neg else Pos), 
		calize (reverse (sub' value2 value1 0)))
	else Bigint (neg1, add' value1 value2 0)

let mul (Bigint (neg1, value1)) (Bigint (neg2, value2)) =
	if neg1 = neg2
	then Bigint (Pos, mul' value1 value2 [])
	else Bigint (Neg, mul' value1 value2 [])

let div (Bigint (neg1, value1)) (Bigint (neg2, value2)) =
	if neg1 = neg2
	then Bigint (Pos, (car (div' value1 value2 0)))
	else Bigint (Neg, (car (div' value1 value2 0)))

let rem (Bigint (neg1, value1)) (Bigint (neg2, value2)) =
	if neg1 = neg2
	then Bigint (Pos, (car (cdr (div' value1 value2 0))))
	else Bigint (Neg, (car (cdr (div' value1 value2 0))))

let pow (Bigint (neg1, value1)) (Bigint (neg2, value2)) =
	if neg2 = Pos
	then Bigint (neg1, pow' (cdr value2) [1] value1 (car value2))
	else zero

end
