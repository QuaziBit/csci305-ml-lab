(***************************************************************
*
* CSCI 305 - ML Programming Lab
*
* Olexandr Matveyev
* alex.matveyev88@gmail.com
*
***************************************************************)

(* Define your data type and functions here *)
datatype 'element Set = Empty | 
            Set of 'element * 'element Set;

print("\n=====================================================\n");
print("\n");
print("isMember: ");
(* 
  accepts [single value v] and [list --> tmp]
  compare [v] value with each value of the [tmp: list];
  if [v] == to some element from [tmp: list] return true else false;
 *)
fun isMember(v, tmp) = 
    if tmp = [] then false
    else if v = hd(tmp) then true
    else isMember(v, tl(tmp));

print("\n");
print("getElement: ");
(* 
  accepts list;
  if list is empty returns Empty;
  if list is not empty will build nested Set structure by making recursive call of its self;
  basically getElement will return the tail of the Set;
 *)
fun getElement [] = Empty |
    getElement(x::xs) =
        Set( x , getElement( xs ) ); 

print("\n");
print("list2Set: ");
(* 
  accepts list;
  if list is empty returns Empty;
  if list is not empty will build Set structure by making call of getElement function;
 *)
fun list2Set(tmp) = 
    if tmp = [] then Empty
    else Set( hd(tmp), getElement( tl(tmp) ) );
print("=====================================================\n\n");

print("=====================================================\n");
(* fun union set1 set2 *)
(*
  accepts list and list;
  if first argument is an empty list, function will return second argument and vise versa;
  if first argument and second argument is not empty will build union set,
  before making union the union function will call isMember function to check if 
  element of list_2 is in the list_1 and if true will make recursive call of its self while true
  if false will add element of the list_2 at the begining of the union set.

  will build new list, after function [list2Set] has to be used to build Set 
*)
print("union: ");
fun union([],list_1) = list_1 |
    union(list_2,[]) = list_2 | 
    union(list_2,list_1) =
        if isMember(hd(list_2),list_1) then union(tl(list_2),list_1)
        else hd(list_2)::union(tl(list_2),list_1);
print("\n");

(* fun intersect set1 set2 *)
(*
  accepts list and list;
  if first argument is an empty list, function will return second argument and vise versa;
  
  if first argument and second argument is not empty will build union set,
  before making union the union function will call isMember function to check if 
  element of list_2 is in the list_1 and if true will make recursive call of its self while true
  if false will add element of the list_2 at the begining of the union set.

  will build new list, after function [list2Set] has to be used to build Set 
*)
print("intersect: ");
fun intersect([],list_1) = [] |
    intersect(list_2,[]) = [] |
    intersect(list_2,list_1) =
        if isMember(hd(list_2),list_1) then hd(list_2) :: intersect(tl(list_2),list_1)
        else intersect(tl(list_2),list_1);
print("\n");
print("=====================================================\n\n");

print("=====================================================\n");
(* Simple function to stringify the contents of a Set of characters *)
fun stringifyCharSet Empty = ""
  | stringifyCharSet (Set(y, ys)) = Char.toString(y) ^ " " ^ stringifyCharSet(ys);

(* Simple function to stringify the contents of a Set of ints *)
fun stringifyIntSet Empty = ""
  | stringifyIntSet (Set(w, ws)) = Int.toString(w) ^ " " ^ stringifyIntSet(ws);

(* Simple function to stringify the contents of a Set of strings *)
fun stringifyStringSet Empty = ""
  | stringifyStringSet (Set(z, zs)) = z ^ " " ^ stringifyStringSet(zs);

(* Simple function that prints a set of integers *)
fun print_int x = print ("{ " ^ stringifyIntSet(x) ^ "}\n");

(* Simple function that prints a set of strings *)
fun print_str x = print ("{ " ^ stringifyStringSet(x) ^ "}\n");

(* Simple function that prints a set of characters *)
fun print_chr x = print ("{ " ^ stringifyCharSet(x) ^ "}\n");
print("=====================================================\n\n");

print("=====================================================\n");
val v_1 = list2Set([1, 3, 2]);
val e_1 = stringifyIntSet v_1;
print("\n");

val v_2 = list2Set([#"a", #"b", #"c"]);
val e_2 = stringifyCharSet v_2;
print("\n");

val v_3 = list2Set([]);
print("\n");

val v_4 = list2Set([6, 2, 2]);
val e_4 = stringifyIntSet v_4;
print("\n");

val v_5 = list2Set(["x", "y", "z", "x"]);
val e_5 = stringifyStringSet v_5;
print("=====================================================\n\n");

print("=====================================================\n");
(* Question 1 *)
print("Question 1\n");
fun f [] = [] (* a *)
   | f (x::xs) = (x + 1) :: (f xs); (* b *)
f [3, 1, 4, 1, 5, 9];
print("\n");

(* Question 5 *)
val quest5 = isMember("one",["1", "2", "3", "4"]);
print ("\nQuestion 5: " ^ Bool.toString(quest5) ^ "\n");
print("=====================================================\n\n");

print("=====================================================\n");
(* Question 7 *)
val quest7 = list2Set(["it", "was", "the", "best", "of", "times,", "it", "was", "the", "worst", "of", "times"]);
print("Question 7\n");
print_str quest7;
print "\n";
print("=====================================================\n\n");

print("=====================================================\n");
(* Question 9 *)
print "\nQuestion 9: ";
print_str ( list2Set(union(["green", "eggs", "and"],["ham"])) );

(* Question 10 *)
print "\nQuestion 10: ";
print_str ( list2Set(intersect(["stewed", "tomatoes", "and", "macaroni"],["macaroni", "and", "cheese"])) );
print("=====================================================\n\n");
