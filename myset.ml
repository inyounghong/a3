(* Definitions for sets. *)

exception TODO

(* An interface for set modules *)
module type SET =
sig
  type elt  (* type of elements in the set *)
  type set  (* abstract type for the set *)

  val empty : set

  val is_empty : set -> bool

  val insert : elt -> set -> set

  (* same as insert x empty *)
  val singleton : elt -> set

  val union : set -> set -> set
  val intersect : set -> set -> set

  (* remove an element from the set -- if the
   * element isn't present, does nothing. *)
  val remove : elt -> set -> set

  (* returns true iff the element is in the set *)
  val member : set -> elt -> bool

  (* chooses some member from the set, removes it
   * and returns that element plus the new set.
   * If the set is empty, returns None. *)
  val choose : set -> (elt * set) option

  (* fold a function across the elements of the set
   * in some unspecified order. *)
  val fold : (elt -> 'a -> 'a) -> 'a -> set -> 'a

  (* functions to convert our types to a string. useful for debugging. *)
  val string_of_set : set -> string
  val string_of_elt : elt -> string

  (* runs our tests. *)
  val run_tests : unit -> unit
end



(* parameter to Set modules -- we must pass in some
 * type for the elements of a set, a comparison
 * function, and a way to stringify it.
 *)
module type COMPARABLE =
sig
  type t
  val compare : t -> t -> Order.order
  val string_of_t : t -> string

  (* The functions below are used for testing. See TESTING
   * EXPLANATION *)

  (* Generate a value of type t. The same t is always returned *)
  val gen : unit -> t

  (* Generate a random value of type t. *)
  val gen_random : unit -> t

  (* Generate a t greater than the argument. *)
  val gen_gt : t -> unit -> t

  (* Generate a t less than the argument. *)
  val gen_lt : t -> unit -> t

  (* Generate a t between the two arguments. Return None if no such
   * t exists. *)
  val gen_between : t -> t -> unit -> t option
end



(* An example implementation of our COMPARABLE signature. Use this
 * struct for testing. *)
module IntComparable : COMPARABLE =
struct
  open Order
  type t = int
  let compare x y = if x < y then Less else if x > y then Greater else Eq
  let string_of_t = string_of_int
  let gen () = 0
  let gen_random =
    let _ = Random.self_init () in
    (fun () -> Random.int 10000)
  let gen_gt x () = x + 1
  let gen_lt x () = x - 1
  let gen_between x y () =
    let (lower, higher) = (min x y, max x y) in
    if higher - lower < 2 then None else Some (higher - 1)
end



(* A simple, list-based implementation of sets. *)
module ListSet(C: COMPARABLE) : (SET with type elt = C.t) =
struct
  open Order
  type elt = C.t
  type set = elt list

  (* INVARIANT: sorted, no duplicates *)
  let empty = []
  let is_empty xs =
    match xs with
      | [] -> true
      | _ -> false
  let singleton x = [x]
  let rec insert x xs =
    match xs with
      | [] -> [x]
      | y::ys -> (match C.compare x y with
          | Greater -> y::(insert x ys)
          | Eq -> xs
          | Less -> x::xs)

  let union xs ys = List.fold_right insert xs ys
  let rec remove y xs =
    match xs with
      | [] -> []
      | x::xs1 -> (match C.compare y x with
          | Eq -> xs1
          | Less -> xs
          | Greater -> x::(remove y xs1))

  let rec intersect xs ys =
    match xs, ys with
      | [], _ -> []
      | _, [] -> []
      | xh::xt, yh::yt -> (match C.compare xh yh with
          | Eq -> xh::(intersect xt yt)
          | Less -> intersect xt ys
          | Greater -> intersect xs yt)

  let rec member xs x =
    match xs with
      | [] -> false
      | y::ys -> (match C.compare x y with
          | Eq -> true
          | Greater -> member ys x
          | Less -> false)

  let choose xs =
    match xs with
      | [] -> None
      | x::rest -> Some (x,rest)
  let fold f e = List.fold_left (fun a x -> f x a) e

  let string_of_elt = C.string_of_t
  let string_of_set (s: set) : string =
    let f = (fun y e -> y ^ "; " ^ C.string_of_t e) in
    "set([" ^ (List.fold_left f "" s) ^ "])"


  (****************************************************************)
  (* Tests for our ListSet functor                                *)
  (* These are just examples of tests, your tests should be a lot *)
  (* more thorough than these.                                    *)
  (****************************************************************)

  (* adds a list of (key,value) pairs in left-to-right order *)
  let insert_list (d: set) (lst: elt list) : set =
    List.fold_left (fun r k -> insert k r) d lst

  let rec generate_random_list (size: int) : elt list =
    if size <= 0 then []
    else (C.gen_random()) :: (generate_random_list (size - 1))

  let test_insert () =
    let elts = generate_random_list 100 in
    let s1 = insert_list empty elts in
    List.iter (fun k -> assert(member s1 k)) elts ;
    ()

  let test_remove () =
    let elts = generate_random_list 100 in
    let s1 = insert_list empty elts in
    let s2 = List.fold_right (fun k r -> remove k r) elts s1 in
    List.iter (fun k -> assert(not (member s2 k))) elts ;
    ()

  let test_union () =
    let elts = generate_random_list 50 in
    let elts2 = generate_random_list 50 in
    let s1 = insert_list empty elts in
    let s2 = insert_list empty elts2 in
    let s3 = union s1 s2 in
    List.iter (fun k -> assert(member s3 k)) elts ;
    List.iter (fun k -> assert(member s3 k)) elts2 ;
    let s4 = union empty empty in
    assert(is_empty s4);
    ()

  let test_intersect () =
    let elts = generate_random_list 100 in
    let elts2 = generate_random_list 100 in
    let s1 = insert_list empty elts in
    let s2 = insert_list empty elts2 in
    let s3 = intersect s1 s2 in
    List.iter (fun k ->
      assert(
        if (member s3 k) then
          (member s2 k) && (member s1 k)
        else
          ((not (member s2 k)) || (not(member s1 k)) ))) elts ;
    let s4 = intersect s1 empty in
    assert (is_empty s4);
    let s5 = intersect s1 s1 in
    assert (not(is_empty s5));
    assert (s5 = s1);
    ()

  let test_member () =
    let elts = generate_random_list 100 in
    let s1 = insert_list empty elts in
    List.iter (fun k -> assert(member s1 k)) elts ;
    List.iter (fun k -> assert(not(member empty k))) elts ;
    ()

  let test_choose () =
    assert (choose empty = None);
    let elts = generate_random_list 10 in
    let s1 = insert_list empty elts in
    let el = match (choose s1) with | None -> failwith "no list" | Some x -> x in
    assert((member s1 (fst el)) && (not(member (snd el) (fst el)))) ;
    ()

  let test_fold () =
    let f1 (e:elt) (s:string) : string =
      (string_of_elt e)^s in
    let f2 (s:string) (e:elt) : string =
      (string_of_elt e)^s in
    assert ((fold f1 "" empty)="");
    let elt1 = C.gen_random() in
    let elt2 = C.gen_random() in
    let elt3 = C.gen_random() in
    let s1 = insert_list empty (elt1::elt2::elt3::[]) in
    (assert ((fold f1 "" s1) = (List.fold_left f2 "" s1)));
    ()

  let test_is_empty () =
    assert(is_empty empty);
    let elts = generate_random_list 100 in
    let s1 = insert_list empty elts in
    assert(not(is_empty s1));
    ()

  let test_singleton () =
    let el = C.gen_random() in
    let s1 = insert el empty in
    assert(s1 = (singleton el));
    ()

  let run_tests () =
    Printf.printf "Running tests\n\n";
    test_insert () ;
    test_remove () ;
    test_union () ;
    test_intersect () ;
    test_member () ;
    test_choose () ;
    test_fold () ;
    test_is_empty () ;
    test_singleton () ;
    ()

end


(******************************************************************)
(* DictSet: a functor that creates a SET by calling our           *)
(* Dict.Make functor                                              *)
(******************************************************************)

module DictSet(C : COMPARABLE) : (SET with type elt = C.t) =
struct
  module D = Dict.Make(struct
    type key = C.t
    type value = C.t
    let compare = C.compare
    let string_of_key = C.string_of_t
    let string_of_value = C.string_of_t

    let gen_key () = C.gen()
    let gen_key_gt x () = C.gen_gt x ()
    let gen_key_lt x () = C.gen_lt x ()
    let gen_key_random () = C.gen_random ()
    let gen_key_between x y () = C.gen_between x y ()
    let gen_value () = gen_key ()
    let gen_pair () = let p = gen_key() in (p,p)
  end)

  type elt = D.key
  type set = D.dict
  let empty = D.empty
  let is_empty (s:set) =
    s = empty

  let insert (e:elt) (s:set) = D.insert s e e
  let singleton (e:elt) = insert e empty

  let remove (e:elt) (s:set) = D.remove s e
  let member (s:set) (e:elt) = D.member s e
  let choose (s:set) =
    let option1 = D.choose s in
      match option1 with
      | None -> None
      | Some (x,y,z) -> Some (x,z)

  let fold (f:(elt -> 'a -> 'a)) (a1:'a) (s:set) =
    D.fold (fun a b x -> f b x) a1 s

  let matchoption (t:(elt*set) option) =
    match t with
    | Some c -> c
    | _ -> failwith "fail"

  let rec union (s1:set) (s2:set) =
    if (is_empty s2) then
      s1
    else let tuple = matchoption (choose s2) in
         let new_s1 = insert (fst tuple) s1 in
         union new_s1 (snd tuple)

  let rec intersect_helper (s1:set) (s2:set) (r:set) =
    if (is_empty s2) then
      r
    else let tuple = matchoption (choose s2) in
         if (member s1 (fst tuple)) then
           intersect_helper s1 (snd tuple) (insert (fst tuple) r)
         else
           intersect_helper s1 (snd tuple) r

  let intersect (s1:set) (s2:set) =
    intersect_helper s1 s2 empty

  (* implement the rest of the functions in the signature! *)

  let string_of_elt = D.string_of_key
  let string_of_set s = D.string_of_dict s

  (****************************************************************)
  (* Tests for our DictSet functor                                *)
  (* Use the tests from the ListSet functor to see how you should *)
  (* write tests. However, you must write a lot more              *)
  (* comprehensive tests to test ALL your functions.              *)
  (****************************************************************)

  (* add your test functions to run_tests *)

  let insert_list (d: set) (lst: elt list) : set =
    List.fold_left (fun r k -> insert k r) d lst

  let rec generate_random_list (size: int) : elt list =
    if size <= 0 then []
    else (C.gen_random()) :: (generate_random_list (size - 1))

  let test_insert () =
    (* Inserting into empty list *)
    let elts = generate_random_list 100 in
    let s1 = insert_list empty elts in
    List.iter (fun k -> assert(member s1 k)) elts ;

    (* Inserting into existing list *)
    let elts2 = generate_random_list 10 in
    let s2 = insert_list s1 elts2 in
    List.iter (fun k -> assert(member s2 k)) elts2 ;

    (* Inserting the same element into list *)
    let elt1 = C.gen_random () in
    let s3 = insert elt1 empty in
    let s4 = insert elt1 s3 in
    assert(s3 = s4);
    ()

  let test_remove () =
    (* let elts = generate_random_list 100 in
    let s1 = insert_list empty elts in
    let s2 = List.fold_right (fun k r -> remove k r) elts s1 in
    List.iter (fun k -> assert(not (member s2 k))) elts ; *)
    ()

  let test_union () =
    (* let elts = generate_random_list 50 in
    let elts2 = generate_random_list 50 in
    let s1 = insert_list empty elts in
    let s2 = insert_list empty elts2 in
    let s3 = union s1 s2 in
    List.iter (fun k -> assert(member s3 k)) elts ;
    List.iter (fun k -> assert(member s3 k)) elts2 ;
    let s4 = union empty empty in
    assert(is_empty s4); *)
    ()

  let test_intersect () =
(*     let elts = generate_random_list 100 in
    let elts2 = generate_random_list 100 in
    let s1 = insert_list empty elts in
    let s2 = insert_list empty elts2 in
    let s3 = intersect s1 s2 in
    List.iter (fun k ->
      assert(
        if (member s3 k) then
          (member s2 k) && (member s1 k)
        else
          ((not (member s2 k)) || (not(member s1 k)) ))) elts ;
    let s4 = intersect s1 empty in
    assert (is_empty s4);
    let s5 = intersect s1 s1 in
    assert (not(is_empty s5));
    assert (s5 = s1); *)
    ()

  let test_member () =
    (* let elts = generate_random_list 100 in
    let s1 = insert_list empty elts in
    List.iter (fun k -> assert(member s1 k)) elts ;
    List.iter (fun k -> assert(not(member empty k))) elts ; *)
    ()

  let test_choose () =
    (* assert (choose empty = None);
    let elts = generate_random_list 10 in
    let s1 = insert_list empty elts in
    let el = match (choose s1) with | None -> failwith "no list" | Some x -> x in
    assert((member s1 (fst el)) && (not(member (snd el) (fst el)))) ; *)
    ()

(*   let test_fold () =
    ()

  let test_is_empty () =
    assert(is_empty empty);
    let elts = generate_random_list 100 in
    let s1 = insert_list empty elts in
    assert(not(is_empty s1));
    ()

  let test_singleton () =
    let el = C.gen_random() in
    let s1 = insert el empty in
    assert(s1 = (singleton el));
    () *)

  let run_tests () =
    Printf.printf "Running tests\n\n";
    test_insert () ;
(*     test_remove () ;
    test_union () ;
    test_intersect () ;
    test_member () ;
    test_choose () ;
    test_fold () ;
    test_is_empty () ;
    test_singleton () ; *)
    ()
end




(******************************************************************)
(* Run our tests.                                                 *)
(******************************************************************)

(* Create a set of ints using our ListSet functor. *)
module IntListSet = ListSet(IntComparable)
let _ = IntListSet.run_tests()

(* Create a set of ints using our DictSet functor
 *
 * Uncomment out the lines below when you are ready to test your
 * 2-3 dict set implementation *)

module IntDictSet = DictSet(IntComparable)
let _ = IntDictSet.run_tests()



(******************************************************************)
(* Make: a functor that creates a SET by calling our              *)
(* ListSet or DictSet functors                                    *)
(******************************************************************)
module Make(C : COMPARABLE) : (SET with type elt = C.t) =
  (* Change this line to use our dictionary implementation when your are
   * finished. *)
  (* ListSet (C) *)
  DictSet (C)