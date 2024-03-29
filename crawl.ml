open Util
open CrawlerServices
open Order
open Pagerank



(* RandomWalkRanker and QuantumRanker are for karma questions only *)
module MoogleRanker
  = InDegreeRanker (PageGraph) (PageScore)
  (*
     = RandomWalkRanker (PageGraph) (PageScore) (struct
       let do_random_jumps = Some 0.20
       let num_steps = 1000
     end)
  *)
  (*
   = QuantumRanker (PageGraph) (PageScore) (struct
       let alpha = 0.01
       let num_steps = 1
       let debug = true
     end)
  *)

(* Dictionaries mapping words (strings) to sets of crawler links *)
module WordDict = Dict.Make(
  struct
    type key = string
    type value = LinkSet.set
    let compare = string_compare
    let string_of_key = (fun s -> s)
    let string_of_value = LinkSet.string_of_set

    (* These functions are for testing purposes *)
    let gen_key () = ""
    let gen_key_gt x () = gen_key ()
    let gen_key_lt x () = gen_key ()
    let gen_key_random () = gen_key ()
    let gen_key_between x y () = None
    let gen_value () = LinkSet.empty
    let gen_pair () = (gen_key(),gen_value())
  end)

(* A query module that uses LinkSet and WordDict *)
module Q = Query.Query(
  struct
    module S = LinkSet
    module D = WordDict
  end)

let print s =
  let _ = Printf.printf "%s\n" s in
  flush_all()


(***********************************************************************)
(*    PART 1: CRAWLER                                                  *)
(***********************************************************************)

(* Adds any non-visited links from [links] to the frontier and returns the
   frontier *)
let rec addToFrontier (frontier: LinkSet.set) (visited:LinkSet.set)
                      (links:link list) =
  match links with
  | [] -> frontier
  | hd::tl ->
    if (not(LinkSet.member visited hd)) then
      let new_frontier = LinkSet.insert hd frontier in
      addToFrontier new_frontier visited tl
    else
      addToFrontier frontier visited tl

(* Adds words in [words] with the corresponding link to [dict], and returns
  the final [dict] *)
let rec addToWordDict (dict:WordDict.dict) (words:string list) (link:link) =
  match words with
  | [] -> dict
  | hd::tl ->
    if (WordDict.member dict hd) then
      let links =
        (match (WordDict.lookup dict hd) with
          | Some x -> x
          | None -> LinkSet.empty) in
      let new_links = LinkSet.insert link links in
      let new_dict = WordDict.insert dict hd new_links in
      addToWordDict new_dict tl link
    else
      let link_set = LinkSet.empty in
      let new_dict = WordDict.insert dict hd (LinkSet.insert link link_set) in
      addToWordDict new_dict tl link

(* TODO: Build an index as follows:
 *
 * Remove a link from the frontier (the set of links that have yet to
 * be visited), visit this link, add its outgoing links to the
 * frontier, and update the index so that all words on this page are
 * mapped to linksets containing this url.
 *
 * Keep crawling until we've
 * reached the maximum number of links (n) or the frontier is empty. *)
let rec crawl (n:int) (frontier: LinkSet.set)
    (visited : LinkSet.set) (d:WordDict.dict) : WordDict.dict =
  (* Frontier is empty or max n is hit *)
  if (n = 0 || LinkSet.is_empty frontier) then
    d
  else
    let tuple =
      (match (LinkSet.choose frontier) with
        | Some x -> x
        | _ -> failwith "empty linkset") in
    let link = fst tuple in
    let new_frontier = snd tuple in

    if not ((CrawlerServices.get_page link) = None) then
      let page =
        (match (CrawlerServices.get_page link) with
          | Some x -> x
          | _ -> failwith "no page" ) in
      let new_visited = LinkSet.insert link visited in
      let links = page.links in
      let words = page.words in
      let final_frontier = addToFrontier new_frontier new_visited links in
      let new_dict = addToWordDict d words link in
      crawl (n-1) final_frontier new_visited new_dict
    else
      crawl n new_frontier visited d


let crawler () =
  crawl num_pages_to_search (LinkSet.singleton initial_link) LinkSet.empty
    WordDict.empty


(* Debugging note: if you set debug=true in moogle.ml, it will print out your
 * index after crawling. *)
