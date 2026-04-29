module Int64Set = Data_point.Int64Set

type t = (int * int, Int64Set.t option) Hashtbl.t

let empty : t = Hashtbl.create 16

let find (t : t) (key : int * int) : Int64Set.t option option =
  Hashtbl.find_opt t key


let of_results (results : (int * int * Int64Set.t option) list) : t =
  let t = Hashtbl.create (List.length results) in
  List.iter
    (fun (q_idx, dp_idx, fp) -> Hashtbl.replace t (q_idx, dp_idx) fp)
    results;
  t
