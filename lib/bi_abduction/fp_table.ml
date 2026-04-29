module Int64Set = Data_point.Int64Set

type t = (int, Int64Set.t option) Hashtbl.t

let empty : t = Hashtbl.create 16

let find (t : t) (q_idx : int) : Int64Set.t option option =
  Hashtbl.find_opt t q_idx


let of_results (results : (int * Int64Set.t option) list) : t =
  let t = Hashtbl.create (List.length results) in
  List.iter (fun (q_idx, fp) -> Hashtbl.replace t q_idx fp) results;
  t
