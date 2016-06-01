open Dag.DAG

let tri_topologique dag =
    let list_vertex g =
        fold_vertex (
            fun v l ->
                if (pred g v) = [] then v::l
                else l
        ) g []
    and inclusion_aux z v y =
        let lp = pred dag v in
        if (List.fold_right (fun v add ->
            add && (
                let res = List.find_all (fun w -> w = v) z in
                (res = [])
            )
        ) lp true)
        then v::y
        else y
    in let y = list_vertex dag and z = [] in
    let rec tri_rec y z =
        match y with
        | [] -> z
        | t::q ->
                let zp = t::z
                in let yp =
                    let ls = succ dag t in
                    List.fold_right (inclusion_aux zp) ls y
                in tri_rec yp zp
    in tri_rec y z;;
