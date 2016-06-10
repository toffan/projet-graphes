open Dag.DAG

(* Calcule les sommets de g sans prédecesseurs. *)
let list_root_vertex g = fold_vertex
    (fun v l -> if (pred g v) = [] then v::l
                else l
    ) g [];;

let tri_topologique dag =
    let inclusion_aux z v y =
        (* Liste des prédecesseurs de v dans dag. *)
        let lp = pred dag v in
        if (List.fold_right (fun v add ->
            add && (
                let res = List.find_all (fun w -> w = v) z in
                (res = [])
            )
        ) lp true)
        then v::y
        else y

    in let y = list_root_vertex dag and z = [] in

    let rec tri_rec y z mark = (
        match y with
        (* Si y est vide alors le calcul est terminé et le résultat est dans z. *)
        | [] -> z
        | vi::q -> (
                Mark.set vi mark;
                (* zp : z à l'étape suivante.
                 * yp : y à l'étape suivante. *)
                let zp = vi::z in
                let yp = (
                    (* ls : liste des successeurs de vi (dans dag). *)
                    let ls = succ dag vi in
                    (* Calcul du nouvel ensemble Y. *)
                    List.fold_right (inclusion_aux zp) ls y)
                in tri_rec yp zp (mark+1)))
    in tri_rec y z 1;;
