open Dag.DAG

(* Calcule les sommets de g sans prédecesseurs. *)
let list_root_vertex g = fold_vertex
    (fun v l -> if (pred g v) = [] then l@[v]
                else l
    ) g [];;

let tri_topologique dag =
    let inclusion_aux z vj y = (
        (* Liste des prédecesseurs de vj dans dag. *)
        let lp = pred dag vj in (
            if (List.fold_right (fun v add ->
                add && (
                    ((List.find_all (fun w -> w = v) z) != [])
                )
            ) lp true)
            then (
                (Printf.printf "Ajoute %d : %s : %d\n" (Mark.get vj) (Dag.DAG.Display.vertex_name vj) (Dag.Vertex.memory (V.label vj)));
                y@[vj])
            else (
                (Printf.printf "Ignore %d : %s : %d\n" (Mark.get vj) (Dag.DAG.Display.vertex_name vj) (Dag.Vertex.memory (V.label vj)));
            y)
        ))

    in let y = list_root_vertex dag and z = [] in

    let rec tri_rec y z mark = (
        match y with
        (* Si y est vide alors le calcul est terminé et le résultat est dans z. *)
        | [] -> z
        | vi::yq -> (
                (Printf.printf "Traitement de (%d -> %d) : %s : %d\n" (Mark.get vi) mark (Dag.DAG.Display.vertex_name vi) (Dag.Vertex.memory (V.label vi)));
                Mark.set vi mark;
                (* zp : z à l'étape suivante.
                 * yp : y à l'étape suivante. *)
                let zp = z@[vi] in
                let yp = (
                    (* ls : liste des successeurs de vi (dans dag). *)
                    let ls = succ dag vi in
                    (* Calcul du nouvel ensemble Y. *)
                    List.fold_right (inclusion_aux zp) ls yq)
                in tri_rec yp zp (mark+1)))
    in tri_rec y z 1;;

type trace = (Dag.DAG.vertex list) list

(* entrees:
   - un DAG dag
   - un noeud v
   - une liste de noeuds l
   sorties:
   - vrai si l contient au moins un prédecesseur de v (dans dag).
   - faux sinon.
   *)
let depend dag v l =
    let vp = pred dag v in
    (* Calcul de l'intersection de l et vp. *)
    List.fold_right (fun e res ->
        res || ((List.find_all (fun x -> x = e) l) != [])
    ) vp false;;

let ordonnanceur_sans_heuristique r dag =
    let l = tri_topologique dag in
    List.fold_right (fun v trace ->
        let current = List.hd trace in
        (* Si v a un parent dans current OU si length(current)>=r, on fait un nouvel étage.*)
        if ((List.length current) >= r || (depend dag v l)) then
            [v]::trace
        else
            (v::current)::(List.tl trace)
    ) l [[]];;

