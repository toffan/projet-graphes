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

(* Entrées :
   - un DAG dag
   - un noeud v
   - une liste de noeuds l
   Sorties :
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

(* Entrées :
   - Une liste de noeuds triés d'un DAG.
   Sorties :
   - unit
   Sémantique : Marque les noeuds du DAG dans l'ordre de la liste. *)
let marque_tri l =
    let null =
    List.fold_right (fun v mark -> Mark.set v mark; (mark+1)) l 1 in ();;

(* Entrées :
   - un DAG dag
   Sorties :
   - unit
   Sémantique :
   - Marque les noeuds avec leur taille de chemin critique (taille du plus long chemin reliant le noeud à un puit). *)
let marque_chemin_critique dag =
    let l = list_root_vertex dag in
    let rec aux depth v = (
        let vdepth = (
        fold_succ (fun vc d ->
            (* vc : noeud courant du niveau actuel (de successeurs).
             * d : profondeur maximale du niveau actuel.*)
            ((max d (aux d vc))+1)
        ) dag v depth)
        in Mark.set v vdepth; vdepth)
    in List.iter (fun v -> let null = aux 0 v in ()) l;;

(* Fonction de tri par taille de chemin critique.
 * Entrées :
   - un DAG dag
   Sorties :
   - Liste de noeuds triés par ordre décroissant de taille de chemin critique (taille du plus long chemin reliant le noeud à un puit)
   - Pour deux noeuds ayant la même taille de chemin critique, on trie par ordre topologique croissant. *)
let tri_chemin_critique v1 v2 =
    let m1 = Mark.get v1 and m2 = Mark.get v2 in
    compare m2 m1;;

(* Entrées :
   - un DAG dag
   - un noeud v
   - une liste de noeuds l
   Sorties :
   - vrai si l contient au tous les prédecesseurs de v (dans dag).
   - faux sinon.
   *)
let dependances_satisfaites dag v l =
    let vp = pred dag v in
    (* Calcul de l'intersection de l et vp. *)
    List.fold_right (fun e res ->
        res && ((List.find_all (fun x -> x = e) l) != [])
    ) vp true;;

let ordonnanceur_avec_heuristique r dag =
    let l = tri_topologique dag in (
        (* Marque les noeuds en fonction de leur chemin critique. *)
        marque_chemin_critique dag;

        let rec aux todo trace =
            match todo with
            | [] -> trace
            | v::q -> (
                (* Calcul de la liste des noeuds faisables. *)
                let (ready, next) =
                    List.fold_right (fun v accu ->
                        let (r, n) = accu in
                        if (dependances_satisfaites dag v (List.flatten trace)) then
                            (* Ajout de v dans r selon le tri par chemin critique décroissant. *)
                            ((List.merge (tri_chemin_critique) r [v]), n)
                        else
                            (* On ajoute v à la liste des noeuds restants à traiter. *)
                            (r, n@[v])
                    ) todo ([],[])
                in
                (* Ajout des r premiers sommets de ready dans current. *)
                let (current,surplus) =
                    List.fold_right (fun v accu ->
                        let (c,s) = accu in
                        if List.length c < r then (c@[v],s)
                        else (c,s@[v])
                    ) ready ([],[])
                in aux (surplus@next) (current::trace))
        in let res = aux l [] in (
            (* Restaure le marquage des noeuds en fonction de leur ordre topologique. *)
            marque_tri l;

            res
        )
    );;

