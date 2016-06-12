open Dag.DAG

(* Entrées :
   - un DAG dag
 * Sorties :
   - Liste des sommets de g sans prédecesseurs. *)
let list_root_vertex g = fold_vertex
    (fun v l -> if (pred g v) = [] then l@[v]
                else l
    ) g [];;


(* Entrées:
   - un DAG
 * Sorties:
   - une liste des sommets du DAG ordonnées selon un tri topologique
 *  Spécifications :
   - vous implementerez l'algorithme 1 de l'enonce, en utilisant un format de file pour Y (section 1) *)
let tri_topologique dag =
    let inclusion_aux z y vj = (
        (* Liste des prédecesseurs de vj dans dag. *)
        let lp = pred dag vj in (
            if (List.fold_right (fun v add ->
                add && (
                    ((List.find_all (fun w -> w = v) z) != [])
                )
            ) lp true)
            then (
                (*Printf.printf "Ajoute %d : %s : %d\n" (Mark.get vj) (Dag.DAG.Display.vertex_name vj) (Dag.Vertex.memory (V.label vj));*)
                y@[vj])
            else (
                (*Printf.printf "Ignore %d : %s : %d\n" (Mark.get vj) (Dag.DAG.Display.vertex_name vj) (Dag.Vertex.memory (V.label vj));*)
            y)
        ))

    in let y = list_root_vertex dag and z = [] in

    let rec tri_rec y z mark = (
        match y with
        (* Si y est vide alors le calcul est terminé et le résultat est dans z. *)
        | [] -> z
        | vi::yq -> (
                (*Printf.printf "Traitement de (%d -> %d) : %s : %d\n" (Mark.get vi) mark (Dag.DAG.Display.vertex_name vi) (Dag.Vertex.memory (V.label vi));*)
                Mark.set vi mark;
                (* zp : z à l'étape suivante.
                 * yp : y à l'étape suivante. *)
                let zp = z@[vi] in
                let yp = (
                    (* ls : liste des successeurs de vi (dans dag). *)
                    let ls = succ dag vi in
                    (* Calcul du nouvel ensemble Y. *)
                    List.fold_left (inclusion_aux zp) yq ls)
                in tri_rec yp zp (mark+1)))
    in tri_rec y z 1;;


(* trace d'execution
   definie en Section 2 de l'enonce (voir Figure 2) *)
type trace = (Dag.DAG.vertex list) list


(* Entrées :
   - un DAG dag
   - un noeud v
   - une liste de noeuds l
 * Sorties :
   - vrai si l contient au moins un prédecesseur de v (dans dag).
   - faux sinon. *)
let depend dag v l =
    let vp = pred dag v in
    (* Calcul de l'intersection de l et vp. *)
    List.fold_right (fun e res ->
        res || ((List.find_all (fun x -> x = e) l) != [])
    ) vp false;;


(* Entrées:
   - un nombre entier de ressources r
   - un DAG
 * Sorties:
   - une trace d'execution du DAG
 *  Spécifications :
   - le DAG est suppose non pondere
   - pas de contrainte mémoire (section 3)
   - vous n'utiliserez pas d'heuristique *)
let ordonnanceur_sans_heuristique r dag =
    let l = tri_topologique dag in
    List.rev (List.fold_left (fun trace v ->
        let current = List.hd trace in
        (* Si v a un parent dans current OU si length(current)>=r, on fait un nouvel étage.*)
        if ((List.length current) >= r || (depend dag v current)) then
            [v]::trace
        else
        (* Sinon on ajoute v à l'étage courant.*)
            (current@[v])::(List.tl trace)
    ) [[]] l);;


(* Entrées :
   - Une liste de noeuds triés d'un DAG.
 * Sorties :
   - unit
 * Sémantique : Marque les noeuds du DAG dans l'ordre de la liste.
   Utilisée pour restaurer le marquage des noeuds par tri topologique. *)
let marque_tri l =
    let null =
    List.fold_left (fun mark v -> Mark.set v mark; (mark+1)) 1 l in ();;


(* Entrées :
   - un DAG dag
   - un booléen permettant de changer la manière de calculer le chemin critique :
     -> use_sum : somme des chemins critiques des fils.
     -> !use_sum : 1 + maximum des chemins critiques des fils.
 * Sorties :
   - unit
   Sémantique :
   - Marque les noeuds avec leur taille de chemin critique. *)
let marque_chemin_critique dag use_sum =
    let l = list_root_vertex dag in
    let rec aux v = (
        let vdepth = 1 + (
        fold_succ (fun vc d ->
            (* vc : noeud courant du niveau actuel (de successeurs).
             * d : profondeur maximale du niveau actuel.*)
            if use_sum then
                (d + (aux vc))
            else
                (max d (aux vc))
        ) dag v (0))
        in Mark.set v vdepth; vdepth)
    in List.iter (fun v -> let null = aux v in ()) l;;


(* Fonction de comparaison par taille de chemin critique.
   Utilisée pour trier une liste de noeuds par taille de chemin critique.
 * Entrées :
   - deux noeuds à comparer v1 et v2.
 * Sorties :
   - 0 si m1 et m2 ont même taille de chemin critique.
   - entier positif si m2 a un chemin critique plus grand que m1.
   - entier négatif si m1 a un chemin critique plus grand que m2.
   *)
let tri_chemin_critique v1 v2 =
    let m1 = Mark.get v1 and m2 = Mark.get v2 in
    compare m2 m1;;


(* Entrées :
   - un DAG dag
   - un noeud v
   - une liste de noeuds l
 * Sorties :
   - vrai si l contient tous les prédecesseurs de v (dans dag).
   - faux sinon.
   *)
let dependances_satisfaites dag v l =
    let vp = pred dag v in
    (* Calcul de l'intersection de l et vp. *)
    List.fold_right (fun e res ->
        res && ((List.find_all (fun x -> x = e) l) != [])
    ) vp true;;


(* Entrées :
   - une liste de noeuds step.
 * Sorties :
   - La somme des mémoires requises par chaque noeud de step. *)
let mem_sum step =
    List.fold_right (fun v m ->
        let mv = (Dag.Vertex.memory (V.label v)) in (m + mv)
        ) step 0;;


(* Entrées:
   - un nombre entier de ressources r
   - memoire disponible m ou 0 si l'on se place dans le cas sans contraintes de mémoire.
   - un booléen optimiser_memoire (utilise une heuristique adaptée à la gestion de la mémoire).
   - un DAG
 * Sorties:
   - une trace d'execution du DAG
   *)
let ordonnanceur_avec_heuristique_et_memoire r m optimiser_memoire dag =
    let l = tri_topologique dag in (
        (* Marque les noeuds en fonction de leur chemin critique. *)
        marque_chemin_critique dag optimiser_memoire;

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
                            ((List.merge (tri_chemin_critique) [v] r), n)
                        else
                            (* On ajoute v à la liste des noeuds restants à traiter. *)
                            (r, v::n)
                    ) todo ([],[])
                in
                (* Ajout des r premiers sommets de ready dans current. *)
                let (current,surplus) =
                    List.fold_left (fun accu v ->
                        let (c,s) = accu in
                        if List.length c < r then (
                            if m > 0 then (
                                if (mem_sum c + Dag.Vertex.memory (V.label v)) <= m then
                                    (c@[v],s)
                                else
                                    (c,s@[v])
                            )
                            else (c@[v],s)
                        )
                        else (c,s@[v])
                    ) ([],[]) ready
                in aux (surplus@next) (trace@[current]))
        in let res = aux l [] in (
            (* Restaure le marquage des noeuds en fonction de leur ordre topologique. *)
            marque_tri l;

            res
        )
    );;


(* Entrées:
   - un nombre entier de ressources r
   - un DAG
 * Sorties:
   - une trace d'execution du DAG
 *  Spécifications :
   - le DAG est suppose non pondere
   - pas de contrainte mémoire (section 3)
   - vous utiliserez une heuristique pour ameliorer la duree de la trace *)
let ordonnanceur_avec_heuristique r dag =
    ordonnanceur_avec_heuristique_et_memoire r 0 false dag;;


(* Entrées:
   - un nombre entier de ressources r
   - memoire disponible M
   - un DAG
 * Sorties:
   - une trace d'execution du DAG
 *  Spécifications :
   - le DAG est suppose non pondere
   - on suppose une contrainte mémoire (section 4)
   - vous utiliserez la meme heuristique que le cas non-contraint
   *)
let ordonnanceur_contrainte_memoire r m dag =
    ordonnanceur_avec_heuristique_et_memoire r m false dag;;


(* Entrées:
   - un nombre entier de ressources r
   - memoire disponible M
   - un DAG
 * Sorties:
   - une trace d'execution du DAG
 *  Spécifications :
   - le DAG est suppose non pondere
   - on suppose une contrainte mémoire (section 4)
   - vous utiliserez une heuristique specifique au cas contraint
   *)
let ordonnanceur_contrainte_memoire_bonus r m dag =
    ordonnanceur_avec_heuristique_et_memoire r m true dag;;

