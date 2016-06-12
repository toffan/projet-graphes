open Main
open Dag.DAG

let afficher_liste name l =
    Printf.printf "--------------- Affichage de : %s ---------------\n" name;
    List.iter (fun v -> Printf.printf "%d : (%s:%d)\n" (Mark.get v) (Dag.DAG.Display.vertex_name v) (Dag.Vertex.memory (V.label v))) l;
    Printf.printf "--------------- Fin d'affichage de : %s ---------------\n" name;;

let afficher_trace name l =
    Printf.printf "--------------- Affichage de : %s ---------------\n" name;
    List.iter (fun step ->
    List.iter (fun v -> Printf.printf "%d : (%s:%d)\n" (Mark.get v) (Dag.DAG.Display.vertex_name v) (Dag.Vertex.memory (V.label v))) step;
    Printf.printf "\n";) l;
    Printf.printf "--------------- Fin d'affichage de : %s ---------------\n" name;;

let dag = Dag_test.dag4 and name = "dag4";;
let r = 3 and m = 3;;

let tri_dag = tri_topologique dag;;
let ord_sh_dag = ordonnanceur_sans_heuristique r dag;;
let ord_ah_dag = ordonnanceur_avec_heuristique r dag;;
let ord_cm_dag = ordonnanceur_contrainte_memoire r m dag;;
let ord_cm_ah_dag = ordonnanceur_contrainte_memoire_bonus r m dag;;

afficher_liste (String.concat "" ["Tri de "; name]) tri_dag;;
afficher_trace (String.concat "" ["Ordonnancement sans heuristique de "; name]) ord_sh_dag;;
afficher_trace (String.concat "" ["Ordonnancement avec heuristique de "; name]) ord_ah_dag;;
afficher_trace (String.concat "" ["Ordonnancement avec contrainte mémoire de "; name]) ord_cm_dag;;
afficher_trace (String.concat "" ["Ordonnancement avec contrainte mémoire et heuristique de "; name]) ord_cm_ah_dag;;

(*afficher_liste "succ va2" (succ Dag_test.dag2 Dag_test.va2);;
afficher_liste "succ vr2" (succ Dag_test.dag2 Dag_test.vr2);;
afficher_liste "succ ve2" (succ Dag_test.dag2 Dag_test.ve2);;*)

(*iter_vertex (fun v -> Printf.printf "%d : (%s:%d)\n" (Mark.get v) (Dag.DAG.Display.vertex_name v) (Dag.Vertex.memory (V.label v))) Dag_test.dag2;*)
