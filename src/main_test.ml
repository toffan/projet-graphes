open Main
open Dag.DAG

let afficher_liste name l =
    Printf.printf "--------------- Affichage de : %s ---------------\n" name;
    List.iter (fun v -> Printf.printf "%d : (%s:%d)\n" (Mark.get v) (Dag.DAG.Display.vertex_name v) (Dag.Vertex.memory (V.label v))) l;
    Printf.printf "--------------- Fin d'affichage de : %s ---------------\n" name;;

let tri_dag1 = tri_topologique (Dag_test.dag1);;
let tri_dag2 = tri_topologique (Dag_test.dag2);;
let tri_dag3 = tri_topologique (Dag_test.dag3);;
let tri_dag4 = tri_topologique (Dag_test.dag4);;
let tri_dag_sujet = tri_topologique (Dag_test.dag_sujet);;

afficher_liste "Tri de dag1" tri_dag1;;
afficher_liste "Tri de dag2" tri_dag2;;
afficher_liste "Tri de dag3" tri_dag3;;
afficher_liste "Tri de dag4" tri_dag4;;
afficher_liste "Tri de dag_sujet" tri_dag_sujet;;

