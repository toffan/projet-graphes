open Main
open Dag.DAG

let dagt = tri_topologique (Dag_test.dag_sujet);;
List.iter (fun v -> Printf.printf "%d : %s : %d\n" (Mark.get v) (Dag.DAG.Display.vertex_name v) (Dag.Vertex.memory (V.label v))) dagt;;
