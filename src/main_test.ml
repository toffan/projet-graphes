open Main
open Dag.DAG

let dag1t = tri_topologique (Dag_test.dag1);;
List.iter (fun v -> Printf.printf "%d : %s : %d\n" (Mark.get v) (Dag.DAG.Display.vertex_name v) (Dag.Vertex.memory (V.label v))) dag1t;;
