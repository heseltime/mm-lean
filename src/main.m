OutputFormat[i_Integer] := "I[" <> ToString[i] <> "]"
OutputFormat[s_String] := "T[\"" <> s <> "\"]"
OutputFormat[s_Symbol] := "Y[" <> ToString[s] <> "]"
OutputFormat[h_[args___]] := 
  "A" <> OutputFormat[h] <> "[" <> 
    StringRiffle[Apply[List, Map[OutputFormat, Hold[args]]], ","] <> 
  "]"
  
(* RunLeanTactic[x_, t_String, p_String, b_?BooleanQ, i_?StringQ]:=Module[{s,cmd}, s=OpenWrite["temp.lean", CharacterEncoding -> "UTF8"]; cmd=StringForm["run_cmd `1` \"`2`\" `3` >>= write_string",t,x // OutputFormat, If[b, "tt", "ff"]]; WriteString[s, StringForm["import main `1`", i], "\n", cmd]; Close[s];RunThrough[p <> " temp.lean",0];Import["temp.txt", CharacterEncoding -> "UTF8"]]; *)

(* RunLeanTactic[x_,t_,p_]:=RunLeanTactic[x,t,p,False,""] *)
(* RunLeanTactic[x_,t_,p_,b_?BooleanQ] := RunLeanTactic[x,t,p,b,""] *)
(* RunLeanTactic[x_,t_,p_,i_String] := RunLeanTactic[x,t,p,False,i] *)

(* ProveUsingLeanTactic[x_, t_String, p_String, b_?BooleanQ] := Module[{s,cmd}, s=OpenWrite["temp.lean", CharacterEncoding -> "UTF8"]; cmd=StringForm["run_cmd prove_using_tac (`1`) \"`2`\" `3` >>= write_string",t,x // OutputFormat, If[b, "tt", "ff"]]; WriteString[s, "import main", "\n", cmd]; Close[s];RunThrough[p <> " temp.lean",0];Import["temp.txt", CharacterEncoding -> "UTF8"]]; *)

(* ProveUsingLeanTactic[x_,t_,p_] := ProveUsingLeanTactic[x,t,p,False] *)

(* ProveInteractively[e_] := Module[{s,t,cmd,ts}, s=OpenWrite["temp.lean", CharacterEncoding -> "UTF8"]; t=OpenWrite["interactive_temp.lean", CharacterEncoding -> "UTF8"]; cmd=StringForm["run_cmd translate \"`1`\" >>= write_string",e // OutputFormat]; WriteString[s, "import main", "\n", cmd]; Close[s];RunThrough[p <> " temp.lean",0];ts=Import["temp.txt", CharacterEncoding -> "UTF8"]; cmd=StringForm["example : `1` := _", ts]; WriteString[t, cmd]; Close[t]; RunProcess[{"emacs","interactive_temp.lean"}];]; *)

(* SelectLeanPremises[e_] := RunLeanTactic[e, "find_relevant_facts", "premise_selection"] *)

SetAttributes[OutputFormat, HoldFirst];
(* SetAttributes[ProveUsingLeanTactic, HoldFirst]; *)
SetAttributes[ProveInteractively, HoldFirst];
SetAttributes[SelectLeanPremises, HoldFirst];

cache = "";


HandleLeanServerResponse[p_ProcessObject] := 
 Module[{msg, line, flag, res}, flag = True; msg = "error"; 
  While[flag, line = ImportString[ReadLine[p], "JSON"]; 
   If[("response" /. line) == "all_messages", flag = True; 
    msg = ("msgs" /. line), 
    If[("response" /. line) == "ok" && ("message" /. line) == 
       "file unchanged", msg = cache]; flag = False]]; cache = msg; 
  msg];

SendToLeanServer[p_ProcessObject, content_String] :=
 Module[{cmd},
  cmd = StringForm[
    "{\"seq_num\":0, \"command\":\"sync\", \"file_name\": \
\"dummy.lean\", \"content\":\"``\"}", content];
  WriteLine[p, cmd // ToString]];


LeanMode[] :=
 StartProcess[{LeanExecutable, "-j0", "-D pp.unicode=true",
	       "--server"}];


GetLeanProof[p_ProcessObject, name_String] :=
	Module[{res, content},
	       content = StringForm["import grid_view import imports set_option pp.beta true open tactic.interactive #grid_view ``", name];
	       SendToLeanServer[p, content // ToString];
	       res = HandleLeanServerResponse[p]; ("text" /. res[[1]]) // ToExpression];


(* Tree Version - WSS2023 *)
 
(* Dummy version: getLeanTree[proofCell_] := 
 Tree[24, {0, 
   Tree[23, {1, 
     Tree[22, {2, 
       Tree[21, {3, 
         Tree[20, {4, 
           Tree[19, {Tree[8, {5, Tree[7, {5, Tree[6, {3}]}]}], 
             Tree[18, {Tree[
                17, {10, Tree[16, {11, Tree[15, {10, 14, 13}]}]}]}], 
             4}]}]}]}]}]}]*)

proofID[Grid[{___, {ID, id_}, ___}, ___]] := id;

subproofs[
   Grid[{___, {Proofs, 
      OpenerView[{Arguments, 
        Column[subproofs_, ___]}, ___]}, ___}, ___]] := subproofs;
subproofs[proof_] := {};

getLeanTree[proof_] := 
 Tree[proofID[proof], proofStructure /@ subproofs[proof]]

getLeanTreeLeft[proof_] := 
 Tree[proofID[proof], proofStructure /@ subproofs[proof], 
  TreeLayout -> Left]


getLeanSize[proofCell_] := 
 leanTree = Module[{leanTree}, TreeSize[getLeanTree[proofCell]]]

getLeanDepth[proofCell_] := 
 leanTree = Module[{leanTree}, TreeDepth[getLeanTree[proofCell]]]

getLeanMaxID[proofCell_] := Max[VertexList[getLeanTree[proofCell]]]


getLeanAxioms[proofCell_] := Sort[Select[VertexList[getLeanTree[proofCell]], VertexOutDegree[getLeanTree[proofCell], #] == 0 &][[All,1]]]

getLeanNumberOfAxioms[proofCell_] := Length[getLeanAxioms[proofCell]]

getLeanNumberOfAxiomsUnique[proofCell_] := Length[DeleteDuplicates[getLeanAxioms[proofCell]]]


vertexShapeFunction[{x_, y_}, name_, {w_, h_}] := 
  Block[{leaves}, 
   leaves = 
    Select[VertexList[graph], VertexOutDegree[graph, #] == 0 &];
   If[MemberQ[leaves, name], {Green, Disk[{x, y}, 0.1]}, {Blue, 
     Rectangle[{x, y} - .1, {x, y} + .1]}]];

showLeanAxioms[proofCell_] := TreeGraph[getLeanTree[proofCell], 
  VertexShapeFunction -> vertexShapeFunction, 
  EdgeShapeFunction -> {{"HalfFilledArrow", "ArrowSize" -> 0}}]


(* End of Tree Version *)


GetLeanInfo[p_ProcessObject, name_String] :=
	Module[{res, content},
	       content = StringForm["import main imports set_option pp.beta true open tactic.interactive #view_info ``", name];
	       SendToLeanServer[p, content // ToString];
	       res = HandleLeanServerResponse[p]; ("text" /. res[[1]]) // ToExpression];


ProveUsingLeanTactic[p_ProcessObject, x_, t_String, b_?BooleanQ] :=
	Module[{res, content},
	       content = StringForm["import main run_cmd prove_using_tac_with_grid_view (`1`) \\\"`2`\\\" `3` >>= tactic.trace",t,x // OutputFormat, If[b, "tt", "ff"]];
	       SendToLeanServer[p, content // ToString];
	       res = HandleLeanServerResponse[p]; If[b, ("text" /. res[[1]]) // ToExpression, ("text" /. res[[1]])]];

ProveUsingLeanTactic[p_,x_,t_] := ProveUsingLeanTactic[p,x,t,False]


RunLeanTactic[p_ProcessObject, x_, t_String, b_?BooleanQ] :=
	Module[{res, content},
	       content = StringForm["import main run_cmd `1` \\\"`2`\\\" >>= tactic.trace",t,x // OutputFormat];
	       SendToLeanServer[p, content // ToString];
	       res = HandleLeanServerResponse[p]; If[b, ("text" /. res[[1]]) // ToExpression, ("text" /. res[[1]])]];

RunLeanTactic[x_,t_,p_]:=RunLeanTactic[x,t,p,False]


SelectLeanPremises[p_, e_] := RunLeanTactic[p, e, "find_relevant_facts", False]
(* RunLeanTactic[p_ProcessObject, x_, t_String, b_?BooleanQ, i_?StringQ] := *)
(* 	Module[{res, content}, *)
(* 	       content = StringForm["import main `1` run_cmd `2` \\\"`3`\\\" `4` >>= tactic.trace",i,t,x // OutputFormat, If[b, "tt", "ff"]]; *)
(* 	       SendToLeanServer[p, content // ToString]; *)
(* 	       res = HandleLeanServerResponse[p]; If[b, ("text" /. res[[1]]) // ToExpression, ("text" /. res[[1]])]]; *)

(* RunLeanTactic[x_,t_,p_]:=RunLeanTactic[x,t,p,False,""] *)
(* RunLeanTactic[x_,t_,p_,b_?BooleanQ] := RunLeanTactic[x,t,p,b,""] *)
(* RunLeanTactic[x_,t_,p_,i_String] := RunLeanTactic[x,t,p,False,i] *)
SetAttributes[RunLeanTactic, HoldRest];
SetAttributes[ProveUsingLeanTactic, HoldRest];



QuitLeanMode[p_ProcessObject] := KillProcess[p];
