(* Mathematica Package *)
(* Created by Mathematica Plugin for IntelliJ IDEA *)

(* :Title: Linking *)
(* :Context: Linking` *)
(* :Author: kmisiunas *)
(* :Date: 2016-06-22 *)

(* Versions *)
(* (2016-06-22) Initial version writen largeley on the basis of KLab *)

(* :Mathematica Version: 10 *)
(* :Copyright: (c) 2016 kmisiunas *)
(* :Keywords: *)
(* :Discussion: Algorithm for linking together points into trajectories *)

BeginPackage["Linking`", {"SortedLists`"}]
(* Exported symbols added here with SymbolName::usage *)

LinkingFindPossibilities::usage =
    "LinkingFindPossibilities[starts_, endings_, (options)] discovers all possible links";

LinkingFormatTracks::usage = "";

LinkingAlgorithm::usage =
    "LinkingAlgorithm[starts_, endings_, (options)] performs linking betwen starts and ends.
     Use Linking[] for a user friendly exprerience";

Linking::usage =
    "Linking[points|tracks, (options)] performs data linking.
     Options:
      - LinkingMaxDt 
      - LinkingMaxDxyz
      - LinkingStartId
      - LinkingCostFn
      - LinkingDebug";

LinkingExplodeTrack::usage = "LinkingExplodeTrack[points|tracks, (options)]";
out
(* Cost functions *)

LinkingCostFnStandard::usage = "Cost function optimised for particle diffusion in channels"

(* Option parameters *)

LinkingMaxDt::usage =
    "Option: maximum number of frames to consider when linking trajectories (default is 10)";
LinkingMaxDxyz::usage =
    "Option: maximum distance to even consider linking. To improve performance (default is 20)";
LinkingDebug::usage =
    "Option: prints additional information fr debuging the code (default is False)";
LinkingStartId::usage =
    "Option: set id number for stating the track labeling (default is 1)";
LinkingCostFn::usage =
    "Option (Important): specifies the linking price between two points.
    It must be a function takeing two positions as an argument, i.e. (#2[[1]] - #1[[1]] &)
    Some default functions:
    ";

Begin["`Private`"]

(* === Options === *)
allLinkingOptions := {
  LinkingDebug-> False,
  LinkingMaxDt -> 10,
  LinkingMaxDxyz -> 20,
  LinkingCostFn -> LinkingCostFnStandard,
  LinkingStartId -> 1
};



(* === Generate the links first ===  *)
Options[LinkingFindPossibilities] = allLinkingOptions;

LinkingFindPossibilities[starts_, endings_, opts: OptionsPattern[]] := Module[
  {p1, p2, p2t, minIds, maxIds, potential},
  (* sort first by time *)
  p1 = SortBy[starts, First];
  p2 = SortBy[endings, First];
  (* find a vector with max and min ids *)
  p2t = p2[[All, 1]];
  minIds = BinarySearchUp[p2t, # + 1] & /@ p1[[All, 1]];
  maxIds = BinarySearchDown[p2t, # + OptionValue[LinkingMaxDt] ] & /@ p1[[All, 1]];
  (* create all posible links *)
  potential = Flatten[ Table[
    (Start[p1[[i]]] -> Ending[p2[[#]]]) &/@ Range[minIds[[i]], maxIds[[i]] ],
    {i, Length[starts]}
  ] , 1 ];
  (* filter distances *)
  Select[ potential, EuclideanDistance[ #[[1,1, {2,3,4}]] , #[[2,1, {2,3,4}]] ] <= OptionValue[LinkingMaxDxyz] &]
] ;


(* === Solver === *)

Options[LinkingAlgorithm] = allLinkingOptions;

LinkingAlgorithm[starts_, endings_, opts: OptionsPattern[]] := Module[
  {costFunction, possibleLinks, allPossibleLinks, allCosts, sol, links},
  costFunction[Start[from_] -> Ending[to_]] := OptionValue[LinkingCostFn][from, to];
  possibleLinks = LinkingFindPossibilities[starts, endings, opts];
  allPossibleLinks = Join[possibleLinks, addNetworkBeginning[starts], addNetworkFinals[starts, endings]];
  allCosts = Join[
    costFunction /@ possibleLinks, addNetworkBeginningCosts[starts], addNetworkFinalsCosts[starts, endings]   ];

  sol = FindMinimumCostFlow[ Graph[allPossibleLinks,  EdgeCost -> allCosts], "b", "f", "EdgeList"];
  (* remove algorithm notations and return links *)
  dropSegmentNotation /@ Select[ sol, #[[1]] =!= "b" && #[[2]] =!= "f" &]
];


(* helper methods: add single starting and ending points to all posible beginnings and ends *)
addNetworkBeginning[starts_] := ("b" -> Start[#]) & /@ starts;
addNetworkBeginningCosts[starts_] := ConstantArray[0.0, Length[starts]];
addNetworkFinals[starts_, endings_] := Join[
  (Start[#] -> "f") & /@ starts,
  (Ending[#] -> "f") & /@ endings
];
addNetworkFinalsCosts[starts_, ends_] := Join[
  ConstantArray[1.0, Length[starts]] ,
  ConstantArray[0.0, Length[ends]]
] ;

(* helper methods: strip additional components *)
dropSegmentNotation[Start[p1_] -> Ending[p2_]] := p1 -> p2 ;
dropSegmentNotation[Start[p1_] \[DirectedEdge] Ending[p2_]] := p1 -> p2 ;

Options[LinkingFormatTracks] = allLinkingOptions;

LinkingFormatTracks[links_, opts: OptionsPattern[]] := Module[
  {graph, tracks, ids},
  graph = Graph[links];
  (*If[OptionValue[LinkingDebug], Print@Graph[graph, VertexShapeFunction -> (Tooltip[Point[#1], #2] &)] ];*)
  tracks = SortBy[First] /@ WeaklyConnectedComponents[graph];
  ids = (Range@Length[tracks] + OptionValue[LinkingStartId] - 1);
  AssociationThread[ ids -> SortBy[ #[[1,1]] & ][tracks] ]
];

LinkingExplodeTrack[track:{___List}] := FoldList[(#1[[2]] -> #2 &), track[[1]] -> track[[2]] , track[[3;;]] ];

LinkingExplodeTrack[track_Rule] := LinkingExplodeTrack[track[[2]]] ;



(* === UI Functions ===*)

Options[Linking] = allLinkingOptions ;

(* for raw points *)
Linking[points:{___List}, opts: OptionsPattern[]] := Module[
  {},
  If[OptionValue[LinkingDebug], Print["[Debug] ", "Detected raw points as input."]];
  LinkingFormatTracks[
    LinkingAlgorithm[points, points, opts],
    opts
  ]
];

Linking[{}, opts: OptionsPattern[]] := <| |>;

(* for tracks *)
Linking[tracks_Association, opts: OptionsPattern[]] := Module[
  {newLinks, oldLinks},
  If[OptionValue[LinkingDebug], Print["[Debug] ", "Detected tracks as input."]];
  newLinks = LinkingAlgorithm[Last/@Values[tracks], First/@Values[tracks], opts];
  If[OptionValue[LinkingDebug], Print["[Debug] ", "Found ", Length[newLinks], " new links"]];
  oldLinks = Flatten[ LinkingExplodeTrack/@Values[tracks] , 1 ];
  LinkingFormatTracks[ Join[newLinks,oldLinks] ,opts ]
];



(* === Common linking Cost Functions === *)

LinkingCostFnStandard := (0.1 (#2[[1]] - #1[[1]] - 1) + 0.1 EuclideanDistance[#2[[{2, 3, 4}]], #1[[{2, 3, 4}]]] &)

(* Uses the distribution of detectionsto deterimne where particles are likely to exit and enter the channel*)
(* only works with channels along the x axis*)
LinkingCostFnChannel[points_:{___List}] := Module[
  {interval},
  {interval} = Interval @ Quantile[  Flatten[points[[ All, 2]]  ] , {1/10, 9/10}];
  ( If[IntervalMemberQ[interval, #1[[2]]], 0.2, 1] *
      ( 0.25 (#2[[1]] - #1[[1]] - 1) + 0.1 EuclideanDistance[#2[[{2, 3, 4}]], #1[[{2, 3, 4}]]] )
        &)
]



End[] (* `Private` *)

EndPackage[]