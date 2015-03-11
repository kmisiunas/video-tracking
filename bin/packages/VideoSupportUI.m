(* ::Package:: *)

(* ::Title:: *)
(*UI for vidoe traking / support features*)


(* ::Subtitle:: *)
(* by Karolis Misiunas *)


(* ::Text:: *)
(*The package is for fitting a sub pixel resolution to an image*)
(*Version 1 (2014-03-08) - initial release. *)


(* ::Section:: *)
(* Package Declarations*)


BeginPackage["VideoSupportUI`"]

(* ---  Declarations  --- *)

UIAnalyseFrame::usage =
  "Produces a panel for manual inspection of tracking algorithm."


Begin["`Private`"]

(* ::Section:: *)
(*Package Implementations*)

(*todo: update*)
UIAnalyseFrame[i_Integer] := Module[{img, imgBin,imgSubBG, posT,pImg, listProp},
  img = VideoGet[ Min[i, VideoLength[] ] ];
  imgBin=FrameBinarize[img];
  posT = GetPositions @ imgBin;
  imgSubBG= ShowTrackOverlayed[VideoGetForeground@img//ImageAdjust[#,{0,10}]&, posT];
  listProp = {{
  "frame"-> Min@{i, VideoLength[]},
  "centroid"->ComponentMeasurements[imgBin, "Centroid"],
  "circularity"-> ComponentMeasurements[imgBin, "Circularity"],
  "area"->ComponentMeasurements[imgBin, "Area"],
  "holes"->ComponentMeasurements[imgBin, "Holes"],
  "results"-> posT
  }} //Transpose;

  pImg = Panel[{{"Raw", "Binirize", "Img-BG, enhance"},
  {img,imgBin, imgSubBG}}//TableForm];

  Panel[{{pImg}} ~ Join ~ listProp //TableForm ]
]

(* ::Section:: *)
(*The End*)


End[ ]

EndPackage[ ]
