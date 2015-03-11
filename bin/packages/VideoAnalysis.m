(* ::Package:: *)

(* ::Title:: *)
(*VideoAnalysis package -> recognise where particles are in a video*)


(* ::Subtitle:: *)
(* by Karolis Misiunas *)


(* ::Text:: *)
(*Version 1 (2014-02-19) - initial release. *)
(*Version 2 (2015-03-10) - Background estimate functions exported to BackgroundEstimate.m 
                           Renamed To VideoAnalysis.m to represent purpose of the program better
                           Changed names of functions to VideoAnalyse and VideoAnalyseFrame 
                           Performance: optimised ComponentMeasurements calls*)


(* ::Section:: *)
(* Package Declarations*)


BeginPackage["VideoAnalysis`", 
  {"VideoIO`", "SubPixelFit`", "VideoSupportUI`", "VideoAnalysisHelpers`", "BackgroundEstimate`"} ]


SubstractBG::usage = 
  "substracts background image"

FrameBinarize::usage = 
  "binirizes the image, with settings"

VideoAnalyseFrame::usage = 
  "VideoAnalyseFrame[frame_] returns positions of detected particles in the frame"

VideoAnalyse::usage = 
  "VideoAnalyse[range_] performs video analysis on specified range or on all if not specified"



(* options associated with analysis *)
Options[VideoTracking] = { 
    "Threshold" -> Automatic (*the treshhold value*),
    "FilterArea" -> {9, 40} (* the size range in px *),
    "FilterElongation" -> {0.0, 0.3} (*the circularity requrement for the shape*),
    "AnalysisBlockSize" -> 4000 (* do analysis in lumps, update BG in between *),
    "UpdateBackgroung" -> False (*to update BG automatically or not*) ,
    "MinThreshold" -> 0.05 (*minimum threshold for binirizing the image under automatic mode*)
}


(* ::Section:: *)
(* Implementations - Background*)

Begin["`Private`"]

(* ::Section:: *)
(*Implementations - Particle Recognition*)

SubstractBG[img_Image] := ImageSubtract[ img, BackgroundCurrent[] ]

(*todo: remove IF for every try!*)
FrameBinarize[img_Image] := If[ OptionValue[VideoTracking,Threshold] === Automatic,
    Binarize[ img , Max[ FindThreshold[img] , OptionValue[VideoTracking, MinThreshold]] ] ,
    Binarize[ img , OptionValue[VideoTracking,Threshold] ]
]

FindParticles[img_Image] := ComponentMeasurements[ img,
  (*properties to read*)
  {"Area", "Elongation", "AdjacentBorderCount"},
  (*filter out too small particles*)
  #1 >= OptionValue[VideoTracking, "FilterArea"][[1]] &
]

WithinRangeQ[value_?Internal`RealValuedNumericQ, {min_, max_}] := min <= value <= max

(*input from FindParticles*)
SelectSingleParticles[list_] := Select[ list, 
  (*area*)
  #[[2,1]] ~ WithinRangeQ ~ OptionValue[VideoTracking, "FilterArea"] &&
  (*elongation*)
  #[[2,2]] ~ WithinRangeQ ~ OptionValue[VideoTracking, "FilterElongation"] &
  (*don't deal with AdjacentBorderCount as it SubPixelFit might tolerate it*)
][[;;,1]]

(*input from FindParticles*)
SelectOverlappingParticles[list_, excludeIds_?VectorQ] := Select[ list, 
  (*do interpret single particle again*)
  !MemberQ[excludeIds, #[[1]]] && (
  (*area*)
  #[[2,1]] ~ WithinRangeQ ~ (2*OptionValue[VideoTracking, "FilterArea"]) &&
  (*elongation - for two particles*)
  #[[2,2]] ~ WithinRangeQ ~ {0.4, 0.8} || 
  (*we have neighbours!*)
  #[[2,3]] > 0 
  )& 
][[;;,1]]

ReportOverlapOccurances[list_?VectorQ, frame_Integer] := 
  If[Length@list > 0, Print["Overlapping particles at frame "<>ToString@frame] ];



(* ::Section:: *)
(*Implementations - Computation Execution and Distribution *)

(*output list of {t, x,y,...} *)
VideoAnalyseFrame[frame_Integer] := Block[
    {img, imgWBG, partilces, single},
    img = SubstractBG @ VideoGet[ frame ];
    imgWBG = FrameBinarize @ img;
    partilces = FindParticles[ imgWBG ];
    single = SelectSingleParticles[partilces];
    ReportOverlapOccurances[ SelectOverlappingParticles[partilces, single], frame ];
    Prepend[#, frame] &/@ ForallFitSubPixel[ img, imgWBG , single]
]

(*fancy but does not run in parallel because of other methods are not able to be invoked on multiple cores?*)
VideoAnalyse[range_?VectorQ] := Module[
    {blockSize, blockUpdate},
    blockSize = OptionValue[VideoTracking, AnalysisBlockSize];
    blockUpdate[i_Integer] := (
      If[ OptionValue[VideoTracking, UpdateBackgroung],  BackgroungUpdate@Range[i+1, i+blockSize] ];
      Print["Analysing: frames ["<> ToString[i+1] <> " : "<> ToString[Min[VideoLength[], i + blockSize]]<>"]"];
    );
    blockUpdate[0];
    Part[ Reap @ Do[  If[Divisible[i, blockSize], blockUpdate[i] ];
                      Sow@ VideoAnalyseFrame[i], {i, range}
                    ], 2, 1] ~ Flatten ~ 1
]

VideoAnalyse[] := VideoAnalyse @ Range @ VideoLength[]


(* ::Section:: *)
(*The End*)


End[ ]

EndPackage[ ]
