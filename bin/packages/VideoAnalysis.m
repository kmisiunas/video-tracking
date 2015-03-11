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
                           Performance: optimised ComponentMeasurements calls
                           VideoGetForeground: options for different particle types*)


(* ::Section:: *)
(* Package Declarations*)


BeginPackage["VideoAnalysis`", 
  {"VideoIO`", "SubPixelFit`", "VideoSupportUI`", "VideoAnalysisHelpers`", "BackgroundEstimate`"} ]


VideoGetForeground::usage = 
  "VideoGetForeground[image_] subtracts background from image to get the foreground.
   Options for \"ForegroundMethod\": 
   \"Bright\" (default) -> works well for normal bright particles (ImageSubstact),
   \"Dark\" -> extracts only the elements that are darker than the background,
   \"Difference\" -> uses ImageDiffrence that enhances Airy disk features,
   \"Cut\" (todo) ->  try cutting the particle out from the image,
   Automatic (todo) ->  determine the method automatically"

VideoFrameBinarize::usage = 
  "VideoFrameBinarize[image_] binarizes the image with global settings for analysis.
  Options for \"Threshold\":
  Automatic -> use the inbuilt automatic method,
  x_?NumericQ -> set 'x' for the threshold,
  \"FromVideo\" (todo) -> finds Automatic value from entire video.
  Options for \"MinThreshold\":
  x_?NumericQ -> limits Automatic Threshold method to some minimum value."

VideoAnalyseFrame::usage = 
  "VideoAnalyseFrame[frame_] returns positions of detected particles in the frame"

VideoAnalyse::usage = 
  "VideoAnalyse[range_] performs video analysis on specified range or on all if not specified"

(*messages for user*)

VideoGetForeground::notMethod = 
  "Method not supported. type ?VideoGetForeground to see available option settings."
VideoFrameBinarize::notMethod = 
  "Method not supported. type ?VideoFrameBinarize to see available option settings."


(* options associated with analysis *)
Options[VideoTracking] = { 
    "Threshold" -> Automatic (*the threshold value*),
    "MinThreshold" -> 0.05 (*minimum threshold for binirizing the image under automatic mode*) ,
    "FilterArea" -> {9, 40} (* the size range in px *),
    "FilterElongation" -> {0.0, 0.3} (*the circularity requirement for the shape*),
    "AnalysisBlockSize" -> 4000 (* do analysis in lumps, update BG in between *),
    "UpdateBackgroung" -> False (*to update BG automatically or not*) ,
    "ForegroundMethod" -> "Bright" (*method for separating foreground from background*)
}


(* ::Section:: *)
(* Implementations - Background*)

Begin["`Private`"]

(* ::Section:: *)
(*Implementations - Particle Recognition*)

VideoGetForeground[img_Image] := Switch[ OptionValue[VideoTracking,"ForegroundMethod"],
  "Bright", ImageSubtract[ img, BackgroundCurrent[] ] , 
  "Dark",   ImageSubtract[  ImageDifference[img, BackgroundCurrent[]],
                            ImageSubtract[ img, BackgroundCurrent[]]    ], 
  "Difference", ImageDifference[img, BackgroundCurrent[]],
  _, Message[VideoGetForeground::notMethod]; Abort[] 
]

VideoFrameBinarize[img_Image] := Switch[ OptionValue[VideoTracking,"Threshold"],
  Automatic,   Binarize[ img , 
                Max[ FindThreshold[img] , OptionValue[VideoTracking, "MinThreshold"] ]  ] , 
  _?NumericQ,  Binarize[ img , OptionValue[VideoTracking,"Threshold"] ],
  _, Message[VideoFrameBinarize::notMethod]; Abort[] 
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
    img = VideoGetForeground @ VideoGet[ frame ];
    imgWBG = VideoFrameBinarize @ img;
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
      Print["Analysing: frames [["<> ToString[i+1] <> " ;; "<> ToString[Min[VideoLength[], i + blockSize]]<>"]"];
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
