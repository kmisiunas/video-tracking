(* ::Package:: *)

(* ::Title:: *)
(*ParticleTracking package -> recognise where particles are in a video*)


(* ::Subtitle:: *)
(* by Karolis Misiunas *)


(* ::Text:: *)
(*Version 1 (2014-02-19) - initial release. *)
(*Version 2 (2015-03-10) - Background estimate functions exported to BackgroundImage.m
                           Renamed To VideoAnalysis.m to represent purpose of the program better
                           Changed names of functions to VideoAnalyse and VideoAnalyseFrame 
                           Performance: optimised ComponentMeasurements calls
                           VideoGetForeground: options for different particle types*)
(*Version 3 (2016-07-07) - Renamed from VideoAnalysis to ParticleTracking
                           Updated to make the analysis more persistant on errors.
                           If error occusrs, other tracks get a special label to indicate error.
                           Preparing to move to functional coding by passib around buffer values *)


(* ::Section:: *)
(* Package Declarations*)


BeginPackage["ParticleTracking`",
  {"Video`","VideoIO`", "SubPixelFit`", "BackgroundImage`"} ]


ParticleTracking::usage = "";

ParticleDetection::usage = "";


ForegroundImage::usage =
  "VideoGetForeground[image_] subtracts background from image to get the foreground.
   Options for \"ForegroundMethod\": 
   \"Bright\" (default) -> works well for normal bright particles (ImageSubstact),
   \"Dark\" -> extracts only the elements that are darker than the background,
   \"Difference\" -> uses ImageDiffrence that enhances Airy disk features,
   \"Cut\" (todo) ->  try cutting the particle out from the image,
   Automatic (todo) ->  determine the method automatically"




VideoProblemInFrames::usage =
  "VideoProblemsInFrames[] returns a list of frames that had some problem detected in it.
   VideoProblemsInFrames[frame_, description_] adds a problem to the list.
   VideoProblemsInFrames[Clear] removes all old data."

(*messages for user*)

ForegroundImage::notMethod =
  "Method not supported. "
VideoFrameBinarize::notMethod = 
  "Method not supported. "


(* options associated with analysis *)
Options[ParticleTracking] = {
  "Threshold" -> Automatic (*the threshold value*),
  "MinThreshold" -> 0.05 (*minimum threshold for binirizing the image under automatic mode*) ,
  "FilterArea" -> {9, 40} (* the size range in px *),
  "FilterElongation" -> {0.0, 0.3} (*the circularity requirement for the shape*),
  "AnalysisBlockSize" -> 1000 (* do analysis in lumps, update BG in between *),
  "Background" -> Automatic, (*supply background image externally *)
  "UpdateBackgroung" -> False (*to update BG automatically or not*) ,
  "ForegroundMethod" -> "Bright" (*method for separating foreground from background*),
  "InsistantDetection" -> True (*try detection multiple times*),
  "MinParticleDistance" -> 4 (* minimum distance between partilces when joining particles *),
  "Parallel" -> True

}

(* ::Section:: *)
(* Implementations - Background*)

Begin["`Private`"]

Options[ParticleDetection] = Options[ParticleTracking];
Options[ParticlesMergeSets] = Options[ParticleTracking];
Options[ForegroundImage] = Options[ParticleTracking];
Options[FindParticles] = Options[ParticleTracking];
Options[SelectSingleParticles] = Options[ParticleTracking];

(* === Main methods === *)


(*Retun: {{t,x,y,z,error,...}___} *)
ParticleTracking[video_?VideoQ, opts: OptionsPattern[]] := ParticleTracking[video["Images"], video["Ids"], opts];

ParticleTracking[images_, ids_, opts: OptionsPattern[]] := Module[
  {bg},
  bg = If[ ImageQ@OptionValue["Background"],
    OptionValue["Background"],
    (Print["Backgrund image: ", #]; #) &@ BackgroundImage[images]  ]; (*small input might result in error*)
  Flatten[#,1] &@ If[ OptionValue["Parallel"],
    PrintTemporary["Tracking Particles (parrallel): [[", Min[ids] ;; Max[ids], "]]"];
    Parallelize[
      Map[
        ParticleDetection[ #[[1]] -> ForegroundImage[#[[2]], bg, opts],  opts] &,
        Transpose[{ids, images}]
      ]
      ,
      Method -> Automatic,
      DistributedContexts ->
        {	"context`", "SubPixelFit`", "PartilceTracking`", "SubPixelFit`Private`", "ParticleTracking`Private`"}
    ],
    PrintTemporary["Tracking Particles (1 CPU): [[", Min[ids] ;; Max[ids], "]]"];
    Map[
      ParticleDetection[ #[[1]] -> ForegroundImage[#[[2]], bg, opts],  opts] &,
      Transpose[{ids, images}]
    ]
  ]
];



(*output list of {t, x, y, z, error, ...} *)
ParticleDetection[t_ -> foreground_Image, opts: OptionsPattern[]] := Module[
  {imgWBG, detections, qualityDetections, particles, newParticles},
  (* Step 1: normal detection *)
  imgWBG = Binarize[ foreground , OptionValue["Threshold"]];
  detections = FindParticles[ imgWBG , opts ];
  qualityDetections = SelectSingleParticles[detections, opts];
  particles = Prepend[#, t] &/@ (FitSubPixel[ foreground, ExpandBox[#,1] ] &/@ qualityDetections[[All, 2, 1 ]] );
  If[qualityDetections =!= detections , (* check if there are errors in reporting*)
    particles[[All,5]] = 0.6 * particles[[All,5]] (*mark as LQ-Pos *)
  ];
  (* Step 2 (optional): robust detection - search for more particles *)
  (* todo experimental method *)
  If[ Length[qualityDetections] != Length[detections] && OptionValue["InsistantDetection"] &&
      NumberQ@OptionValue["Threshold"], (* check if there are errors in reporting*)
    newParticles = ParticleDetection[t -> foreground,
      "InsistantDetection" -> False,
      "Threshold" ->  OptionValue["Threshold"]*1.5,
      "FilterArea" -> {OptionValue["FilterArea"][[1]]*0.6, OptionValue["FilterArea"][[2]]}
    ];
    particles = ParticlesMergeSets[ particles, newParticles, opts];
  ];
  Return[ particles ]
];

ParticleDetection[foreground_Image, opts: OptionsPattern[]] := ParticleDetection[0.0 -> foreground, opts ];


(* merges particle detections into one set, where they don't overlap - time not separated!
   Marks the secondary set as lower quality                           *)
ParticlesMergeSets[main_, secondary_, opts: OptionsPattern[]] := Module[
  {minDistance, unique},
  minDistance[pos_] := Min[ EuclideanDistance[pos, #] &/@ main[[All, {2,3}]] ];
  unique = Select[ secondary, minDistance[ #[[{2,3}]] ] >= OptionValue["MinParticleDistance"] &];
  unique[[All,5]] = 0.6 * unique[[All,5]];
  Join[main, unique]
];




(* === Small methods === *)

ForegroundImage[img_Image, bg_Image, opts: OptionsPattern[]] := Switch[ OptionValue["ForegroundMethod"],
  "Bright", ImageSubtract[ img, bg ] ,
  "Dark",   ImageSubtract[  ImageDifference[img, bg], ImageSubtract[ img, bg ] ],
  "Difference", ImageDifference[img, bg],
  _, Message[ForegroundImage::notMethod]; Abort[]
];



FindParticles[img_Image, opts: OptionsPattern[]] := ComponentMeasurements[ img,
  (*properties to read*)
  {"BoundingBox", "Area", "Elongation"}, (*todo: perimeter for checking of shape is convex*)
  (*filter out too small particles*)
  #2 >= OptionValue["FilterArea"][[1]] &
];

WithinRangeQ[value_, {min_, max_}] := min <= value <= max ;


(*input from FindParticles*)
SelectSingleParticles[list_, opts: OptionsPattern[]] := Select[ list,
  (*area*)
  #[[2,2]] ~ WithinRangeQ ~ OptionValue["FilterArea"] &&
  (*elongation*)
  #[[2,3]] ~ WithinRangeQ ~ OptionValue["FilterElongation"] &
  (*don't deal with AdjacentBorderCount as it SubPixelFit might tolerate it*)
];




(* ::Section:: *)
(*The End*)


End[ ]

EndPackage[ ]
