(* ::Package:: *)

(* ::Title:: *)
(*VideoAnalysisHelpers aid software for VideoAnalysis*)


(* ::Subtitle:: *)
(* by Karolis Misiunas *)


(* ::Text:: *)
(*Contains additional functions to aid analysis*)
(*Version 1.0 (2015-02-02) - BinirizeVideoLevel v1 *)
(*Version 2.0 (2015-02-15) - change name space to Video... and add finding interesting bits *)
(*Version 2.1 (2015-03-10) - Renamed to make space for main package *)

(* ::Section:: *)
(* Package Declarations*)


BeginPackage["VideoAnalysisHelpers`", {"VideoAnalysis`","VideoIO`"}]

(* ---  Declarations  --- *)

VideoFindThreshold::usage =
  "VideoFindThreshold[range_=all] determines the optimal binarize level by detecting frames with particles"

VideoFindActiveFrames::usage =
  "VideoFindActiveFrames[scanDensity_= auto] shows where active frames are inside the video"

VideoDetectVibrations::usage = 
  "VideoDetectVibrations[] first call initiates the routine, second call returns vibration count on raw frames"

Begin["`Private`"]

(* ::Section:: *)
(*Package Implementations*)


VideoFindThreshold[range_] := Module[ {binList,MeanBinPlace,hist,peaks, thePeak},
  (*estimate the bin list*)
  binList = FindThreshold[#, Method -> "Cluster"] &/@ SubstractBG /@ VideoGet /@ range;
  (*centers HistogramList output *)
  MeanBinPlace[data_] := { (data[[1, 2 ;;]] + data[[1, ;; -2]])/2 , data[[2]] };
  hist = N@Transpose@MeanBinPlace@HistogramList@binList;
  (*find peaks*)
  peaks = Interpolation[ 
    Transpose@{ Range@Length@hist, hist[[;; , 1]]}] /@ (FindPeaks@hist[[;; , 2]])[[;;,1]  ];
  (*select one peak*)
  thePeak = Last@peaks;
  (*show report*)
  Print@Panel[Column[{
   Text[Style["BinirizeVideoLevel[] - analysis results", FontSize -> 14, 
     FontFamily -> "Helvetica"]],
   Show[
    Histogram[binList, Automatic, "Probability" , AxesLabel -> {"Threshold", "P"}],
    Graphics[{Red, PointSize@Large , Point[{#, 0.05}]&/@peaks}],
    ImageSize -> Medium
    ],
   "Recommended Threshold value is " <> ToString@thePeak
   }]];
   (*return*)
  thePeak
]

VideoFindThreshold[] :=
    VideoFindThreshold[ RandomSample[ Range @ VideoLength[], Min[1000, VideoLength[]] ] ]

(*============ VideoFindActiveFrames =============*)

VideoFindActiveFrames[scanDensity_Integer] := Module[
  {data},
  data =
    {#, ImageMeasurements[ SubstractBG@VideoGet[#], "MeanIntensity"] } &/@ Range[1, VideoLength[], scanDensity];
  ListPlot[data,
    Frame -> True,
    FrameLabel -> {"Frame number", "Foreground intensity"}
  ]
]

VideoFindActiveFrames[] := VideoFindActiveFrames[ Round[ VideoLength[]/1000 ]]



(*============ VideoDetectVibrations =============*)

VideoDetectVibrations[] := "todo"

(* ::Section:: *)
(*The End*)

End[ ]

EndPackage[ ]
