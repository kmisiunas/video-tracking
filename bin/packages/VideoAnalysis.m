(* ::Package:: *)

(* ::Title:: *)
(*VideoAnalysis aid sofware*)


(* ::Subtitle:: *)
(* by Karolis Misiunas *)


(* ::Text:: *)
(*Contains additional functions to aid analysis*)
(*Version 1 (2015-02-02) - BinirizeVideoLevel v1 *)
(*Version 2 (2015-02-15) - change name space to Video... and add finding intresting bits *)

(* ::Section:: *)
(* Package Declarations*)


BeginPackage["VideoAnalysis`", {"VideoTracking`","VideoIO`"}]

(* ---  Declarations  --- *)

VideoFindThreshold::usage =
  "VideoFindThreshold[range_=all] determines the optimal binarize level by detecting frames with particles"

VideoFindActiveFrames::usage =
  "VideoFindActiveFrames[scanDensity_= auto] shows where active frames are inside the video"

VideoReadFrameID::usage =
  "VideoReadFrameID[image_] returns video frame ID from 4 pixels in top right corrner of a frame"

VideoDetectVibrations::usage = 
  "VideoDetectVibrations[] first call initiates the routine, second call returns vibration count on raw frames"

Begin["`Private`"]

(* ::Section:: *)
(*Package Implementations*)


VideoFindThreshold[range_] := Module[ {binList,MeanBinPlace,hist,peaks, thePeak},
  (*estimate the bin list*)
  binList = FindThreshold[#, Method -> "Cluster"] &/@ SubstractBG /@ GetFrame /@ range;
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
    VideoFindThreshold[ RandomSample[ Range @ NumberOfFrames[], Min[1000, NumberOfFrames[]] ] ]

(*============ VideoFindActiveFrames =============*)

VideoFindActiveFrames[scanDensity_Integer] := Module[
  {data},
  data =
    {#, ImageMeasurements[ SubstractBG@GetFrame[#], "MeanIntensity"] } &/@ Range[1, NumberOfFrames[], scanDensity];
  ListPlot[data,
    Frame -> True,
    FrameLabel -> {"Frame number", "Foreground intensity"}
  ]
]

VideoFindActiveFrames[] := VideoFindActiveFrames[ Round[ NumberOfFrames[]/1000 ]]

(*============ VideoReadFrameID =============*)

toInt[acc_, add_] := acc*256 + add;
VideoReadFrameID[image_Image] :=
    Fold[ toInt, 0, Reverse @ PixelValue[image, {1;;4, ImageDimensions[image][[2]] }, "Byte"] ]


(*============ VideoDetectVibrations =============*)

VideoDetectVibrations[] := "todo"

(* ::Section:: *)
(*The End*)

End[ ]

EndPackage[ ]
