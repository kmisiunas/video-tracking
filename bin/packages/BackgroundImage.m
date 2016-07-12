(* ::Package:: *)

(* ::Title:: *)
(*Background Estimate methods*)


(* ::Subtitle:: *)
(* by Karolis Misiunas *)


(* ::Text:: *)
(*Version 2 (2015-03-10) - separated from VideoTracking.m
                           Renamed functions to match pattern Background... *)
(*Version 3 (2016-07-07) - compatable with new video format; renamed package to BackgroundImage  *)


(* ::Section:: *)
(* Package Declarations*)


BeginPackage["BackgroundImage`", {"Video`"}]


BackgroundImage::usage =
    "BackgroundImage[images|video] gives the best estimate for background image"


BackgroundFnSmartMean::usage = 
  "BackgroundFnSmartMean[images] estimates mean background image using smart algorithm"

BackgroundFnMean::usage = 
  "BackgroundFnMean[images] estimates background image using mean over specified range"

BackgroundFnMedian::usage = 
  "BackgroundFnMedian[images] estimates background image using median over specified range"



Options[BackgroundImage] = {
    Method -> BackgroundFnMedian (*determine which background computation algorithm to use*),
    "NumberOfFrames" -> 600 (* number of images to use *)
}

(* ::Section:: *)
(* Implementations - Background*)

Begin["`Private`"]


BackgroundImage[vid_?VideoQ, opts: OptionsPattern[]] :=  BackgroundImage[vid["Images"], opts];

BackgroundImage[images_List, opts: OptionsPattern[]] :=
    OptionValue[Method][ RandomSample[images, Min[OptionValue["NumberOfFrames"], Length[images]]], opts];


(*=========  Algorithms  ========*)

BackgroundFnMean[images_List, opts: OptionsPattern[]] := Image @ Mean[ ImageData /@ images ] ;

BackgroundFnMedian[images_List, opts: OptionsPattern[]] := Image @ Median[ ImageData /@ images ] ;

BackgroundFnMedianSubsample[range_?VectorQ] :=
  Image @ Median[ ImageData /@ (VideoGet @ RandomSample[range, Min[600, Length@range]] ) ] ;

(*  Algorithm averages not moving parts of the image over many frames
    Not buffered to allow mean image for large video files that are sparsely sampled
    => Early testing show higher noise than Median algorithm
    *)
BackgroundFnSmartMean[images_List,  opts: OptionsPattern[]] := Module[{RunBGAnalysis},
    RunBGAnalysis[sum0_, n0_, attemptsLeft_] := Module[
        {frames, sum, n, ComputeN, ns},
        Print["BackgroundFnSmartMean: Not updated for new format yet"]; Abort[];
        frames = RandomSample[ VideoGet @ RandomSample[range, Min[200, Length@range]] ];
        FindStationaryPoints[i_] := Module[{d1,d2, filterAt},
            filterAt = 0.025; (* set where to binirize imges *)
            d1 = ImageDifference[frames[[i-1]],frames[[i]]]// Binarize[#,filterAt]&// ColorNegate;
            d2 = ImageDifference[frames[[i]],frames[[i+1]]]// Binarize[#,filterAt]&// ColorNegate;
            ImageMultiply[d1,d2]
        ];
        ns = FindStationaryPoints /@ Range[2,Length@frames-1];
        n = n0 + Total[ ImageData /@ ns ];
        sum = sum0 + Total[ ImageData /@ Thread[ImageMultiply[frames[[#]],ns[[#-1]]]&/@ Range[2,Length@frames-1]]];
        (*return result*)
        If[ (MemberQ[n, 0.0, 2] || MemberQ[n, 1.0, 2]) && attemptsLeft > 0, 
            RunBGAnalysis[ sum, n, attemptsLeft-1 ],
            Image @ (sum/n)
        ]
    ];
    RunBGAnalysis[0, 0, 4]
]



(* ::Section:: *)
(*The End*)


End[ ]

EndPackage[ ]
