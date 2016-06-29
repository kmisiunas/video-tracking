(* ::Package:: *)

(* ::Title:: *)
(*Background Estimate methods*)


(* ::Subtitle:: *)
(* by Karolis Misiunas *)


(* ::Text:: *)
(*Version 2 (2015-03-10) - separated from VideoTracking.m
                           Renamed functions to match pattern Background... *)


(* ::Section:: *)
(* Package Declarations*)


BeginPackage["BackgroundEstimate`", {"VideoIO`"}]



BackgroundCurrent::usage = 
  "BackgroundCurrent[] return stored background image"

BackgroundUpdate::usage = 
  "BackgroundUpdate[img_ | range_ | None-> Automatic] updates stored background image"

BackgroundFnSmartMean::usage = 
  "BackgroundFnSmartMean[range_] estimates mean background image using smart algorithm"

BackgroundFnMean::usage = 
  "BackgroundFnMean[range_] estimates background image using mean over specified range"

BackgroundFnMedian::usage = 
  "BackgroundFnMedian[range_] estimates background image using median over specified range"

BackgroundFnMedianSubsample::usage = 
  "Same as BackgroundFnMedian, but takes only 600 random images"

Options[BackgroundEstimate] = { 
    "BackgroundFunction" -> BackgroundFnMedianSubsample (*determine which background computation algorithm to use*),
}

(* ::Section:: *)
(* Implementations - Background*)

Begin["`Private`"]

BackgroundCurrent[] := background

BackgroundUpdate[img_Image]:= ( background = img );

(*updates bg with safety fall back to mean bg algorithm*)
BackgroundUpdate[range_?VectorQ] := 
  BackgroundUpdate @ Quiet @ Check[ 
    OptionValue[BackgroundEstimate, "BackgroundFunction"] @ range , 
        Print@"Background Estimate: initial algorithm failed, using backup BackgroundFnMedian algorithm";insta
        BackgroundFnMedian @ range
    ];

(*automatic background updater*)
BackgroundUpdate[] := BackgroundUpdate @ Range @ VideoLength[];


(*=========  Algorithms  ========*)

BackgroundFnMean[range_?VectorQ] := Image @ Mean[ ImageData /@ (VideoGet @ range) ] ;

BackgroundFnMedian[range_?VectorQ] := Image @ Median[ ImageData /@ (VideoGet @ range) ] ;

BackgroundFnMedianSubsample[range_?VectorQ] := 
  Image @ Median[ ImageData /@ (VideoGet @ RandomSample[range, Min[600, Length@range]] ) ] ;

(*  Algorithm averages not moving parts of the image over many frames
    Not buffered to allow mean image for large video files that are sparsely sampled
    *)
BackgroundFnSmartMean[range_?VectorQ] := Module[{RunBGAnalysis},
    RunBGAnalysis[sum0_, n0_, attemptsLeft_] := Module[
        {frames, sum, n, ComputeN, ns},
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
