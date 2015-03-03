(* ::Package:: *)

(* ::Title:: *)
(*Video Traking Package*)


(* ::Subtitle:: *)
(* by Karolis Misiunas *)


(* ::Text:: *)
(*Version 1 (2014-02-19) - initial release. *)


(* ::Section:: *)
(* Package Declarations*)


BeginPackage["VideoTracking`",{"VideoIO`", "SubPixelFit`", "VideoSupportUI`", "FFmpeg`", "VideoAnalysis`"}]


SubstractBG::usage = 
    "substracts background image"

UpdateBackgroung::usage = 
    "updates background image based on specified frames"

GetPositions::usage = 
    "analyses images and finds positions of colloids if there are any"

FrameBinarize::usage = 
    "binirizes the image, with settings"

CurrentBackground::usage = 
    "return current global background image"

SmartMeanBackground::usage = 
    "SmartMeanBackground[range_] estimates mean background image using smart algorithm"

MeanBackground::usage = 
    "MeanBackground[range_] estimates mean background image using"

SelectParticles::usage = 
    "SelectParticles[img_] selects the particle that fit search parameters"

AnalyseFrame::usage = 
    "AnalyseFrame[frame_] returns positions at the given frame"

RunAnalysis::usage = 
    "RunAnalysis[from_ , to_] does video traking analysis in parallel"

PrepareAnalysis::usage = 
    "PrepareAnalysis[] does the nessesary background for the fast analysis"


(* options associated with analysis *)
Options[VideoTracking] = { 
    "Threshold" -> Automatic (*the treshhold value*),
    "FilterArea" -> {9, 40} (* the size range in px *),
    "FilterCircularity" -> {0.9, 1.5} (*the circularity requrement for the shape*),
    "AnalysisBlockSize" -> 4000 (* do analysis in lumps, update BG in between *),
    "UpdateBackgroung" -> False (*to update BG automatically or not*) ,
    "BackgroundAlgorithm" -> SmartMeanBackground (*determine which background computation algorithm to use*),
    "MinThreshold" -> 0.05 (*minimum threshold for binirizing the image under automatic mode*)
}


(* ::Section:: *)
(* Implementations - Background*)

Begin["`Private`"]

background = {};

CurrentBackground[] := background

SubstractBG[img_Image]:= ImageSubtract[ img, background]

UpdateBackgroung[img_Image]:= ( background = img )

(*updates bg with safety fall back to mean bg algorithm*)
UpdateBackgroung[ids_]:= 
  UpdateBackgroung @ Quiet @ Check[ 
    OptionValue[VideoTracking,BackgroundAlgorithm] @ ids , 
        Print@"Algorithm Failed: falling back to mean background algorithm.";
        MeanBackground @ ids
    ];

UpdateBackgroung[from_Integer,to_Integer]:= UpdateBackgroung @ Range[from, to];

(*automatic background updater*)
UpdateBackgroung[] := UpdateBackgroung[1, NumberOfFrames[]];

MeanBackground[range_] := Image @ Mean[ ImageData /@ (GetFrame /@ range) ]

(*  Algorithm averages not moving parts of the image over many frames
    Not buffered to allow mean image for large video files that are sparsly sampled
    *)
SmartMeanBackground[range_] := Module[{RunBGAnalysis},
    RunBGAnalysis[sum0_, n0_, attemptsLeft_] := Module[
        {frames, sum, n, ComputeN, ns},
        frames = RandomSample[ GetFrame /@ RandomSample[range, Min[200, Length@range]] ];
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
(*Implementations - Analysis algorithms *)

(*todo: remove IF for every try!*)
FrameBinarize[img_Image] := If[ OptionValue[VideoTracking,Threshold] === Automatic,
    Binarize[ img , Max[ FindThreshold[img] , OptionValue[VideoTracking, MinThreshold]] ] ,
    Binarize[ img , OptionValue[VideoTracking,Threshold] ]
]

RunAnalysis[from_:1, to_: NumberOfFrames[]] := Module[
    {reults, blockSize, Unzipper, AnalyseBlock},

    Unzipper[el_] := If[ Length @ el[[2]] > 0,
        Map[ {el[[1]] , #[[1]], #[[2]]} &, el[[2]] ],
        {}
    ];

    AnalyseBlock[ids_] := Module[{positions, zipped},
        PrintTemporary @ ( "Analysis: of frames ["<>ToString@Min@ids<>", "<>ToString@Max@ids<>"]" );
        If[ OptionValue[VideoTracking, UpdateBackgroung],  UpdateBackgroung[ids] ];
        positions = ParallelMap[ AnalyseFrame[#] &, ids ];
        zipped = Transpose @ {ids, positions};
        Flatten[ Map[Unzipper[#] & , zipped ], 1 ]
    ];

    blockSize = OptionValue[VideoTracking, AnalysisBlockSize];

    results = Table[ 
        AnalyseBlock @ Range[from + (i-1)*blockSize, Min[(from + i*blockSize-1), NumberOfFrames[] ] ] ,  
        {i, 1, Ceiling[(to-from+1)/blockSize]}
    ];
    Flatten[ results, 1 ]
]

GetPositions[img_] := SelectParticles[img] /. ComponentMeasurements[img, "Centroid"]

SelectParticles[img_] := Intersection[ 
    FilterSize[img, OptionValue[VideoTracking, FilterArea] ]  
    ,FilterCircularity[img, OptionValue[VideoTracking, FilterCircularity] ]
    ,FilterHoles[img] 
]

AnalyseFrame[frame_Integer] := Module[
    {img, imgWBG, takePartilces},
    img = SubstractBG @ GetFrame[ frame ];
    imgWBG = FrameBinarize @ img;
    takePartilces = SelectParticles @ imgWBG;
    ForFitSubPixel[ img, imgWBG , takePartilces]
]

(* ::Section:: *)
(*Private Methods*)

FilterSize[img_, range_]:= Module[{area, min, max},
    {min, max} = range;
    area = ComponentMeasurements[img, "Area"];
    Select[area, #[[2]]>= min&&#[[2]]<= max &][[;;,1]]
]

FilterCircularity[img_, range_]:=Module[{prop, min, max},
    {min, max} = range;
    prop = ComponentMeasurements[img, "Circularity"];
    Select[prop, #[[2]]>= min&&#[[2]]<= max &][[;;,1]]
]

FilterHoles[img_]:=Module[{prop},
    prop = ComponentMeasurements[img, "Holes"];
    Select[prop, #[[2]]==0&][[;;,1]]
]





(* ::Section:: *)
(*The End*)


End[ ]

EndPackage[ ]
