(* ::Package:: *)

(* ::Title:: *)
(*Video Traking Package*)


(* ::Subtitle:: *)
(* by Karolis Misiunas *)


(* ::Text:: *)
(*Version 1 (2014-02-19) - initial release. *)
(*Version 2 (2015-03-10) - Background estimate functions exported to BackgroundEstimate.m *)


(* ::Section:: *)
(* Package Declarations*)


BeginPackage["VideoTracking`", 
  {"VideoIO`", "SubPixelFit`", "VideoSupportUI`", "VideoAnalysis`", "BackgroundEstimate`"} ]


SubstractBG::usage = 
  "substracts background image"

FrameBinarize::usage = 
  "binirizes the image, with settings"

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
    "MinThreshold" -> 0.05 (*minimum threshold for binirizing the image under automatic mode*)
}


(* ::Section:: *)
(* Implementations - Background*)

Begin["`Private`"]



SubstractBG[img_Image]:= ImageSubtract[ img, background]


(* ::Section:: *)
(*Implementations - Analysis algorithms *)

(*todo: remove IF for every try!*)
FrameBinarize[img_Image] := If[ OptionValue[VideoTracking,Threshold] === Automatic,
    Binarize[ img , Max[ FindThreshold[img] , OptionValue[VideoTracking, MinThreshold]] ] ,
    Binarize[ img , OptionValue[VideoTracking,Threshold] ]
]

(*todo: tolerate new output*)
RunAnalysis[from_:1, to_: VideoLength[]] := Module[
    {reults, blockSize, Unzipper, AnalyseBlock},

    Unzipper[{id_, res_}] := {id} ~ Join ~ # &/@ res;

    AnalyseBlock[ids_] := Module[{positions, zipped},
        PrintTemporary @ ( "Analysis: of frames ["<>ToString@Min@ids<>", "<>ToString@Max@ids<>"]" );
        If[ OptionValue[VideoTracking, UpdateBackgroung],  UpdateBackgroung[ids] ];
        positions = ParallelMap[ AnalyseFrame[#] &, ids ];
        zipped = Transpose @ {ids, positions};
        Flatten[ Map[Unzipper[#] & , zipped ], 1 ]
    ];

    blockSize = OptionValue[VideoTracking, AnalysisBlockSize];

    results = Table[ 
        AnalyseBlock @ Range[from + (i-1)*blockSize, Min[(from + i*blockSize-1), VideoLength[] ] ] ,  
        {i, 1, Ceiling[(to-from+1)/blockSize]}
    ];
    Flatten[ results, 1 ]
]

(*slow! todo: collect measurements *)
SelectParticles[img_] := Intersection[ 
    FilterSize[img, OptionValue[VideoTracking, FilterArea] ]  
    ,FilterCircularity[img, OptionValue[VideoTracking, FilterCircularity] ]
    ,FilterHoles[img] 
]

AnalyseFrame[frame_Integer] := Module[
    {img, imgWBG, takePartilces},
    img = SubstractBG @ VideoGet[ frame ];
    imgWBG = FrameBinarize @ img;
    takePartilces = SelectParticles @ imgWBG;
    ForallFitSubPixel[ img, imgWBG , takePartilces]
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
