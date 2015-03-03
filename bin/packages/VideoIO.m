(* ::Package:: *)

(* ::Title:: *)
(*Video IO Tools Package*)


(* ::Subtitle:: *)
(* by Karolis Misiunas *)


(* ::Text:: *)
(*This package aims to manage video import buffering and saving*)
(*Version 1 (2014-02-19) - initial release. *)
(*Version 1.1 (2014-11-06) - improved performance for selecting ROI. *)

(* ::Plan for v2::*)
(*
  1. Add multiple ROIs or images by having packs. If not specified - use default 
  2. Deal with stream: GetNextFrame[], NextFrameQ[], SetNextFrame[]
  3. Load video persistence techniques: combine old method with load entire video. 


*)


(* ::Section:: *)
(* Package Declarations*)


BeginPackage["VideoIO`", {"FFmpeg`"}]

(* ---  ROI functions  --- *)

SelectROI::usage = 
		"UI for Region Of Intrest selection"

ShowROI::usage = 
		"ShowROI[roi_: videoROI, frame_Int: 1] shows ROI on current video file"

ImageTakeROI::usage = 
		"ImageTakeROI[img_, roi_:ROICurrent]  returns a smaller image that was cut with rectangular ROI (not final!)"

CreateRectROI::usage = 
		"CreateRectROI[pos_, size_] creates a rectangulat Region Of Intrest"

(* ---  Get Frame functions  --- *)

SelectVideo::usage = 
		"SelectVideo[file] loads the video file into analysis.
		If file is ommited, it will bring dialog."

SelectVideo::nofile = "file does not exist.";

GetRawFrames::usage = 
  "GetRawFrames[range_] loads frames without the buffer"

GetFrames::usage =
  "Returns a list of frames from \"videoFile\" video and cuts it using videoROI"

GetFrame::usage = 
	"Get a buffered frame from VideoTracking`videoFile"

RawVideoDimmensions::usage = 
	"returns size of input video"

NumberOfFrames::usage =
  "Returns number of frames in video file \"videoFile\""

ClearBuffer::usage =
  "ClearBuffer[] clears entire buffered data"

LoadAllFrames::usage =
  "LoadAllDrames[] loads all frames to the memory"


(* ---  Global variables functions  --- *)

VideoFile::usage = 
	"the video file under analysis"

ROICurrent::usage = 
	"active region of intrest for the analysis {{x0,y0},{x1,y1}}."

ROIFullImage::usage = 
	"roi encapsulation the entire image"

(* options associated with analysis *)
Options[VideoIO] = { 
  "BufferSizeMB" -> 1000 (*number of frames to store in the buffer*),
  "BufferBlockSize" -> 400 (*number of frames to load into memory at one time *)
}

CutFrame::usage = "todo"

(* ::Section:: *)
(*Package Implementations - Public*)


Begin["`Private`"]

(* ::Section:: *)
(*GetFrame and Buffering*)

(* Descrption:
    The buffer loads croped images into the memory - it does that in a separate processor *)

GetFrame[frame_] := (
  If[ ! BufferContainsQ[frame] , AskBufferFor[frame] ];
  bufferVideo[[ WhichBlock[frame], frame - (WhichBlock[frame] - 1)*blockSize ]]
)

WhichBlock[frame_] := Ceiling[ (frame - 0.5)/blockSize ]

ClearBuffer[] := Module[ {} ,
  Clear[bufferVideo];
  (*prepare for new*)
  blockSize = OptionValue[VideoIO, BufferBlockSize];
  singleFrameSize = ByteCount @ GetFrames[1];
  bufferVideo = Array[{} &, 1000];
  bufferVideoTimeStamp = Array[0 &, 1000];
  (*other stuff*)
  If[singleFrameSize*blockSize /10^6 > OptionValue[VideoIO, BufferBlockSize],
    Print@"Warrning: buffer too small for current ROI."  ]
]

AskBufferFor[frame_] := If[ bufferVideo[[ WhichBlock @ frame ]] == {} , LoadBufferBlock[ frame ] ]

(*loads files into memory buffer. Before loading checks memory consumption*)
LoadBufferBlock[frame_] := Module[{from, to},
  If[ BufferMemorySize[] > OptionValue[VideoIO, BufferSizeMB] , MakeSpaceInBuffer[] ];
  from = Floor[frame - 0.5, blockSize] + 1 ;
  to = Min[ Ceiling[frame - 0.5, blockSize] , NumberOfFrames[] ] ;
  PrintTemporary @ ("Buffer: loading frame block: ["<>ToString@from<>", "<> ToString@to<>"]");
  bufferVideo[[ WhichBlock @ frame ]] = GetFrames @ Range[from, to] ;
  bufferVideoTimeStamp[[ WhichBlock @ frame ]] = AbsoluteTime[];
]

(*MB used by the buffer*)
BufferMemorySize[] := singleFrameSize * Total[ Length /@ bufferVideo ] / 10^6 // N

MakeSpaceInBuffer[] := Module[ {oldestBlockId, creationDates},
  creationDates = MapIndexed[{#2[[1]], #1} &, bufferVideoTimeStamp ] ;
  oldestBlockId = SortBy[ Select[creationDates, #[[2]]>0 &], Last][[1,1]] ;
  PrintTemporary @ ("Buffer: deleting frame block: ["<>ToString@(oldestBlockId*blockSize+1)<>", "<> ToString@((oldestBlockId+1)*blockSize)<>"]");
  bufferVideo[[ oldestBlockId ]] = {};
  bufferVideoTimeStamp[[ oldestBlockId ]] = 0;
]

BufferContainsQ[frame_] := bufferVideoTimeStamp[[ WhichBlock @ frame ]] > 0 

(*loads all frames using ffmpeg!*)
LoadAllFrames[] := Module[{dim, stream, res, size, newFrameCount, expectedNoOfFrames},
  size = OptionValue[VideoIO, BufferBlockSize];
  expectedNoOfFrames = FFImport[ VideoFile[], "FrameCount"];
  {stream, dim} = FFInputStreamAt[ VideoFile[], 1, All]; (*might cause error because of order!*)

  res = 
    Reap @ Catch @ Do[ (*run though entire file til there is no frames*)
      Quiet @ Check[ 
        If[ Divisible[i,500], PrintTemporary["Buffer: loaded "~~ToString@i~~" frames"] ];
        Sow[ CutFrame @ FFGetNextFrame[stream, dim] ]
        , Throw[i-1] ],
      {i, NumberOfFrames[]}
    ];

  newFrameCount = Length@res[[2,1]] - 1;
  Print["Buffer: expected " ~~ ToString@expectedNoOfFrames ~~ 
        " frames. ffmpeg found " ~~ ToString@newFrameCount ];
  (*warn if the difference is very large*)
  If[ Abs[newFrameCount/expectedNoOfFrames -1] > 0.05, 
      Print@Style["Warring: Loaded less than 95% of expected frames. FFMPEG might have failed.", Red]
  ];
  numberOfFrames = newFrameCount; (*update number of frames*)
  (*update buffer*)
  bufferVideo = Partition[ res[[2,1]] , size, size, 1, {}];
  bufferVideoTimeStamp[[ ;; Length@bufferVideo ]] = AbsoluteTime[];
  Close[stream];
  Print @ "Buffer: loaded entire video";
]



(* ::Section:: *)
(*Get frames Implementations*)

videoFile := ""

SelectVideo[] := SelectVideo[ SystemDialogInput["FileOpen", Directory[]] ]

SelectVideo[file_String] := 
	If[ FileExistsQ @ file, 
		PrepareVideoInput[file],
		Message[SelectVideo::nofile, file] ]

PrepareVideoInput[file_String] := Module[ {img},
  numberOfFrames = Import[file,{"FrameCount"}];
  videoFile = file;
  img = GetRawFrames[1][[1]];
  widthRaw = ImageDimensions[img][[1]]; 
  heightRaw = ImageDimensions[img][[2]]; 
  ClearBuffer[];
  FileNameTake[file] <> " has "<>ToString@numberOfFrames <> 
    " frames and is "<> ToString@widthRaw <>
    "x"<>ToString@heightRaw
]

VideoFile[] := videoFile

NumberOfFrames[] := numberOfFrames

(*consider improving performance here *)
GetFrames[frames_] := CutFrame /@ GetRawFrames [ frames ]

GetRawFrames[frames_] := Flatten[ {FFImport[videoFile,{"Frames",frames}]} , 1]

GetRawFrames2[frames_] := Flatten[ {FFImport[videoFile,{"Frames",frames, True}]} , 1]

CutFrame[img_Image] := ColorConvert[ ImageTakeROI[img, ROICurrent[]], "Grayscale"]

ROIDimmensions[] := 
  { (Max@# - Min@#) &@ROICurrent[][[;; , 1]],
    (Max@# - Min@#) &@ROICurrent[][[;; , 2]] }

RawVideoDimmensions[] := {widthRaw, heightRaw}


(* ::Section:: *)
(*ROI function implementations*)


currentROI := ROIFullImage[]

ROICurrent[] := currentROI

ROIFullImage[] := {{0,heightRaw}, {widthRaw,heightRaw}, {widthRaw,0},{0,0}}


ShowROI[roi_, img_] := Module[
  {height, width, com},
  {width, height} = ImageDimensions@img;
  com = Mean /@ {roi[[;; , 1]], roi[[;; , 2]]};
  Show[
   img, 
   Graphics[{Red, Line@Append[roi, roi[[1]] ], Point /@ roi,
     (*center of mass *)
     Orange,
     Line@{com - {10, 0}, com + {10, 0}},
     Line@{com - {0, 10}, com + {0, 10}}
     }],
   ImageSize -> Medium]
  ]

ShowROI[roi_: ROICurrent[], frame_Integer: 1] := ShowROI[roi, VideoBufferedImport[frame]];

bufferVideoSingleCmd = {};

(*buffered load of image*)
VideoBufferedImport[input_] := If[input === bufferVideoSingleCmd,
	bufferVideoSingle,
	bufferVideoSingle = Import[videoFile, {"Frames", input}]
]

SelectROI[] := Module[
 {width, height, img, pos},

 {width, height, pos} = If[ROICurrent[] === ROIFullImage[],
   {100, 20, RawVideoDimmensions[]/2},
   {(Max@# - Min@#) &@ROICurrent[][[;; , 1]],
    (Max@# - Min@#) &@ROICurrent[][[;; , 2]],
    {(Mean@#) &@ROICurrent[][[;; , 1]],
     (Mean@#) &@ROICurrent[][[;; , 2]]}}
   ];
 img = FFImport[VideoFile[], {"Frames", 1}];
 
 DialogInput[DialogNotebook[
   {
    Button["Select ROI",
     DialogReturn@
      SelectROI@CreateRectROI[Round@pos, {width, height}]],


    Panel[ Grid@Transpose@{{
         Row[{
           Panel@Grid[{
              {TextCell["ROI Position"]},
              {"x", 
               Manipulator[
                Dynamic@pos[[1]], {1, RawVideoDimmensions[][[1]], 
                 1}], Dynamic@pos[[1]]},
              {"y", 
               Manipulator[
                Dynamic@pos[[2]], {1, RawVideoDimmensions[][[2]], 
                 1}], Dynamic@pos[[2]]}
              }] ,
           Panel@Grid[{
              {TextCell["ROI dimmensions"]},
              {"width", 
               Manipulator[
                Dynamic@width, {1, RawVideoDimmensions[][[1]], 1}], 
               Dynamic@width},
              {"height", 
               Manipulator[
                Dynamic@height, {1, RawVideoDimmensions[][[2]], 1}], 
               Dynamic@height}
              }]
           }, " "],

         (*image and bounds + (optional) Locator*)
         Dynamic@Show[
           ShowROI[CreateRectROI[Round@pos, {width, height}] , img],
           (*Graphics[Locator[Dynamic@pos2]],*)
           
           ImageSize -> Large
         ],

         (* small image of croped area*)
        Dynamic@Show[
          ImageTakeROI[img, 
            CreateRectROI[Round@pos, {width, height}]],
              ImageSize -> Small
        ]
         
      }}]
    },
   
   WindowTitle -> "Select Region Of Intrest"
   ]
  ]
 ]

SelectROI[roi_] := Module[{}, 
  currentROI = roi;
  ClearBuffer[];
  roi 
] 

(*designed for ordered ROI from Top-Left clockwise *)
ImageTakeROI[img_, roi_] := 
	ImageTake[img, heightRaw - {roi[[1,2]],roi[[3,2]]}, {roi[[1,1]],roi[[3,1]]}]

CreateRectROI[pos_, size_] := {
	{pos[[1]] - Floor[size[[1]]/2], 	pos[[2]] + Ceiling[size[[2]]/2]},
	{pos[[1]] + Ceiling[size[[1]]/2], 	pos[[2]] + Ceiling[size[[2]]/2]},
	{pos[[1]] + Ceiling[size[[1]]/2], 	pos[[2]] - Floor[size[[2]]/2]},
	{pos[[1]] - Floor[size[[1]]/2], 	pos[[2]] - Floor[size[[2]]/2]}  
  }

(* ::Section:: *)
(*The End*)


End[ ]

EndPackage[ ]
