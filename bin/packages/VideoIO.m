(* ::Package:: *)

(* ::Title:: *)
(*Video IO Tools Package*)


(* ::Subtitle:: *)
(* by Karolis Misiunas *)


(* ::Text:: *)
(*This package aims to manage video import buffering and saving*)
(*Version 1.0 (2014-02-19) - initial release. *)
(*Version 1.1 (2014-11-06) - improved performance for selecting ROI. *)
(*Version 2.0 (2015-03-09) - Renamed functions sot he first name indicates idea space: ROI..., Video...
                             Produces Frame id counter - and scans for id where possible 
                             ROI.m exported as separate package *)

(* ::Plan for future::*)
(*
  1. Add multiple ROIs or images by having packs. If not specified - use default 
  2. Deal with stream: GetNextFrame[], NextFrameQ[], SetNextFrame[]
  3. Load video persistence techniques: combine old method with load entire video. 
  4. Add Frame number reading to all Frame loading routines
*)


(* ::Section:: *)
(* Package Declarations*)


BeginPackage["VideoIO`", {"FFmpeg`", "VideoAnalysisHelpers`", "ROI`"}]


(* ---  Get Frame functions  --- *)


VideoGet::usage = 
  "VideoGet[no_ || range_] returns a buffered video frame that was cut to ROICurrent[]. 
  Can pass a flag \"NoBuffer\" to have unbuffered read from file."

VideoGetRaw::usage = 
  "VideoGetRaw[no_ || range_] loads a frame straight from VideoFile[] file with no processing"  

VideoFrameID::usage =
  "VideoFrameID[no_(optional)] returns id of a frame or full list of them. 
  (if there was none set by video will give 1..VideoLength[])"

VideoFile::usage = "the video file under analysis"

VideoSelect::usage = 
		"VideoSelect[file] loads the video file into analysis.
		If file is omitted, it will bring dialogue."

VideoSelect::nofile = "file does not exist.";

VideoBufferAll::usgae = 
  "VideoBufferAll[] attempts to buffer all the images in to the memory (efficiently)"

VideoClearBuffer::usage =
  "VideoClearBuffer[] clears entire buffered data"

VideoLength::usage =
  "VideoLength[] returns number of frames in buffered video"

VideoAddProcessRawFrame::usage = 
  "VideoAddProcessRawFrame[ func[no_Integer, img_Image] ] allows to add pre-processing step 
  that is called each time Raw frame is processed in early stages (good for additional operations)"

(* ---  Global variables functions  --- *)

(* options associated with analysis *)
Options[VideoIO] = { 
  "BufferSizeMB" -> 1000 (*number of frames to store in the buffer*),
  "BufferBlockSize" -> 400 (*number of frames to load into memory at one time *),
  "FrameIdFromFrame" -> False (*frame top left corner has frame id *)
}

(* ::Section:: *)
(*Package Implementations - Public*)


Begin["`Private`"]


(* ::Section:: *)
(*Get frames Implementations*)

(*initial values*)
videoFile = ""; numberOfFrames = 0; framesIds = {};
preprocessRawFrames = {}; (*types of functions to apply for pre processing raw frames*)


(*private: process RAW frame - including pre-processing state*)
VideoProcessRawFrame[no_Integer, img_Image] := (
  #[no, img] &/@ preprocessRawFrames;
  ColorConvert[ ImageTrim[img, ROICurrent[] + {{0.5, 0.5}, {-0.5, -0.5}}], "Grayscale"]
)


VideoAddProcessRawFrame[fn_] := Module[{},
  preprocessRawFrames = DeleteDuplicates@Append[preprocessRawFrames, fn];
  Print["VideoAddProcessRawFrame: new function count is "<>ToString@Length@preprocessRawFrames]
]


VideoFile[] := videoFile

VideoSelect[file_String] := 
	If[ FileExistsQ@file, PrepareVideoInput[file],	Message[VideoSelect::nofile, file] ]
VideoSelect[] := VideoSelect[ SystemDialogInput["FileOpen", Directory[]] ]

PrepareVideoInput[file_String] := Module[ {img, modifyFrameID},
  numberOfFrames = FFImport[file, {"FrameCount"}];
  dimensions = FFImport[file, {"ImageSize"}]; 
  videoFile = file;
  framesIds = Range@numberOfFrames;
  ROISelect[{{0,0}, dimensions } ];
  If[ OptionValue[VideoIO, "FrameIdFromFrame"], (*try reading frame ids along the way?*)
    modifyFrameID[no_Integer, img_Image] := (framesIds[[no]] = VideoReadFrameID[img]);
    VideoAddProcessRawFrame @ modifyFrameID
  ]
  VideoClearBuffer[];
  FileNameTake[file] <> " has "<>ToString@numberOfFrames <> 
    " frames and is "<> ToString@dimensions[[1]] <>"x"<>ToString@dimensions[[2]]
]

VideoLength[] := numberOfFrames


(* Descrption:
    The buffer loads cropped images into the memory - it does that in a separate processor 
    Performance wise this might not be the smartest way *)
VideoGet[frame_Integer] := (
  If[ ! BufferContainsQ[frame] , AskBufferFor[frame] ];
  bufferVideo[[ WhichBlock[frame], frame - (WhichBlock[frame] - 1)*blockSize ]]
)

VideoGet[range_?VectorQ] := VideoGet/@range
VideoGet[frame_Integer, "NoBuffer"] := VideoProcessRawFrame@VideoGetRaw@frame
VideoGet[range_?VectorQ, "NoBuffer"] := VideoProcessRawFrame/@VideoGetRaw/@range

VideoGetRaw[frame_Integer] := FFImport[ VideoFile[], {"Frames", frame} ]
VideoGetRaw[range_?VectorQ] := FFImport[ VideoFile[], {"Frames", range} ]

VideoFrameID[] := framesIds
VideoFrameID[no_Integer] := framesIds[[no]]

(* ::Section:: *)
(*Buffering*)


WhichBlock[frame_] := Ceiling[ (frame - 0.5)/blockSize ]

VideoClearBuffer[] := Module[ {} ,
  Clear[bufferVideo];
  (*prepare for new*)
  blockSize = OptionValue[VideoIO, "BufferBlockSize"];
  singleFrameSize = ByteCount @ VideoGet[1, "NoBuffer"];
  bufferVideo = Array[{} &, 1000];
  bufferVideoTimeStamp = Array[0 &, 1000];
  (*other stuff*)
  If[singleFrameSize*blockSize /10^6 > OptionValue[VideoIO, "BufferBlockSize"],
    Print@"Warrning: buffer too small for current ROI."  ]
]

AskBufferFor[frame_] := If[ bufferVideo[[ WhichBlock @ frame ]] == {} , LoadBufferBlock[ frame ] ]

(*loads files into memory buffer. Before loading checks memory consumption*)
LoadBufferBlock[frame_] := Module[{from, to},
  If[ BufferMemorySize[] > OptionValue[VideoIO, BufferSizeMB] , MakeSpaceInBuffer[] ];
  from = Floor[frame - 0.5, blockSize] + 1 ;
  to = Min[ Ceiling[frame - 0.5, blockSize] , VideoLength[] ] ;
  PrintTemporary @ ("Buffer: loading frame block: ["<>ToString@from<>", "<> ToString@to<>"]");
  bufferVideo[[ WhichBlock @ frame ]] = VideoGet[ Range[from, to] , "NoBuffer"] ;
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
VideoBufferAll[] := Module[{dim, stream, res, size, newFrameCount, expectedNoOfFrames, images},
  size = OptionValue[VideoIO, "BufferBlockSize"];
  expectedNoOfFrames = FFImport[ VideoFile[], "FrameCount"];
  {stream, dim} = FFInputStreamAt[ VideoFile[], 1, All]; (*might cause error because of order!*)

  res = 
    Reap @ Catch @ Do[ (*run though entire file till there is no frames*)
      Quiet @ Check[ 
        If[ Divisible[i,500], PrintTemporary["Buffer: loaded "~~ToString@i~~" frames"] ];
        Sow @ VideoProcessRawFrame[ i, FFGetNextFrame[stream, dim] ]
        , Throw[i-1] ],
      {i, expectedNoOfFrames}
    ];
  images = If[ res[[1]] == Null, res[[2,1]], res[[2,1, ;;res[[1]] ]] ];
  newFrameCount = Length@images;
  Print["Buffer: expected " ~~ ToString@expectedNoOfFrames ~~ 
        " frames. ffmpeg found " ~~ ToString@newFrameCount ];
  (*warn if the difference is very large*)
  If[ Abs[newFrameCount/expectedNoOfFrames -1] > 0.05, 
      Print@Style["Warring: Loaded less than 95% of expected frames. FFMPEG might have failed.", Red]
  ];
  numberOfFrames = newFrameCount; (*update number of frames*)
  framesIds = framesIds[[ ;;VideoLength[] ]]; (*shorten the list to correspond to the new buffer*)
  (*update buffer*)
  bufferVideo = Partition[ images , size, size, 1, {}];
  bufferVideoTimeStamp[[ ;; Length@bufferVideo ]] = AbsoluteTime[];
  Close[stream];
  Print @ "Buffer: loaded entire video";
]

(* ::Section:: *)
(*The End*)


End[ ]

EndPackage[ ]
