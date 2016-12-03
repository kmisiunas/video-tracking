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
(*Version 3.0 (2015-11-14) - Allow to specify Import library (eg. Import or FFImport) *)
(*Version 3.1 (2015-12-04) - Stable under unexpected FF stream closure  *)
(*Version 3.2 (2016-01-08) - TIF stack support:
                             Migrated from importing using "Frames" -> "ImageList" for tiff stack support*)
(*Version 4.0 (2016-07-07) - Moved to Video file format                        *)
(*Version 4.1 (2016-12-01) - Usage of Internal`Bag to reduce breaking   *)

(* ::Plan for future::*)
(*
  1. Add multiple ROIs or images by having packs. If not specified - use default 
  2. Deal with stream: GetNextFrame[], NextFrameQ[], SetNextFrame[]
  3. Load video persistence techniques: combine old method with load entire video. 
  4. Add Frame number reading to all Frame loading routines
*)


(* ToDo for TIF :
    1. ImageSize returns list of sizes. Need to catch this.
    2. "FrameCount" does not work, must use	"ImageCount"
*)

(* ::Section:: *)
(* Package Declarations*)


BeginPackage["VideoIO`", {"FFmpeg`", "Video`"}];

$HistoryLength = 2; (*prevent memory overload*)

SetOptions[FFmpeg,
  "Colors"->1 (*number of color channels*),
  "ColorCommand"->"gray" (*indicator for "-pix_fmt" parameter:gray/rgb24*)
];

VideoIO::usage =
    ""

VideoReadFrameID::usage =
    "VideoReadFrameID[image_] returns video frame ID from 4 pixels in top right corrner of a frame"


VideoIO::noFramesLoaded = "No video frames loaded. Aborting.";
VideoIO::roiNotRecognised = "The supplied ROI was not recognised.";
VideoIO::multipleROIs = "Multiple ROIs currently disabled";
VideoIO::missingFrames = "FFmpeg stream broke. File should contain `1` frames, but FFmpeg loaded only `2`";
On[VideoIO::missingFrames];
On[VideoIO::noFramesLoaded];

(* ---  Global variables functions  --- *)

(* options associated with analysis *)
Options[VideoIO] = {
  "FrameIdFromFrame" -> False (*frame top left corner has frame id *),
  Method -> "ffmpeg" (*function to use for importing images*),
  "ROI" -> {{0,0},{100000,100000}}
};

(* ::Section:: *)
(*Package Implementations - Public*)


Begin["`Private`"];


ImportExport`RegisterImport["Video", importWithROIs ];


(* ::Section:: *)
(*Get frames Implementations*)
Options[importWithROIs] = Options[VideoIO];

(* mighty video importer. Returns: Video[] *)
importWithROIs[ filename_String, opts: OptionsPattern[] ]  := Which[
  FileExtension[filename] == "tif" || FileExtension[filename] == "tiff",
    naitiveImportWithROIs[normaliseROI[OptionValue["ROI"]],  filename, opts ],
  ToLowerCase@OptionValue[Method] === "ffmpeg",
    ffImportWithROIs[normaliseROI[OptionValue["ROI"]], filename, opts ] ,
  True (*default*),
    naitiveImportWithROIs[normaliseROI[OptionValue["ROI"]], filename, opts ]
];

normaliseROI[roi_] := (Message[VideoIO::roiNotRecognised];Print[roi]; roi);
normaliseROI[roi : {{_?NumberQ, _?NumberQ}, {_?NumberQ, _?NumberQ}}] := {roi};
normaliseROI[rois : {{{_?NumberQ, _?NumberQ}, {_?NumberQ, _?NumberQ}}, ___}] :=
    (Message[VideoIO::multipleROIs]; Abort[]);


(* key importer *)
Options[ffImportWithROIs] = Options[VideoIO];
ffImportWithROIs[rois_, filename_String, opts: OptionsPattern[] ] := Module[
  {dim, stream, size, numberOfFrames, expectedNoOfFrames, images, bagImages, bagIds, result},
  expectedNoOfFrames = FFImport[ filename, "FrameCount"];
  {stream, dim} = FFInputStreamAt[ filename, 1, expectedNoOfFrames]; (*might cause error because of order!*)
  bagImages = Internal`Bag[Range[expectedNoOfFrames]];
  bagIds =    Internal`Bag[Range[expectedNoOfFrames]];
  Monitor[ (* Show the progress to the user *)
    result = (* Format { {t, Image(roi1), Image(roi2)___}, image 2 and so on} *)
        Quiet @ Catch @ Do[ (*run though entire file till there is no frames*)
          {Internal`BagPart[bagIds, i], Internal`BagPart[bagImages, i]} =
              CheckOutputFormat[i] @ VideoProcessRawFrame[rois, i, FFGetNextFrame[stream, dim], opts ] ,
          {i, expectedNoOfFrames}
        ],
    Row[{"Loading video ", ProgressIndicator[i, {1, expectedNoOfFrames}]}]
  ]; (*end Monitor*)
  Close[stream];
  (* Check for problems *)
  If[IntegerQ[result] && result === 0, Message[VideoIO::noFramesLoaded]; Abort[]];
  If[IntegerQ[result] && result > 0  ,
    bagIds = Internal`Bag[Internal`BagPart[bagIds, 1;;result]];
    bagImages = Internal`Bag[Internal`BagPart[bagImages, 1;;result]];
  ];
  (*warn if there were discrepencied between loaded file amounts*)
  If[ Internal`BagLength[bagImages] =!= expectedNoOfFrames ,
    Message[VideoIO::missingFrames, expectedNoOfFrames, numberOfFrames];
    Print @ Style["[Warning] Vidoe file should contain " <> ToString@expectedNoOfFrames <>
        " frames, but FFmpeg loaded only " <> Internal`BagLength[bagImages] , Red];
  ];
  (* prepare video file *)
  (* todo onlu works with one ROI *)
  FormatVideoOutputBag[filename, Internal`BagPart[bagIds, All] ][ bagImages, rois[[1]] ]
];


(*private: process RAW frame - including pre-processing state*)
(* Throw an error if there is one *)
Options[VideoProcessRawFrame] = Options[VideoIO];
VideoProcessRawFrame[rois_, no_Integer, img_Image, opts: OptionsPattern[]] := Join[
  {If[ OptionValue["FrameIdFromFrame"], VideoReadFrameID[img], no ]}  ,
  (*images for all rois*)
(* todo onlu works with one ROI *)
  ColorConvert[ ImageTrim[img, # + {{0.5, 0.5}, {-0.5, -0.5}}], "Grayscale"] &/@ rois[[{1}]]
];

CheckOutputFormat[no_][in_] := If[ IntegerQ[in[[1]]] && ImageQ[in[[2]]], in, Throw[no - 1]];

FormatVideoOutputBag[filename_String, ids_][baggedImages_, roi_] := Video[<|
  "File" -> filename,
  "RawBag" -> baggedImages,
  "Ids" -> ids,
  "Length" -> Internal`BagLength[baggedImages],
  "Size" -> ImageDimensions[Internal`BagPart[baggedImages, 1]],
  "ROI" -> roi,
  "Images" -> "place holder" (*should never see this really*)
|>];


FormatVideoOutput[filename_String, ids_][images_, roi_] := Video[<|
  "File" -> filename,
  "RawBag" -> Internal`Bag[images],
  "Ids" -> ids,
  "Length" -> Length[images],
  "Size" -> ImageDimensions[First[images]],
  "ROI" -> roi,
  "Images" -> "place holder"
|>];




Options[naitiveImportWithROIs] = Options[VideoIO];
naitiveImportWithROIs[rois_, filename_String, opts: OptionsPattern[] ] := Module[
  {dim, stream, res, size, numberOfFrames, expectedNoOfFrames, images, chuncks},
  expectedNoOfFrames = If[FileExtension[filename] == "tif" || FileExtension[filename] == "tiff" ,
    Import[ filename, "ImageCount"],
    Import[ filename, "FrameCount"]
  ];
  chuncks = Partition[ Range[expectedNoOfFrames],  500, 500, 1, {}];
  {stream, dim} = FFInputStreamAt[ filename, 1, expectedNoOfFrames]; (*might cause error because of order!*)
  Monitor[ (* Show the progress to the user *)
    images = (* Format { {t, Image(roi1), Image(roi2)___}, image 2 and so on} *)
        Table[
          (CheckOutputFormat[#1] @ VideoProcessRawFrame[rois, #1, #2, opts ]) & @@ # &/@ Transpose[
            {chuncks[[i]], Import[filename, {"ImageList", chuncks[[i]]} ]}
          ],
          {i, 1, Length[chuncks]}
        ] ~ Flatten ~ 1
        ,
    Row[{"Loading video ", ProgressIndicator[i, {1, Length[chuncks]}]}] (* animation: slow? *)
  ]; (*end Monitor*)
  If[Length[images]<2  , Message[VideoIO::noFramesLoaded]; Abort[]];
  numberOfFrames = Length[images];
  (*warn if there were discrepencied between loaded file amounts*)
  If[ numberOfFrames =!= expectedNoOfFrames , Message[VideoIO::missingFrames];
    Print @ Style["[Warning] Vidoe file should contain " <> ToString@expectedNoOfFrames <>
        " frames, but Import[] loaded only " <> ToString@numberOfFrames , Red];
  ];
  (*prepare video files *)
  FormatVideoOutput[filename, images[[All, 1]] ][ #[[1]], #[[2]] ] &/@ Transpose[
    {Transpose[images[[All, 2;;]]], rois}
  ]//First
];


(* ::Section:: *)
(*The End*)


(*============ VideoReadFrameID =============*)

toInt[acc_, add_] := acc*256 + add;
VideoReadFrameID[image_Image] :=
  Fold[ toInt, 0, Reverse @ PixelValue[ColorConvert[image, "Grayscale"], {1;;4, ImageDimensions[image][[2]] }, "Byte"] ]


End[ ];

EndPackage[ ];
