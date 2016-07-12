(* ::Package:: *)

(* ::Title:: *)
(*ROI = Region Of Interest*)


(* ::Subtitle:: *)
(* by Karolis Misiunas *)


(* ::Text:: *)
(*This package aims to manage video import buffering and saving*)
(*Version 2.0 (2015-03-09) - Exported from VideoIO.m package 
                             Compatibility with Mathematica's region specification*)
(*Version 3.0 (2016-07-07) - No variables stored in background -> all variables are retured *)

(* ==Specs== *)
(*Make ROI the same as Mathematica in built function: { lower left corner, top right corner} *)

(* ::Section:: *)
(* Package Declarations*)


BeginPackage["ROI`", {"FFmpeg`"}]

(* ---  ROI functions  --- *)

ROISelect::usage =
		"UI for Region Of Interest selection"

ROIShow::usage = 
		"ROIShow[roi_: videoROI, frame_Int: 1] shows ROI on current video file"

ROIImageTake::usage = 
		"ROIImageTake[img_, roi_:ROICurrent]  returns a smaller image that was cut with rectangular ROI (not final!)"

ROIFullImage::usage = 
	"ROIFullImage[] returns ROI for entire raw video"

ROIQ::usage = 
  "ROIQ[roi_] returns True if input is formatted as ROI"

ROIDimmensions::usage = 
  "ROIDimmensions[] gives width and height of ROI"

(* ::Section:: *)
(*Package Implementations - Public*)


Begin["`Private`"]


(* ::Section:: *)
(*ROI function implementations*)


ROISelect[filename_, roiOld_:{}] := Module[
  {width, height, img, pos, rawWidth, rawHeight, roi},

  img = FFImport[filename, {"ImageList", 1}];
  {rawWidth, rawHeight} = ImageDimensions[img];
  roi = If[ROIQ[roiOld], roiOld, {{0,0}, {rawWidth, rawHeight}} ];
  {width, height} = ROIDimmensions@roi;
  pos = Round[ (roi[[1]] + roi[[2]])/2 ];
  Speak["Please select region of intrest."];

 DialogInput[DialogNotebook[
   {
     Grid[Transpose@{{
         Row[{
           Panel@Grid[{
              {TextCell["ROI Position"]},
              {"x", 
               Manipulator[
                Dynamic@pos[[1]], 
                {1, rawWidth, 1}], Dynamic@pos[[1]]},
              {"y", 
               Manipulator[
                Dynamic@pos[[2]], 
                {1, rawHeight, 1}], Dynamic@pos[[2]]}
              }] ,
           Panel@Grid[{
              {TextCell["ROI dimmensions"]},
              {"width", 
               Manipulator[
                Dynamic@width, 
                {1, rawWidth, 1}], Dynamic@width},
              {"height", 
               Manipulator[
                Dynamic@height, 
                {1, rawHeight, 1}], Dynamic@height}
              }],
           Button["Select\nROI", DialogReturn@ROICreateRect[Round@pos, {width, height}] ,
             ImageSize-> {Automatic, 70} , Alignment -> Center]
           },  " ", Alignment -> {Center,	Center}],

         Row[{
         (*image and bounds + (optional) Locator*)
         Dynamic@Show[
           ROIShow[ ROICreateRect[Round@pos, {width, height}] , img],
           (*Graphics[Locator[Dynamic@pos2]],*)
           
           ImageSize -> Large
         ],

         (* small image of croped area*)
        Dynamic@Show[
          ImageTrim[ img, ROICreateRect[ Round@pos, {width, height}] ],
          ImageSize -> Small
        ]
        }, "   "]
         
      }}
      ]
    },
   WindowTitle -> "Select Region Of Intrest"
   ]
  ]
 ]


ROICreateRect[pos_, size_] := Round @ {
  	{pos[[1]] - Floor[size[[1]]/2], 	pos[[2]] - Floor[size[[2]]/2]},
  	{pos[[1]] + Ceiling[size[[1]]/2], 	pos[[2]] + Ceiling[size[[2]]/2]}
  };


ROIDimmensions[roi_?ROIQ] := roi[[2]] - roi[[1]];

ROIQ[roi_] := MatchQ[roi, {{_Integer, _Integer}, {_Integer, _Integer}}];


ROIShow[roi_, img_Image] := Block[
  {height, width, com},
  {width, height} = ImageDimensions@img;
  com = (roi[[1]] + roi[[2]]) / 2;
  Show[
    img, 
    Graphics[{
      (*contours*)
      Opacity[0],EdgeForm[Directive[Red,Opacity[1]]], Rectangle@@roi, 
      (*points indicating bounds*)
      Opacity[1], Red, PointSize[Medium], Point /@ roi,
      (*center of mass *)
      Orange,
      Line@{com - {10, 0}, com + {10, 0}},
      Line@{com - {0, 10}, com + {0, 10}}
    }],
   ImageSize -> Medium]
  ];





(* ::Section:: *)
(*The End*)


End[ ]

EndPackage[ ]
