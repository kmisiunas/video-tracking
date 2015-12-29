(* ::Package:: *)

(* ::Title:: *)
(*ROI = Region Of Interest*)


(* ::Subtitle:: *)
(* by Karolis Misiunas *)


(* ::Text:: *)
(*This package aims to manage video import buffering and saving*)
(*Version 2.0 (2015-03-09) - Exported from VideoIO.m package 
                             Compatibility with Mathematica's region specification*)

(* ==Specs== *)
(*Make ROI the same as Mathematica in built function: { lower left corner, top right corner} *)

(* ::Section:: *)
(* Package Declarations*)


BeginPackage["ROI`", {"FFmpeg`", "VideoIO`"}]

(* ---  ROI functions  --- *)

ROISelect::usage = 
		"UI for Region Of Interest selection"

ROIShow::usage = 
		"ROIShow[roi_: videoROI, frame_Int: 1] shows ROI on current video file"

ROIImageTake::usage = 
		"ROIImageTake[img_, roi_:ROICurrent]  returns a smaller image that was cut with rectangular ROI (not final!)"

ROICurrent::usage = 
	"active region of interest for the analysis {{x0,y0},{x1,y1}}."

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


currentROI = {{0,0}, {0,0}}; 

ROICurrent[] := currentROI

ROIQ[roi_] := MatchQ[roi, {{_Integer, _Integer}, {_Integer, _Integer}}]

ROISelect[roi_?ROIQ] := Module[{}, 
  currentROI = roi;
  ClearBuffer[]; (*must reset buffer because places changed*)
  roi 
] 

ROISelect[] := Module[
  {width, height, img, pos, rawWidth, rawHeight}, 

  img = VideoIO`VideoGetRaw[1];
  {rawWidth, rawHeight} = ImageDimensions[img];
  {width, height} = ROIDimmensions@ROICurrent[];
  pos = Round[ (ROICurrent[][[1]] + ROICurrent[][[2]])/2 ];

 
 DialogInput[DialogNotebook[
   {
    Button["Select ROI",
     DialogReturn@ROISelect@ROICreateRect[Round@pos, {width, height}]  ],

    Panel[ Grid@Transpose@{{
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
              }]
           }, " "],

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
        }]
         
      }}]
    },
   WindowTitle -> "Select Region Of Intrest"
   ]
  ]
 ]

ROICreateRect[pos_, size_] := Round @ {
  	{pos[[1]] - Floor[size[[1]]/2], 	pos[[2]] - Floor[size[[2]]/2]},
  	{pos[[1]] + Ceiling[size[[1]]/2], 	pos[[2]] + Ceiling[size[[2]]/2]}
  }


ROIDimmensions[roi_?ROIQ] := roi[[2]] - roi[[1]]
ROIDimmensions[] := ROIDimmensions[ ROICurrent[] ]


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
  ]

ROIShow[roi_: ROICurrent[], frame_Integer: 1] := ROIShow[roi, VideoBufferedImport[frame]];

(*buffered load of image*)
bufferVideoSingleCmd = {};
VideoBufferedImport[frame_] := If[frame === bufferVideoSingleCmd,
  bufferVideoSingle,
  bufferVideoSingle = VideoGetRaw[frame]; bufferVideoSingleCmd = frame
]

(* ::Section:: *)
(*The End*)


End[ ]

EndPackage[ ]
