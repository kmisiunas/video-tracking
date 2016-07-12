(* Mathematica Package *)
(* Created by Mathematica Plugin for IntelliJ IDEA *)

(* :Title: Video *)
(* :Context: Video` *)
(* :Author: kmisiunas *)
(* :Date: 2016-07-07 *)

(* :Package Version: 0.1 *)
(* :Mathematica Version: *)
(* :Copyright: (c) 2016 kmisiunas *)
(* :Keywords: *)
(* :Discussion:

# This is a format representing video files

This was developed based on these discussions:
http://mathematica.stackexchange.com/questions/2590/how-can-one-define-a-custom-data-object

*)



BeginPackage["Video`"]
(* Exported symbols added here with SymbolName::usage *)

Video::usage =
    "Vidoe is a video file. Inside is Association container.
    |Get the Assoiation via Normal[someVideo]. Example for creating a Video:
    |Video[<|
    |  \"File\" -> path to file,
    |  \"Images\" -> List of images,
    |  \"Length\" -> Number of images,
    |  \"Size\" -> Size of each image,
    |  \"Ids\" -> List of time stapms for each image,
    |  \"ROI\" -> ROI that the image was cut to,
    ||>]
    |
    |The data structure can be accessed using: Length, Normal, ImageDimensions, Keys";


VideoQ::usege = "Checks if file conforms to video standard";

TimeStamp::usage =
    "TimeStamp[_Video] gives all the ids  in that video
    |TimeStamp[_Video, frame_] gives ids of specific frame";


Begin["`Private`"]

MakeBoxes[Video[vid_Association], _ ] ^:=
    InterpretationBox[
      RowBox[{"Video", "[", #1, ",", #2, ", N=", #3, "]"}],
      Video[Append[vid, "Images" -> vid["Images"][[{1}]] ] ] (*drop most images for UI copying*)
    ] & @@ ToBoxes /@ {
      Tooltip[FileNameTake[#], #] &@vid["File"],
      Tooltip[ToString[vid["Size"][[1]]] ~~ "x" ~~ ToString[ vid["Size"][[2]] ] ,
        vid["Images"][[1]] ],
      vid["Length"]
    };

Video[vid_Association][[part___]] ^:= vid["Images"][[part]];

Video[vid_Association] // Length ^:= vid["Length"];

Video[vid_Association] // Normal ^:= vid;

Video[vid_Association] // ImageDimensions ^:= vid["Size"];

Video[vid_Association] // Keys ^:= Keys[vid];

Video[vid_Association][i_Integer] := vid["Images"][[i]];

Video[vid_Association][st_String] := vid[st];

Video[vid_Association]["Id", i_Integer] := vid["Ids"][[i]];

Video[vid_Association]["FrameFromId", i_] := AssociationThread[ vid["Ids"] -> Range[Length[vid["Ids"]]] ][i];

(*needs VideoIO *)
Video[vid_Association] // Import ^:= Import[vid["File"], "Video", ROI -> vid["ROI"],
  "FrameIdFromFrame" -> If[First@vid["Ids"] == 1, False, True] ];

(* === Other methods === *)

VideoQ[_] := False;
VideoQ[Video[vid_]] :=
  If[
    AssociationQ[vid] &&
        KeyExistsQ[vid, "File"] &&
        KeyExistsQ[vid, "Images"] &&
        KeyExistsQ[vid, "Length"] &&
        KeyExistsQ[vid, "Size"] &&
        KeyExistsQ[vid, "Ids"] &&
        KeyExistsQ[vid, "ROI"] ,
    Return[True],
    Return[False]
  ];

TimeStamp[Video[vid_]] := vid["TimeStamp"];
TimeStamp[Video[vid_], frame_] := vid["TimeStamp"][[frame]];

End[] (* `Private` *)

EndPackage[]