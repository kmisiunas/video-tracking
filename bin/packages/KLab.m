(* ::Package:: *)

(*
  # Description

  Standard formats for importing and exporting trajectories with additional information saved in the files.
  The code is compatible with old versions of KLab that predated Wolfram Language implementations.
  Since new file standard aim to compress the data into and use .zip extenssion

  # Author and Copyright

  Karolis Misiunas karolis@misiunas.com

  # Requirements

  Mathematica 10.2 for RawJSON

	# Versions

	1.0 - initial release. Designed around KLab 0.1.7
  2.0  (2014-09-10) - Supports importing track assemblies directly. Improved AlignTracks
  3.0  (2015-03-11) - Supports importing tracks with additional properties
                      Import[file_, "KLab"] now a registered function
  3.1  (2016-01-07) - Renamed from "KLab" to "KLabTracking" to reflect package usege with track importing only
  4.0  (2016-07-08) - Made part of vidoe-tracking where new linking resides
                      New Version 3 standard, in which tracks are specified in the base layer witn numeric ids
                      New auto compression standard with .zip extenssion


 *)

BeginPackage["KLab`", {"Linking`"}];

(*important*)
KLab::usage =
  "Import[file, \"Tracks\"] imports tracks from json of zipped json file.
  |Import[file, \"Tracks\", \"Info\" -> True] gets meta data descriptions for the track
  |Import[file, \"Tracks\", \"PrintInfo\" -> True] prints metadata to screen";

KLab::invalidTrack = "Suplied trach does not conform to KLab standard format.";
KLab::unrecognisedExportExtenssion = "Please use .json or .zip extenssion";
KLab::commandDeprecated =
    "Command no longer supported. Use Import[ file, \"Klab\" , \"Info\" -> True, \"PrintInfo\" -> True] instead ";
KLab::jsonFormatError = "Problem detected with internal json specification. No version supplied.";

TrackAssembly::usage =
    "TrackAssembly[tracks_] links multiple tracks into one assembly
    |TrackAssembly[tracks_, \"\"] links multiple tracks into one assembly";

TrackAssembly::noOfFramesChanged =
    "Number of frames changed during TrackAssembly[] from `1` to `2`."

AlignTracks::usage =
    "AlignTracks[tracks_{{t,x,y?}...}] aligns given tracks into a matrix according to their time component.
    |All tracks must have same number of components!";




Options[Klab] = {
  "Info" -> False,
  "PrintInfo" -> False
};

(* Implementations *)
Begin["`Private`"];

(* ====================================== Import[] ================================== *)

(* relic *)
ImportExport`RegisterImport[
  "KLab",
  {
    "Tracks" :> Message[KLab::commandDeprecated]; Return[] ,
    "Info" :> Message[KLab::commandDeprecated]; Return[],
    importAutomatic (*default importer *)
  }
];

ImportExport`RegisterImport["Tracks", importAutomatic ];

Options[importAutomatic] := Options[Klab];
(*function that deterimes the version and acts accordingly*)
importAutomatic[filename_String, opts: OptionsPattern[] ] :=
  Switch[ FileExtension[filename],
    "zip", (* supported only from V3+ *)
      importTracks[filename, opts],

    "json",
      If[ determineVersion[filename, opts] >= 3, importTracks[filename, opts], importTracksOld[filename, opts]],

    _ ,  (* not recognised as KLab track *)
      Message[KLab::unrecognisedExportExtenssion]; Abort[]
  ];

Options[determineVersion] := Options[Klab];
determineVersion[filename_String, opts: OptionsPattern[] ] := Module[
  {json},
  json = Import[filename, "RawJSON"];
  Which[
    KeyExistsQ[json, "Version"], json["Version"],
    KeyExistsQ[json, "TrackAssembly"] && KeyExistsQ[json["TrackAssembly"], "version"], json["TrackAssembly"]["version"],
    _ , Message[KLab::jsonFormatError]; Abort[]
  ]
];


(* Since Version 3 => new versions of KLab tracks *)

Options[importTracks] := Options[Klab];
importTracks[filename_String, opts: OptionsPattern[] ] := Module[
  {json, tracks, info},
  json = Switch[ FileExtension[filename],
    "zip", ImportString[ Import[filename, First@Import[filename] ] , "RawJSON"] ,
    "json", Import[filename, "RawJSON"]
  ];
  tracks := KeyMap[ToExpression, KeySelect[ json , IntegerQ@ToExpression[#] &] ];
  info := KeySelect[ json , Not@IntegerQ@ToExpression[#] &] ~ Append ~ ( "Length" -> Length[tracks] );
  If[ OptionValue["PrintInfo"], Print@printTrackInformation[ info, opts ]; ];
  If[ OptionValue["Info"], info , tracks ]
];

printTrackInformation[assoc_, opts: OptionsPattern[]]:= Panel@Grid[
  Transpose@{Keys[assoc], Values[assoc]}
];


(* For old versions of KLab (v<=2*)

Options[importTracksOld] := Options[Klab];
importTracksOld[filename_String, opts: OptionsPattern[] ] := Module[
  {raw,  tracks, info},
  raw =  Import[filename, "RawJSON"]["TrackAssembly"];(*structure sensitive - KLab 0.3 tested*)
  info :=  KeyDrop[ raw , "Track"];
  tracks  := <| {"id" -> "positions"} /. # & /@ raw["Track"] |>;
  If[ OptionValue["PrintInfo"], Print@printTrackInformationOld[ info ]; ];
  If[ OptionValue["Info"], info , tracks]
];

Options[printTrackInformationOld] := Options[Klab];
printTrackInformationOld[assoc_] := Panel[Column[
  {
    Text[Style["KLab: Imported Track Assembly", FontSize -> 14, FontFamily -> "Helvetica"]],
    "time" -> DateObject[assoc["time"]/1000 + 2208988800]
  } ~ Join ~
      ((# -> assoc[#]) &/@ {"experiment", "comment", "units", "array_format", "number"})
]];

(* ====================================== Export[] ================================== *)

ImportExport`RegisterExport["KLab", ExportFuntion];

Options[ExportFuntion] = {
  "FittingParameters" -> None,
  "LinkingParameters" -> None,
  "Comment" -> None,
  "File" -> None,
  "UnlinkedPos" -> None,
  "Units" -> {"frame", "px_x", "px_y", "px_z", "probability", "px"},
  "Format" -> {"FrameId", "x", "y", "z", "Quality", "Size", "Angle(optional)","Ellongation(optional)"}

};

ExportFuntion[filename_, data_, opts: OptionsPattern[]  ] := Module[
  {json, addParamter},
  (* check for validate data here Todo: more explicit quality control *)
  If[Not@AssociationQ[data] , Message[KLab::invalidTrack]; Abort[]];
  json = KeyMap[ToString, data];
  addParamter[name_]:= If[OptionValue[name] =!= None, PrependTo[json, name -> OptionValue[name]]  ];
  addParamter /@ {"FittingParameters", "LinkingParameters", "Comment", "File", "UnlinkedPos", "Units", "Format"};
  PrependTo[json, "TimeStamp" -> DateString["ISODateTime"]];
  PrependTo[json, "Version" -> 3 ];
  Switch[ FileExtension[filename],
    "zip" ,
      Export[filename,  {FileBaseName[filename]~~".json" -> ExportString[json, "RawJSON", "Compact"->True]}, "Rules"],
    "json" ,
      Export[filename, json, "RawJSON", "Compact" -> False] ,
    _ ,
      Message[KLab::unrecognisedExportExtenssion]; Abort[]
  ]
];


(* ====================================== AlignTracks[] ================================== *)

SelectSameSize[matrix_] := With[
  {length = Length@Transpose@RandomSample[matrix, 5]},
  Select[ matrix, Length@# == length &]
];

(* error might be here - observed non-deterministic behaviour - not investigated *)
AlignTracks[tracks_] :=
    Prepend[Flatten[#[[All, 2 ;;]]], #[[1, 1]]] & /@
        SplitBy[ SortBy[ Flatten[ tracks, 1], First], First] // SelectSameSize;

AlignTracks[tracks_?( MatchQ[#, {_Integer -> _, ___}] &)] := AlignTracks[ tracks[[All, 2]] ];


(* ====================================== TrackAssembly ================================== *)

Options[TrackAssembly] = {
  "KeepOrigianlFrames" -> True,
  "FrameGap" -> 1000,
  "Linking" -> True,
  "KeepOriginalIds" -> False,
  "Debug" -> False
}

TrackAssembly[files_List/;VectorQ[files, StringQ], opts: OptionsPattern[] ] := Module[
  {all},
  all = Import[#, "Tracks"] &/@ Select[files, FileExistsQ];
  Print["Imported ", Length[all] ,"/", Length[files], " files with tracks"];
  TrackAssembly[all, opts]
];


TrackAssembly[ tracks_List/;VectorQ[tracks,AssociationQ], opts: OptionsPattern[] ] := Module[
  {flat, newFrameIds, final },
  $RecursionLimit = 10000;
  newFrameIds = If[ OptionValue["KeepOrigianlFrames"], tracks, changeTimes[tracks, 1, OptionValue["FrameGap"]] ];
  flat = If[ OptionValue["KeepOriginalIds"],
    Join@@newFrameIds,
    AssociationThread[Range@Length[#] -> #] &@ Flatten[Values/@newFrameIds, 1]
  ];
  final = If[ OptionValue["Linking"] , Linking[flat, opts], flat];
  (* inspection *)
  If[Total[Length/@Values@final] =!= Total[Length/@Flatten[Values/@tracks, 1]] ,
    Message[TrackAssembly::noOfFramesChanged, Total[Length/@Flatten[Values/@tracks, 1]],Total[Length/@Values@final] ]
  ];
  final
];

changeTimes[ listOftracks_List , from_ , gap_ ] := Module[
  {tracks, min, max, new, adjustTrack},
  tracks = First[listOftracks];
  {min, max} = MinMax[ Flatten[ Values@tracks, 1][[All, 1]] ];
  adjustTrack[track_] := Map[ Join[ {#[[1]] - min + from} , #[[2;;]] ]&, track];
  new = adjustTrack/@tracks;
  Append[ changeTimes[Rest[listOftracks], from + gap + (max - min), gap] , new ]
];

changeTimes[{}, _, _] := {};

End[ ];

EndPackage[ ]



