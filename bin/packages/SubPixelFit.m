(* ::Package:: *)

(* ::Title:: *)
(*Sub Pixel position determining algorithm*)


(* ::Subtitle:: *)
(* by Karolis Misiunas *)


(* ::Text:: *)
(*The package is for fitting a sub pixel resolution to an image*)
(*Version 1 (2014-03-08) - initial release. *)
(*Version 2 (2014-04-29) - introduced "quality" parameter that gives 
  the probability of the fit being correct.*)
(*Version 2.1 (2015-03-01) - trim output precision to reflect limited accuracy of sub pixel fit*)
(*Version 3 (2015-03-06) - Output restructured: {x,y,z, correctness, size, angle, flattening}
                           Changed ImageToList to have index from 1
                           Updated the sub pixel fitting routines 
                           Added ExpandBox[] method because Gaussian fitting prefers larger areas *)
(*Version 3.1 (2015-12-15) - Increased rounding of output to 0.001 to better account for errors *)

(*== Specs ==*)
(* Output: List of {x,y,z, correctness, size*, angle*, flattening*} *)

(* ::Section:: *)
(* Package Declarations*)


BeginPackage["SubPixelFit`"]

(* ---  Declarations  --- *)

ImageToList::usage = "gives image as a list {x,y, intensity}"

FitSubPixel::usage = "FitSubPixel[img_, box_] fits a sub pixel routine to the particle inside the box"

ForallFitSubPixel::usage = 
  "ForallFitSubPixel[img_Image, binImg_Image, ids_] finds multiple sub-pixel resolution peaks 
  (Automatic box selection - optimised for multiple particles)"



(* ::Section:: *)
(*Package Implementations - Public*)


Begin["`Private`"]

(* ::Section:: *)
(*Implementation*)


ImageToList[img_Image] := 
 Flatten[MapIndexed[{#2[[2]], #2[[1]], #1} &, 
   Reverse[ImageData[img, "Byte"], 1], {2}], 1] 


FitSubPixel[img_Image, box_] := 
  GetPosition[ SPFGaussianOptimised@ImageTrim[ img, # + {{0.5, 0.5}, {-0.5, -0.5}}], # ] &@ N[box]

(*private: absolute position of particles in the frames*)
GetPosition[res_, box_] := 
  N@Round[  (res[[{1,2}]] + box[[1]]) ~ Join ~ res[[3;;]] , 0.001]

(*todo: add judgement on box size!*)
ForallFitSubPixel[img_Image, binImg_Image, ids_] := 
  FitSubPixel[img, ExpandBox[#,1]] &/@ (ids /. ComponentMeasurements[binImg, "BoundingBox"])

(*private: method expands bounding box*)
ExpandBox[box_, add_] := {
  { Max[ 0, box[[1,1]] - add ] , Max[ 0, box[[1,2]] - add ] },
  box[[2]] + {add, add} (*ImageTrim handles larger boxes automatically*)
} 



(*Main fitting methods - tested on 2015-03-06*)

SPFGaussianFixed[img_Image] := Block[
  {data, fit, performFit, a, y, x, mx, my, s, dimensions, 
   failedResults},
  data = ImageToList[img];
  dimensions = ImageDimensions[img];
  performFit := FindFit[data,
    a Exp@(-(-my + y)^2/(2 (s^2)) - (-mx + x)^2/(2 s^2)),
    {{a, Max@data}, {mx, dimensions[[1]]/2}, {my, 
      dimensions[[2]]/2}, {s, Mean@dimensions/2}}, {x, y},
    AccuracyGoal -> 3, PrecisionGoal -> 3];
  failedResults := {dimensions[[1]]/2 , dimensions[[2]]/2, 0 , 
    dimensions[[2]]/2};
  fit = Quiet[Check[performFit, Return@failedResults]];
  {mx, my, s} = {mx, my, s} /. fit ;
  (*deal with errors*)
  
  If[mx <= 0 || mx >= dimensions[[1]] || my <= 0 || 
    my >= dimensions[[2]] || s > 0.66*Mean@dimensions, 
   Return@failedResults];
  (*format the reply*)
  {mx - 0.5, my - 0.5, 0,
   (*fit quality*)
   1,
   (*size*)
   Abs[s]
   }
  ]

SPFGaussianOptimised[img_Image] := Block[
  {data, fit, performFit, a, angle, y, x, mx, my, sx, sy, dimensions, 
   p, mx0, my0},
  data = ImageToList[img];
  dimensions = ImageDimensions[img];
  (*find early position guess - centroid*)
  p = data[[All, 3]] // N;
  p /= Total[p];
  mx0 = data[[All, 1]]~Dot~p;
  my0 = data[[All, 2]]~Dot~p;
  (*define fit*)
  performFit := FindFit[data,
    a E^(-(((-my + y) Cos[angle] - (-mx + x) Sin[
                angle])^2/(2 sy^2)) - ((-mx + x) Cos[
              angle] + (-my + y) Sin[angle])^2/(2 sx^2)),
    {{a, Max@data}, {angle, 0.0}, {mx, mx0}, {my, my0}, {sx, 
      dimensions[[1]]/2}, {sy, dimensions[[2]]/2}}, {x, y},
    AccuracyGoal -> 3, PrecisionGoal -> 3];
  (*fit*)
  fit = Quiet[Check[performFit, $Failed]];
  (*if failed use simpler routine to find at least the positions*)
  
  If[fit === $Failed, Return@SPFGaussianFixed[img] ];
  {mx, my, sx, sy, angle} = {mx, my, sx, sy, angle} /. fit ;
  (*check parameters - if off use simpler routine*)
  
  If[mx <= 0 || mx >= dimensions[[1]] || my <= 0 || 
    my >= dimensions[[2]] || sx > 0.77*dimensions[[1]] || 
    sy > 0.77*dimensions[[2]],
    Return@SPFGaussianFixed[img] ];
  If[mx <= 0 || mx >= dimensions[[1]] || my <= 0 || 
    my >= dimensions[[2]] || sx > 0.77*dimensions[[1]] || 
    sy > 0.77*dimensions[[2]],
    Print@"you should never read this message"];
  (*format the otput*)
  {
   (*position*)
   mx - 0.5, my - 0.5, 0,
   (*fit quality*)
   1 (*bad one elliminated for tother routines*),
   (*size*)
   Sqrt@Abs[sx *sy],
   (*angle*)
   Mod[If[sx > sy, angle, angle + Pi/2.0], N@Pi],
   (*flattening*)
   1.0 - Abs@If[sx > sy, sy/sx, sx/sy]
   }
  ]



(* ::Section:: *)
(*The End*)


End[ ]

EndPackage[ ]
