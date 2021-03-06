(* ::Package:: *)

(* ::Title:: *)
(*Sub Pixel position determining algorithm*)


(* ::Subtitle:: *)
(* by Karolis Misiunas *)


(* ::Text:: *)
(*The package is for fitting a sub pixel resolution to an image*)
(*Version 1.0 (2014-03-08) - initial release. *)
(*Version 2.0 (2014-04-29) - introduced "quality" parameter that gives
  the probability of the fit being correct.*)
(*Version 2.1 (2015-03-01) - trim output precision to reflect limited accuracy of sub pixel fit*)
(*Version 3.0 (2015-03-06) - Output restructured: {x,y,z, correctness, size, angle, flattening}
                           Changed ImageToList to have index from 1
                           Updated the sub pixel fitting routines 
                           Added ExpandBox[] method because Gaussian fitting prefers larger areas *)
(*Version 3.1 (2015-12-15) - Increased rounding of output to 0.001 to better account for errors *)
(*Version 3.2 (2016-01-08) - Changed method for FindFit[] to avoid errors that occures 2.7% of the times.
                             See Piezo tracking tests for more info. *)
(*Version 3.3 (2016-07-07) - Deleted the FitSubPixelForAll[]; removed slow method as a backup. *)

(*== Specs ==*)
(* Output: List of {x,y,z, correctness, size*, angle*, flattening*} *)

(* ::Section:: *)
(* Package Declarations*)


BeginPackage["SubPixelFit`"]

(* ---  Declarations  --- *)

FitSubPixel::usage = "FitSubPixel[img_, box_] fits a sub pixel routine to the particle inside the box";

ExpandBox::usage = "";

ImageToList::usage = "gives image as a list {x,y, intensity}";


(* ::Section:: *)
(*Package Implementations - Public*)


Begin["`Private`"]

(* ::Section:: *)
(*Implementation*)


ImageToList[img_Image] := Flatten[ MapIndexed[{#2[[2]], #2[[1]], #1} &, Reverse[ImageData[img, "Byte"], 1], {2}], 1];

FitSubPixelTest[img_Image, box_] := {0.5,0.5,0.0,1.0}
FitSubPixel[img_Image, box_] :=
  GetPosition[ SPFGaussianOptimised@ImageTrim[ img, # + {{0.5, 0.5}, {-0.5, -0.5}}], # ] &@ N[box]

(*private: absolute position of particles in the frames*)
GetPosition[res_, box_] := N @ Round[ (res[[{1,2}]] + box[[1]]) ~ Join ~ res[[3;;]] , 0.001];

(*GetPosition = Compile[*)
  (*{{res, _Complex, 1}, {box, _Complex, 2}},*)
  (*N@Round[  (res[[{1,2}]] + box[[1]]) ~ Join ~ res[[3;;Length[res]]] , 0.001]*)
(*];*)

(*private: method expands bounding box*)
ExpandBox[box_, add_] := {
  { Max[ 0, box[[1,1]] - add ] , Max[ 0, box[[1,2]] - add ] },
  box[[2]] + {add, add} (*ImageTrim handles larger boxes automatically*)
} 



(*Main fitting methods - tested on 2015-03-06*)

SPFGaussianOptimised[img_Image] := Module[
  {data, fit, performFit, a, angle, y, x, mx, my, sx, sy, dimensions, p, mx0, my0},
  data = ImageToList[img];
  dimensions = ImageDimensions[img];
  (*find early position guess - centroid*)
  {mx0, my0} = centroidFit[data];
  (*define fit*)
  performFit := FindFit[data,
    a Exp[-(((-my + y) Cos[angle] - (-mx + x) Sin[angle])^2/(2 sy^2)) -
           ((-mx + x) Cos[angle] + (-my + y) Sin[angle])^2/(2 sx^2)   ] ,
    (*great initial guess*)
    {{a, Max[data]}, {angle, 0.0}, {mx, mx0}, {my, my0}, {sx, dimensions[[1]]/2}, {sy, dimensions[[2]]/2}},
    {x, y},
    AccuracyGoal -> 3, PrecisionGoal -> 3, Method -> "LevenbergMarquardt"];
  (*fit*)
  fit = Quiet[Check[performFit, $Failed]];
  (*if failed use simpler routine to find at least the positions*)
  If[fit === $Failed , Return@SPFGaussianFixed[img] ];
  {mx, my, sx, sy, angle} = {mx, my, sx, sy, angle} /. fit ;
  (*check parameters - if off use simpler routine*)
  If[mx <= 0 || mx >= dimensions[[1]] || my <= 0 || my >= dimensions[[2]], Return@SPFGaussianFixed[img] ];
  (* when partilce size is small, it typically indicates fitting error *)
  If[Sqrt@Abs[sx*sy] < 0.9, Return@SPFGaussianFixed[img]  ];
  (*format the otput*)
  {
    (*position*)
    mx - 0.5, my - 0.5, 0.0,
    (*fit quality*)
    1.0 (*todo*),
    (*size*)
    Sqrt@Abs[sx *sy],
    (*angle*)
    Mod[If[sx > sy, angle, angle + Pi/2.0], N@Pi],
    (*flattening*)
    1.0 - Abs@If[sx > sy, sy/sx, sx/sy]
  }
];

(* fast centroid method*)
centroidFit = Compile[{{data, _Complex, 2}}, Module[{p,mx0,my0},
  p = data[[All, 3]] // N;
  p /= Total[p];
  mx0 = data[[All, 1]]~Dot~p;
  my0 = data[[All, 2]]~Dot~p;
  {mx0,my0}
] ];




SPFGaussianFixed[img_Image] := Module[
  {data, fit, performFit, a, y, x, mx, my, s, dimensions, failedResults, mx0, my0},
  data = ImageToList[img];
  dimensions = ImageDimensions[img];
  {mx0, my0} = centroidFit[data];
  performFit := FindFit[data,
    a Exp@(-(-my + y)^2/(2 (s^2)) - (-mx + x)^2/(2 s^2)),
    {{a, Max@data}, {mx, mx0}, {my, my0}, {s, Mean@dimensions/2}}, {x, y},
    AccuracyGoal -> 3, PrecisionGoal -> 3, Method-> "LevenbergMarquardt"];
  failedResults := {mx0, my0, 0.0 , 0.0, dimensions[[2]]/2};
  fit = Quiet[Check[performFit, Return@failedResults]];
  {mx, my, s} = {mx, my, s} /. fit ;
  (*deal with errors*)
  If[mx <= 0 || mx >= dimensions[[1]] || my <= 0 || my >= dimensions[[2]] || s > 0.66*Mean@dimensions,
    Return@failedResults];
  (*format the reply*)
  {mx - 0.5, my - 0.5, 0,
  (*fit quality*)
    1.,
  (*size*)
    Abs[s]
  }
];



(* ::Section:: *)
(*The End*)


End[ ]

EndPackage[ ]
