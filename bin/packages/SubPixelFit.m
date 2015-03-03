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


(*Specs:*)
(* - returned position should start with coordinates (0,0) :*)

(* ::Section:: *)
(* Package Declarations*)


BeginPackage["SubPixelFit`"]

(* ---  Declarations  --- *)

BoundParticleImg::usage = ""

ImageToList::usage = ""

FitSubPixel::usage = ""

ForFitSubPixel::usage = ""


(* ::Section:: *)
(*Package Implementations - Public*)


Begin["`Private`"]

(* ::Section:: *)
(*Implementation*)

BoundParticleImg[img_Image, binImg_Image, id_Integer] := 
 ImageTrim[ 
  img, (id /. ComponentMeasurements[binImg, "BoundingBox"]) ]

ImageToList[img_Image] := 
 Flatten[MapIndexed[{#2[[2]] - 1, #2[[1]] - 1, #1} &, 
   Reverse[ImageData[img, "Byte"], 1], {2}], 1] 


FitSubPixel[img_, box_] := SubPixel2DGaussianFixedFit[ ImageTrim[ img, box ] ]

GetPosition[fit_, box_] := N@Round[ ("pos" /. fit) + box[[1]] , 0.001]

SelectQualityFit[fit_, box_] := If[ ("quality"/.fit) < 0.75, {}, GetPosition[fit, box] ]

ProcessOneBox[img_Image, box_] := SelectQualityFit[ FitSubPixel[img, box], box]

ForFitSubPixel[img_Image, binImg_Image, ids_] := Select[
  ProcessOneBox[img, #] &/@ (ids /. ComponentMeasurements[binImg, "BoundingBox"]),
  Length@# > 0 & 
]

(*main method*)
SubPixel2DGaussianFixedFit[img_Image] := 
 Module[ {data, fit, performFit, a, y, x, mx, my, b, sx, sy},
  data = ImageToList[img] ;
  performFit :=  NonlinearModelFit[data, 
    a Exp@(-(-my + y)^2/(2 (sy^2) ) - (-mx + x)^2/(2 sx^2)), 
    {{a, Max@data}, 
     {mx, ImageDimensions[img][[1]]/2}, 
     {my, ImageDimensions[img][[2]]/2}, 
     {sx, ImageDimensions[img][[1]]/2}, 
     {sy, ImageDimensions[img][[2]]/2}},
    {x, y}];

  fit = Quiet[Check[ performFit , $Failed]];
  If[fit===$Failed, Null , {mx, my, sx, sy} = {mx, my, sx, sy} /. fit["BestFitParameters"]];

  { "pos" -> {mx, my},
    "quality" -> (*check fit success, chech that is within image, check that has reasonable parameters*)
      If[ fit === $Failed , 0,
      If[ mx <= 0 || mx >= ImageDimensions[img][[1]] || my <= 0 || my >= ImageDimensions[img][[2]], 0,
      If[ sx > 0.66*ImageDimensions[img][[1]] || sy > 0.66*ImageDimensions[img][[2]], 0, 
      (*Else return*) 1 ]]]
   }
  ]


SubPixel2DGaussianSimpleFit[img_Image] := 
 Module[ {data, fit, a, y, x, mx, my, b, s},
  data = ImageToList[img] ;
  fit =  NonlinearModelFit[data, 
    a Exp@(-(-my + y)^2/(2 (s^2) ) - (-mx + x)^2/(2 s^2)), {{a, 
      Max@data}, {mx, ImageDimensions[img][[1]]/2}, {my, 
      ImageDimensions[img][[2]]/2}, {s, 3}},
    {x, y}];
  {"pos" ->
     {mx, my} /. fit["BestFitParameters"]
   }
  ]



(* ::Section:: *)
(*The End*)


End[ ]

EndPackage[ ]
