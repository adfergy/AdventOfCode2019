 -- Part A

data Instruction = U Int | D Int | L Int | R Int

type Path = [Instruction]
type Coordinate = (Int, Int)
type Route = [Coordinate]
type Distance = Int
type Line = (Coordinate, Coordinate)

inputA :: Path
inputA = [R 998,U 547,L 703,D 251,L 776,U 837,R 100,U 240,R 197,D 216,L 220,U 606,L 437,U 56,R 940,U 800,L 968,D 464,L 870,D 797,L 545,D 824,R 790,U 5,R 347,D 794,R 204,U 538,L 247,U 385,L 103,D 260,L 590,U 813,L 549,U 309,L 550,U 321,R 862,D 686,R 368,D 991,R 451,D 836,R 264,D 138,L 292,D 319,L 784,D 369,R 849,U 865,R 776,D 726,R 223,D 118,L 790,D 208,L 836,D 592,R 310,D 36,R 991,U 674,L 205,U 407,R 422,U 350,L 126,D 320,L 239,U 353,L 509,U 48,R 521,D 544,L 157,D 551,R 614,D 493,R 407,D 965,R 498,U 248,R 826,U 573,L 782,D 589,R 616,D 992,L 806,D 745,R 28,U 142,L 333,D 849,L 858,D 617,R 167,U 341,R 46,U 940,L 615,D 997,L 447,D 604,R 148,U 561,R 925,D 673,R 441,U 200,R 458,U 193,L 805,D 723,L 208,U 600,L 926,U 614,R 660,D 183,L 408,D 834,R 248,U 354,L 110,U 391,L 37,U 599,L 287,U 28,R 859,D 936,L 404,D 952,R 11,U 20,R 708,U 218,L 800,U 750,R 936,D 213,R 6,D 844,R 728,D 391,R 114,U 406,R 390,U 791,L 199,D 397,R 476,D 583,R 99,U 419,R 575,D 836,L 896,U 780,L 77,U 964,R 441,U 723,R 248,D 170,R 527,D 94,L 39,U 645,L 338,D 728,R 503,U 641,L 358,D 287,R 171,U 368,R 176,D 986,R 821,U 912,L 231,D 206,L 451,U 900,L 35,D 490,R 190,D 180,L 937,D 500,R 157,U 989,L 336,U 202,R 178,U 52,R 931,U 306,L 85,D 866,R 756,U 715,L 521,D 977,R 936,U 4,R 207,D 384,L 785,U 138,L 682,U 488,L 537,U 250,L 877,D 446,R 849,U 35,R 258,U 784,R 263,D 494,L 324,U 601,R 302,U 473,L 737,D 143,R 184,D 967,R 95,U 51,L 713,U 733,R 297,U 740,R 677,D 715,R 750,U 143,L 980,U 260,R 915,D 535,R 202,U 460,R 365,U 956,L 73,U 441,R 182,D 982,L 869,D 755,L 837,D 933,L 856,D 341,R 189,D 519,L 387,D 144,R 575,U 682,R 317,U 838,R 154,D 201,R 237,D 410,L 43,U 853,L 495,U 983,L 953,U 220,R 697,D 592,R 355,U 377,R 792,U 824,L 441,U 783,R 258,D 955,R 451,D 178,L 151,D 435,L 232,U 923,L 663,U 283,L 92,D 229,R 514]

inputB :: Path
inputB = [L 995,U 122,R 472,U 470,R 725,U 906,L 83,U 672,R 448,U 781,L 997,U 107,R 66,D 966,L 780,D 181,L 662,U 158,R 804,D 837,L 237,U 164,L 98,U 582,R 925,D 806,L 153,D 843,R 601,U 941,L 968,D 528,R 482,D 586,R 15,U 370,L 592,U 836,R 828,U 676,R 606,D 20,R 841,U 117,L 262,U 377,R 375,U 503,R 166,D 398,R 161,D 9,R 140,D 188,R 895,D 226,R 77,U 28,L 727,D 72,L 51,U 425,R 370,D 377,L 801,D 525,R 102,D 568,L 416,D 300,R 415,U 199,R 941,U 211,R 285,U 719,L 259,U 872,L 959,U 350,L 196,D 830,R 515,U 899,R 298,U 875,R 946,U 797,R 108,U 461,R 999,D 49,L 369,D 472,R 83,D 265,L 825,D 163,R 162,U 906,L 816,D 241,L 587,D 801,R 601,D 630,R 937,U 954,L 379,D 347,R 831,D 337,L 192,D 649,L 853,U 270,R 162,D 892,L 26,D 663,L 276,U 891,R 843,U 67,R 225,D 88,R 686,U 662,R 794,D 814,L 200,D 887,R 567,U 363,L 863,U 16,R 975,D 470,R 714,U 771,L 267,D 402,R 75,U 98,L 686,U 565,R 584,D 402,L 824,D 927,R 71,U 39,L 174,D 494,L 358,D 626,R 616,D 369,R 471,U 881,L 428,U 53,R 862,U 749,L 847,D 944,L 887,D 695,R 442,U 870,L 993,U 315,L 878,U 100,L 480,D 354,L 12,D 533,L 236,D 364,R 450,U 679,L 926,D 391,R 313,D 953,L 560,D 740,L 974,D 119,L 370,U 404,R 339,U 233,R 901,U 514,R 584,D 495,R 308,U 170,R 759,U 592,R 388,U 396,R 477,U 670,R 906,D 687,L 874,U 352,R 124,U 700,R 289,D 524,L 93,D 817,R 408,D 776,L 235,D 928,L 534,D 296,R 116,U 995,L 63,D 903,R 758,U 881,L 530,U 498,R 573,D 626,L 26,U 269,R 237,U 287,L 840,D 603,R 948,D 567,R 89,U 552,L 299,D 774,R 863,D 182,R 773,D 108,L 137,U 88,L 731,U 793,L 267,U 902,L 41,U 258,L 156,U 361,R 389,D 839,L 976,U 960,L 342,D 489,R 816,U 391,L 393,U 601,R 255,D 629,R 832,U 877,L 34,D 373,L 809,D 679,L 104,U 901,R 157,U 468,R 143,U 896,L 637,D 577,L 545,D 486,L 970,D 130,L 305,D 909,R 984,D 500,L 935,U 949,R 525,D 547,L 786,U 106,L 269,D 511,L 919]

example1a = [R 75,D 30,R 83,U 83,L 12,D 49,R 71,U 7,L 72]
example1b = [U 62,R 66,U 55,R 34,D 71,R 55,D 58,R 83]

example2a = [R 98,U 47,R 26,D 63,R 33,U 87,L 62,D 20,R 33,U 53,R 51]
example2b = [U 98,R 91,D 20,R 16,D 67,R 40,U 7,R 15,U 6,R 7]


getLines :: Coordinate -> Path -> [Line]
getLines _ [] = []
getLines start (i:is) = let (sx, sy) = start in
                        let calcEnd r u = (sx+r,sy+u) in  
                            case i of
                                U n -> let end = calcEnd 0 n in 
                                            (start,end) : (getLines end is)
                                D n -> let end = calcEnd 0 (-n) in 
                                            (start,end) : (getLines end is)
                                L n -> let end = calcEnd n 0 in 
                                            (start,end) : (getLines end is)
                                R n -> let end = calcEnd (-n) 0 in 
                                            (start,end) : (getLines end is)

doIntersect :: Line -> Line -> Bool
doIntersect ((x00, y00),(x01, y01)) ((x10, y10),(x11, y11)) 
                                | x00 == x01 = (y10 == y11) && ((x00<x10 && x00>x11) || (x00>x10 && x00<x11)) && ((y10<y00 && y10>y01) || (y10>y00 && y10<y01))
                                | y00 == y01 = (x10 == x11) && ((y00<y10 && y00>y11) || (y00>y10 && y00<y11)) && ((x10<x00 && x10>x01) || (x10>x00 && x10<x01))

intersectingLines :: [Line] -> [Line] -> [(Line, Line)]
intersectingLines xs ys = [(x,y) | x<-xs, y<-ys, doIntersect x y]

pointsOfIntersection :: Line -> Line -> [Coordinate]
pointsOfIntersection ((x00, y00),(x01, y01)) ((x10, y10),(x11, y11)) 
                                | x00 == x01 = [(x00,y10)]
                                | y00 == y01 = [(x10,y00)]


intersections :: [Coordinate]
intersections = filter (/=(0,0)) (concat ( map (uncurry pointsOfIntersection) (intersectingLines (getLines (0,0) inputA) (getLines (0,0) inputB))))

calculateManhatten :: Coordinate -> Distance
calculateManhatten (a,b) = abs(a) + abs(b)

answerA = minimum (map calculateManhatten intersections)

-- answerA = 731

-- Part B

calculateSteps :: Path -> Coordinate -> Coordinate -> Int
calculateSteps (i:is) (cx,cy) (tx,ty) =  let coords (a,b) (c,d) = (a+c,b+d) in
                                         let calcEnd r u = (cx+r,cy+u) in
                                            case i of
                                                U n -> if elem (tx,ty) (map (coords (cx,cy)) (zip (repeat 0) [0..n])) then (abs(tx-cx)+abs(ty-cy)) 
                                                            else n + (calculateSteps is (calcEnd 0 n) (tx,ty)) 

                                                D n -> if elem (tx,ty) (map (coords (cx,cy)) (zip (repeat 0) [(-n)..0])) then (abs(tx-cx)+abs(ty-cy)) 
                                                            else n + (calculateSteps is (calcEnd 0 (-n)) (tx,ty)) 

                                                L n -> if elem (tx,ty) (map (coords (cx,cy)) (zip [0..n] (repeat 0))) then (abs(tx-cx)+abs(ty-cy)) 
                                                            else n + (calculateSteps is (calcEnd n 0) (tx,ty)) 

                                                R n -> if elem (tx,ty) (map (coords (cx,cy)) (zip [(-n)..0] (repeat 0))) then (abs(tx-cx)+abs(ty-cy)) 
                                                            else n + (calculateSteps is (calcEnd (-n) 0) (tx,ty)) 


answerB = let f xs = (calculateSteps inputA (0,0) xs) + (calculateSteps inputB (0,0) xs) in
    minimum ( map f intersections )

--answerB = 5672








