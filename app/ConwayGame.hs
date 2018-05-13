module ConwayGame (Board(..), randomBoard, step)  where

import System.Random



data Board = Board [[Bool]]
data Coord = Coord Int Int deriving (Eq, Show)



step :: Board -> Board
step board@(Board dta) =
    (Board updatedData)
    where
        updatedData = [[updateCell board (Coord x y)
                            | (y, _) <- zip [0..] row] 
                        | (x, row) <- zip [0..] dta]



updateCell :: Board -> Coord -> Bool
updateCell board pos =
    if board @@ pos then
        if liveNeighbs <= 1 then False      -- underpopulation
        else if liveNeighbs <= 3 then True  -- lives
        else False                          -- overpopulation
    else
        if liveNeighbs == 3 then True       -- reproduction
        else False                          -- dies
    where
        neighbs = neighboursData board pos
        liveNeighbs = foldl (\acc neighbVal -> if neighbVal then acc + 1 else acc) 0 neighbs

neighboursData :: Board -> Coord -> [Bool]
neighboursData board pos =
    fmap (\coord -> board @@ coord) nhbrs
    where
        nhbrs = neighbours pos (size board)


neighbours :: Coord -> (Int, Int) -> [Coord]
neighbours (Coord x y) (maxX, maxY) = 
    [Coord i j
        | i <- [x-1, x, x+1]
        , j <- [y-1, y, y+1]
        , i >= 0 , i < maxX
        , j >= 0 , j < maxY
        , (i, j) /= (x, y)
    ]


(@@) :: Board -> Coord -> Bool
(Board dta) @@ (Coord x y) = dta!!x!!y

size :: Board -> (Int, Int)
size (Board dta) = (length dta, length (dta!!0))


instance Show Board where
    show (Board dta) =
        foldl (\acc s -> acc ++ "\n" ++ s) "" strings
        where
            strings = fmap (\row -> [if value then '*' else ' ' | value <- row ]) dta





randomBoard :: StdGen -> Int -> Int -> Board
randomBoard g x y = 
    Board (take x (chunks y (randomRs (True, False) g)))
    where
        chunks size xs = take size xs : chunks size (drop size xs)
