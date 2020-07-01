{-# LANGUAGE OverloadedStrings #-}

module Mehanics.Map.Map
(getView)
where

import Miso
import Miso.String hiding (map)

type GameMap = [[MapElem]]

data MapElem = Wall | Path
  deriving (Show, Eq)

getSourceMap :: GameMap
getSourceMap =
  [ [Wall, Wall, Wall],
    [Wall, Path, Wall],
    [Wall, Path, Wall],
    [Wall, Wall, Wall]
  ]


-- findPath :: GameMap -> MapPath
-- findPath gm = undefined

getView :: a -> View action
--TODO: сделать корректное формирование вьюхи
getView _ = div_ [class_ "game-map"] items
  where
    items = map (\row -> tr_ [] $ map toHtml row) $ getSourceMap
    toHtml Wall = td_ [class_ "game-map__wall"] []
    toHtml Path = td_ [class_ "game-map__path"] []
