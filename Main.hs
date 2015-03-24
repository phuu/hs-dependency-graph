-- | Main entry point to the application.
module Main where

import Data.Functor ((<$>))
import Data.List (find)

-- Actions

type ActionName = String
type Dependencies = [ActionName]
data Action = Action { getName :: ActionName, getDependencies :: Dependencies } deriving(Show,Eq)

{-

      A
     / \
    B   C
   / \   \
  D   E — G
 /       / \
F ————— H   I

-}

actions :: [Action]
actions =
    [ Action "A" []
    , Action "B" ["A"]
    , Action "C" ["A"]
    , Action "D" ["B"]
    , Action "E" ["B"]
    , Action "F" ["D"]
    , Action "G" ["E", "C"]
    , Action "H" ["F", "G"]
    , Action "I" ["G"]
    ]

dedupe :: Eq a => [a] -> [a]
dedupe = foldl (\seen v -> if v `elem` seen then seen else seen ++ [v]) []

walk :: [Action] -> ActionName -> Dependencies
walk actions name =
  case find ((== name) . getName) actions of
    (Just action) ->
      let depsWalk = concatMap (walk actions) (getDependencies action)
      in (dedupe depsWalk) ++ [(getName action)]
    _ -> []

-- | The main entry point.
main :: IO ()
main = do
    putStrLn $ show $ walk actions "H"
