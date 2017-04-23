module AudiComp.Core.PreludeExtensions where

maybeToEither leftValue maybe =
  case maybe of
    Just x -> Right x
    Nothing -> Left leftValue

bindRight :: (b -> Either a c) -> Either a b -> Either a c
bindRight f either =
  case either of
    Left l -> Left l
    Right r -> f r

bindLeft :: (a -> Either b c) -> Either a c -> Either b c
bindLeft f either =
  case either of
    Left l -> f l
    Right r -> Right r
