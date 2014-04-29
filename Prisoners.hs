data Decision = Betray | Silent deriving (Show, Eq)
data Strategy = AlwaysBetray
              | AlwaysSilent

decide :: Strategy -> [Decision] -> Decision
decide AlwaysBetray _ = Betray
decide AlwaysSilent _ = Silent

main = do
  print $ decide AlwaysBetray []
