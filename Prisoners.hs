data Decision = Betray | Silent deriving (Show, Eq)
data Strategy = AlwaysBetray
              | AlwaysSilent
              | TitForTat

justBetrayed :: [Decision] -> Bool
justBetrayed ds
  | null ds     = False
  | d == Betray = True
  | otherwise   = False
  where d = head ds

decide :: Strategy -> [Decision] -> Decision
decide AlwaysBetray _ = Betray
decide AlwaysSilent _ = Silent
decide TitForTat ds
  | justBetrayed ds = Betray
  | otherwise       = Silent

main = do
  print $ decide AlwaysBetray []
  print $ decide TitForTat []
  print $ decide TitForTat [Betray, Silent]
  print $ decide TitForTat [Silent, Betray, Silent]
