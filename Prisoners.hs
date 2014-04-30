import Data.Maybe

data Decision = Betray | Silent deriving (Show, Eq)
data Strategy = AlwaysBetray
              | AlwaysSilent
              | TitForTat
              deriving (Show)

isBetray :: Decision -> Bool
isBetray d = d == Betray

isSilent :: Decision -> Bool
isSilent d = d == Silent

justBetrayed :: [Decision] -> Bool
justBetrayed ds
  | null ds    = False
  | isBetray d = True
  | otherwise  = False
  where d = head ds

decide :: Strategy -> [Decision] -> Decision
decide AlwaysBetray _ = Betray
decide AlwaysSilent _ = Silent
decide TitForTat ds
  | justBetrayed ds = Betray
  | otherwise       = Silent

guessStrategy :: [Decision] -> [Decision] -> Maybe Strategy
guessStrategy opponents mine
  | all isBetray opponents = Just AlwaysBetray
  | all isSilent opponents = Just AlwaysSilent
  | otherwise = Nothing

main = do
  print $ decide AlwaysBetray []
  print $ decide TitForTat []
  print $ decide TitForTat [Betray, Silent]
  print $ decide TitForTat [Silent, Betray, Silent]

  print $ guessStrategy [Betray, Betray, Betray, Betray] [Betray, Silent, Silent, Silent]
  print $ guessStrategy [Silent, Silent, Betray, Betray] [Silent, Betray, Betray, Betray]
  print $ guessStrategy [Silent, Silent, Silent, Silent] [Silent, Betray, Betray, Betray]
