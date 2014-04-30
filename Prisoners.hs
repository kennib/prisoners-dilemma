import Data.Maybe

data Decision = Betray | Silent deriving (Show, Eq)
data Strategy = AlwaysBetray
              | AlwaysSilent
              | TitForTat
              deriving (Show, Enum)

allStrategies = [AlwaysBetray ..]

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

isStrategy :: [Decision] -> [Decision] -> Strategy -> Bool
isStrategy opponents _    AlwaysBetray = all isBetray opponents
isStrategy opponents _    AlwaysSilent = all isSilent opponents
isStrategy opponents mine TitForTat    = and (zipWith (==) (tail opponents) mine)
isStrategy _ _ _ = False

guessStrategies :: [Decision] -> [Decision] -> [Strategy]
guessStrategies opponents mine = filter (isStrategy opponents mine) allStrategies 

main = do
  print $ decide AlwaysBetray []
  print $ decide TitForTat []
  print $ decide TitForTat [Betray, Silent]
  print $ decide TitForTat [Silent, Betray, Silent]

  print $ guessStrategies [Betray, Betray, Betray, Betray] [Betray, Silent, Silent, Silent]
  print $ guessStrategies [Betray, Betray, Betray, Betray] [Betray, Betray, Betray, Betray]
  print $ guessStrategies [Silent, Silent, Betray, Betray] [Silent, Betray, Betray, Betray]
  print $ guessStrategies [Silent, Silent, Silent, Silent] [Silent, Betray, Betray, Betray]
  print $ guessStrategies [Silent, Silent, Betray, Silent] [Silent, Betray, Silent, Silent]
