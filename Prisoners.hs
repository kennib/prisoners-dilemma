data Decision = Betray | Silent deriving (Show, Eq)
data Strategy = AlwaysBetray
              | AlwaysSilent
              | TitForTat
              | Spite
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
decide Spite ds
  | any isBetray ds = Betray
  | otherwise       = Silent

decisions :: Strategy -> [Decision] -> [Decision]
decisions strategy ds
  | length (ds) > 0 = [decide strategy (tail ds)] ++ decisions strategy (tail ds)
  | otherwise       = []

isStrategy :: [Decision] -> [Decision] -> Strategy -> Bool
isStrategy opponents mine strategy     = decisions strategy mine == opponents

guessStrategies :: [Decision] -> [Decision] -> [Strategy]
guessStrategies opponents mine = filter (isStrategy opponents mine) allStrategies 

main = do
  -- Always Betray
  print $ guessStrategies [Betray, Betray, Betray, Betray] [Silent, Silent, Silent, Betray]
  -- Tit for Tat / Spite
  print $ guessStrategies [Betray, Betray, Betray, Silent] [Betray, Betray, Betray, Betray]
  -- Tit for Tat
  print $ guessStrategies [Betray, Silent, Betray, Silent] [Silent, Betray, Silent, Betray]
  -- Always Silent
  print $ guessStrategies [Silent, Silent, Silent, Silent] [Betray, Betray, Betray, Betray]
