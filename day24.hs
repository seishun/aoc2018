import Control.Monad
import Control.Monad.State
import Data.Foldable
import Data.Function
import Data.List
import Data.Maybe
import Data.Ord

import Data.Sequence (Seq)
import qualified Data.Sequence as Seq

data Faction = ImmuneSystem | Infection deriving (Eq)
type Attack = (Int, String, Int)
data Group = Group
  { units        :: Int
  , hitPoints    :: Int
  , attackDamage :: Int
  , attackType   :: String
  , initiative   :: Int
  , weaknesses   :: [String]
  , immunities   :: [String]
  } deriving (Eq)

parseAttack :: [String] -> Attack
parseAttack ["an","attack","that","does",damage_,t,"damage","at","initiative",initiative_] =
  (read damage_, t, read initiative_)

parseWeak :: [String] -> ([String], [String], Attack)
parseWeak ("immune":"to":xs) = parseImmune xs
parseWeak ("with":xs) = ([], [], parseAttack xs)
parseWeak (t:xs) =
  let (immune, weak, attack) = parseWeak xs
  in (immune, t:weak, attack)

parseImmune :: [String] -> ([String], [String], Attack)
parseImmune ("weak":"to":xs) = parseWeak xs
parseImmune ("with":xs) = ([], [], parseAttack xs)
parseImmune (t:xs) =
  let (immune, weak, attack) = parseImmune xs
  in (t:immune, weak, attack)

parseGroup :: String -> Group
parseGroup line =
  let (units_:"units":"each":"with":hp_:"hit":"points":xs) = words $ filter (`notElem` "(),;") line
      (immune, weak, (damage, t, initiative)) = case xs of
        ("immune":"to":xs') -> parseImmune xs'
        ("weak":"to":xs') -> parseWeak xs'
        ("with":xs') -> ([], [], parseAttack xs')
  in Group (read units_) (read hp_) damage t initiative weak immune

parse' :: Faction -> [String] -> [(Faction, Group)]
parse' faction ("":xs) = parse xs
parse' faction (line:xs) = (faction, parseGroup line) : parse' faction xs
parse' faction [] = []

parse :: [String] -> [(Faction, Group)]
parse ("Immune System:":xs) = parse' ImmuneSystem xs
parse ("Infection:":xs) = parse' Infection xs

damage :: Int -> String -> Group -> Int
damage power t target
  | t `elem` immunities target = 0
  | t `elem` weaknesses target = power * 2
  | otherwise = power

power :: Group -> Int
power attacker = max 0 $ attackDamage attacker * units attacker

attack :: Seq (Faction, Group) -> (Int, Int) -> Seq (Faction, Group)
attack groups (attacker, target) =
  let attacker' = snd $ groups `Seq.index` attacker
      target' = snd $ groups `Seq.index` target
      loss = damage (power attacker') (attackType attacker') target' `div` hitPoints target'
  in Seq.adjust' (\(f, g) -> (f, g { units = units g - loss })) target groups

select :: [(Faction, Group)] -> (Int, (Faction, Group)) -> [Int] -> (Maybe (Group, (Int, Int)), [Int])
select groups (attacker, (faction, group)) selected =
  let enemies = do
        (target, (faction', group')) <- zip [0..] groups
        guard $ target `notElem` selected
        guard $ faction' /= faction
        let damage' = damage (power group) (attackType group) group'
        guard $ damage' > 0
        return ((damage', power group', initiative group'), target)
  in case enemies of
    [] -> (Nothing, selected)
    _ ->
      let target = snd $ maximum enemies
      in (Just (group, (attacker, target)), target : selected)

fight :: [(Faction, Group)] -> [(Faction, Group)]
fight groups =
  let attackers = sortOn (\(_, (_, g)) -> Down (power g, initiative g)) $ zip [0..] groups
      selections = catMaybes $ fst $ runState (mapM (state . select groups) attackers) []
      attacks = map snd $ sortOn (\(g, _) -> Down (initiative g)) selections
  in filter (\(f, g) -> units g > 0) $ toList $ foldl attack (Seq.fromList groups) attacks

combat :: [(Faction, Group)] -> Maybe (Faction, Int)
combat groups =
  let groups' = fight groups
  in if groups' == groups
  then case groupBy ((==) `on` fst) groups of
    [((faction,_):_)] -> Just (faction, sum $ map (units . snd) groups)
    _ -> Nothing
  else combat groups'

boost :: [(Faction, Group)] -> Int -> [(Faction, Group)]
boost groups increase = do
  (faction, group) <- groups
  return $ case faction of
    ImmuneSystem -> (faction, group { attackDamage = attackDamage group + increase })
    Infection -> (faction, group)

part1 :: String -> Int
part1 = snd . fromJust . combat . parse . lines

part2 :: String -> Int
part2 input =
  let groups = parse $ lines input
  in snd $ head $ filter ((== ImmuneSystem) . fst) $ catMaybes $ map (combat . boost groups) [0..]
