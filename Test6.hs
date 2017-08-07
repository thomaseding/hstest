{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ViewPatterns #-}


module Test where


import Control.Monad.State
import Data.List (intercalate)
import GHC.Exts (Constraint)


--------------------------------------------------------------------------------------------------------------------------

data RawHandle :: * where
    RawHandle :: String -> Int -> RawHandle

data Type = Minion | Spell | Player

type UserConstraint (k :: (Type -> Constraint)) = (k 'Minion)

data Handle :: Type -> * where
    MinionHandle :: RawHandle -> Handle 'Minion
    SpellHandle :: RawHandle -> Handle 'Spell
    PlayerHandle :: RawHandle -> Handle 'Player


data Elect :: (Type -> Constraint) -> * where
    OwnerOf :: (k a) => Handle a -> (Handle 'Player -> Elect k) -> Elect k
    A :: A k -> Elect k
    Effect :: Effect k -> Elect k    

data A :: (Type -> Constraint) -> * where
    Minion' :: [Requirement 'Minion] -> (Handle 'Minion -> Elect k) -> A k
    Player' :: [Requirement 'Player] -> (Handle 'Player -> Elect k) -> A k

data Effect :: (Type -> Constraint) -> * where
    Elect :: Elect k -> Effect k
    DoNothing :: Effect k
    Enchant :: (k a) => Handle a -> Enchantment a k -> Effect k

data Requirement :: Type -> * where
    OwnedBy :: Handle 'Player -> Requirement 'Minion
    Damaged :: Requirement 'Minion


data Enchantment :: Type -> (Type -> Constraint) -> * where
    SwapStats :: Enchantment 'Minion k

data Aura :: (Type -> Constraint) -> * where
    While :: (k a) => Handle a -> [Requirement a] -> Aura k -> Aura k
    Has :: Handle 'Minion -> Enchantment 'Minion k -> Aura k
 

data Ability :: Type -> (Type -> Constraint) -> * where
    Aura :: (k a) => (Handle a -> Aura k) -> Ability a k
    Deathrattle :: (k a) => (Handle a -> Elect k) -> Ability a k

data MinionCard (k :: Type -> Constraint) = MinionCard {
    _minionAbilities :: [Ability 'Minion k]
} deriving ()

data HandCard :: (Type -> Constraint) -> * where
    HandCardMinion :: MinionCard k -> HandCard k



--------------------------------------------------------------------------------------------------------------------------


data ShowState = ShowState {
    handleSeed :: Int
} deriving ()


newtype ShowCard a = ShowCard {
    unShowCard :: State ShowState a
} deriving (Functor, Applicative, Monad, MonadState ShowState)


runShowCard :: ShowCard a -> a
runShowCard m = evalState (unShowCard m) $ ShowState {
    handleSeed = 0 }


--------------------------------------------------------------------------------------------------------------------------

class GenHandle (a :: Type) where
    genHandle :: String -> ShowCard (Handle a)

instance GenHandle 'Minion where
    genHandle = liftM MinionHandle . rawGenHandle

instance GenHandle 'Player where
    genHandle = liftM PlayerHandle . rawGenHandle

instance GenHandle 'Spell where
    genHandle = liftM SpellHandle . rawGenHandle

rawGenHandle :: String -> ShowCard RawHandle
rawGenHandle str = do
    n <- gets handleSeed
    let handle = RawHandle str n
    modify $ \st -> st { handleSeed = n + 1 }
    return handle

rawReadHandle :: RawHandle -> ShowCard String
rawReadHandle = \case
    RawHandle str _ -> return str


readHandle :: Handle a -> ShowCard String
readHandle = \case
    MinionHandle raw -> rawReadHandle raw
    PlayerHandle raw -> rawReadHandle raw
    SpellHandle raw -> rawReadHandle raw

genNumberedHandle :: (GenHandle a) => String -> ShowCard (Handle a)
genNumberedHandle str = do
    n <- gets handleSeed
    genHandle $ str ++ "_" ++ show n





--------------------------------------------------------------------------------------------------------------------------


type Showy = GenHandle



showCard :: HandCard Showy -> String
showCard = boxText


boxText :: HandCard Showy -> String
boxText = runShowCard . liftM (unlines . filter (not . null) . lines) . \case
    HandCardMinion minion -> showAbilities $ _minionAbilities minion


--------------------------------------------------------------------------------------------------------------------------

is :: String -> String -> Bool
is = (==)


this :: String
this = "THIS"


you :: String
you = "YOU"


opponent :: String
opponent = "OPPONENT"


--------------------------------------------------------------------------------------------------------------------------


showOwnerOf :: (x -> ShowCard String) -> Handle a -> (Handle 'Player -> x) -> ShowCard String
showOwnerOf showX handle cont = do
    player <- readHandle handle >>= \case
        (is this -> True) -> genHandle you
        str -> genHandle ("OWNER_OF[" ++ str ++ "]")
    showX $ cont player


showMinion :: [Requirement 'Minion] -> (Handle 'Minion -> Elect Showy) -> ShowCard String
showMinion requirements cont = do
    requirementsStr <- showRequirements requirements
    handle <- genNumberedHandle $ "MINION[" ++ requirementsStr ++ "]"
    showElect $ cont handle

showPlayer :: [Requirement 'Player] -> (Handle 'Player -> Elect Showy) -> ShowCard String
showPlayer requirements cont = do
    requirementsStr <- showRequirements requirements
    handle <- genNumberedHandle $ "PLAYER[" ++ requirementsStr ++ "]"
    showElect $ cont handle




showAbilities :: (Showy a) => [Ability a Showy] -> ShowCard String
showAbilities = liftM unlines . mapM showAbility

showAbility :: (Showy a) => Ability a Showy -> ShowCard String
showAbility = \case
    Aura aura -> showAuraAbility aura
    Deathrattle cont -> showDeathrattle cont


showAuraAbility :: (Showy a) => (Handle a -> Aura Showy) -> ShowCard String
showAuraAbility cont = genHandle this >>= showAura . cont


showAura :: Aura Showy -> ShowCard String
showAura = \case
    While handle requirements cont -> showWhile handle requirements cont
    Has handle enchantment -> showHas handle enchantment

showHas :: (Showy a) => Handle a -> Enchantment a Showy -> ShowCard String
showHas handle enchantment = do
    handleStr <- readHandle handle
    enchantmentStr <- showEnchantment enchantment
    return $ unwords [handleStr, "has", enchantmentStr]

showWhile :: (Showy a) => Handle a -> [Requirement a] -> Aura Showy -> ShowCard String
showWhile handle requirements aura = case requirements of
    [] -> showAura aura
    _ -> do
        handleStr <- readHandle handle
        requirementsStr <- showRequirements requirements
        auraStr <- showAura aura
        return $ "While " ++ handleStr ++ "[" ++ requirementsStr ++ "]: " ++ auraStr


showRequirements :: [Requirement a] -> ShowCard String
showRequirements = liftM (intercalate "," . filter (not . null)) . showRequirements'


showRequirements' :: [Requirement a] -> ShowCard [String]
showRequirements' = \case
    [] -> return []
    r : rs -> showRequirement r >>= \s -> liftM (s :) $ showRequirements' rs


showRequirement :: Requirement a -> ShowCard String
showRequirement = \case
    Damaged -> return "DAMAGED"
    OwnedBy handle -> readHandle handle >>= return . \case
        (is you -> True) -> "FRIENDLY"
        (is opponent -> True) -> "ENEMY"
        str -> "OWNED_BY[" ++ str ++ "]"

showEnchant :: (Showy a) => Handle a -> Enchantment a Showy -> ShowCard String
showEnchant minion enchantment = do
    minionStr <- readHandle minion
    enchantmentStr <- showEnchantment enchantment
    return $ unwords ["Give", minionStr, enchantmentStr]

showEnchantment :: (Showy a) => Enchantment a Showy -> ShowCard String
showEnchantment = \case
    SwapStats -> return "Swapped attack and health"

showDeathrattle :: (Showy a) => (Handle a -> Elect Showy) -> ShowCard String
showDeathrattle cont = do
    effectStr <- genHandle this >>= showElect . cont
    return $ "Deathrattle: " ++ effectStr


showEffect :: Effect Showy -> ShowCard String
showEffect = \case
    Elect elect -> showElect elect
    DoNothing -> return "DoNothing"
    Enchant handle enchantments -> showEnchant handle enchantments


showElect :: Elect Showy -> ShowCard String
showElect = \case
    A x -> showA x
    Effect x -> showEffect x
    OwnerOf handle cont -> showOwnerOf showElect handle cont


showA :: A Showy -> ShowCard String
showA = \case
    Minion' requirements cont -> showMinion requirements cont
    Player' requirements cont -> showPlayer requirements cont





class AnyType (a :: Type)
instance AnyType a
--type A' = Showy
--type B x = Showy x -- BUG??
--type C x = (Showy x, AnyType x)
--class (Showy a) => D a
--type K a = (Showy a, AnyType a)

--class (Showy a) => K (a :: Type)
--instance K a

type K = Showy


test :: HandCard K -> String
test = showCard


