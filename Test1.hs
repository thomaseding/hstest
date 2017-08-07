{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}


module Test where



type RawHandle = Int

data Spell
data Minion

data Handle :: * -> * where
    SpellHandle :: RawHandle -> Handle Spell
    MinionHandle :: RawHandle -> Handle Minion

class GenHandle a where
    genHandle :: String -> Handle a

instance GenHandle Spell where
    genHandle _ = SpellHandle 0

instance GenHandle Minion where
    genHandle _ = MinionHandle 1



get1 :: a b -> b
get1 = error "get1"


test :: Handle a -> Handle a
test _ = genHandle "str"



