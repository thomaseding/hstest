{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeFamilyDependencies #-}


module Test where



type RawHandle = Int

data Spell
data Minion

data AnyHandle :: * -> * where
    AnyHandle :: RawHandle -> AnyHandle a

type family Handle a = h | h -> a where
    Handle Spell = AnyHandle Spell
    Handle Minion = AnyHandle Minion



class GenHandle a where
    genHandle :: String -> Handle a

instance GenHandle Spell where
    genHandle _ = AnyHandle 0

instance GenHandle Minion where
    genHandle _ = AnyHandle 1



test :: Handle a -> Handle a
test _ = genHandle "test"



