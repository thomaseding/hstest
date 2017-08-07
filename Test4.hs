{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeFamilyDependencies #-}


module Test where



type RawHandle = Int

data Spell
data Minion

data family Handle where
    SpellHandle = AnyHandle Spell
    Handle Minion = AnyHandle Minion


{-
class GenHandle handle where
    genHandle :: String -> handle

instance GenHandle (Handle Spell) where
    genHandle _ = AnyHandle 0 :: Handle Spell

instance GenHandle (Handle Minion) where
    genHandle _ = AnyHandle 1



test :: Handle a -> Handle a
test _ = genHandle "test"
-}


