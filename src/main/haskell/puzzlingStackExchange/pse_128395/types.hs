module Types where

--
-- general types
--

-- type Function z y = z -> y

type Value z = z

type Row z = [Value z]

type Triangle z = Row (Row z)

type Pair z y = (z, y)

type Entry k v = (k, v)

type EntryRow k v = Row (Entry k v)

type EntryTriangle k v = Triangle (Entry k v)