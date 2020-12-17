{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Types where

type Key = String
data Value = StrValue String | NumValue Int | BoolValue Bool | NodeObj Object | NodeArr Array
type Type = String

type Object = [(Key, Value)]
type Array = [Value]
