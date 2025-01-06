module Utils.Trace (traceLines) where

import Debug.Trace (trace)

traceLines :: [String] -> a -> a
traceLines lines a = foldr trace a lines
