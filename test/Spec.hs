module Main (main) where

import Test.HUnit
import Tarefa1Spec (testesTarefa1)
import Tarefa2Spec (testesTarefa2)
import Tarefa3Spec (testesTarefa3)

main :: IO ()
main = do
  putStrLn "Running all test suites..."
  runTestTTAndExit $ TestList [testesTarefa1, testesTarefa2, testesTarefa3]
  putStrLn "All test suites passed!"