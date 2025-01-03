module Main (main) where

import Test.HUnit
import Tarefa1Spec (testesTarefa1)
import Tarefa2Spec (testesTarefa2)
import Tarefa3Spec (testesTarefa3)

main :: IO ()
main = do
  putStrLn "A correr todos os testes..."
  counts <- runTestTT $ TestList [testesTarefa1, testesTarefa2, testesTarefa3]
  if errors counts + failures counts == 0
    then putStrLn "Todos os testes passaram!"
    else putStrLn "Alguns testes falharam."
