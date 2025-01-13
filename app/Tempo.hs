module Tempo where

import ImmutableTowers
import LI12425
import Tarefa2
import Tarefa3

reageTempo :: Tempo -> ImmutableTowers -> ImmutableTowers
reageTempo tempo it@(ImmutableTowers estado _ Gameplay)
  | ganhouJogo estado = it {ecraJogo = Win}
  | perdeuJogo estado = it {ecraJogo = GameOver}
  | otherwise =
      let jogoAtualizado = atualizaJogo tempo estado
      in it {estadoJogo = jogoAtualizado}
reageTempo _ it = it