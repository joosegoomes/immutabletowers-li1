module Tempo where

import ImmutableTowers
import LI12425
import Tarefa3

reageTempo :: Tempo -> ImmutableTowers -> ImmutableTowers
reageTempo tempo it = it {estadoJogo = atualizaJogo tempo (estadoJogo it)}
