module Tempo where

import ImmutableTowers
import LI12425
import Tarefa2
import Tarefa3
import Debug.Trace (trace)

reageTempo :: Tempo -> ImmutableTowers -> ImmutableTowers
reageTempo tempo it@(ImmutableTowers estado torreSelecionada Gameplay)
  | ganhouJogo estado = it {ecraJogo = Win}
  | perdeuJogo estado = it {ecraJogo = GameOver}
  | otherwise =
      let jogoAtualizado = atualizaJogo tempo estado
          torres = torresJogo jogoAtualizado
          infoTorres = unlines $ map (\torre -> "Torre em posição: " ++ show (posicaoTorre torre) ++
                                                ", Dano: " ++ show (danoTorre torre) ++
                                                ", Alcance: " ++ show (alcanceTorre torre) ++
                                                ", Tipo de projetil: " ++ show (tipoProjetil (projetilTorre torre))) torres
      in trace ("Torres em jogo:\n" ++ infoTorres) it {estadoJogo = jogoAtualizado}
reageTempo _ it = it