module Tempo where

import ImmutableTowers
import LI12425
import Tarefa3
import Debug.Trace (trace)

reageTempo :: Tempo -> ImmutableTowers -> ImmutableTowers
reageTempo tempo it =
  let jogoAtualizado = atualizaJogo tempo (estadoJogo it)
      torres = torresJogo jogoAtualizado
      -- Função auxiliar para formatar as torres como string
      infoTorres = unlines $ map (\torre -> "Torre em posição: " ++ show (posicaoTorre torre) ++
                                            ", Dano: " ++ show (danoTorre torre) ++
                                            ", Alcance: " ++ show (alcanceTorre torre) ++
                                            ", Tipo de projetil: " ++ show (tipoProjetil (projetilTorre torre))) torres
  in trace ("Torres em jogo:\n" ++ infoTorres) it {estadoJogo = jogoAtualizado}
