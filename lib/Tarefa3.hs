{-|
Module      : Tarefa3
Description : Mecânica do Jogo
Copyright   : Jose dos Santos Gomes <a110367@alunos.uminho.pt>
              Guilherme Figueiredo Castro <a110452@alunos.uminho.pt>


Módulo para a realização da Tarefa 3 de LI1 em 2024/25.
-}
module Tarefa3 where

import LI12425

-- | A função 'atualizaJogo' recebe um intervalo de tempo e um jogo, e retorna o jogo atualizado.
atualizaJogo :: Tempo -> Jogo -> Jogo
atualizaJogo deltaTempo jogo@Jogo 
  {baseJogo = base@Base {vidaBase = vida, creditosBase = creditos}, inimigosJogo = inimigos, torresJogo = torres, portaisJogo = portais, mapaJogo = mapa} 
  = jogo { baseJogo = base {vidaBase = atualizaVidaBase vida inimigos},   
           inimigosJogo = inimigosAtualizados,
           torresJogo = torresAtualizadas,   
           portaisJogo = portaisAtualizados deltaTempo portais}
    where 
          -- Função para calcular a nova vida da base     
          atualizaVidaBase :: Float -> [Inimigo] -> Float
          atualizaVidaBase vida [] = vida
          atualizaVidaBase vida (inimigo:inimigosRestantes)
            | posicaoInimigo inimigo == posicaoBase base = atualizaVidaBase (vida - ataqueInimigo inimigo) inimigosRestantes
            | otherwise = atualizaVidaBase vida inimigosRestantes
          inimigosAtualizados = undefined
          torresAtualizadas = undefined
          -- Atualização dos portais, atualizando todas as ondas filtradas de um portal, recursivamente 
          portaisAtualizados :: Tempo -> [Portal] -> [Portal]
          portaisAtualizados _ [] = []
          portaisAtualizados tempo (p:ps) = p {ondasPortal = filtraOndas (map (atualizaOndas tempo) (ondasPortal p))} : portaisAtualizados tempo ps
          -- Atualização das ondas com o decorrer do tempo, atualizando o tempo que falta para o próximo inimigo e ondar entrar
          atualizaOndas :: Tempo -> Onda -> Onda
          atualizaOndas tempoDecorrido onda = onda {tempoOnda = tempoOnda onda - tempoDecorrido, entradaOnda = entradaOnda onda - tempoDecorrido}
          --Remove as ondas que já entraram excepto a que está a decorrer (ainda tem inimigos por enviar) 
          filtraOndas :: [Onda] -> [Onda]
          filtraOndas [] = []
          filtraOndas ondas = filter (\onda -> entradaOnda onda > 0 || not (null (inimigosOnda onda))) ondas