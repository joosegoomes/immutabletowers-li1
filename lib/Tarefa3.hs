{-|
Module      : Tarefa3
Description : Mecânica do Jogo
Copyright   : Jose dos Santos Gomes <a110367@alunos.uminho.pt>
              Guilherme Figueiredo Castro <a110452@alunos.uminho.pt>

Módulo para a realização da Tarefa 3 de LI1 em 2024/25.
-}
module Tarefa3 where

import LI12425
import Tarefa1
import Tarefa2

deriving instance Eq Projetil
deriving instance Eq Inimigo

-- Atualiza o estado do jogo com base no tempo decorrido.
atualizaEstadoJogo :: Tempo -> Jogo -> Jogo
atualizaEstadoJogo deltaTempo jogo@Jogo {baseJogo = base, inimigosJogo = inimigos, torresJogo = torres, portaisJogo = portais} =
  jogoAtualizado
  where
    (inimigosAtualizados, baseAtualizada) = atualizaEstadoInimigos deltaTempo inimigos base torres jogo
    (torresAtualizadas, inimigosComDano) = atualizaEstadoTorres deltaTempo torres inimigosAtualizados
    portaisAtualizados = atualizaEstadoPortais deltaTempo portais inimigosComDano jogo
    jogoAtualizado = jogo {baseJogo = baseAtualizada, inimigosJogo = inimigosComDano, torresJogo = torresAtualizadas, portaisJogo = portaisAtualizados}

-- Atualiza o estado dos inimigos: movimenta, aplica efeitos e remove mortos ou os que atingiram a base.
atualizaEstadoInimigos :: Tempo -> [Inimigo] -> Base -> [Torre] -> Jogo -> ([Inimigo], Base)
atualizaEstadoInimigos deltaTempo inimigos base torres jogo =
  (inimigosAtivos, baseAtualizada)
  where
    inimigosComEfeitos = map aplicaEfeitosProjetis inimigos
    inimigosMovidos = map (moveInimigo deltaTempo jogo) inimigosComEfeitos
    inimigosAtivos = filter (\i -> vidaInimigo i > 0 && posicaoInimigo i /= posicaoBase (baseJogo jogo)) inimigosMovidos
    danoTotal = sum [ataqueInimigo i | i <- inimigosMovidos, posicaoInimigo i == posicaoBase (baseJogo jogo)]
    baseAtualizada = base {vidaBase = max 0 (vidaBase base - danoTotal)}

-- Aplica os efeitos ativos nos inimigos, como Fogo, Gelo e Resina.
aplicaEfeitosProjetis :: Inimigo -> Inimigo
aplicaEfeitosProjetis inimigo = foldl aplicaEfeito inimigo (projeteisInimigo inimigo)
  where
    aplicaEfeito inim (Projetil Fogo (Finita t)) = inim {vidaInimigo = vidaInimigo inim - 5 * min t 1}
    aplicaEfeito inim (Projetil Gelo _) = inim {velocidadeInimigo = 0}
    aplicaEfeito inim (Projetil Resina _) = inim {velocidadeInimigo = velocidadeInimigo inim * 0.7}
    aplicaEfeito inim _ = inim

-- Movimenta os inimigos com base na sua direção e velocidade.
moveInimigo :: Tempo -> Jogo -> Inimigo -> Inimigo
moveInimigo deltaTempo jogo inimigo@Inimigo {posicaoInimigo = (x, y), direcaoInimigo = dir, velocidadeInimigo = vel} =
  case dir of
    Norte -> inimigo {posicaoInimigo = (x, y - vel * deltaTempo)}
    Sul   -> inimigo {posicaoInimigo = (x, y + vel * deltaTempo)}
    Este  -> inimigo {posicaoInimigo = (x + vel * deltaTempo, y)}
    Oeste -> inimigo {posicaoInimigo = (x - vel * deltaTempo, y)}

-- Atualiza as torres: dispara projéteis e aplica dano aos inimigos no alcance.
atualizaEstadoTorres :: Tempo -> [Torre] -> [Inimigo] -> ([Torre], [Inimigo])
atualizaEstadoTorres deltaTempo torres inimigos = (torresAtualizadas, inimigosAtualizados)
  where
    torresComCooldown = map (\t -> t {tempoTorre = max 0 (tempoTorre t - deltaTempo)}) torres
    (inimigosAtualizados, torresAtualizadas) = foldl (\(inis, ts) torre -> let (inis', torre') = disparaProjetilTorre torre inis in (inis', ts ++ [torre'])) (inimigos, []) torresComCooldown

disparaProjetilTorre :: Torre -> [Inimigo] -> ([Inimigo], Torre)
disparaProjetilTorre torre inimigos
  | tempoTorre torre > 0 = (inimigos, torre) -- Torre ainda em cooldown
  | otherwise =
      let alvos = take (rajadaTorre torre) (inimigosNoAlcance torre inimigos)
          inimigosAtingidos = map (atingeInimigo torre) alvos
      in (inimigosAtingidos ++ filter (`notElem` alvos) inimigos, torre {tempoTorre = cicloTorre torre})

-- Atualiza os portais: ativa novas ondas de inimigos.
atualizaEstadoPortais :: Tempo -> [Portal] -> [Inimigo] -> Jogo -> [Portal]
atualizaEstadoPortais deltaTempo portais inimigos jogo = map atualizaPortal portais
  where
    atualizaPortal portal@Portal {ondasPortal = ondas} =
      portal {ondasPortal = map atualizaOnda ondas}
      where
        atualizaOnda onda@Onda {tempoOnda = t, inimigosOnda = inimigosOnda'} =
          if t <= 0 && not (null inimigosOnda')
            then onda {inimigosOnda = tail inimigosOnda', tempoOnda = cicloOnda onda, entradaOnda = max 0 (entradaOnda onda - deltaTempo)}
            else onda {tempoOnda = max 0 (t - deltaTempo)}

--tarefa 3 atualizada-- 