{-|
Module      : Tarefa3
Description : Mecânica do Jogo
Copyright   : Jose dos Santos Gomes <a110367@alunos.uminho.pt>
              Guilherme Figueiredo Castro <a110452@alunos.uminho.pt>

Módulo para a realização da Tarefa 3 de LI1 em 2024/25.
-}
module Tarefa3 where

import LI12425

-- Atualiza o estado do jogo em função do tempo decorrido
atualizaJogo :: Tempo -> Jogo -> Jogo
atualizaJogo dt jogo = jogo {
    inimigosJogo = novosInimigos,
    torresJogo = novasTorres,
    baseJogo = novaBase,
    portaisJogo = novosPortais
}
  where
    -- Atualizar o estado dos inimigos
    (novosInimigos, novaBase) = atualizaInimigos dt (inimigosJogo jogo) (baseJogo jogo) (mapaJogo jogo)

    -- Atualizar o estado das torres
    novasTorres = map (atualizaTorre dt novosInimigos) (torresJogo jogo)

    -- Atualizar o estado dos portais
    novosPortais = map (atualizaPortal dt novosInimigos) (portaisJogo jogo)

-- Atualiza os inimigos no mapa
atualizaInimigos :: Tempo -> [Inimigo] -> Base -> Mapa -> ([Inimigo], Base)
atualizaInimigos dt inimigos base mapa = foldr processaInimigo ([], base) inimigos
  where
    processaInimigo inimigo (atualizados, baseAtual)
      | vidaInimigo inimigo <= 0 =
          (atualizados, baseAtual { creditosBase = creditosBase baseAtual + butimInimigo inimigo })
      | chegouBase inimigo (posicaoBase baseAtual) =
          (atualizados, baseAtual { vidaBase = vidaBase baseAtual - ataqueInimigo inimigo })
      | otherwise =
          (movimentaInimigo dt mapa inimigo : atualizados, baseAtual)

    chegouBase inimigo posBase =
      let (xi, yi) = posicaoInimigo inimigo
          (xb, yb) = posBase
      in abs (xi - xb) < 0.5 && abs (yi - yb) < 0.5

-- Movimenta o inimigo no mapa
movimentaInimigo :: Tempo -> Mapa -> Inimigo -> Inimigo
movimentaInimigo dt mapa inimigo
  | congelado inimigo = inimigo -- Inimigo congelado não se move
  | otherwise =
      inimigo { posicaoInimigo = (x + dx * vel * dt, y + dy * vel * dt) }
  where
    (x, y) = posicaoInimigo inimigo
    (dx, dy) = direcaoParaDelta (direcaoInimigo inimigo)
    vel = velocidadeInimigo inimigo * ajustaVelocidade (projeteisInimigo inimigo)

    congelado inimigo = any (\p -> tipoProjetil p == Gelo) (projeteisInimigo inimigo)

-- Ajusta a velocidade do inimigo com base nos efeitos de projéteis
ajustaVelocidade :: [Projetil] -> Float
ajustaVelocidade [] = 1
ajustaVelocidade (Projetil Resina _ : _) = 0.5
ajustaVelocidade _ = 1

-- Atualiza o estado de uma torre
atualizaTorre :: Tempo -> [Inimigo] -> Torre -> Torre
atualizaTorre dt inimigos torre
  | tempoTorre torre > 0 = torre { tempoTorre = tempoTorre torre - dt }
  | null alvos = torre
  | otherwise = torre { tempoTorre = cicloTorre torre }
  where
    alvos = inimigosNoAlcance torre inimigos

-- Determina os inimigos no alcance da torre
inimigosNoAlcance :: Torre -> [Inimigo] -> [Inimigo]
inimigosNoAlcance torre inimigos = filter (\inimigo -> distancia (posicaoTorre torre) (posicaoInimigo inimigo) <= alcanceTorre torre) inimigos
  where
    distancia (x1, y1) (x2, y2) = sqrt ((x1 - x2)^2 + (y1 - y2)^2)

-- Atualiza o estado de um portal
atualizaPortal :: Tempo -> [Inimigo] -> Portal -> Portal
atualizaPortal dt inimigos portal = portal { ondasPortal = novasOndas }
  where
    novasOndas = map (atualizaOnda dt) (ondasPortal portal)

-- Atualiza o estado de uma onda de inimigos
atualizaOnda :: Tempo -> Onda -> Onda
atualizaOnda dt onda
  | entradaOnda onda > 0 = onda { entradaOnda = entradaOnda onda - dt }
  | tempoOnda onda > 0 = onda { tempoOnda = tempoOnda onda - dt }
  | not (null (inimigosOnda onda)) = onda { inimigosOnda = tail (inimigosOnda onda) }
  | otherwise = onda

-- Determina o delta de movimento com base na direção
direcaoParaDelta :: Direcao -> (Float, Float)
direcaoParaDelta Norte = (0, -1)
direcaoParaDelta Sul   = (0, 1)
direcaoParaDelta Este  = (1, 0)
direcaoParaDelta Oeste = (-1, 0)

-- Aplica os efeitos de projéteis aos inimigos
aplicaEfeitosProjetis :: Inimigo -> Inimigo
aplicaEfeitosProjetis inimigo = foldl aplicaEfeito inimigo (projeteisInimigo inimigo)
  where
    aplicaEfeito inimigo (Projetil Fogo (Finita t)) = 
      inimigo {vidaInimigo = vidaInimigo inimigo - 5 * min t 1}
    aplicaEfeito inimigo (Projetil Gelo _) = 
      inimigo {velocidadeInimigo = 0} -- Congela o inimigo
    aplicaEfeito inimigo (Projetil Resina _) = 
      inimigo {velocidadeInimigo = velocidadeInimigo inimigo * 0.5} -- Reduz velocidade
    aplicaEfeito inimigo _ = inimigo

--tarefa 3 concluída--