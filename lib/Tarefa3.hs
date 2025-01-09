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
atualizaJogo tempo jogo = 
  jogo {inimigosJogo = novosInimigos,
        torresJogo = novasTorres,
        baseJogo = novaBase,
        portaisJogo = novosPortais}
  where
    -- Atualizar inimigos e base
    (novosInimigos, novaBase) = atualizaInimigos tempo (inimigosJogo jogo) (baseJogo jogo) (mapaJogo jogo)

    -- Atualizar torres com base nos novos estados dos inimigos
    novasTorres = map (atualizaTorre tempo novosInimigos) (torresJogo jogo)

    -- Atualizar portais
    novosPortais = map (atualizaPortal tempo) (portaisJogo jogo)

-- Atualiza os inimigos no mapa
atualizaInimigos :: Tempo -> [Inimigo] -> Base -> Mapa -> ([Inimigo], Base)
atualizaInimigos dt inimigos base mapa = foldr processaInimigo ([], base) inimigos
  where
    processaInimigo :: Inimigo -> ([Inimigo], Base) -> ([Inimigo], Base)
    processaInimigo inimigo (atualizados, baseAtual)
      | vidaInimigo inimigo <= 0 = 
          (atualizados, baseAtual {creditosBase = creditosBase baseAtual + butimInimigo inimigo})
      | chegouBase inimigo (posicaoBase baseAtual) = 
          (atualizados, baseAtual {vidaBase = vidaBase baseAtual - ataqueInimigo inimigo})
      | otherwise = 
          let inimigoAtualizado = aplicaEfeitosProjeteis $ movimentaInimigo dt mapa inimigo
          in (inimigoAtualizado : atualizados, baseAtual)

-- Atualiza a base ao receber dano de inimigos
atualizaBase :: Base -> Inimigo -> Base
atualizaBase base inimigo 
  | vidaInimigo inimigo <= 0 = base {creditosBase = creditosBase base + butimInimigo inimigo}
  | chegouBase inimigo (posicaoBase base) = base {vidaBase = vidaBase base - ataqueInimigo inimigo}
  | otherwise = base

-- Função auxiliar para verificar se o inimigo chegou à base
chegouBase :: Inimigo -> Posicao -> Bool
chegouBase inimigo (xb, yb) =
  let (xi, yi) = posicaoInimigo inimigo
  in abs (xi - xb) < 0.5 && abs (yi - yb) < 0.5


-- Movimenta o inimigo no mapa
movimentaInimigo :: Tempo -> Mapa -> Inimigo -> Inimigo
movimentaInimigo dt _ inimigo
  | congelado inimigo = inimigo -- Inimigo congelado não se move
  | otherwise =
      inimigo { posicaoInimigo = (x + dx * velocidade * dt, y + dy * velocidade * dt) }
  where
    (x, y) = posicaoInimigo inimigo
    (dx, dy) = direcaoParaDelta (direcaoInimigo inimigo)
    velocidade = velocidadeInimigo inimigo * ajustaVelocidade (projeteisInimigo inimigo)

    congelado :: Inimigo -> Bool
    congelado = any (\projetil -> tipoProjetil projetil == Gelo) . projeteisInimigo

-- Ajusta a velocidade do inimigo com base nos projéteis
ajustaVelocidade :: [Projetil] -> Float
ajustaVelocidade [] = 1
ajustaVelocidade ps
  | any (\projetil -> tipoProjetil projetil == Resina) ps = 0.5
  | otherwise = 1

-- Aplica os efeitos dos projéteis no inimigo
aplicaEfeitosProjeteis :: Inimigo -> Inimigo
aplicaEfeitosProjeteis inimigo = foldl aplicaEfeito inimigo (projeteisInimigo inimigo)
  where
    aplicaEfeito :: Inimigo -> Projetil -> Inimigo
    aplicaEfeito acc (Projetil Fogo (Finita t)) = 
      acc {vidaInimigo = vidaInimigo acc - 5 * min t 1}
    aplicaEfeito acc (Projetil Fogo Infinita) = 
      acc {vidaInimigo = vidaInimigo acc - 5} -- Dano contínuo por segundo
    aplicaEfeito acc (Projetil Gelo Infinita) = 
      acc {velocidadeInimigo = 0} -- Congela o inimigo
    aplicaEfeito acc (Projetil Resina Infinita) = 
      acc {velocidadeInimigo = velocidadeInimigo acc * 0.5} -- Reduz velocidade permanentemente
    aplicaEfeito acc _ = acc

-- Atualiza as torres
atualizaTorre :: Tempo -> [Inimigo] -> Torre -> Torre
atualizaTorre dt inimigos torre
  | tempoTorre torre > 0 = torre {tempoTorre = tempoTorre torre - dt}
  | null alvos = torre
  | otherwise = torre {tempoTorre = cicloTorre torre, projetilTorre = disparaProjetil torre alvos}
  where
    alvos = inimigosNoAlcance torre inimigos

-- Função que determina inimigos no alcance da torre
inimigosNoAlcance :: Torre -> [Inimigo] -> [Inimigo]
inimigosNoAlcance torre = filter (\inimigo -> distancia (posicaoTorre torre) (posicaoInimigo inimigo) <= alcanceTorre torre) 

-- Calcula a distância entre a torre e o inimigo
distancia :: Posicao -> Posicao -> Float
distancia (x1, y1) (x2, y2) = sqrt ((x1 - x2)^(2 :: Integer) + (y1 - y2)^(2 :: Integer))

-- Função que dispara o projétil pela torre 
disparaProjetil :: Torre -> [Inimigo] -> Projetil
disparaProjetil torre alvos
  | null alvos = Projetil (tipoProjetil (projetilTorre torre)) Infinita  -- Caso não haja inimigos, cria um projétil "vazio"
  | otherwise  = Projetil (tipoProjetil (projetilTorre torre)) (Finita 1)  -- Dispara no inimigo mais próximo com tempo limitado

-- Atualiza o estado dos portais
atualizaPortal :: Tempo -> Portal -> Portal
atualizaPortal dt portal = portal {ondasPortal = map (atualizaOnda dt) (ondasPortal portal)}

-- Atualiza uma onda de inimigos
atualizaOnda :: Tempo -> Onda -> Onda
atualizaOnda dt onda
  | entradaOnda onda > 0 = onda {entradaOnda = entradaOnda onda - dt}
  | tempoOnda onda > 0 = onda {tempoOnda = tempoOnda onda - dt}
  | otherwise = onda

-- Direção para delta de movimento
direcaoParaDelta :: Direcao -> (Float, Float)
direcaoParaDelta Norte = (0, -1)
direcaoParaDelta Sul   = (0, 1)
direcaoParaDelta Este  = (1, 0)
direcaoParaDelta Oeste = (-1, 0)
