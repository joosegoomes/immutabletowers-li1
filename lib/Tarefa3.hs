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

atualizaJogo :: Tempo -> Jogo -> Jogo
atualizaJogo tempo jogo = 
  jogo  {inimigosJogo = novosInimigos,
         torresJogo   = novasTorres,
         baseJogo     = novaBase,
         portaisJogo  = novosPortais}
  where
    -- Atualizar inimigos com movimento e aplicar efeitos
    novosInimigos = atualizaInimigos tempo (inimigosJogo jogo) (baseJogo jogo) (mapaJogo jogo)

    -- Atualizar base com base no dano causado pelos inimigos
    novaBase = foldl atualizaBase (baseJogo jogo) novosInimigos

    -- Atualizar torres com base nos novos estados dos inimigos
    novasTorres = map (atualizaTorre tempo novosInimigos) (torresJogo jogo)

    -- Atualizar portais
    novosPortais = map (atualizaPortal tempo) (portaisJogo jogo)

-- Atualiza os inimigos no mapa
atualizaInimigos :: Tempo -> [Inimigo] -> Base -> Mapa -> [Inimigo]
atualizaInimigos tempo inimigos base mapa = [aplicaEfeitosProjeteis (movimentaInimigo tempo mapa inimigo) | inimigo <- inimigos]

atualizaInimigo :: Tempo -> Base -> Mapa -> Inimigo -> Inimigo
atualizaInimigo tempo base mapa inimigo = movimentaInimigo tempo mapa inimigo

-- Atualiza a base ao receber dano de inimigos que chegaram
-- Atualiza a base ao receber dano de inimigos que chegaram
atualizaBase :: Base -> Inimigo -> Base
atualizaBase base inimigo
  | chegouBase inimigo (posicaoBase base) = base 
      { vidaBase = max 0 (vidaBase base - ataqueInimigo inimigo), creditosBase = creditosBase base + butimInimigo inimigo } -- Adiciona os créditos do inimigo
  | otherwise = base


-- Função auxiliar para verificar se o inimigo chegou à base
chegouBase :: Inimigo -> Posicao -> Bool
chegouBase inimigo (xb, yb) =
  let (xi, yi) = posicaoInimigo inimigo
  in abs (xi - xb) < 0.5 && abs (yi - yb) < 0.5

-- Movimenta o inimigo no mapa
movimentaInimigo :: Tempo -> Mapa -> Inimigo -> Inimigo
movimentaInimigo tempo mapa inimigo = 
  let (x, y) = posicaoInimigo inimigo
  in if any (\p -> tipoProjetil p == Gelo) (projeteisInimigo inimigo) then inimigo
     else case direcaoInimigo inimigo of
          Norte -> if eTerra ( x, y + 1) mapa then inimigo {posicaoInimigo = (x, y + velocidadeInimigo inimigo * tempo)} else if eTerra ( x + 1,   y) mapa then inimigo {direcaoInimigo = Este} else inimigo {direcaoInimigo = Oeste}
          Sul -> if eTerra ( x, y - 1) mapa then inimigo {posicaoInimigo = (x, y - velocidadeInimigo inimigo * tempo)} else if eTerra ( x + 1,   y) mapa then inimigo {direcaoInimigo = Este} else inimigo {direcaoInimigo = Oeste}
          Este -> if eTerra ( x + 1,  y) mapa then inimigo {posicaoInimigo = (x + velocidadeInimigo inimigo * tempo, y)} else if eTerra ( x,   y + 1) mapa then inimigo {direcaoInimigo = Norte} else inimigo {direcaoInimigo = Sul}
          Oeste -> if eTerra ( x - 1,  y) mapa then inimigo {posicaoInimigo = (x - velocidadeInimigo inimigo * tempo, y)} else if eTerra ( x,   y + 1) mapa then inimigo {direcaoInimigo = Norte} else inimigo {direcaoInimigo = Sul}

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
