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
import Debug.Trace (trace)
import Data.Maybe
import Data.List (find)

-- Atualiza o estado do jogo (inimigos e base)
atualizaJogo :: Tempo -> Jogo -> Jogo
atualizaJogo tempo jogo =  
  jogo  {inimigosJogo = fst novosInimigos,
         torresJogo   = novasTorres,
         baseJogo     = novaBase,
         portaisJogo  = novosPortais}
  where
    -- Atualizar inimigos com movimento e aplicar efeitos
    novosInimigos = atualizaInimigos tempo (inimigosJogo jogo) (baseJogo jogo) (mapaJogo jogo)

    -- Atualizar base com base nos novos estados dos inimigos
    novaBase = foldl (\base inimigo -> 
                          if chegouBase inimigo (baseJogo jogo)  -- Verifica se o inimigo chegou à base
                          then base {vidaBase = max 0 (vidaBase base - ataqueInimigo inimigo)}  -- Subtrai o dano
                          else base) 
                     (baseJogo jogo) 
                     (fst novosInimigos)

    -- Atualizar torres com base nos novos estados dos inimigos
    novasTorres = map (atualizaTorre tempo (fst novosInimigos)) (torresJogo jogo)

    -- Atualizar portais
    novosPortais = map (atualizaPortal tempo) (portaisJogo jogo)

atualizaInimigos :: Tempo -> [Inimigo] -> Base -> Mapa -> ([Inimigo], Base)
atualizaInimigos tempo inimigos base mapa =
  let (inimigosAtualizados, baseAtualizada) = foldl processaInimigo ([], base) inimigos
  in (reverse inimigosAtualizados, baseAtualizada)
  where
    -- Processa cada inimigo, movendo e aplicando dano à base se necessário
    processaInimigo (vivos, baseAtualizada) inimigo =
      case movimentaInimigo tempo mapa inimigo of
        Nothing -> (vivos, baseAtualizada {vidaBase = max 0 (vidaBase baseAtualizada - ataqueInimigo inimigo)})
        Just inimigoAtualizado -> (inimigoAtualizado : vivos, baseAtualizada)

-- Aplica dano à base se o inimigo chegou à base
aplicaDanoBase :: Base -> Inimigo -> Base
aplicaDanoBase base inimigo
  | chegouBase inimigo base = base {vidaBase = max 0 (vidaBase base - ataqueInimigo inimigo)}
  | otherwise = base

-- Filtra inimigos vivos, removendo os derrotados ou os que chegaram à base
filtraInimigosVivos :: [Inimigo] -> Base -> ([Inimigo], Base)
filtraInimigosVivos inimigos base =
  let (inimigosVivos, baseAtualizada) = foldl processaInimigo ([], base) inimigos
  in (reverse inimigosVivos, baseAtualizada)
  where
    -- Filtra e processa cada inimigo
    processaInimigo (vivos, baseAtualizada) inimigo
      | vidaInimigo inimigo <= 0 || chegouBase inimigo base = 
          -- Inimigo derrotado ou chegou à base, acumula butim e subtrai dano
          (vivos, baseAtualizada {creditosBase = creditosBase baseAtualizada + butimInimigo inimigo,vidaBase = if chegouBase inimigo base 
                       then max 0 (vidaBase baseAtualizada - ataqueInimigo inimigo)  -- Subtrai o dano
                       else vidaBase baseAtualizada })
      | otherwise = (inimigo : vivos, baseAtualizada)  -- Mantém o inimigo vivo

-- Atualiza o estado do jogo (inimigos e base)
atualizaEstado :: Tempo -> [Inimigo] -> Base -> Mapa -> ([Inimigo], Base)
atualizaEstado tempo inimigos base mapa =
  let (inimigosAtualizados, baseComDano) = atualizaInimigos tempo inimigos base mapa
      baseFinal = foldl (\baseAtualizado inimigo -> 
                          if chegouBase inimigo base  -- Verifica se o inimigo chegou à base
                          then baseAtualizado {vidaBase = max 0 (vidaBase baseAtualizado - ataqueInimigo inimigo)}  -- Subtrai o dano
                          else baseAtualizado) 
                        baseComDano 
                        inimigosAtualizados
  in (inimigosAtualizados, baseFinal)

-- Verifica se o inimigo chegou à base
chegouBase :: Inimigo -> Base -> Bool
chegouBase inimigo base =
  let (x, y) = posicaoInimigo inimigo
      (xb, yb) = posicaoBase base
  in distancia (x, y) (xb, yb) <= 0.2

movimentaInimigo :: Tempo -> Mapa -> Inimigo -> Maybe Inimigo
movimentaInimigo tempo mapa inimigo =
  let (x, y) = posicaoInimigo inimigo
      (xb, yb) = (13, 8)  -- Coordenadas da base
      vel = velocidadeInimigo inimigo * tempo
      proxPos = case direcaoInimigo inimigo of
                  Norte -> (x, y + vel)
                  Sul   -> (x, y - vel)
                  Este  -> (x + vel, y)
                  Oeste -> (x - vel, y)
      -- Centered position when the enemy is very close to the base
      centeredAtBase = distancia (x, y) (xb, yb) <= 0.2
      -- Update position if the next position is valid
      moveOrRotate =
        if eTerra proxPos mapa  -- Proximal position is valid
        then Just inimigo {posicaoInimigo = proxPos}
        else rotacionaDirecao inimigo mapa  -- Rotate to find valid path
  in if centeredAtBase
     then Nothing  -- Remove enemy from the game and deal damage to the base
     else if any (\p -> tipoProjetil p == Gelo) (projeteisInimigo inimigo)
          then Just inimigo  -- Frozen enemies don't move
          else moveOrRotate

-- Rotate the enemy's direction to find a valid path
rotacionaDirecao :: Inimigo -> Mapa -> Maybe Inimigo
rotacionaDirecao inimigo mapa =
  let (x, y) = posicaoInimigo inimigo
      novasDirecoes = case direcaoInimigo inimigo of
                        Norte -> [Este, Oeste]
                        Sul   -> [Este, Oeste]
                        Este  -> [Norte, Sul]
                        Oeste -> [Norte, Sul]
      novaPosicao d = case d of
                        Norte -> (x, y + 0.52)
                        Sul   -> (x, y - 0.52)
                        Este  -> (x + 0.52, y)
                        Oeste -> (x - 0.52, y)
      direcaoValida = find (\d -> eTerra (novaPosicao d) mapa) novasDirecoes
  in case direcaoValida of
       Just novaDirecao -> Just inimigo {direcaoInimigo = novaDirecao}
       Nothing          -> Just inimigo  -- No valid direction found

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