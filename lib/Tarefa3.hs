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

-- Atualiza os inimigos no mapa e remove os derrotados
atualizaInimigos :: Tempo -> [Inimigo] -> Base -> Mapa -> ([Inimigo], Base)
atualizaInimigos tempo inimigos base mapa =
  let inimigosAtualizados = [aplicaEfeitosProjeteis inimigoAtualizado | Just inimigoAtualizado <- map (movimentaInimigo tempo mapa) inimigos]
      (vivos, baseAtualizada) = filtraInimigosVivos inimigosAtualizados base
  in (vivos, baseAtualizada)

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

-- Função auxiliar para verificar se o inimigo chegou à base
chegouBase :: Inimigo -> Base -> Bool
chegouBase inimigo base = distancia (posicaoInimigo inimigo) (posicaoBase base) <= 0.2

-- Movimenta o inimigo no mapa
movimentaInimigo :: Tempo -> Mapa -> Inimigo -> Maybe Inimigo
movimentaInimigo tempo mapa inimigo = 
  let (x, y) = posicaoInimigo inimigo
      (xb, yb) = (13, 8)  -- Coordenadas da base
  in if any (\p -> tipoProjetil p == Gelo) (projeteisInimigo inimigo) then Just inimigo
     else if x == xb && y == yb then Nothing  -- O inimigo chegou à base e deve ser removido
          else case direcaoInimigo inimigo of
            Norte -> if eTerra (x, y + 0.52) mapa  -- Verifica se o inimigo pode continuar subindo
                     then Just inimigo {posicaoInimigo = (x, y + velocidadeInimigo inimigo * tempo)}  -- Move para o próximo tile norte
                     else if eTerra (x + 0.52, y) mapa  -- Verifica se o inimigo pode virar para o leste após subir
                          then Just inimigo {direcaoInimigo = Este}  -- Vira para o leste
                          else Just inimigo {direcaoInimigo = Oeste}  -- Caso contrário, vira para o oeste
            Sul -> if eTerra (x, y - 0.52) mapa 
                   then Just inimigo {posicaoInimigo = (x, y - velocidadeInimigo inimigo * tempo)} 
                   else if eTerra (x + 0.52, y) mapa 
                        then Just inimigo {direcaoInimigo = Este} 
                        else Just inimigo {direcaoInimigo = Oeste}
            Este -> if eTerra (x + 0.52, y) mapa 
                    then Just inimigo {posicaoInimigo = (x + velocidadeInimigo inimigo * tempo, y)} 
                    else if eTerra (x, y + 0.52) mapa 
                         then Just inimigo {direcaoInimigo = Norte} 
                         else Just inimigo {direcaoInimigo = Sul}
            Oeste -> if eTerra (x - 0.52, y) mapa 
                     then Just inimigo {posicaoInimigo = (x - velocidadeInimigo inimigo * tempo, y)} 
                     else if eTerra (x, y + 0.52) mapa 
                          then Just inimigo {direcaoInimigo = Norte} 
                          else Just inimigo {direcaoInimigo = Sul}

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