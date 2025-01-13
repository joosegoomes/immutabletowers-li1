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
    novosInimigos = atualizaInimigos tempo (inimigosJogo jogo) (baseJogo jogo) (mapaJogo jogo)  -- Atualiza inimigos
     -- where (x,y) = (atualizaPortais portaisJogo)

    -- Atualizar base com base nos novos estados dos inimigos
    novaBase = foldl (\base inimigo ->
                          if chegouBase inimigo (baseJogo jogo)  -- Verifica se o inimigo chegou à base
                          then base {vidaBase = max 0 (vidaBase base - ataqueInimigo inimigo)}  -- Subtrai o dano
                          else base)
                     (baseJogo jogo)
                     (fst novosInimigos)

    -- Atualizar torres com base nos novos estados dos inimigos
    novasTorres = fst (atualizaTorres tempo (fst novosInimigos) (torresJogo jogo) (baseJogo jogo))

    -- Atualizar portais
    novosPortais = fst (atualizaPortais tempo (portaisJogo jogo))

-- Atualiza os inimigos no mapa e aplica suas ações (movimentação, dano à base)
atualizaInimigos :: Tempo -> [Inimigo] -> Base -> Mapa -> ([Inimigo], Base)
atualizaInimigos tempo inimigos base mapa =
  let inimigosAtualizados = mapMaybe (movimentaInimigo tempo mapa) inimigos
      baseComDano = foldl aplicaDanoBase base inimigos
  in (inimigosAtualizados, baseComDano)

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

-- Movimenta o inimigo no mapa
movimentaInimigo :: Tempo -> Mapa -> Inimigo -> Maybe Inimigo
movimentaInimigo tempo mapa inimigo =
  let (x, y) = posicaoInimigo inimigo
      (xB, yB) = (13, 8)  -- Coordenadas da base
      -- Aplica efeitos dos projéteis ao inimigo antes de mover
      inimigoAjustado = aplicaEfeitosProjeteis inimigo
      velocidadeAjustada = ajustaVelocidade inimigoAjustado * velocidadeInimigo inimigoAjustado
      proxPos = case direcaoInimigo inimigoAjustado of
                  Norte -> (x, y + velocidadeAjustada * tempo)
                  Sul   -> (x, y - velocidadeAjustada * tempo)
                  Este  -> (x + velocidadeAjustada * tempo, y)
                  Oeste -> (x - velocidadeAjustada * tempo, y)
  in if distancia (x, y) (xB, yB) <= 0.2  -- O inimigo chegou à base
     then Nothing  -- Remove o inimigo da lista
     else if eTerra proxPos mapa  -- O próximo passo é um terreno válido
          then Just inimigoAjustado {posicaoInimigo = proxPos}  -- Atualiza posição
          else rotacionaDirecao inimigoAjustado mapa  -- Tenta rotacionar direção

-- Rotaciona a direção do inimigo ao encontrar um obstáculo
rotacionaDirecao :: Inimigo -> Mapa -> Maybe Inimigo
rotacionaDirecao inimigo mapa =
  let (x, y) = posicaoInimigo inimigo
      novasDirecoes = case direcaoInimigo inimigo of
                        Norte -> [Este, Oeste]
                        Sul   -> [Este, Oeste]
                        Este  -> [Norte, Sul]
                        Oeste -> [Norte, Sul]
      direcaoValida = find (\direcao -> eTerra (novaPosicao direcao) mapa) novasDirecoes
      novaPosicao direcao = case direcao of
                        Norte -> (x, y + 0.52)
                        Sul   -> (x, y - 0.52)
                        Este  -> (x + 0.52, y)
                        Oeste -> (x - 0.52, y)
  in case direcaoValida of
       Just novaDirecao -> Just inimigo {direcaoInimigo = novaDirecao}
       Nothing          -> Just inimigo  -- Nenhuma direção válida, mantém como está

-- Ajusta a velocidade do inimigo com base nos projéteis
ajustaVelocidade :: Inimigo -> Float
ajustaVelocidade inimigo
  | any (\projetil -> tipoProjetil projetil == Resina) (projeteisInimigo inimigo) = 0.7
  | any (\projetil -> tipoProjetil projetil == Gelo) (projeteisInimigo inimigo) = 0
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

-- Atualiza a torre e os inimigos, aplicando efeitos e ajustando vida
atualizaTorre :: Tempo -> [Inimigo] -> Torre -> (Torre, [Inimigo])
atualizaTorre tempo inimigos torre
  | tempoTorre torre > 0 = (torre {tempoTorre = tempoTorre torre - tempo}, inimigos)
  | null alvos = (torre, inimigos)
  | otherwise = (torreAtualizada, inimigosAtualizados)
  where
    alvos = inimigosNoAlcance torre inimigos
    proj = disparaProjetil torre alvos
    torreAtualizada = torre {tempoTorre = cicloTorre torre, projetilTorre = proj}
    inimigosAtualizados = map (atingeInimigo torre) inimigos

-- Atualiza todas as torres e os inimigos em um único passo
atualizaTorres :: Tempo -> [Inimigo] -> [Torre] -> Base -> ([Torre], [Inimigo])
atualizaTorres tempo inimigos torres base =
  let (torresAtualizadas, inimigosAtualizados) = foldl atualiza ([], inimigos) torres
  in (torresAtualizadas, fst (filtraInimigosVivos inimigosAtualizados base))
  where
    atualiza (torresAtualizadas, inimigosRestantes) torre =
      let (torreAtualizada, novosInimigos) = atualizaTorre tempo inimigosRestantes torre
      in (torresAtualizadas ++ [torreAtualizada], novosInimigos)

-- Função que dispara o projétil pela torre
disparaProjetil :: Torre -> [Inimigo] -> Projetil
disparaProjetil torre alvos
  | null alvos = Projetil (tipoProjetil (projetilTorre torre)) Infinita  -- Caso não haja inimigos, cria um projétil "vazio"
  | otherwise  = Projetil (tipoProjetil (projetilTorre torre)) (Finita 1)  -- Dispara no inimigo mais próximo com tempo limitado

-- Atualiza o estado dos portais
atualizaPortais :: Tempo -> [Portal] -> ([Portal], [Inimigo])
atualizaPortais tempo portais = 
  let (novosPortais, inimigosGerados) = unzip $ map (atualizaPortal tempo) portais
  in (novosPortais, concat inimigosGerados)

-- Atualiza o estado de um único portal
atualizaPortal :: Tempo -> Portal -> (Portal, [Inimigo])
atualizaPortal tempo portal = 
  let (novasOndas, inimigosGerados) = unzip $ map (atualizaOnda tempo) (ondasPortal portal)
  in (portal {ondasPortal = novasOndas}, concat inimigosGerados)

-- Atualiza uma onda de inimigos
atualizaOnda :: Tempo -> Onda -> (Onda, [Inimigo])
atualizaOnda tempo onda
  | entradaOnda onda > 0 = (onda {entradaOnda = entradaOnda onda - tempo}, [])
  | tempoOnda onda <= 0  = (onda {tempoOnda = cicloOnda onda}, lancarInimigos (onda {tempoOnda = cicloOnda onda}))
  | otherwise = (onda, lancarInimigos onda)

-- Função para lançar os inimigos da onda
lancarInimigos :: Onda -> [Inimigo]
lancarInimigos onda = inimigosOnda onda

  -- tempoOnda <= 0 -> tempoOnda == cicloOnda 
  