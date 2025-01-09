module Eventos where

import Graphics.Gloss.Interface.Pure.Game
import ImmutableTowers
import LI12425

-- Reage aos eventos de tempo e cliques
reageEventos :: Event -> ImmutableTowers -> ImmutableTowers
reageEventos (EventKey (SpecialKey key) Down _ _) it
  | key `elem` [KeyEnter, KeySpace] = iniciaJogo it
reageEventos (EventKey (MouseButton LeftButton) Down _ pos) it
  | posicaoDentroBotaoStart pos = iniciaJogo it
  | otherwise = compraTorre pos it
reageEventos _ it = it

-- Verifica se a posição do clique está no botão "Start"
posicaoDentroBotaoStart :: Posicao -> Bool
posicaoDentroBotaoStart (x, y) = x >= -1.0 && x <= 1.0 && y >= -0.5 && y <= 0.5

-- Inicia o jogo, saindo do menu
iniciaJogo :: ImmutableTowers -> ImmutableTowers
iniciaJogo it = it {estadoJogo = estadoInicialJogo}

-- Compra uma torre, se possível
compraTorre :: Posicao -> ImmutableTowers -> ImmutableTowers
compraTorre pos it =
  case encontraTorreNaLoja pos (lojaJogo (estadoJogo it)) of
    Just (creditos, torre) ->
      if creditosSuficientes creditos it
      then atualizaCreditosETorres creditos torre it
      else it
    Nothing -> it

-- Verifica se há créditos suficientes para a compra
creditosSuficientes :: Creditos -> ImmutableTowers -> Bool
creditosSuficientes creditos it = creditosBase (baseJogo (estadoJogo it)) >= creditos

-- Atualiza os créditos e adiciona a torre comprada
atualizaCreditosETorres :: Creditos -> Torre -> ImmutableTowers -> ImmutableTowers
atualizaCreditosETorres creditos torre it =
  it { estadoJogo = jogoAtualizado }
  where
    jogo = estadoJogo it
    jogoAtualizado = jogo
      { baseJogo = (baseJogo jogo) { creditosBase = creditosBase (baseJogo jogo) - creditos },
        torresJogo = torre : torresJogo jogo
      }

-- Verifica se uma torre da loja foi clicada
encontraTorreNaLoja :: Posicao -> [(Creditos, Torre)] -> Maybe (Creditos, Torre)
encontraTorreNaLoja pos loja =
  case filter (\(_, torre) -> posDentroArea pos (posicaoTorre torre)) loja of
    (torre : _) -> Just torre
    [] -> Nothing

-- Verifica se a posição está dentro de uma área específica
posDentroArea :: Posicao -> Posicao -> Bool
posDentroArea (x, y) (cx, cy) =
  abs (x - cx) < 0.5 && abs (y - cy) < 0.5

-- Coloca uma torre no mapa, se a posição for válida
colocaTorreNoMapa :: Posicao -> ImmutableTowers -> ImmutableTowers
colocaTorreNoMapa pos it@(ImmutableTowers {estadoJogo = jogo, torreDestacada = Just torre})
  | posicaoValida pos (mapaJogo jogo) (torresJogo jogo) =
      it { estadoJogo = jogo { torresJogo = novaTorre : torresJogo jogo } }
  | otherwise = it
  where
    novaTorre = torre { posicaoTorre = pos }
colocaTorreNoMapa _ it = it -- Sem torre selecionada

-- Verifica se uma posição é válida para colocar a torre
posicaoValida :: Posicao -> Mapa -> [Torre] -> Bool
posicaoValida pos mapa torres =
  not (torreJaExiste pos torres) &&
  terrenoValidoParaTorre pos mapa &&
  posDentroMapa pos mapa

-- Verifica se já existe uma torre na posição
torreJaExiste :: Posicao -> [Torre] -> Bool
torreJaExiste pos torres = any (\t -> posicaoTorre t == pos) torres

-- Valida se o terreno é apropriado para uma torre
terrenoValidoParaTorre :: Posicao -> Mapa -> Bool
terrenoValidoParaTorre pos mapa =
  tipoTerreno pos mapa == Relva

-- Verifica se a posição está dentro dos limites do mapa
posDentroMapa :: Posicao -> Mapa -> Bool
posDentroMapa (x, y) mapa = ix >= 0 && ix < length mapa && iy >= 0 && iy < length (head mapa)
  where
    ix = floor x
    iy = floor y

-- Retorna o tipo de terreno na posição
tipoTerreno :: Posicao -> Mapa -> Terreno
tipoTerreno (x, y) mapa
  | posDentroMapa (x, y) mapa = mapa !! iy !! ix
  | otherwise = error "Posição fora dos limites do mapa"
  where
    ix = floor x
    iy = floor y
