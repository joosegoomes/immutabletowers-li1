module Eventos where

import Graphics.Gloss.Interface.Pure.Game
import LI12425
import Debug.Trace
import Desenhar
import Tarefa1
import ImmutableTowers

reageEventos :: Event -> ImmutableTowers -> ImmutableTowers
reageEventos (EventKey (SpecialKey KeyEnter) Down _ _) it@(ImmutableTowers _ _ MenuInicial) =
    it {ecraJogo = Gameplay, estadoJogo = estadoInicialJogo}
reageEventos (EventKey (SpecialKey KeyEnter) Down _ _) it@(ImmutableTowers _ _ Win) =
    it {ecraJogo = MenuInicial}
reageEventos (EventKey (SpecialKey KeyEnter) Down _ _) it@(ImmutableTowers _ _ GameOver) =
    it {ecraJogo = MenuInicial}
reageEventos (EventKey (MouseButton LeftButton) Down _ (x, y)) it@(ImmutableTowers estado torreSelecionada Gameplay) =
    trace ("Clique detectado em: " ++ show (coordenadasParaMatriz (x, y))) $
        case torreSelecionada of
            NadaSelecionado -> selecionarTorre (clicaNaLoja (x, y)) it
            TorreFogo       -> colocaTorre (x, y) torreSelecionada it
            TorreGelo       -> colocaTorre (x, y) torreSelecionada it
            TorreResina     -> colocaTorre (x, y) torreSelecionada it
reageEventos _ it = it

-- Seleciona a torre desejada, desde que o jogador tenha créditos suficientes
selecionarTorre :: TorreSelecionada -> ImmutableTowers -> ImmutableTowers
selecionarTorre torre it@(ImmutableTowers estado NadaSelecionado Gameplay) =
    let saldo = creditosBase (baseJogo estado)
        custo = case torre of
                    TorreResina -> 75
                    TorreFogo   -> 150
                    TorreGelo   -> 300
                    NadaSelecionado -> 0
    in if saldo >= custo && custo /= 0
       then it {estadoJogo = (estado {baseJogo = (baseJogo estado) {creditosBase = creditosBase (baseJogo estado) - custo}}), torreSelecionada = torre}
       else it
selecionarTorre _ it = it

colocaTorre :: (Float, Float) -> TorreSelecionada -> ImmutableTowers -> ImmutableTowers
colocaTorre (x, y) torre it@(ImmutableTowers estado _ Gameplay) =
    let posicao = coordenadasParaMatriz (x, y)
        validaRelva = eRelva posicao (mapaJogo estado)
        validaTorre = not (existeTorre posicao (torresJogo estado))
    in if validaRelva && validaTorre
       then let novaTorre = criarTorre torre posicao
            in it {estadoJogo = estado {torresJogo = novaTorre : torresJogo estado}, torreSelecionada = NadaSelecionado}
       else it

-- Cria a torre de acordo com o tipo selecionado
criarTorre :: TorreSelecionada -> Posicao -> Torre
criarTorre TorreFogo (px, py) = Torre (fromInteger (floor px), fromInteger (floor py)) 10 100 1 1 0 (Projetil Fogo (Finita 5))
criarTorre TorreGelo (px, py) = Torre (fromInteger (floor px), fromInteger (floor py))  5 120 1 2 0 (Projetil Gelo (Finita 7))
criarTorre TorreResina (px, py) = Torre (fromInteger (floor px), fromInteger (floor py))  2 80 3 3 0 (Projetil Resina (Finita 10))
criarTorre NadaSelecionado _ = error "Nenhuma torre selecionada"

-- Verifica se já existe uma torre na posição
existeTorre :: Posicao -> [Torre] -> Bool
existeTorre posicao = any (\torre -> posicaoTorre torre == posicao)

-- Determina a torre clicada na loja, usando uma forma mais flexível
clicaNaLoja :: (Float, Float) -> TorreSelecionada
clicaNaLoja (x, y)
  | dentroDaLoja x y (-805) (-600) (-470) (-230) = TorreResina
  | dentroDaLoja x y (-355) (-145) (-470) (-230) = TorreFogo
  | dentroDaLoja x y 96 304 (-470) (-230) = TorreGelo
  | otherwise = NadaSelecionado

-- Verifica se as coordenadas estão dentro de um retângulo de limites dados
dentroDaLoja :: Float -> Float -> Float -> Float -> Float -> Float -> Bool
dentroDaLoja x y xmin xmax ymin ymax = x >= xmin && x <= xmax && y >= ymin && y <= ymax

coordenadasParaMatriz :: (Float, Float) -> (Float, Float)
coordenadasParaMatriz (px, py) =
  let
    -- Deslocamento para alinhar a origem do mapa com a origem da tela
    offsetX = -925  -- Coordenada X mínima da tela
    offsetY = 505   -- Coordenada Y máxima da tela (invertido para alinhar com a matriz)

    -- Coordenadas normalizadas para o sistema de referência do mapa
    normalizadoX = px - offsetX
    normalizadoY = offsetY - py

    -- Converter para índices da matriz (arredondando para o índice mais próximo)
    coluna = fromIntegral (floor (normalizadoX / tamanhoTerreno))
    linha  = fromIntegral (floor (normalizadoY / tamanhoTerreno))
  in
    (coluna, linha)

-- X maximo = 500
-- X minimo = -925
-- Y minimo = -210
-- Y maximo = 505