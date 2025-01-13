module Eventos where

import Graphics.Gloss.Interface.Pure.Game
import LI12425
import Debug.Trace
import Desenhar
import Tarefa1
import ImmutableTowers

reageEventos :: Event -> ImmutableTowers -> ImmutableTowers
reageEventos (EventKey (MouseButton LeftButton) Down _ (mx, my)) it@(ImmutableTowers estado torreSelecionada) =
    trace ("Clique detectado em: " ++ show (mx, my)) $
        case torreSelecionada of
            NadaSelecionado -> selecionarTorre (cliqueNaLoja (mx,my)) it
            TorreFogo       -> tentarColocarTorre (mx, my) torreSelecionada it
            TorreGelo       -> tentarColocarTorre (mx, my) torreSelecionada it
            TorreResina     -> tentarColocarTorre (mx, my) torreSelecionada it
reageEventos _ it = it

-- Determina a torre clicada na loja, usando uma forma mais flexível
cliqueNaLoja :: (Float, Float) -> TorreSelecionada
cliqueNaLoja (mx, my) =
  trace ("Clique detectado em: " ++ show (mx, my)) $
  if isInBounds mx my (-805) (-600) (-470) (-230) then TorreResina
  else if isInBounds mx my (-355) (-145) (-470) (-230) then TorreFogo
  else if isInBounds mx my 96 304 (-470) (-230) then TorreGelo
  else NadaSelecionado

-- Verifica se as coordenadas estão dentro de um retângulo de limites dados
isInBounds :: Float -> Float -> Float -> Float -> Float -> Float -> Bool
isInBounds mx my xmin xmax ymin ymax = mx >= xmin && mx <= xmax && my >= ymin && my <= ymax

coordenadasParaMatriz :: (Float, Float) -> (Float, Float)
coordenadasParaMatriz (px, py) =
  let
    -- Ajustar coordenadas do ecrã para alinhar com a origem do mapa
    offsetX = -895-- Posição inicial em x
    offsetY = 470  -- Posição inicial em y (invertido para alinhar com a matriz)
    -- Coordenadas normalizadas para a origem do mapa
    normalizadoX = px - offsetX
    normalizadoY = offsetY - py
    -- Converter para índices da matriz
    coluna = (normalizadoX / tamanhoTerreno)
    linha  = (normalizadoY / tamanhoTerreno)
  in
    ((coluna), (linha))

-- Seleciona a torre desejada, desde que o jogador tenha créditos suficientes
selecionarTorre :: TorreSelecionada -> ImmutableTowers -> ImmutableTowers
selecionarTorre torre it@(ImmutableTowers estado NadaSelecionado) =
    let saldo = creditosBase (baseJogo estado)
        custo = case torre of
                    TorreResina -> 75
                    TorreFogo   -> 150
                    TorreGelo   -> 300
                    NadaSelecionado -> 0
    in if saldo >= custo && custo /= 0
       then it {estadoJogo = (estado {baseJogo = (baseJogo estado) {creditosBase = creditosBase (baseJogo estado) - custo}}), torreSelecionada = torre}
       else it
selecionarTorre torre it = it

tentarColocarTorre :: (Float, Float) -> TorreSelecionada -> ImmutableTowers -> ImmutableTowers
tentarColocarTorre (mx, my) torre it@(ImmutableTowers estado _) =
    let posicao = coordenadasParaMatriz (mx, my)
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