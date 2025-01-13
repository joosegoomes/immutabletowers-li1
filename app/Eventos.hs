module Eventos where

import Graphics.Gloss.Interface.Pure.Game
import LI12425
import Debug.Trace
import Desenhar
import Tarefa1
import ImmutableTowers

reageEventos :: Event -> ImmutableTowers -> ImmutableTowers
-- Handle 'Enter' to move from menu to gameplay
reageEventos (EventKey (SpecialKey KeyEnter) Down _ _) it@(ImmutableTowers _ _ MenuInicial) =
    -- Reinicia o jogo ao começar do menu inicial
    it {ecraJogo = Gameplay, estadoJogo = estadoInicialJogo}
-- Handle 'Enter' to return to the menu from win or game over screens
reageEventos (EventKey (SpecialKey KeyEnter) Down _ _) it@(ImmutableTowers _ _ Win) =
    it {ecraJogo = MenuInicial}
reageEventos (EventKey (SpecialKey KeyEnter) Down _ _) it@(ImmutableTowers _ _ GameOver) =
    it {ecraJogo = MenuInicial}
-- Handle clicks in gameplay (existing behavior)
reageEventos (EventKey (MouseButton LeftButton) Down _ (mx, my)) it@(ImmutableTowers estado torreSelecionada Gameplay) =
    trace ("Clique detectado em: " ++ show (mx, my)) $
        case torreSelecionada of
            NadaSelecionado -> selecionarTorre (clicaNaLoja (mx, my)) it
            TorreFogo       -> colocaTorre (mx, my) torreSelecionada it
            TorreGelo       -> colocaTorre (mx, my) torreSelecionada it
            TorreResina     -> colocaTorre (mx, my) torreSelecionada it
-- Default case
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
colocaTorre (mx, my) torre it@(ImmutableTowers estado _ Gameplay) =
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

-- Determina a torre clicada na loja, usando uma forma mais flexível
clicaNaLoja :: (Float, Float) -> TorreSelecionada
clicaNaLoja (mx, my)
  | dentroDaLoja mx my (-805) (-600) (-470) (-230) = TorreResina
  | dentroDaLoja mx my (-355) (-145) (-470) (-230) = TorreFogo
  | dentroDaLoja mx my 96 304 (-470) (-230) = TorreGelo
  | otherwise = NadaSelecionado

-- Verifica se as coordenadas estão dentro de um retângulo de limites dados
dentroDaLoja :: Float -> Float -> Float -> Float -> Float -> Float -> Bool
dentroDaLoja mx my xmin xmax ymin ymax = mx >= xmin && mx <= xmax && my >= ymin && my <= ymax

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