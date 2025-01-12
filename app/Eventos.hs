module Eventos where

import Graphics.Gloss.Interface.Pure.Game
import LI12425
import Debug.Trace
import Desenhar
import Tarefa1
import ImmutableTowers

reageEventos :: Event -> ImmutableTowers -> ImmutableTowers
reageEventos (EventKey (MouseButton LeftButton) Down _ (x, y)) it = trace ("Clique registrado em: " ++ show (x, y)) it    
reageEventos _ it = it

-- Reage aos eventos do jogo, como a seleção e posicionamento de torres
reageJogo :: Event -> ImmutableTowers -> ImmutableTowers
reageJogo (EventKey (MouseButton LeftButton) Up _ (mx, my)) it@(ImmutableTowers estado _) =
    case cliqueNaLoja (mx, my) of
        TorreFogo   -> selecionarTorre TorreFogo it
        TorreGelo   -> selecionarTorre TorreGelo it
        TorreResina -> selecionarTorre TorreResina it
        NadaSelecionado ->
            case torreSelecionada estado of
                NadaSelecionado -> it -- Nenhuma torre está selecionada, feedback visual pode ser adicionado aqui
                torre -> tentarColocarTorre (mx, my) torre it
reageJogo _ it = it

-- Determina a torre clicada na loja, usando uma forma mais flexível
cliqueNaLoja :: (Float, Float) -> TorreSelecionada
cliqueNaLoja (mx, my)
    | isInBounds mx my (-805) (-600) (-470) (-230) = TorreResina
    | isInBounds mx my (-355) (-145) (-470) (-230) = TorreFogo
    | isInBounds mx my 96 304 (-470) (-230) = TorreGelo
    | otherwise = NadaSelecionado

-- Verifica se as coordenadas estão dentro de um retângulo de limites dados
isInBounds :: Float -> Float -> Float -> Float -> Float -> Float -> Bool
isInBounds mx my xmin xmax ymin ymax = mx >= xmin && mx <= xmax && my >= ymin && my <= ymax

-- Seleciona a torre desejada, desde que o jogador tenha créditos suficientes
selecionarTorre :: TorreSelecionada -> ImmutableTowers -> ImmutableTowers
selecionarTorre torre it@(ImmutableTowers estado _) =
    let custo = custoTorre torre
        baseAtual = baseJogo estado
    in if creditosBase baseAtual >= custo
       then ImmutableTowers (estado {baseJogo = baseAtual {creditosBase = creditosBase baseAtual - custo}, torreSelecionada = Just torre})
       else it

-- Calcula o custo de uma torre
custoTorre :: TorreSelecionada -> Int
custoTorre TorreResina = 75
custoTorre TorreFogo   = 150
custoTorre TorreGelo   = 300
custoTorre NadaSelecionado = 0

-- Tenta colocar a torre no mapa, se for válido
tentarColocarTorre :: (Float, Float) -> TorreSelecionada -> ImmutableTowers -> ImmutableTowers
tentarColocarTorre (mx, my) torre it@(ImmutableTowers estado) =
    let posicao = (mx, my)
    in if eRelva posicao (mapaJogo estado) && not (existeTorre posicao (torresJogo estado))
       then let novaTorre = criarTorre torre posicao
            in ImmutableTowers (estado { torresJogo = novaTorre : torresJogo estado, torreSelecionada = Nothing })
       else it -- Feedback could be added here, such as a message for invalid placement

-- Cria a torre de acordo com o tipo selecionado
criarTorre :: TorreSelecionada -> Posicao -> Torre
criarTorre TorreFogo (px, py) = Torre (px, py) 10 100 1 1 0 (Projetil Fogo (Finita 5))
criarTorre TorreGelo (px, py) = Torre (px, py) 5 120 1 2 0 (Projetil Gelo (Finita 7))
criarTorre TorreResina (px, py) = Torre (px, py) 2 80 3 3 0 (Projetil Resina (Finita 10))
criarTorre NadaSelecionado _ = error "Nenhuma torre selecionada"

-- Verifica se já existe uma torre na posição
existeTorre :: Posicao -> [Torre] -> Bool
existeTorre posicao = any (\torre -> posicaoTorre torre == posicao)



