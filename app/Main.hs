module Main where

import Desenhar
import Eventos
import Graphics.Gloss
import ImmutableTowers
import Tempo
import LI12425

janela :: Display
janela = InWindow "Immutable Towers" (1920, 1080) (0, 0)

fundo :: Color
fundo = white

fr :: Int
fr = 60

it :: ImmutableTowers
it = ImmutableTowers estadoInicialJogo torreDestacada imagensJogo

estadoInicialJogo :: Jogo --trocar os valores
estadoInicialJogo = Jogo
  {baseJogo = Base
      {vidaBase = 100.0,
       posicaoBase = (5.0, 9.0), -- Posição inicial no mapa
       creditosBase = 100},
   portaisJogo = [Portal
      {posicaoPortal = (0.0, 0.0), -- Posição do portal inicial
       ondasPortal = [Onda {inimigosOnda = [Inimigo
              {posicaoInimigo = (0.0, 0.0),
               direcaoInimigo = Sul,
               vidaInimigo = 50.0,
               velocidadeInimigo = 1.0,
               ataqueInimigo = 5.0,
               butimInimigo = 10,
               projeteisInimigo = [] }],
               cicloOnda = 3.0,
               tempoOnda = 3.0,
               entradaOnda = 0.0 }] }],
    torresJogo = [],
    mapaJogo = inicialMapa,
    inimigosJogo = [],
    lojaJogo = [
      (100, Torre
        {posicaoTorre = (3.0, 3.0),
         danoTorre = 20.0,
         alcanceTorre = 5.0,
         rajadaTorre = 1,
         cicloTorre = 1.5,
         tempoTorre = 0.0,
         projetilTorre = Projetil
            { tipoProjetil = Fogo,
              duracaoProjetil = Finita 2.0
            }
        }),
      (150, Torre
        { posicaoTorre = (5.0, 5.0),
          danoTorre = 10.0,
          alcanceTorre = 4.0,
          rajadaTorre = 1,
          cicloTorre = 2.0,
          tempoTorre = 0.0,
          projetilTorre = Projetil
            {tipoProjetil = Gelo,
             duracaoProjetil = Finita 1.5}
        }),
      (50, Torre
         {posicaoTorre = (7.0, 7.0),
          danoTorre = 5.0,
          alcanceTorre = 6.0,
          rajadaTorre = 1,
          cicloTorre = 1.0,
          tempoTorre = 0.0,
          projetilTorre = Projetil
            {tipoProjetil = Resina,
             duracaoProjetil = Continua}
              })]
        }

torreDestacada :: Maybe Torre
torreDestacada = Just Torre
  { posicaoTorre = (0, 0),         -- Posição neutra (fora do mapa ou inicial)
    danoTorre = 0,                 -- Sem dano até ser personalizada
    alcanceTorre = 0,              -- Sem alcance inicial
    rajadaTorre = 0,               -- Não dispara até ser configurada
    cicloTorre = 0,                -- Sem ciclo até ser ajustada
    tempoTorre = 0,                -- Sem tempo de espera
    projetilTorre = Projetil
      { tipoProjetil = Fogo,       -- Tipo inicial, pode ser alterado pelo jogador
        duracaoProjetil = Infinita -- Duração infinita apenas como padrão
      }
  }

-- Carrega as imagens BMP para uso no jogo
imagensJogo :: IO [Picture]
imagensJogo = do
  mainMenu <- loadBMP "imagensBMP/MainMenu.bmp"
  torreFogo <- loadBMP "imagensBMP/TorreFogo.bmp"
  torreGelo <- loadBMP "imagensBMP/TorreGelo.bmp"
  basePNG <- loadBMP "imagensBMP/Base.bmp"
  return [mainMenu, torreFogo, torreGelo, basePNG]


-- | Função principal
main :: IO ()
main = do
  -- Carregar imagens
  imagemMenu <- loadBMP "imagensBMP/MainMenu.bmp"
  imagemTorreFogo <- loadBMP "imagensBMP/torrefogo.bmp"
  imagemTorreGelo <- loadBMP "imagensBMP/torregelo.bmp"
  imagemBase <- loadBMP "imagensBMP/Base.bmp"
  play janela fundo fr it desenhaJogo reageEventos reageTempo
    where it = ImmutableTowers {}


