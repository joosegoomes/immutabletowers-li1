module Main where

import Desenhar
import Eventos
import Graphics.Gloss
import ImmutableTowers
import LI12425
import Tempo

janela :: Display
janela = InWindow "Immutable Towers" (1920, 1080) (0, 0)

fundo :: Color
fundo = white

fr :: Int
fr = 60

it :: ImmutableTowers
it = ImmutableTowers estadoInicialJogo Nothing imagensJogo

estadoInicialJogo :: Jogo --trocar os valores
estadoInicialJogo = Jogo
  {baseJogo = Base
      {vidaBase = 1000.0,
       posicaoBase = (5, 5), -- Posição inicial no mapa
       creditosBase = 150},
   portaisJogo = [Portal
      {posicaoPortal = (7, 3), -- Posição do portal inicial
       ondasPortal = [Onda {inimigosOnda = [Inimigo
              {posicaoInimigo = (0.0, 0.0),
               direcaoInimigo = Sul,
               vidaInimigo = 50.0,
               velocidadeInimigo = 1.0,
               ataqueInimigo = 5.0,
               butimInimigo = 25,
               projeteisInimigo = [] }],
               cicloOnda = 3.0,
               tempoOnda = 3.0,
               entradaOnda = 0.0}] }],
    torresJogo = [],
    mapaJogo = inicialMapa,
    inimigosJogo = [],
    lojaJogo = [
          (150, Torre {posicaoTorre = (0.0, 0.0), danoTorre = 20.0, alcanceTorre = 5.0,
                      rajadaTorre = 1, cicloTorre = 1.5, tempoTorre = 0.0,
                      projetilTorre = Projetil {tipoProjetil = Fogo, duracaoProjetil = Finita 2.0}}),
          (300, Torre {posicaoTorre = (0.0, 0.0), danoTorre = 10.0, alcanceTorre = 4.0,
                      rajadaTorre = 1, cicloTorre = 2.0, tempoTorre = 0.0,
                      projetilTorre = Projetil {tipoProjetil = Gelo, duracaoProjetil = Finita 1.5}}),
          (75, Torre {posicaoTorre = (0.0, 0.0), danoTorre = 5.0, alcanceTorre = 6.0,
                      rajadaTorre = 1, cicloTorre = 1.0, tempoTorre = 0.0,
                      projetilTorre = Projetil {tipoProjetil = Resina, duracaoProjetil = Continua}})]}

main :: IO ()
main = do
  let imagemMapa = translate (-(fromIntegral largura / 1.45)) (fromIntegral altura / 1.48) (desenhaMapa mapa)
  -- Carrega as imagens
  imagemBase <- desenhaBase "imagensBMP/BaseBMP.bmp" 5 5  
  imagemVida <- desenhaVida "imagensBMP/vida.bmp" 16 (-3)
  imagemMoeda <- desenhaMoeda "imagensBMP/CreditosBMP.bmp" 14 (0)
  imagemTabua <- desenhaTabua "imagensBMP/Tabua.bmp" 17 (0)
  imagemPortal1 <- desenhaPortal "imagensBMP/PortalBMP.bmp" 7 3
  imagemPortal2 <- desenhaPortal "imagensBMP/PortalBMP.bmp" 10 12

  -- Carrega o produto final
  uiAcabada <- desenhaLoja "imagensBMP/TorreFogoLoja.bmp"  "imagensBMP/TorreGeloLoja.bmp" 
                           (pictures [imagemMapa, imagemBase, imagemPortal1, imagemPortal2, imagemVida, imagemMoeda, imagemTabua])

  play janela fundo fr it (return uiAcabada) reageEventos reageTempo



