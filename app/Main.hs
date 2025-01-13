module Main where

import Desenhar
import Eventos
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import ImmutableTowers
import LI12425
import Tempo

janela :: Display
janela = InWindow "Tower Defense" (1920, 1080) (0, 0) 

fundo :: Color
fundo = white

fr :: Int
fr = 60

it :: ImmutableTowers
it = ImmutableTowers estadoInicialJogo NadaSelecionado MenuInicial

main :: IO ()
main = do
  let imagemMapa = desenhaMapa mapa
  menu <- loadBMP "imagensBMP/MenuPrincipal.bmp"
  vitoria <- loadBMP "imagensBMP/Vitoria.bmp"
  derrota <- loadBMP "imagensBMP/Derrota.bmp"

  -- Carrega as imagens dos elementos do jogo
  imagemBase <- desenhaBase "imagensBMP/BaseBMP.bmp" 13 8 
  imagemVida <- desenhaVida "imagensBMP/Vida.bmp" 16 (-3)
  imagemMoeda <- desenhaMoeda "imagensBMP/CreditosBMP.bmp" 14 (0)
  imagemTabua <- desenhaTabua "imagensBMP/Tabua.bmp" 17 (0)
  imagemPortal1 <- desenhaPortal "imagensBMP/PortalBMP.bmp" 1 0
  imagemPortal2 <- desenhaPortal "imagensBMP/PortalBMP.bmp" 4 9
  torreFogo <- loadBMP "imagensBMP/TorreFogo.bmp"
  torreGelo <- loadBMP "imagensBMP/TorreGelo.bmp"
  torreResina <- loadBMP "imagensBMP/TorreResina.bmp"
  inimigoBMP <- loadBMP "imagensBMP/InimigoImagem.bmp"
  inimigoFlip <- loadBMP "imagensBMP/InimigoFlipped.bmp"
  lojaPicture <- desenhaLoja "imagensBMP/TorreFogoLoja.bmp" "imagensBMP/TorreGeloLoja.bmp" "imagensBMP/TorreResinaLoja.bmp" 

  background <- loadBMP "imagensBMP/Fundo.bmp"

  let uiPictures (ImmutableTowers _ _ MenuInicial) = pictures [menu]
      uiPictures (ImmutableTowers estadoJogo _ Gameplay) = pictures 
          [translate 0 0 background, translate (-895) 470 $ pictures [imagemMapa, imagemBase, imagemPortal1, imagemPortal2, 
            desenhaInimigos (inimigosJogo estadoJogo) inimigoBMP inimigoFlip, desenhaTorres (torresJogo estadoJogo) [torreFogo, torreGelo, torreResina]],
           pictures [imagemVida, imagemMoeda, imagemTabua, escreveVida (baseJogo estadoJogo), escreveCreditos (baseJogo estadoJogo)], lojaPicture]
      uiPictures (ImmutableTowers _ _ Win) = pictures [vitoria]
      uiPictures (ImmutableTowers _ _ GameOver) = pictures [derrota]

  play janela fundo fr it uiPictures reageEventos reageTempo