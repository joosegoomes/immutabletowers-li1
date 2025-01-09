module ImmutableTowers where

import Graphics.Gloss
import LI12425

data ImmutableTowers = ImmutableTowers
   {estadoJogo :: Jogo, 
    torreDestacada :: Maybe Torre,
    imagens :: Imagens}

data Imagens = ImagensJogo
  {mainMenu :: Picture,
  portalPNG :: Picture,
  inimigoPNG :: Picture, 
  torreResina :: Picture,
  torreFogo :: Picture, 
  torreGelo :: Picture, 
  basePNG :: Picture}