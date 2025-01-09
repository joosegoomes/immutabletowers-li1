module Desenhar where

import Graphics.Gloss
import ImmutableTowers
import LI12425

-- | Estrutura principal do jogo
data MundoJogo = MundoJogo
   {estado :: Jogo,        -- Estado atual do jogo
    imagemMenu :: Picture,       -- Imagem para o menu inicial
    imagemTorreFogo :: Picture,  -- Imagem da torre de fogo
    imagemTorreGelo :: Picture,
    imagemTorreResina :: Picture,
    imagemInimigo :: Picture,
    imagemPortal :: Picture,
    imagemBase :: Picture}   -- Imagem da torre de gelo

desenha :: Jogo -> Imagens -> Picture
desenha jogo imagens =
  pictures
     [desenharMapa (mapaJogo jogo),
      desenharInimigos (inimigosJogo jogo) (inimigoPNG imagens),
      desenharPortais (portaisJogo jogo) (portalPNG imagens),
      desenharTorres (torresJogo jogo) (torreFogo imagens) (torreGelo imagens) (torreResina imagens),
      desenharBase (baseJogo jogo) (basePNG imagens)]

-- | Exemplo de mapa 10x10
mapa :: Mapa
mapa =
  [ [a, t, a, a, r, r, a, a, a, a],
    [a, t, a, a, a, a, a, a, a, a],
    [r, t, r, a, a, a, a, a, a, a],
    [r, t, r, a, r, r, a, a, a, a],
    [r, t, r, r, r, r, r, r, r, a],
    [t, t, t, t, t, t, t, t, t, r],
    [r, r, a, r, t, r, r, r, t, r],
    [r, r, a, a, t, r, r, r, t, r],
    [r, a, a, a, t, a, a, r, t, r],
    [a, a, a, a, t, a, a, r, t, t]
  ]
  where
    t = Terra
    r = Relva
    a = Agua

-- | Dimensões da janela
larguraJanela, alturaJanela :: Int
larguraJanela = 1920
alturaJanela = 1080

-- | Dimensões de cada Terreno no mapa
larguraTerreno, alturaTerreno :: Float
larguraTerreno = fromIntegral larguraJanela / fromIntegral (length (head mapa))
alturaTerreno = fromIntegral alturaJanela / fromIntegral (length mapa)

-- | Converter tipo de Terreno para cor
corTerreno :: Terreno -> Color
corTerreno Terra = makeColorI 139 69 19 255 -- Castanho para caminho
corTerreno Relva = makeColorI 34 139 34 255 -- Verde para relva
corTerreno Agua  = makeColorI 70 130 180 255 -- Azul para água

-- | Desenhar um Terreno na sua posição
desenharTerreno :: Terreno -> Float -> Float -> Picture
desenharTerreno terreno x y = translate posX posY $ color (corTerreno terreno) $ rectangleSolid larguraTerreno alturaTerreno
  where
    posX = x * larguraTerreno - (fromIntegral larguraJanela / 2) + (larguraTerreno / 2)
    posY = -y * alturaTerreno + (fromIntegral alturaJanela / 2) - (alturaTerreno / 2)

-- | Desenhar o mapa inteiro
desenharMapa :: Mapa -> Picture
desenharMapa mapa =
  pictures [desenharTerreno terreno (fromIntegral x) (fromIntegral y)
           | (linha, y) <- zip mapa [0..], (terreno, x) <- zip linha [0..]]

desenharInimigos :: [Inimigo] -> Picture -> Picture
desenharInimigos inimigos imagemInimigo =
  pictures [translate posX posY imagemInimigo | inimigo <- inimigos, let (posX, posY) = ajustarPosicao (posicaoInimigo inimigo)]
  where
    ajustarPosicao (x, y) =
      (x * larguraTerreno - (fromIntegral larguraJanela / 2) + (larguraTerreno / 2),
       -y * alturaTerreno + (fromIntegral alturaJanela / 2) - (alturaTerreno / 2))

desenharPortais :: [Portal] -> Picture -> Picture
desenharPortais portais imagemPortal =
  pictures [translate posX posY imagemPortal | portal <- portais, let (posX, posY) = ajustarPosicao (posicaoPortal portal)]
  where
    ajustarPosicao (x, y) =
      (x * larguraTerreno - (fromIntegral larguraJanela / 2) + (larguraTerreno / 2),
       -y * alturaTerreno + (fromIntegral alturaJanela / 2) - (alturaTerreno / 2))

desenharTorres :: [Torre] -> Picture -> Picture -> Picture -> Picture
desenharTorres torres imagemFogo imagemGelo imagemResina =
  pictures [desenharTorre torre | torre <- torres]
  where
    desenharTorre torre =
      translate posX posY (selecionarImagem torre)
      where
        (posX, posY) = ajustarPosicao (posicaoTorre torre)
        selecionarImagem t
          | tipoProjetil (projetilTorre t) == Fogo = imagemFogo
          | tipoProjetil (projetilTorre t) == Gelo = imagemGelo
          | tipoProjetil (projetilTorre t) == Resina = imagemResina
          | otherwise = blank
    ajustarPosicao (x, y) =
      (x * larguraTerreno - (fromIntegral larguraJanela / 2) + (larguraTerreno / 2),
       -y * alturaTerreno + (fromIntegral alturaJanela / 2) - (alturaTerreno / 2))

desenharBase :: Base -> Picture -> Picture
desenharBase base imagemBase =
  translate posX posY imagemBase
  where
    (posX, posY) = ajustarPosicao (posicaoBase base)
    ajustarPosicao (x, y) =
      (x * larguraTerreno - (fromIntegral larguraJanela / 2) + (larguraTerreno / 2),
       -y * alturaTerreno + (fromIntegral alturaJanela / 2) - (alturaTerreno / 2))
