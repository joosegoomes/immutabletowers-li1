module Desenhar where
import Graphics.Gloss
import LI12425

mapa :: Mapa
mapa =
  [[a, t, a, a, r, r, a, a, a, r, t, t, t, r, r, r, r, a, a, a],
   [a, t, a, a, a, a, a, a, a, r, t, r, t, t, r, r, r, r, a, a],
   [r, t, r, a, a, a, a, a, a, a, t, r, r, t, t, r, r, r, a, a],
   [r, t, r, a, r, r, a, a, a, a, t, a, a, a, t, t, r, r, r, a],
   [r, t, r, r, r, r, r, r, r, a, t, a, a, a, r, t, t, r, r, a],
   [r, t, t, t, t, t, t, t, t, r, t, r, a, a, r, r, t, t, r, r],
   [r, r, a, r, t, r, r, r, t, r, t, r, a, a, r, r, r, t, t, r],
   [r, r, a, a, t, r, r, r, t, r, t, r, a, a, r, r, r, r, t, r],
   [r, a, a, a, t, a, a, r, t, r, t, r, a, t, t, t, t, t, t, r],
   [a, a, a, a, t, a, a, r, t, t, t, r, a, a, r, r, r, r, r, r]]
  where
   t = Terra
   r = Relva
   a = Agua

largura, altura :: Int
largura = 1300
altura = 700

-- | Ajusta o tamanho de cada Terreno de acordo com o mapa
tamanhoTerreno :: Float
tamanhoTerreno = min (fromIntegral largura / fromIntegral (length (head mapa))) (fromIntegral altura / fromIntegral (length mapa)) * 1.1 

-- | Cria cores para os diferentes tipos de Terreno
corDoTerreno :: Terreno -> Color
corDoTerreno Terra = makeColorI 139 69 19 255 -- Cor da Terra
corDoTerreno Relva = makeColorI 34 139 34 255 -- Cor da Relva
corDoTerreno Agua  = makeColorI 70 130 180 255 -- Cor da Agua

-- | Desenha um unico terreno
desenhaterreno :: Terreno -> Float -> Float -> Picture
desenhaterreno terreno x y = translate (x * tamanhoTerreno) (-y * tamanhoTerreno) $ color (corDoTerreno terreno) $ rectangleSolid tamanhoTerreno tamanhoTerreno

-- | Desenha o Mapa inteiro
desenhaMapa :: Mapa -> Picture
desenhaMapa mapa = pictures [desenhaterreno terreno (fromIntegral x) (fromIntegral y) | (row, y) <- zip mapa [0..], (terreno, x) <- zip row [0..]]

desenhaBase :: FilePath -> Int -> Int -> IO Picture
desenhaBase imagePath x y = do
  baseImagem <- loadBMP imagePath
  let imagemAjustada = scale (0.2) (0.2) baseImagem
      posX = fromIntegral x * tamanhoTerreno - fromIntegral largura / 4.15
      posY = -fromIntegral y * tamanhoTerreno + fromIntegral altura / 2.6
  return $ translate posX posY imagemAjustada

desenhaPortal :: FilePath -> Int -> Int -> IO Picture
desenhaPortal imagePath x y = do
  portalImagem <- loadBMP imagePath
  let imagemAjustada = scale (0.07) (0.07) portalImagem
      posX = fromIntegral x * tamanhoTerreno - fromIntegral largura / 0.981
      posY = -fromIntegral y * tamanhoTerreno + fromIntegral altura / 1.02
  return $ translate posX posY imagemAjustada

desenhaVida :: FilePath -> Int -> Int -> IO Picture
desenhaVida imagePath x y = do
  vidaImagem <- loadBMP imagePath
  let imagemAjustada = scale (1) (1) vidaImagem
      posX = fromIntegral x * tamanhoTerreno - fromIntegral largura / 3
      posY = -fromIntegral y * tamanhoTerreno + fromIntegral altura / 3.5 
  return $ translate posX posY imagemAjustada
  
desenhaMoeda :: FilePath -> Int -> Int -> IO Picture
desenhaMoeda imagePath x y = do
  moedaImagem <- loadBMP imagePath
  let imagemAjustada = scale (0.15) (0.15) moedaImagem
      posX = fromIntegral x * tamanhoTerreno - fromIntegral largura / 3.15 
      posY = -fromIntegral y * tamanhoTerreno + fromIntegral altura / 3.5 
  return $ translate posX posY imagemAjustada

desenhaTabua :: FilePath -> Int -> Int -> IO Picture
desenhaTabua imagePath x y = do
  tabuaImagem <- loadBMP imagePath
  let imagemAjustada = scale (0.3) (0.3) tabuaImagem
      posX = fromIntegral x * tamanhoTerreno - fromIntegral largura / 3  
      posY = -fromIntegral y * tamanhoTerreno + fromIntegral altura / 3.4
  return $ translate posX posY imagemAjustada

desenhaLoja :: FilePath -> FilePath -> Picture -> IO Picture
desenhaLoja imagePath1 imagePath2 mapPicture = do
  torrefogo <- loadBMP imagePath1 
  torregelo <- loadBMP imagePath2 
  let imagemAjustada1 = scale 0.3 0.3 torrefogo -- Ajusta o tamanho
  let imagemAjustada2 = scale 0.3 0.3 torregelo 
  return $ pictures 
    [mapPicture, 
     translate (-300) (-fromIntegral altura / 2) imagemAjustada1, 
     translate (200) (-fromIntegral altura / 2) imagemAjustada2] -- Ajusta a posicao
