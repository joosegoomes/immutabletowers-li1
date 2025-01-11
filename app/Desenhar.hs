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

qualquercoisa :: Mapa -> Posicao -> [(Terreno,Posicao)]
qualquercoisa [] _ = []
qualquercoisa ([]:xs) (x,y) = qualquercoisa xs (0,y+1)
qualquercoisa ((h:hs):ts) (x,y) = (h,(x,y)) : qualquercoisa (hs:ts) (x+1,y) 

-- | Ajusta o tamanho de cada Terreno de acordo com o mapa
tamanhoTerreno :: Float
tamanhoTerreno = min (fromIntegral largura / fromIntegral (length (head mapa))) (fromIntegral altura / fromIntegral (length mapa)) * 1.1 

-- | Cria cores para os diferentes tipos de Terreno
corDoTerreno :: Terreno -> Color
corDoTerreno Terra = makeColorI 139 69 19 255 -- Cor da Terra
corDoTerreno Relva = makeColorI 34 139 34 255 -- Cor da Relva
corDoTerreno Agua  = makeColorI 70 130 180 255 -- Cor da Agua

-- | Desenha um unico terreno
desenhaterreno :: (Terreno,Posicao) -> Picture
desenhaterreno (terreno, (x,y)) = translate (x * tamanhoTerreno) (-y * tamanhoTerreno) $ color (corDoTerreno terreno) $ rectangleSolid tamanhoTerreno tamanhoTerreno

-- | Desenha o Mapa inteiro
desenhaMapa :: Mapa -> Picture
desenhaMapa mapa = pictures (map desenhaterreno (qualquercoisa mapa (0,0)))

desenhaBase :: FilePath -> Int -> Int -> IO Picture
desenhaBase ficheiroImagem x y = do
  baseImagem <- loadBMP ficheiroImagem
  let imagemAjustada = scale (0.15) (0.15) baseImagem
      posX = fromIntegral x * tamanhoTerreno 
      posY = -fromIntegral y * tamanhoTerreno 
  return $ translate posX (posY) imagemAjustada

desenhaPortal :: FilePath -> Int -> Int -> IO Picture
desenhaPortal ficheiroImagem x y = do
  portalImagem <- loadBMP ficheiroImagem
  let imagemAjustada = scale (0.07) (0.07) portalImagem
      posX = fromIntegral x * tamanhoTerreno 
      posY = -fromIntegral y * tamanhoTerreno
  return $ translate posX posY imagemAjustada

desenhaTorre :: FilePath -> Int -> Int -> IO Picture
desenhaTorre ficheiroImagem x y = do
  torreImagem <- loadBMP ficheiroImagem
  let imagemAjustada = scale (0.15) (0.15) torreImagem
      posX = fromIntegral x * tamanhoTerreno 
      posY = -fromIntegral y * tamanhoTerreno 
  return $ translate posX posY imagemAjustada

desenhaVida :: FilePath -> Int -> Int -> IO Picture
desenhaVida ficheiroImagem x y = do
  vidaImagem <- loadBMP ficheiroImagem
  let imagemAjustada = scale (1) (1) vidaImagem
      posX = fromIntegral x * tamanhoTerreno - fromIntegral largura / 3
      posY = -fromIntegral y * tamanhoTerreno + fromIntegral altura / 3.5 
  return $ translate posX posY imagemAjustada

escreveVida :: Base -> Picture
escreveVida base = translate 675 404 $ scale 0.2 0.2 $ color black $ text $ show (vidaBase base)

escreveCreditos :: Base -> Picture
escreveCreditos base = translate 760 190 $ scale 0.2 0.2 $ color yellow $ text $ show (creditosBase base)

desenhaMoeda :: FilePath -> Int -> Int -> IO Picture
desenhaMoeda ficheiroImagem x y = do
  moedaImagem <- loadBMP ficheiroImagem
  let imagemAjustada = scale (0.15) (0.15) moedaImagem
      posX = fromIntegral x * tamanhoTerreno - fromIntegral largura / 3.15 
      posY = -fromIntegral y * tamanhoTerreno + fromIntegral altura / 3.5 
  return $ translate posX posY imagemAjustada

desenhaTabua :: FilePath -> Int -> Int -> IO Picture
desenhaTabua ficheiroImagem x y = do
  tabuaImagem <- loadBMP ficheiroImagem
  let imagemAjustada = scale (0.3) (0.3) tabuaImagem
      posX = fromIntegral x * tamanhoTerreno - fromIntegral largura / 3  
      posY = -fromIntegral y * tamanhoTerreno + fromIntegral altura / 3.4
  return $ translate posX posY imagemAjustada

desenhaLoja :: FilePath -> FilePath -> Picture -> IO Picture
desenhaLoja ficheiroImagem1 ficheiroImagem2 mapPicture = do
  torrefogo <- loadBMP ficheiroImagem1 
  torregelo <- loadBMP ficheiroImagem2 
  let imagemAjustada1 = scale 0.3 0.3 torrefogo -- Ajusta o tamanho
  let imagemAjustada2 = scale 0.3 0.3 torregelo 
  return $ pictures 
    [mapPicture, 
     translate (-300) (-fromIntegral altura / 2) imagemAjustada1, 
     translate (200) (-fromIntegral altura / 2) imagemAjustada2] -- Ajusta a posicao

desenhaInimigo :: Inimigo -> Picture 
desenhaInimigo inimigo = translate (x * tamanhoTerreno) (-y * tamanhoTerreno) $ color red $ circleSolid 10
  where
    (x, y) = posicaoInimigo inimigo
