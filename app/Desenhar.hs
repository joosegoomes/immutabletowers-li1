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

coordenadas :: Mapa -> Posicao -> [(Terreno,Posicao)]
coordenadas [] _ = []
coordenadas ([]:linhas) (x,y) = coordenadas linhas (0,y+1)
coordenadas ((terreno:terrenos):linhas) (x,y) = (terreno,(x,y)) : coordenadas (terrenos:linhas) (x+1,y) 

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

desenhaMapa :: Mapa -> Picture
desenhaMapa mapa =
  pictures [translate (fromIntegral x * tamanhoTerreno) (fromIntegral (-y) * tamanhoTerreno)(color (corDoTerreno terreno) (rectangleSolid tamanhoTerreno tamanhoTerreno))
           | (y, linha) <- zip [0..] mapa, (x, terreno) <- zip [0..] linha]

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

desenhaTorre :: [Picture] -> Torre -> Picture
desenhaTorre [torreFogo,torreGelo,torreResina] torre =
  let (x, y) = posicaoTorre torre
      imagemTorre = case tipoProjetil (projetilTorre torre) of
        Fogo   -> torreFogo
        Gelo   -> torreGelo
        Resina -> torreResina
  in translate (x * tamanhoTerreno) (-y * tamanhoTerreno) $ scale 0.1 0.1 imagemTorre -- Ajuste o tamanho conforme necessário

desenhaTorres :: [Torre] -> [Picture] -> Picture
desenhaTorres torres [torreFogo,torreGelo,torreResina] = Pictures $ map (desenhaTorre [torreFogo,torreGelo,torreResina]) torres 

desenhaVida :: FilePath -> Int -> Int -> IO Picture
desenhaVida ficheiroImagem x y = do
  vidaImagem <- loadBMP ficheiroImagem
  let imagemAjustada = scale (1.5) (1.5) vidaImagem
      posX = fromIntegral x * tamanhoTerreno - fromIntegral largura / 3
      posY = -fromIntegral y * tamanhoTerreno + fromIntegral altura / 3.5 
  return $ translate posX posY imagemAjustada

escreveVida :: Base -> Picture
escreveVida base = translate 675 400 $ scale 0.25 0.25 $ color black $ text $ show (round (vidaBase base))

escreveCreditos :: Base -> Picture
escreveCreditos base = translate 745 190 $ scale 0.3 0.3 $ color yellow $ text $ show (creditosBase base)

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

desenhaLoja :: FilePath -> FilePath -> FilePath -> IO Picture
desenhaLoja ficheiroImagem1 ficheiroImagem2 ficheiroImagem3 = do
  torreFogo <- loadBMP ficheiroImagem1 
  torreGelo <- loadBMP ficheiroImagem2 
  torreResina <- loadBMP ficheiroImagem3
  let imagemAjustada1 = scale 0.31 0.31 torreFogo -- Ajusta o tamanho
  let imagemAjustada2 = scale 0.3 0.3 torreGelo 
  let imagemAjustada3 = scale 0.28 0.28 torreResina
  return $ pictures  
     [translate (-250) (-fromIntegral altura / 2) imagemAjustada1, 
     translate (200) (-fromIntegral altura / 2) imagemAjustada2,
     translate (-700) (-fromIntegral altura / 2) imagemAjustada3] -- Ajusta a posicao

{- desenhaInimigo :: Picture -> Inimigo -> Picture
desenhaInimigo inimigoBMP inimigo = translate (x * tamanhoTerreno - tamanhoTerreno/2) (-y * tamanhoTerreno + tamanhoTerreno/2) $ scale 0.4 0.4 inimigoBMP -- Ajuste o tamanho conforme necessário
                                  where (x, y) = posicaoInimigo inimigo -}
desenhaInimigo :: Inimigo -> Picture
desenhaInimigo inimigo = translate (x * tamanhoTerreno) (-y * tamanhoTerreno) $ color red $ rectangleSolid tamanhoTerreno tamanhoTerreno
  where (x, y) = posicaoInimigo inimigo

desenhaInimigos :: [Inimigo] -> Picture
desenhaInimigos inimigos = Pictures $ map desenhaInimigo inimigos

{- desenhaInimigos :: [Inimigo] -> Picture -> Picture
desenhaInimigos inimigos inimigoBMP = Pictures $ map (desenhaInimigo inimigoBMP) inimigos -}