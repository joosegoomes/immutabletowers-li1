{-|
Module      : Tarefa1
Description : Invariantes do Jogo
Copyright   : Jose dos Santos Gomes <a110367@alunos.uminho.pt>
              Guilherme Figueiredo Castro <a110452@alunos.uminho.pt>

Módulo para a realização da Tarefa 1 de LI1 em 2024/25.
-}
module Tarefa1 where

import LI12425

-- | Valida se um estado de jogo é válido, de acordo com as regras definidas.
-- 
-- Esta função verifica as regras definidas para a validade do estado do jogo:
-- 1. Relativamente a portais:
--   - (1.a) Existe pelo menos um portal.
--   - (1.b) Estão posicionados sobre terra.
--   - (1.c) Existe pelo menos um caminho de terra ligando um portal à base.
--   - (1.d) Não podem estar sobrepostos a torres ou à base.
--   - (1.e) Há no máximo uma onda ativa por portal.
--
-- A função retorna True se todos os critérios forem cumpridos, e False caso contrário.
validaJogo :: Jogo -> Bool
validaJogo Jogo{portaisJogo = [], baseJogo = Base{posicaoBase = _}} = False
validaJogo Jogo{baseJogo = Base{posicaoBase = b}, portaisJogo = portais, mapaJogo = mapa, torresJogo = torres} =
  -- Verifica se existem portais, e se todos estão válidos
  not (null portais) && all (\portal -> validaPortal portal mapa b torres) portais

-- | Valida o portal considerando as regras fornecidas.
-- 
-- - (1.b) Verifica se o portal está posicionado sobre terra.
-- - (1.c) Verifica se existe um caminho de terra entre o portal e a base.
-- - (1.d) Verifica se o portal não está sobreposto à base ou torres.
-- - (1.e) Verifica se há no máximo uma onda ativa por portal.
validaPortal :: Portal -> Mapa -> Posicao -> [Torre] -> Bool
validaPortal Portal{posicaoPortal = posPortal, ondasPortal = ondas} mapa basePos torres =
  -- Regras para o portal
  eTerra posPortal mapa &&                               -- (1.b) O portal deve estar sobre terra
  validaCaminhoTerra posPortal mapa basePos &&           -- (1.c) Deve existir um caminho de terra para a base
  not (posicaoOcupada posPortal basePos torres) &&       -- (1.d) Não pode sobrepor a base ou torres
  validaOndas (Portal{posicaoPortal = posPortal, ondasPortal = ondas}) && -- (1.e) No máximo uma onda ativa por portal
  length ondas <= 1  -- Garantir que no máximo uma onda ativa por portal

-- | Função para verificar se uma posição está ocupada pela base ou torres.
-- 
-- - (1.d) Verifica se a posição do portal está ocupada pela base ou por uma torre.
posicaoOcupada :: Posicao -> Posicao -> [Torre] -> Bool
posicaoOcupada portalPos basePos torres =
  portalPos == basePos || any (\Torre{posicaoTorre = posTorre} -> portalPos == posTorre) torres

-- | Função que verifica se existe um caminho de terra entre o portal e a base.
-- 
-- - (1.c) Verifica se existe um caminho de terra ligando o portal à base.
validaCaminhoTerra :: Posicao -> Mapa -> Posicao -> Bool
validaCaminhoTerra (x, y) mapa b =
  eTerra (x, y) mapa &&
  (caminhoTerra (x-1, y) mapa b ||
   caminhoTerra (x+1, y) mapa b ||
   caminhoTerra (x, y-1) mapa b ||
   caminhoTerra (x, y+1) mapa b)

-- | Função que valida o caminho de terra recursivamente.
-- 
-- - (1.c) Verifica se existe um caminho de terra entre o portal e a base, considerando todas as direções possíveis.
caminhoTerra :: Posicao -> Mapa -> Posicao -> Bool
caminhoTerra p@(x, y) mapa b
  | p == b  = True  -- Se chegou à base, retorna True
  | not (eTerra p mapa) = False  -- Se a posição não for Terra, retorna False
  | otherwise = caminhoTerra (x-1, y) mapa b || caminhoTerra (x+1, y) mapa b || caminhoTerra (x, y-1) mapa b || caminhoTerra (x, y+1) mapa b

-- | Verifica se uma posição específica do mapa é do tipo "Terra".
-- 
-- - (1.b) Verifica se a posição do mapa é terra.
eTerra :: Posicao -> Mapa -> Bool
eTerra (x, y) mapa =
  let x' = floor x
      y' = floor y
  in y' >= 0 && y' < length mapa && x' >= 0 && x' < length (mapa !! y') && (mapa !! y' !! x') == Terra

-- | Valida se há no máximo uma onda ativa por portal.
-- 
-- - (1.e) Verifica se o portal tem no máximo uma onda ativa.
validaOndas :: Portal -> Bool
validaOndas Portal{ondasPortal = ondas} = length ondas <= 1
