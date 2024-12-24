{-|
Module      : Tarefa1
Description : Invariantes do Jogo
Copyright   : Jose dos Santos Gomes <a110367@alunos.uminho.pt>
              Guilherme Figueiredo Castro <a110452@alunos.uminho.pt>

Módulo para a realização da Tarefa 1 de LI1 em 2024/25.
-}
module Tarefa1 where

import LI12425

-- Valida se um estado de jogo é válido
validaJogo :: Jogo -> Bool
validaJogo Jogo{portaisJogo = [], baseJogo = Base{posicaoBase = _}} = False
validaJogo jogo@(Jogo{baseJogo = base, portaisJogo = portais, mapaJogo = mapa, torresJogo = torres}) =
  -- Verifica se existem portais, e se todos estão válidos
  not (null portais) && all (\portal -> validaPortal portal mapa (posicaoBase base) torres) portais &&
  -- Verifica se todos os inimigos por lançar e em jogo são válidos
  validaInimigosPorLancar (inimigosJogo jogo) portais &&
  validaInimigosEmJogo (inimigosJogo jogo) mapa (posicaoBase base) torres &&
  -- Verifica se todas as torres são válidas
  all (validaTorre mapa torres) torres &&
  -- Verifica se a base é válida
  validaBase base mapa torres portais

-- Valida o portal considerando as regras fornecidas
validaPortal :: Portal -> Mapa -> Posicao -> [Torre] -> Bool
validaPortal Portal{posicaoPortal = posPortal, ondasPortal = ondas} mapa basePos torres =
  eTerra posPortal mapa &&                              -- (1.b)
  validaCaminhoTerra posPortal mapa basePos &&           -- (1.c)
  not (posicaoOcupada posPortal basePos torres) &&      -- (1.d)
  validaOndas ondas &&  -- (1.e)
  length ondas <= 1  -- Máximo uma onda ativa por portal

-- Verifica se a posição está ocupada pela base ou torres
posicaoOcupada :: Posicao -> Posicao -> [Torre] -> Bool
posicaoOcupada portalPos basePos torres =
  portalPos == basePos || any (\Torre{posicaoTorre = posTorre} -> portalPos == posTorre) torres

-- Valida se há um caminho de terra entre o portal e a base
validaCaminhoTerra :: Posicao -> Mapa -> Posicao -> Bool
validaCaminhoTerra (x, y) mapa b =
  eTerra (x, y) mapa &&
  (caminhoTerra (x-1, y) mapa b ||
   caminhoTerra (x+1, y) mapa b ||
   caminhoTerra (x, y-1) mapa b ||
   caminhoTerra (x, y+1) mapa b)

-- Valida o caminho de terra recursivamente
caminhoTerra :: Posicao -> Mapa -> Posicao -> Bool
caminhoTerra p@(x, y) mapa b
  | p == b  = True
  | not (eTerra p mapa) = False
  | otherwise = caminhoTerra (x-1, y) mapa b || caminhoTerra (x+1, y) mapa b || caminhoTerra (x, y-1) mapa b || caminhoTerra (x, y+1) mapa b

-- Verifica se a posição do mapa é "Terra"
eTerra :: Posicao -> Mapa -> Bool
eTerra (x, y) mapa =
  let x' = floor x
      y' = floor y
  in y' >= 0 && y' < length mapa && x' >= 0 && x' < length (mapa !! y') && (mapa !! y' !! x') == Terra

-- Verifica se a posição do mapa é "Relva"
eRelva :: Posicao -> Mapa -> Bool
eRelva (x, y) mapa =
  let x' = floor x
      y' = floor y
  in y' >= 0 && y' < length mapa && x' >= 0 && x' < length (mapa !! y') && (mapa !! y' !! x') == Relva

-- Valida se a lista de projéteis ativos está "normalizada"
validaProjeteisAtivos :: [Projetil] -> Bool
validaProjeteisAtivos projeteis =
  not (temDuplicados projeteis) &&
  not (contémFogoEResina projeteis || contémFogoEGelo projeteis)
  where
    temDuplicados :: [Projetil] -> Bool
    temDuplicados [] = False
    temDuplicados (x:xs) = any (\p -> tipoProjetil p == tipoProjetil x) xs || temDuplicados xs

    contémFogoEResina :: [Projetil] -> Bool
    contémFogoEResina ps = temTipo Fogo ps && temTipo Resina ps

    contémFogoEGelo :: [Projetil] -> Bool
    contémFogoEGelo ps = temTipo Fogo ps && temTipo Gelo ps

    temTipo :: TipoProjetil -> [Projetil] -> Bool
    temTipo t = any (\p -> tipoProjetil p == t)

-- Valida se todos os inimigos por lançar cumprem os critérios
validaInimigosPorLancar :: [Inimigo] -> [Portal] -> Bool
validaInimigosPorLancar inimigos portais = all (`validaInimigoPorLancar` portais) inimigos

-- Valida se um inimigo por lançar cumpre os critérios
validaInimigoPorLancar :: Inimigo -> [Portal] -> Bool
validaInimigoPorLancar Inimigo{posicaoInimigo = pos, vidaInimigo = vida, projeteisInimigo = projeteis, velocidadeInimigo = vel} portais =
  any (\Portal{posicaoPortal = posPortal} -> pos == posPortal) portais &&
  vida > 0 && 
  null projeteis &&
  vel >= 0 &&
  validaProjeteisAtivos projeteis

-- Valida se todos os inimigos em jogo cumprem os critérios
validaInimigosEmJogo :: [Inimigo] -> Mapa -> Posicao -> [Torre] -> Bool
validaInimigosEmJogo inimigos mapa basePos torres = all (\inimigo -> validaInimigoEmJogo inimigo mapa basePos torres) inimigos

-- Valida se um inimigo em jogo cumpre os critérios
validaInimigoEmJogo :: Inimigo -> Mapa -> Posicao -> [Torre] -> Bool
validaInimigoEmJogo Inimigo{posicaoInimigo = pos, velocidadeInimigo = vel, projeteisInimigo = projeteis} mapa basePos torres =
  eTerra pos mapa && 
  not (posicaoOcupada pos basePos torres) && 
  vel >= 0 &&
  validaProjeteisAtivos projeteis

-- Valida se uma torre cumpre os critérios
validaTorre :: Mapa -> [Torre] -> Torre -> Bool
validaTorre mapa torres Torre{posicaoTorre = pos, alcanceTorre = alcance, rajadaTorre = rajada, cicloTorre = ciclo} =
  eRelva pos mapa &&  -- (3.a)
  alcance > 0 &&      -- (3.b)
  rajada > 0 &&       -- (3.c)
  ciclo >= 0 &&       -- (3.d)
  not (torreSobreposta pos torres)  -- (3.e)

-- Verifica se uma torre está sobreposta a outra
torreSobreposta :: Posicao -> [Torre] -> Bool
torreSobreposta pos torres = any (\Torre{posicaoTorre = posTorre} -> pos == posTorre) torres

-- Valida se a base cumpre os critérios
validaBase :: Base -> Mapa -> [Torre] -> [Portal] -> Bool
validaBase Base{posicaoBase = pos, creditosBase = credito} mapa torres portais =
  eTerra pos mapa &&  -- (4.a)
  credito >= 0 &&     -- (4.b)
  not (posicaoOcupada pos pos torres) &&  -- (4.c)
  not (any (\Portal{posicaoPortal = posPortal} -> pos == posPortal) portais)

-- Valida se as ondas de um portal são válidas
validaOndas :: [Onda] -> Bool
validaOndas = all validaOnda

-- Valida se uma onda é válida
validaOnda :: Onda -> Bool
validaOnda Onda{inimigosOnda = inimigos} = not (null inimigos)

--tarefa1 concluída--