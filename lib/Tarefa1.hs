{-|
Module      : Tarefa1
Description : Invariantes do Jogo
Copyright   : Jose dos Santos Gomes <a110367@alunos.uminho.pt>
              Guilherme Figueiredo Castro <a110452@alunos.uminho.pt>

Módulo para a realização da Tarefa 1 de LI1 em 2024/25.
-}
module Tarefa1 where

import LI12425

{-|
Valida se um estado de jogo é válido.

### Exemplos:
>>> validaJogo Jogo { portaisJogo = [], baseJogo = Base { posicaoBase = (0,0), creditosBase = 0 }, mapaJogo = [], torresJogo = [], inimigosJogo = [] }
False
>>> validaJogo Jogo { portaisJogo = [Portal {posicaoPortal = (1,1), ondasPortal = []}], baseJogo = Base { posicaoBase = (0,0), creditosBase = 10 }, mapaJogo = [[Terra]], torresJogo = [], inimigosJogo = [] }
True
-}
validaJogo :: Jogo -> Bool
validaJogo Jogo{portaisJogo = [], baseJogo = Base{posicaoBase = _}} = False
validaJogo jogo@(Jogo{baseJogo = base, portaisJogo = portais, mapaJogo = mapa, torresJogo = torres}) =
  not (null portais) && all (\portal -> validaPortal portal mapa (posicaoBase base) torres) portais &&
  validaInimigosPorLancar (inimigosJogo jogo) portais &&
  validaInimigosEmJogo (inimigosJogo jogo) mapa (posicaoBase base) torres &&
  all (validaTorre mapa torres) torres &&
  validaBase base mapa torres portais

{-|
Valida o portal considerando as regras fornecidas.

### Exemplos:
>>> validaPortal Portal { posicaoPortal = (1,1), ondasPortal = [] } [[Terra]] (0,0) []
True
>>> validaPortal Portal { posicaoPortal = (1,1), ondasPortal = [] } [[Relva]] (0,0) []
False
-}
validaPortal :: Portal -> Mapa -> Posicao -> [Torre] -> Bool
validaPortal Portal{posicaoPortal = posPortal, ondasPortal = ondas} mapa basePos torres =
  eTerra posPortal mapa &&
  validaCaminhoTerra posPortal mapa basePos &&
  not (posicaoOcupada posPortal basePos torres) &&
  validaOndas ondas &&
  length ondas <= 1

{-|
Verifica se a posição está ocupada pela base ou torres.

### Exemplos:
>>> posicaoOcupada (0,0) (0,0) []
True
>>> posicaoOcupada (1,1) (0,0) [Torre { posicaoTorre = (1,1), alcanceTorre = 2, rajadaTorre = 1, cicloTorre = 3 }]
True
-}
posicaoOcupada :: Posicao -> Posicao -> [Torre] -> Bool
posicaoOcupada portalPos basePos torres =
  portalPos == basePos || any (\Torre{posicaoTorre = posTorre} -> portalPos == posTorre) torres

{-|
Valida se há um caminho de terra entre o portal e a base.

### Exemplos:
>>> validaCaminhoTerra (0,0) [[Terra]] (0,0)
True
>>> validaCaminhoTerra (0,1) [[Relva]] (0,0)
False
-}
validaCaminhoTerra :: Posicao -> Mapa -> Posicao -> Bool
validaCaminhoTerra (x, y) mapa b =
  eTerra (x, y) mapa &&
  (caminhoTerra (x-1, y) mapa b ||
   caminhoTerra (x+1, y) mapa b ||
   caminhoTerra (x, y-1) mapa b ||
   caminhoTerra (x, y+1) mapa b)

{-|
Valida o caminho de terra recursivamente.

### Exemplos:
>>> caminhoTerra (0,0) [[Terra]] (0,0)
True
>>> caminhoTerra (1,1) [[Terra, Relva], [Relva, Relva]] (0,0)
False
-}
caminhoTerra :: Posicao -> Mapa -> Posicao -> Bool
caminhoTerra p@(x, y) mapa b
  | p == b  = True
  | not (eTerra p mapa) = False
  | otherwise = caminhoTerra (x-1, y) mapa b || caminhoTerra (x+1, y) mapa b || caminhoTerra (x, y-1) mapa b || caminhoTerra (x, y+1) mapa b

{-|
Verifica se a posição do mapa é "Terra".

### Exemplos:
>>> eTerra (0,0) [[Terra]]
True
>>> eTerra (1,1) [[Terra, Relva], [Relva, Relva]]
False
-}
eTerra :: Posicao -> Mapa -> Bool
eTerra (x, y) mapa =
  let x' = floor x
      y' = floor y
  in y' >= 0 && y' < length mapa && x' >= 0 && x' < length (mapa !! y') && (mapa !! y' !! x') == Terra

{-|
Verifica se a posição do mapa é "Relva".

### Exemplos:
>>> eRelva (0,0) [[Relva]]
True
>>> eRelva (1,1) [[Relva, Terra], [Terra, Terra]]
False
-}
eRelva :: Posicao -> Mapa -> Bool
eRelva (x, y) mapa =
  let x' = floor x
      y' = floor y
  in y' >= 0 && y' < length mapa && x' >= 0 && x' < length (mapa !! y') && (mapa !! y' !! x') == Relva

{-|
Valida se a lista de projéteis ativos está "normalizada".

### Exemplos:
>>> validaProjeteisAtivos [Projetil { tipoProjetil = Fogo }]
True
>>> validaProjeteisAtivos [Projetil { tipoProjetil = Fogo }, Projetil { tipoProjetil = Fogo }]
False
-}
validaProjeteisAtivos :: [Projetil] -> Bool
validaProjeteisAtivos projeteis =
  not (temDuplicados projeteis) &&
  not (contemFogoEResina projeteis || contemFogoEGelo projeteis)
  where
    temDuplicados :: [Projetil] -> Bool
    temDuplicados [] = False
    temDuplicados (x:xs) = any (\p -> tipoProjetil p == tipoProjetil x) xs || temDuplicados xs

    contemFogoEResina :: [Projetil] -> Bool
    contemFogoEResina ps = temTipo Fogo ps && temTipo Resina ps

    contemFogoEGelo :: [Projetil] -> Bool
    contemFogoEGelo ps = temTipo Fogo ps && temTipo Gelo ps

    temTipo :: TipoProjetil -> [Projetil] -> Bool
    temTipo t = any (\p -> tipoProjetil p == t)

{-|
Valida se todos os inimigos por lançar cumprem os critérios.

### Exemplos:
>>> validaInimigosPorLancar [] []
True
>>> validaInimigosPorLancar [Inimigo { posicaoInimigo = (1,1), vidaInimigo = 10, projeteisInimigo = [], velocidadeInimigo = 1 }] [Portal { posicaoPortal = (1,1), ondasPortal = [] }]
True
-}
validaInimigosPorLancar :: [Inimigo] -> [Portal] -> Bool
validaInimigosPorLancar inimigos portais = all (\inimigo -> validaInimigoPorLancar inimigo portais) inimigos

{-|
Valida se um inimigo por lançar cumpre os critérios.

### Exemplos:
>>> validaInimigoPorLancar Inimigo { posicaoInimigo = (1,1), vidaInimigo = 10, projeteisInimigo = [], velocidadeInimigo = 1 } [Portal { posicaoPortal = (1,1), ondasPortal = [] }]
True
>>> validaInimigoPorLancar Inimigo { posicaoInimigo = (1,2), vidaInimigo = 10, projeteisInimigo = [], velocidadeInimigo = 1 } [Portal { posicaoPortal = (1,1), ondasPortal = [] }]
False
-}
validaInimigoPorLancar :: Inimigo -> [Portal] -> Bool
validaInimigoPorLancar Inimigo{posicaoInimigo = pos, vidaInimigo = vida, projeteisInimigo = projeteis, velocidadeInimigo = vel} portais =
  any (\Portal{posicaoPortal = posPortal} -> pos == posPortal) portais &&
  vida > 0 && 
  null projeteis &&
  vel >= 0 &&
  validaProjeteisAtivos projeteis

{-|
Valida se todos os inimigos em jogo cumprem os critérios.

### Exemplos:
>>> validaInimigosEmJogo [] [[Terra]] (0,0) []
True
>>> validaInimigosEmJogo [Inimigo { posicaoInimigo = (1,1), vidaInimigo = 10, projeteisInimigo = [], velocidadeInimigo = 1 }] [[Terra]] (0,0) []
True
-}

validaInimigosEmJogo :: [Inimigo] -> Mapa -> Posicao -> [Torre] -> Bool
validaInimigosEmJogo inimigos mapa basePos torres = all (\inimigo -> validaInimigoEmJogo inimigo mapa basePos torres) inimigos

{-|
Valida se um inimigo em jogo cumpre os critérios.

### Exemplos:
>>> validaInimigoEmJogo Inimigo { posicaoInimigo = (1,1), vidaInimigo = 10, projeteisInimigo = [], velocidadeInimigo = 1 } [[Terra]] (0,0) []
True
>>> validaInimigoEmJogo Inimigo { posicaoInimigo = (1,1), vidaInimigo = 10, projeteisInimigo = [], velocidadeInimigo = -1 } [[Terra]] (0,0) []
False
-}
validaInimigoEmJogo :: Inimigo -> Mapa -> Posicao -> [Torre] -> Bool
validaInimigoEmJogo Inimigo{posicaoInimigo = pos, velocidadeInimigo = vel, projeteisInimigo = projeteis} mapa basePos torres =
  eTerra pos mapa && 
  not (posicaoOcupada pos basePos torres) && 
  vel >= 0 &&
  validaProjeteisAtivos projeteis

{-|
Valida se uma torre cumpre os critérios.

### Exemplos:
>>> validaTorre [[Relva]] [] Torre { posicaoTorre = (0,0), alcanceTorre = 1, rajadaTorre = 1, cicloTorre = 2 }
True
>>> validaTorre [[Terra]] [] Torre { posicaoTorre = (0,0), alcanceTorre = 1, rajadaTorre = 1, cicloTorre = 2 }
False
-}
validaTorre :: Mapa -> [Torre] -> Torre -> Bool
validaTorre mapa torres Torre{posicaoTorre = pos, alcanceTorre = alcance, rajadaTorre = rajada, cicloTorre = ciclo} =
  eRelva pos mapa &&  -- (3.a)
  alcance > 0 &&      -- (3.b)
  rajada > 0 &&       -- (3.c)
  ciclo >= 0 &&       -- (3.d)
  not (torreSobreposta pos torres)  -- (3.e)


{-|
Verifica se uma torre está sobreposta a outra.

### Exemplos:
>>> torreSobreposta (0,0) [Torre { posicaoTorre = (0,0), alcanceTorre = 2, rajadaTorre = 1, cicloTorre = 3 }]
True
>>> torreSobreposta (1,1) [Torre { posicaoTorre = (0,0), alcanceTorre = 2, rajadaTorre = 1, cicloTorre = 3 }]
False
-}
torreSobreposta :: Posicao -> [Torre] -> Bool
torreSobreposta pos torres = any (\Torre{posicaoTorre = posTorre} -> pos == posTorre) torres


{-|
Valida se a base cumpre os critérios.

### Exemplos:
>>> validaBase Base { posicaoBase = (0,0), creditosBase = 10 } [[Terra]] [] []
True
>>> validaBase Base { posicaoBase = (0,0), creditosBase = -1 } [[Terra]] [] []
False
-}

validaBase :: Base -> Mapa -> [Torre] -> [Portal] -> Bool
validaBase Base{posicaoBase = pos, creditosBase = credito} mapa torres portais =
  eTerra pos mapa &&  -- (4.a)
  credito >= 0 &&     -- (4.b)
  not (posicaoOcupada pos pos torres) &&  -- (4.c)
  not (any (\Portal{posicaoPortal = posPortal} -> pos == posPortal) portais)


{-|
Valida se as ondas de um portal são válidas.

### Exemplos:
>>> validaOndas [Onda { inimigosOnda = [Inimigo { posicaoInimigo = (1,1), vidaInimigo = 10, projeteisInimigo = [], velocidadeInimigo = 1 }] }]
True
>>> validaOndas [Onda { inimigosOnda = [] }]
False
-}

validaOndas :: [Onda] -> Bool
validaOndas = all validaOnda

 {-|
Valida se uma onda é válida.

### Exemplos:
>>> validaOnda Onda { inimigosOnda = [Inimigo { posicaoInimigo = (1,1), vidaInimigo = 10, projeteisInimigo = [], velocidadeInimigo = 1 }] }
True
>>> validaOnda Onda { inimigosOnda = [] }
False
-}

validaOnda :: Onda -> Bool
validaOnda Onda{inimigosOnda = inimigos} = not (null inimigos)

  --tarefa1 concluída--

