{-|
Module      : Tarefa2
Description : Auxiliares do Jogo
Copyright   : Jose dos Santos Gomes <a110367@alunos.uminho.pt>
              Guilherme Figueiredo Castro <a110452@alunos.uminho.pt>

Módulo para a realização da Tarefa 2 de LI1 em 2024/25.
-}
module Tarefa2 where

import LI12425

{-|
A função 'inimigosNoAlcance' recebe uma torre e uma lista de inimigos e retorna uma lista dos inimigos que estão ao alcance da torre.

### Exemplos:
>>> inimigosNoAlcance Torre { posicaoTorre = (0, 0), alcanceTorre = 2 } [Inimigo { posicaoInimigo = (1, 1), vidaInimigo = 10, projeteisInimigo = [], velocidadeInimigo = 1 }]
[Inimigo {posicaoInimigo = (1,1), vidaInimigo = 10, projeteisInimigo = [], velocidadeInimigo = 1}]
>>> inimigosNoAlcance Torre { posicaoTorre = (0, 0), alcanceTorre = 1 } [Inimigo { posicaoInimigo = (2, 2), vidaInimigo = 10, projeteisInimigo = [], velocidadeInimigo = 1 }]
[]
-}
inimigosNoAlcance :: Torre -> [Inimigo] -> [Inimigo]
inimigosNoAlcance torre = filter (\inimigo -> distancia (posicaoInimigo inimigo) (posicaoTorre torre) <= alcanceTorre torre)

distancia :: Posicao -> Posicao -> Float
distancia (x1, y1) (x2, y2) = sqrt ((x1 - x2) ^ (2 :: Integer) + (y1 - y2) ^ (2 :: Integer))

{-|
Aplica o dano de uma torre a um inimigo, reduzindo sua vida e atualizando os projéteis.

### Exemplos:
>>> atingeInimigo Torre { danoTorre = 5, projetilTorre = Projetil Fogo (Finita 3) } Inimigo { vidaInimigo = 10, projeteisInimigo = [], posicaoInimigo = (1,1), velocidadeInimigo = 1 }
Inimigo {posicaoInimigo = (1,1), vidaInimigo = 5, projeteisInimigo = [Projetil Fogo (Finita 3)], velocidadeInimigo = 1}
>>> atingeInimigo Torre { danoTorre = 15, projetilTorre = Projetil Fogo (Finita 3) } Inimigo { vidaInimigo = 10, projeteisInimigo = [], posicaoInimigo = (1,1), velocidadeInimigo = 1 }
Inimigo {posicaoInimigo = (1,1), vidaInimigo = 0, projeteisInimigo = [Projetil Fogo (Finita 3)], velocidadeInimigo = 1}
-}
-- Atinge um inimigo com a torre e atualiza a vida e os projéteis
atingeInimigo :: Torre -> Inimigo -> Inimigo
atingeInimigo Torre {danoTorre = dano, projetilTorre = projTorre} inimigo@Inimigo {vidaInimigo = vida, projeteisInimigo = projInimigo} =
  inimigo {vidaInimigo = max 0 (vida - dano), projeteisInimigo = atualizaProjeteis projTorre projInimigo}
  where
    -- Atualiza os projéteis que atingem o inimigo
    atualizaProjeteis :: Projetil -> [Projetil] -> [Projetil]
    atualizaProjeteis proj [] = [proj]
    atualizaProjeteis proj (p:ps)
      | cancelaMutuamente proj p = atualizaProjeteis proj ps
      | dobraDuracao proj p = proj {duracaoProjetil = dobra proj} : atualizaProjeteis proj ps
      | dobraDuracao' proj p = p {duracaoProjetil = dobra p} : atualizaProjeteis p ps
      | otherwise = p : atualizaProjeteis proj ps

    -- Função que cancela projéteis mutuamente (como Fogo e Gelo)
    cancelaMutuamente :: Projetil -> Projetil -> Bool
    cancelaMutuamente (Projetil Gelo _) (Projetil Fogo _) = True
    cancelaMutuamente (Projetil Fogo _) (Projetil Gelo _) = True
    cancelaMutuamente _ _ = False

    -- Função que dobra a duração de um projétil quando ele interage com outro
    dobraDuracao :: Projetil -> Projetil -> Bool
    dobraDuracao (Projetil Fogo _) (Projetil Resina _) = True
    dobraDuracao _ _ = False

    -- Função complementar para verificar a interação oposta
    dobraDuracao' :: Projetil -> Projetil -> Bool
    dobraDuracao' (Projetil Resina _) (Projetil Fogo _) = True
    dobraDuracao' _ _ = False

    -- Dobra a duração do projétil
    dobra :: Projetil -> Duracao
    dobra (Projetil _ (Finita t)) = Finita (2 * t)  -- Dobra o tempo se for finito
    dobra (Projetil _ Infinita) = Infinita  -- Se for infinito, mantém como Infinito

{-|
Verifica se o jogo terminou.

### Exemplos:
>>> terminouJogo Jogo { baseJogo = Base { vidaBase = 0 }, inimigosJogo = [] }
True
>>> terminouJogo Jogo { baseJogo = Base { vidaBase = 10 }, inimigosJogo = [Inimigo { posicaoInimigo = (1,1), vidaInimigo = 10, projeteisInimigo = [], velocidadeInimigo = 1 }] }
False
-}
terminouJogo :: Jogo -> Bool
terminouJogo jogo = ganhouJogo jogo || perdeuJogo jogo

{-|
Verifica se o jogador ganhou o jogo (condição de vitória).

### Exemplos:
>>> ganhouJogo Jogo { baseJogo = Base { vidaBase = 10 }, inimigosJogo = [] }
True
>>> ganhouJogo Jogo { baseJogo = Base { vidaBase = 10 }, inimigosJogo = [Inimigo { posicaoInimigo = (1,1), vidaInimigo = 10, projeteisInimigo = [], velocidadeInimigo = 1 }] }
False
-}
ganhouJogo :: Jogo -> Bool
ganhouJogo Jogo {baseJogo = Base {vidaBase = vida}, inimigosJogo = inimigos} = vida > 0 && null inimigos

{-|
Verifica se o jogador perdeu o jogo (condição de derrota).

### Exemplos:
>>> perdeuJogo Jogo { baseJogo = Base { vidaBase = 0 } }
True
>>> perdeuJogo Jogo { baseJogo = Base { vidaBase = 10 } }
False
-}
perdeuJogo :: Jogo -> Bool
perdeuJogo Jogo {baseJogo = Base {vidaBase = vida}} = vida <= 0

{-|
Ativa o próximo inimigo na onda de um portal.

### Exemplos:
>>> ativaInimigo Portal { ondasPortal = [Onda { inimigosOnda = [Inimigo { posicaoInimigo = (1,1), vidaInimigo = 10, projeteisInimigo = [], velocidadeInimigo = 1 }] }] } Jogo { inimigosJogo = [], portaisJogo = [] }
Jogo {inimigosJogo = [Inimigo {posicaoInimigo = (1,1), vidaInimigo = 10, projeteisInimigo = [], velocidadeInimigo = 1}], portaisJogo = [Portal {ondasPortal = [Onda {inimigosOnda = []}]}]}
>>> ativaInimigo Portal { ondasPortal = [] } Jogo { inimigosJogo = [], portaisJogo = [] }
Jogo {inimigosJogo = [], portaisJogo = []}
-}
ativaInimigo :: Portal -> Jogo -> Jogo 
ativaInimigo Portal {ondasPortal = []} jogo = jogo
ativaInimigo portal@Portal {ondasPortal = (onda:ondas)} jogo = 
  jogo { inimigosJogo = inimigosJogo jogo ++ novosInimigos, portaisJogo = atualizaPortal portal } 
  where 
    novosInimigos = take 1 (inimigosOnda onda) -- Ativa apenas o próximo inimigo 
    ondaAtualizada = onda {inimigosOnda = drop 1 (inimigosOnda onda)} 
    atualizaPortal p = [p {ondasPortal = ondaAtualizada : ondas}]
