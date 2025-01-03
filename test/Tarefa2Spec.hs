module Tarefa2Spec (testesTarefa2) where

import Test.HUnit
import Tarefa2
import LI12425

-- Teste 1: inimigosNoAlcance com inimigos dentro e fora do alcance
testeInimigosNoAlcance :: Test
testeInimigosNoAlcance = TestCase $ do
  let torre = Torre {posicaoTorre = (2, 2), danoTorre = 10, alcanceTorre = 3, rajadaTorre = 1, cicloTorre = 1, tempoTorre = 0, projetilTorre = Projetil Fogo Infinita}
      inimigo1 = Inimigo {posicaoInimigo = (3, 3), direcaoInimigo = Norte, vidaInimigo = 100, velocidadeInimigo = 1, ataqueInimigo = 10, butimInimigo = 10, projeteisInimigo = []}
      inimigo2 = Inimigo {posicaoInimigo = (10, 10), direcaoInimigo = Este, vidaInimigo = 100, velocidadeInimigo = 1, ataqueInimigo = 10, butimInimigo = 10, projeteisInimigo = []}
      resultado = inimigosNoAlcance torre [inimigo1, inimigo2]
  assertBool "Deve conter apenas inimigo1 no alcance" (verificaListaInimigos resultado [inimigo1])

-- Teste 2: atingeInimigo com dano e projéteis aplicados
testeAtingeInimigo :: Test
testeAtingeInimigo = TestCase $ do
  let torre = Torre {posicaoTorre = (0, 0), danoTorre = 20, alcanceTorre = 5, rajadaTorre = 1, cicloTorre = 1, tempoTorre = 0, projetilTorre = Projetil Fogo (Finita 3)}
      inimigo = Inimigo {posicaoInimigo = (1, 1), direcaoInimigo = Norte, vidaInimigo = 50, velocidadeInimigo = 1, ataqueInimigo = 10, butimInimigo = 10, projeteisInimigo = []}
      resultado = atingeInimigo torre inimigo
  assertBool "Inimigo deve perder vida e ter projétil aplicado"
    (vidaInimigo resultado == 30 &&
     verificaProjetil (head $ projeteisInimigo resultado) (Projetil Fogo (Finita 3)))

-- Teste 3: ganhouJogo verifica vitória corretamente
testeGanhouJogo :: Test
testeGanhouJogo = TestCase $ do
  let jogo = Jogo {baseJogo = Base {vidaBase = 100, posicaoBase = (1, 1), creditosBase = 50}, portaisJogo = [], torresJogo = [], mapaJogo = [[Terra]], inimigosJogo = [], lojaJogo = []}
  assertBool "Jogo deve ser ganho quando não há inimigos e base tem vida" (ganhouJogo jogo)

-- Teste 4: perdeuJogo verifica derrota corretamente
testePerdeuJogo :: Test
testePerdeuJogo = TestCase $ do
  let jogo = Jogo {baseJogo = Base {vidaBase = 0, posicaoBase = (1, 1), creditosBase = 50}, portaisJogo = [], torresJogo = [], mapaJogo = [[Terra]], inimigosJogo = [], lojaJogo = []}
  assertBool "Jogo deve ser perdido quando a vida da base é zero" (perdeuJogo jogo)

-- Teste 5: ativaInimigo ativa o próximo inimigo da onda
testeAtivaInimigo :: Test
testeAtivaInimigo = TestCase $ do
  let inimigo1 = Inimigo {posicaoInimigo = (0, 0), direcaoInimigo = Norte, vidaInimigo = 100, velocidadeInimigo = 1, ataqueInimigo = 10, butimInimigo = 10, projeteisInimigo = []}
      onda = Onda {inimigosOnda = [inimigo1], cicloOnda = 1, tempoOnda = 0, entradaOnda = 0}
      portal = Portal {posicaoPortal = (0, 0), ondasPortal = [onda]}
      jogo = Jogo {baseJogo = Base {vidaBase = 100, posicaoBase = (1, 1), creditosBase = 50}, portaisJogo = [portal], torresJogo = [], mapaJogo = [[Terra]], inimigosJogo = [], lojaJogo = []}
      resultado = ativaInimigo portal jogo
  assertBool "Deve ativar o próximo inimigo da onda e removê-lo da lista"
    (not (null (inimigosJogo resultado)) && null (inimigosOnda $ head $ ondasPortal $ head $ portaisJogo resultado))

-- Funções auxiliares
verificaListaInimigos :: [Inimigo] -> [Inimigo] -> Bool
verificaListaInimigos [] [] = True
verificaListaInimigos (x:xs) (y:ys) = verificaInimigo x y && verificaListaInimigos xs ys
verificaListaInimigos _ _ = False

verificaInimigo :: Inimigo -> Inimigo -> Bool
verificaInimigo i1 i2 =
  posicaoInimigo i1 == posicaoInimigo i2 &&
  direcaoInimigo i1 == direcaoInimigo i2 &&
  vidaInimigo i1 == vidaInimigo i2 &&
  velocidadeInimigo i1 == velocidadeInimigo i2 &&
  ataqueInimigo i1 == ataqueInimigo i2 &&
  butimInimigo i1 == butimInimigo i2 &&
  verificaListaProjetil (projeteisInimigo i1) (projeteisInimigo i2)

verificaListaProjetil :: [Projetil] -> [Projetil] -> Bool
verificaListaProjetil [] [] = True
verificaListaProjetil (p1:ps1) (p2:ps2) = verificaProjetil p1 p2 && verificaListaProjetil ps1 ps2
verificaListaProjetil _ _ = False

verificaProjetil :: Projetil -> Projetil -> Bool
verificaProjetil p1 p2 =
  tipoProjetil p1 == tipoProjetil p2 &&
  duracaoProjetil p1 == duracaoProjetil p2

-- Agrupando todos os testes
testesTarefa2 :: Test
testesTarefa2 = TestList [testeInimigosNoAlcance, testeAtingeInimigo, testeGanhouJogo, testePerdeuJogo, testeAtivaInimigo]