module Tarefa1Spec (testesTarefa1) where

import Test.HUnit
import Tarefa1
import LI12425 -- Importa os tipos e estruturas fornecidas no enunciado

-- Teste 1: Valida jogo com um estado válido
testeValidaJogoValido :: Test
testeValidaJogoValido = TestCase $ do
  let mapaValido = [[Terra, Terra], [Relva, Relva]]
      baseValida = Base {posicaoBase = (1, 1), vidaBase = 100, creditosBase = 50}
      portalValido = Portal {posicaoPortal = (0, 0), ondasPortal = [Onda {inimigosOnda = [], cicloOnda = 1, tempoOnda = 0, entradaOnda = 0}]}
      jogoValido = Jogo {baseJogo = baseValida, portaisJogo = [portalValido], torresJogo = [], mapaJogo = mapaValido, inimigosJogo = [], lojaJogo = []}
  assertBool "Estado de jogo válido deve retornar True" (validaJogo jogoValido)

-- Teste 2: Valida jogo sem portais
testeValidaJogoSemPortais :: Test
testeValidaJogoSemPortais = TestCase $ do
  let mapaValido = [[Terra, Terra], [Relva, Relva]]
      baseValida = Base {posicaoBase = (1, 1), vidaBase = 100, creditosBase = 50}
      jogoSemPortais = Jogo {baseJogo = baseValida, portaisJogo = [], torresJogo = [], mapaJogo = mapaValido, inimigosJogo = [], lojaJogo = []}
  assertBool "Estado de jogo sem portais deve retornar False" (not $ validaJogo jogoSemPortais)

-- Teste 3: Valida jogo com portal fora de terra
testePortalForaDeTerra :: Test
testePortalForaDeTerra = TestCase $ do
  let mapaValido = [[Relva, Relva], [Relva, Relva]]
      baseValida = Base {posicaoBase = (1, 1), vidaBase = 100, creditosBase = 50}
      portalInvalido = Portal {posicaoPortal = (0, 0), ondasPortal = [Onda {inimigosOnda = [], cicloOnda = 1, tempoOnda = 0, entradaOnda = 0}]}
      jogoInvalido = Jogo {baseJogo = baseValida, portaisJogo = [portalInvalido], torresJogo = [], mapaJogo = mapaValido, inimigosJogo = [], lojaJogo = []}
  assertBool "Portal fora de terra deve retornar False" (not $ validaJogo jogoInvalido)

-- Teste 4: Valida torre fora da relva
testeTorreForaDeRelva :: Test
testeTorreForaDeRelva = TestCase $ do
  let mapaValido = [[Relva, Terra], [Relva, Relva]]
      baseValida = Base {posicaoBase = (1, 1), vidaBase = 100, creditosBase = 50}
      torreInvalida = Torre {posicaoTorre = (0, 1), danoTorre = 10, alcanceTorre = 5, rajadaTorre = 1, cicloTorre = 1, tempoTorre = 0, projetilTorre = Projetil {tipoProjetil = Fogo, duracaoProjetil = Infinita}}
      portalValido = Portal {posicaoPortal = (0, 0), ondasPortal = [Onda {inimigosOnda = [], cicloOnda = 1, tempoOnda = 0, entradaOnda = 0}]}
      jogoInvalido = Jogo {baseJogo = baseValida, portaisJogo = [portalValido], torresJogo = [torreInvalida], mapaJogo = mapaValido, inimigosJogo = [], lojaJogo = []}
  assertBool "Torre fora da relva deve retornar False" (not $ validaJogo jogoInvalido)

-- Teste 5: Valida base com posição inválida
testeBasePosicaoInvalida :: Test
testeBasePosicaoInvalida = TestCase $ do
  let mapaValido = [[Relva, Terra], [Relva, Relva]]
      baseInvalida = Base {posicaoBase = (0, 0), vidaBase = 100, creditosBase = 50}
      portalValido = Portal {posicaoPortal = (0, 1), ondasPortal = [Onda {inimigosOnda = [], cicloOnda = 1, tempoOnda = 0, entradaOnda = 0}]}
      jogoInvalido = Jogo {baseJogo = baseInvalida, portaisJogo = [portalValido], torresJogo = [], mapaJogo = mapaValido, inimigosJogo = [], lojaJogo = []}
  assertBool "Base com posição inválida deve retornar False" (not $ validaJogo jogoInvalido)

-- Agrupando todos os testes
testesTarefa1 :: Test
testesTarefa1 = TestList [testeValidaJogoValido, testeValidaJogoSemPortais, testePortalForaDeTerra, testeTorreForaDeRelva, testeBasePosicaoInvalida]

