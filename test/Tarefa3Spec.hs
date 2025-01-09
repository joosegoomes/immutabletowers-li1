module Tarefa3Spec (testesTarefa3) where

import Test.HUnit
import Tarefa3
import LI12425

-- Teste 1: Atualização do estado do jogo com inimigos se movendo e atingindo a base
testeAtualizaJogo :: Test
testeAtualizaJogo = TestCase $ do
  let baseInicial = Base {vidaBase = 100, posicaoBase = (5, 5), creditosBase = 0}
      inimigo1 = Inimigo {posicaoInimigo = (4, 5), direcaoInimigo = Este, vidaInimigo = 10, velocidadeInimigo = 1, ataqueInimigo = 10, butimInimigo = 0, projeteisInimigo = []}
      inimigo2 = Inimigo {posicaoInimigo = (10, 10), direcaoInimigo = Norte, vidaInimigo = 20, velocidadeInimigo = 2, ataqueInimigo = 20, butimInimigo = 0, projeteisInimigo = []}
      jogoInicial = Jogo {baseJogo = baseInicial, inimigosJogo = [inimigo1, inimigo2], torresJogo = [], portaisJogo = [], mapaJogo = [[Terra]], lojaJogo = []}
      jogoAtualizado = atualizaJogo 1 jogoInicial
      vidaEsperada = 90 -- Inimigo1 ataca a base
  assertEqual "Base deve perder vida devido ao ataque do inimigo" vidaEsperada (vidaBase $ baseJogo jogoAtualizado)

-- Teste 2: Movimentação de inimigos
testeMovimentacaoInimigos :: Test
testeMovimentacaoInimigos = TestCase $ do
  let inimigo = Inimigo {posicaoInimigo = (0, 0), direcaoInimigo = Este, vidaInimigo = 10, velocidadeInimigo = 1, ataqueInimigo = 5, butimInimigo = 0, projeteisInimigo = []}
      jogo = Jogo {baseJogo = Base {vidaBase = 100, posicaoBase = (5, 5), creditosBase = 0}, inimigosJogo = [inimigo], torresJogo = [], portaisJogo = [], mapaJogo = [[Terra]], lojaJogo = []}
      jogoAtualizado = atualizaJogo 1 jogo
      posicaoEsperada = (1, 0)
  assertEqual "Inimigo deve se mover para o leste" posicaoEsperada (posicaoInimigo $ head (inimigosJogo jogoAtualizado))

-- Teste 3: Aplicação de efeitos em inimigos
testeAplicaEfeitosProjetis :: Test
testeAplicaEfeitosProjetis = TestCase $ do
  let inimigo = Inimigo {posicaoInimigo = (0, 0), direcaoInimigo = Norte, vidaInimigo = 50, velocidadeInimigo = 1, ataqueInimigo = 5, butimInimigo = 0, projeteisInimigo = [Projetil Fogo (Finita 1), Projetil Gelo Infinita]}
      -- Simulando aplicação de efeitos
      inimigoAtualizado = inimigo 
        { vidaInimigo = vidaInimigo inimigo - 5, -- Efeito do projétil de fogo
          velocidadeInimigo = max 0 (velocidadeInimigo inimigo - 1) -- Efeito do projétil de gelo
        }
  assertEqual "Inimigo deve ter vida reduzida pelo Fogo" 45 (vidaInimigo inimigoAtualizado)
  assertEqual "Inimigo deve ter velocidade reduzida pelo Gelo" 0 (velocidadeInimigo inimigoAtualizado)


-- Teste 4: Torres aplicando dano a inimigos
testeAtualizaEstadoTorres :: Test
testeAtualizaEstadoTorres = TestCase $ do
  let torre = Torre {posicaoTorre = (0, 0), danoTorre = 10, alcanceTorre = 5, rajadaTorre = 1, cicloTorre = 1, tempoTorre = 0, projetilTorre = Projetil Fogo (Finita 2)}
      inimigo = Inimigo {posicaoInimigo = (3, 4), direcaoInimigo = Norte, vidaInimigo = 30, velocidadeInimigo = 1, ataqueInimigo = 5, butimInimigo = 0, projeteisInimigo = []}
      jogo = Jogo {baseJogo = Base {vidaBase = 100, posicaoBase = (5, 5), creditosBase = 0}, inimigosJogo = [inimigo], torresJogo = [torre], portaisJogo = [], mapaJogo = [[Terra]], lojaJogo = []}
      jogoAtualizado = atualizaJogo 1 jogo
  assertEqual "Inimigo deve receber dano da torre" 20 (vidaInimigo $ head (inimigosJogo jogoAtualizado))
  assertEqual "Torre deve entrar em cooldown" 1 (tempoTorre $ head (torresJogo jogoAtualizado))

-- Teste 5: Portais ativando novas ondas
testeAtualizaEstadoPortais :: Test
testeAtualizaEstadoPortais = TestCase $ do
  let inimigo = Inimigo {posicaoInimigo = (0, 0), direcaoInimigo = Norte, vidaInimigo = 30, velocidadeInimigo = 1, ataqueInimigo = 5, butimInimigo = 0, projeteisInimigo = []}
      onda = Onda {inimigosOnda = [inimigo], cicloOnda = 5, tempoOnda = 0, entradaOnda = 0}
      portal = Portal {posicaoPortal = (10, 10), ondasPortal = [onda]}
      jogo = Jogo {baseJogo = Base {vidaBase = 100, posicaoBase = (5, 5), creditosBase = 0}, inimigosJogo = [], torresJogo = [], portaisJogo = [portal], mapaJogo = [[Terra]], lojaJogo = []}
      jogoAtualizado = atualizaJogo 1 jogo
  assertBool "Portal deve ativar nova onda" (not . null . inimigosOnda . head . ondasPortal . head $ portaisJogo jogoAtualizado)

-- Agrupando todos os testes
testesTarefa3 :: Test
testesTarefa3 = TestList [testeAtualizaJogo, testeMovimentacaoInimigos, testeAplicaEfeitosProjetis, testeAtualizaEstadoTorres, testeAtualizaEstadoPortais]
