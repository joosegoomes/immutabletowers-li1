  {-|
  Module      : Tarefa2
  Description : Auxiliares do Jogo
  Copyright   : Jose dos Santos Gomes <a110367@alunos.uminho.pt>
                Guilherme Figueiredo Castro <a110452@alunos.uminho.pt>

  Módulo para a realização da Tarefa 2 de LI1 em 2024/25.
  -}
  module Tarefa2 where

  import LI12425

  -- | A função 'inimigosNoAlcance' recebe uma torre e uma lista de inimigos e retorna uma lista dos inimigos que estão ao alcance da torre.
  -- (Alínea 1)
  inimigosNoAlcance :: Torre -> [Inimigo] -> [Inimigo]
  inimigosNoAlcance torre = filter (\inimigo -> distancia (posicaoInimigo inimigo) (posicaoTorre torre) <= alcanceTorre torre)
    where distancia (x1, y1) (x2, y2) = sqrt ((x1 - x2) ^ (2 :: Integer) + (y1 - y2) ^ (2 :: Integer))

  -- | 'atingeInimigo' aplica o dano de uma torre a um inimigo, reduzindo a vida do inimigo.
  --   Também atualiza a lista de projéteis do inimigo com base nas sinergias entre os projéteis.
  -- (Alínea 2)
  atingeInimigo :: Torre -> Inimigo -> Inimigo
  atingeInimigo Torre {danoTorre = dano, projetilTorre = projTorre} inimigo@Inimigo {vidaInimigo = vida, projeteisInimigo = projInimigo} =
    inimigo { vidaInimigo = max 0 (vida - dano), projeteisInimigo = atualizaProjeteis projTorre projInimigo}
    where
      -- Funções de sinergia de projéteis
      atualizaProjeteis :: Projetil -> [Projetil] -> [Projetil]
      atualizaProjeteis proj [] = [proj]
      atualizaProjeteis proj (p:ps)
        | cancelaMutuamente proj p = atualizaProjeteis proj ps
        | dobraDuracao proj p = proj {duracaoProjetil = dobra proj} : atualizaProjeteis proj ps
        | dobraDuracao' proj p = p {duracaoProjetil = dobra p} : atualizaProjeteis p ps
        | otherwise = p : atualizaProjeteis proj ps

      -- Cancelamentos mutuamente
      cancelaMutuamente :: Projetil -> Projetil -> Bool
      cancelaMutuamente (Projetil Gelo _) (Projetil Fogo _) = True
      cancelaMutuamente (Projetil Fogo _) (Projetil Gelo _) = True
      cancelaMutuamente _ _ = False

      -- Dobra a duração de projéteis
      dobraDuracao :: Projetil -> Projetil -> Bool
      dobraDuracao (Projetil Fogo _) (Projetil Resina _) = True
      dobraDuracao _ _ = False

      dobraDuracao' :: Projetil -> Projetil -> Bool
      dobraDuracao' (Projetil Resina _) (Projetil Fogo _) = True
      dobraDuracao' _ _ = False

      dobra :: Projetil -> Duracao
      dobra (Projetil _ (Finita t)) = Finita (2 * t)
      dobra (Projetil _ Infinita) = Infinita


  -- | 'terminouJogo' verifica se o jogo terminou, seja por vitória ou derrota.
  -- (Alínea 3)
  terminouJogo :: Jogo -> Bool
  terminouJogo jogo = ganhouJogo jogo || perdeuJogo jogo

  -- | 'ganhouJogo' verifica se o jogador ganhou o jogo, ou seja, não há mais inimigos e a vida da base é maior que 0. (win condition)
  ganhouJogo :: Jogo -> Bool
  ganhouJogo Jogo {baseJogo = Base {vidaBase = vida}, inimigosJogo = inimigos} = vida > 0 && null inimigos

  -- | 'perdeuJogo' verifica se o jogador perdeu o jogo, ou seja, a vida da base é 0 ou menor. (lose condition)
  perdeuJogo :: Jogo -> Bool
  perdeuJogo Jogo {baseJogo = Base {vidaBase = vida}} = vida <= 0

  -- | 'ativaInimigo' ativa o próximo inimigo na onda de um portal e atualiza o estado do jogo.
  -- (Alínea 4)
  ativaInimigo :: Portal -> Jogo -> Jogo 
  ativaInimigo Portal {ondasPortal = []} jogo = jogo
  ativaInimigo portal@Portal {ondasPortal = (onda:ondas)} jogo = 
    jogo { inimigosJogo = inimigosJogo jogo ++ novosInimigos, portaisJogo = atualizaPortal portal } 
    where 
    novosInimigos = take 1 (inimigosOnda onda) -- Ativa apenas o próximo inimigo 
    ondaAtualizada = onda {inimigosOnda = drop 1 (inimigosOnda onda)} 
    atualizaPortal p = [p {ondasPortal = ondaAtualizada : ondas}]

  -- Tarefa 2 concluída --