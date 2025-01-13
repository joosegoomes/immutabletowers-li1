module ImmutableTowers where

import LI12425
import Desenhar

data ImmutableTowers = ImmutableTowers {estadoJogo :: Jogo, torreSelecionada :: TorreSelecionada, ecraJogo :: EcraJogo} deriving (Show)

-- Representa o tipo de torre atualmente selecionada
data TorreSelecionada = NadaSelecionado
                     | TorreFogo
                     | TorreGelo
                     | TorreResina
                     deriving (Eq, Show)

data EcraJogo = MenuInicial | Gameplay | Win | GameOver deriving (Eq, Show) 

estadoInicialJogo :: Jogo --trocar os valores
estadoInicialJogo = Jogo
  {baseJogo = Base
      {vidaBase = 1000.0,
       posicaoBase = (13, 8), -- Posição inicial no mapa
       creditosBase = 100000},
   portaisJogo = [
   Portal
      {posicaoPortal = (1, 0), -- Posição do portal 1
        ondasPortal = [Onda 
          {inimigosOnda = [Inimigo
              {posicaoInimigo = (1.0, 0.0),
               direcaoInimigo = Norte,
               vidaInimigo = 50.0,
               velocidadeInimigo = 1.0,
               ataqueInimigo = 5.0,
               butimInimigo = 25,
               projeteisInimigo = []}],
            cicloOnda = 30,
            tempoOnda = 3.0,
            entradaOnda = 3.0}]},
   Portal
      {posicaoPortal = (4, 9), -- Posição do portal 2
        ondasPortal = [Onda 
          {inimigosOnda = [Inimigo
              {posicaoInimigo = (4.0, 9.0),
               direcaoInimigo = Sul,
               vidaInimigo = 50.0,
               velocidadeInimigo = 1.0,
               ataqueInimigo = 5.0,
               butimInimigo = 25,
               projeteisInimigo = []}],
            cicloOnda = 30,
            tempoOnda = 3.0,
            entradaOnda = 3.0}]}],
    torresJogo = [],
    mapaJogo = mapa,
 inimigosJogo = [
   Inimigo
        {posicaoInimigo = (1, 0),           -- Posição inicial (x, y)
         direcaoInimigo = Sul,            -- Direção inicial (movimento para baixo)
         vidaInimigo = 100,                 -- Vida inicial
         velocidadeInimigo = 3,             -- Velocidade inicial
         ataqueInimigo = 1000,                -- Dano causado
         butimInimigo = 50,                 -- Créditos ao ser derrotado
         projeteisInimigo = []}],
    lojaJogo = [
          (75, Torre {posicaoTorre = (0.0, 0.0), danoTorre = 10.0, alcanceTorre = 6.0,
                      rajadaTorre = 5, cicloTorre = 1.0, tempoTorre = 0,
                      projetilTorre = Projetil {tipoProjetil = Resina, duracaoProjetil = Infinita}}),
          (150, Torre {posicaoTorre = (0.0, 0.0), danoTorre = 15.0, alcanceTorre = 5.0,
                      rajadaTorre = 3, cicloTorre = 1.5, tempoTorre = 0,
                      projetilTorre = Projetil {tipoProjetil = Fogo, duracaoProjetil = Finita 2.0}}),
          (300, Torre {posicaoTorre = (0.0, 0.0), danoTorre = 25.0, alcanceTorre = 4.0,
                      rajadaTorre = 3, cicloTorre = 2.0, tempoTorre = 0,
                      projetilTorre = Projetil {tipoProjetil = Gelo, duracaoProjetil = Finita 1.5}})]}