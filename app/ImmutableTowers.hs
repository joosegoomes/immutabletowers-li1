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
   portaisJogo = [Portal
      {posicaoPortal = (7, 3), -- Posição do portal inicial
       ondasPortal = [Onda {inimigosOnda = [Inimigo
              {posicaoInimigo = (0.0, 0.0),
               direcaoInimigo = Sul,
               vidaInimigo = 50.0,
               velocidadeInimigo = 1.0,
               ataqueInimigo = 5.0,
               butimInimigo = 25,
               projeteisInimigo = [] }],
               cicloOnda = 3.0,
               tempoOnda = 3.0,
               entradaOnda = 0.0}] }],
    torresJogo = [],
    mapaJogo = mapa,
    inimigosJogo = [Inimigo
       {posicaoInimigo = (1, 1),           -- Posição inicial (x, y)
        direcaoInimigo = Sul,           -- Direção inicial (movimento para a direita)
        vidaInimigo = 100,                 -- Vida inicial
        velocidadeInimigo = 5,             -- Velocidade inicial
        ataqueInimigo = 10,                -- Dano causado
        butimInimigo = 50,                 -- Créditos ao ser derrotado
        projeteisInimigo = []}],             -- Nenhum projétil inicialmente
    lojaJogo = [
          (150, Torre {posicaoTorre = (0.0, 0.0), danoTorre = 20.0, alcanceTorre = 5.0,
                      rajadaTorre = 1, cicloTorre = 1.5, tempoTorre = 0.0,
                      projetilTorre = Projetil {tipoProjetil = Fogo, duracaoProjetil = Finita 2.0}}),
          (300, Torre {posicaoTorre = (0.0, 0.0), danoTorre = 10.0, alcanceTorre = 4.0,
                      rajadaTorre = 1, cicloTorre = 2.0, tempoTorre = 0.0,
                      projetilTorre = Projetil {tipoProjetil = Gelo, duracaoProjetil = Finita 1.5}}),
          (75, Torre {posicaoTorre = (0.0, 0.0), danoTorre = 5.0, alcanceTorre = 6.0,
                      rajadaTorre = 1, cicloTorre = 1.0, tempoTorre = 0.0,
                      projetilTorre = Projetil {tipoProjetil = Resina, duracaoProjetil = Infinita}})]}
