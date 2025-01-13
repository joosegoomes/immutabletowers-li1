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
 ondasPortal = [ 
   -- Onda 1
   Onda 
     {inimigosOnda = [Inimigo
         {posicaoInimigo = (1.0, 0.0),
          direcaoInimigo = Sul,
          vidaInimigo = 60.0,
          velocidadeInimigo = 1.0,
          ataqueInimigo = 50.0,
          butimInimigo = 30,
          projeteisInimigo = []}],
      cicloOnda = 25,
      tempoOnda = 3.5,
      entradaOnda = 2.5},
   -- Onda 2
   Onda 
     {inimigosOnda = [Inimigo
         {posicaoInimigo = (1.0, 0.0),
          direcaoInimigo = Sul,
          vidaInimigo = 70.0,
          velocidadeInimigo = 1.2,
          ataqueInimigo = 50.0,
          butimInimigo = 35,
          projeteisInimigo = []}],
      cicloOnda = 28,
      tempoOnda = 3.2,
      entradaOnda = 3.0},
   -- Onda 3
   Onda 
     {inimigosOnda = [Inimigo
         {posicaoInimigo = (1.0, 0.0),
          direcaoInimigo = Sul,
          vidaInimigo = 80.0,
          velocidadeInimigo = 1.1,
          ataqueInimigo = 50.0,
          butimInimigo = 40,
          projeteisInimigo = []}],
      cicloOnda = 30,
      tempoOnda = 3.0,
      entradaOnda = 3.5},
   -- Onda 4
   Onda 
     {inimigosOnda = [Inimigo
         {posicaoInimigo = (1.0, 0.0),
          direcaoInimigo = Sul,
          vidaInimigo = 90.0,
          velocidadeInimigo = 1.3,
          ataqueInimigo = 50.0,
          butimInimigo = 45,
          projeteisInimigo = []}],
      cicloOnda = 26,
      tempoOnda = 3.6,
      entradaOnda = 2.8},
   -- Onda 5
   Onda 
     {inimigosOnda = [Inimigo
         {posicaoInimigo = (1.0, 0.0),
          direcaoInimigo = Sul,
          vidaInimigo = 100.0,
          velocidadeInimigo = 1.0,
          ataqueInimigo = 100.0,
          butimInimigo = 50,
          projeteisInimigo = []}],
      cicloOnda = 32,
      tempoOnda = 3.4,
      entradaOnda = 3.2},
   -- Onda 6
   Onda 
     {inimigosOnda = [Inimigo
         {posicaoInimigo = (1.0, 0.0),
          direcaoInimigo = Sul,
          vidaInimigo = 110.0,
          velocidadeInimigo = 1.2,
          ataqueInimigo = 100.0,
          butimInimigo = 55,
          projeteisInimigo = []}],
      cicloOnda = 35,
      tempoOnda = 4.0,
      entradaOnda = 4.0},
   -- Onda 7
   Onda 
     {inimigosOnda = [Inimigo
         {posicaoInimigo = (1.0, 0.0),
          direcaoInimigo = Sul,
          vidaInimigo = 120.0,
          velocidadeInimigo = 1.1,
          ataqueInimigo = 100.0,
          butimInimigo = 60,
          projeteisInimigo = []}],
      cicloOnda = 38,
      tempoOnda = 4.2,
      entradaOnda = 4.5},
   -- Onda 8
   Onda 
     {inimigosOnda = [Inimigo
         {posicaoInimigo = (1.0, 0.0),
          direcaoInimigo = Sul,
          vidaInimigo = 130.0,
          velocidadeInimigo = 1.0,
          ataqueInimigo = 100.0,
          butimInimigo = 65,
          projeteisInimigo = []}],
      cicloOnda = 40,
      tempoOnda = 4.3,
      entradaOnda = 4.7},
   -- Onda 9
   Onda 
     {inimigosOnda = [Inimigo
         {posicaoInimigo = (1.0, 0.0),
          direcaoInimigo = Sul,
          vidaInimigo = 140.0,
          velocidadeInimigo = 1.4,
          ataqueInimigo = 150.0,
          butimInimigo = 70,
          projeteisInimigo = []}],
      cicloOnda = 42,
      tempoOnda = 4.6,
      entradaOnda = 5.0},
   -- Onda 10
   Onda 
     {inimigosOnda = [Inimigo
         {posicaoInimigo = (1.0, 0.0),
          direcaoInimigo = Norte,
          vidaInimigo = 150.0,
          velocidadeInimigo = 1.2,
          ataqueInimigo = 15.0,
          butimInimigo = 75,
          projeteisInimigo = []}],
      cicloOnda = 33,
      tempoOnda = 4.5,
      entradaOnda = 4.2},
   -- Onda 11
   Onda 
     {inimigosOnda = [Inimigo
         {posicaoInimigo = (1.0, 0.0),
          direcaoInimigo = Sul,
          vidaInimigo = 160.0,
          velocidadeInimigo = 0.8,
          ataqueInimigo = 200.0,
          butimInimigo = 80,
          projeteisInimigo = []}],
      cicloOnda = 45,
      tempoOnda = 4.8,
      entradaOnda = 5.2}]},
   Portal
      {posicaoPortal = (4, 9), -- Posição do portal 2
       ondasPortal = [
        -- Onda 1
        Onda 
            {inimigosOnda = [Inimigo
                {posicaoInimigo = (4.0, 9.0),
                direcaoInimigo = Norte,
                vidaInimigo = 60.0,
                velocidadeInimigo = 1.0,
                ataqueInimigo = 50.0,
                butimInimigo = 30,
                projeteisInimigo = []}],
            cicloOnda = 25,
            tempoOnda = 3.5,
            entradaOnda = 2.5},
        -- Onda 2
        Onda 
            {inimigosOnda = [Inimigo
                {posicaoInimigo = (4.0, 9.0),
                direcaoInimigo = Norte,
                vidaInimigo = 70.0,
                velocidadeInimigo = 1.2,
                ataqueInimigo = 50.0,
                butimInimigo = 35,
                projeteisInimigo = []}],
            cicloOnda = 28,
            tempoOnda = 3.2,
            entradaOnda = 3.0},
        -- Onda 3
        Onda 
            {inimigosOnda = [Inimigo
                {posicaoInimigo = (4.0, 9.0),
                direcaoInimigo = Norte,
                vidaInimigo = 80.0,
                velocidadeInimigo = 1.1,
                ataqueInimigo = 50.0,
                butimInimigo = 40,
                projeteisInimigo = []}],
            cicloOnda = 30,
            tempoOnda = 3.0,
            entradaOnda = 3.5},
        -- Onda 4
        Onda 
            {inimigosOnda = [Inimigo
                {posicaoInimigo = (4.0, 9.0),
                direcaoInimigo = Norte,
                vidaInimigo = 90.0,
                velocidadeInimigo = 1.3,
                ataqueInimigo = 50.0,
                butimInimigo = 45,
                projeteisInimigo = []}],
            cicloOnda = 26,
            tempoOnda = 3.6,
            entradaOnda = 2.8},
        -- Onda 5
        Onda 
            {inimigosOnda = [Inimigo
                {posicaoInimigo = (4.0, 9.0),
                direcaoInimigo = Norte,
                vidaInimigo = 100.0,
                velocidadeInimigo = 1.0,
                ataqueInimigo = 100.0,
                butimInimigo = 50,
                projeteisInimigo = []}],
            cicloOnda = 32,
            tempoOnda = 3.4,
            entradaOnda = 3.2},
        -- Onda 6
        Onda 
            {inimigosOnda = [Inimigo
                {posicaoInimigo = (4.0, 9.0),
                direcaoInimigo = Norte,
                vidaInimigo = 110.0,
                velocidadeInimigo = 1.2,
                ataqueInimigo = 100.0,
                butimInimigo = 55,
                projeteisInimigo = []}],
            cicloOnda = 35,
            tempoOnda = 4.0,
            entradaOnda = 4.0},
        -- Onda 7
        Onda 
            {inimigosOnda = [Inimigo
                {posicaoInimigo = (4.0, 9.0),
                direcaoInimigo = Norte,
                vidaInimigo = 120.0,
                velocidadeInimigo = 1.1,
                ataqueInimigo = 100.0,
                butimInimigo = 60,
                projeteisInimigo = []}],
            cicloOnda = 38,
            tempoOnda = 4.2,
            entradaOnda = 4.5},
        -- Onda 8
        Onda 
            {inimigosOnda = [Inimigo
                {posicaoInimigo = (4.0, 9.0),
                direcaoInimigo = Norte,
                vidaInimigo = 130.0,
                velocidadeInimigo = 1.0,
                ataqueInimigo = 100.0,
                butimInimigo = 65,
                projeteisInimigo = []}],
            cicloOnda = 40,
            tempoOnda = 4.3,
            entradaOnda = 4.7},
        -- Onda 9
        Onda 
            {inimigosOnda = [Inimigo
                {posicaoInimigo = (4.0, 9.0),
                direcaoInimigo = Norte,
                vidaInimigo = 140.0,
                velocidadeInimigo = 1.4,
                ataqueInimigo = 150.0,
                butimInimigo = 70,
                projeteisInimigo = []}],
            cicloOnda = 42,
            tempoOnda = 4.6,
            entradaOnda = 5.0},
        -- Onda 10
        Onda 
            {inimigosOnda = [Inimigo
                {posicaoInimigo = (4.0, 9.0),
                direcaoInimigo = Norte,
                vidaInimigo = 150.0,
                velocidadeInimigo = 1.2,
                ataqueInimigo = 15.0,
                butimInimigo = 75,
                projeteisInimigo = []}],
            cicloOnda = 33,
            tempoOnda = 4.5,
            entradaOnda = 4.2},
        -- Onda 11
        Onda 
            {inimigosOnda = [Inimigo
                {posicaoInimigo = (4.0, 9.0),
                direcaoInimigo = Norte,
                vidaInimigo = 160.0,
                velocidadeInimigo = 0.8,
                ataqueInimigo = 200.0,
                butimInimigo = 80,
                projeteisInimigo = []}],
            cicloOnda = 45,
            tempoOnda = 4.8,
            entradaOnda = 5.2}]}],
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
          (75, Torre {posicaoTorre = (0.0, 0.0), danoTorre = 10.0, alcanceTorre = 3.0, -- editar alcance
                      rajadaTorre = 5, cicloTorre = 3.0, tempoTorre = 0,
                      projetilTorre = Projetil {tipoProjetil = Resina, duracaoProjetil = Infinita}}),
          (150, Torre {posicaoTorre = (0.0, 0.0), danoTorre = 15.0, alcanceTorre = 2.0,
                      rajadaTorre = 3, cicloTorre = 5.0, tempoTorre = 0,
                      projetilTorre = Projetil {tipoProjetil = Fogo, duracaoProjetil = Finita 3}}),
          (300, Torre {posicaoTorre = (0.0, 0.0), danoTorre = 25.0, alcanceTorre = 2.0, -- editar alcance
                      rajadaTorre = 3, cicloTorre = 5.0, tempoTorre = 0,
                      projetilTorre = Projetil {tipoProjetil = Gelo, duracaoProjetil = Finita 2}})]}