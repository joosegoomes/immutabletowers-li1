# Laboratórios de Informática I

Immutable Towers é um jogo do gênero [Tower Defense](https://en.wikipedia.org/wiki/Tower_defense), desenvolvido em [Haskell](https://www.haskell.org/) no âmbito da cadeira Laboratórios de Informática I, do curso de Engenharia Informática na Universidade do Minho (ano letivo 2024/25).

O objetivo do jogo é impedir que ondas de inimigos alcancem a base do jogador. Para isso, é necessário posicionar estrategicamente torres que atacam automaticamente os inimigos, evitando que estes destruam a base.

## Nota final do projeto: 16/20

## Enunciado do projeto

O enunciado do projeto pode ser consultado [aqui](./enunciado_projeto.pdf)

## Desenvolvido por

- [José dos Santos Gomes](https://github.com/joosegoomes) - a110367
- [Guilherme Figueiredo Castro](https://github.com/GF-Castro) - a110452

## Executável

Pode compilar e executar o programa através dos comandos `build` e `run` do Cabal.

```bash
cabal run --verbose=0
```

## Interpretador

Para abrir o interpretador do Haskell (GHCi) com o projeto carregado, utilize o comando `repl` do Cabal

```bash
cabal repl
```

## Testes

O projecto utiliza a biblioteca [HUnit](https://hackage.haskell.org/package/HUnit) para fazer testes unitários.

Execute os testes com o comando `test` do Cabal e utilize a flag `--enable-coverage` para gerar um relatório de cobertura de testes.

```bash
cabal test --enable-coverage
```

Execute os exemplos da documentação como testes com a biblioteca
[`doctest`](https://hackage.haskell.org/package/doctest). Para instalar o
executavel utilize o comando `cabal install doctest`.

```bash
cabal repl --build-depends=QuickCheck,doctest --with-ghc=doctest --verbose=0
```

## Documentação

A documentação do projeto pode ser gerada recorrendo ao [Haddock](https://haskell-haddock.readthedocs.io/).

```bash
cabal haddock
```