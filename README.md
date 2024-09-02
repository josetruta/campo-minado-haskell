![Haskell](https://img.shields.io/badge/Haskell-5e5086?style=for-the-badge&logo=haskell&logoColor=white)

# Campo Minado

Projeto do clássico jogo de **Campo Minado** para o terminal desenvolvido em linguagem funcional para a disciplina de *Paradigmas de Linguagem de Programação*, do curso de *Ciência da Computação*, na UFCG. A linguagem de programação funcional utilizada no projeto é o **Haskell**.

## Iniciando o jogo
1. Caso ainda não tenha, instale o Cabal pelo [GHCup](https://www.haskell.org/ghcup/).
2. Instale pelo clone deste diretório do Git.
```
git clone https://github.com/josetruta/campo-minado-haskell.git
cd campo-minado-haskell
cabal run
```
3. Tudo pronto para iniciar!

## Como jogar

Nosso **Campo Minado** é dotado de três diferentes modos de jogo (cada um com três níveis de dificuldades!).

- **Clássico:** O modo clássico de se jogar campo minado. Tome cuidado por onde pisa, pode acabar explodindo sua cabeça! Se algum lugar parece suspeito, apenas o marque com uma bandeirinha.
- **Survival:** Uma releitura do modo clássico, com uma dificuldade acrescentada - só coloque uma bandeira na posição em que há uma bomba; caso contrário, *game over*!
- **Contra o Tempo:** Neste modo, você terá que concluir uma partida do modo Clássico em um determinado limite de tempo para a sua vitória! Você terá 100, 200 e 360 segundos nas dificuldades Fácil, Médio e Díficil, respectivamente.

### Controles

**No menu inicial:**

- `C` - Escolhe modo Clássico.
- `S` - Escolhe modo Survival.
- `T` - Escolhe modo Contra o Tempo.
- Como default, é escolhido o modo Clássico.

**No menu de dificuldade:**

- `F` - Escolhe dificuldade Fácil.
- `M` - Escolhe dificuldade Médio.
- `D` - Escolhe dificuldade Difícil.
- Como default, é escolhido a dificuldade Fácil.

**Ações de jogo:**

- `D YX` - Desenterra área na coordenada dada.
- `B YX` - Adiciona bandeira na coordenada dada.
- Como default, é adicionado uma bandeira na coordenada repassada.

## Autores

Este projeto tem como colaboradores:

- [Carlos Henrique Tavares de Melo](https://github.com/c4rlos-henrique)
- [Davi Lima de Medeiros](https://github.com/DaviLdM)
- [Dagbegnon Noel Aklou](https://github.com/Noelakl1995)
- [José do Bomfim Truta Neto](https://github.com/josetruta)
- [Igor Tejo Bezerra Ribeiro Nogueira](https://github.com/igortejo)
