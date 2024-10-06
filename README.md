![Prolog](https://img.shields.io/badge/Prolog-5e5086?style=for-the-badge&logo=prolog&logoColor=white)

# Campo Minado

Projeto do clássico jogo de **Campo Minado** para o terminal desenvolvido em linguagem lógica para a disciplina de *Paradigmas de Linguagem de Programação*, do curso de *Ciência da Computação*, na UFCG. A linguagem de programação lógica utilizada no projeto é o **Prolog**.

## Iniciando o jogo
1. Caso ainda não tenha, instale o  pelo [SWI-Prolog](https://www.swi-prolog.org/download/stable).
2. Instale pelo clone deste diretório do Git.
```
git clone https://github.com/josetruta/campo-minado-prolog.git
```
3. Consulte `main.` para iniciar o jogo!

## Como jogar

Nosso **Campo Minado** é dotado de três diferentes modos de jogo (cada um com três níveis de dificuldades!).

- **Clássico:** O modo clássico de se jogar campo minado. Tome cuidado por onde pisa, pode acabar explodindo sua cabeça! Se algum lugar parece suspeito, apenas o marque com uma bandeirinha.
- **Survival:** Uma releitura do modo clássico, com uma dificuldade acrescentada - só coloque uma bandeira na posição em que há uma bomba; caso contrário, *game over*!
- **Contra o Tempo:** Neste modo, você terá que concluir uma partida do modo Clássico em um determinado limite de tempo para a sua vitória! Você terá 200, 400 e 600 segundos nas dificuldades Fácil, Médio e Díficil, respectivamente.

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

- `D X Y` - Desenterra área na coordenada dada.
- `B X Y` - Adiciona bandeira na coordenada dada.
- Como default, é adicionado uma bandeira na coordenada repassada.

## Autores

Este projeto tem como colaboradores:

- [Davi Lima de Medeiros](https://github.com/DaviLdM)
- [Dagbegnon Noel Aklou](https://github.com/Noelakl1995)
- [José do Bomfim Truta Neto](https://github.com/josetruta)