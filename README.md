# PLP-Projects
## 1. Descrição do Projeto

&nbsp; O ForCobrinha baseia-se em dois jogos antológicos: o clássico jogo da cobrinha monocromática e o milenar jogo da forca. Neste divertido jogo, o jogador deverá ser capaz de controlar uma cobrinha, atrás de comida para seu crescimento, e ao mesmo tempo realizar palpites para as letras de uma palavra, e desta forma, garantido a vitória em um mapa. O jogo terá inicialmente 4 fases (mapas).

&nbsp; Cada vez que o jogador passar uma fase, a fase seguinte terá uma palavra de uma categoria diferente (fruta, cidade, cor, …) e com um maior número de letras.

## 2. Regras

1. Para o jogador tentar realizar um palpite de qual letra está na palavra oculta, ele deve primeiro conseguir capturar a frutinha com a cobrinha;
2. Caso o jogador consiga comer a frutinha, o jogo entrará em uma pausa, e o jogador deverá realizar o seu palpite;
3. Cada vez que o palpite do jogador estiver correto, a cobrinha irá crescer uma quantidade fixa;
4. A condição de vitória de cada etapa, se dá pelo acerto da palavra e a cobrinha ainda estando viva;
5. Caso o palpite esteja incorreto, o jogador será penalizado, a cobrinha irá diminuir uma quantidade fixa;
6. O mapa, no qual a cobrinha estará se movimentando, possui limitações (paredes), desta forma, caso a cobrinha bata na parede ela irá morrer. Nesta ocasião, será game over;
7. Caso a cobrinha perca todo seu tamanho, será game over;
8. A cobrinha pode subir, descer, ir para a esquerda ou direita. Caso esteja indo para cima, não poderá subitamente ir para baixo, ou seja, não serão admitidas mudanças de movimentos opostas à direção corrente, isto causará também game over.

## Instalação

&nbsp; É necessário ter instalado em seu computador o necessário para compilar e executar códigos feitos utilizando Haskell, para isto recomendamos utilizar o <a href="https://www.haskell.org/ghcup/" target _blank>GHCup</a>, pois com ele é possível instalar além do GHC, o Stack e o Cabal, ferramentas importantes para o gerenciamento e criação dos packages de um projeto Haskell.

&nbsp; O projeto foi feito utilizando o stack, e aconselhamos utiliza-lo para executá-lo, pois ele irá instalar e condfigurar as dependências do projeto automáticamente.

### Dependências (WINDOWS)

&nbsp; Infelizmente ao utilizar o Windows como SO, deve-se realizar um passo a mais, visto que iremos utilizar uma interface gráfica `gloss`. Para o funcionamento correto deve-se realizar o download do <a href="http://freeglut.sourceforge.net/" target _blank>freeGLUT</a>, copiar o arquivo `freeglut-MinGW-3.0.0-1.mp.zip\freeglut\bin\x64\freeglut.dll` para o diretório `C:\Windows\System32` e renomeá-lo para `glut32.dll`. 

> Caso apresente problemas quanto ao `System.Random` pode-se instalar via stack usando: `stack install random`
### Após o download
&nbsp; O procedimento de instalação para execução do jogo é:
- `stack setup`
- `stack ini` 
- `stack build`
## Execução
&nbsp; Para executar o projeto basta utilizar:
- `stack run`
> Aconselha-se que após a cada execução seja utilizado o comando `stack clean`. 

## Comandos do jogo
&nbsp; Para o movimento da cobrinha:
- As setas do teclado.
&nbsp; Para a entrada da letra do jogo da forca:
- Os caracteres alfabéticos do teclado
> Vale salientar que o jogo não ira utilizar acentos ou caracteres especiais, tais como 'ç', 'é' e etc.
&nbsp; Comandos especiais:
1. Dentro do jogo da cobrinha:
- Barra de espaço pausa o jogo, e na tela de pause é possível voltar para o Menu ao pressionar a letra 'q';
2. Dentro do Menu é possível navegar utilizando as setas Up e Down do teclado e para selecionar bastar pressionar Enter.
3. Tanto na tela de record quanto na tela de criadores é possível voltar para o Menu ao pressionar a letra 'q'
4. Em todos momentos do jogo é possível fechar a janela pressionando a tecla ESC.
