# paperfold_game

This puzzle game provides to the user a paper folding simulator. The goal of each level of the game is to find the sequence of folding to obtain the expected result.

a level is defined by the size (heigth, width) of a paper and the expected string.

the repository https://github.com/frankhillard/paperfold_ai.git contains the implementation of paper folding functions written in scala language.

The game UI starts by displaying the main menu with a button "start" and a button "how to play".

the start button triggers the beginning of the first level. The levels directory contains the level as a JSON file.

The UI consists of the display of a grid representing the paper. this grid allows the user to select a line separator that separate two lines or a column separator that separate two columns. Once the separator (horizontal or vertical) is selected, user must select the right part or the left part (for a vertical folding), or select the upper part or the bottom part (for a horizontal folding). Once separator and part selected, the paper is folded respectively to the selected separator and part. It redisplay the paper after folding is performed.

once the grid size is 1 by 1, we consider the attempt of the user finished and we can compare to resulting string to the expected one. If strings are equal the level is considered as completed, otherwise it is considered failed. If the level is failed display a button "retry" to reload the level.

Once a level is completed display a button "retry" to reload the initial paper and a button "next" to start the next level.

## Run

```
python3 -m http.server 8080
```

Then open http://127.0.0.1:8080/.
