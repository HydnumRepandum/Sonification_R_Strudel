# PoliSonification

This repo is an adaptation of the work Frank Pattyn (https://github.com/FrankPat/Sonification), all the right is reserved to him.

# Change

1) Rewrite the complete code from MATLAB to R
2) Adapt to take csv file as input from temporal series relate to Political Sciences like the democracy index from the V-dem institute (https://v-dem.net/data_analysis/VariableGraph/)
3) Present a Framework on how to transfer the .abc file to Strudel (javascript interpreter) 

# How to use it

1) Import your csv file from your time series in the Data2Music.r file. You will have an ABC as output.
     The .abc file can be transformed into sheet music or Midi file using online abc tools (see https://abcnotation.com/ for more info).
2) Import your .abc file in the logiciel easyABC, convert it to Midi (https://sourceforge.net/projects/easyabc/)
3) Use the repo from Emanuel-de-Jong to convert a Midi file to Strudel code  (https://github.com/Emanuel-de-Jong/MIDI-To-Strudel)
4) Copy-Paste the output in strudel (https://strudel.cc/)
5) Now its your turn to let magic operate, try to create and fail, or sucess. When you have finished save your code in a javasript file (.js)

* You can find my test of creation on the folder JS_for_Strudel *
