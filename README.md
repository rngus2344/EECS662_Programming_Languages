# EECS662_Programming_Languages
# To run the project:
ghci

:l p[Project_Number]

# In Project 1, by using Plus, Minus, Mult, or Div for the [Operator], you can do following:
evalErr (Num [Number])

evalErr (Sqr [Number])

evalErr ([Operator] (Num [Number]) (Num [Number]))


evalMaybe (Num [Number])

evalMaybe (Sqr [Number])

evalMaybe ([Operator] (Num [Number]) (Num [Number]))


evalM (Num [Number])

evalM (Sqr [Number])

evalM ([Operator] (Num [Number]) (Num [Number]))

# In Project 2, by using Plus, Minus, Mult, Div, And, Or, or Leq for the [Operator], you can do following:
evalM (Num [Number])

evalM (Boolean [Boolean])

evalM (IsZero (Num [Number]))

evalM (If (Boolean [Boolean]) (Num [Number])||(Boolean [Boolean]) (Num [Number])||(Boolean [Boolean]))

evalM ([Operator] (Num [Number])||(Boolean [Boolean]) (Num [Number])||(Boolean [Boolean]))


typeofM (Num [Number])

typeofM (Boolean [Boolean])

typeofM (IsZero (Num [Number]))

typeofM (If (Boolean [Boolean]) (Num [Number])||(Boolean [Boolean]) (Num [Number])||(Boolean [Boolean]))

typeofM ([Operator] (Num [Number])||(Boolean [Boolean]) (Num [Number])||(Boolean [Boolean]))


optimize (Num [Number])

optimize (Boolean [Boolean])

optimize (IsZero (Num [Number]))

optimize (If (Boolean [Boolean]) (Num [Number])||(Boolean [Boolean]) (Num [Number])||(Boolean [Boolean]))

optimize ([Operator] (Num [Number])||(Boolean [Boolean]) (Num [Number])||(Boolean [Boolean]))

# In Project 3, by using Plus, Minus, Mult, Div, And, Or, or Leq for the [Operator], you can do following (leave the empty square brackets as it is. It is necessary):
evalS (Num [Number])

evalS (Boolean [Boolean])

evalS (IsZero (Num [Number]))

evalS (Id "[Character]")

evalS (Bind "[Character]" (Num [Number])||(Boolean [Boolean]) [Statement])

evalS (If (Boolean [Boolean]) (Num [Number])||(Boolean [Boolean]) (Num [Number])||(Boolean [Boolean]))

evalS ([Operator] (Num [Number])||(Boolean [Boolean]) (Num [Number])||(Boolean [Boolean]))


evalM [] (Num [Number])

evalM [] (Boolean [Boolean])

evalM [] (IsZero (Num [Number]))

evalM [] (Id "[Character]")

evalM [] (Bind "[Character]" (Num [Number])||(Boolean [Boolean]) [Statement])

evalM [] (If (Boolean [Boolean]) (Num [Number])||(Boolean [Boolean]) (Num [Number])||(Boolean [Boolean]))

evalM [] ([Operator] (Num [Number])||(Boolean [Boolean]) (Num [Number])||(Boolean [Boolean]))


typeofM [] (Num [Number])

typeofM [] (Boolean [Boolean])

typeofM [] (IsZero (Num [Number]))

typeofM [] (Id "[Character]")

typeofM [] (Bind "[Character]" (Num [Number])||(Boolean [Boolean]) [Statement])

typeofM [] (If (Boolean [Boolean]) (Num [Number])||(Boolean [Boolean]) (Num [Number])||(Boolean [Boolean]))

typeofM [] ([Operator] (Num [Number])||(Boolean [Boolean]) (Num [Number])||(Boolean [Boolean]))

# In Project 4, by using Plus, Minus, Mult, or Div for the [Operator], and PlusX, MinusX, MultX, or DivX for the [OperatorX], you can do following (leave the empty square brackets as it is. It is necessary):
evalDyn [] (Num [Number])

evalDyn [] (Id "[Character]")

evalDyn [] (If0 (Num [Number]) (Num [Number]) (Num [Number]))

evalDyn [] (Lambda "[Character]" (Num [Number]))

evalDyn [] (App [Statement] [Statement])

evalDyn [] ([Operator] (Num [Number]) (Num [Number]))


evalStat [] (Num [Number])

evalStat [] (Id "[Character]")

evalStat [] (If0 (Num [Number]) (Num [Number]) (Num [Number]))

evalStat [] (Lambda "[Character]" (Num [Number]))

evalStat [] (App [Statement] [Statement])

evalStat [] ([Operator] (Num [Number]) (Num [Number]))


elabTerm (NumX [Number])

elabTerm (IdX "[Character]")

elabTerm (If0X (Num [Number]) (Num [Number]) (Num [Number]))

elabTerm (LambdaX "[Character]" (Num [Number]))

elabTerm (AppX [Statement] [Statement])

elabTerm (BindX "[Character]" (Num [Number]) [Statement])

elabTerm ([OperatorX] (Num [Number]) (Num [Number]))

