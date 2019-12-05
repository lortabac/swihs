# Swihs

Call SWI-Prolog from Haskell.

```
>>> :set -XOverloadedStrings
>>> initSwipl
True
>>> fruit = F1 "fruit"
>>> eats = F2 "eats"
>>> assertz $ fruit "apple"
>>> assertz $ fruit "pear"
>>> assertz $ eats "john" "X" :- fruit "X"
>>> queryList $ eats "john" "X"
[fromList [("X",Atom "apple")],fromList [("X",Atom "pear")]]
```
