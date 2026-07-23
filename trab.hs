
-- Definição das árvore sintática para representação dos programas:

-- Expressões aritméticas:
{- HLINT ignore "Redundant ==" -}
{- HLINT ignore "Redundant bracket" -}
data E = Num Int
      |Var String
      |Soma E E
      |Sub E E
      |Mult E E
      |Div E E
   deriving(Eq,Show)

-- Expressões booleanas:
data B = TRUE
      | FALSE
      | Not B
      | And B B
      | Or  B B
      | Leq E E    -- menor ou igual
      | Igual E E  -- verifica se duas expressões aritméticas são iguais
   deriving(Eq,Show)

-- Comandos imperativos:
data C = While B C
    | If B C C
    | Seq C C
    | Atrib E E
    | Skip
    | TenTimes C   ---- Executa o comando C 10 vezes
    | Repeat C B --- Repeat C until B: executa C enquanto B é falso
    | Loop E E C      ---- Loop e1 e2 c: executa (e2 - e1) vezes o comando C 
    | DuplaATrib E E E E -- recebe 2 variáveis e 2 expressões (DuplaATrib (Var v1) (Var v2) e1 e2) e faz v1:=e1 e v2:=e2
    | AtribCond B E E E --- AtribCond b (Var v1) e1 e2: se b for verdade, então faz v1:e1, se B for falso faz v1:=e2
    | Swap E E -- swap(x,y): troca o conteúdo das variáveis x e y 
   deriving(Eq,Show)


-----------------------------------------------------
-----
----- As próximas funções, servem para manipular a memória (sigma)
-----
------------------------------------------------


--- A próxima linha de código diz que o tipo memória é equivalente a uma lista de tuplas, onde o
--- primeiro elemento da tupla é uma String (nome da variável) e o segundo um Inteiro
--- (conteúdo da variável):


type Memoria = [(String,Int)]

exSigma :: Memoria
exSigma = [ ("x", 10), ("temp",0), ("y",0)]


--- A função procuraVar recebe uma memória, o nome de uma variável e retorna o conteúdo
--- dessa variável na memória. Exemplo:
---
--- *Main> procuraVar exSigma "x"
--- 10


procuraVar :: Memoria -> String -> Int
procuraVar [] s = error ("Variavel " ++ s ++ " nao definida no estado")
procuraVar ((s,i):xs) v
  | s == v     = i
  | otherwise  = procuraVar xs v


--- A função mudaVar, recebe uma memória, o nome de uma variável e um novo conteúdo para essa
--- variável e devolve uma nova memória modificada com a varíável contendo o novo conteúdo. A
--- chamada
---
--- *Main> mudaVar exSigma "temp" 20
--- [("x",10),("temp",20),("y",0)]
---
---
--- essa chamada é equivalente a operação exSigma[temp->20]

mudaVar :: Memoria -> String -> Int -> Memoria
mudaVar [] v n = error ("Variavel " ++ v ++ " nao definida no estado")
mudaVar ((s,i):xs) v n
  | s == v     = (s,n):xs
  | otherwise  = (s,i): mudaVar xs v n


-------------------------------------
---
--- Completar os casos comentados das seguintes funções:
---
---------------------------------


ebigStep :: (E,Memoria) -> Int
ebigStep (Var x,s) = procuraVar s x
ebigStep (Num n,s) = n
ebigStep (Soma e1 e2,s)  = ebigStep (e1,s) + ebigStep (e2,s)
ebigStep (Sub e1 e2,s)  = ebigStep (e1,s) - ebigStep (e2,s)
ebigStep (Mult e1 e2,s)  = ebigStep (e1,s) * ebigStep (e2,s)
ebigStep (Div e1 e2,s)  = ebigStep (e1,s) `div` ebigStep (e2,s)


bbigStep :: (B,Memoria) -> Bool
bbigStep (TRUE,s)  = True
bbigStep (FALSE,s) = False

bbigStep (Not b,s)
   | bbigStep (b,s) == True     = False
   | otherwise                  = True

bbigStep (And b1 b2,s )
   | bbigStep (b1, s) == True = bbigStep (b2, s)
   | bbigStep (b1, s) == False = False

bbigStep (Or b1 b2,s )
   | bbigStep (b1, s) == True = True
   | bbigStep (b1, s) == False = bbigStep (b2, s)

bbigStep (Leq e1 e2,s) = ebigStep (e1,s) <= ebigStep (e2,s)

bbigStep (Igual e1 e2,s) = ebigStep (e1,s) == ebigStep (e2,s)


cbigStep :: (C,Memoria) -> (C,Memoria)
cbigStep (Skip,s) = (Skip,s)

-- If   --- avaliar B, desviar para c1 ou c2
cbigStep (If b c1 c2,s)
   | bbigStep (b, s) == True = cbigStep (c1, s)
   | otherwise              = cbigStep (c2, s)

-- Seq C1 C2 --- roda c1, captura o estado resultante s', roda c2 a partir de s'
cbigStep (Seq c1 c2,s) =
   let (_, s') = cbigStep (c1, s)
   in cbigStep (c2, s')

-- Atrib   --- avaliar E, chamar mudaVar, retornar (Skip, s')
cbigStep (Atrib (Var x) e,s) = (Skip, mudaVar s x (ebigStep (e, s)))

-- While    ---- se b é true, executa Seq(c, while); se false, skip
cbigStep(While b c, s)
   | bbigStep (b, s) == True = cbigStep (Seq c (While b c), s)
   | otherwise              = (Skip, s)

-- TenTimes C   ---- Executa o comando C 10 vezes
cbigStep (TenTimes c, s) =
   cbigStep (Seq c (Seq c (Seq c (Seq c (Seq c (Seq c (Seq c (Seq c (Seq c c)))))))), s)

-- Repeat C B --- Repeat C until B: executa C enquanto B é falso
cbigStep(Repeat c b, s) = cbigStep (Seq c (While (Not b) c), s)

-- Loop E E C      ---- Loop e1 e2 c: executa (e2 - e1) vezes o comando C
cbigStep (Loop e1 e2 c, s)
   | bbigStep (Leq e2 e1, s) = (Skip, s)          -- (e2 - e1) <= 0: para
   | otherwise =
       let (_, s') = cbigStep (c, s)
       in cbigStep (Loop (Soma e1 (Num 1)) e2 c, s')

-- DuplaATrib E E E E -- recebe 2 variáveis e 2 expressões (DuplaATrib (Var v1) (Var v2) e1 e2) e faz v1:=e1 e v2:=e2
cbigStep (DuplaATrib v1 v2 e1 e2, s) = cbigStep (Seq (Atrib v1 e1) (Atrib v2 e2), s)

--AtribCond B E E E --- AtribCond b (Var v1) e1 e2: se b for verdade, então faz v1:e1, se B for falso faz v1:=e2
cbigStep (AtribCond b v1 e1 e2, s) = cbigStep (If b (Atrib v1 e1) (Atrib v1 e2), s)

-- Swap E E -- swap(x,y): troca o conteúdo das variáveis x e y 
cbigStep (Swap (Var x) (Var y), s) =
  let vx = procuraVar s x
      vy = procuraVar s y
  in (Skip, mudaVar (mudaVar s x vy) y vx)

--------------------------------------
---
--- Exemplos de programas para teste
---
--- O ALUNO DEVE IMPLEMENTAR EXEMPLOS DE PROGRAMAS QUE USEM:
--- * Loop
--- * Dupla Atribuição
--- * Repeat until
--- * swap
--- * atrib cond
-------------------------------------

exSigma2 :: Memoria
exSigma2 = [("x",3), ("y",0), ("z",0)]


---
--- O progExp1 é um programa que usa apenas a semântica das expressões aritméticas. Esse
--- programa já é possível rodar com a implementação inicial  fornecida:

progExp1 :: E
progExp1 = Soma (Num 3) (Soma (Var "x") (Var "y"))

---
--- para rodar:
-- *Main> ebigStep (progExp1, exSigma)
-- 13
-- *Main> ebigStep (progExp1, exSigma2)
-- 6

--- Para rodar os próximos programas é necessário primeiro implementar as regras da semântica
---


---
--- Exemplos de expressões booleanas:


teste1 :: B
teste1 = (Leq (Soma (Num 3) (Num 3))  (Mult (Num 2) (Num 3)))

teste2 :: B
teste2 = (Leq (Soma (Var "x") (Num 3))  (Mult (Num 2) (Num 3)))


---
-- Exemplos de Programas Imperativos:

testec1 :: C
testec1 = (Seq (Seq (Atrib (Var "z") (Var "x")) (Atrib (Var "x") (Var "y")))
               (Atrib (Var "y") (Var "z")))

fatorial :: C
fatorial = (Seq (Atrib (Var "y") (Num 1))
                (While (Not (Igual (Var "x") (Num 1)))
                       (Seq (Atrib (Var "y") (Mult (Var "y") (Var "x")))
                            (Atrib (Var "x") (Sub (Var "x") (Num 1))))))



-- Exemplos de programas para teste

--- cbigStep (exLoop, memLoop)
--- (Skip,[("soma",10),("n",5)])
memLoop :: Memoria
memLoop = [("soma",0), ("n",5)]

exLoop :: C
exLoop = Loop (Num 0) (Var "n")
              (Atrib (Var "soma") (Soma (Var "soma") (Num 2)))


--- cbigStep (exDupla, memDupla)
--- (Skip,[("x",10),("y",11)])
memDupla :: Memoria
memDupla = [("x",1), ("y",0)]

exDupla :: C
exDupla = DuplaATrib (Var "x") (Var "y")
                     (Num 10)
                     (Soma (Var "x") (Num 1))


--- cbigStep (exRepeat, memRepeat)
--- (Skip,[("x",5)])
memRepeat :: Memoria
memRepeat = [("x",0)]

exRepeat :: C
exRepeat = Repeat (Atrib (Var "x") (Soma (Var "x") (Num 1)))
                  (Igual (Var "x") (Num 5))


--- cbigStep (exSwap, memSwap)
--- (Skip,[("x",2),("y",1)])
memSwap :: Memoria
memSwap = [("x",1), ("y",2)]

exSwap :: C
exSwap = Swap (Var "x") (Var "y")


--- cbigStep (exAtribCond, memCond)
--- (Skip,[("x",3),("y",8),("maior",8)])
memCond :: Memoria
memCond = [("x",3), ("y",8), ("maior",0)]

exAtribCond :: C
exAtribCond = AtribCond (Leq (Var "x") (Var "y"))
                        (Var "maior")
                        (Var "y")
                        (Var "x")