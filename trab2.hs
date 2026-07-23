
-- Definição das árvore sintática para representação dos programas:

data E = Num Int
      |Var String
      |Soma E E
      |Sub E E
      |Mult E E
      |Div E E
   deriving(Eq,Show)

data B = TRUE
      | FALSE
      | Not B
      | And B B
      | Or  B B
      | Leq E E
      | Igual E E  -- verifica se duas expressões aritméticas são iguais
   deriving(Eq,Show)

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
  | s == v     = ((s,n):xs)
  | otherwise  = (s,i): mudaVar xs v n


-------------------------------------
---
--- Completar os casos comentados das seguintes funções:
---
---------------------------------

smallStepE :: (E, Memoria) -> (E, Memoria)
-- VAR
smallStepE (Var x, s)                  = (Num (procuraVar s x), s)

-- SOMA1, SOMA2, SOMA3
smallStepE (Soma (Num n1) (Num n2), s) = (Num (n1 + n2), s)
smallStepE (Soma (Num n) e, s)         = let (el,sl) = smallStepE (e,s)
                                         in (Soma (Num n) el, sl)
smallStepE (Soma e1 e2,s)              = let (el,sl) = smallStepE (e1,s)
                                         in (Soma el e2,sl)

-- MULT1, MULT2, MULT3
smallStepE (Mult (Num n1) (Num n2), s) = (Num (n1 * n2), s)
smallStepE (Mult (Num n) e, s)         = let (el,sl) = smallStepE (e,s)
                                         in (Mult (Num n) el, sl)
smallStepE (Mult e1 e2,s)              = let (el,sl) = smallStepE (e1,s)
                                         in (Mult el e2,sl)

-- SUB1, SUB2, SUB3
smallStepE (Sub (Num n1) (Num n2), s)  = (Num (n1 - n2), s)
smallStepE (Sub (Num n) e, s)          = let (el,sl) = smallStepE (e,s)
                                         in (Sub (Num n) el, sl)
smallStepE (Sub e1 e2,s)               = let (el,sl) = smallStepE (e1,s)
                                         in (Sub el e2,sl)

-- DIV1, DIV2, DIV3
smallStepE (Div (Num n1) (Num n2), s)  = (Num (n1 `div` n2), s)
smallStepE (Div (Num n) e, s)          = let (el,sl) = smallStepE (e,s)
                                         in (Div (Num n) el, sl)
smallStepE (Div e1 e2,s)               = let (el,sl) = smallStepE (e1,s)
                                         in (Div el e2,sl)


smallStepB :: (B,Memoria) -> (B, Memoria)

-- NOT1, NOT2, NOT3
smallStepB (Not TRUE, s)   = (FALSE, s)
smallStepB (Not FALSE, s)  = (TRUE, s)
smallStepB (Not b, s)      = let (bl,sl) = smallStepB (b,s)
                             in (Not bl, sl)

-- AND1, AND2, AND3
smallStepB (And TRUE b, s)  = (b, s)
smallStepB (And FALSE b, s) = (FALSE, s)
smallStepB (And b1 b2, s)   = let (b1l,sl) = smallStepB (b1,s)
                              in (And b1l b2, sl)

-- OR1, OR2, OR3
smallStepB (Or TRUE b, s)  = (TRUE, s)
smallStepB (Or FALSE b, s) = (b, s)
smallStepB (Or b1 b2, s)   = let (b1l,sl) = smallStepB (b1,s)
                             in (Or b1l b2, sl)

-- LEQ1, LEQ2, LEQ3
smallStepB (Leq (Num n1) (Num n2), s) = (if n1 <= n2 then TRUE else FALSE, s)
smallStepB (Leq (Num n) e, s)         = let (el,sl) = smallStepE (e,s)
                                        in (Leq (Num n) el, sl)
smallStepB (Leq e1 e2, s)             = let (el,sl) = smallStepE (e1,s)
                                        in (Leq el e2, sl)

-- IGUAL
smallStepB (Igual (Num n1) (Num n2), s) = (if n1 == n2 then TRUE else FALSE, s)
smallStepB (Igual (Num n) e, s)         = let (el,sl) = smallStepE (e,s)
                                          in (Igual (Num n) el, sl)
smallStepB (Igual e1 e2, s)             = let (el,sl) = smallStepE (e1,s)
                                          in (Igual el e2, sl)


smallStepC :: (C,Memoria) -> (C,Memoria)

-- IF1, IF2, IF3
smallStepC (If TRUE c1 c2, s)  = (c1, s)
smallStepC (If FALSE c1 c2, s) = (c2, s)
smallStepC (If b c1 c2, s)     = let (bl,sl) = smallStepB (b,s)
                                 in (If bl c1 c2, sl)

-- SEQ1, SEQ2
smallStepC (Seq Skip c2, s) = (c2, s)
smallStepC (Seq c1 c2, s)   = let (c1l,sl) = smallStepC (c1,s)
                              in (Seq c1l c2, sl) 

-- ATRIB1, ATRIB2
smallStepC (Atrib (Var x) (Num n), s) = (Skip, mudaVar s x n)
smallStepC (Atrib (Var x) e, s)       = let (el,sl) = smallStepE (e,s)
                                        in (Atrib (Var x) el, sl)

-- WHILE
smallStepC (While b c, s) = (If b (Seq c (While b c)) Skip, s)


-- TenTimes C   ---- Executa o comando C 10 vezes
smallStepC (TenTimes c, s) = (Loop (Num 0) (Num 10) c, s)

-- Repeat C B --- Repeat C until B: executa C enquanto B é falso
smallStepC (Repeat c b, s) = (Seq c (If b Skip (Repeat c b)), s)

-- Loop E E C      ---- Loop e1 e2 c: executa (e2 - e1) vezes o comando C 
smallStepC (Loop (Num n1) (Num n2) c, s)
    | n1 >= n2  = (Skip, s)
    | otherwise = (Seq c (Loop (Num (n1+1)) (Num n2) c), s)
smallStepC (Loop (Num n1) e2 c, s) =
    let (e2l,sl) = smallStepE (e2,s)
    in (Loop (Num n1) e2l c, sl)
smallStepC (Loop e1 e2 c, s) =
    let (e1l,sl) = smallStepE (e1,s)
    in (Loop e1l e2 c, sl)

-- DuplaATrib E E E E -- recebe 2 variáveis e 2 expressões (DuplaATrib (Var v1) (Var v2) e1 e2) e faz v1:=e1 e v2:=e2
smallStepC (DuplaATrib (Var v1) (Var v2) (Num n1) (Num n2), s) =
    (Skip, mudaVar (mudaVar s v1 n1) v2 n2)
smallStepC (DuplaATrib (Var v1) (Var v2) (Num n1) e2, s) =
    let (e2l,sl) = smallStepE (e2,s)
    in (DuplaATrib (Var v1) (Var v2) (Num n1) e2l, sl)
smallStepC (DuplaATrib (Var v1) (Var v2) e1 e2, s) =
    let (e1l,sl) = smallStepE (e1,s)
    in (DuplaATrib (Var v1) (Var v2) e1l e2, sl)

-- AtribCond B E E E --- AtribCond b (Var v1) e1 e2: se b for verdade, então faz v1:e1, se B for falso faz v1:=e2
smallStepC (AtribCond TRUE  (Var v) e1 e2, s) = (Atrib (Var v) e1, s)
smallStepC (AtribCond FALSE (Var v) e1 e2, s) = (Atrib (Var v) e2, s)
smallStepC (AtribCond b (Var v) e1 e2, s) =
    let (bl,sl) = smallStepB (b,s)
    in (AtribCond bl (Var v) e1 e2, sl)

-- Swap E E -- swap(x,y): troca o conteúdo das variáveis x e y 
smallStepC (Swap (Var x) (Var y), s) =
    (DuplaATrib (Var x) (Var y) (Var y) (Var x), s)

----------------------
--  INTERPRETADORES
----------------------


--- Interpretador para Expressões Aritméticas:
isFinalE :: E -> Bool
isFinalE (Num n) = True
isFinalE _       = False


interpretadorE :: (E,Memoria) -> (E, Memoria)
interpretadorE (e,s) = if (isFinalE e) then (e,s) else interpretadorE (smallStepE (e,s))

--- Interpretador para expressões booleanas


isFinalB :: B -> Bool
isFinalB TRUE    = True
isFinalB FALSE   = True
isFinalB _       = False

-- Descomentar quanto a função smallStepB estiver implementada:

interpretadorB :: (B,Memoria) -> (B, Memoria)
interpretadorB (b,s) = if (isFinalB b) then (b,s) else interpretadorB (smallStepB (b,s))


-- Interpretador da Linguagem Imperativa

isFinalC :: C -> Bool
isFinalC Skip    = True
isFinalC _       = False

-- Descomentar quando a função smallStepC estiver implementada:

interpretadorC :: (C,Memoria) -> (C, Memoria)
interpretadorC (c,s) = if (isFinalC c) then (c,s) else interpretadorC (smallStepC (c,s))


--------------------------------------
---
--- Exemplos de programas para teste
---
--- O ALUNO DEVE IMPLEMENTAR EXEMPLOS DE PROGRAMAS QUE USEM 
--- OS COMANDOS NOVOS PRINCIPALMENTE O TRATAMENTO DE EXCEÇÕES
--

exSigma2 :: Memoria
exSigma2 = [("x",3), ("y",0), ("z",0)]


---
--- O progExp1 é um programa que usa apenas a semântica das expressões aritméticas. Esse
--- programa já é possível rodar com a implementação que fornecida:

progExp1 :: E
progExp1 = Soma (Num 3) (Soma (Var "x") (Var "y"))

---
--- para rodar:
-- A função smallStepE anda apenas um passo na avaliação da Expressão

-- *Main> smallStepE (progExp1, exSigma)
-- (Soma (Num 3) (Soma (Num 10) (Var "y")),[("x",10),("temp",0),("y",0)])

-- Note que no exemplo anterior, o (Var "x") foi substituido por (Num 10)

-- Para avaliar a expressão até o final, deve-se usar o interpretadorE:

-- *Main> interpretadorE (progExp1 , exSigma)
-- (Num 13,[("x",10),("temp",0),("y",0)])

-- *Main> interpretadorE (progExp1 , exSigma2)
-- (Num 6,[("x",3),("y",0),("z",0)])


--- Para rodar os próximos programas é necessário primeiro implementar as regras que faltam
--- e descomentar os respectivos interpretadores


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

--- Memória auxiliar para os testes, com variáveis extras usadas abaixo
exSigma3 :: Memoria
exSigma3 = [("x",3), ("y",7), ("z",0), ("cont",0)]


--- *Main> interpretadorC (testeSwap, exSigma3)
--- esperado: x=7, y=3, z=0, cont=0
testeSwap :: C
testeSwap = Swap (Var "x") (Var "y")


--- *Main> interpretadorC (testeDuplaATrib, exSigma3)
--- esperado: x=6, y=7, z=10, cont=0
testeDuplaATrib :: C
testeDuplaATrib = DuplaATrib (Var "z") (Var "x")
                             (Soma (Var "x") (Var "y"))
                             (Sub (Var "y") (Num 1))


--- *Main> interpretadorC (testeAtribCond, exSigma3)
--- esperado: x=3, y=7, z=1, cont=0
testeAtribCond :: C
testeAtribCond = AtribCond (Leq (Var "x") (Var "y")) (Var "z") (Num 1) (Num 0)


--- *Main> interpretadorC (testeTenTimes, exSigma3)
--- esperado: x=3, y=7, z=0, cont=10
testeTenTimes :: C
testeTenTimes = TenTimes (Atrib (Var "cont") (Soma (Var "cont") (Num 1)))


--- *Main> interpretadorC (testeLoop, exSigma3)
--- esperado: x=3, y=7, z=0, cont=3
testeLoop :: C
testeLoop = Loop (Num 2) (Num 5) (Atrib (Var "cont") (Soma (Var "cont") (Num 1)))


--- *Main> interpretadorC (testeRepeat, exSigma3)
--- esperado: x=3, y=7, z=0, cont=5
testeRepeat :: C
testeRepeat = Repeat (Atrib (Var "cont") (Soma (Var "cont") (Num 1)))
                     (Igual (Var "cont") (Num 5))