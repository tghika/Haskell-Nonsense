import Data.Void

main :: IO ()
main = do

  putStr ">> 0 = "
  printEl $ nat(0)
  putStr ">> 1 = "
  printEl $ nat(1)
  putStr ">> 2 = "
  printEl $ nat(2)
  
  putStr ">> True && False = "
  printEl $ pair(true,false) -: and'
  putStr ">> True || False = "
  printEl $ pair(true,false) -: or'
  putStrLn "    (True:=inj1, False:=inj2)"

  myDup <- return $ dup
  -- Duplicating 0 -> (0,0)
  putStr ">> dup 0 = "
  printEl $ nat(0) -: myDup

  -- Duplicating 1 -> (1,1)
  putStr ">> dup 1 = "
  printEl $ pair(nameOf(myDup), nat(1)) -: ev


  myAdd <- return $ (recurs(nameOf(id), trans(ev-:succ'))***id)-:ev
  -- 9 + 2 = 11
  putStr ">> 9+2 = "
  printEl $ pair(nat(9),nat(2)) -: myAdd

  -- 2 + 9 = 11
  putStr ">> 2+9 = "
  printEl $ pair(nat(2),nat(9)) -: myAdd

  
  myAdd29 <- return $ pair(id,termArr-:nat(29))-:myAdd
  -- ((-)+29)(2) = 31
  putStr ">> ((-)+29)(2) = "
  printEl $ nat(2) -: myAdd29

  -- ((-)+29)(9) = 38
  putStr ">> ((-)+29)(9) = "
  printEl $ nat(9) -: myAdd29


  myMul <- return $ (recurs(nameOf(termArr-:nat(0)), trans(pair(ev,prj2)-:myAdd))***id)-:ev
  -- 9 * 2 = 18
  putStr ">> 9*2 = "
  printEl $ pair(nat(9),nat(2)) -: myMul

  -- 2 * 9 = 18
  putStr ">> 2*9 = "
  printEl $ pair(nat(2),nat(9)) -: myMul

  
  mySq <- return $ dup-:myMul
  -- 2 ^ 2 = 4
  putStr ">> 2^2 = "
  printEl $ nat(2) -: mySq

  -- 9 ^ 2 = 81
  putStr ">> 9^2 = "
  printEl $ nat(9) -: mySq


  mySumOfSq <- return $ (mySq***mySq) -: myAdd
  -- 12^2+19^2 = 505
  putStr ">> 12^2+19^2 = "
  printEl $ pair(nat(12),nat(19)) -: mySumOfSq


  myFact <- return $ (recurs(pair(nat(0),nat(1)), pair(prj1-:succ', (succ'***id)-:myMul)))-:prj2
  -- 0! = 1
  putStr ">> 0! = "
  printEl $ nat(0) -: myFact

  -- 5! = 120
  putStr ">> 5! = "
  printEl $ nat(5) -: myFact


  someFnc1 <- return $ coPair(id,myAdd)
  -- coPair(id,+) $ (0;inj1)     = 0
  putStr ">> coPair(id,+)$(0;inj1) = "
  printEl $ (nat(0) -: inj1)              -: someFnc1
  -- coPair(id,+) $ ((1,2);inj2) = 3 
  putStr ">> coPair(id,+)$((1,2);inj2) = "
  printEl $ (pair(nat(1),nat(2)) -: inj2) -: someFnc1
  
  
  someFnc2 <- return $ pair(pair(termArr-:nat(0),id)-:eq', pair(termArr-:nat(1)-:dup,pair(termArr-:nat(0),id)))-:if'
  -- (if [INPUT]==0 then (1,1) else (0,[INPUT]))(0) = (1,1)
  putStr ">> (if [INPUT]==0 then (1,1) else (0,[INPUT]))(0) = "
  printEl $ nat(0) -: someFnc2
  -- (if [INPUT]==0 then (1,1) else (0,[INPUT]))(2) = (0,2)
  putStr ">> (if [INPUT]==0 then (1,1) else (0,[INPUT]))(2) = "
  printEl $ nat(2) -: someFnc2
  

--------------
-- Nonsense --
--------------

class MyShow a where
  myShow :: a -> String
  
instance MyShow () where
  myShow _ = "*"
  
instance (MyShow a, MyShow b) => MyShow (Either a b) where
  myShow x = case x of
    Left z -> if (myShow z == "*") then "inj1" else (myShow z) ++ ";inj1"
    Right z -> if (myShow z == "*") then "inj2" else (myShow z) ++ ";inj2"

instance (MyShow a, MyShow b) => MyShow (a,b) where
  myShow (x,y) = "(" ++ myShow x ++ "," ++ myShow y ++ ")"
  
instance MyShow Int where
  myShow = show
  
instance MyShow Nat where
  myShow (Nat i) = myShow (length i)
  --myShow (Nat i) = "zero" ++ (foldr ((++).(const ";succ")) [] i)


-- X の要素を圏論に倣って終対象から X への射(Global element)として扱うための関数
el::a -> (Pt -> a)
el = (const::a -> (Pt -> a))


-- Global elements 用 ユーティリティ
(===) :: Eq a =>  (Pt -> a) -> (Pt -> a) -> Bool
(===) x y = (x() == y())

showEl :: MyShow a => (Pt -> a) -> String
showEl x = (myShow $ x()) 

printEl :: MyShow a => (Pt -> a) -> IO ()
printEl = putStrLn . showEl


-- Diagrammatic-order な射の合成演算
(-:) = flip (.)


-- # 始対象と終対象

type Empty = Void
type Pt = ()

-- 始対象からの一意的な射 !:∅->X (始対象の仲介射)
initArr :: Empty -> a
initArr = absurd

-- 終対象への一意的な射 !:X->1 (終対象の仲介射)
-- (圏論的には定値関数 const は逆にこの射 ! を使って定義される)
termArr :: a -> Pt
termArr = const ()


-- # 余積対象と積対象
type Coprod a b = Either a b
type Prod   a b = (a,b)

-- 入射
inj1 :: a -> Coprod a b
inj1 = Left

inj2 :: b -> Coprod a b
inj2 = Right

-- 射影
prj1 :: Prod a b -> a
prj1 = fst

prj2 :: Prod a b -> b
prj2 = snd

-- 余積対象の仲介射
coPair :: (a -> c, b -> c) -> (Coprod a b -> c)
coPair = uncurry either

-- 積対象の仲介射
pair   :: (c -> a, c -> b) -> (c -> Prod a b)
pair = uncurry $ (<*>) . fmap (,)

-- 畳み込み
fol = coPair(id, id)

-- 対角射
dup = pair(id, id)

-- 射同士の余積
(+++) :: (a1 -> b1) -> (a2 -> b2) -> (Coprod a1 a2 -> Coprod b1 b2)
(+++) f g = coPair(f -: inj1 , g -: inj2)

-- 射同士の積
(***) :: (a1 -> b1) -> (a2 -> b2) -> (Prod   a1 a2 -> Prod   b1 b2)
(***) f g =   pair(prj1 -: f, prj2 -: g)

-- Twist の形式的双対
coTw :: Coprod a b -> Coprod b a
coTw = coPair(inj2, inj1)

-- Twist
tw :: Prod a b -> Prod b a
tw = pair(prj2, prj1)


-- # Exponential 対象
type Exp b a = a -> b

-- 評価射
-- (圏論的には uncurry という操作は逆にこの射 ev を使って実現される)
ev :: Prod (Exp b a) a -> b
ev = uncurry id

-- 射の転置 (transpose) の構成
-- (Exponential 対象の仲介射)
trans :: (Prod c a -> b) -> (c -> Exp b a)
trans = curry

-- 射 h:a->b の Exponential 対象 (Exp b a) の要素への変換 
nameOf :: (a -> b) -> (Pt -> Exp b a)
nameOf h = trans(prj2-:h)


-- # 自然数対象 (NNO)
data Nat = Nat{imp::[()]} deriving Eq

zero :: Pt -> Nat
zero = el (Nat [])

succ' :: Nat -> Nat
succ' (Nat i) = Nat (():i)

-- 整数リテラルを使って NNO の Global elements としての自然数を得るための小細工
nat :: Int -> (Pt -> Nat)
nat i = zero -: (foldr (.) id (replicate i succ'))

-- recursion data x_0:1->X と f:X->X から recurs(x_0, f):Nat->X を構成する関数
-- (自然数対象の仲介射)
recurs :: (Pt -> a, a -> a) -> (Nat -> a)
recurs = ((flip ($) ())***id)-:((curry((id***(length.imp))-:uncurry(!!))).(uncurry.flip $ iterate))


-- # 2の値を持つ型
type Bool'= Coprod Pt Pt

true :: Pt -> Bool'
true = inj1

false :: Pt -> Bool'
false = inj2

if' :: Prod Bool' (Prod a a) -> a
if' = (coPair(nameOf(prj1), nameOf(prj2))***id)-:ev

-- Equality
-- トポスであれば、Equality はモニック射である対角射の分類射を使って導入する
eq' :: Eq a => Prod a a -> Bool'
eq' (a,b) = case (a==b) of
  True -> Left ()
  False -> Right ()

not' :: Bool' -> Bool'
not' x = case x of
  Left ()  -> Right ()
  Right () -> Left ()

-- And
-- トポスであれば、And はモニック射である <true,true>:1->Ω×Ω の分類射 χ[<true,true>]:Ω×Ω->Ω を使って導入する
and' :: Prod Bool' Bool' -> Bool'
and' x = case x of
  (Left (), Left ()) -> Left ()
  _ -> Right ()

-- Or
or' :: Prod Bool' Bool' -> Bool'
or' = (not'***not')-:and'-:not'
