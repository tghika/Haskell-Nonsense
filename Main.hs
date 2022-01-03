main :: IO ()
main = do

  myDup <- return $ dup
  -- Duplicating 0 -> (0,0)
  putStr ">> dup 0 = "
  printEl $ el(0) -: myDup

  -- Duplicating 1 -> (1,1)
  putStr ">> dup 1 = "
  printEl $ el(1) -: myDup


  myAdd <- return $ (recurs(curry(prj2), curry(ev -: succ))***id)-:ev
  -- 9 + 2 = 11
  putStr ">> 9+2 = "
  printEl $ el(9,2) -: myAdd

  -- 2 + 9 = 11
  putStr ">> 2+9 = "
  printEl $ el(2,9) -: myAdd


  myMul <- return $ (recurs(curry(prj2-:termArr-:el(0)), curry(pair(ev,prj2)-:myAdd))***id)-:ev
  -- 9 * 2 = 18
  putStr ">> 9*2 = "
  printEl $ el(9,2) -: myMul

  -- 2 * 9 = 18
  putStr ">> 2*9 = "
  printEl $ el(2,9) -: myMul


  myFact <- return $ (recurs(el(0,1), pair(prj1-:succ, (succ***id)-:myMul)))-:prj2
  -- 0! = 1
  putStr ">> 0! = "
  printEl $ el(0) -: myFact

  -- 5! = 120
  putStr ">> 5! = "
  printEl $ el(5) -: myFact



--------------
-- Nonsense --
--------------

-- X の要素を圏論に倣って終対象から X への射(Global element)として扱うための関数
el::a -> (() -> a)
el = (const::a -> (() -> a))

-- Global elements 用 ユーティリティ
(===) :: Eq a =>  (() -> a) -> (() -> a) -> Bool
(===) x y = (x() == y())

showEl :: Show a => (() -> a) -> String
showEl x = (show $ x()) 

printEl :: Show a => (() -> a) -> IO ()
printEl = putStrLn . showEl

-- Diagrammatic-order な射の合成演算
(-:) = flip (.)

-- 終対象への一意的な射 !:X->1
-- (圏論的には定数関数 const は逆にこの射 ! を使って定義される)
termArr = const ()

-- 余積対象と積対象
type Coprod a b = Either a b
type Prod   a b = (a,b)

-- 入射
inj1 = Left
inj2 = Right

-- 射影
prj1 = fst
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
coTw = coPair(inj2, inj1)

-- Twist
tw = pair(prj2, prj1)

-- 評価射
ev = uncurry id

-- recursion data q:1->A and f:A->A から recurs(q, f) を構成する関数
-- (疑似的な自然数対象の仲介射)
recurs = ((flip ($) ()) *** id) -: uncurry (curry ((!!).(uncurry.flip $ iterate)))
