{-# LANGUAGE ScopedTypeVariables #-}  -- Включаем некоторые расширения компилятора.
import Test.Tasty
import Test.Tasty.HUnit
import Data.Proxy
import Map
import qualified Data.Map.Strict as SMap
import MapInstance
import NaiveList(NaiveList)  -- Импортируем только тип NaiveList, но не его конструкторы Nil/Cons, чтобы не путались с конструкторами NaiveTree.
import NaiveTree

main :: IO ()
main = defaultMain testMap

{-|
  Генерирует группу тестов для конкретной реализации 'Map'
  с определённым именем.

  Мы хотим писать тесты один раз для всех возможных реализаций 'Map'.
  В чистом Haskell нам может помочь параметрический полиморфизм,
  но для этого нужно, чтобы в сигнатуре функции присутствовал
  тип из класса 'Map', который мы хотим протестировать.

  Специально для этих целей существует обёртка 'Data.Proxy', он
  позволяет передавать в функции даже типы высшего порядка.
-}
mapTests :: Map m => String -> Proxy m -> TestTree
mapTests name (_ :: Proxy m) =
    -- Чтобы можно было связать типовую переменную m здесь и в let ниже, нужно расширение ScopedTypeVariables.
    testGroup name [
        testGroup "Test empty, singleton" [
            testCase "empty" $
                let map = empty :: m Int String in
                Map.null map @?= True,

            testCase "singleton" $
                let map = singleton 12 "Dujina" :: m Int String in
                Map.size map @?= 1
        ],

        testGroup "Test fromList" [
            testCase "on empty map" $
                let map = fromList [] :: m Int String in
                Map.null map @?= True,

            testCase "on map" $
                let map = Map.fromList [(2, "a"), (1, "b"), (3, "c"), (1, "x")] :: m Int String in
                True @?= ( Map.size map == 3 &&
                           Map.lookup 1 map == Just "x" &&
                           Map.lookup 2 map == Just "a" &&
                           Map.lookup 3 map == Just "c" ),

            testCase "toAscList . fromList sorts list" $
                let map = Map.fromList [(2, "a"), (1, "b"), (3, "c"), (1, "x")] :: m Int String in
                Map.toAscList map @?= [(1, "x"), (2, "a"), (3, "c")]
        ],


        testGroup "Test insert" [
            testCase "into empty map" $
                let map = empty :: m Int String in
                let map' = Map.insert 2 "B" map in
                Map.lookup 2 map' @?= Just "B",

            testCase "changes value" $
                let map = singleton 2 "B" :: m Int String in
                let map' = Map.insert 2 "toB" map in
                Map.lookup 2 map' @?= Just "toB"
        ],

        testGroup "Test insertWith" [
            testCase "into empty map" $
                let map = empty :: m Int String in
                let map' = Map.insertWith (const $ const "wtf") 2 "B" map in
                Map.lookup 2 map' @?= Just "B",

            testCase "existing values" $
                let map = singleton 2 "B" :: m Int String in
                let map' = Map.insertWith (++) 2 "to" map in
                Map.lookup 2 map' @?= Just "toB"
        ],

        testGroup "Test insertWithKey" [
            testCase "into empty map" $
                let map = empty :: m Int String in
                    let map' = Map.insertWithKey (\k new old -> (show k) ++ new ++ old) 2 "B" map in
                Map.lookup 2 map' @?= Just "B",

            testCase "existing value" $
                let map = singleton 2 "B" :: m Int String in
                    let map' = Map.insertWithKey (\k new old -> (show k) ++ new ++ old) 2 "to" map in
                Map.lookup 2 map' @?= Just "2toB"
        ],

        testGroup "Test delete" [
            testCase "from empty map" $
                let map = empty :: m Int String in
                let map' = Map.delete 2 map in
                Map.null map' @?= True,

            testCase "not existing key" $
                let map = singleton 2 "B" :: m Int String in
                let map' = Map.delete 3 map in
                True @?= ( Map.size map' == 1 &&
                           Map.lookup 2 map' == Just "B" &&
                           Map.lookup 3 map' == Nothing ),

            testCase "key" $
                let map = Map.fromList [(3, "C"), (2, "B")] :: m Int String in
                let map' = Map.delete 3 map in
                True @?= ( Map.size map' == 1 &&
                           Map.lookup 2 map' == Just "B" &&
                           Map.lookup 3 map' == Nothing )
        ],

        testGroup "Test adjust" [
            testCase "on empty map" $
                let map = empty :: m Int String in
                let map' = Map.adjust ("to" ++) 2 map in
                Map.null map' @?= True,

            testCase "not existing key" $
                let map = singleton 2 "B" :: m Int String in
                let map' = Map.adjust ("to" ++) 3 map in
                True @?= ( Map.size map' == 1 &&
                           Map.lookup 2 map' == Just "B"),

            testCase "existing key" $
                let map = singleton 2 "B" :: m Int String in
                let map' = Map.adjust ("to" ++) 2 map in
                True @?= ( Map.size map' == 1 &&
                           Map.lookup 2 map' == Just "toB")
        ],

        testGroup "Test adjustWithKey" [
            testCase "on empty map" $
                let map = empty :: m Int String in
                let map' = Map.adjustWithKey (\k x -> show k ++ "to" ++ x) 2 map in
                Map.null map' @?= True,

            testCase "not existing key" $
                let map = singleton 2 "B" :: m Int String in
                let map' = Map.adjustWithKey (\k x -> show k ++ "to" ++ x) 3 map in
                True @?= ( Map.size map' == 1 &&
                           Map.lookup 2 map' == Just "B"),

            testCase "existing key" $
                let map = singleton 2 "B" :: m Int String in
                let map' = Map.adjustWithKey (\k x -> show k ++ "to" ++ x) 2 map in
                True @?= ( Map.size map' == 1 &&
                           Map.lookup 2 map' == Just "2toB")
        ],

        testGroup "Test update" [
            testCase "on empty map" $
                let map = empty :: m Int String in
                let map' = Map.update (\x -> if x == "B" then Just "toB" else Nothing) 2 map in
                Map.null map' @?= True,

            testCase "not existing key" $
                let map = singleton 2 "B" :: m Int String in
                let map' = Map.update (\x -> if x == "B" then Just "toB" else Nothing) 3 map in
                True @?= ( Map.size map' == 1 &&
                           Map.lookup 2 map' == Just "B"),

            testCase "existing key" $
                let map = singleton 2 "B" :: m Int String in
                let map' = Map.update (\x -> if x == "B" then Just "toB" else Nothing) 2 map in
                True @?= ( Map.size map' == 1 &&
                           Map.lookup 2 map' == Just "toB"),

            testCase "if func = Nothing" $
                let map = singleton 2 "not B" :: m Int String in
                let map' = Map.update (\x -> if x == "B" then Just "toB" else Nothing) 2 map in
                True @?= ( Map.size map' == 0 &&
                           Map.lookup 2 map' == Nothing)
        ],

        testGroup "Test updateWithKey" [
            testCase "on empty map" $
                let map = empty :: m Int String in
                let map' = Map.updateWithKey (\k x -> if x == "B" then Just (show k ++ "toB") else Nothing) 2 map in
                Map.null map' @?= True,

            testCase "not existing key" $
                let map = singleton 2 "B" :: m Int String in
                let map' = Map.updateWithKey (\k x -> if x == "B" then Just (show k ++ "toB") else Nothing) 3 map in
                True @?= ( Map.size map' == 1 &&
                           Map.lookup 2 map' == Just "B"),

            testCase "existing key" $
                let map = singleton 2 "B" :: m Int String in
                let map' = Map.updateWithKey (\k x -> if x == "B" then Just (show k ++ "toB") else Nothing) 2 map in
                True @?= ( Map.size map' == 1 &&
                           Map.lookup 2 map' == Just "2toB")
        ],

        testGroup "Test member/notMember" [
            testCase "on empty map" $
                let map = empty :: m Int String in
                Map.member 2 map @?= False,

            testCase "not existing key" $
                let map = singleton 2 "B" :: m Int String in
                Map.member 3 map @?= False,

            testCase "existing key" $
                let map = singleton 2 "B" :: m Int String in
                Map.member 2 map @?= True,

            testCase "(notMember) on empty map" $
                let map = empty :: m Int String in
                Map.notMember 2 map @?= True,

            testCase "(notMember) not existing key" $
                let map = singleton 2 "B" :: m Int String in
                Map.notMember 3 map @?= True,

            testCase "(notMember) existing key" $
                let map = singleton 2 "B" :: m Int String in
                Map.notMember 2 map @?= False
        ],

        testGroup "Test alter, lookup" [
            testCase "into empty map" $
                let map = empty :: m Int String in
                let map' = Map.alter (const $ Just "A") 1 map in
                Map.lookup 1 map' @?= Just "A",

            testCase "into singleton map" $
                let map = singleton 2 "B" :: m Int String in
                let map' = Map.alter (const $ Just "C") 3 map in
                True @?= ( Map.lookup 2 map' == Just "B" &&
                           Map.lookup 3 map' == Just "C" ),

            testCase "into singleton map" $
                let map = singleton 2 "B" :: m Int String in
                let map' = Map.alter (const $ Just "Y") 2 map in
                Map.lookup 2 map' @?= Just "Y",

            testCase "element that doesn't exist" $
                let map = singleton 2 "B" :: m Int String in
                let map' = Map.alter (const Nothing) 3 map in
                True @?= ( Map.lookup 1 map' == Nothing &&
                           Map.lookup 2 map' == Just "B" ),

            testCase "delete/alter" $
                let map = singleton 2 "B" :: m Int String in
                let map' = Map.alter (const Nothing) 2 map in
                Map.lookup 2 map' @?= Nothing
        ]
    ]

testNaiveTree :: TestTree
testNaiveTree = testGroup "Test NaiveTree" [
        testGroup "merge" [
            testCase "merge empty" $
                merge Nil Nil @?= (Nil :: NaiveTree () ())
            ,
            testCase "merge two nodes" $
                -- Ваша реализация может выдавать другое дерево, соответствующее
                -- последовательности 1, 2.
                merge (Node 1 "a" Nil Nil) (Node 2 "b" Nil Nil)
                    @?= Node 1 "a" Nil (Node 2 "b" Nil Nil)
        ]
    ]

testMap :: TestTree
testMap = testGroup "Testing implementations of trees"
    [
        mapTests "Data.Map.Strict" (Proxy :: Proxy SMap.Map),
        mapTests "NaiveList" (Proxy :: Proxy NaiveList),
        mapTests "NaiveTree" (Proxy :: Proxy NaiveTree),
        testNaiveTree
    ]
