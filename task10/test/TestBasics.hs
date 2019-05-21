import Test.Tasty
import Test.Tasty.HUnit

import Basics

main :: IO ()
main = defaultMain testsBasics

testsBasics :: TestTree
testsBasics = testGroup "Unit tests for Basics tasks"
    [testCase "head' works on non-empty list" $
        head' [1,2,3] @?= 1

    , testCase "head' works on endless list" $
        head' [1..] @?= 1

    , testCase "tail' works on non-empty list" $
        tail' [1,2,3] @?= [2,3]

    , testCase "tail' works on empty list" $
        tail' ([]::[Int]) @?= []

    , testCase "tail' works on endless list" $
        take' 100 (tail' [1,2..]) @?= take' 100 [2,3..]

    , testCase "take' takes 1 element from 3-element list" $
        take' 1 [1,2,3] @?= [1]

    , testCase "take' takes 10 elements from endless list" $
        take' 10 [1..] @?= [1..10]

    , testCase "drop' drops 1 element from 3-element list" $
        drop' 1 [1,2,3] @?= [2,3]

    , testCase "drop' works on empty list" $
        drop' 10 ([]::[Int]) @?= []
    
    , testCase "drop' drops 10 elements from 30-element list" $
        drop' 10 [1..30] @?= [11..30]

    , testCase "drop' drops 1 element from endless list" $
        take' 100 (drop' 1 [1..]) @?= take' 100 [2..]

    , testCase "filter' selects only even numbers from 0 to 10" $
        filter' even [0..10] @?= [0,2..10]
    
    , testCase "filter' selects only even numbers form endless list" $
        take' 100 (filter' even [1..]) @?= take' 100 [2,4..]

    , testCase "foldl'' can be used for finding sum of elements" $
        foldl'' (+) 0 [1,2,3] @?= 6

    , testCase "foldl'' can be used for finding product of elements" $
        foldl'' (*) 1 [1,2,3] @?= 6

    , testCase "foldl'' can be used for non-associative operation" $
        foldl'' (\x a -> "(" ++ (show a) ++ x ++ ")") "" [1,2,3] @?= "(3(2(1)))"

    , testCase "concat' works on finite lists as expected" $
        concat' [1,2,3] [4,5,6] @?= [1..6]

    , testCase "concat' works on finite and infinite list" $
        take' 100 (concat' [1,2] [3..]) @?= take' 100 [1..] 

    , testCase "quickSort actualy sorts the list" $
        quickSort' [5,2,3,4,1] @?= [1..5]
    ]
