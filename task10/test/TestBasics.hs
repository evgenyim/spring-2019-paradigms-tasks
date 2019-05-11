import Test.Tasty
import Test.Tasty.HUnit

import Basics

main :: IO ()
main = defaultMain testsBasics

testsBasics :: TestTree
testsBasics = testGroup "Unit tests for Basics tasks"
    [testCase "head' works on non-empty list" $
        head' [1,2,3] @?= 1

    , testCase "head' works on infinite list too" $
        head' [1..] @?= 1

    , testCase "tail' works on non-empty list too" $
        tail' [1,2,3] @?= [2,3]

    , testCase "tail' works on non-empty infinite list too" $
        take' 10 (tail' [1..]) @?= take 10 [2..]

    , testCase "take' takes 1 element from 3-element list" $
        take' 1 [1,2,3] @?= [1]

    , testCase "take' takes 6 element from infinity list" $
        take'  6 [1..] @?= [1,2,3,4,5,6]

    , testCase "drop' drops 1 element from 3-element list" $
        drop' 1 [1,2,3] @?= [2,3]

    , testCase "drop' drops 6 element from infinity list" $
        take' 10 (drop' 6 [1..]) @?= take 10 [7..]

    , testCase "filter' selects only even numbers from 0 to 10" $
        filter' even [0..10] @?= [0,2..10]

    , testCase "filter' selects only even numbers from all positive numbers" $
        take' 10 (filter' even [1..]) @?= take 10 ([2,4..])

    , testCase "foldl'' can be used for finding sum of elements" $
        foldl'' (+) 0 [1,2,3] @?= 6

    , testCase "foldl'' can be used with nonassociative function" $
        foldl'' (^) 2 [1,2,3] @?= 64

    , testCase "concat' works on finite lists as expected" $
        concat' [1,2,3] [4,5,6] @?= [1..6]

    , testCase "concat' works when first list is infinite as expected" $
        take' 6 (concat' [1..] [1,2,3]) @?= take' 6 ([1..])

    , testCase "concat' works when second list is infinite as expected" $
        take' 6 (concat' [1,2,3] [1..]) @?= [1,2,3,1,2,3]

    , testCase "quickSort actualy sorts the list" $
        quickSort' [5,2,3,4,1] @?= [1..5]
    ]
