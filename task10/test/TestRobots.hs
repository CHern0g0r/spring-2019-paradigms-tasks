import Test.Tasty
import Test.Tasty.HUnit

import Robots

main :: IO ()
main = defaultMain testsRobots

testsRobots :: TestTree
testsRobots = let
        walter = robot "Walter" 50 50
        jesse = robot "Jesse" 30 10
        gus = robot "Gus" 100 (-100)
        mike = robot "Mike" 70 50
    in testGroup "Unit tests for Robots task"
        [ testCase "Test for getName" $
            getName walter @?= "Walter"
        , testCase "Test for printRobot" $
            printRobot walter @?= "Walter, attack: 50, health: 50"
        , testCase "Test for getAttack" $
            getAttack walter @?= 50
        , testCase "Test for getHealth" $
            getHealth walter @?= 50
        , testCase "Test for setName" $
            setName "Heisenberg" walter @?= ("Heisenberg", 50, 50)
        , testCase "Test for setAttack" $
            setAttack 500 walter @?= ("Walter", 500, 50)
        , testCase "Test for setHealth" $
            setHealth 0 walter @?= ("Walter", 50, 0)
        , testCase "Test for damage" $
            damage walter 50 @?= ("Walter", 50, 0)
        , testCase "Test for isAlive" $
            isAlive walter @?= True
        , testCase "Test for isAlive" $
            isAlive gus @?= False
        , testCase "Fight between 2 robots" $
            fight mike walter @?= ("Walter", 50, -20)
        , testCase "Fight dead and alive robots" $
            fight gus walter @?= ("Walter", 50, 50)
        , testCase "Test for 3 rounds" $
            threeRoundFight jesse walter @?= ("Walter", 50, 20)
        , testCase "Test for survivors" $
            survivors @?= [("Byvaliy", 100, 100)]
        ]
