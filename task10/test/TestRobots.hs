import Test.Tasty
import Test.Tasty.HUnit

import Robots

main :: IO ()
main = defaultMain testsRobots

testsRobots :: TestTree
testsRobots = let
        walter = robot "Walter" 50 50
        marvin = robot "Marvin" 20 60
    in testGroup "Unit tests for Robots task"
        [ testCase "Test for getName" $
            getName marvin @?= "Marvin"
        , testCase "Test for getAttack" $
            getAttack marvin @?= 20
        , testCase "Test for getHealth" $
            getHealth marvin @?= 60
        , testCase "Test for printRobot" $
            printRobot walter @?= "Walter, attack: 50, health: 50"
        , testCase "Test for setName" $
            setName "Walt" walter @?= ("Walt", 50, 50)
        , testCase "Test for setAttack" $
            setAttack 100 walter @?= ("Walter", 100, 50)
        , testCase "Test for setHealth" $
            setHealth 100 walter @?= ("Walter", 50, 100)
        , testCase "Test for damage" $
            damage walter 10 @?= ("Walter", 50, 40)
        , testCase "Test for isAlive True" $
            isAlive walter @?= True
        , testCase "Test for isAlive False" $
            isAlive ("Test", 100, -10) @?= False
        , testCase "Test for fight" $
            fight walter marvin @?= ("Marvin", 20, 10)
        , testCase "Test for threeRoundFight" $
            threeRoundFight marvin walter @?= ("Marvin", 20, 10)
        , testCase "Test for neueRobotAttak" $
            neueRobotAttak marvin @?= ("Marvin", 20, -440)
        , testCase "Test for survivors" $
            survivors @?= [("Twins", 200, 1200),("Zeus", 300, 700)]
        ]
