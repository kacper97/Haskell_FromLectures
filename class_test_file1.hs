
import Data.List(sort,sortBy,group,groupBy)

-- Lab test for Haskell. This is worth 10% of your overall score. This test is worth 100 Marks. 
-- This file is made up of 
-- 		SECTION A - 20 Mark
--	 	SECTION B - 20 Marks
--		SECTION C - 40 Marks
-- The other file (clas_test_fixing errors.hs) is worth 20 Marks.
-- Please put your name below (where indicated) and zip the files into a .zip file using the naming convention 
-- first letter of first name + last name (e.g. mmeagher.zip)


-- PLEASE FILL THIS IN
-- Student Name : 

--SECTION A - 20 Marks

-- For each named declaration below. Use the comment preceeding
-- it as a guide to creating a comprehension that computes the
-- same value as that displayed in the comment.

-- [8,9,10,11,12]
x1:: [Integer]  
x1 = [8,9,10,11,12]

-- [10,20,30,40,50,60]
x2:: [Integer]
x2 = [10,20,30,40,50,60]

-- -- [(1,2),(2,3),(3,4),(4,5)]
x3:: [(Integer,Integer)]
x3 = [(1,2),(2,3),(3,4),(4,5)]

-- write the sign function that returnss
--  +1 if its argument is positive
--  -1 if its argument is negative
--   0 if its argument is zero
signx :: Integer -> Integer  
signx x = 
  if (x > 0) then x == 1
  else if (x==0)  then x == 0
  else if (x < 0) then x == -1
j1:: (String,Integer)
j1 = ("Kaper",21)
j2:: [Integer]
j2 = [2,3,1,4,5]
j3:: Char
j3 = 'a'
j4:: Double
j4 = 20.1
j5:: (Integer,String,Integer,Char)
j5 = (2,"Kacper",123,'a')
j6:: ([Char],(Bool,String))
j6 = (['a','b','c'],(True, "Kacper"))
j7:: [[Bool]]
j7 = [[True,False]]
j8:: [(String,Bool)]
j8 = [("Kacper",True),("Hon",False)]

-- SECTION C  - 40 Marks
-- For each named expression replace "undefined"
-- with an expression that computes as per give description. 
people = 
[("Tim",24,"Waterford")
,("Tom",36,"Kilkenny")
,("Mary",19,"Waterford")
,("Zach",41,"Kilkenny")
,("Ann",9,"Waterford")
,("Jane",50,"Tipperary")
,("Harry",71,"Wexford")  
,("Jim",80,"Wexford")
,("Robert",23,"Tipperary")
,("Lois",32,"Waterford")
,("Barbara",50,"Tipperary")
,("Caleb",15,"Tipperary")
,("Vicki",24,"Kilkenny")
,("David",50,"Waterford")
,("Justin",50,"Kilkenny")
,("Andrew",29,"Kilkenny")
]

name   (nm,ag,cy) = nm  
age   (nm,ag,cy)  =  ag
county (nm,ag,cy)  = cy 

------------------------------------
-- the names of all people who live in Tiperrary

p1 = sort [name p | p <- people, county p =="Tiperrary"]
-------------------------------------------
-- How many people live in Waterford

p2 = length [name p | p <- people, county p == "Waterford"]


-------------------------------------------------
-- the list of ages of people who live in Kilkenny

p3 = sort [age p | p <- people , county p =="Kilkenny"]
-------------------------------------------------
-- the average age of people who live in Kilkenny

p4 = undefined