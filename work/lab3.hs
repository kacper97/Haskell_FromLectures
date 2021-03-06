--- Naming Values worksheet


------------------------------------------------
-- pi is a real number which is approximately
-- three and 1,416 ten thousandths. Define a Double
-- with this approximate value.

pie:: Double               -- Mis-spelling deliberate
--pie = undefined
pie = 3.1416


--------------------------------------------------
-- The formula for the area of a circle is 
-- pie times r squared, where r is the radius of 
-- the circle. Define a Double which is the area 
-- in square centimeters of a circle with diameter 
-- 10 centimeters.

areaCircleDiameter10:: Double
--areaCircleDiameter10 = undefined
areaCircleDiameter10 = pie*10


----------------------------------------------
-- Define an Integer that is the number of 
-- seconds in a week.

secondsIn1Week:: Integer
--secondsIn1Week = undefined
secondsIn1Week = 60*60*24*7


---------------------------------------------
-- Define a List of Integers with the elements
-- 1 to 10 in increasing order

oneToTen:: [Integer]
--oneToTen = undefined
oneToTen = [1,2,3,4,5,6,7,8,9,10]


---------------------------------------------
-- Define a string that is your first name

yourFirstName:: String
--yourFirstName = undefined
yourFirstName = "Kacper"


---------------------------------------------
-- Define an Integer that is your age

yourAge:: Integer     
--yourAge = undefined
yourAge = 21

---------------------------------------------
-- Define a Bool truthvalue that is 
-- (yourAge is greater than 19) or (yourFirstName is "Tim")

compareName:: Bool
--compareName = undefined
compareName = yourAge>19 || yourFirstName=="Tim"

---------------------------------------------
-- Define an Double that is the average of 
-- 3.0, 7.42, and 24.8

average:: Double
--average = undefined
average = (3.0+7.42+24.8)/3

---------------------------------------------
-- Define an Integer that is the remainder
-- when 14563 is divided by 22. Hint use the 
-- "mod" operator. Try it out to see how it works

remainder::Integer 
--remainder = undefined
remainder = mod 14563 22


---------------------------------------------
-- Define a tuple that has your first name 
-- and your age.

tuple:: (String,Integer)
--tuple = undefined
tuple = ("Kacper",21)

---------------------------------------------
-- Compute the difference between your
-- approximation of pie, and the fraction
-- 22 divided by 7.

difference:: Double
--difference = undefined
difference = pie - (22/7)
