----------------------------------
-- add a type declaration
-- to each of the named expressions
i1:: Integer  -- I have done the first one for you
i1 = 45

i2:: String
i2 = "123"

i3:: Bool
i3 = 45 <= i1

i4:: Char
i4 = 'c'

i5:: [[Char]]
i5 = ["abc","ok"]

i6:: String
i6 = head i5

i7:: String
i7 = tail "abc"  -- Recall a string is a shorthand for a list of Char

i8:: (Bool, Float)
i8 = (True,4.5)

i9:: [Integer]
i9 = [i1,34]

-------------------------------------------------
-- For each named expression replace "undefined"
-- with an expression with the same type as the declaration


j1:: (String,Integer)
j1 = ("Kacper",3)

j2:: [Integer]
j2 = [1,3]

j3:: Char
j3 = 'a'

j4:: Double
j4 = 244.1322

j5:: (Integer,String,Integer,Char)
j5 = (21,"Jack",3,'a')

j6:: ([Char],(Bool,String))
j6 = (['a'],(True,"Kacper"))

j7:: [Bool]
j7 = [True]

j8:: (String,Bool)
j8 = ("Paul",True)