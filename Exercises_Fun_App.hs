--Write bigCubes that takes a list and returns a
--list of cubes that are > 500
bigCubes:: [Int] -> [Int]
bigCubes xs =   filter (>500) $ map (^3) xs
-- map the cube on the list and filter on it 

--Write lottaBiggest that takes a list and replicates
--the largest element 4 times. lottaBiggest [2,5,3,1] => [5,5,5,5]
lottaBiggest::
--Write powers that takes a number and creates a
--list of that number squared, cubed, and
--quadrupled. powers 2 => [4,8,16]

--Assume people are dining. We have a list of tip
--percents (assume people tip at different rates):*Main> let pcts = [0.15, 0.2, 0.21]

-- We have a list of bills (what people owe, minus tip) *Main> let amts = [20.5, 30, 25]
--Write calcBill that takes amts and pcts and
--calculates what each person will pay, based on
--their amt and pct. Then apply a 4% tax rate. *Main>calcBillamtspcts
--[24.518,37.44,31.46]