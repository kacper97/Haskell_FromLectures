annualSalaryCalc' :: (RealFloat a) => a -> a -> String 
annualSalaryCalc' hourlyRate weekHoursOfWork
 | annualSalary <= smallSalary = "Poor child, try to get another job"
 | annualSalary <= mediumSalary = "Money, Money, Money!"
 | annualSalary <= highSalary = "Ri￠hie Ri￠h"
 | otherwise = "Hello Elon Musk!"
  where
  annualSalary = hourlyRate * (weekHoursOfWork * 52)
  (smallSalary, mediumSalary, highSalary) = (40000, 120000, 200000)
--29/1/19
factors :: Int -> [Int]
factors n =
        [x | x <- [1..n], n`mod`x == 0]
prime :: Int -> Bool
prime n = 
        factors n == [1,n]