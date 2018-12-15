###F-Test
## Numerator of the numerator is the difference between unrestricted and restricted rˆ2
## Numerator of the denomiantor is 1 minus unrestricted rˆ2
numerator <- (0.756 - 0.747)/2 ##Denominator is the number of linear restrictions
denominator <- (1 - 0.756)/(100 - 4) ##Denominator is sample size minus number of parameters in the unrestricted model (including the constant)

f_value <- round(numerator/denominator,1)

#Associated p-value
round(1 - pf(f_value, df1 = 2,df2 = 96),2)

#Critical Value
round(qf(0.95, 2, 96),1) ##95% Percentile
round(qf(0.9,2,96),1) ##90%Percentile




