##this dataframe contains a subset of one year of a longitudinal survey (it's 2011)

## each line is a household
## B1_X is data about first household member
## B2_X is data about second household member
## etc.

## ID_W is survey wave (all == 20 here)
## ID_H is household ID (for matching across survey waves)
## hhwgt is household survey weight
## nfm is number of family members

## IDIND1 is unique identifier of first household member (for matching with individual survey)
## B1_4 is sex of first household member - this data contains only women-headed households (==2)
## B1_5 is birth year of first household member

## subsequent variables identify the woman's own children (and others in hh)
## IDIND2 is unique identifier of second household member (for matching with individual survey)
## B2_4 is sex of second household member (male ==1, female ==2)
## B2_5 is birth year of second household member
## B2_9_1 IDs whether second household member is bio. child of household head (if yes ==4, if no == any other number)

## same is true for subsequent HH members - for i in 3-24
## IDIND(i) is unique identifier
## B(i)_4 is sex
## B(i)_5 is birth year
## B(i)_9_1 is relation to HH head


## so, 
## 1. for each HH, must identify number of children and their birth years;
## 2. identify mom's birth year;
## 3. use those two pieces of data to find age at which each child was born;
## 4. get that into some kind of dataframe
## later I'll have to deal with issues like finding kids who have already moved out (prob by comparing
## different survey years and choosing the most complete?), getting data from the women's individual surveys
## (esp education data) matched to the household data,
## dealing with male-headed households that have a mom and kids in them, etc.

