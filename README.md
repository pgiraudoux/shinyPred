# shinyPred

This is a shiny app to analyse the impact of predation and trapping on a growing vole population

## Import trapping plan file

A trapping plant file is just a text file tab delimited with two headed colums. In the file 'piegeage1_mar2juin.txt', you have the following


```
Jour	N_camp
2	30
30	30
90	30
120	30

```

This means that trapping is carried out on days 2, 30, 90, 120 and that 30 voles where trapped each day.

An empty file such as below can be uploaded to set trapping effort to zero (until I find a clean way to do it otherwise...)

```
Jour	N_camp
1	0
```


## Caveats

There are few controls at this stage. It is therefore advisable not to plan trapping day 1 (the day where the initial values of the population is given; population growth and trapping starts day 2; trapping day 1 would not be taken into account), or any day > 360 (which exceeds the time limit).


## Ecology

- The number of predators is constant;
- The functional response is of the Holling II type (in short, the variation in the proportion of voles in the diet saturates at 100% with high densities of voles);
- The intrinsic rate of voles (r0) population growth means that one vole female produces, by embedded generations, potentially 100 voles (50 females) during a breeding season;
- The growth is logistic, and the biotic capacity is fixed to the maximum number of voles that the environment can support;
- The breeding season starts on March 1 and lasts 8 months (8 x 30 days), then for 4 months (4 x 30 days) the breeding stops (no other mortality other than that due to predation is taken into account during this period).


