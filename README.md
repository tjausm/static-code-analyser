This tool can perform two static code analyses on a simple procedural language, namely: Constant Propagation Analysis and Live Variable Analysis.

### Constant Propagation Analysis
This analysis identifies variables that maintain a constant value throughout a program's execution.

Consider this example:

```
x := 1;
y := x + 2;
z := y + 3;
```

Here, x remains constant at 1. Constant Propagation Analysis computes which variables are constant.

### Live Variable Analysis

This analysis categorizes variables into two groups: faint and strongly live.

Consider this example:

```
x := 1; 
x := x − 1; 
x := 2;
```
x is consistently faint, as it's either dead or used solely for computing other faint variables. The Live Variable Analysis computes, the set of live variables.
