# ProgLang-HW1

## group members
    Yifan Li (liy42)

    Ziqing Wang (wangz27)
## Features/ bugs
    There's no bug in our solutions.

    We have 4 Reduction functions, and 4 helper functions.
### Reduction Functions
    function reducer is the reduce function called in Main function. It will reduce the lambda expression via alpha renaming, beta reduction or eta reduction

    function alpha allows bound variable names in one lambda expression to be changed

    function beta reduces one lambda expression by replacing the old String with a new lexp

    function eta reduces one lambda expression by eta reduction: \x.(E x)->E

### Helpter Functions
    function helper is the helper function of function reducer, which checks break point and helps with reducer recursion

    functions remove and freevars are used to find free variables in lambda expression, function getNewVar is used to rename a variable. These functions help alpha reduction.
