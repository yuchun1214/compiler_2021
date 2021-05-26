#ifndef COMMON_H
#define COMMON_H

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>
#include <math.h>

#define _initialize_stack(stack, type)\
    stack->_entries = malloc(sizeof(type)*100); \
    stack->_idx = 0;\
    stack->_capacity = 100;

#define _realloc_stack(stack, type) \
    stack->_entries = realloc(stack->_entries, sizeof(type) * (stack->_capacity += 100))

#define _stack_top(stack, type)\
    ((type*)stack->_entries)[stack->_idx - 1]

#define _stack_pop(stack)\
    if(stack->_idx > 0) stack->_idx -= 1;

#define _stack_push(stack, type, var)\
    if((stack->_idx + 1) > stack->_capacity)\
        if((_realloc_stack(stack, type)) == NULL)\
            exit(100);\
    ((type*)stack->_entries)[stack->_idx] = var;\
    stack->_idx += 1;

#define _stack_get(stack, type, idx)\
    ((type*)stack->_entries)[idx]


#endif /* COMMON_H */
