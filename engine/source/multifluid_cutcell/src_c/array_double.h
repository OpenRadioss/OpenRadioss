#ifndef ARRAY_DOUBLE_H
#define ARRAY_DOUBLE_H

#include <stdint.h>
#include "my_real_c.inc"

/*
Module defining dynamic 2D array of doubles
*/

typedef struct {
    my_real_c* data;
    uint64_t ncols, nrows, capacity;
} Array_double;

void double_capacity_arr_double(Array_double* v);
Array_double* alloc_empty_arr_double();
Array_double* alloc_with_init_arr_double(const my_real_c* points, uint64_t ncols, uint64_t nrows);
Array_double* alloc_with_capacity_arr_double(uint64_t nrows, uint64_t ncols);
void dealloc_arr_double(Array_double* v);
my_real_c* get_ijth_elem_arr_double(const Array_double* v, const uint64_t i, const uint64_t j);
void set_ijth_elem_arr_double(Array_double* v, const uint64_t i, const uint64_t j, my_real_c* d);
void copy_arr_double(const Array_double* src, Array_double* dest);
void print_arr_double(const Array_double* v);



#endif