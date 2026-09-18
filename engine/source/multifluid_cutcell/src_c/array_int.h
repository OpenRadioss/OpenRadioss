#ifndef ARRAY_INT_H
#define ARRAY_INT_H

#include <stdint.h>
/*
Module defining dynamic 2D array of unsigned 64bits integers
*/

typedef struct {
    int64_t* data;
    uint64_t ncols, nrows, capacity;
} Array_int;

void double_capacity_arr_int(Array_int* v);
Array_int* alloc_empty_arr_int();
Array_int* alloc_with_init_arr_int(const int64_t* points, uint64_t ncols, uint64_t nrows);
Array_int* alloc_with_capacity_arr_int(uint64_t nrows, uint64_t ncols);
void dealloc_arr_int(Array_int* v);
int64_t* get_ijth_elem_arr_int(const Array_int* v, const uint64_t i, const uint64_t j);
void set_ijth_elem_arr_int(Array_int* v, const uint64_t i, const uint64_t j, int64_t* d);
void copy_arr_int(const Array_int* src, Array_int* dest);
void print_arr_int(const Array_int* v);



#endif