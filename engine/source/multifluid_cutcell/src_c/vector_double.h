#ifndef VECTOR_DOUBLE_H
#define VECTOR_DOUBLE_H

#include <stdint.h>
#include "my_real_c.inc"
#include "vector_int.h"

/*
Structure of dynamical 1D array of double (mimic std::Vector<double>).
*/
typedef struct {
    my_real_c* data;
    uint64_t size, capacity;
} Vector_double;

void push_back_vec_double(Vector_double** v_ptr, const my_real_c* point);
void double_capacity_vec_double(Vector_double* v);
Vector_double* alloc_empty_vec_double();
Vector_double* alloc_with_init_vec_double(const my_real_c* points, uint64_t size);
Vector_double* alloc_with_capacity_vec_double(uint64_t size);
void dealloc_vec_double(Vector_double* v);
my_real_c* get_ith_elem_vec_double(const Vector_double* v, uint64_t i);
void set_ith_elem_vec_double(Vector_double* v, uint64_t i, my_real_c* d);
void copy_vec_double(const Vector_double* src, Vector_double* dest);
Vector_double* cat_vec_double(const Vector_double* v1, const Vector_double* v2);
void print_vec_double(const Vector_double* v);
void sort_vec_double(const Vector_double* src, Vector_double* sorted_array, Vector_uint* permutation_inds);



#endif