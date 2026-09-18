#include "array_double.h"
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <math.h>

Array_double* alloc_empty_arr_double(){
    Array_double* v = (Array_double*)malloc(sizeof(Array_double));
    v->data = (my_real_c*)malloc(sizeof(my_real_c));
    v->nrows = 0;
    v->ncols = 0;
    v->capacity = 1;

    return v;
}

//Allocated with all data = 0
Array_double* alloc_with_capacity_arr_double(uint64_t nrows, uint64_t ncols){
    Array_double* v = (Array_double*)malloc(sizeof(Array_double));
    v->data = (my_real_c*)calloc(nrows*ncols, sizeof(my_real_c));
    v->nrows = nrows;
    v->ncols = ncols;
    v->capacity = nrows*ncols;

    return v;
}

Array_double* alloc_with_init_arr_double(const my_real_c* data, uint64_t ncols, uint64_t nrows){
    Array_double* v = (Array_double*)malloc(sizeof(Array_double));
    v->data = (my_real_c*)calloc(ncols*nrows, sizeof(my_real_c));
    memcpy(v->data, data, ncols*nrows*sizeof(my_real_c));
    v->nrows = nrows;
    v->ncols = ncols;
    v->capacity = ncols*nrows;

    return v;
}

void double_capacity_arr_double(Array_double* v){
    v->capacity *= 2;
    v->data = (my_real_c*) realloc(v->data, v->capacity*sizeof(my_real_c));
}

void dealloc_arr_double(Array_double* v){
    if(v!=NULL){
        if (v->data) free(v->data);
        v->data = NULL;
        v->capacity = 0;
        v->nrows = 0;
        v->ncols = 0;
    }
}

double* get_ijth_elem_arr_double(const Array_double* v, const uint64_t i, const uint64_t j){
    my_real_c* p;
    if ((i<v->nrows) && (j<v->ncols)) {
        return v->data + i*v->ncols + j;
    } else {
        p = (my_real_c*) malloc(sizeof(my_real_c));
        *p = nan("");
        return p;
    }
}

void set_ijth_elem_arr_double(Array_double* v, const uint64_t i, const uint64_t j, my_real_c* d){
    if (j >= v->ncols){
        printf("ERROR in set set_ijth_elem_arr_double : j bigger than number of columns!");
    }
    while(i*v->ncols+j >= v->capacity) double_capacity_arr_double(v);
    if (i >= v->nrows) v->nrows = i+1;
    
    v->data[i*v->ncols+j] = *d;
}

void copy_arr_double(const Array_double* src, Array_double* dest){
    if (src == NULL){
        if (dest != NULL) {
            free(dest->data);
            dest->nrows = 0;
            dest->ncols = 0;
            dest->capacity = 0;
        }
    } else {
        if (dest != NULL){
            free(dest->data);
            dest->data = (my_real_c*)malloc(src->capacity*sizeof(my_real_c));
            memcpy(dest->data, src->data, src->nrows*src->ncols*sizeof(my_real_c));
            dest->nrows = src->nrows;
            dest->ncols = src->ncols;
            dest->capacity = src->capacity;
        }
    }
}

void print_arr_double(const Array_double* v){
    unsigned long int i, j;
    my_real_c* vi;

    printf("v = [");
    for (i = 0; i<v->nrows-1; i++){
        for (j = 0; j<v->ncols-1; j++){
            vi = get_ijth_elem_arr_double(v, i, j);
            printf("%.3e, ", *vi);
        }
        vi = get_ijth_elem_arr_double(v, i, v->ncols-1);
        printf("%.3e\n", *vi);
    }
    for (j = 0; j<v->ncols-1; j++){
        vi = get_ijth_elem_arr_double(v, v->nrows-1, j);
        printf("%.3e, ", *vi);
    }
    vi = get_ijth_elem_arr_double(v, v->nrows-1, v->ncols-1);
    printf("%.3e]\n", *vi);
}