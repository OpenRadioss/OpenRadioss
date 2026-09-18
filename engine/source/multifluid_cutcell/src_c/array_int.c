#include "array_int.h"
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <math.h>
#include <limits.h>

Array_int* alloc_empty_arr_int(){
    Array_int* v = (Array_int*)malloc(sizeof(Array_int));
    v->data = (int64_t*)malloc(sizeof(int64_t));
    v->nrows = 0;
    v->ncols = 0;
    v->capacity = 1;

    return v;
}

//Allocated with all data = 0
Array_int* alloc_with_capacity_arr_int(uint64_t nrows, uint64_t ncols){
    Array_int* v = (Array_int*)malloc(sizeof(Array_int));
    v->data = (int64_t*)calloc(nrows*ncols, sizeof(int64_t));
    v->nrows = nrows;
    v->ncols = ncols;
    v->capacity = nrows*ncols;

    return v;
}

Array_int* alloc_with_init_arr_int(const int64_t* data, uint64_t ncols, uint64_t nrows){
    Array_int* v = (Array_int*)malloc(sizeof(Array_int));
    v->data = (int64_t*)calloc(ncols*nrows, sizeof(int64_t));
    memcpy(v->data, data, ncols*nrows*sizeof(int64_t));
    v->nrows = nrows;
    v->ncols = ncols;
    v->capacity = ncols*nrows;

    return v;
}

void double_capacity_arr_int(Array_int* v){
    v->capacity *= 2;
    v->data = (int64_t*) realloc(v->data, v->capacity*sizeof(int64_t));
}

void dealloc_arr_int(Array_int* v){
    if(v!=NULL){
        free(v->data);
        v->data = NULL;
        v->capacity = 0;
        v->nrows = 0;
        v->ncols = 0;
    }
}

int64_t* get_ijth_elem_arr_int(const Array_int* v, const uint64_t i, const uint64_t j){
    int64_t* p;
    if ((i<v->nrows) && (j<v->ncols)) {
        return v->data + i*v->ncols + j;
    } else {
        p = (int64_t*) malloc(sizeof(int64_t));
        *p = INT64_MAX;
        return p;
    }
}

void set_ijth_elem_arr_int(Array_int* v, const uint64_t i, const uint64_t j, int64_t* d){
    if (j >= v->ncols){
        printf("ERROR in set set_ijth_elem_arr_int : j bigger than number of columns!");
    }
    while(i*v->ncols+j >= v->capacity) double_capacity_arr_int(v);
    if (i >= v->nrows) v->nrows = i+1;
    
    v->data[i*v->ncols+j] = *d;
}

void copy_arr_int(const Array_int* src, Array_int* dest){
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
            dest->data = (int64_t*)malloc(src->capacity*sizeof(int64_t));
            memcpy(dest->data, src->data, src->nrows*src->ncols*sizeof(int64_t));
            dest->nrows = src->nrows;
            dest->ncols = src->ncols;
            dest->capacity = src->capacity;
        }
    }
}

void print_arr_int(const Array_int* v){
    unsigned long int i, j;
    int64_t* vi;

    printf("v = [");
    if(v->nrows>0 && v->ncols>0){
        for (i = 0; i<v->nrows-1; i++){
            for (j = 0; j<v->ncols-1; j++){
                vi = get_ijth_elem_arr_int(v, i, j);
                printf("%ld, ", *vi);
            }
            vi = get_ijth_elem_arr_int(v, i, v->ncols-1);
            printf("%ld\n", *vi);
        }
        for (j = 0; j<v->ncols-1; j++){
            vi = get_ijth_elem_arr_int(v, v->nrows-1, j);
            printf("%ld, ", *vi);
        }
        vi = get_ijth_elem_arr_int(v, v->nrows-1, v->ncols-1);
        printf("%ld]\n", *vi);
    } else {
        printf("]\n");
    }
}