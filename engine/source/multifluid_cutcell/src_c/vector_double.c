#include "vector_double.h"
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <math.h>

Vector_double* alloc_empty_vec_double(){
    return alloc_with_capacity_vec_double(1);
}

Vector_double* alloc_with_capacity_vec_double(uint64_t size){
    Vector_double* v = (Vector_double*)malloc(sizeof(Vector_double));
    v->data = (my_real_c*)malloc(size*sizeof(my_real_c));
    v->size = 0;
    v->capacity = size;

    return v;
}

Vector_double* alloc_with_init_vec_double(const my_real_c* data, uint64_t size){
    Vector_double* v = (Vector_double*)malloc(sizeof(Vector_double));
    v->data = (my_real_c*)malloc(size*sizeof(my_real_c));
    memcpy(v->data, data, size*sizeof(my_real_c));
    v->size = size;
    v->capacity = size;

    return v;
}

void double_capacity_vec_double(Vector_double* v){
    v->capacity *= 2;
    v->data = (my_real_c*) realloc(v->data, v->capacity*sizeof(my_real_c));
}

void push_back_vec_double(Vector_double** v_ptr, const my_real_c* point){
    Vector_double* v = *v_ptr;
    if ((v->capacity == 0) || (v->data == NULL)){
        *v_ptr = alloc_with_init_vec_double(point, 1);
    }
    else if (v->size >= v->capacity)
    {
        double_capacity_vec_double(v);
        v->data[v->size] = *point;
        v->size += 1;
    }
    else {
        v->data[v->size] = *point;
        v->size += 1;
    }
}

void dealloc_vec_double(Vector_double* v){
    if(v!=NULL){
        free(v->data);
        v->data = NULL;
        v->capacity = 0;
        v->size = 0;
    }
}

my_real_c* get_ith_elem_vec_double(const Vector_double* v, uint64_t i){
    my_real_c* p;
    if (i<v->size) {
        return v->data + i;
    } else {
        p = (my_real_c*) malloc(sizeof(my_real_c));
        *p = nan("");
        return p;
    }
}

void set_ith_elem_vec_double(Vector_double* v, uint64_t i, my_real_c* d){
    while(i >= v->capacity) double_capacity_vec_double(v);
    if (i >= v->size) v->size = i+1;
    
    v->data[i] = *d;
}

void copy_vec_double(const Vector_double* src, Vector_double* dest){
    if (src == NULL){
        if (dest != NULL) {
            free(dest->data);
            dest->size = 0;
            dest->capacity = 0;
        }
    } else {
        if (dest != NULL){
            free(dest->data);
            dest->data = (my_real_c*)malloc(src->capacity*sizeof(my_real_c));
            memcpy(dest->data, src->data, src->size*sizeof(my_real_c));
            dest->size = src->size;
            dest->capacity = src->capacity;
        }
    }
}

Vector_double* cat_vec_double(const Vector_double* v1, const Vector_double* v2){
    Vector_double *catv = alloc_with_capacity_vec_double(v1->size + v2->size);
    catv->size = v1->size + v2->size;
    memcpy(catv->data, v1->data, v1->size*sizeof(my_real_c));
    memcpy(catv->data+v1->size, v2->data, v2->size*sizeof(my_real_c));
    return catv;
}

void print_vec_double(const Vector_double* v){
    unsigned int i;
    my_real_c* vi;

    printf("v = [");
    for (i = 0; i<v->size-1; i++){
        vi = get_ith_elem_vec_double(v, i);
        printf("%.3e, ", *vi);
    }
    vi = get_ith_elem_vec_double(v, v->size-1);
    printf("%.3e]\n", *vi);
}

typedef struct {
    my_real_c r;
    uint64_t i;
} Indexed_my_real_c;


static int compare_reals(const void*a, const void *b){
    Indexed_my_real_c* r1 = (Indexed_my_real_c*) a;
    Indexed_my_real_c* r2 = (Indexed_my_real_c*) b;

    if (r1->r < r2->r){
        return -1;
    } else if (r1->r > r2->r) {
        return 1;
    } else {
        return 0;
    }
}

/// @brief Sort `src`, giving the permutation to sort it (meaning sorted_array = src[permutation_inds]).
/// @param src [IN] Array to sort.
/// @param sorted_array [OUT] Sorted array.
/// @param permutation_inds [OUT] Sorting permutation.
void sort_vec_double(const Vector_double* src, Vector_double* sorted_array, Vector_uint* permutation_inds){
    uint64_t i;
    Indexed_my_real_c* indxd_arr = (Indexed_my_real_c*)malloc(src->size*sizeof(Indexed_my_real_c));

    for(i=0; i<src->size; i++){
        indxd_arr[i] = (Indexed_my_real_c){src->data[i], i};
    }

    qsort(indxd_arr, src->size, sizeof(Indexed_my_real_c), compare_reals);

    for(i=0; i<src->size; i++){
        set_ith_elem_vec_double(sorted_array, i, &(indxd_arr[i].r));
        set_ith_elem_vec_uint(permutation_inds, i, &(indxd_arr[i].i));
    }

    free(indxd_arr);
}