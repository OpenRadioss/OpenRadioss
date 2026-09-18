#include "vector_int.h"
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <math.h>

/*--------------------------------------------------------
                INT8
--------------------------------------------------------*/
Vector_int8* alloc_empty_vec_int8(){
    return alloc_with_capacity_vec_int8(1);
}

Vector_int8* alloc_with_capacity_vec_int8(uint64_t size){
    Vector_int8* v = (Vector_int8*)malloc(sizeof(Vector_int8));
    v->data = (int8_t*)malloc(size*sizeof(int8_t));
    v->size = 0;
    v->capacity = size;

    return v;
}

Vector_int8* alloc_with_init_vec_int8(const int8_t* data, uint64_t size){
    Vector_int8* v = (Vector_int8*)malloc(sizeof(Vector_int8));
    v->data = (int8_t*)malloc(size*sizeof(int8_t));
    memcpy(v->data, data, size*sizeof(int8_t));
    v->size = size;
    v->capacity = size;

    return v;
}

void double_capacity_vec_int8(Vector_int8* v){
    v->capacity *= 2;
    v->data = (int8_t*) realloc(v->data, v->capacity*sizeof(int8_t));
}

void push_back_vec_int8(Vector_int8** v_ptr, const int8_t* point){
    Vector_int8* v = *v_ptr;
    if ((v->capacity == 0) || (v->data == NULL)){
        *v_ptr = alloc_with_init_vec_int8(point, 1);
    }
    else if (v->size >= v->capacity)
    {
        double_capacity_vec_int8(v);
        v->data[v->size] = *point;
        v->size += 1;
    }
    else {
        v->data[v->size] = *point;
        v->size += 1;
    }
}

void dealloc_vec_int8(Vector_int8* v){
    if(v!=NULL){
        if(v->data){
            free(v->data);
            v->data = NULL;
        }
        v->capacity = 0;
        v->size = 0;
    }
}

int8_t* get_ith_elem_vec_int8(const Vector_int8* v, uint64_t i){
    int8_t* p;
    if (i<v->size) {
        return v->data + i;
    } else {
        p = (int8_t*) malloc(sizeof(int8_t));
        *p = 0;
        return p;
    }
}

void set_ith_elem_vec_int8(Vector_int8* v, uint64_t i, const int8_t* d){
    while(i >= v->capacity) double_capacity_vec_int8(v);
    if (i >= v->size) v->size = i+1;
    
    v->data[i] = *d;
}

void copy_vec_int8(const Vector_int8* src, Vector_int8* dest){
    if (src == NULL){
        if (dest != NULL) {
            free(dest->data);
            dest->size = 0;
            dest->capacity = 0;
        }
    } else {
        if (dest != NULL){
            free(dest->data);
            dest->data = (int8_t*)malloc(src->capacity*sizeof(int8_t));
            memcpy(dest->data, src->data, src->size*sizeof(int8_t));
            dest->size = src->size;
            dest->capacity = src->capacity;
        }
    }
}

void print_vec_int8(const Vector_int8* v){
    uint64_t i;
    int8_t* vi;

    printf("v = [");
    if(v->size>0){
        for (i = 0; i<v->size-1; i++){
            vi = get_ith_elem_vec_int8(v, i);
            printf("%d, ", *vi);
        }
        vi = get_ith_elem_vec_int8(v, v->size-1);
        printf("%d]\n", *vi);
    } else {
        printf("]\n");
    }
}

Vector_int8* cat_vec_int8(const Vector_int8* v1, const Vector_int8* v2){
    Vector_int8 *catv = alloc_with_capacity_vec_int8(v1->size + v2->size);
    catv->size = v1->size + v2->size;
    memcpy(catv->data, v1->data, v1->size*sizeof(int8_t));
    memcpy(catv->data+v1->size, v2->data, v2->size*sizeof(int8_t));
    return catv;
}

/*--------------------------------------------------------
                LONG INT
--------------------------------------------------------*/
Vector_int* alloc_empty_vec_int(){
    return alloc_with_capacity_vec_int(1);
}

Vector_int* alloc_with_capacity_vec_int(uint64_t size){
    Vector_int* v = (Vector_int*)malloc(sizeof(Vector_int));
    v->data = (long int*)malloc(size*sizeof(long int));
    v->size = 0;
    v->capacity = size;

    return v;
}

Vector_int* alloc_with_init_vec_int(const long int* data, uint64_t size){
    Vector_int* v = (Vector_int*)malloc(sizeof(Vector_int));
    v->data = (long int*)malloc(size*sizeof(long int));
    memcpy(v->data, data, size*sizeof(long int));
    v->size = size;
    v->capacity = size;

    return v;
}

void double_capacity_vec_int(Vector_int* v){
    v->capacity *= 2;
    v->data = (long int*) realloc(v->data, v->capacity*sizeof(long int));
}

void push_back_vec_int(Vector_int** v_ptr, const long int* point){
    Vector_int* v = *v_ptr;
    if ((v->capacity == 0) || (v->data == NULL)){
        *v_ptr = alloc_with_init_vec_int(point, 1);
    }
    else if (v->size >= v->capacity)
    {
        double_capacity_vec_int(v);
        v->data[v->size] = *point;
        v->size += 1;
    }
    else {
        v->data[v->size] = *point;
        v->size += 1;
    }
}

void dealloc_vec_int(Vector_int* v){
    if(v!=NULL){
        if(v->data){
            free(v->data);
            v->data = NULL;
        }
        v->capacity = 0;
        v->size = 0;
    }
}

long int* get_ith_elem_vec_int(const Vector_int* v, uint64_t i){
    long int* p;
    if (i<v->size) {
        return v->data + i;
    } else {
        p = (long int*) malloc(sizeof(long int));
        *p = 0;
        return p;
    }
}

void set_ith_elem_vec_int(Vector_int* v, uint64_t i, const long int* d){
    while(i >= v->capacity) double_capacity_vec_int(v);
    if (i >= v->size) v->size = i+1;
    
    v->data[i] = *d;
}

void copy_vec_int(const Vector_int* src, Vector_int* dest){
    if (src == NULL){
        if (dest != NULL) {
            free(dest->data);
            dest->size = 0;
            dest->capacity = 0;
        }
    } else {
        if (dest != NULL){
            free(dest->data);
            dest->data = (long int*)malloc(src->capacity*sizeof(long int));
            memcpy(dest->data, src->data, src->size*sizeof(long int));
            dest->size = src->size;
            dest->capacity = src->capacity;
        }
    }
}

void print_vec_int(const Vector_int* v){
    uint64_t i;
    long int* vi;

    printf("v = [");
    if(v->size>0){
        for (i = 0; i<v->size-1; i++){
            vi = get_ith_elem_vec_int(v, i);
            printf("%ld, ", *vi);
        }
        vi = get_ith_elem_vec_int(v, v->size-1);
        printf("%ld]\n", *vi);
    } else {
        printf("]\n");
    }
}

Vector_int* cat_vec_int(const Vector_int* v1, const Vector_int* v2){
    Vector_int *catv = alloc_with_capacity_vec_int(v1->size + v2->size);
    catv->size = v1->size + v2->size;
    memcpy(catv->data, v1->data, v1->size*sizeof(long int));
    memcpy(catv->data+v1->size, v2->data, v2->size*sizeof(long int));
    return catv;
}

/*--------------------------------------------------------
                UNSIGNED LONG INT
--------------------------------------------------------*/

Vector_uint* alloc_empty_vec_uint(){
    return alloc_with_capacity_vec_uint(1);
}


Vector_uint* alloc_with_capacity_vec_uint(uint64_t size){
    Vector_uint* v = (Vector_uint*)malloc(sizeof(Vector_uint));
    v->data = (long unsigned int*)malloc(size*sizeof(long unsigned int));
    v->size = 0;
    v->capacity = size;

    return v;
}

Vector_uint* alloc_with_init_vec_uint(const long unsigned int* data, uint64_t size){
    Vector_uint* v = (Vector_uint*)malloc(sizeof(Vector_uint));
    v->data = (long unsigned int*)malloc(size*sizeof(long unsigned int));
    memcpy(v->data, data, size*sizeof(long unsigned int));
    v->size = size;
    v->capacity = size;

    return v;
}

void double_capacity_vec_uint(Vector_uint* v){
    v->capacity *= 2;
    v->data = (long unsigned int*) realloc(v->data, v->capacity*sizeof(long unsigned int));
}

void push_back_vec_uint(Vector_uint** v_ptr, const uint64_t* point){
    Vector_uint* v = *v_ptr;
    if ((v->capacity == 0) || (v->data == NULL)){
        *v_ptr = alloc_with_init_vec_uint(point, 1);
    }
    else if (v->size >= v->capacity)
    {
        double_capacity_vec_uint(v);
        v->data[v->size] = *point;
        v->size += 1;
    }
    else {
        v->data[v->size] = *point;
        v->size += 1;
    }
}

void push_back_unique_vec_uint(Vector_uint** v_ptr, const uint64_t* point){
    uint64_t i;
    int8_t isin;
    Vector_uint* v = *v_ptr;

    if ((v->size == 0) || (v->data == NULL)){
        dealloc_vec_uint(*v_ptr);
        free(*v_ptr);
        *v_ptr = alloc_with_init_vec_uint(point, 1);
        return;
    }
    while (v->size >= v->capacity)
    {
        double_capacity_vec_uint(v);
    }

    isin = 0;
    for(i = 0; i<v->size; i++){
        if (v->data[i] == *point){
            isin = 1;
            break;
        }
    }
    if (!isin) {
        v->data[v->size] = *point;
        v->size += 1;
    }
}

void dealloc_vec_uint(Vector_uint* v){
    if(v!=NULL){
        if(v->data){
            free(v->data);
            v->data = NULL;
        }
        v->capacity = 0;
        v->size = 0;
    }
}

uint64_t* get_ith_elem_vec_uint(const Vector_uint* v, uint64_t i){
    uint64_t* p;
    if (i<v->size) {
        return v->data + i;
    } else {
        p = (uint64_t*) malloc(sizeof(uint64_t));
        *p = 0;
        return p;
    }
}

void set_ith_elem_vec_uint(Vector_uint* v, uint64_t i, uint64_t* d){
    while(i >= v->capacity) double_capacity_vec_uint(v);
    if (i >= v->size) v->size = i+1;
    
    v->data[i] = *d;
}

void copy_vec_uint(const Vector_uint* src, Vector_uint* dest){
    if (src == NULL){
        if (dest != NULL) {
            free(dest->data);
            dest->size = 0;
            dest->capacity = 0;
        }
    } else {
        if (dest != NULL){
            free(dest->data);
            dest->data = (uint64_t*)malloc(src->capacity*sizeof(uint64_t));
            memcpy(dest->data, src->data, src->size*sizeof(uint64_t));
            dest->size = src->size;
            dest->capacity = src->capacity;
        }
    }
}

void print_vec_uint(const Vector_uint* v){
    uint64_t i;
    uint64_t* vi;

    printf("v = [");
    if(v->size>0){
        for (i = 0; i<v->size-1; i++){
            vi = get_ith_elem_vec_uint(v, i);
            printf("%ld, ", *vi);
        }
        vi = get_ith_elem_vec_uint(v, v->size-1);
        printf("%ld]\n", *vi);
    } else {
        printf("]\n");
    }
}

Vector_uint* cat_vec_uint(const Vector_uint* v1, const Vector_uint* v2){
    Vector_uint *catv = alloc_with_capacity_vec_uint(v1->size + v2->size);
    catv->size = v1->size + v2->size;
    memcpy(catv->data, v1->data, v1->size*sizeof(unsigned long int));
    memcpy(catv->data+v1->size, v2->data, v2->size*sizeof(unsigned long int));
    return catv;
}

static int compare_uint(const void* a, const void* b)
{
   uint64_t int_a = * ( (uint64_t*) a );
   uint64_t int_b = * ( (uint64_t*) b );

   // an easy expression for comparing
   return (int_a > int_b) - (int_a < int_b);
}
void sort_vec_uint(Vector_uint* v){
    qsort(v->data, v->size, sizeof(uint64_t), compare_uint);
}

int8_t is_in_vec_uint(const Vector_uint* v, const uint64_t* point){
    uint64_t i;
    int8_t isin = 0;

    if ((v->size == 0) || (v->data == NULL)){
        return 0;
    }

    for(i = 0; i<v->size; i++){
        if (v->data[i] == *point){
            isin = 1;
            break;
        }
    }
    return isin;
}

/*--------------------------------------------------------
                INT 64 BITS
--------------------------------------------------------*/

Vector_int64* alloc_empty_vec_int64(){
    return alloc_with_capacity_vec_int64(1);
}

Vector_int64* alloc_with_capacity_vec_int64(uint64_t size){
    Vector_int64* v = (Vector_int64*)malloc(sizeof(Vector_int64));
    v->data = (int64_t*)malloc(size*sizeof(int64_t));
    v->size = 0;
    v->capacity = size;

    return v;
}

Vector_int64* alloc_with_init_vec_int64(const int64_t* data, uint64_t size){
    Vector_int64* v = (Vector_int64*)malloc(sizeof(Vector_int64));
    v->data = (int64_t*)malloc(size*sizeof(int64_t));
    memcpy(v->data, data, size*sizeof(int64_t));
    v->size = size;
    v->capacity = size;

    return v;
}

void double_capacity_vec_int64(Vector_int64* v){
    v->capacity *= 2;
    v->data = (int64_t*) realloc(v->data, v->capacity*sizeof(int64_t));
}

void push_back_vec_int64(Vector_int64** v_ptr, const int64_t* point){
    Vector_int64* v = *v_ptr;
    if ((v->capacity == 0) || (v->data == NULL)){
        *v_ptr = alloc_with_init_vec_int64(point, 1);
    }
    else if (v->size >= v->capacity)
    {
        double_capacity_vec_int64(v);
        v->data[v->size] = *point;
        v->size += 1;
    }
    else {
        v->data[v->size] = *point;
        v->size += 1;
    }
}

void push_back_unique_vec_int64(Vector_int64** v_ptr, const int64_t* point){
    uint64_t i;
    int8_t isin;
    Vector_int64* v = *v_ptr;

    if ((v->size == 0) || (v->data == NULL)){
        dealloc_vec_int64(*v_ptr);
        free(*v_ptr);
        *v_ptr = alloc_with_init_vec_int64(point, 1);
        return;
    }
    if (v->size >= v->capacity)
    {
        double_capacity_vec_int64(v);
    }

    isin = 0;
    for(i = 0; i<v->size; i++){
        if (v->data[i] == *point){
            isin = 1;
            break;
        }
    }
    if (!isin) {
        v->data[v->size] = *point;
        v->size += 1;
    }
}

void dealloc_vec_int64(Vector_int64* v){
    if(v!=NULL){
        if(v->data){
            free(v->data);
            v->data = NULL;
        }
        v->capacity = 0;
        v->size = 0;
    }
}

int64_t* get_ith_elem_vec_int64(const Vector_int64* v, uint64_t i){
    int64_t* p;
    if (i<v->size) {
        return v->data + i;
    } else {
        p = (int64_t*) malloc(sizeof(int64_t));
        *p = 0;
        return p;
    }
}

void set_ith_elem_vec_int64(Vector_int64* v, uint64_t i, int64_t* d){
    while(i >= v->capacity) double_capacity_vec_int64(v);
    if (i >= v->size) v->size = i+1;
    
    v->data[i] = *d;
}

void copy_vec_int64(const Vector_int64* src, Vector_int64* dest){
    if (src == NULL){
        if (dest != NULL) {
            free(dest->data);
            dest->size = 0;
            dest->capacity = 0;
        }
    } else {
        if (dest != NULL){
            free(dest->data);
            dest->data = (int64_t*)malloc(src->capacity*sizeof(int64_t));
            memcpy(dest->data, src->data, src->size*sizeof(int64_t));
            dest->size = src->size;
            dest->capacity = src->capacity;
        }
    }
}

void print_vec_int64(const Vector_int64* v){
    uint64_t i;
    int64_t* vi;

    printf("v = [");
    if(v->size>0){
        for (i = 0; i<v->size-1; i++){
            vi = get_ith_elem_vec_int64(v, i);
            printf("%ld, ", *vi);
        }
        vi = get_ith_elem_vec_int64(v, v->size-1);
        printf("%ld]\n", *vi);
    } else {
        printf("]\n");
    }
}

static int compare_int64( const void* a, const void* b)
{
   int64_t int_a = * ( (int64_t*) a );
   int64_t int_b = * ( (int64_t*) b );

   // an easy expression for comparing
   return (int_a > int_b) - (int_a < int_b);
}
void sort_vec_int64(Vector_int64* v){
    qsort(v->data, v->size, sizeof(int64_t), compare_int64);
}

Vector_int64* cat_vec_int64(const Vector_int64* v1, const Vector_int64* v2){
    Vector_int64 *catv = alloc_with_capacity_vec_int64(v1->size + v2->size);
    catv->size = v1->size + v2->size;
    memcpy(catv->data, v1->data, v1->size*sizeof(int64_t));
    memcpy(catv->data+v1->size, v2->data, v2->size*sizeof(int64_t));
    return catv;
}