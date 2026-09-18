#include "solver_for_graphblas.h"
#include "csparse.h"

//Used for now, should be changed when a solver is introduced in SuiteSparse::GraphBLAS.
void solver_for_graphblas_FP_sym(GrB_Matrix A, my_real_c* b){
    GrB_Index Ap_len, Ai_len, Ax_len, unsigned_m, unsigned_n;
    GrB_Index *Ap, *Ai;
    int *signed_Ap, *signed_Ai; 
    GrB_Index i;
    my_real_c *Ax;
    cs matrix_format;

    GrB_Matrix_exportSize(&Ap_len, &Ai_len, &Ax_len, GrB_CSC_FORMAT, A);
    Ap = (GrB_Index*)malloc(Ap_len*sizeof(GrB_Index));
    Ai = (GrB_Index*)malloc(Ai_len*sizeof(GrB_Index));
    signed_Ap = (int*)malloc(Ap_len*sizeof(int));
    signed_Ai = (int*)malloc(Ai_len*sizeof(int));
    Ax = (my_real_c*)malloc(Ax_len*sizeof(my_real_c));
    GrB_Matrix_export(Ap, Ai, Ax, &Ap_len, &Ai_len, &Ax_len, GrB_CSC_FORMAT, A);
    
    for(i=0; i<Ap_len; i++)
        signed_Ap[i] = (int)(Ap[i]);
    for(i=0; i<Ai_len; i++)
        signed_Ai[i] = (int)(Ai[i]);
    

    GrB_Matrix_nrows(&(unsigned_m), A);
    GrB_Matrix_ncols(&(unsigned_n), A);
    matrix_format.m = unsigned_m;
    matrix_format.n = unsigned_n;
    matrix_format.p = signed_Ap;
    matrix_format.i = signed_Ai;
    matrix_format.x = Ax;
    matrix_format.nz = -1;
    matrix_format.nzmax = Ax_len;

    cs_cholsol(&matrix_format, b, 1);

    free(Ap);
    free(signed_Ap);
    free(Ai);
    free(signed_Ai);
    free(Ax);
}
