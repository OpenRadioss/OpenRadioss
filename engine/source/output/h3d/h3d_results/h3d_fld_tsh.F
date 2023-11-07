Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2024 Altair Engineering Inc.
Copyright>
Copyright>        This program is free software: you can redistribute it and/or modify
Copyright>        it under the terms of the GNU Affero General Public License as published by
Copyright>        the Free Software Foundation, either version 3 of the License, or
Copyright>        (at your option) any later version.
Copyright>
Copyright>        This program is distributed in the hope that it will be useful,
Copyright>        but WITHOUT ANY WARRANTY; without even the implied warranty of
Copyright>        MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
Copyright>        GNU Affero General Public License for more details.
Copyright>
Copyright>        You should have received a copy of the GNU Affero General Public License
Copyright>        along with this program.  If not, see <https://www.gnu.org/licenses/>.
Copyright>
Copyright>
Copyright>        Commercial Alternative: Altair Radioss Software
Copyright>
Copyright>        As an alternative to this open-source version, Altair also offers Altair Radioss
Copyright>        software under a commercial license.  Contact Altair to discuss further if the
Copyright>        commercial version may interest you: https://www.altair.com/radioss/.
Chd|====================================================================
Chd|  H3D_FLD_TSH                   source/output/h3d/h3d_results/h3d_fld_tsh.F
Chd|-- called by -----------
Chd|        H3D_SKIN_SCALAR               source/output/h3d/h3d_results/h3d_skin_scalar.F
Chd|-- calls ---------------
Chd|        FAIL_FLD_TSH                  source/materials/fail/fld/fail_fld_tsh.F
Chd|        ELBUFDEF_MOD                  ../common_source/modules/mat_elem/elbufdef_mod.F
Chd|====================================================================
      SUBROUTINE H3D_FLD_TSH(ELBUF_TAB,FAIL,
     .                       IR,IS,IT,ILAY,IFAIL,
     .                       NPF,TF,NGL,EVAR,NEL )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE ELBUFDEF_MOD    
      USE FAIL_PARAM_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "mvsiz_p.inc"
#include      "com04_c.inc"
#include      "com08_c.inc"
#include      "param_c.inc"
#include      "tabsiz_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER , INTENT(IN) :: IR,IS,IT,ILAY,IFAIL,NEL 
      INTEGER , DIMENSION(NEL), INTENT(IN) ::  NGL
      my_real , DIMENSION(3,MVSIZ), INTENT(IN) ::  EVAR
      my_real , DIMENSION(STF), INTENT(IN) ::  TF
      INTEGER , DIMENSION(SNPC), INTENT(IN) ::  NPF
      TYPE (ELBUF_STRUCT_), TARGET :: ELBUF_TAB
      TYPE (FAIL_PARAM_) ,INTENT(IN) :: FAIL
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C----------------------------------------------- 
      INTEGER I,I1,II,J,IL
      INTEGER NPAR,NFUNC,IPG,NVARF
      INTEGER, DIMENSION(:) ,POINTER   :: FLD_IDX,FOFF
      my_real, DIMENSION(:), POINTER   :: UVARF,DFMAX,TDEL,DAM
      my_real, DIMENSION(NEL) :: ES1,ES2,ES4,ES5,ES6

      TYPE(G_BUFEL_)  ,POINTER :: GBUF     
      TYPE(L_BUFEL_)  ,POINTER :: LBUF     
      TYPE(BUF_FAIL_) ,POINTER :: FBUF 
C-----------------------------------------------
      GBUF => ELBUF_TAB%GBUF
      LBUF => ELBUF_TAB%BUFLY(ILAY)%LBUF(IR,IS,IT)                            
      FBUF => ELBUF_TAB%BUFLY(ILAY)%FAIL(IR,IS,IT)                            
      NPAR   = FAIL%NUPARAM
      NFUNC  = FAIL%NFUNC    
      UVARF => FBUF%FLOC(IFAIL)%VAR  
      NVARF  = FBUF%FLOC(IFAIL)%NVAR 
      DFMAX => FBUF%FLOC(IFAIL)%DAMMX 
      TDEL  => FBUF%FLOC(IFAIL)%TDEL   
      FLD_IDX=> FBUF%FLOC(IFAIL)%INDX
      FOFF   => FBUF%FLOC(IFAIL)%OFF
      DAM    => FBUF%FLOC(IFAIL)%DAM
      ES1(1:NEL) = EVAR(1,1:NEL)
      ES2(1:NEL) = EVAR(2,1:NEL)
      ES4(1:NEL) = TWO*EVAR(3,1:NEL)

      CALL FAIL_FLD_TSH(
     1          NEL       ,NPAR      ,NVARF     ,NFUNC    ,FAIL%IFUNC ,
     2          NPF       ,TF        ,TT        ,DT1      ,FAIL%UPARAM,   
     3          UVARF     ,NGL       ,IT        ,ILAY     ,IT         ,
     5          ES1       ,ES2       ,ES4       ,ES5      ,ES6        ,
     6          LBUF%OFF  ,FOFF      ,FLD_IDX   ,DAM      ,DFMAX     )
C-----------
      RETURN
      END SUBROUTINE H3D_FLD_TSH
