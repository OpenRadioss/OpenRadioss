Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2022 Altair Engineering Inc.
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
      SUBROUTINE H3D_FLD_TSH(ELBUF_TAB,
     .                       IR,IS,IT,ILAY,IFAIL,IP,MX,
     .                       IPM,NPF,TF,BUFMAT,
     .                       NGL,EVAR,NEL )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE ELBUFDEF_MOD    
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "mvsiz_p.inc"
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "com06_c.inc"
#include      "com08_c.inc"
#include      "param_c.inc"
#include      "tabsiz_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER , INTENT(IN) :: IR,IS,IT,ILAY,IFAIL,IP,MX,NEL 
      INTEGER , DIMENSION(NEL), INTENT(IN) ::  NGL
      my_real , DIMENSION(3,MVSIZ), INTENT(IN) ::  EVAR
      my_real , DIMENSION(SBUFMAT), TARGET ::  BUFMAT
      my_real , DIMENSION(STF), INTENT(IN) ::  TF
      INTEGER , DIMENSION(SNPC), INTENT(IN) ::  NPF
      INTEGER , DIMENSION(NPROPMI,NUMMAT), TARGET ::  IPM
      TYPE (ELBUF_STRUCT_), TARGET :: ELBUF_TAB
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C----------------------------------------------- 
      INTEGER I,I1,II,J,IL
      INTEGER NPAR,IADBUF,NFUNC,IPG,NVARF
      INTEGER, DIMENSION(:) ,POINTER   :: FLD_IDX,FOFF,IFUNC
      my_real, DIMENSION(:), POINTER   :: UPARAMF,UVARF,DFMAX,
     .                                    TDEL,DAM
      my_real, DIMENSION(NEL) :: ES1,ES2,ES4,ES5,ES6

      TYPE(G_BUFEL_)  ,POINTER :: GBUF     
      TYPE(L_BUFEL_)  ,POINTER :: LBUF     
      TYPE(BUF_FAIL_) ,POINTER :: FBUF 
C-----------------------------------------------
        GBUF => ELBUF_TAB%GBUF
        LBUF => ELBUF_TAB%BUFLY(ILAY)%LBUF(IR,IS,IT)                            
        FBUF => ELBUF_TAB%BUFLY(ILAY)%FAIL(IR,IS,IT)                            
        NPAR   = IPM(112+IP, MX)   
        IADBUF = IPM(114+IP ,MX)
        UPARAMF => BUFMAT(IADBUF:IADBUF+NPAR) 
        NFUNC  = IPM(115+IP, MX)             
        IFUNC  => IPM(115+IP+1:115+IP+NFUNC, MX)         
        UVARF => FBUF%FLOC(IFAIL)%VAR  
        NVARF  = FBUF%FLOC(IFAIL)%NVAR 
        DFMAX => FBUF%FLOC(IFAIL)%DAMMX 
        TDEL  => FBUF%FLOC(IFAIL)%TDEL   
        FLD_IDX=> FBUF%FLOC(IFAIL)%INDX
        FOFF   => FBUF%FLOC(IFAIL)%OFF
        DAM    => FBUF%FLOC(IFAIL)%DAM
        ES1(1:NEL) = EVAR(1,1:NEL)
        ES2(1:NEL) = EVAR(2,1:NEL)
        ES4(1:NEL) = EVAR(3,1:NEL)
         CALL FAIL_FLD_TSH(
     1            NEL       ,NPAR      ,NVARF     ,NFUNC    ,IFUNC      ,
     2            NPF       ,TF        ,TT        ,DT1      ,UPARAMF,   
     3            UVARF     ,NGL       ,IT        ,ILAY     ,IT         ,
     5            ES1       ,ES2       ,ES4       ,ES5      ,ES6        ,
     6            LBUF%OFF  ,FOFF      ,FLD_IDX   ,DAM      ,DFMAX     )
C
      RETURN
      END SUBROUTINE H3D_FLD_TSH
