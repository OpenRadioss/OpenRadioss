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
Chd|  LASER1                        source/loads/laser/laser1.F   
Chd|-- called by -----------
Chd|        ALEWDX                        source/ale/grid/alewdx.F      
Chd|-- calls ---------------
Chd|        LASER2                        source/loads/laser/laser2.F   
Chd|        LASER3                        source/loads/laser/laser2.F   
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|====================================================================
      SUBROUTINE LASER1(LAS ,XLAS ,MS    ,X    ,V     ,
     .                  W   ,WA   ,IPARG ,IXQ  ,PM    ,
     .                  TF  ,NPF , ELBUF_TAB)
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
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "com06_c.inc"
#include      "com08_c.inc"
#include      "units_c.inc"
#include      "param_c.inc"
#include      "scr11_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER LAS(*), IPARG(NPARG,*), IXQ(7,*), NPF(*)
C     REAL
      my_real
     .   XLAS(*), MS(*), X(3,*), V(3,*), W(3,*), WA(3,*),
     .   TF(*), PM(NPROPM,*)
      TYPE(ELBUF_STRUCT_), DIMENSION(NGROUP) :: ELBUF_TAB
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER K1, K2, I, NL, N1, N2, IFUNC, IAFUNC, ITYP
C-----------------------------------------------
      K1 = 1
      K2 = 1
      DO I=1,NLASER
       ITYP    = LAS(K1+5)
       NL = LAS(K1)
       IF(ITYP==0)THEN
         N1 = LAS(K1+1)
         N2 = LAS(K1+2)
         WA(3,N1) = ZERO
         WA(3,N2) = ZERO
       ENDIF
       K1 = K1 + 6 + 2*(NL+1)
       K2 = K2 + 6
      ENDDO
C
      K1 = 1
      K2 = 1
      DO I=1,NLASER
       NL = LAS(K1)
       N1 = LAS(K1+1)
       N2 = LAS(K1+2)
       IFUNC   = LAS(K1+3)
       IAFUNC  = LAS(K1+4)
       ITYP    = LAS(K1+5)
       IF(ITYP==1)THEN
         CALL LASER3(NL        ,N1      ,N2   ,IFUNC    ,IAFUNC    ,
     .               LAS(K1+6) ,XLAS(K2),X    ,ELBUF_TAB,PM        ,
     .               WA        ,IPARG   ,IXQ  ,TF       ,NPF       )
       ELSE
         CALL LASER2(NL       ,N1      ,N2   ,IFUNC    ,IAFUNC    ,
     .              LAS(K1+6) ,XLAS(K2),X    ,ELBUF_TAB,PM        ,
     .              WA        ,IPARG   ,IXQ  ,TF       ,NPF       )
       ENDIF
       K1 = K1 + 6 + 2*(NL+1)
       K2 = K2 + 6
      ENDDO
C
      K1 = 1
      DO I=1,NLASER
       ITYP    = LAS(K1+5)
       NL = LAS(K1)
       IF(ITYP==0)THEN
         N1 = LAS(K1+1)
         N2 = LAS(K1+2)
         WA(1,N1) = ONE
         WA(1,N2) = ONE
       ENDIF
       K1 = K1 + 6 + 2*(NL+1)
      ENDDO
C
C
      K1 = 1
      K2 = 1
      DO I=1,NLASER
       ITYP    = LAS(K1+5)
       NL = LAS(K1)
       IF(ITYP==0)THEN
         N1 = LAS(K1+1)
         N2 = LAS(K1+2)
         IF(WA(1,N1) == ONE)THEN
          W(3,N1) = V(3,N1) - WA(3,N1) / MS(N1)
          WA(1,N1) = ZERO
          WA(3,N1) = ZERO
         ENDIF
         IF(WA(1,N2) == ONE)THEN
          W(3,N2) = V(3,N2) - WA(3,N2) / MS(N2)
          WA(1,N2) = ZERO
          WA(3,N2) = ZERO
         ENDIF
       ENDIF
       K1 = K1 + 6 + 2*(NL+1)
       K2 = K2 + 6
      ENDDO
C
      RETURN
      END
