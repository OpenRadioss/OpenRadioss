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
Chd|  INTAL1                        source/ale/inter/intal1.F     
Chd|-- called by -----------
Chd|        RESOL                         source/engine/resol.F         
Chd|-- calls ---------------
Chd|        I9MAIN2                       source/interfaces/int09/i9main2.F
Chd|        I9MAIN3                       source/interfaces/int09/i9main3.F
Chd|        INTAL2                        source/ale/inter/intal2.F     
Chd|        H3D_MOD                       share/modules/h3d_mod.F       
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|====================================================================
      SUBROUTINE INTAL1(IPARI,X      ,V       ,
     2                  A    ,ISKEW  ,SKEW    ,LCOD   ,WA      ,
     3                  MS   ,ITAB   ,FSAV    ,INTBUF_TAB      ,
     4                  FCONT,FNCONT ,FTCONT  ,H3D_DATA )
C-----------------------------------------------
C   M o d u l e s
C----------------------------------------------- 
      USE INTBUFDEF_MOD
      USE H3D_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "param_c.inc"
#include      "secur_c.inc"
#include      "task_c.inc"
#include      "units_c.inc"
#include      "warn_c.inc"
#include      "scr05_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER,INTENT(INOUT)              :: IPARI(NPARI,NINTER), ISKEW(*), LCOD(*), ITAB(NUMNOD)
      my_real,INTENT(INOUT)              :: X(3,NUMNOD), V(3,NUMNOD), A(3,NUMNOD), SKEW(*), WA(*), MS(*),FSAV(NTHVKI,*)
      TYPE(INTBUF_STRUCT_),INTENT(INOUT) :: INTBUF_TAB(*)
      my_real,INTENT(INOUT) :: FCONT(3,*),FNCONT(3,*),FTCONT(3,*)
      TYPE(H3D_DATABASE) :: H3D_DATA
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER N, NTY, NMN, NINT
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------
      DO N=1,NINTER
       NTY   =IPARI(7,N)
       IF(NTY == 1)THEN
         NMN   =IPARI(6,N)
         NINT=N
         CALL INTAL2(
     1        IPARI(1,N)   ,INTBUF_TAB(N)       ,X           ,V    ,
     2        A            ,ISKEW        ,SKEW  ,LCOD        ,WA   ,
     3        WA(3*NMN+1)  ,MS           ,ITAB  ,FSAV(1,NINT),FCONT,
     4        FNCONT       ,FTCONT       ,H3D_DATA )
       ELSEIF(NTY == 9) THEN
         NMN   =IPARI(6,N)
         NINT=N
         IF(N2D == 0)THEN
          CALL I9MAIN3(
     1      IPARI(1,N)   ,INTBUF_TAB(N),X        ,V ,
     2      A            ,ISKEW        ,SKEW         ,LCOD     ,WA,
     3      WA(3*NMN+1)  ,MS           ,WA(6*NMN+1)  ,WA(7*NMN+1),ITAB,
     4      FSAV(1,N)    )
         ELSE
          CALL I9MAIN2(
     1      IPARI(1,N)   ,INTBUF_TAB(N),X         ,V,
     2      A            ,ISKEW        ,SKEW         ,LCOD      ,WA,
     3      WA(3*NMN+1)  ,MS           ,WA(6*NMN+1)  ,WA(7*NMN+1),ITAB,
     4      FSAV(1,N)    )
         ENDIF
       ENDIF
      ENDDO !next N
C-----------------------------------------------
      RETURN
C
      END
