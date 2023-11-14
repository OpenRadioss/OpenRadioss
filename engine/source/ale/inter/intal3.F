Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2023 Altair Engineering Inc.
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
Chd|  INTAL3                        source/ale/inter/intal3.F     
Chd|-- called by -----------
Chd|        ALEWDX                        source/ale/grid/alewdx.F      
Chd|-- calls ---------------
Chd|        I9WALE                        source/interfaces/int09/i9wale.F
Chd|        INT12W                        source/ale/inter/int12w.F     
Chd|        INTAL4                        source/ale/inter/intal4.F     
Chd|        ELBUFDEF_MOD                  ../common_source/modules/mat_elem/elbufdef_mod.F
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|====================================================================
      SUBROUTINE INTAL3(IPARI,X        ,V      ,
     2                  A    ,VG      ,ISKEW     ,SKEW     ,ICODE  ,
     3                  IXS  ,IXQ     ,ELBUF_TAB,IPARG  ,
     4                  PM   ,NALE    ,INTBUF_TAB  )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
       USE ELBUFDEF_MOD           
       USE INTBUFDEF_MOD
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
#include      "task_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IPARI(NPARI,NINTER), ICODE(*) ,
     .        IXS(NIXS,NUMELS),IXQ(NIXQ,NUMELQ), IPARG(NPARG,NGROUP),
     .        ISKEW(*), NALE(*)
      my_real X(3,NUMNOD), V(3,NUMNOD), A(3,NUMNOD), VG(*), SKEW(*), PM(NPROPM,NUMMAT)
      TYPE(ELBUF_STRUCT_), DIMENSION(NGROUP) :: ELBUF_TAB
      TYPE(INTBUF_STRUCT_) INTBUF_TAB(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER N, NTY, ITIED
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------
      DO N=1,NINTER
       NTY   =IPARI(7,N)
       IF(NTY == 1)THEN
        IF(ISPMD == 0)THEN
         CALL INTAL4(
     1        IPARI(1,N),INTBUF_TAB(N),X            ,V     ,
     2        A         ,VG           ,ICODE        ,ISKEW ,SKEW  )
        END IF
       ELSEIF(NTY == 9) THEN
         CALL I9WALE(
     1        IPARI(1,N),INTBUF_TAB(N),X         ,V         ,
     2        A         ,VG           ,ICODE     ,ISKEW     ,SKEW      ,
     3        IXS       ,IXQ          ,ELBUF_TAB ,IPARG     ,
     4        PM        ,NALE         )
       ELSEIF(NTY == 12) THEN
        IF(ISPMD == 0)THEN
           ITIED=NINT(INTBUF_TAB(N)%VARIABLES(1))
C Grid Velocities of secondary nodes are not interpolated
C ONLY in case "TIED"
         IF (ITIED  == 1 .OR. ITIED == 2) CALL INT12W(
     1        IPARI(1,N),INTBUF_TAB(N),X            ,V     ,
     2        A         ,VG           ,ICODE        ,ISKEW ,SKEW  )
        ENDIF
       ENDIF
      ENDDO!next N
C
      RETURN
C
      END
