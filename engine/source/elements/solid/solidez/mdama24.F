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
Chd|  MDAMA24                       source/elements/solid/solidez/mdama24.F
Chd|-- called by -----------
Chd|        SZHOUR3                       source/elements/solid/solidez/szhour3.F
Chd|        SZHOUR3_OR                    source/elements/solid/solidez/szhour3_or.F
Chd|-- calls ---------------
Chd|        CBATRAN3V                     source/elements/solid/solidez/cbatran3v.F
Chd|        GETTRANSV                     source/elements/solid/solidez/gettransv.F
Chd|        MMODUL24C                     source/elements/solid/solidez/mmodul24c.F
Chd|        ELBUFDEF_MOD                  ../common_source/modules/mat_elem/elbufdef_mod.F
Chd|====================================================================
      SUBROUTINE MDAMA24(ELBUF_STR,JFT     ,JLT     ,PM    ,MAT    ,
     .                   DAMA_G )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE ELBUFDEF_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER JFT, JLT  
      INTEGER MAT(*)
C     REAL
      my_real
     .   PM(NPROPM,*),DAMA_G(MVSIZ,3)
      TYPE (ELBUF_STRUCT_), TARGET :: ELBUF_STR
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,MX,IAD,J,K,ipr,NBDAMA,ISYM
C     REAL
      my_real
     .   CC(MVSIZ,3,3),B(MVSIZ,3,3),G33(MVSIZ,3,3)
      my_real
     .   NU,LAMDA,GG,C1,QC(MVSIZ,9),QCG(MVSIZ,9),QG(MVSIZ,9),
     .   QGC(MVSIZ,9),G3(MVSIZ,3),DAM
      my_real
     .   C3(MVSIZ,3),DAMANG(MVSIZ,6)
      TYPE(L_BUFEL_) ,POINTER :: LBUF
C-----------------------------------------------
       LBUF => ELBUF_STR%BUFLY(1)%LBUF(1,1,1)
        MX = MAT(1)
C-------get concrete part (CC,G3) and ANG in damage system	 
        CALL MMODUL24C(JLT    ,PM(1,MX),LBUF%DAM,LBUF%CRAK ,
     .                 CC     ,G3      ,LBUF%ANG,DAMANG ,NBDAMA)
       IF (NBDAMA==0) THEN
        DAMA_G(JFT:JLT,1:3)= ZERO
       ELSE
         C3(JFT:JLT,1:3)=PM(24,MX)
C----- 	
         CALL GETTRANSV(JFT,JLT,DAMANG ,QC,QCG,QGC,QG)
        B(JFT:JLT,1:3,1:3)=ZERO
C      
        DO J= 1,3
         DO I=JFT,JLT
          B(I,J,J)=G3(I,J)
         ENDDO
        ENDDO
        ISYM = 1
        CALL CBATRAN3V(JFT   ,JLT    ,QC   ,CC ,QC ,ISYM) 
        CALL CBATRAN3V(JFT   ,JLT    ,QGC  ,B ,QGC,ISYM) 
C      
        DO J= 1,3
         DO I=JFT,JLT
           CC(I,J,J)=CC(I,J,J)+FOUR*B(I,J,J)
         ENDDO
        ENDDO
C 
        DO J= 1,3
        DO I=JFT,JLT
          DAM = ONE-CC(I,J,J)/C3(I,J)
          DAMA_G(I,J)=MAX(ZERO,DAM)
        ENDDO
        ENDDO
       END IF !(NBDAMA==0) THEN
C
      RETURN
      END
