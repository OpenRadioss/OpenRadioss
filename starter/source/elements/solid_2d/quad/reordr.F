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
Chd|  REORDR                        source/elements/solid_2d/quad/reordr.F
Chd|-- called by -----------
Chd|        LCE16Q3                       source/elements/solid_2d/quad/lce16q.F
Chd|        LCE16S3                       source/elements/reader/hm_read_solid.F
Chd|-- calls ---------------
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|====================================================================
      SUBROUTINE REORDR(IX    ,NX     ,NEL    ,PM     ,IPOINT ,
     .                  IPARTS,NGRELE ,IGRELEM)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE GROUPDEF_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "param_c.inc"
#include      "scr03_c.inc"
#include      "scr17_c.inc"
#include      "tablen_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NX, NEL,NGRELE
      INTEGER IX(NX,*), IPOINT(2,*),IPARTS(*)
C     REAL
      my_real
     .   PM(NPROPM,*)
C-----------------------------------------------
      TYPE (GROUP_)  , DIMENSION(NGRELE) :: IGRELEM
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER IADR(0:MAXLAW+1), NL, M, I, MLAWP1, MT, MLAW, J,K,N,IAD
C-----------------------------------------------
C   E x t e r n a l   F u n c t i o n s
C-----------------------------------------------
      NL=MAXLAW
C
      DO 50 M=0,NL+1
   50 IADR(M)=0
C---------------------------------------
C     CALCUL DU NOMBRE D'ELEMENT PAR LOI
C---------------------------------------
      DO 100 I=1,NEL
      IF(IX(1,I).LT.0) THEN
        MLAWP1=17+1
      ELSE
        MT=IX(1,I)
        MLAWP1=NINT(PM(19,MT))+1
      ENDIF
      IADR(MLAWP1)=IADR(MLAWP1)+1
  100 CONTINUE
C------------------------------------------
C     CALCUL DE L'ADRESSE D'ELEMENT PAR LOI
C     SKIP DE LA LOI 11 MIS A LA FIN
C------------------------------------------
      IADR(11+1)=0
      DO 150 M=1,NL
  150 IADR(M)=IADR(M)+IADR(M-1)
      IADR(11)=IADR(NL+1)+IADR(NL)
C--------------------------------------------------
C     CALCUL DU POINTEUR D'ELEMENTS CLASSES PAR LOI
C--------------------------------------------------
      DO 400 I=1,NEL
      IF(IX(1,I).LT.0) THEN
        MLAW=17
      ELSE
        MT=IX(1,I)
        MLAW=NINT(PM(19,MT))
      ENDIF
      IADR(MLAW)=IADR(MLAW)+1
      IPOINT(1,I)=IADR(MLAW)
  400 CONTINUE
C------------------------------------------
C     PERMUTATION DES ELEMENTS PAR LOI
C------------------------------------------
      DO 600 J=1,NX
      DO 500 I=1,NEL
        IPOINT(2,I)=IX(J,I)
  500 CONTINUE
      DO 550 I=1,NEL
        IX(J,IPOINT(1,I))=IPOINT(2,I)
  550 CONTINUE
  600 CONTINUE
C------------------------------------------
      IF(INVERS.LT.40)RETURN
C------------------------------------------
C     PERMUTATION DES PARTS
C------------------------------------------
      DO I=1,NEL
        IPOINT(2,I)=IPARTS(I)
      ENDDO
      DO I=1,NEL
        IPARTS(IPOINT(1,I))=IPOINT(2,I)
      ENDDO
C------------------------------------------
C     RENUMEROTATION DES ELEMENTS DANS LES GROUPES
C------------------------------------------
      DO K=1,NGRELE
        N = IGRELEM(K)%NENTITY
        DO I=1,N
          IF (IGRELEM(K)%ENTITY(I).LE.NEL)
     .        IGRELEM(K)%ENTITY(I) = IPOINT(1,IGRELEM(K)%ENTITY(I))
        ENDDO
      ENDDO
C
      RETURN
      END
