Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2025 Altair Engineering Inc.
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
!||====================================================================
!||    i11sto_vox    ../engine/source/interfaces/intsort/i11sto.F
!||--- called by ------------------------------------------------------
!||    i11trivox     ../engine/source/interfaces/intsort/i11trivox.F
!||--- calls      -----------------------------------------------------
!||    i11pen3_vox   ../engine/source/interfaces/intsort/i11pen3.F
!||--- uses       -----------------------------------------------------
!||    tri7box       ../engine/share/modules/tri7box.F
!||====================================================================
      SUBROUTINE I11STO_VOX(
     1      J_STOK,IRECTS,IRECTM,X     ,II_STOK,
     2      CAND_S,CAND_M,NSN4  ,NOINT ,MARGE,
     3      I_MEM ,PROV_S,PROV_M,ESHIFT,ADDCM  ,
     4      CHAINE,NRTS, ITAB   ,IFPEN ,IFORM  ,
     5      GAPMIN,DRAD ,IGAP   ,GAP_S ,GAP_M  ,
     7      GAP_S_L, GAP_M_L ,DGAPLOAD)

C============================================================================
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE TRI7BOX
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "comlock.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER I_MEM, NRTS, NIN, ITAB(*)
      INTEGER J_STOK,NSN4,NOINT,IFORM,IGAP
      INTEGER IRECTS(2,*),IRECTM(2,*),CAND_S(*),CAND_M(*),ADDCM(*),
     .        CHAINE(2,*),IFPEN(*),II_STOK
      INTEGER PROV_S(MVSIZ),PROV_M(MVSIZ),ESHIFT
C     REAL
      my_real
     .   X(3,*), GAPMIN, MARGE, 
     .   GAP_S(*), GAP_M(*), GAP_S_L(*), GAP_M_L(*)
      my_real , INTENT(IN) :: DGAPLOAD,DRAD
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,K_STOK,I_STOK,IAD0,IAD,IADFIN
      INTEGER I_STOK_FIRST 
C     REAL
      my_real
     .   PENE(MVSIZ)
C-----------------------------------------------
        K_STOK=0

c       CALL I11PEN3(J_STOK ,PROV_S,PROV_M,TZINF ,X     ,
c    .               IRECTS ,IRECTM,PENE  ,NRTS    )
        CALL I11PEN3_VOX(J_STOK ,PROV_S ,PROV_M ,GAPMIN ,DRAD    , 
     .                    MARGE  ,GAP_S ,GAP_M   ,GAP_S_L,GAP_M_L ,
     .                    IGAP   ,X     ,IRECTS ,IRECTM  ,PENE   ,
     .                    NRTS   ,DGAPLOAD)

C-----------------------------------------------
C il faut un lock sur toute la boucle (modification de chaine)
#include "lockon.inc"
C-----------------------------------------------
C elimination des couples deja trouves : une edge
C escclave peut occuper plusieurs voxels. enbalyant les voxels
C de l'edge main pour trouver les seconds correspondantes
C on peut donc trouver plusieurs occurence de l'edge second.
C-----------------------------------------------
        I_STOK = II_STOK
        DO I=1,J_STOK
          IF(PENE(I)>ZERO)THEN
            IAD=ADDCM(PROV_M(I))
            J=0
            DO WHILE(IAD/=0.AND.J<NSN4)
              J=J+1
              IF(CHAINE(1,IAD)==PROV_S(I))THEN
                PENE(I) = ZERO 
                IAD=0
              ELSE
                IAD0=IAD
                IAD=CHAINE(2,IAD)
              ENDIF
            ENDDO
            IF(PENE(I)>ZERO)THEN
              K_STOK = K_STOK + 1
                IADFIN=II_STOK+1
                IF(IADFIN>NSN4) THEN
                  I_MEM = 2
#include "lockoff.inc"
                  RETURN
                ENDIF
                II_STOK   = IADFIN 
              CHAINE(1,IADFIN)=PROV_S(I)
              CHAINE(2,IADFIN)=0
              IF(ADDCM(PROV_M(I))==0)THEN
                ADDCM(PROV_M(I))=IADFIN
              ELSE
                CHAINE(2,IAD0)=IADFIN
              ENDIF
            ENDIF
          ENDIF
        ENDDO
        IF(K_STOK==0) THEN
#include "lockoff.inc"         
          RETURN
        ENDIF

          I_STOK_FIRST = I_STOK
          DO I=1,J_STOK
            IF(PENE(I)>ZERO)THEN 
              I_STOK = I_STOK + 1    
              CAND_S(I_STOK) = PROV_S(I)    
              CAND_M(I_STOK) = PROV_M(I)+ESHIFT   
c             IFPEN(I_STOK) = 0
            ENDIF
          END DO

        IF (IFORM==2 .AND. I_STOK > I_STOK_FIRST) IFPEN(I_STOK_FIRST+1:I_STOK)=0

C-----------------------------------------------
#include "lockoff.inc"
      RETURN
      END




C            OLD ROUTINE 
!||====================================================================
!||    i11sto    ../engine/source/interfaces/intsort/i11sto.F
!||--- called by ------------------------------------------------------
!||    i11tri    ../engine/source/interfaces/intsort/i11tri.F
!||--- calls      -----------------------------------------------------
!||    i11pen3   ../engine/source/interfaces/intsort/i11pen3.F
!||--- uses       -----------------------------------------------------
!||    tri7box   ../engine/share/modules/tri7box.F
!||====================================================================
      SUBROUTINE  I11STO(
     1     J_STOK,IRECTS,IRECTM,X     ,II_STOK,
     2     CAND_S,CAND_M,NSN4  ,NOINT ,TZINF ,
     3     I_MEM ,PROV_S,PROV_M,ESHIFT,ADDCM,
     4     CHAINE,NRTS, ITAB   ,IFPEN ,IFORM)
C============================================================================
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE TRI7BOX
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "comlock.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER I_MEM, NRTS, NIN, ITAB(*)
      INTEGER J_STOK,NSN4,NOINT,IFORM
      INTEGER IRECTS(2,*),IRECTM(2,*),CAND_S(*),CAND_M(*),ADDCM(*),
     .        CHAINE(2,*),IFPEN(*),II_STOK
      INTEGER PROV_S(MVSIZ),PROV_M(MVSIZ),ESHIFT
C     REAL
      my_real
     .   X(3,*),TZINF
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,K_STOK,I_STOK,IAD0,IAD,IADFIN
      INTEGER I_STOK_FIRST 
C     REAL
      my_real
     .   PENE(MVSIZ)
C-----------------------------------------------
        K_STOK=0

        CALL I11PEN3(J_STOK ,PROV_S,PROV_M,TZINF ,X     ,
     .               IRECTS ,IRECTM,PENE  ,NRTS    )
C-----------------------------------------------
C il faut un lock sur toute la boucle (modification de chaine)
#include "lockon.inc"
C-----------------------------------------------
C elimination des couples deja trouves : une edge
C escclave peut occuper plusieurs voxels. enbalyant les voxels
C de l'edge main pour trouver les seconds correspondantes
C on peut donc trouver plusieurs occurence de l'edge second.
C-----------------------------------------------
        I_STOK = II_STOK
        DO I=1,J_STOK
          IF(PENE(I)>ZERO)THEN
            IAD=ADDCM(PROV_M(I))
            J=0
            DO WHILE(IAD/=0.AND.J<NSN4)
              J=J+1
              IF(CHAINE(1,IAD)==PROV_S(I))THEN
                PENE(I) = ZERO 
                IAD=0
              ELSE
                IAD0=IAD
                IAD=CHAINE(2,IAD)
              ENDIF
            ENDDO
            IF(PENE(I)>ZERO)THEN
              K_STOK = K_STOK + 1
                IADFIN=II_STOK+1
                IF(IADFIN>NSN4) THEN
                  I_MEM = 2
#include "lockoff.inc"
                  RETURN
                ENDIF
                II_STOK   = IADFIN 
              CHAINE(1,IADFIN)=PROV_S(I)
              CHAINE(2,IADFIN)=0
              IF(ADDCM(PROV_M(I))==0)THEN
                ADDCM(PROV_M(I))=IADFIN
              ELSE
                CHAINE(2,IAD0)=IADFIN
              ENDIF
            ENDIF
          ENDIF
        ENDDO

        IF(K_STOK==0) THEN
#include "lockoff.inc"         
          RETURN
        ENDIF

          I_STOK_FIRST = I_STOK
          DO I=1,J_STOK
            IF(PENE(I)>ZERO)THEN 
              I_STOK = I_STOK + 1    
              CAND_S(I_STOK) = PROV_S(I)    
              CAND_M(I_STOK) = PROV_M(I)+ESHIFT   
c             IFPEN(I_STOK) = 0
            ENDIF
          END DO

        IF (IFORM==2 .AND. I_STOK > I_STOK_FIRST) IFPEN(I_STOK_FIRST+1:I_STOK)=0

C-----------------------------------------------
#include "lockoff.inc"
      RETURN
      END



