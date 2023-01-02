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
Chd|  ALE51_UPWIND2                 source/ale/ale51/ale51_upwind2.F
Chd|-- called by -----------
Chd|        AFLUXT                        source/ale/ale51/afluxt.F     
Chd|        ALE51_FINISH                  source/ale/ale51/ale51_finish.F
Chd|        ALE51_INIT                    source/ale/ale51/ale51_init.F 
Chd|-- calls ---------------
Chd|        ALE_CONNECTIVITY_MOD          ../common_source/modules/ale_connectivity_mod.F
Chd|====================================================================
      SUBROUTINE ALE51_UPWIND2(PM,X,IXQ,FLUX,FLU1,ALE_CONNECT,
     .               ITRIMAT,DDVOL,QMV,IFLG)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE ALE_CONNECTIVITY_MOD
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
#include      "scr05_c.inc"
#include      "scr_spmd_c.inc"
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "vect01_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IXQ(NIXQ,NUMELQ), ITRIMAT,IFLG,IAD2
      my_real PM(NPROPM,NUMMAT), FLUX(4,*), FLU1(*),DDVOL(*),QMV(8,*), X(3,NUMNOD)
      TYPE(t_ale_connectivity), INTENT(IN) :: ALE_CONNECT
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,II,MAT(MVSIZ)
      my_real REDUC,QMV5,QMV6,QMV7,QMV8,
     .        FLUX1(MVSIZ), FLUX2(MVSIZ), FLUX3(MVSIZ), FLUX4(MVSIZ),
     .        UPWL(4,MVSIZ), R1, R2, R3, R4
C-----------------------------------------------
      DO I=LFT,LLT
        FLUX1(I) = FLUX(1,I)
        FLUX2(I) = FLUX(2,I)
        FLUX3(I) = FLUX(3,I)
        FLUX4(I) = FLUX(4,I)
      ENDDO
C-----------------------------------------------
      DO I=LFT,LLT
        II=I+NFT
        MAT(I)=IXQ(1,II)
      ENDDO
C-----------------------------------------------
C     UPWIND
C-----------------------------------------------
      DO J=1,4
        DO I=LFT,LLT
          UPWL(J,I)=PM(16,MAT(I))
        ENDDO
      ENDDO
C
      !======================================================!
      !  BOUNDARY FACE : no volume flux by default           !
      !    slip wall bc                                      !
      !======================================================!
      DO I=LFT,LLT
       IAD2 = ALE_CONNECT%ee_connect%iad_connect(I + NFT) 
       REDUC=PM(92,MAT(I))
       II=ALE_CONNECT%ee_connect%connected(IAD2 + 1 - 1)
       IF(II == 0)THEN
        FLUX1(I)=FLUX1(I)*REDUC
       ENDIF
C
       II=ALE_CONNECT%ee_connect%connected(IAD2 + 2 - 1)
       IF(II == 0)THEN
        FLUX2(I)=FLUX2(I)*REDUC
       ENDIF
C
       II=ALE_CONNECT%ee_connect%connected(IAD2 + 3 - 1)
       IF(II == 0)THEN
        FLUX3(I)=FLUX3(I)*REDUC
       ENDIF
C
       II=ALE_CONNECT%ee_connect%connected(IAD2 + 4 - 1)
       IF(II == 0)THEN
        FLUX4(I)=FLUX4(I)*REDUC
       ENDIF
C
      ENDDO
C     
      IF(IFLG == 1)THEN
         IF (N2D  ==  1) THEN
            !!! Planar integration of momentum when 2D axi
            DO I=LFT,LLT
               II = I + NFT
               FLUX(1,I) = FLUX1(I)-UPWL(1,I)*ABS(FLUX1(I))
               FLUX(2,I) = FLUX2(I)-UPWL(2,I)*ABS(FLUX2(I))
               FLUX(3,I) = FLUX3(I)-UPWL(3,I)*ABS(FLUX3(I))
               FLUX(4,I) = FLUX4(I)-UPWL(4,I)*ABS(FLUX4(I))
C     
               !!! Divide by mean radius
               QMV(5,I)  = FLUX1(I)+UPWL(1,I)*ABS(FLUX1(I))
               QMV(6,I)  = FLUX2(I)+UPWL(2,I)*ABS(FLUX2(I))
               QMV(7,I)  = FLUX3(I)+UPWL(3,I)*ABS(FLUX3(I))
               QMV(8,I)  = FLUX4(I)+UPWL(4,I)*ABS(FLUX4(I))
C                                   
               FLU1(I) = QMV(5,I)  +  QMV(6,I)  +  QMV(7,I)  +  QMV(8,I)

               R1 = HALF * (X(2, IXQ(2, II)) + X(2, IXQ(3, II)))
               R2 = HALF * (X(2, IXQ(3, II)) + X(2, IXQ(4, II)))
               R3 = HALF * (X(2, IXQ(4, II)) + X(2, IXQ(5, II)))
               R4 = HALF * (X(2, IXQ(5, II)) + X(2, IXQ(2, II)))
               
               !!! Beware of r=0 axis
               IF (R1 /= ZERO) QMV(5,I) = QMV(5,I) / R1
               IF (R2 /= ZERO) QMV(6,I) = QMV(6,I) / R2
               IF (R3 /= ZERO) QMV(7,I) = QMV(7,I) / R3
               IF (R4 /= ZERO) QMV(8,I) = QMV(8,I) / R4
            ENDDO
         ELSE
            DO I=LFT,LLT
               FLUX(1,I) = FLUX1(I)-UPWL(1,I)*ABS(FLUX1(I))
               FLUX(2,I) = FLUX2(I)-UPWL(2,I)*ABS(FLUX2(I))
               FLUX(3,I) = FLUX3(I)-UPWL(3,I)*ABS(FLUX3(I))
               FLUX(4,I) = FLUX4(I)-UPWL(4,I)*ABS(FLUX4(I))
               
               QMV(5,I)  = FLUX1(I)+UPWL(1,I)*ABS(FLUX1(I))
               QMV(6,I)  = FLUX2(I)+UPWL(2,I)*ABS(FLUX2(I))
               QMV(7,I)  = FLUX3(I)+UPWL(3,I)*ABS(FLUX3(I))
               QMV(8,I)  = FLUX4(I)+UPWL(4,I)*ABS(FLUX4(I))
                 
               FLU1(I) = QMV(5,I)  +  QMV(6,I)  +  QMV(7,I)  +  QMV(8,I)
            ENDDO
         ENDIF
      ELSE
         DO I=LFT,LLT
            FLUX(1,I) = FLUX1(I)-UPWL(1,I)*ABS(FLUX1(I))
            FLUX(2,I) = FLUX2(I)-UPWL(2,I)*ABS(FLUX2(I))
            FLUX(3,I) = FLUX3(I)-UPWL(3,I)*ABS(FLUX3(I))
            FLUX(4,I) = FLUX4(I)-UPWL(4,I)*ABS(FLUX4(I))

            QMV5  = FLUX1(I)+UPWL(1,I)*ABS(FLUX1(I))
            QMV6  = FLUX2(I)+UPWL(2,I)*ABS(FLUX2(I))
            QMV7  = FLUX3(I)+UPWL(3,I)*ABS(FLUX3(I))
            QMV8  = FLUX4(I)+UPWL(4,I)*ABS(FLUX4(I))

            FLU1(I) = QMV5  +  QMV6  +  QMV7  +  QMV8
         ENDDO
      END IF
C
      IF(ITRIMAT > 0)THEN
        DO I=LFT,LLT
          DDVOL(I)=HALF * ( FLU1(I)+FLUX(1,I)+FLUX(2,I)+FLUX(3,I)+FLUX(4,I) )
        ENDDO
      ENDIF
C----------------------------------------------- 
      RETURN
      END
C
