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
Chd|  SIGROTA_XFE                   source/output/anim/generate/sigrota_xfe.F
Chd|-- called by -----------
Chd|        TENSORC_CRK                   source/output/anim/generate/tensorc_crk.F
Chd|-- calls ---------------
Chd|        UROTOV                        source/airbag/uroto.F         
Chd|        CRACKXFEM_MOD                 share/modules/crackxfem_mod.F 
Chd|        ELBUFDEF_MOD                  ../common_source/modules/mat_elem/elbufdef_mod.F
Chd|====================================================================
      SUBROUTINE SIGROTA_XFE(ELBUF_STR,XFEM_STR,
     1               JFT   ,JLT    ,NFT     ,ILAY     ,NEL  ,
     2               ITY   ,IEL_CRK,IADC_CRK,IADTG_CRK,IXFEM,
     3               ICRK  ,NLAY   ,SIG     ,IVISC    ,CRKEDGE )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE ELBUFDEF_MOD
      USE CRACKXFEM_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER JFT,JLT,NFT,NEL,ILAY,ITY,IXFEM,ICRK,NLAY,IVISC,
     .        IEL_CRK(*),IADC_CRK(4,*),IADTG_CRK(3,*)
      my_real
     .   SIG(MVSIZ,5)
C
      TYPE (ELBUF_STRUCT_), TARGET :: ELBUF_STR
      TYPE (ELBUF_STRUCT_), TARGET :: XFEM_STR
      TYPE (XFEM_EDGE_)   , DIMENSION(*) :: CRKEDGE
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,II,J,N,I1,ELCRK,ILAYCRK,
     .        IADC1,IADC2,IADC3,IADC4,JJ(5)
      my_real
     .   X1(MVSIZ),  X2(MVSIZ),  X3(MVSIZ), X4(MVSIZ),
     .   Y1(MVSIZ),  Y2(MVSIZ),  Y3(MVSIZ), Y4(MVSIZ),
     .   Z1(MVSIZ),  Z2(MVSIZ),  Z3(MVSIZ), Z4(MVSIZ),
     .   X21(MVSIZ), Y21(MVSIZ), Z21(MVSIZ), 
     .   X31(MVSIZ), Y31(MVSIZ), Z31(MVSIZ), 
     .   X32(MVSIZ), Y32(MVSIZ), Z32(MVSIZ), 
     .   X42(MVSIZ), Y42(MVSIZ), Z42(MVSIZ),
     .   E1X(MVSIZ), E1Y(MVSIZ), E1Z(MVSIZ), 
     .   E2X(MVSIZ), E2Y(MVSIZ), E2Z(MVSIZ), 
     .   E3X(MVSIZ), E3Y(MVSIZ), E3Z(MVSIZ), 
     .   E11(MVSIZ),E12(MVSIZ),E13(MVSIZ),
     .   E21(MVSIZ),E22(MVSIZ),E23(MVSIZ), DIR(NEL,2),
     .   V1,V2,V3,VR,VS,AA,BB,SUMA
      my_real, 
     .   DIMENSION(:) ,POINTER :: DIR10,DIR1
      TYPE(G_BUFEL_)  ,POINTER :: GBUF   
      TYPE(L_BUFEL_)  ,POINTER :: LBUF
c
      TYPE(L_BUFEL_)  ,POINTER :: XLBUF
C=======================================================================
      IF (NLAY > 1) THEN  !  (IXFEM == 1) - multilayer
        DIR10 => ELBUF_STR%BUFLY(ILAY)%DIRA ! uncracked layer (tag standard elem)
        DIR1  => XFEM_STR%BUFLY(ILAY)%DIRA ! cracked layer (tag phantom elem)
      ELSE !  (IXFEM == 2)
        DIR10 => ELBUF_STR%BUFLY(1)%DIRA
        DIR1  => XFEM_STR%BUFLY(1)%DIRA
      ENDIF
!
      DO I=1,5
        JJ(I) = NEL*(I-1)
      ENDDO
!
C---------------------
      IF (ITY == 3) THEN
C---------------------
C      shells 4 nodes
C---------------------
        DO I=JFT,JLT
          N=NFT+I
          ELCRK = IEL_CRK(N)
          IADC1 = IADC_CRK(1,ELCRK)
          IADC2 = IADC_CRK(2,ELCRK)
          IADC3 = IADC_CRK(3,ELCRK)
          IADC4 = IADC_CRK(4,ELCRK)
C--------------
C  COORDINATES:
C--------------
C  node 1:
          X1(I) = CRKAVX(ICRK)%X(1,IADC1)
          Y1(I) = CRKAVX(ICRK)%X(2,IADC1)
          Z1(I) = CRKAVX(ICRK)%X(3,IADC1)
C  node 2:
          X2(I) = CRKAVX(ICRK)%X(1,IADC2)
          Y2(I) = CRKAVX(ICRK)%X(2,IADC2)
          Z2(I) = CRKAVX(ICRK)%X(3,IADC2)
C  node 3:
          X3(I) = CRKAVX(ICRK)%X(1,IADC3)
          Y3(I) = CRKAVX(ICRK)%X(2,IADC3)
          Z3(I) = CRKAVX(ICRK)%X(3,IADC3)
C  node 4:
          X4(I) = CRKAVX(ICRK)%X(1,IADC4)
          Y4(I) = CRKAVX(ICRK)%X(2,IADC4)
          Z4(I) = CRKAVX(ICRK)%X(3,IADC4)
        ENDDO
C
        DO I=JFT,JLT
          E1X(I)= X2(I)+X3(I)-X1(I)-X4(I)
          E1Y(I)= Y2(I)+Y3(I)-Y1(I)-Y4(I)
          E1Z(I)= Z2(I)+Z3(I)-Z1(I)-Z4(I)
          E2X(I)= X3(I)+X4(I)-X1(I)-X2(I)
          E2Y(I)= Y3(I)+Y4(I)-Y1(I)-Y2(I)
          E2Z(I)= Z3(I)+Z4(I)-Z1(I)-Z2(I)
          E3X(I)=E1Y(I)*E2Z(I)-E1Z(I)*E2Y(I)
          E3Y(I)=E1Z(I)*E2X(I)-E1X(I)*E2Z(I)
          E3Z(I)=E1X(I)*E2Y(I)-E1Y(I)*E2X(I)
        ENDDO
C
        DO I=JFT,JLT
          E11(I) = E1X(I)
          E12(I) = E1Y(I)
          E13(I) = E1Z(I)
          E21(I) = E2X(I)
          E22(I) = E2Y(I)
          E23(I) = E2Z(I)
        ENDDO
C
        DO I=JFT,JLT
          SUMA=E2X(I)*E2X(I)+E2Y(I)*E2Y(I)+E2Z(I)*E2Z(I)
          E1X(I) = E1X(I)*SUMA + E2Y(I)*E3Z(I)-E2Z(I)*E3Y(I)
          E1Y(I) = E1Y(I)*SUMA + E2Z(I)*E3X(I)-E2X(I)*E3Z(I)
          E1Z(I) = E1Z(I)*SUMA + E2X(I)*E3Y(I)-E2Y(I)*E3X(I)
        ENDDO
C
        DO I=JFT,JLT
          SUMA=E1X(I)*E1X(I)+E1Y(I)*E1Y(I)+E1Z(I)*E1Z(I)
          SUMA=ONE/MAX(SQRT(SUMA),EM20)
          E1X(I)=E1X(I)*SUMA
          E1Y(I)=E1Y(I)*SUMA
          E1Z(I)=E1Z(I)*SUMA
        ENDDO
C
        DO I=JFT,JLT
          SUMA=E3X(I)*E3X(I)+E3Y(I)*E3Y(I)+E3Z(I)*E3Z(I)
          SUMA=ONE/MAX(SQRT(SUMA),EM20)
          E3X(I)=E3X(I)*SUMA
          E3Y(I)=E3Y(I)*SUMA
          E3Z(I)=E3Z(I)*SUMA
          E2X(I)=E3Y(I)*E1Z(I)-E3Z(I)*E1Y(I)
          E2Y(I)=E3Z(I)*E1X(I)-E3X(I)*E1Z(I)
          E2Z(I)=E3X(I)*E1Y(I)-E3Y(I)*E1X(I)
        ENDDO
C
        DO I=JFT,JLT
          SUMA=E2X(I)*E2X(I)+E2Y(I)*E2Y(I)+E2Z(I)*E2Z(I)
          SUMA=ONE/MAX(SQRT(SUMA),EM20)
          E2X(I)=E2X(I)*SUMA
          E2Y(I)=E2Y(I)*SUMA
          E2Z(I)=E2Z(I)*SUMA
        ENDDO
      ELSE
C---------------------
C       shells 3 nodes
C---------------------
        DO I=JFT,JLT
          N=NFT+I
          ELCRK = IEL_CRK(N+NUMELC)
          IADC1 = IADTG_CRK(1,ELCRK)
          IADC2 = IADTG_CRK(2,ELCRK)
          IADC3 = IADTG_CRK(3,ELCRK)
C--------------
C  COORDINATES:
C--------------
C  node 1:
          X1(I) = CRKAVX(ICRK)%X(1,IADC1)
          Y1(I) = CRKAVX(ICRK)%X(2,IADC1)
          Z1(I) = CRKAVX(ICRK)%X(3,IADC1)
C  node 2:
          X2(I) = CRKAVX(ICRK)%X(1,IADC2)
          Y2(I) = CRKAVX(ICRK)%X(2,IADC2)
          Z2(I) = CRKAVX(ICRK)%X(3,IADC2)
C  node 3:
          X3(I) = CRKAVX(ICRK)%X(1,IADC3)
          Y3(I) = CRKAVX(ICRK)%X(2,IADC3)
          Z3(I) = CRKAVX(ICRK)%X(3,IADC3)
        ENDDO
C
        DO I=JFT,JLT
          X21(I)=X2(I)-X1(I)
          Y21(I)=Y2(I)-Y1(I)
          Z21(I)=Z2(I)-Z1(I)
          X31(I)=X3(I)-X1(I)
          Y31(I)=Y3(I)-Y1(I)
          Z31(I)=Z3(I)-Z1(I)
          X32(I)=X3(I)-X2(I)
          Y32(I)=Y3(I)-Y2(I)
          Z32(I)=Z3(I)-Z2(I)
        ENDDO
C
        DO I=JFT,JLT
          E11(I) = X21(I)
          E12(I) = Y21(I)
          E13(I) = Z21(I)
          E21(I) = X31(I)
          E22(I) = Y31(I)
          E23(I) = Z31(I)
        ENDDO
C
        DO I=JFT,JLT
          E1X(I)= X21(I)
          E1Y(I)= Y21(I)
          E1Z(I)= Z21(I)
          SUMA = SQRT(E1X(I)*E1X(I)+E1Y(I)*E1Y(I)+E1Z(I)*E1Z(I))
          SUMA = ONE/MAX(SUMA,EM20)
          E1X(I)=E1X(I)*SUMA
          E1Y(I)=E1Y(I)*SUMA
          E1Z(I)=E1Z(I)*SUMA
        ENDDO
C
        DO I=JFT,JLT
          E3X(I)=Y31(I)*Z32(I)-Z31(I)*Y32(I)
          E3Y(I)=Z31(I)*X32(I)-X31(I)*Z32(I)
          E3Z(I)=X31(I)*Y32(I)-Y31(I)*X32(I)
          SUMA = SQRT(E3X(I)*E3X(I)+E3Y(I)*E3Y(I)+E3Z(I)*E3Z(I))
          SUMA = ONE/MAX(SUMA,EM20)
          E3X(I)=E3X(I)*SUMA
          E3Y(I)=E3Y(I)*SUMA
          E3Z(I)=E3Z(I)*SUMA
        ENDDO
C
        DO I=JFT,JLT
          E2X(I)=E3Y(I)*E1Z(I)-E3Z(I)*E1Y(I)
          E2Y(I)=E3Z(I)*E1X(I)-E3X(I)*E1Z(I)
          E2Z(I)=E3X(I)*E1Y(I)-E3Y(I)*E1X(I)
          SUMA = SQRT(E2X(I)*E2X(I)+E2Y(I)*E2Y(I)+E2Z(I)*E2Z(I))
          SUMA = ONE/MAX(SUMA,EM20)
          E2X(I)=E2X(I)*SUMA
          E2Y(I)=E2Y(I)*SUMA
          E2Z(I)=E2Z(I)*SUMA
        ENDDO
      ENDIF  !  IF (ITY == 3)
C--------------------------------------------------      
      DO I=JFT,JLT
        ELCRK = IEL_CRK(N)
        IF (ITY == 7) ELCRK = IEL_CRK(N+NUMELC)
        ILAYCRK = CRKEDGE(ILAY)%LAYCUT(ELCRK)
        IF (ILAYCRK == 0 .OR. ABS(ILAYCRK) == 1) THEN  !  uncracked layer or
c                                                          just cracked
          AA = DIR10(I)
          BB = DIR10(I + NEL)
        ELSE  !  cracked layer
          AA = DIR1(I)
          BB = DIR1(I + NEL)
        ENDIF
        V1 = AA*E11(I) + BB*E21(I)
        V2 = AA*E12(I) + BB*E22(I)
        V3 = AA*E13(I) + BB*E23(I)
        VR=V1*E1X(I)+V2*E1Y(I)+V3*E1Z(I)
        VS=V1*E2X(I)+V2*E2Y(I)+V3*E2Z(I)
        SUMA=SQRT(VR*VR + VS*VS)
        DIR(I,1) = VR/SUMA
        DIR(I,2) = VS/SUMA
      ENDDO
C
      IF (NLAY > 1) THEN
C  uncracked layer
        LBUF => ELBUF_STR%BUFLY(ILAY)%LBUF(1,1,1)
C  cracked layer
        XLBUF => XFEM_STR%BUFLY(ILAY)%LBUF(1,1,1)
      ELSE
C  uncracked layer
        LBUF => ELBUF_STR%BUFLY(1)%LBUF(1,1,ILAY)
C  cracked layer
        XLBUF => XFEM_STR%BUFLY(1)%LBUF(1,1,ILAY)
      ENDIF
C
      DO I=JFT,JLT
        N=NFT+I
        ELCRK = IEL_CRK(N)
        IF (ITY == 7) ELCRK = IEL_CRK(N+NUMELC)
        ILAYCRK = CRKEDGE(ILAY)%LAYCUT(ELCRK)
        IF (ILAYCRK == 0 .OR. ABS(ILAYCRK) == 1) THEN  !  uncracked layer or
c                                                          just cracked
          DO J=1,5
            SIG(I,J) = LBUF%SIG(JJ(J) + I)
          ENDDO
        ELSE  !  cracked layer
          DO J=1,5
            SIG(I,J) = XLBUF%SIG(JJ(J) + I)
          ENDDO
        ENDIF
      ENDDO
C
      IF (IVISC > 0) THEN
        DO I=JFT,JLT
          N=NFT+I
          ELCRK = IEL_CRK(N)
          IF (ITY == 7) ELCRK = IEL_CRK(N+NUMELC)
          ILAYCRK = CRKEDGE(ILAY)%LAYCUT(ELCRK)
          IF (ILAYCRK == 0 .OR. ABS(ILAYCRK) == 1) THEN  !  uncracked layer or
c                                                            just cracked
            DO J = 1,5 
              SIG(I,J) = SIG(I,J) + LBUF%VISC(JJ(J)+I)
            ENDDO                                      
          ELSE  !  cracked layer
            DO J=1,5
              SIG(I,J) = SIG(I,J) + XLBUF%VISC(JJ(J)+I)
            ENDDO
          ENDIF
        ENDDO
      ENDIF ! IF (IVISC > 0)
C
      CALL UROTOV(JFT,JLT,SIG,DIR,NEL)
!! temporary replaced by (the same) UROTOV() in order to do not affect
!! the other multidimensional buffer ARRAYS which are still not modified
!!      CALL UROTO(JFT,JLT,SIG,DIR)
C
C-----------------------------------------------
      RETURN
      END
