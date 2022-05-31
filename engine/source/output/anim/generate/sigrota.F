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
Chd|  SIGROTA                       source/output/anim/generate/sigrota.F
Chd|-- called by -----------
Chd|        DFUNCC_PLY                    source/output/anim/generate/dfuncc_ply.F
Chd|        H3D_SHELL_TENSOR              source/output/h3d/h3d_results/h3d_shell_tensor.F
Chd|        TENSORC                       source/output/anim/generate/tensorc.F
Chd|        TENSORC_PLY                   source/output/anim/generate/tensorc_ply.F
Chd|-- calls ---------------
Chd|        UROTOV                        source/airbag/uroto.F         
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|====================================================================
      SUBROUTINE SIGROTA(JFT     ,JLT     ,NFT     ,IPT      ,NEL      ,
     2                   NS1     ,X       ,IXC     ,ELBUF_STR,
     3                   SIG     ,ITY     ,IXTG    ,IHBE     ,ISTRAIN  ,
     4                   IVISC   )
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
#include      "param_c.inc"
#include      "scr01_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER JFT, JLT, NFT, NEL, IPT, NS1, IXC(NIXC,*),
     .        ITY, IXTG(NIXTG,*),IHBE ,ISTRAIN ,IVISC
      my_real X(3,*), SIG(MVSIZ,5)
      TYPE (ELBUF_STRUCT_), TARGET :: ELBUF_STR
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,II,J,K,N,I1,NLAY,NPTR,NPTS,NPG,IR,IS,JJ(5),DIM
      my_real
     .   X1(NEL),  X2(NEL),  X3(NEL), X4(NEL),
     .   Y1(NEL),  Y2(NEL),  Y3(NEL), Y4(NEL),
     .   Z1(NEL),  Z2(NEL),  Z3(NEL), Z4(NEL),
     .   X21(NEL), Y21(NEL), Z21(NEL), 
     .   X31(NEL), Y31(NEL), Z31(NEL), 
     .   X32(NEL), Y32(NEL), Z32(NEL), 
     .   X42(NEL), Y42(NEL), Z42(NEL),
     .   E1X(NEL), E1Y(NEL), E1Z(NEL), 
     .   E2X(NEL), E2Y(NEL), E2Z(NEL), 
     .   E3X(NEL), E3Y(NEL), E3Z(NEL), 
     .   E11(NEL),E12(NEL),E13(NEL),
     .   E21(NEL),E22(NEL),E23(NEL), DIR(NEL,2),
     .   PHI,CSP,SNP,U(3,NEL),
     .   V1,V2,V3,VR,VS,AA,BB,SUMA
      my_real, 
     .   DIMENSION(:) , POINTER :: DIR1
      TYPE(G_BUFEL_)  , POINTER :: GBUF   
      TYPE(L_BUFEL_)  , POINTER :: LBUF   
C=======================================================================
      NPTR = ELBUF_STR%NPTR
      NPTS = ELBUF_STR%NPTS
      NLAY = ELBUF_STR%NLAY
      NPG  = NPTR*NPTS
      IF (NLAY > 1) THEN
        DIR1 => ELBUF_STR%BUFLY(IPT)%DIRA
      ELSE
        DIR1 => ELBUF_STR%BUFLY(1)%DIRA
      ENDIF
c
!
      DO I=1,5
        JJ(I) = NEL*(I-1)
      ENDDO
!
      IF (ITY == 3) THEN
C---------------------
C      shells 4 nodes
C---------------------
        DO I=JFT,JLT
          N=NFT+I
          X1(I)=X(1,IXC(2,N))
          Y1(I)=X(2,IXC(2,N))
          Z1(I)=X(3,IXC(2,N))
          X2(I)=X(1,IXC(3,N))
          Y2(I)=X(2,IXC(3,N))
          Z2(I)=X(3,IXC(3,N))
          X3(I)=X(1,IXC(4,N))
          Y3(I)=X(2,IXC(4,N))
          Z3(I)=X(3,IXC(4,N))
          X4(I)=X(1,IXC(5,N))
          Y4(I)=X(2,IXC(5,N))
          Z4(I)=X(3,IXC(5,N))
        ENDDO
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
         X1(I)=X(1,IXTG(2,N))
         Y1(I)=X(2,IXTG(2,N))
         Z1(I)=X(3,IXTG(2,N))
         X2(I)=X(1,IXTG(3,N))
         Y2(I)=X(2,IXTG(3,N))
         Z2(I)=X(3,IXTG(3,N))
         X3(I)=X(1,IXTG(4,N))
         Y3(I)=X(2,IXTG(4,N))
         Z3(I)=X(3,IXTG(4,N))
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
         SUMA  = SQRT(E1X(I)*E1X(I)+E1Y(I)*E1Y(I)+E1Z(I)*E1Z(I))
         SUMA=ONE/MAX(SUMA,EM20)
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
      ENDIF
C--------------------------------------------------      
      DO I=JFT,JLT
        AA = DIR1(I)                              
        BB = DIR1(I + NEL)                             
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
      IF (IHBE == 11) THEN
        DO I=JFT,JLT
          DO J = 1,5
            SIG(I,J) = ZERO
          ENDDO
        ENDDO
        DO I=JFT,JLT
          DO IR=1,NPTR
            DO IS=1,NPTS
              IF (NLAY > 1) THEN
                LBUF => ELBUF_STR%BUFLY(IPT)%LBUF(IR,IS,1)
              ELSE
                LBUF => ELBUF_STR%BUFLY(1)%LBUF(IR,IS,IPT)
              ENDIF
              DO J = 1,5
                SIG(I,J) = SIG(I,J) + LBUF%SIG(JJ(J) + I)/NPG
              ENDDO
            ENDDO
          ENDDO
        ENDDO
        IF (IVISC > 0 ) THEN
          DO I=JFT,JLT
            DO IR=1,NPTR
              DO IS=1,NPTS
                IF (NLAY > 1) THEN
                  LBUF => ELBUF_STR%BUFLY(IPT)%LBUF(IR,IS,1)
                ELSE
                  LBUF => ELBUF_STR%BUFLY(1)%LBUF(IR,IS,IPT)
                ENDIF
                DO J = 1,5                                
                  SIG(I,J) = SIG(I,J) + LBUF%VISC(JJ(J) + I)/NPG
                ENDDO                                     
              ENDDO
            ENDDO 
          ENDDO 
        ENDIF
      ELSE
        IF (NLAY > 1) THEN                                
          LBUF => ELBUF_STR%BUFLY(IPT)%LBUF(1,1,1)      
        ELSE                                              
          LBUF => ELBUF_STR%BUFLY(1)%LBUF(1,1,IPT)      
        ENDIF                                             
        DO I=JFT,JLT
          DO J = 1,5
            SIG(I,J) = LBUF%SIG(JJ(J) + I)
          ENDDO
        ENDDO
        IF (IVISC > 0) THEN
          DO I=JFT,JLT
            DO J = 1,5                                
              SIG(I,J) = SIG(I,J) + LBUF%VISC(JJ(J) + I)  
            ENDDO                                      
          ENDDO
        ENDIF  
      ENDIF
C
      CALL UROTOV(JFT,JLT,SIG,DIR,NEL)
!! temporary replaced by (the same) UROTOV() in order to do not affect
!! the other multidimensional buffer ARRAYS which are still not modified
!!      CALL UROTO(JFT,JLT,SIG,DIR)
C
C-----------------------------------------------
      RETURN
      END
