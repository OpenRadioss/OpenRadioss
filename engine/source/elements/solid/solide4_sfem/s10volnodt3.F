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
Chd|  S10VOLNODT3                   source/elements/solid/solide4_sfem/s10volnodt3.F
Chd|-- called by -----------
Chd|        S4LAGSFEM                     source/elements/solid/solide4_sfem/s4lagsfem.F
Chd|-- calls ---------------
Chd|        FOAT_TO_6_FLOAT               source/system/parit.F         
Chd|        JACOB_J33                     source/elements/solid/solide8e/jacob_j33.F
Chd|        S10DEFOT3                     source/elements/solid/solide10/s10defot3.F
Chd|        S10PIJTO3                     source/elements/solid/solide10/s10pijto3.F
Chd|        ELBUFDEF_MOD                  ../common_source/modules/mat_elem/elbufdef_mod.F
Chd|====================================================================
      SUBROUTINE S10VOLNODT3(
     1   ELBUF_TAB,VOLNOD6,  X,        NC,
     2   OFFG,     SAV,      XDP,      NEL,
     3   NPT)
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
#include      "com04_c.inc"
#include      "scr05_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER, INTENT(IN) :: NPT
      INTEGER, INTENT(IN) ::NEL
      INTEGER, DIMENSION(MVSIZ,10),INTENT(IN) :: NC
      DOUBLE PRECISION
     .   VOLNOD6(6,2*NUMNOD),SAV(NEL,30)
      DOUBLE PRECISION , DIMENSION(3,SXDP/3), INTENT(IN) :: XDP     
C     REAL
      my_real
     .   X(3,NUMNOD),OFFG(NEL)
      TYPE (ELBUF_STRUCT_), TARGET  :: ELBUF_TAB
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, K,N1,N2,NN,N
      INTEGER IP,IBID,NPE
      INTEGER IPERM1(10),IPERM2(10)
      DATA IPERM1/0,0,0,0,1,2,3,1,2,3/
      DATA IPERM2/0,0,0,0,2,3,1,4,4,4/

      my_real
     .  MFXX(MVSIZ),MFXY(MVSIZ),MFXZ(MVSIZ),
     .  MFYX(MVSIZ),MFYY(MVSIZ),MFYZ(MVSIZ),
     .  MFZX(MVSIZ),MFZY(MVSIZ),MFZZ(MVSIZ),
     .  PX(MVSIZ,10),PY(MVSIZ,10),PZ(MVSIZ,10),JAC(MVSIZ),
     .  VX0(MVSIZ,10),VY0(MVSIZ,10),VZ0(MVSIZ,10),VOLP(MVSIZ)
      DOUBLE PRECISION 
     .   XX(MVSIZ,10), YY(MVSIZ,10), ZZ(MVSIZ,10),
     .   X0(MVSIZ,10), Y0(MVSIZ,10), Z0(MVSIZ,10),
     .   DET6(6,MVSIZ)
C-----
      TYPE(L_BUFEL_) ,POINTER :: LBUF     
C----------------------------
C     NODAL COORDINATES     
C----------------------------
C----------------------------
      IF(IRESP == 1) THEN
        DO N=1,10
          DO I=1,NEL               
            NN = MAX(1,NC(I,N))    
            XX(I,N)=XDP(1,NN)          
            YY(I,N)=XDP(2,NN)          
            ZZ(I,N)=XDP(3,NN)          
          ENDDO                      
        ENDDO
      ELSE
        DO N=1,10
          DO I=1,NEL               
            NN = MAX(1,NC(I,N))    
            XX(I,N)=X(1,NN)          
            YY(I,N)=X(2,NN)          
            ZZ(I,N)=X(3,NN)          
          ENDDO                      
        ENDDO
      END IF !(IRESP == 1) THEN
C-------exact for Itetra4=1  
      DO N=5,10
        N1=IPERM1(N)
        N2=IPERM2(N)
        DO I=1,NEL
          IF(NC(I,N)==0)THEN
            XX(I,N) = HALF*(XX(I,N1)+XX(I,N2))
            YY(I,N) = HALF*(YY(I,N1)+YY(I,N2))
            ZZ(I,N) = HALF*(ZZ(I,N1)+ZZ(I,N2))
          ENDIF
        ENDDO
      ENDDO
C----------------------------
C     NODAL COORDINATES INITIALES    |
C----------------------------
      NPE = 10
      DO N=1,NPE
       DO I=1,NEL
        X0(I,N)=SAV(I,N)
        Y0(I,N)=SAV(I,N+NPE)
        Z0(I,N)=SAV(I,N+2*NPE)
       ENDDO
      ENDDO 
C----------------------------
C     DISPLACEMENT     |
C----------------------------
        N = NPE
        DO I=1,NEL
        VX0(I,N)=XX(I,N)-X0(I,N)
        VY0(I,N)=YY(I,N)-Y0(I,N)
        VZ0(I,N)=ZZ(I,N)-Z0(I,N)
        ENDDO
       DO N=1,NPE
        DO I=1,NEL
        VX0(I,N)=XX(I,N)-X0(I,N)-VX0(I,NPE)
        VY0(I,N)=YY(I,N)-Y0(I,N)-VY0(I,NPE)
        VZ0(I,N)=ZZ(I,N)-Z0(I,N)-VZ0(I,NPE)
        ENDDO
       ENDDO 
      IBID = 1      
      DO IP=1,NPT
         LBUF => ELBUF_TAB%BUFLY(IBID)%LBUF(IP,IBID,IBID)
         CALL S10PIJTO3(PX,PY,PZ,LBUF%PIJ,NEL)
         CALL S10DEFOT3(
     1   PX,      PY,      PZ,      VX0,
     2   VY0,     VZ0,     MFXX,    MFXY,
     3   MFXZ,    MFYX,    MFYY,    MFYZ,
     4   MFZX,    MFZY,    MFZZ,    NEL)
         CALL JACOB_J33(
     1   JAC,     MFXX,    MFXY,    MFXZ,
     2   MFYX,    MFYY,    MFYZ,    MFZX,
     3   MFZY,    MFZZ,    NEL)
        DO I=1,NEL
         IF (OFFG(I) == ZERO .OR. ABS(OFFG(I))>ONE) THEN
           VOLP(I)= ZERO
         ELSE
           VOLP(I) = JAC(I)*LBUF%VOL(I)
         ENDIF
        ENDDO
        
        !Parith-On treatment
        CALL FOAT_TO_6_FLOAT(1  ,NEL  ,VOLP ,DET6 )
        
        DO I=1,NEL
          VOLNOD6(1:6,NC(I,IP)) = VOLNOD6(1:6,NC(I,IP)) + DET6(1:6,I)
        ENDDO
C      
      END DO !IP=1,NPT
C
      RETURN
      END SUBROUTINE S10VOLNODT3
