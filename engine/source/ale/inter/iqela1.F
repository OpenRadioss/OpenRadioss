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
Chd|  IQELA1                        source/ale/inter/iqela1.F     
Chd|-- called by -----------
Chd|        INTAL2                        source/ale/inter/intal2.F     
Chd|-- calls ---------------
Chd|        SHAPEH                        source/ale/inter/shapeh.F     
Chd|        ANIM_MOD                      ../common_source/modules/anim_mod.F
Chd|        H3D_MOD                       share/modules/h3d_mod.F       
Chd|====================================================================
      SUBROUTINE IQELA1(X,SKEW,A,E,MSM,IRECT,LMSR,CRST,MSR,
     1                 NSV,ILOC,IRTL,MS,NOR,LCODE,ISKEW,FSAV,ITAB,
     2                 FCONT,FNCONT,FTCONT,H3D_DATA,NSN,NMN)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE H3D_MOD
      USE ANIM_MOD
C-----------------------------------------------
C   D e s c r i p t i o n
C-----------------------------------------------
C This subroutine is related to option /INTER/TYPE1
C and computes forces. See E(1:3)=FSN.N(1:3)
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
#include      "scr07_c.inc"
#include      "scr08_a_c.inc"
#include      "scr14_c.inc"
#include      "scr16_c.inc"
#include      "param_c.inc"
#include      "comlock.inc"
#include      "tabsiz_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER,INTENT(INOUT) :: IRECT(4,*), LMSR(4,*), MSR(*), NSV(*), ILOC(*), IRTL(*),LCODE(*), ISKEW(*),ITAB(NUMNOD)
      my_real,INTENT(INOUT) :: X(3,NUMNOD), SKEW(LSKEW,*), A(SA), E(*), MSM(*), CRST(2,*), MS(*),NOR(3,*),FSAV(*)
      my_real,INTENT(INOUT) :: FCONT(3,*),FNCONT(3,*),FTCONT(3,*)
      INTEGER, INTENT(IN) :: NSN,NMN
      TYPE(H3D_DATABASE) :: H3D_DATA
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER NIR, I, J, I3, J3, I2, J2, I1, J1, II, L, JJ, NN, JJ3, JJ2, JJ1, ISK, LCOD
      my_real H(4), N1, N2, N3, AA(3), SSS, TTT, XMSS, FXI, FYI, FZI, FSN
      my_real :: FSN_SAV, FXI_SAV, FYI_SAV, FZI_SAV,IMPX,IMPY,IMPZ
      LOGICAL ICONT, IPCONT, IANIM
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------
      ICONT  = .FALSE.
      IPCONT = .FALSE.
      IANIM  = .FALSE.
      ICONT  = (ANIM_V(4)+OUTP_V(4) > 0+H3D_DATA%N_VECT_CONT)
      IF(ANIM_V(12)+OUTP_V(12)+H3D_DATA%N_VECT_PCONT > 0)THEN      
       IF( (TT>=TANIM .AND. TT<=TANIM_STOP) .OR.TT >= TOUTP.OR.TT >= H3D_DATA%TH3D.OR.
     .      (MANIM >= 4.AND.MANIM <= 15).OR. H3D_DATA%MH3D /= 0)THEN
         IPCONT = .TRUE.
       ENDIF
      ENDIF
      IF(ICONT .OR. IPCONT)IANIM=.TRUE.

      FSN_SAV = ZERO
      FXI_SAV = ZERO
      FYI_SAV = ZERO
      FZI_SAV = ZERO
      
      NIR=2
      IF(N2D == 0)NIR=4
      !main nodes
      DO I=1,NMN
       J=MSR(I)
       I3=3*I
       I2=I3-1
       I1=I2-1
       MSM(I)=MS(J)
       E(I1)=ZERO
       E(I2)=ZERO
       E(I3)=ZERO
      ENDDO

      !secnd nodes
      DO II=1,NSN
       I=NSV(II)
       J=ILOC(II)
       IF(J >= 1) THEN
         L=IRTL(II)
         DO JJ=1,NIR
          NN=IRECT(JJ,L)
          IY(JJ)=NN
         ENDDO
         !parametric coordinates on main face
         SSS=CRST(1,II)
         TTT=CRST(2,II)
         !normal
         N1=NOR(1,II)
         N2=NOR(2,II)
         N3=NOR(3,II)
         !A(1:3,I) <-> A(3*I - 1:3)
         I3=3*I
         I2=I3-1
         I1=I2-1
         CALL SHAPEH(H,SSS,TTT)
         DO JJ=1,NIR
           J3=3*IY(JJ)
           J2=J3-1
           J1=J2-1
           JJ3=3*MSR(IY(JJ))
           JJ2=JJ3-1
           JJ1=JJ2-1
           AA(1)=A(I1)   !Accel for itab(I), where I=NSV(II)
           AA(2)=A(I2)
           AA(3)=A(I3)
           ISK=ISKEW(IY(JJ))
           LCOD=LCODE(IY(JJ))
           XMSS=MS(I)*H(JJ)
           FXI=AA(1)-A(JJ1)
           FYI=AA(2)-A(JJ2)
           FZI=AA(3)-A(JJ3)
           FSN=(FXI*N1+FYI*N2+FZI*N3)*XMSS
           FSN_SAV = FSN_SAV + FSN
           FXI_SAV = FXI_SAV + FXI*XMSS
           FYI_SAV = FYI_SAV + FYI*XMSS
           FZI_SAV = FZI_SAV + FZI*XMSS
           E(J1)=E(J1)+FSN*N1
           E(J2)=E(J2)+FSN*N2
           E(J3)=E(J3)+FSN*N3
           MSM(IY(JJ))=MSM(IY(JJ))+XMSS
         ENDDO!next JJ
       ENDIF
      ENDDO!next II

      !SUM(E(1:3,*)) is NF(1:3) on main segment 

      !---------------------------------
      ! /TH/INTER
      !     NORMAL IMPULSE BACKUP
      !---------------------------------
      IMPX    = FXI_SAV*DT12
      IMPY    = FYI_SAV*DT12
      IMPZ    = FZI_SAV*DT12
      FSN_SAV = FSN_SAV*DT12
#include "lockon.inc"
      FSAV(1)=FSAV(1)  + IMPX
      FSAV(2)=FSAV(2)  + IMPY
      FSAV(3)=FSAV(3)  + IMPZ
      FSAV(8)=FSAV(8)  + ABS(IMPX)
      FSAV(9)=FSAV(9)  + ABS(IMPY)
      FSAV(10)=FSAV(10)+ ABS(IMPZ)
      FSAV(11)=FSAV(11)+ FSN_SAV
#include "lockoff.inc"
      !---------------------------------


      IF(IANIM)THEN
        !---------------------------------
        ! /ANIM/VECT/CONT
        !---------------------------------
        IF(ICONT)THEN
#include "lockon.inc"
          DO I=1,NMN
            J          = MSR(I)
            I3         = 3*I
            I2         = I3-1
            I1         = I2-1
            FCONT(1,J) = FCONT(1,J)+E(I1) 
            FCONT(2,J) = FCONT(2,J)+E(I2)
            FCONT(3,J) = FCONT(3,J)+E(I3)
          ENDDO
#include "lockoff.inc"
        ENDIF
        !---------------------------------
        ! /ANIM/VECT/PCONT
        !---------------------------------
        IF(IPCONT)THEN
#include "lockon.inc"
          DO I=1,NMN
            J           = MSR(I)
            I3          = 3*I
            I2          = I3-1
            I1          = I2-1
            FNCONT(1,J) = FNCONT(1,J)+E(I1) 
            FNCONT(2,J) = FNCONT(2,J)+E(I2)
            FNCONT(3,J) = FNCONT(3,J)+E(I3)
          ENDDO
#include "lockoff.inc"
        ENDIF
      ENDIF

      

C-----------------------------------------------------

      RETURN
      END
