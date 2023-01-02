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
Chd|  FORCEFINGEO                   source/loads/general/forcefingeo.F
Chd|-- called by -----------
Chd|        RESOL                         source/engine/resol.F         
Chd|-- calls ---------------
Chd|        FINTER                        source/tools/curve/finter.F   
Chd|        FINTER_SMOOTH                 source/tools/curve/finter_smooth.F
Chd|        H3D_MOD                       share/modules/h3d_mod.F       
Chd|====================================================================
      SUBROUTINE FORCEFINGEO(IBFV  ,NPC    ,TF     ,A     ,V    ,X     ,
     2                       VEL   ,SENSOR ,LSENSOR,FSKY  ,FEXT ,ITABM1,
     3                       H3D_DATA)
C-----------------------------------------------
C   M o d u l e s
C----------------------------------------------- 
      USE H3D_MOD 
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "com06_c.inc"
#include      "com08_c.inc"
#include      "impl1_c.inc"
#include      "scr14_c.inc"
#include      "scr16_c.inc"
#include      "parit_c.inc"
C-----------------------------------------------
C   E x t e r n a l  F u n c t i o n s
C-----------------------------------------------
      INTEGER SYSFUS2
      my_real FINTER,FINTER_SMOOTH
      EXTERNAL FINTER,FINTER_SMOOTH
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IBFV(NIFV,*), NPC(*), LSENSOR(NSENSI,*)
      INTEGER ITABM1(*)
      my_real 
     .   TF(*), A(3,*), V(3,*), X(3,*),
     .   VEL(LFXVELR,*), SENSOR(NSENSR,*),
     .   FSKY(8,LSKY),FEXT(3,*)
      TYPE(H3D_DATABASE) :: H3D_DATA
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER N, N1, N2, K
      INTEGER NCUR, NCUR_OLD, ISENS, ISMOOTH
      my_real
     .   AXI, AA, A0, VV, DYDX, TS, TS_OLD,
     .   STARTT, STOPT, FACX, FACY, F1, F2,
     .   XA, YA, ZA, XF, YF, ZF,
     .   FAC, SKEW1, SKEW2, SKEW3, TFEXTT
C=======================================================================
      TFEXTT = ZERO
      TS_OLD = ZERO
      NCUR_OLD = 0
C----------------------------------
C     FORCE CONCENTREE
C     IF(IMACH/=3.OR.IPARIT==0)
C     Code PARITH/OFF
C----------------------------------
      DO N=1,NFXVEL
        IF (IBFV(13,N) /= 2) CYCLE
        NCUR = IBFV(15,N)
        IF (NCUR == 0) CYCLE
        STARTT = VEL(2,N)
        STOPT  = VEL(3,N)
        IF(TT<STARTT) CYCLE
        IF(TT>STOPT ) CYCLE
        N1 = IABS(IBFV(1,N))
        N2 = IBFV(14,N)
c        N2 = SYSFUS2(N2,ITABM1,NUMNOD)
        FACX = VEL(5,N)
        FACY = VEL(8,N)
C
        ISENS=0
        DO K=1,NSENSOR
          IF(IBFV(4,N)==LSENSOR(1,K))ISENS=K
        ENDDO
        IF(ISENS==0)THEN
           TS=TT
        ELSE
           TS = TT-SENSOR(2,ISENS)
           IF(TS < ZERO) CYCLE
        ENDIF
C
        IF(NCUR_OLD/=NCUR.OR.TS_OLD/=TS) THEN
!!          F1 = FINTER(NCUR,(TS-DT1)*FACX,NPC,TF,DYDX)
!!          F2 = FINTER(NCUR,TS*FACX,NPC,TF,DYDX)
          ISMOOTH = 0
          IF (NCUR > 0) ISMOOTH = NPC(2*NFUNCT+NCUR+1)
          IF (ISMOOTH == 0) THEN
            F1 = FINTER(NCUR,(TS-DT1)*FACX,NPC,TF,DYDX)
            F2 = FINTER(NCUR,TS*FACX,NPC,TF,DYDX)
          ELSE
            F1 = FINTER_SMOOTH(NCUR,(TS-DT1)*FACX,NPC,TF,DYDX)
            F2 = FINTER_SMOOTH(NCUR,TS*FACX,NPC,TF,DYDX)
          ENDIF ! IF (ISMOOTH == 0)
          NCUR_OLD = NCUR
          TS_OLD = TS
        ENDIF
C
        A0 = FACY*F1
        AA = FACY*F2
C
        IF(N2D/=1)THEN
         AXI=ONE
        ELSE
         AXI=X(2,N2)
        ENDIF
C
        XA = X(1,N1)
        YA = X(2,N1)
        ZA = X(3,N1)
        XF = X(1,N2)
        YF = X(2,N2)
        ZF = X(3,N2)
        FAC= SQRT((XF-XA)**2+(YF-YA)**2+(ZF-ZA)**2)
        IF(FAC < VEL(7,N)) CYCLE
        SKEW1= (XF-XA)/FAC
        SKEW2= (YF-YA)/FAC
        SKEW3= (ZF-ZA)/FAC
        VV = SKEW1*V(1,N2)+SKEW2*V(2,N2)+SKEW3*V(3,N2)
        A(1,N2) = A(1,N2)+SKEW1*AA
        A(2,N2) = A(2,N2)+SKEW2*AA
        A(3,N2) = A(3,N2)+SKEW3*AA
C
        IF( ANIM_V(5)+OUTP_V(5)+H3D_DATA%N_VECT_FINT+
     .      ANIM_V(6)+OUTP_V(6)+H3D_DATA%N_VECT_FEXT > 0 
     .           .AND.IMPL_S==0) THEN
          FEXT(1,N2) = FEXT(1,N2)+SKEW1*AA
          FEXT(2,N2) = FEXT(2,N2)+SKEW2*AA
          FEXT(3,N2) = FEXT(3,N2)+SKEW3*AA
        ENDIF
        TFEXTT = TFEXTT + DT1*HALF*(A0+AA)*VV*AXI
      ENDDO
C
#include "atomic.inc"
      TFEXT = TFEXT + TFEXTT
#include "atomend.inc"
C
      RETURN
      END
