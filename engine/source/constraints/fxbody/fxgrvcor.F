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
!||    fxgrvcor           ../engine/source/constraints/fxbody/fxgrvcor.F
!||--- called by ------------------------------------------------------
!||    resol              ../engine/source/engine/resol.F
!||--- calls      -----------------------------------------------------
!||    finter             ../engine/source/tools/curve/finter.F
!||--- uses       -----------------------------------------------------
!||    finter_mixed_mod   ../engine/source/tools/finter_mixed.F90
!||    python_funct_mod   ../common_source/modules/python_mod.F90
!||====================================================================
      SUBROUTINE FXGRVCOR(FXBIPM, FXBGRVI , A      , IGRV, AGRV,
     .                    NPC   , TF      , MS     , V   , SKEW,
     .                    FXBGRW, IAD_ELEM, FR_ELEM, WFEXT,python)
        use python_funct_mod, only : python_
        use finter_mixed_mod, only : finter_mixed
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "com06_c.inc"
#include      "com08_c.inc"
#include      "param_c.inc"
#include      "fxbcom.inc"
#include      "com01_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER FXBIPM(NBIPM,*), FXBGRVI(*), IGRV(NIGRV,*), NPC(*), IAD_ELEM(2,*), FR_ELEM(*)
      my_real  A(3,*), AGRV(LFACGRV,*), TF(*), MS(*), V(3,*), SKEW(LSKEW,*), FXBGRW(*)
      DOUBLE PRECISION, INTENT(INOUT) :: WFEXT
      TYPE(python_), intent(inout) :: python
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER NFX, NLG, AGRVI, IADG, IG, NL, NNO, ISK, N2, N1, IFUNC,
     .        I, K1, K2, K3, IAGRV(NUMNOD), J, JJ
      my_real
     .        FI0, FI, DYDX, WFEXTT, VV
      my_real FINTER
      EXTERNAL FINTER
C
      DO I=1,NUMNOD
         IAGRV(I)=1
      ENDDO
      IF (NSPMD>1) THEN
         DO I=1,NSPMD
            DO J=IAD_ELEM(1,I),IAD_ELEM(1,I+1)-1
               JJ=FR_ELEM(J)
               IAGRV(JJ)=IAGRV(JJ)+1
            ENDDO
         ENDDO
      ENDIF
C
      WFEXTT=ZERO
      DO NFX=1,NFXBODY
         NLG=FXBIPM(25,NFX)
         AGRVI=FXBIPM(26,NFX)
         IADG=0
         DO IG=1,NLG
            NL=FXBGRVI(AGRVI+IADG)
            NNO=FXBGRVI(AGRVI+IADG+1)
            ISK=IGRV(2,NL)/10
            N2=IGRV(2,NL)-10*ISK
            IFUNC=IGRV(3,NL)
            IF (IFUNC > 0) THEN
              FI0=AGRV(1,NL)*FINTER_MIXED(python,nfunct,IFUNC,(TT-DT1)*AGRV(2,NL),NPC,TF)
              FI =AGRV(1,NL)*FINTER_MIXED(python,nfunct,IFUNC,TT*AGRV(2,NL),NPC,TF)
            ELSE
              FI0= AGRV(1,NL)
              FI = AGRV(1,NL)
            ENDIF
            IF (ISK<=1) THEN
               DO I=1,NNO
                  N1=FXBGRVI(AGRVI+IADG+I+1)
                  A(N2,N1)=A(N2,N1)-FI
                  WFEXTT=WFEXTT+HALF*(FI0+FI)*MS(N1)*V(N2,N1)*DT1
     .                                /IAGRV(N1)
               ENDDO
            ELSE
               K1=3*N2-2
               K2=3*N2-1
               K3=3*N2
               DO I=1,NNO
                  N1=FXBGRVI(AGRVI+IADG+I+1)
                  VV = SKEW(K1,ISK)*V(1,N1)+SKEW(K2,ISK)*V(2,N1)+
     .                 SKEW(K3,ISK)*V(3,N1)
                  A(1,N1)=A(1,N1)-SKEW(K1,ISK)*FI
                  A(2,N1)=A(2,N1)-SKEW(K2,ISK)*FI
                  A(3,N1)=A(3,N1)-SKEW(K3,ISK)*FI
                  WFEXTT=WFEXTT+HALF*(FI0+FI)*MS(N1)*VV*DT1/IAGRV(N1)
               ENDDO
            ENDIF
            IADG=IADG+2+NNO
         ENDDO
         WFEXT=WFEXT-WFEXTT+FXBGRW(NFX)
      ENDDO
C
      RETURN
      END

