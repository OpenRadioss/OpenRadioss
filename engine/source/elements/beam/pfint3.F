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
Chd|  PFINT3                        source/elements/beam/pfint3.F 
Chd|-- called by -----------
Chd|        PFORC3                        source/elements/beam/pforc3.F 
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE PFINT3(FOR    ,MOM    ,GEO    ,OFFG   ,OFF,
     .                  AL     ,F1     ,F2     ,F3     ,M1 ,
     .                  M2     ,M3     ,STI    ,STIR   ,NEL,
     .                  PID    ,F11    ,F12    ,F21    ,F22,
     .                  F31    ,F32    ,M11    ,M12    ,M21,
     .                  M22    ,M31    ,M32    )
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
#include      "vect01_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NEL,PID(MVSIZ)
C     REAL
      my_real
     .   FOR(NEL,3),MOM(NEL,3),GEO(NPROPG,*),OFFG(*),OFF(*),AL(MVSIZ), 
     .   F1(MVSIZ),F2(MVSIZ),F3(MVSIZ),M1(MVSIZ),M2(MVSIZ),M3(MVSIZ),
     .   STI(MVSIZ), STIR(MVSIZ),
     .   F11(MVSIZ), F12(MVSIZ), F21(MVSIZ),  
     .   F22(MVSIZ), F31(MVSIZ), F32(MVSIZ), 
     .   M11(MVSIZ), M12(MVSIZ), M21(MVSIZ),
     .   M22(MVSIZ), M31(MVSIZ), M32(MVSIZ)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J, IG, IR1Y, IR1Z, IR2Y, IR2Z
C     REAL
      my_real
     .   OFF_L
C---------------------------------------------------
      DO I=LFT,LLT
        F11(I)= -F1(I)
        F21(I)= -F2(I)
        F31(I)= -F3(I)
C
        F12(I)=-F11(I)
        F22(I)=-F21(I)
        F32(I)=-F31(I)
      ENDDO
C
C     CONTRIBUTION AU MOMENT DU CISAILLEMENT
C
      DO I=LFT,LLT
        M11(I)=-M1(I)
        M21(I)=-M2(I)+HALF*AL(I)*F3(I)
        M31(I)=-M3(I)-HALF*AL(I)*F2(I)
C
        M12(I)=-M11(I)
        M22(I)=M2(I)+HALF*AL(I)*F3(I)
        M32(I)=M3(I)-HALF*AL(I)*F2(I)
      ENDDO
C---------------------------------------------------
C     LIBERATION DES ROTATIONS
C---------------------------------------------------
      DO I=LFT,LLT
        IG=PID(I)
        IR1Y=NINT(GEO(8 ,IG))
        IR1Z=NINT(GEO(9 ,IG))
        IR2Y=NINT(GEO(10,IG))
        IR2Z=NINT(GEO(11,IG))
C ... this looks suspicious - DWS
        M21(I)=IR1Y*M21(I)+(1-IR2Y)*M22(I)
        M22(I)=IR2Y*M22(I)+(1-IR1Y)*M21(I)
        M31(I)=IR1Z*M31(I)+(1-IR2Z)*M32(I)
        M32(I)=IR2Z*M32(I)+(1-IR1Z)*M31(I)
      ENDDO
C
      OFF_L = ZERO
      DO I=LFT,LLT
        IF (OFF(I) < ONE) OFFG(I) = OFF(I)
        OFF_L = MIN(OFF_L,OFFG(I))
      ENDDO
      IF (OFF_L < ZERO) THEN
        DO I=LFT,LLT
          IF (OFFG(I) < ZERO) THEN
            F11(I)=ZERO
            F21(I)=ZERO
            F31(I)=ZERO
            M11(I)=ZERO
            M21(I)=ZERO
            M31(I)=ZERO
            F12(I)=ZERO
            F22(I)=ZERO
            F32(I)=ZERO
            M12(I)=ZERO
            M22(I)=ZERO
            M32(I)=ZERO
            STI(I)=ZERO
            STIR(I)=ZERO
          ENDIF
        ENDDO
      ENDIF
C---------------------------------------------------
      RETURN
      END
