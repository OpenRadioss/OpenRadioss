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
Chd|  PUTSIGNOR3                    source/elements/solid/solide8z/putsignor3.F
Chd|-- called by -----------
Chd|        MMAIN                         source/materials/mat_share/mmain.F
Chd|-- calls ---------------
Chd|        IMP_KTAN                      share/modules/impbufdef_mod.F 
Chd|        IMP_KTAN_DEF                  share/modules/impbufdef_mod.F 
Chd|====================================================================
      SUBROUTINE PUTSIGNOR3(JFT ,JLT ,MTN,IR,IS,IT,AL_IMP ,SIGNOR)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE IMP_KTAN
      USE IMP_KTAN_DEF
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
#include      "impl1_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER JFT, JLT ,MTN,IR,IS,IT
C     REAL
      my_real
     .   AL_IMP(*),SIGNOR(MVSIZ,6)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,MX,J,K,IPTR,IPTS,IPTT
C     REAL

      TYPE(L_KTBUFEP_)    , POINTER :: SKBUF
      TYPE(MLAW_TAG_)     , POINTER :: MTAG
C-----------------------------------------------
       IF (IKT==0) RETURN
       MTAG => KTBUF_STR(NG_IMP)%MLAW_TAG(MTN)
       IF (MTAG%L_A_KT>0) THEN
        IPTR=MAX(1,IR)
        IPTS=MAX(1,IS)
        IPTT=MAX(1,IT)
        SKBUF => KTBUF_STR(NG_IMP)%KTBUFEP(IPTR,IPTS,IPTT)
        DO I=JFT ,JLT
         J=6*(I-1)
         SKBUF%A_KT(I)=AL_IMP(I)
         SKBUF%SIGE(J+1)=SIGNOR(I,1)
         SKBUF%SIGE(J+2)=SIGNOR(I,2)
         SKBUF%SIGE(J+3)=SIGNOR(I,3)
         SKBUF%SIGE(J+4)=SIGNOR(I,4)
         SKBUF%SIGE(J+5)=SIGNOR(I,5)
         SKBUF%SIGE(J+6)=SIGNOR(I,6)
        END DO
       END IF
C
      RETURN
      END
