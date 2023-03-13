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
Chd|  SMS_THBCS                     source/ams/sms_thbcs.F        
Chd|-- called by -----------
Chd|        SMS_MASS_SCALE_2              source/ams/sms_mass_scale_2.F 
Chd|-- calls ---------------
Chd|        SMS_BCS1TH                    source/ams/sms_bcs1th.F       
Chd|====================================================================
      SUBROUTINE SMS_THBCS(NODFT ,NODLAST,INDX1   ,ICODT  ,ICODR   ,
     2                     ISKEW ,SKEW  ,A       ,AR      ,FTHREAC ,
     3                     NODREAC,CPTREAC)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NODLAST,INDX1(*),CPTREAC
      INTEGER NODFT,ICODT(*),ICODR(*),ISKEW(*),NODREAC(*)
      my_real
     .    A(3,*),AR(3,*),SKEW(LSKEW,*),FTHREAC(6,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J, N, K, ISK, LCOD, NINDX,
     .        INDX(1024),L,FLAG_MOM
C-----------------------------------------------
C
      IF (CPTREAC >= 1) THEN
        FLAG_MOM = 0
        DO 420 I = NODFT,NODLAST,1024
          NINDX = 0
          DO 400 J=I,MIN(NODLAST,I+1023)
            N=INDX1(J)
            IF (ICODT(N)/=0 .AND. NODREAC(N)/=0)THEN
              NINDX = NINDX + 1
              INDX(NINDX) = N
            ENDIF
  400     CONTINUE
          CALL SMS_BCS1TH(NINDX ,INDX   ,ISKEW  ,ICODT  ,A    ,
     2                    SKEW  ,FTHREAC,NODREAC,FLAG_MOM)
  420   CONTINUE
C
        IF(IRODDL/=0) THEN
          FLAG_MOM = 1
          DO 440 I = NODFT,NODLAST,1024
            NINDX = 0
            DO 430 J=I,MIN(NODLAST,I+1023)
              N=INDX1(J)
              IF (ICODR(N)/=0 .AND. NODREAC(N)>0)THEN
                NINDX = NINDX + 1
                INDX(NINDX) = N
              ENDIF
  430       CONTINUE
            CALL SMS_BCS1TH(NINDX,INDX,ISKEW,ICODR,AR,
     &                  SKEW,FTHREAC,NODREAC,FLAG_MOM)
  440     CONTINUE
        ENDIF
      ENDIF
C
      RETURN
      END

