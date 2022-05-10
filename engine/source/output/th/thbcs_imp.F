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
Chd|  THBCS_IMP                     source/output/th/thbcs_imp.F  
Chd|-- called by -----------
Chd|        RESOL                         source/engine/resol.F         
Chd|-- calls ---------------
Chd|        BCS1TH_IMP                    source/output/th/bcs1th_imp.F 
Chd|====================================================================
      SUBROUTINE THBCS_IMP(NODFT,NODLAST,A,AR,
     1                     FTHREAC  ,NODREAC ,
     2                     CPTREAC  ,FTHDTM,DT3)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "scr14_c.inc"
#include      "impl1_c.inc"
#include      "impl2_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NODREAC(*),CPTREAC,NODFT,NODLAST
      my_real
     .    FTHREAC(6,*),A(3,*),AR(3,*),DT3,FTHDTM(6,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, N, K, NINDX,
     .        INDX(1024),L,J,FLAG_MOM
C-----------------------------------------------
C    Time History
C-----------------------------------------------
        IF (CPTREAC .GE. 1) THEN
          FLAG_MOM = 0
          INDX = 0
C
          DO 460 I = NODFT,NODLAST,1024
            NINDX = 0
            DO 450 N=I,MIN(NODLAST,I+1023)
              IF (NODREAC(N).GT.0)THEN
                NINDX = NINDX + 1
                INDX(NINDX) = N
              ENDIF
  450       CONTINUE
            CALL BCS1TH_IMP(NINDX,INDX,A,FTHREAC,NODREAC,FLAG_MOM,
     &                      FTHDTM,DT3)
  460     CONTINUE
C
          IF(IRODDL.NE.0) THEN
            FLAG_MOM = 3
            DO 480 I = NODFT,NODLAST,1024
              NINDX = 0
              DO 470 N=I,MIN(NODLAST,I+1023)
                IF (NODREAC(N).GT.0)THEN
                  NINDX = NINDX + 1
                  INDX(NINDX) = N
                ENDIF
  470       CONTINUE
            CALL BCS1TH_IMP(NINDX,INDX,AR,FTHREAC,NODREAC,FLAG_MOM,
     &                      FTHDTM,DT3)
  480       CONTINUE
          ENDIF
        ENDIF
C-----------------------------------------------
      RETURN
      END
