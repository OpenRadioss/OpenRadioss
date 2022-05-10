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
Chd|  OUTP_MT                       source/output/sty/outp_mt.F   
Chd|-- called by -----------
Chd|        GENOUTP                       source/output/sty/genoutp.F   
Chd|-- calls ---------------
Chd|        FRETITL2                      source/input/freform.F        
Chd|        SPMD_GLOB_DSUM9               source/mpi/interfaces/spmd_th.F
Chd|====================================================================
      SUBROUTINE OUTP_MT(PM,NPART,PARTSAV,IPART,IPM)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "units_c.inc"
#include      "param_c.inc"
#include      "scr05_c.inc"
#include      "task_c.inc"
#include      "scr16_c.inc"
#include      "scr17_c.inc"
#include      "com01_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IPM(NPROPMI,*)
C     REAL ou REAL*8
      my_real
     .   PM(NPROPM,*),PARTSAV(NPSAV,*)
      INTEGER NPART,IPART(LIPART1,*)
      INTEGER LEN
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,USRMID,K,I1,M
      my_real
     .   MAS,XM,YM,ZM,IE,KE,
     .   PARTSAV2(NPSAV,NPART)
      CHARACTER*100 CARD
C
      DO M=1,NPSAV
        DO I=1,NPART
          PARTSAV2(M,I) = PARTSAV(M,I)
        ENDDO
      END DO
C pre traitement SPMD : gather de PARTSAV et mise a zero sur procs remote
      IF(NSPMD > 1) THEN
        CALL SPMD_GLOB_DSUM9(PARTSAV2,NPSAV*NPART)
        IF(ISPMD.NE.0) THEN
          RETURN
        ENDIF
      ENDIF
      IE=ZERO
      KE=ZERO
      MAS=ZERO
      XM=ZERO
      YM=ZERO
      ZM=ZERO      
      I1=IPART(1,1)
      DO K = 1,NPART+1
        IF(K.LE.NPART)THEN
          I=IPART(1,K)
        ELSE
          I=0
        ENDIF
        IF(I1.EQ.0)THEN
         I1=I
        ELSEIF(I1.NE.I)THEN
         CALL FRETITL2(CARD,IPM(NPROPMI-LTITR+1,I1),40)
         WRITE(IUGEO,'(A,I10)')'/MATER     /',I1
         USRMID = IPM(1,I1)
         IF(USRMID.EQ.0) CARD=' '
         IF (OUTYY_FMT.EQ.2) THEN
           WRITE(IUGEO,'(A)')CARD(1:80)
         ELSE
           WRITE(IUGEO,'(A)')CARD
         END IF
         IF (OUTYY_FMT.EQ.2) THEN
          WRITE(IUGEO,'(A)') '#FORMAT: (I8,1P3E16.9/8X,1P3E16.9) '
          WRITE(IUGEO,'(2A)')'# USRMID INTERNAL_ENERGY  KINETIC_ENERGY',
     .                              '            MASS'
          WRITE(IUGEO,'(2A)')'#             X_MOMENTUM      Y_MOMENTUM',
     .                               '      Z_MOMENTUM'
          WRITE(IUGEO,'(I8,1P3E16.9/8X,1P3E16.9)') USRMID,
     .        IE,KE,MAS,XM,YM,ZM
         ELSE
          WRITE(IUGEO,'(A)') '#FORMAT: (I10,1P3E20.13/8X,1P3E20.13) '
          WRITE(IUGEO,'(2A)')'# USRMID INTERNAL_ENERGY  KINETIC_ENERGY',
     .                              '            MASS'
          WRITE(IUGEO,'(2A)')'#             X_MOMENTUM      Y_MOMENTUM',
     .                               '      Z_MOMENTUM'
          WRITE(IUGEO,'(I10,1P3E20.13/8X,1P3E20.13)') USRMID,
     .          IE,KE,MAS,XM,YM,ZM
         ENDIF
         IE=ZERO
         KE=ZERO
         MAS=ZERO
         XM=ZERO
         YM=ZERO
         ZM=ZERO 
         I1=I
        ENDIF
        IF(I.GT.0)THEN
          IE=IE+PARTSAV2(1,K)
          KE=KE+PARTSAV2(2,K)
          MAS=MAS+PARTSAV2(6,K)
          XM=XM+PARTSAV2(3,K)
          YM=YM+PARTSAV2(4,K)
          ZM=ZM+PARTSAV2(5,K)
        ENDIF
      ENDDO
C
      RETURN
      END
