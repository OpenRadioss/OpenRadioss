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
Chd|  DD_ANI                        source/output/anim/dd_ani.F   
Chd|-- called by -----------
Chd|        DOMDEC1                       source/spmd/domain_decomposition/domdec1.F
Chd|-- calls ---------------
Chd|        CLOSE_C                       source/output/tools/write_routines.c
Chd|        CUR_FIL_C                     source/output/tools/write_routines.c
Chd|        PARSOR                        source/output/anim/parsor.F   
Chd|        SCANOR                        source/output/anim/scanor.F   
Chd|        SLAGCNT                       source/output/anim/slagcnt.F  
Chd|        WRITE_C_C                     source/output/tools/write_routines.c
Chd|        WRITE_I_C                     source/output/tools/write_routines.c
Chd|        WRITE_R_C                     source/output/tools/write_routines.c
Chd|        XYZNOD                        source/output/anim/xyznod.F   
Chd|        XYZNOR                        source/output/anim/xyznor.F   
Chd|        INOUTFILE_MOD                 ../common_source/modules/inoutfile_mod.F
Chd|====================================================================
      SUBROUTINE DD_ANI(X     ,IXS   ,IXQ  ,IXC  ,
     .                  IXT   ,IXP   ,IXR  ,IXTG ,
     .                  IPARG ,KXSP  ,IXSP )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE INOUTFILE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "scr15_c.inc"
#include      "param_c.inc"
#include      "units_c.inc"
#include      "r4r8_p.inc"
#include      "sphcom.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
C     REAL
      my_real
     .   X(*)
      INTEGER IXS(*),IXQ(*),IXC(*),KXSP(NISP,*),IXSP(KVOISPH,*),
     .        IXTG(*),IXT(*),IXP(*),IXR(*),IPARG(NPARG,*)
C-----------------------------------------------
C   F u n c t i o n
C-----------------------------------------------
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      my_real
     &  , DIMENSION(:,:), ALLOCATABLE :: D,XNORM
      INTEGER, DIMENSION(:), ALLOCATABLE :: INVERT,IAD
      CHARACTER*80 STR
      CHARACTER CHANIM*3,FILNAM*12
      INTEGER I, NBF, NBPART, MAGIC, J, FILEN,NNSLAG,
     .        IANIM, NBCSPH, N1, N2,
     .        CTEXT(81),IB,PROC
C     REAL
      my_real
     .        CDG(3), S3000, SCALE,BUFEL(1)
      REAL R4
C
C      
      S3000 = THREE1000
C
Calcul connectivite sph pour A000 domdec
C
      NBCSPH = 0
      DO I = 1, NUMSPH
C maillage hexagonal compact => 12 voisins les + proches
	DO J = 1,MIN(12,KXSP(4,I))
	  N1 = KXSP(3,I)
	  N2 = IXSP(J,I)
	  IF(N1<N2) THEN
	    NBCSPH = NBCSPH + 1
	  END IF
	END DO
      END DO
C---------- -------------------------------------
C   INIT SPECIAL STARTER
C-----------------------------------------------
      ALLOCATE(XNORM(3,NUMNOD))
      ALLOCATE(D(3,NUMNOD))
      ALLOCATE(
     &  INVERT(NUMELC+NUMELTG+NUMELQ+6*NUMELS+NUMELT+NUMELP+NUMELR+
     &         NBCSPH))
      ALLOCATE(IAD(NSPMD+1))
      DO I=1,3
        DO J=1,NUMNOD
          D(I,J) = ZERO
        END DO
      END DO
C-----------------------------------------------
C   OPEN FILE
C-----------------------------------------------
      IANIM=0
      WRITE(CHANIM,'(I3.3)')IANIM
      FILNAM=OUTFILE_NAME(1:OUTFILE_NAME_LEN)//ROOTNAM(1:ROOTLEN)//'A'//CHANIM
      FILEN = ROOTLEN + 4 + OUTFILE_NAME_LEN
C
      DO 1 I=1,FILEN
 1    CTEXT(I)=ICHAR(FILNAM(I:I))
      CALL CUR_FIL_C(0)
C     CALL OPEN_C(CTEXT,FILEN)
      CALL OPEN_C(CTEXT,FILEN,0)
C-----------------------------------------------
C   PART COUNT
C-----------------------------------------------
     
      NBPART = 0
      NBPART = NSPMD
C
      CALL SLAGCNT(IPARG,NNSLAG)
C
      NBF = 6*NNSLAG + NUMELQ + NUMELC + NUMELTG + NUMELP
     .               + NUMELT + NUMELR + NFACX + NBCSPH   
C-----------------------------------------------
C   WRITE CONTROL
C-----------------------------------------------
C
C     MAGIC = 0x5429
      MAGIC = 21545
      CALL WRITE_I_C(MAGIC,1)
      CALL WRITE_I_C(NUMNOD,1)
      CALL WRITE_I_C(NBF,1)
      CALL WRITE_I_C(NBPART,1)
      CALL WRITE_I_C(0,1)
      CALL WRITE_I_C(0,1)
      CALL WRITE_I_C(0,1)
C-----------------------------------------------
      R4 = ZERO
      CALL WRITE_R_C(R4,1)
C-----------------------------------------------
C   NODE X Y Z
C-----------------------------------------------
      CALL XYZNOD(X)
      CALL SCANOR(X,D,CDG,SCALE)
C-----------------------------------------------
C   PART SORT
C-----------------------------------------------
      CALL PARSOR(X    ,D    ,XNORM ,IAD  ,CDG   ,
     .            BUFEL,IPARG,IXS   ,IXQ  ,IXC   ,
     .            IXTG ,IXT  ,IXP   ,IXR  ,INVERT,
     .            KXSP ,IXSP  )
C-----------------------------------------------
C   PART ADD
C-----------------------------------------------
       CALL WRITE_I_C(IAD,NBPART)
C-----------------------------------------------
C   PART HEAD
C-----------------------------------------------
       DO PROC = 1, NSPMD
         WRITE(STR,'(A17,I4)') 'Domaine number : ',PROC
         DO J = 1, 21
           CTEXT(J)=ICHAR(STR(J:J))
         END DO
         CTEXT(22) = 0
         CALL WRITE_C_C(CTEXT,81)
       END DO
C-----------------------------------------------
C   NORMAL
C-----------------------------------------------
      CALL XYZNOR(XNORM)
C-----------------------------------------------
C   FUNC TEXT
C-----------------------------------------------
      CTEXT(81)=0
C-----------------------------------------------
      CALL CLOSE_C
C-----------------------------------------------
C   FIN SPECIAL STARTER
C-----------------------------------------------
       DEALLOCATE(XNORM)
       DEALLOCATE(D)
       DEALLOCATE(INVERT)
       DEALLOCATE(IAD)
C-----------------------------------------------
      WRITE (IOUT,1000)  FILNAM(1:FILEN)
      WRITE (ISTDO,1000) FILNAM(1:FILEN)
 1000 FORMAT (1X,'.. ANIMATION FILE:',1X,A,' WRITTEN')
C
      RETURN
      END
