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
Chd|  MDS_RESTART                   source/restart/ddsplit/mds_restart.F
Chd|-- called by -----------
Chd|        DDSPLIT                       source/restart/ddsplit/ddsplit.F
Chd|-- calls ---------------
Chd|        WRITE_I_C                     source/output/tools/write_routines.c
Chd|====================================================================
      SUBROUTINE MDS_RESTART()
C======================================================================================================
C   ROUTINE DESCRIPTION :
C   ===================
C   Write MDS parameters in Restart
C-----------------------------------------------
C   DUMMY ARGUMENTS DESCRIPTION:
C   ===================
C======================================================================================================
C
      INTEGER I,J,K
      INTEGER MDS_NMAT,LF1,LF2,LF3,MDSMID,NDEPVAR
C
      INTEGER IFILE1(257)
      INTEGER IFILE2(257)
      INTEGER IFILE3(257)
      INTEGER ILABEL(65)
C
      CHARACTER FILE1(257)
      CHARACTER FILE2(257)
      CHARACTER FILE3(257)
      CHARACTER (len=64), DIMENSION(:) , ALLOCATABLE :: LABEL
      CHARACTER (len=64) LAB
      INTEGER, DIMENSION(:) , ALLOCATABLE ::  LLABEL
C 
      FILE1(1:257)=''
      FILE2(1:257)=''
      FILE3(1:257)=''

      CALL GET_MDS_MATERIALS(MDS_NMAT)
      CALL WRITE_I_C(MDS_NMAT,1)

      IF (MDS_NMAT > 0) THEN
        ALLOCATE(LABEL(1024))
        ALLOCATE(LLABEL(1024))
        LABEL=' '
        DO I=1,MDS_NMAT 
            CALL UNSTACK_MDS_MATERIAL (I,
     *                                 MDS_ID,
     *                                 FILE1, LF1,
     *                                 FILE2, LF2,
     *                                 FILE3, LF3,
     *                                 LABEL, LLABEL,
     *                                 NDEPVAR)

            DO J=1,LF1
                IFILE1(J) = ICHAR(FILE1(J))
            ENDDO

            DO J=1,LF2
                IFILE2(J) = ICHAR(FILE2(J))
            ENDDO

            DO J=1,LF3
                IFILE3(J) = ICHAR(FILE3(J))
            ENDDO



            CALL WRITE_I_C(MDS_ID,1)

            CALL WRITE_I_C(LF1,1)
            CALL WRITE_I_C(IFILE1,LF1)

            CALL WRITE_I_C(LF2,1)
            CALL WRITE_I_C(IFILE2,LF2)

            CALL WRITE_I_C(LF3,1)
            CALL WRITE_I_C(IFILE3,LF3)

            CALL WRITE_I_C(NDEPVAR,1)

            DO J=1,NDEPVAR
                CALL WRITE_I_C(LLABEL(J),1)
                LAB = LABEL(J)
                DO K=1,LLABEL(J)
                  ILABEL(K) = ICHAR(LAB(K:K))
                ENDDO
                CALL WRITE_I_C(ILABEL,LLABEL(J))
            ENDDO



        ENDDO
        DEALLOCATE(LABEL)
        DEALLOCATE(LLABEL)
      ENDIF

      END
