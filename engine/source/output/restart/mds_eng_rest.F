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
Chd|  ENG_READ_MDS                  source/output/restart/mds_eng_rest.F
Chd|-- called by -----------
Chd|        RDRESB                        source/output/restart/rdresb.F
Chd|-- calls ---------------
Chd|        READ_I_C                      source/output/tools/sortie_c.c
Chd|        MDS_RESTART_MOD               ../common_source/modules/mds_rest.mod.F
Chd|====================================================================
      SUBROUTINE ENG_READ_MDS()
C======================================================================================================
C   ROUTINE DESCRIPTION :
C   ===================
C   Write MDS parameters in Restart
C-----------------------------------------------
C   DUMMY ARGUMENTS DESCRIPTION:
C   ===================
C======================================================================================================
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MDS_RESTART_MOD 
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,K
      INTEGER LF,LLABEL,MDSMID,NDEPVAR
      INTEGER IFILE(256)
      INTEGER S
      CHARACTER*256 FIL
      CHARACTER (LEN=64) LAB
C-----------------------------------------------

      CALL READ_I_C(MDS_NMAT,1)

      ALLOCATE(MDS_MATID(MDS_NMAT))
      ALLOCATE(MDS_NDEPSVAR(MDS_NMAT))
      ALLOCATE(MDS_LABEL(1024,MDS_NMAT))
      ALLOCATE(MDS_FILES(256,3,MDS_NMAT))

      MDS_LABEL=' ' 
      MDS_FILES=' ' 

         call my_flush(6)
       DO I=1,MDS_NMAT

         CALL READ_I_C(MDS_MATID(I),1)
       
         ! File 1
         ! -------
         CALL READ_I_C(LF,1)
         CALL READ_I_C(IFILE,LF)
         MDS_FILES(1:256,1,I)=''
         DO J=1,LF
           MDS_FILES(J,1,I)=CHAR(IFILE(J))
         ENDDO

         ! File 2
         ! -------
         CALL READ_I_C(LF,1)
         CALL READ_I_C(IFILE,LF)
         MDS_FILES(1:256,2,I)=''
         DO J=1,LF
           MDS_FILES(J,2,I)=CHAR(IFILE(J))
         ENDDO

         ! File 3
         ! -------
         CALL READ_I_C(LF,1)
         CALL READ_I_C(IFILE,LF)
         MDS_FILES(1:256,3,I)=''
         DO J=1,LF
           MDS_FILES(J,3,I)=CHAR(IFILE(J))
         ENDDO

         ! LABEL
         ! ------
         CALL READ_I_C(MDS_NDEPSVAR(I),1)
         
         DO J=1,MDS_NDEPSVAR(I)
           CALL READ_I_C(LLABEL,1)
           CALL READ_I_C(IFILE,LLABEL)

           LAB = ' ' 
           DO K=1,LLABEL
             LAB(K:K)=CHAR(IFILE(K))
           ENDDO
           MDS_LABEL(J,I) = LAB
         ENDDO

         ! debug prints
         ! ------------
         ! print*,' ' 
         ! print*,'fil1:',MDS_FILES(1:64,1,I)
         ! print*,'fil2:',MDS_FILES(1:64,2,I)
         ! print*,'fil3:',MDS_FILES(1:64,3,I)
         ! print*,'label:',MDS_LABEL(1:64,I)
         ! print*,'NDEPSVAR:',MDS_NDEPSVAR(I) 
         ! print*,' ' 

      ENDDO

      END

Chd|====================================================================
Chd|  ENG_WRT_MDS                   source/output/restart/mds_eng_rest.F
Chd|-- called by -----------
Chd|        WRRESTP                       source/output/restart/wrrestp.F
Chd|-- calls ---------------
Chd|        WRITE_I_C                     source/output/tools/sortie_c.c
Chd|        MDS_RESTART_MOD               ../common_source/modules/mds_rest.mod.F
Chd|====================================================================
      SUBROUTINE ENG_WRT_MDS()
C======================================================================================================
C   ROUTINE DESCRIPTION :
C   ===================
C   Write MDS parameters in Restart
C-----------------------------------------------
C   DUMMY ARGUMENTS DESCRIPTION:
C   ===================
C======================================================================================================
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MDS_RESTART_MOD 
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,K
      INTEGER LF,LLABEL,MDSMID,NDEPVAR
      INTEGER IFILE(256) 
      CHARACTER (LEN=256) SFIL
      
C-----------------------------------------------

      CALL WRITE_I_C(MDS_NMAT,1)


      DO I=1,MDS_NMAT

         ! print*,' '
         ! print*,'-----------'
         ! print*,i

         CALL WRITE_I_C(MDS_MATID(I),1)
       
         ! File 1
         ! -------
         DO J=1,256
           SFIL(J:J) = MDS_FILES(J,1,I)
         ENDDO
         LF=LEN_TRIM(SFIL)
         DO J=1,LF
           IFILE(J)= ICHAR(SFIL(J:J))
         ENDDO
         ! print*,'SFIL-1:',SFIL(1:LF)

         CALL WRITE_I_C(LF,1)
         CALL WRITE_I_C(IFILE,LF)

         ! File 2
         ! -------
         DO J=1,256
           SFIL(J:J) = MDS_FILES(J,2,I)
         ENDDO
         LF=LEN_TRIM(SFIL)
         DO J=1,LF
           IFILE(J)= ICHAR(SFIL(J:J))
         ENDDO
         ! print*,'SFIL-2:',SFIL(1:LF)
         CALL WRITE_I_C(LF,1)
         CALL WRITE_I_C(IFILE,LF)

         ! File 3
         ! -------
         DO J=1,256
           SFIL(J:J) = MDS_FILES(J,3,I)
         ENDDO
         LF=LEN_TRIM(SFIL)
         DO J=1,LF
           IFILE(J)= ICHAR(SFIL(J:J))
         ENDDO
         ! print*,'SFIL-3:',SFIL(1:LF)
         CALL WRITE_I_C(LF,1)
         CALL WRITE_I_C(IFILE,LF)

         ! LABEL
         ! ------
         CALL WRITE_I_C(MDS_NDEPSVAR(I),1)
         DO J=1,MDS_NDEPSVAR(I)

             SFIL = MDS_LABEL(J,I)
             LF=LEN_TRIM(SFIL)
             
             DO K=1,LF
                IFILE(K)= ICHAR(SFIL(K:K))
             ENDDO
             CALL WRITE_I_C(LF,1)
             CALL WRITE_I_C(IFILE,LF)

         ENDDO

      ENDDO

      END

