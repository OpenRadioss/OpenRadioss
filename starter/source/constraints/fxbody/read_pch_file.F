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
!||    read_pch_file          ../starter/source/constraints/fxbody/read_pch_file.F
!||--- called by ------------------------------------------------------
!||    hm_read_fxb1           ../starter/source/constraints/fxbody/hm_read_fxb.F
!||    hm_setfxrbyon          ../starter/source/constraints/fxbody/hm_setfxrbyon.F
!||--- calls      -----------------------------------------------------
!||    ancmsg                 ../starter/source/output/message/message.F
!||    fxrline                ../starter/source/constraints/fxbody/hm_read_fxb.F
!||    usr2sys                ../starter/source/system/sysfus.F
!||--- uses       -----------------------------------------------------
!||    message_mod            ../starter/share/message_module/message_mod.F
!||====================================================================
      SUBROUTINE READ_PCH_FILE(FLAG,MATRIX,ITAG,MATRIX_ADD,CPT_STIFF,
     .                         CPT_MASS,ITABM1,PCH_FILE,ID,TITR)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE INOUTFILE_MOD
      USE MESSAGE_MOD
      USE NAMES_AND_TITLES_MOD , ONLY : NCHARTITLE
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER ID,ITABM1(*),ITAG(*),MATRIX_ADD(4,*), CPT_STIFF,CPT_MASS,FLAG
      my_real MATRIX(*)
      CHARACTER PCH_FILE*100
      CHARACTER(LEN=NCHARTITLE) :: TITR
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,REF,IFO,TIN,TOUT,POLAR,NCOL,DUM,ID_NOD1,ID_NOD2,J,ISTOP
      my_real RELL
      CHARACTER DEBUT*8,NAME*8,MATRIX_TYPE*16,NWLINE*100,MESS*40

      INTEGER :: LEN_TMP_NAME,I_ERR
      CHARACTER(len=2148) :: TMP_NAME
C-----------------------------------------------
C   E x t e r n a l   F u n c t i o n s
C-----------------------------------------------
      INTEGER USR2SYS
      DATA MESS/'FLEXIBLE BODY DEF - READ OF PCH FILE '/
C=======================================================================
C
C     FLAG = 0 -> PREREADING FOR NODES DETECTION AND NUMBERING (SETRFXBYON.F)
C     FLAG = 1 -> READING OF NODES AND MATRIX (LECFXB1.F)
C
      REF = 71
      I_ERR = 0  
C
      LEN_TMP_NAME = INFILE_NAME_LEN+len_trim(PCH_FILE)
      TMP_NAME=INFILE_NAME(1:INFILE_NAME_LEN)//PCH_FILE(1:len_trim(PCH_FILE))
      OPEN(UNIT=REF,FILE=TMP_NAME(1:LEN_TMP_NAME),
     .      ACCESS='SEQUENTIAL',FORM='FORMATTED',STATUS='UNKNOWN')  
C
      CALL FXRLINE(REF,NWLINE,ID,TITR)
      READ(NWLINE,'((A,A,5I8,I16))') DEBUT,NAME,DUM,IFO,TIN,TOUT,POLAR,NCOL
C
      IF ((IFO == 6).AND.(TIN < 3)) THEN
C
C-------  Symmetric matrix - real (no imaginary part) -----------
C
C       Reading of a new block --
        DO WHILE (I_ERR == 0)
C
          ISTOP=0
          DO WHILE (ISTOP==0)
            READ(REF,'(A)',END=100) NWLINE
            IF (NWLINE(1:5)=='DMIG ') THEN
              READ(NWLINE,'((A,A,5I8,I16))') DEBUT,NAME,DUM,IFO,TIN,TOUT,POLAR,NCOL
            ELSEIF ((NWLINE(1:1)/='#').AND.((NWLINE(1:1)/='$')).AND.((LEN_TRIM(NWLINE)/=0))) THEN
              ISTOP=1
            ENDIF
          ENDDO
C
          READ(NWLINE,'((A,A,2I16))') DEBUT,MATRIX_TYPE,ID_NOD1,I
          ID_NOD1=USR2SYS(ID_NOD1,ITABM1,MESS,ID)
          IF (ID_NOD1 /= 0) THEN
            ITAG(ID_NOD1) = 1
          ELSE
            RETURN
          ENDIF
C
          CALL FXRLINE(REF,NWLINE,ID,TITR)
C
          IF (ADJUSTL(MATRIX_TYPE) == 'KAAX') THEN
            DO WHILE (NWLINE(1:1) == '*')
              READ(NWLINE,'((A,I16,I16,F16.3))') DEBUT,ID_NOD2,J,RELL
              CPT_STIFF = CPT_STIFF + 1
              ID_NOD2=USR2SYS(ID_NOD2,ITABM1,MESS,ID)
              IF (ID_NOD2 /= 0) THEN
                ITAG(ID_NOD2) = 1
                IF (FLAG == 1) THEN
                  MATRIX(CPT_STIFF-1) = RELL
                  MATRIX_ADD(1,CPT_STIFF-1) = ID_NOD1   
                  MATRIX_ADD(2,CPT_STIFF-1) = ID_NOD2
                  MATRIX_ADD(3,CPT_STIFF-1) = I 
                  MATRIX_ADD(4,CPT_STIFF-1) = J 
                ENDIF
              ELSE
                RETURN
              ENDIF   
              READ(REF,'(A)',IOSTAT=I_ERR,END=100) NWLINE
            END DO
C
          ELSEIF (ADJUSTL(MATRIX_TYPE) == 'MAAX') THEN
            DO WHILE (NWLINE(1:1) == '*')
              READ(NWLINE,'((A,I16,I16,F16.3))') DEBUT,ID_NOD2,J,RELL
              CPT_MASS = CPT_MASS + 1
              ID_NOD2=USR2SYS(ID_NOD2,ITABM1,MESS,ID)
              IF (ID_NOD2 /= 0) THEN
                IF (FLAG == 1) THEN
                  MATRIX(CPT_MASS-1) = RELL
                  MATRIX_ADD(1,CPT_MASS-1) = ID_NOD1   
                  MATRIX_ADD(2,CPT_MASS-1) = ID_NOD2
                  MATRIX_ADD(3,CPT_MASS-1) = I 
                  MATRIX_ADD(4,CPT_MASS-1) = J 
                ENDIF
              ELSE
                RETURN
              ENDIF   
              READ(REF,'(A)',IOSTAT=I_ERR,END=100) NWLINE
            END DO
C
          ELSEIF ((ADJUSTL(MATRIX_TYPE)=='BAAX').OR.(ADJUSTL(MATRIX_TYPE)=='PAX').OR.ADJUSTL(MATRIX_TYPE)=='K4AAX') THEN
C--         Input not compatible
            I_ERR = 1
            IF (FLAG == 0) THEN
              CALL ANCMSG(MSGID=1738,
     .                  MSGTYPE=MSGERROR,
     .                  ANMODE=ANINFO_BLIND_1,
     .                  I1=ID,
     .                  C1=TITR,
     .                  C2=ADJUSTL(MATRIX_TYPE))
            ENDIF            
C
          ENDIF      
C
          BACKSPACE(REF)
C
        END DO
C
100     CONTINUE
C
      ENDIF
C
      CLOSE(UNIT=REF,STATUS='KEEP')                            
C 
      RETURN     
C
      END SUBROUTINE READ_PCH_FILE
