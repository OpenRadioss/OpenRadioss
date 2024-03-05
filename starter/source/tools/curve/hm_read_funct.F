Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2024 Altair Engineering Inc.
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
Chd|  HM_READ_FUNCT                 source/tools/curve/hm_read_funct.F
Chd|-- called by -----------
Chd|        LECTUR                        source/starter/lectur.F       
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        FRETITL                       source/starter/freform.F      
Chd|        HM_GET_FLOATV                 source/devtools/hm_reader/hm_get_floatv.F
Chd|        HM_GET_FLOAT_ARRAY_INDEX      source/devtools/hm_reader/hm_get_float_array_index.F
Chd|        HM_GET_INTV                   source/devtools/hm_reader/hm_get_intv.F
Chd|        HM_OPTION_COUNT               source/devtools/hm_reader/hm_option_count.F
Chd|        HM_OPTION_IS_ENCRYPTED        source/devtools/hm_reader/hm_option_is_encrypted.F
Chd|        HM_OPTION_READ_KEY            source/devtools/hm_reader/hm_option_read_key.F
Chd|        HM_OPTION_START               source/devtools/hm_reader/hm_option_start.F
Chd|        HM_OPTION_READ_MOD            share/modules1/hm_option_read_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|        TABLE_MOD                     share/modules1/table_mod.F    
Chd|====================================================================
      SUBROUTINE HM_READ_FUNCT(NPC     ,PLD      ,NFUNCT ,TABLE    ,NPTS_ALLOC,
     .                         NOM_OPT ,FUNCRYPT ,UNITAB ,LSUBMODEL)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE TABLE_MOD
      USE MESSAGE_MOD
      USE SUBMODEL_MOD
      USE HM_OPTION_READ_MOD
      USE UNITAB_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "scr17_c.inc"
#include      "units_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NFUNCT, NPTS_ALLOC
      INTEGER NPC(*),FUNCRYPT(*)
C     REAL
      my_real
     .   PLD(NPTS_ALLOC)
      TYPE(TTABLE) TABLE(*)
      INTEGER NOM_OPT(LNOPT1,*)
      TYPE(SUBMODEL_DATA), DIMENSION(NSUBMOD), INTENT(IN) :: LSUBMODEL
      TYPE(UNIT_TYPE_), INTENT(IN) :: UNITAB
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,L,FUNC_ID,NPTS,STAT,N,II,ISMOOTH
      INTEGER :: IPYTHON ! is it a FUNCT_PYTHON
C     REAL
      my_real
     .   TIME, FUNCT, BID, F5(5)
      my_real
     .   XSCALE,YSCALE,XSHIFT,YSHIFT
      CHARACTER TITR*nchartitle,MESS*40,KEY*20
      DATA MESS/' FUNCTION & TABLES DEFINITION           '/
      LOGICAL :: IS_ENCRYPTED, IS_AVAILABLE
      INTEGER :: NB_FUNCT, NB_FUNCT_SMOOTH, IPT, NPT
C--------------------------------------------------
C     B e g i n n i n g   o f   S u b r o u t i n e
C--------------------------------------------------
      IF (NFUNCT == 0) RETURN

      STAT = 0

      WRITE (IOUT,2000) NFUNCT

!     Initialization
      IS_ENCRYPTED = .FALSE.
      IS_AVAILABLE = .FALSE.

      NPC(1)=1
      L     =0
C--------------------------------------------------
C READING /FUNCT ( ISMOOTH = 0, IPYTHON = 0)
C--------------------------------------------------
      CALL HM_OPTION_COUNT('/FUNCT', NB_FUNCT)
      CALL HM_OPTION_COUNT('/FUNCT_SMOOTH', NB_FUNCT_SMOOTH)
      IF (NB_FUNCT > 0) THEN
         CALL HM_OPTION_START('/FUNCT')
         DO I = 1, NB_FUNCT
c
            CALL HM_OPTION_READ_KEY(LSUBMODEL, 
     .                              OPTION_TITR = TITR,
     .                              OPTION_ID = FUNC_ID,
     .                              KEYWORD1 = KEY)
c 
            ISMOOTH = 0
            IPYTHON = 0
            IF(KEY(6:12) == '_SMOOTH') ISMOOTH = 1
            IF(KEY(6:12) == '_PYTHON') IPYTHON = 1
            
            IF(ISMOOTH == 0 .AND. IPYTHON == 0 ) THEN
              CALL HM_OPTION_IS_ENCRYPTED(IS_ENCRYPTED)   
              L = L + 1   
              CALL FRETITL(TITR,NOM_OPT(LNOPT1-LTITR+1,L),LTITR)                         
              NOM_OPT(1, L) = FUNC_ID
              NPC(NFUNCT + L + 1) = FUNC_ID                                       
              NPC(2 * NFUNCT + L + 1) = ISMOOTH
              NPC(L + 1) = NPC(L)                                          
              NPTS = 0   
              WRITE(IOUT, 2100) FUNC_ID
!     Number of points
              CALL HM_GET_INTV('numberofpoints', NPT, IS_AVAILABLE, LSUBMODEL)
c
              DO IPT = 1, NPT
                 CALL HM_GET_FLOAT_ARRAY_INDEX('points', TIME, 2 * IPT - 1, IS_AVAILABLE, LSUBMODEL, UNITAB)
                 CALL HM_GET_FLOAT_ARRAY_INDEX('points', FUNCT, 2 * IPT, IS_AVAILABLE, LSUBMODEL, UNITAB)             
                 IF (.NOT. IS_ENCRYPTED) THEN
                    WRITE(IOUT,'(3X,1PG20.13,2X,1G20.13)') TIME,FUNCT
                 ENDIF
                 NPTS = NPTS + 1 
                 PLD(NPC(L + 1)) = TIME                                     
                 IF (NPTS > 1) THEN  
                    IF (PLD(NPC(L+1)) <= PLD(NPC(L+1)-2)) THEN
!     Decreasing time line
                       CALL ANCMSG(MSGID = 156, MSGTYPE = MSGERROR, ANMODE = ANINFO_BLIND_1,
     .                      I1 = FUNC_ID, C1 = TITR, I2 = NPTS, I3 = NPTS-1)
                    ENDIF
                 ENDIF     
                 NPC(L + 1) = NPC(L + 1) + 1                                      
                 PLD(NPC(L + 1)) = FUNCT                                         
                 NPC(L + 1) = NPC(L + 1) + 1
              ENDDO
c
              IF (NPT < 2) THEN           
                CALL ANCMSG(MSGID=1874, MSGTYPE=MSGWARNING, ANMODE=ANINFO_BLIND_1,
     .                      I1=FUNC_ID,
     .                      C1=TITR)
              END IF           
c
!     build table structure
              TABLE(L)%NOTABLE=FUNC_ID
              TABLE(L)%NDIM =1
!     
              ALLOCATE(TABLE(L)%X(1),STAT=stat)
              IF (STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .             MSGTYPE=MSGERROR,
     .             C1='TABLE')
              ALLOCATE(TABLE(L)%X(1)%VALUES(NPTS),STAT=stat)
              IF (STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .             MSGTYPE=MSGERROR,
     .             C1='TABLE')
              ALLOCATE(TABLE(L)%Y,STAT=stat)
              IF (STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .             MSGTYPE=MSGERROR,
     .             C1='TABLE')
              ALLOCATE(TABLE(L)%Y%VALUES(NPTS),STAT=stat)
              IF (STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .             MSGTYPE=MSGERROR,
     .             C1='TABLE')
!     
              DO N=1,NPTS
                 TABLE(L)%X(1)%VALUES(N)=PLD(NPC(L)+2*N-2)
                 TABLE(L)%Y%VALUES(N)   =PLD(NPC(L)+2*N-1)
              ENDDO
              IF (IS_ENCRYPTED) THEN
                 WRITE(IOUT,'(A)')'CONFIDENTIAL DATA'
                 FUNCRYPT(L) = 1
              ENDIF
            ENDIF
         ENDDO
      ENDIF
C--------------------------------------------------
C READING /FUNCT_SMOOTH ( ISMOOTH = 1)
C--------------------------------------------------
      IF (NB_FUNCT_SMOOTH > 0) THEN
         CALL HM_OPTION_START('/FUNCT_SMOOTH')
         DO I = 1, NB_FUNCT_SMOOTH
c
            CALL HM_OPTION_READ_KEY(LSUBMODEL, 
     .                              OPTION_TITR = TITR,
     .                              OPTION_ID = FUNC_ID,
     .                              KEYWORD1 = KEY)
c
            CALL HM_OPTION_IS_ENCRYPTED(IS_ENCRYPTED)  
            ISMOOTH = 1
            L = L + 1 
            CALL FRETITL(TITR,NOM_OPT(LNOPT1-LTITR+1,L),LTITR)                           
            NOM_OPT(1, L) = FUNC_ID
            NPC(NFUNCT + L + 1) = FUNC_ID                                       
            NPC(2 * NFUNCT + L + 1) = ISMOOTH
            NPC(L + 1) = NPC(L)                                          
            NPTS = 0  
            WRITE(IOUT, 2200) FUNC_ID
c
            CALL HM_GET_FLOATV('A_SCALE_X' ,XSCALE ,IS_AVAILABLE ,LSUBMODEL ,UNITAB)
            CALL HM_GET_FLOATV('F_SCALE_Y' ,YSCALE ,IS_AVAILABLE ,LSUBMODEL ,UNITAB)
            CALL HM_GET_FLOATV('A_SHIFT_X' ,XSHIFT ,IS_AVAILABLE ,LSUBMODEL ,UNITAB)
            CALL HM_GET_FLOATV('F_SHIFT_Y' ,YSHIFT ,IS_AVAILABLE ,LSUBMODEL ,UNITAB)
            IF (XSCALE == ZERO) XSCALE = ONE          
            IF (YSCALE == ZERO) YSCALE = ONE

            WRITE (IOUT,2300)
            IF (.NOT. IS_ENCRYPTED) 
     .           WRITE(IOUT,'(3X,1PG20.13,3(2X,1G20.13))') XSCALE,YSCALE,XSHIFT,YSHIFT
            WRITE (IOUT,2400)

!     Number of points
            CALL HM_GET_INTV('numberofpoints', NPT, IS_AVAILABLE, LSUBMODEL)
c
            DO IPT = 1, NPT
               CALL HM_GET_FLOAT_ARRAY_INDEX('points', TIME, 2 * IPT - 1, IS_AVAILABLE, LSUBMODEL, UNITAB)
               CALL HM_GET_FLOAT_ARRAY_INDEX('points', FUNCT, 2 * IPT, IS_AVAILABLE, LSUBMODEL, UNITAB)
c
               TIME  = TIME  * XSCALE + XSHIFT
               FUNCT = FUNCT * YSCALE + YSHIFT
c             
               IF (.NOT. IS_ENCRYPTED) THEN
                  WRITE(IOUT,'(3X,1PG20.13,2X,1G20.13)') TIME,FUNCT
               ENDIF
               NPTS = NPTS + 1 
               PLD(NPC(L + 1)) = TIME                                     
               IF (NPTS > 1) THEN  
                  IF (PLD(NPC(L+1)) <= PLD(NPC(L+1)-2)) THEN
!     Decreasing time line
                     CALL ANCMSG(MSGID = 156, MSGTYPE = MSGERROR, ANMODE = ANINFO_BLIND_1,
     .                    I1 = FUNC_ID, C1 = TITR, I2 = NPTS, I3 = NPTS-1)
                  ENDIF
               ENDIF     
               NPC(L + 1) = NPC(L + 1) + 1                                      
               PLD(NPC(L + 1)) = FUNCT                                          
               NPC(L + 1) = NPC(L + 1) + 1
            ENDDO
c
            IF (NPT < 2) THEN           
              CALL ANCMSG(MSGID=1874, MSGTYPE=MSGWARNING, ANMODE=ANINFO_BLIND_1,
     .                    I1=FUNC_ID,
     .                    C1=TITR)
            END IF           
c
!     build table structure
            TABLE(L)%NOTABLE=FUNC_ID
            TABLE(L)%NDIM =1
!     
            ALLOCATE(TABLE(L)%X(1),STAT=stat)
            IF (STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .           MSGTYPE=MSGERROR,
     .           C1='TABLE')
            ALLOCATE(TABLE(L)%X(1)%VALUES(NPTS),STAT=stat)
            IF (STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .           MSGTYPE=MSGERROR,
     .           C1='TABLE')
            ALLOCATE(TABLE(L)%Y,STAT=stat)
            IF (STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .           MSGTYPE=MSGERROR,
     .           C1='TABLE')
            ALLOCATE(TABLE(L)%Y%VALUES(NPTS),STAT=stat)
            IF (STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .           MSGTYPE=MSGERROR,
     .           C1='TABLE')
!     
            DO N=1,NPTS
               TABLE(L)%X(1)%VALUES(N)=PLD(NPC(L)+2*N-2)
               TABLE(L)%Y%VALUES(N)   =PLD(NPC(L)+2*N-1)
            ENDDO
            IF (IS_ENCRYPTED) THEN
              WRITE(IOUT,'(A)')'CONFIDENTIAL DATA'
              FUNCRYPT(L) = 1
            ENDIF
         ENDDO
      ENDIF

C     
      RETURN
C-----------------------------------------------------------------
2000  FORMAT(//
     .       '    LOAD CURVES'/
     .       '    -----------'/
     .       '    NUMBER OF LOAD CURVES. . . . . . . . =',I10/)
2100  FORMAT(/'    LOAD CURVE ID . . . . . . . . . . . =',I10//
     .        '    X                     Y               ')
2200  FORMAT(/'    LOAD SMOOTH CURVE ID . . . . . .  . =',I10)
2300  FORMAT(/'    XSCALE                YSCALE                XSHIFT
     .                YSHIFT              ')
2400  FORMAT(/'    X                     Y               ')
      END
