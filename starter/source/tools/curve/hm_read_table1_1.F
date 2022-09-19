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
Chd|  HM_READ_TABLE1_1              source/tools/curve/hm_read_table1_1.F
Chd|-- called by -----------
Chd|        HM_READ_TABLE1                source/tools/curve/hm_read_table.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        HM_GET_FLOAT_ARRAY_INDEX      source/devtools/hm_reader/hm_get_float_array_index.F
Chd|        HM_GET_INTV                   source/devtools/hm_reader/hm_get_intv.F
Chd|        HM_OPTION_IS_ENCRYPTED        source/devtools/hm_reader/hm_option_is_encrypted.F
Chd|        HM_OPTION_READ_KEY            source/devtools/hm_reader/hm_option_read_key.F
Chd|        HM_OPTION_START               source/devtools/hm_reader/hm_option_start.F
Chd|        HM_OPTION_READ_MOD            share/modules1/hm_option_read_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|        TABLE_MOD                     share/modules1/table_mod.F    
Chd|====================================================================
      SUBROUTINE HM_READ_TABLE1_1(NTABLE1,NTABLE, TABLE ,NFUNCT,
     .                            NPC ,PLD,L,
     .                            NOM_OPT,UNITAB, LSUBMODEL)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE TABLE_MOD
      USE MESSAGE_MOD
      USE SUBMODEL_MOD
      USE HM_OPTION_READ_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   A n a l y s e   M o d u l e
C-----------------------------------------------
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NTABLE, NTABLE1,L,NFUNCT,NPC(*)
C     REAL
      my_real
     .    PLD(*)
      TYPE(TTABLE) TABLE(*)
      INTEGER NOM_OPT(LNOPT1,*)
      TYPE(SUBMODEL_DATA), DIMENSION(NSUBMOD), INTENT(IN) :: LSUBMODEL
      my_real, INTENT(IN) :: UNITAB(LUNIT, NUNITS)
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "scr17_c.inc"
#include      "scr03_c.inc"
#include      "units_c.inc"
#include      "warn_c.inc"
#include      "submod_c.inc"
#include      "sysunit.inc"
C-----------------------------------------------
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER ITYPE, IBID, NFUNCT0
      INTEGER I, J, K, II, N, NDIM, NX(4), NY,
     .        JREC, NPTS, STAT, LL,NPT,IPT
      INTEGER IERROR, NF, IDFUNC, NP
C     REAL
      my_real
     .   BID, F5(5), TIME, FUNCT
      CHARACTER TITR*nchartitle, MESS*40, KEY*ncharfield,
     .          TITR1*nchartitle
      DATA MESS/' FUNCTION & TABLE DEFINITION            '/
      LOGICAL :: IS_ENCRYPTED, IS_AVAILABLE
C======================================================================|
      IS_ENCRYPTED = .FALSE.
      IS_AVAILABLE = .FALSE.

      CALL HM_OPTION_START('/TABLE/1')
      DO I=1,NTABLE1


       CALL HM_OPTION_READ_KEY(LSUBMODEL,
     .                         OPTION_TITR = TITR,
     .                         OPTION_ID = LL)
      CALL HM_OPTION_IS_ENCRYPTED(IS_ENCRYPTED)
C-----------------------------------------------
       NX(1) = 0
       NX(2) = 0
       NX(3) = 0
       NX(4) = 0

       CALL HM_GET_INTV('ORDER', NDIM, IS_AVAILABLE, LSUBMODEL)
       IF(NDIM/=1.AND.NDIM/=2.AND.NDIM/=3.AND.NDIM/=4)THEN
         CALL ANCMSG(MSGID=777,
     .               MSGTYPE=MSGERROR,
     .               ANMODE=ANINFO_BLIND_1,
     .               I1=LL,
     .               C1=TITR)
       END IF
       CALL HM_GET_INTV('curverows', NPT, IS_AVAILABLE, LSUBMODEL) !size of X

       IF(NDIM==1)THEN
          L=L+1
          NPTS=NPT
          NPC(NFUNCT+L+1)=LL
          NPC(L+1)=NPC(L)

          NPTS=0
c read abscissa values
          DO IPT = 1, NPT
             CALL HM_GET_FLOAT_ARRAY_INDEX('table2darray',TIME,2*IPT-1,IS_AVAILABLE,LSUBMODEL,UNITAB)
c read ordinate values
             CALL HM_GET_FLOAT_ARRAY_INDEX('table2darray',FUNCT,2*IPT,IS_AVAILABLE,LSUBMODEL, UNITAB)
             NPTS=NPTS+1
             PLD(NPC(L+1))=TIME
             IF(NPTS > 1 .AND. PLD(NPC(L+1)) <= PLD(NPC(L+1)-2))THEN
                 CALL ANCMSG(MSGID=156,
     .                       MSGTYPE=MSGERROR,
     .                       ANMODE=ANINFO_BLIND_1,
     .                       I1=LL,
     .                       C1=TITR1,
     .                       I2=NPTS,
     .                       I3=NPTS-1)
             ENDIF
             NPC(L+1)=NPC(L+1)+1
             PLD(NPC(L+1))=FUNCT
             NPC(L+1)=NPC(L+1)+1
          ENDDO ! IPT

C         build table structure
          TABLE(L)%NOTABLE=LL
          TABLE(L)%NDIM =1
          ALLOCATE(TABLE(L)%X(1),STAT=stat)
          IF (STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                               MSGTYPE=MSGERROR,
     .                         C1='TABLE')
          ALLOCATE(TABLE(L)%X(1)%VALUES(NPTS),STAT=stat)
          IF (STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                               MSGTYPE=MSGERROR,
     .                         C1='TABLE')

          ALLOCATE(TABLE(L)%Y,STAT=stat)
          IF (STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                               MSGTYPE=MSGERROR,
     .                         C1='TABLE')
          ALLOCATE(TABLE(L)%Y%VALUES(NPTS),STAT=stat)
          IF (STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                               MSGTYPE=MSGERROR,
     .                         C1='TABLE')

          DO N=1,NPTS
            TABLE(L)%X(1)%VALUES(N)=PLD(NPC(L)+2*N-2)
            TABLE(L)%Y%VALUES(N)   =PLD(NPC(L)+2*N-1)
          END DO
C
          IF (IS_ENCRYPTED)THEN
            WRITE(IOUT,'(5X,A,//)')'CONFIDENTIAL DATA'
          ELSE
            WRITE(IOUT,2100) TABLE(L)%NOTABLE, TABLE(L)%NDIM
            DO K=1,TABLE(L)%NDIM
              NX(K)=SIZE( TABLE(L)%X(K)%VALUES )
              WRITE(IOUT,2200) K
              WRITE(IOUT,2250) (TABLE(L)%X(K)%VALUES(N),N=1,NX(K))
            END DO
            NY=SIZE(TABLE(L)%Y%VALUES)
            WRITE(IOUT,2300)
            WRITE(IOUT,2350) (TABLE(L)%Y%VALUES(N),N=1,NY)
          END IF

        ENDIF!(NDIM==1)
       ENDDO !I=1,NTABLE0

      RETURN
C-----------------------------------------------------------------
2000  FORMAT(//
     .        '    TABLES'/
     .        '    ------'/
     .        '    NUMBER OF TABLES . . . . . . . . . . =',I10/)
2100  FORMAT(/'    TABLE ID . . . . . . . . . . . . . . =',I10/
     .        '    NUMBER OF PARAMETERS . . . . . . . . =',I10/)
2200  FORMAT(/'    VALUES FOR PARAMETER NUMBER. . . . . .',I4,':'/)
2250  FORMAT((3X,5(1X,G20.13))/)
2300  FORMAT(/'    ORDINATE VALUES . . . . . . . . . . . :'/)
2350  FORMAT((3X,5(1X,G20.13))/)
      END
