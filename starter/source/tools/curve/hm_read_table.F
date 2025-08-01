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
!||    hm_read_table1         ../starter/source/tools/curve/hm_read_table.F
!||--- called by ------------------------------------------------------
!||    lectur                 ../starter/source/starter/lectur.F
!||--- calls      -----------------------------------------------------
!||    hm_option_count        ../starter/source/devtools/hm_reader/hm_option_count.F
!||    hm_read_table1_0       ../starter/source/tools/curve/hm_read_table1_0.F
!||    hm_read_table1_1       ../starter/source/tools/curve/hm_read_table1_1.F
!||--- uses       -----------------------------------------------------
!||    hm_option_read_mod     ../starter/share/modules1/hm_option_read_mod.F
!||    message_mod            ../starter/share/message_module/message_mod.F
!||    reader_old_mod         ../starter/share/modules1/reader_old_mod.F90
!||    submodel_mod           ../starter/share/modules1/submodel_mod.F
!||    table_mod              ../starter/share/modules1/table_mod.F
!||====================================================================
      SUBROUTINE HM_READ_TABLE1(NTABLE, TABLE ,NFUNCT ,NPC ,PLD,NOM_OPT, UNITAB, LSUBMODEL)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE TABLE_MOD
      USE MESSAGE_MOD
      USE SUBMODEL_MOD , ONLY : NSUBMOD, SUBMODEL_DATA
      USE HM_OPTION_READ_MOD
      USE UNITAB_MOD
      USE NAMES_AND_TITLES_MOD , ONLY : NCHARTITLE, NCHARFIELD
      USE READER_OLD_MOD , ONLY : KFUNCT, NSLASH
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
      INTEGER NTABLE,NFUNCT,NPC(*)
      my_real PLD(*)
      TYPE(TTABLE) TABLE(*)
      INTEGER NOM_OPT(LNOPT1,*)
      TYPE(SUBMODEL_DATA), DIMENSION(NSUBMOD), INTENT(IN) :: LSUBMODEL
      TYPE(UNIT_TYPE_), INTENT(IN) :: UNITAB

C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER ITYPE, IBID, NFUNCT0, NTABLE0,NTABLE1, NFUNCT_PYTHON
      INTEGER I, J, K, II, N, L, NDIM, NX(4), NY,
     .        JREC, NPTS, STAT, LL
      INTEGER IERROR, NF, IDFUNC, NP
      my_real BID, F5(5), TIME, FUNCT
      CHARACTER(LEN=NCHARTITLE) :: TITR,TITR1
      CHARACTER :: MESS*40
      CHARACTER(LEN=NCHARFIELD)::KEY
      DATA MESS/' FUNCTION & TABLE DEFINITION            '/
      LOGICAL :: IS_ENCRYPTED, IS_AVAILABLE     
C======================================================================|
      IF(NTABLE  == 0) RETURN 
!     Initialization
      IS_ENCRYPTED = .FALSE.
      IS_AVAILABLE = .FALSE.

      CALL HM_OPTION_COUNT('/FUNCT', NFUNCT0)
      CALL HM_OPTION_COUNT('/FUNCT_PYTHON', NFUNCT_PYTHON)
      NFUNCT0=NSLASH(KFUNCT)+NFUNCT0
      ! python functions are not associated with a table
      WRITE (IOUT,2000) NTABLE-(NFUNCT0-NFUNCT_PYTHON)

      L      =NFUNCT0 ! total number /TABLE + /FUNCT
      CALL HM_OPTION_COUNT('/TABLE/0', NTABLE0)
      CALL HM_OPTION_COUNT('/TABLE/1', NTABLE1)
C----------------------------
C ORDER 1 TABLES
C----------------------------
C----------------------------
C /FUNCT/TABLE/0/id, NDIM=1
C----------------------------
      IF (NTABLE0> 0) THEN
       CALL HM_READ_TABLE1_0(NTABLE0,NTABLE, TABLE ,NFUNCT ,NPC ,PLD,L,NOM_OPT, UNITAB, LSUBMODEL)      
      ENDIF
C----------------------------
C /FUNCT/TABLE/1/id, NDIM=1
C----------------------------
      IF(NTABLE1> 0) THEN
       CALL HM_READ_TABLE1_1(NTABLE1,NTABLE, TABLE ,NFUNCT ,NPC ,PLD,L,NOM_OPT, UNITAB, LSUBMODEL)      
      ENDIF
C
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
!||====================================================================
!||    hm_read_table2         ../starter/source/tools/curve/hm_read_table.F
!||--- called by ------------------------------------------------------
!||    lectur                 ../starter/source/starter/lectur.F
!||--- calls      -----------------------------------------------------
!||    hm_option_count        ../starter/source/devtools/hm_reader/hm_option_count.F
!||    hm_read_table2_0       ../starter/source/tools/curve/hm_read_table2_0.F
!||    hm_read_table2_1       ../starter/source/tools/curve/hm_read_table2_1.F
!||    udouble                ../starter/source/system/sysfus.F
!||--- uses       -----------------------------------------------------
!||    hm_option_read_mod     ../starter/share/modules1/hm_option_read_mod.F
!||    message_mod            ../starter/share/message_module/message_mod.F
!||    submodel_mod           ../starter/share/modules1/submodel_mod.F
!||    table_mod              ../starter/share/modules1/table_mod.F
!||====================================================================
      SUBROUTINE HM_READ_TABLE2(NTABLE, TABLE ,NFUNCT , UNITAB, LSUBMODEL)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE TABLE_MOD
      USE MESSAGE_MOD
      USE SUBMODEL_MOD
      USE HM_OPTION_READ_MOD
      USE UNITAB_MOD
      USE NAMES_AND_TITLES_MOD , ONLY : NCHARTITLE, NCHARFIELD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NTABLE,NFUNCT
      TYPE(TTABLE) TABLE(*)
      TYPE(SUBMODEL_DATA), DIMENSION(NSUBMOD), INTENT(IN) :: LSUBMODEL
      TYPE(UNIT_TYPE_), INTENT(IN) :: UNITAB
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER NTABLE1, NTABLE0,ITYPE, IBID, IDTAB(NTABLE)
      INTEGER I, J, K, II, N, L, ID, NDIM, NX(4), NY,
     .        JREC, NPTS, STAT, IDS, LL, LX(2), NOK(4)
      INTEGER,DIMENSION(:),ALLOCATABLE :: JPERM1,JPERM2
      INTEGER,DIMENSION(:,:),ALLOCATABLE :: ITAG
      INTEGER IERROR, NF, IDFUNC, NP, KK, IDEB, IFIN, IOK, NN, N1, N2, N11, N12, N13, KK1
      my_real BID, F5(5), XX, X1, X2, X234(3), YY, Y1, Y2, R, XMIN, XMAX, TIME, FUNCT,SCALEY
      my_real,DIMENSION(2) :: XD2
      my_real,DIMENSION(:),ALLOCATABLE :: XV1, XSTOR1, XSTOR2
      my_real,DIMENSION(:,:),ALLOCATABLE :: XV2
      CHARACTER(LEN=NCHARTITLE) :: TITR
      CHARACTER :: MESS*40
      CHARACTER(LEN=NCHARFIELD) :: KEY
      DATA MESS/' FUNCTION & TABLE DEFINITION            '/
      LOGICAL :: IS_ENCRYPTED, IS_AVAILABLE
C======================================================================|
c
      IF(NTABLE == 0) RETURN
!     Initialization
      IS_ENCRYPTED = .FALSE.
      IS_AVAILABLE = .FALSE.
      L = NFUNCT
C----------------------------
C HIGHER ORDERS TABLES
C----------------------------
      CALL HM_OPTION_COUNT('/TABLE/0', NTABLE0)
      CALL HM_OPTION_COUNT('/TABLE/1', NTABLE1)
C----------------------------
C /FUNCT/TABLE/0/id, NDIM>1
C----------------------------
      IF (NTABLE0> 0) THEN
       CALL HM_READ_TABLE2_0(NTABLE0,TABLE,L ,NFUNCT , UNITAB, LSUBMODEL)      
      ENDIF
C----------------------------
C /FUNCT/TABLE/1/id, NDIM>1
C----------------------------
      IF (NTABLE1> 0) THEN
       CALL HM_READ_TABLE2_1(NTABLE,NTABLE1, TABLE, L , UNITAB, LSUBMODEL)      
      ENDIF
C-------------------------------------
C Recherche des ID doubles (functions & tables)
C-------------------------------------
      DO L=1,NTABLE
       IDTAB(L)=TABLE(L)%NOTABLE
      END DO
      IDS =  79
      I = 0
      J = 0
c      CALL ANCNTS(IDS,I)
      CALL UDOUBLE(IDTAB,1,NTABLE,MESS,0,BID)
c      CALL ANCNTG(IDS,I,J)
      IDS = 56
c      CALL ANCHECK(IDS)
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
