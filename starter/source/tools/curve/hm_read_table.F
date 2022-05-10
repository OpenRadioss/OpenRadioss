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
Chd|  HM_READ_TABLE1                source/tools/curve/hm_read_table.F
Chd|-- called by -----------
Chd|        LECTUR                        source/starter/lectur.F       
Chd|-- calls ---------------
Chd|        FREERR                        source/starter/freform.F      
Chd|        HM_OPTION_COUNT               source/devtools/hm_reader/hm_option_count.F
Chd|        HM_READ_TABLE1_0              source/tools/curve/hm_read_table1_0.F
Chd|        HM_READ_TABLE1_1              source/tools/curve/hm_read_table1_1.F
Chd|        HM_OPTION_READ_MOD            share/modules1/hm_option_read_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|        TABLE_MOD                     share/modules1/table_mod.F    
Chd|====================================================================
      SUBROUTINE HM_READ_TABLE1(NTABLE, TABLE ,NFUNCT ,NPC ,PLD,NOM_OPT, UNITAB, LSUBMODEL)
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
      INTEGER NTABLE,NFUNCT,NPC(*)
C     REAL ou REAL*8
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
      INTEGER ITYPE, IBID, NFUNCT0, NTABLE0,NTABLE1
      INTEGER I, J, K, II, N, L, NDIM, NX(4), NY,
     .        JREC, NPTS, STAT, LL
      INTEGER IERROR, NF, IDFUNC, NP
C     REAL ou REAL*8
      my_real
     .   BID, F5(5), TIME, FUNCT
      CHARACTER TITR*nchartitle, MESS*40, KEY*ncharfield,
     .          TITR1*nchartitle
      DATA MESS/' FUNCTION & TABLE DEFINITION            '/
      LOGICAL :: IS_CRYPTED, IS_AVAILABLE
C======================================================================|
      IF(NTABLE  == 0) RETURN 
!     Initialization
      IS_CRYPTED = .FALSE.
      IS_AVAILABLE = .FALSE.

      CALL HM_OPTION_COUNT('/FUNCT', NFUNCT0)
      NFUNCT0=NSLASH(KFUNCT)+NFUNCT0
      WRITE (IOUT,2000) NTABLE-NFUNCT0

      L      =NFUNCT0 ! total numbre /TABLE + /FUNCT
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
 999  CALL FREERR(1)   
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
Chd|====================================================================
Chd|  HM_READ_TABLE2                source/tools/curve/hm_read_table.F
Chd|-- called by -----------
Chd|        LECTUR                        source/starter/lectur.F       
Chd|-- calls ---------------
Chd|        FREERR                        source/starter/freform.F      
Chd|        HM_OPTION_COUNT               source/devtools/hm_reader/hm_option_count.F
Chd|        HM_READ_TABLE2_0              source/tools/curve/hm_read_table2_0.F
Chd|        HM_READ_TABLE2_1              source/tools/curve/hm_read_table2_1.F
Chd|        UDOUBLE                       source/system/sysfus.F        
Chd|        HM_OPTION_READ_MOD            share/modules1/hm_option_read_mod.F
Chd|        INTERFACE_TABLE_MOD           share/modules1/table_mod.F    
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|        TABLE_MOD                     share/modules1/table_mod.F    
Chd|====================================================================
      SUBROUTINE HM_READ_TABLE2(NTABLE, TABLE ,NFUNCT , UNITAB, LSUBMODEL)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE TABLE_MOD
      USE INTERFACE_TABLE_MOD
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
      INTEGER NTABLE,NFUNCT
C     REAL ou REAL*8
      TYPE(TTABLE) TABLE(*)
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
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER NTABLE1, NTABLE0,ITYPE, IBID, IDTAB(NTABLE)
      INTEGER I, J, K, II, N, L, ID, NDIM, NX(4), NY,
     .        JREC, NPTS, STAT, IDS, LL, LX(2), NOK(4)
      INTEGER,DIMENSION(:),ALLOCATABLE :: JPERM1,JPERM2
      INTEGER,DIMENSION(:,:),ALLOCATABLE :: ITAG
      INTEGER IERROR, NF, IDFUNC, NP, 
     .        KK, IDEB, IFIN, IOK, NN, N1, N2, N11, N12, N13, KK1
C     REAL ou REAL*8
      my_real
     .   BID, F5(5), XX, X1, X2, X234(3), YY, Y1, Y2, R, XMIN, XMAX,
     .    TIME, FUNCT,SCALEY
      my_real,
     .        DIMENSION(2) :: XD2
      my_real,
     .        DIMENSION(:),ALLOCATABLE :: XV1, XSTOR1, XSTOR2
      my_real,
     .        DIMENSION(:,:),ALLOCATABLE :: XV2
      CHARACTER TITR*nchartitle, MESS*40, KEY*ncharfield
      DATA MESS/' FUNCTION & TABLE DEFINITION            '/
      LOGICAL :: IS_CRYPTED, IS_AVAILABLE
C======================================================================|
c
      IF(NTABLE == 0) RETURN
!     Initialization
      IS_CRYPTED = .FALSE.
      IS_AVAILABLE = .FALSE.
      L      =NFUNCT
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
      IF(NTABLE1> 0) THEN
       CALL HM_READ_TABLE2_1(NTABLE,NTABLE1, TABLE,L ,NFUNCT , UNITAB, LSUBMODEL)      
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
 999  CALL FREERR(1)
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
