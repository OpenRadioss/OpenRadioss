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
Chd|  HM_READ_INTER_TYPE10          source/interfaces/int10/hm_read_inter_type10.F
Chd|-- called by -----------
Chd|        HM_READ_INTER_STRUCT          source/interfaces/reader/hm_read_inter_struct.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        HM_GET_FLOATV                 source/devtools/hm_reader/hm_get_floatv.F
Chd|        HM_GET_INTV                   source/devtools/hm_reader/hm_get_intv.F
Chd|        NGR2USR                       source/system/nintrr.F        
Chd|        ELBUFTAG_MOD                  share/modules1/elbuftag_mod.F 
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE HM_READ_INTER_TYPE10(
     .        IPARI    ,STFAC    ,FRIGAP   ,IGRNOD   ,IGRSURF  ,
     .        LSUBMODEL,UNITAB   ,TITR     ,NOINT    ,KEY      )
C=======================================================================
C   M o d u l e s
C-----------------------------------------------
      USE UNITAB_MOD
      USE MESSAGE_MOD
      USE GROUPDEF_MOD
      USE ELBUFTAG_MOD
      USE SUBMODEL_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
#include      "scr03_c.inc"
#include      "scr06_c.inc"
#include      "scr17_c.inc"
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "units_c.inc"
#include      "warn_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER  :: NOINT,KEY
      my_real  :: STFAC
      INTEGER, DIMENSION(*)  :: IPARI
      my_real, DIMENSION(*)  :: FRIGAP
      CHARACTER*nchartitle ,INTENT(IN)             :: TITR
      TYPE (UNIT_TYPE_) ,INTENT(IN) ::  UNITAB
      TYPE (SUBMODEL_DATA), DIMENSION(*),INTENT(IN) :: LSUBMODEL
C-----------------------------------------------
      TYPE (GROUP_)  ,TARGET ,DIMENSION(NGRNOD)  :: IGRNOD
      TYPE (SURF_)   ,TARGET ,DIMENSION(NSURF)   :: IGRSURF
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER :: NTYP,IS1,IS2,ISU1,ISU2,IDELKEEP,ILEV,IGAP,INACTI,IDEL10,MULTIMP,ITIED
      my_real :: FRIC,GAP,STARTT,STOPT,VISC,BUMULT,GAPMAX,FPENMAX
      INTEGER, DIMENSION(:), POINTER :: INGR2USR
      LOGICAL :: IS_AVAILABLE
C-----------------------------------------------
C   E x t e r n a l   F u n c t i o n s
C-----------------------------------------------
      INTEGER NGR2USR
C=======================================================================
c     Initializations
c--------------------------------------------------------
      NTYP     = 10
      IGAP     = 0
      ILEV     = 0
      MULTIMP  = 4
      IDELKEEP = 0
      IDEL10   = 0
      GAPMAX   = INFINITY
      FPENMAX  = ZERO
      IS_AVAILABLE = .FALSE.
c--------------------------------------------------------
c     Read input fields
c--------------------------------------------------------
card1
      CALL HM_GET_INTV  ('secondaryentityids'  ,ISU1   ,IS_AVAILABLE, LSUBMODEL)
      CALL HM_GET_INTV  ('mainentityids' ,ISU2   ,IS_AVAILABLE, LSUBMODEL)
      CALL HM_GET_INTV  ('NodDel10'        ,IDEL10 ,IS_AVAILABLE, LSUBMODEL)
card2
      CALL HM_GET_FLOATV('TYPE10_SCALE'    ,STFAC  ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('GAP'             ,GAP    ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('TSTART'          ,STARTT ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('TSTOP'           ,STOPT  ,IS_AVAILABLE, LSUBMODEL, UNITAB)
card2
      CALL HM_GET_INTV  ('Itied'           ,ITIED  ,IS_AVAILABLE, LSUBMODEL)
      CALL HM_GET_INTV  ('INACTIV'         ,INACTI ,IS_AVAILABLE, LSUBMODEL)
      CALL HM_GET_FLOATV('STIFF_DC'        ,VISC   ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV('SORT_FACT'       ,BUMULT ,IS_AVAILABLE, LSUBMODEL, UNITAB)
c--------------------------------------------------------
c     Checks
c--------------------------------------------------------
        IF (INACTI == 5) CALL ANCMSG(MSGID=1162,
     .                        MSGTYPE=MSGERROR,
     .                        ANMODE=ANINFO,
     .                        I1=NOINT,
     .                        C1=TITR)
c
      IS1 = 2   ! SECONDARY  surface input by node group
      IS2 = 1   ! main surface input by surface Id
      INGR2USR => IGRNOD(1:NGRNOD)%ID
      IF (ISU1 /= 0) ISU1 = NGR2USR(ISU1,INGR2USR,NGRNOD)
      INGR2USR => IGRSURF(1:NSURF)%ID
      ISU2 = NGR2USR(ISU2,INGR2USR,NSURF)
      IF (IDEL10 < 0) THEN
         IDELKEEP=1
         IDEL10 = ABS(IDEL10)
      END IF
      IF (IDEL10 > 2.OR. N2D == 1) IDEL10 = 0
c
      FRIC = ITIED
      IF (STFAC == ZERO) STFAC = ONE_FIFTH
      IF (VISC == ZERO)  VISC  = FIVEEM2
      IF (BUMULT == ZERO) BUMULT = BMUL0
      IF (STOPT == ZERO) STOPT = INFINITY
c--------------------------------------------------------
c     Fill interface buffers
c--------------------------------------------------------
      FRIGAP(1)  = FRIC
      FRIGAP(2)  = GAP
      FRIGAP(3)  = STARTT
      FRIGAP(4)  = BUMULT
      FRIGAP(10) = ZERO     ! only in engine for storing number of couples candidates
      FRIGAP(11) = STOPT
      FRIGAP(14) = VISC
      FRIGAP(16) = GAPMAX
      FRIGAP(27) = FPENMAX
      FRIGAP(15) = ZERO
c
      IPARI(7)  = NTYP
      IPARI(11) = 0
      IPARI(17) = IDEL10
      IPARI(13) = IS1*10 + IS2
      IPARI(20) = ILEV
      IPARI(21) = IGAP
      IPARI(22) = INACTI
      IPARI(65) = 0
      IPARI(15) = NOINT
      IPARI(23) = MULTIMP
      IPARI(45) = ISU1
      IPARI(46) = ISU2
      IPARI(61) = IDELKEEP
c--------------------------------------------------------
c     Printout
c--------------------------------------------------------
       ITIED = NINT(FRIC)
       WRITE(IOUT,1510) ITIED,STFAC,GAP,STARTT,STOPT,
     .                  BUMULT,INACTI,VISC,MULTIMP
c
       IF (IDEL10 /= 0) THEN
         WRITE(IOUT,'(A,A,I5/)')
     .'    DELETION FLAG ON FAILURE OF MAIN ELEMENT',
     .' (1:YES-ALL/2:YES-ANY) SET TO ',IDEL10
         IF (IDELKEEP == 1) THEN
           WRITE(IOUT,'(A)')
     .'    IDEL: DO NOT REMOVE NON-CONNECTED NODES FROM SECONDARY SURFACE'
         ENDIF
       ENDIF
       WRITE(IOUT,'(6X,A)')'SECONDARY SURFACE INPUT BY NODES'
       WRITE(IOUT,'(6X,A)')'MAIN SURFACE INPUT BY SEGMENTS'

c--------------------------------------------------------
 1510 FORMAT(//
     .    '    TYPE 10 TIED - AUTO IMPACTING               ' //,
     .    '    ITIED . . . . . . . . . . . . . . . . . . . ',I1/,
     .    '       0: TIED DURING IMPACT - REBOUND AUTORIZED'/,
     .    '       1: TIED AFTER IMPACT NO REBOUND AUTORIZED'/,
     .    '    STIFFNESS FACTOR. . . . . . . . . . . . . ',1PG20.13/,
     .    '    MINIMUM GAP . . . . . . . . . . . . . . . ',1PG20.13/,
     .    '    START TIME. . . . . . . . . . . . . . . . ',1PG20.13/,
     .    '    STOP TIME . . . . . . . . . . . . . . . . ',1PG20.13/,
     .    '    BUCKET FACTOR . . . . . . . . . . . . . . ',1PG20.13/,
     .    '    DE-ACTIVATION OF INITIAL PENETRATIONS . . ',I10/,
     .    '    CRITICAL DAMPING FACTOR . . . . . . . . . ',1PG20.13/,
     .    '    MEAN POSSIBLE NUMBER OF IMPACT/NODE . . . ',I5/)
c-----------
      RETURN
      END
