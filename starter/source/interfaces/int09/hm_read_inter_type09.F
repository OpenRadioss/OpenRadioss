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
!||    hm_read_inter_type09   ../starter/source/interfaces/int09/hm_read_inter_type09.F
!||--- called by ------------------------------------------------------
!||    hm_read_inter_fsi      ../starter/source/interfaces/reader/hm_read_inter_fsi.F
!||--- calls      -----------------------------------------------------
!||    hm_get_floatv          ../starter/source/devtools/hm_reader/hm_get_floatv.F
!||    hm_get_intv            ../starter/source/devtools/hm_reader/hm_get_intv.F
!||    ngr2usr                ../starter/source/system/nintrr.F
!||--- uses       -----------------------------------------------------
!||    message_mod            ../starter/share/message_module/message_mod.F
!||    submodel_mod           ../starter/share/modules1/submodel_mod.F
!||====================================================================
      SUBROUTINE HM_READ_INTER_TYPE09(
     1        IPARI      ,STFAC      ,FRIGAP     ,NOINT     ,
     2        IGRSURF    ,UNITAB     ,LSUBMODEL  ,NPARI     ,
     3        NPARIR)
C============================================================================
C
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE UNITAB_MOD , ONLY : UNIT_TYPE_
      USE MESSAGE_MOD
      USE GROUPDEF_MOD , ONLY : SURF_
      USE SUBMODEL_MOD , ONLY : SUBMODEL_DATA, NSUBMOD
      USE NAMES_AND_TITLES_MOD , ONLY : NCHARTITLE, NCHARKEY
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER,INTENT(IN) :: NPARI,NPARIR
      INTEGER ISU1,ISU2,NOINT
      INTEGER IPARI(NPARI)
      my_real STFAC
      my_real FRIGAP(NPARIR)
      TYPE (UNIT_TYPE_),INTENT(IN) :: UNITAB
      TYPE(SUBMODEL_DATA), DIMENSION(NSUBMOD), INTENT(IN) :: LSUBMODEL
      TYPE (SURF_)   ,TARGET , DIMENSION(NSURF)   :: IGRSURF
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "units_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,L, NTYP,IS1, IS2,IEULT,IGSTI,ILEV,IBUC,
     .   IBC1, IBC2, IBC3,IBC1M, IBC2M, IBC3M,INTKG
      my_real
     .   FRIC,GAP,STARTT,STOPT,FHEAT,STENS,VISC
      CHARACTER(LEN=40)::MESS
      CHARACTER(LEN=NCHARTITLE)::MSGTITL
      CHARACTER(LEN=NCHARKEY) ::  OPT,KEY,KEY1
!
      INTEGER, DIMENSION(:), POINTER :: INGR2USR
      LOGICAL :: IS_AVAILABLE
C-----------------------------------------------
C   E x t e r n a l   F u n c t i o n s
C-----------------------------------------------
      INTEGER NGR2USR
C-----------------------------------------------
C=======================================================================
C     READING ALE INTERFACE /INTER/TYPE9
C=======================================================================
      ! Initializations of integer values
      IS1   = 0
      IS2   = 0
      IBC1  = 0
      IBC2  = 0
      IBC3  = 0
      IBC1M = 0
      IBC2M = 0
      IBC3M = 0
      IGSTI = 0
      ILEV  = 0  
      INTKG = 0
      IBUC  = 0
      IEULT = 0
      ! Initializations of float values
      FRIC   = ZERO
      GAP    = ZERO
      STARTT = ZERO
      STOPT  = INFINITY
      VISC   = ZERO
      FHEAT  = ZERO
      STENS  = ZERO
      ! Type 
      NTYP      = 9
      IPARI(15) = NOINT
      IPARI(7)  = NTYP
C=======================================================================
C     CARD 1 : Surfaces ids
C=======================================================================
      ! Lagrangian surface
      CALL HM_GET_INTV('surf_IDA', ISU1, IS_AVAILABLE, LSUBMODEL)
      ! ALE surface
      CALL HM_GET_INTV('surf_IDL', ISU2, IS_AVAILABLE, LSUBMODEL)
      ! Checks
      IS1=1
      INGR2USR => IGRSURF(1:NSURF)%ID
      ISU1=NGR2USR(ISU1,INGR2USR,NSURF)
      IF(ISU2==0)THEN
        IS2=0
      ELSE
        IS2=1
        ISU2=NGR2USR(ISU2,INGR2USR,NSURF)
      ENDIF
      ! Storage IPARI FRIGAP
      IPARI(45) = ISU1
      IPARI(46) = ISU2
      IPARI(13) = IS1*10+IS2
      IPARI(20) = ILEV
C=======================================================================
C     CARD 2 : Thermal resistance + Friction coefficient + Init GAP
C=======================================================================
      ! Thermal resistance
      CALL HM_GET_FLOATV('R_TH'  ,STFAC    ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      ! Friction coefficient
      CALL HM_GET_FLOATV('FRIC'  ,FRIC     ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      ! Initial Gap
      CALL HM_GET_FLOATV('GAP'   ,GAP      ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      ! Storage IPARI FRIGAP
      FRIGAP(1)  = FRIC
      FRIGAP(2)  = GAP
      FRIGAP(3)  = STARTT
      FRIGAP(11) = STOPT
C=======================================================================
C     CARD 3 : Thermal bridge + Euler + Upwind + Surface tension
C=======================================================================
      ! Thermal bridge flag
      CALL HM_GET_INTV('I_TH'      ,IBUC     ,IS_AVAILABLE, LSUBMODEL)
      ! Euler flag
      CALL HM_GET_INTV('I_EUL'     ,IEULT    ,IS_AVAILABLE, LSUBMODEL)  
      ! Upwind value
      CALL HM_GET_FLOATV('UPWIND'  ,VISC     ,IS_AVAILABLE, LSUBMODEL, UNITAB)
      ! Surface tension force
      CALL HM_GET_FLOATV('Fs'      ,STENS    ,IS_AVAILABLE, LSUBMODEL, UNITAB) 
      ! Storage IPARI FRIGAP
      IPARI(14)  = IEULT
      FRIGAP(4)  = FHEAT
      FRIGAP(15) = STENS 
      IPARI(11)  = 4*IBC1+2*IBC2+IBC3 + 8 *(4*IBC1M+2*IBC2M+IBC3M)
      FRIGAP(14) = VISC
      IPARI(12)  = IBUC
      IPARI(65)  = INTKG
C=======================================================================
C     PRINTOUT
C=======================================================================
C
      WRITE(IOUT,1509) IBUC,IPARI(14),STFAC,FRIC,GAP,VISC,STENS
C
      IF(IS1==0)THEN
        WRITE(IOUT,'(6X,A)')'NO ALE SURFACE INPUT'
      ELSEIF(IS1==1)THEN
        WRITE(IOUT,'(6X,A)')'ALE SURFACE INPUT BY SEGMENTS'
      ELSEIF(IS1==2)THEN
        WRITE(IOUT,'(6X,A)')'ALE SURFACE INPUT BY NODES'
      ELSEIF(IS1==3)THEN
        WRITE(IOUT,'(6X,A)')'ALE SURFACE INPUT BY SEGMENTS'
      ELSEIF(IS1==4 )THEN
        WRITE(IOUT,'(6X,A)')'ALE SIDE INPUT BY BRICKS'
      ELSEIF(IS1==5 )THEN
        WRITE(IOUT,'(6X,A)')'ALE SIDE INPUT BY SOLID ELEMENTS'
      ENDIF
      IF(IS2==0)THEN
        WRITE(IOUT,'(6X,A)')'NO LAGRANGIAN SURFACE INPUT'
      ELSEIF(IS2==1)THEN
        WRITE(IOUT,'(6X,A)')'LAGRANGIAN SURFACE INPUT BY SEGMENTS'
      ELSEIF(IS2==2)THEN
        WRITE(IOUT,'(6X,A)')'LAGRANGIAN SURFACE INPUT BY NODES'
      ELSEIF(IS2==3)THEN
        WRITE(IOUT,'(6X,A)')'LAGRANGIAN SURFACE INPUT BY SEGMENTS'
      ELSEIF(IS2==4)THEN
        WRITE(IOUT,'(6X,A)')'LAGRANGIAN SURFACE REFERS ',
     .                      'TO HYPER-ELLIPSOIDAL SURFACE'
      ENDIF
C
C--------------------------------------------------------------
 1000 FORMAT(/1X,'  INTERFACE NUMBER :',I10,1X,A)
C------------
 1509 FORMAT(//
     .    '    TYPE==9  ALE-THERMAL SLIDING AND VOIDS  ' //,
     .    '    THERMAL BRIDGE (1 YES 0 NO) . . . . . . . ',I10/,
     .    '    TANG. DIR. EULER.(FREE SURF.) (1 YES 0 NO)',I10/,
     .    '    THERMAL RESISTANCE. . . . . . . . . . . . ',1PG20.13/,
     .    '    FRICTION FACTOR . . . . . . . . . . . . . ',1PG20.13/,
     .    '    INITIAL GAP . . . . . . . . . . . . . . . ',1PG20.13/,
     .    '    FREE SURFACE UPWIND . . . . . . . . . . . ',1PG20.13/,
     .    '    SURFACE TENSION . . . . . . . . . . . . . ',1PG20.13//)
    
      RETURN
      END
