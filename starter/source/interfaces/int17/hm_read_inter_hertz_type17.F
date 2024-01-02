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
Chd|  HM_READ_INTER_HERTZ_TYPE17    source/interfaces/int17/hm_read_inter_hertz_type17.F
Chd|-- called by -----------
Chd|        HM_READ_INTER_STRUCT          source/interfaces/reader/hm_read_inter_struct.F
Chd|-- calls ---------------
Chd|        HM_GET_FLOATV                 source/devtools/hm_reader/hm_get_floatv.F
Chd|        HM_GET_INTV                   source/devtools/hm_reader/hm_get_intv.F
Chd|        NGR2USR                       source/system/nintrr.F        
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE HM_READ_INTER_HERTZ_TYPE17(
     1        IPARI       ,FRIGAP       ,NOINT    ,NI       ,NTYP        ,
     2        IGRBRIC     ,UNITAB       ,LSUBMODEL,TITR     )
C============================================================================
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
      USE GROUPDEF_MOD
      USE SUBMODEL_MOD
      USE UNITAB_MOD       
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "scr17_c.inc"
#include      "scr06_c.inc"
#include      "com04_c.inc"
#include      "units_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NOINT,NI,NTYP
      INTEGER IPARI(*)
      my_real
     .   FRIGAP(*)
      CHARACTER TITR*nchartitle
      TYPE(SUBMODEL_DATA), DIMENSION(NSUBMOD), INTENT(IN) :: LSUBMODEL
      TYPE (UNIT_TYPE_), INTENT(IN) :: UNITAB 
      TYPE (GROUP_)  ,TARGET, DIMENSION(NGRBRIC) :: IGRBRIC
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER 
     .   IS1,IS2,ISU1,ISU2,ISU10,ISU20,MULTIMP,ITIED,UID
      my_real
     .   STARTT,BUMULT,STOPT,FRIC,GAP
      LOGICAL IS_AVAILABLE
      INTEGER, DIMENSION(:), POINTER :: INGR2USR
C-----------------------------------------------
C   E x t e r n a l   F u n c t i o n s
C-----------------------------------------------
      INTEGER NGR2USR
C=======================================================================
C     READING INTERFACE /INTER/HERTZ/TYPE17
C=======================================================================
C Initializations of reals and integers
      IS1     = 0
      IS2     = 0
      STARTT  = ZERO
      STOPT   = INFINITY
      FRIC    = ZERO
      GAP     = ZERO
      BUMULT  = BMUL0
      MULTIMP = 24
      ITIED   = 0
      IS_AVAILABLE = .FALSE.
C------------------------------------------------------------
!  Printing out titles and interface ID
      WRITE(IOUT,1000) NOINT,TRIM(TITR)
C------------------------------------------------------------
!  Reading cards
C------------------------------------------------------------
C  Card1
C------------------------------------------------------------
      CALL HM_GET_INTV('secondaryentityids'  ,ISU10 ,IS_AVAILABLE, LSUBMODEL)  
      CALL HM_GET_INTV('mainentityids' ,ISU20 ,IS_AVAILABLE, LSUBMODEL)  
C------------------------------------------------------------
C  Card2
C------------------------------------------------------------
      CALL HM_GET_FLOATV('FRIC',FRIC ,IS_AVAILABLE, LSUBMODEL, UNITAB)
C------------------------------------------------------------
!     Checking values
      IS1 = 5
      IS2 = 5
      INGR2USR => IGRBRIC(1:NGRBRIC)%ID
      ISU1=NGR2USR(ISU10,INGR2USR,NGRBRIC)
      ISU2=NGR2USR(ISU20,INGR2USR,NGRBRIC)
C------------------------------------------------------------
! Storage in IPARI and FRIGAP
C------------------------------------------------------------
      IPARI(7)  = NTYP
      IPARI(13) = IS1*10+IS2
      IPARI(15) = NOINT
      IPARI(23) = MULTIMP
      IPARI(30) = ITIED
      IPARI(45) = ISU1
      IPARI(46) = ISU2
C
      FRIGAP(1)  = FRIC
      FRIGAP(2)  = GAP
      FRIGAP(3)  = STARTT
      FRIGAP(4)  = BUMULT
      FRIGAP(5)  = ONE
      FRIGAP(11) = STOPT
C------------------------------------------------------------
C     PRINTOUT
C------------------------------------------------------------
C
       WRITE(IOUT,1717) ISU10,ISU20,FRIC,STARTT,STOPT
C
      IF(IS1==0)THEN
        WRITE(IOUT,'(6X,A)')'NO SECONDARY SURFACE INPUT'
      ELSEIF(IS1==1)THEN
        WRITE(IOUT,'(6X,A)')'SECONDARY SURFACE INPUT BY SEGMENTS'
      ELSEIF(IS1==2)THEN
        WRITE(IOUT,'(6X,A)')'SECONDARY SURFACE INPUT BY NODES'
      ELSEIF(IS1==3)THEN
        WRITE(IOUT,'(6X,A)')'SECONDARY SURFACE INPUT BY SEGMENTS'
      ELSEIF(IS1==4 )THEN
        WRITE(IOUT,'(6X,A)')'SECONDARY SIDE INPUT BY BRICKS'
      ELSEIF(IS1==5 )THEN
        WRITE(IOUT,'(6X,A)')'SECONDARY SIDE INPUT BY SOLID ELEMENTS'        
      ENDIF
      IF(IS2==0)THEN
        WRITE(IOUT,'(6X,A)')'NO MAIN SURFACE INPUT'
      ELSEIF(IS2==1)THEN
        WRITE(IOUT,'(6X,A)')'MAIN SURFACE INPUT BY SEGMENTS'
      ELSEIF(IS2==2)THEN
        WRITE(IOUT,'(6X,A)')'MAIN SURFACE INPUT BY NODES'
      ELSEIF(IS2==3)THEN
        WRITE(IOUT,'(6X,A)')'MAIN SURFACE INPUT BY SEGMENTS'
      ELSEIF(IS2==4)THEN
        WRITE(IOUT,'(6X,A)')'MAIN SURFACE REFERS ',
     .                      'TO HYPER-ELLIPSOIDAL SURFACE'
      ENDIF
C  
C------------
      RETURN
C--------------------------------------------------------------
 1000 FORMAT(/1X,'  INTERFACE NUMBER :',I10,1X,A)
C--------------------------------------------------------------

 1717 FORMAT(//
     .    '    TYPE==17 SURFACE/SURFACE 16 NODES THICK SHELL' /,
     .    '              HERTZ PRESSURE FORMULATION' //,
     .    '    FIRST SOLID ELEMENT GROUP. . . . . . .  . . ',I10/,
     .    '    SECOND SOLID ELEMENT GROUP . . . . . .  . . ',I10/,
     .    '    FRICTION FACTOR . . . . . . . . . . . . . . ',1PG20.13/,
     .    '    START TIME. . . . . . . . . . . . . . . . . ',1PG20.13/,
     .    '    STOP TIME . . . . . . . . . . . . . . . . . ',1PG20.13/)
      END
