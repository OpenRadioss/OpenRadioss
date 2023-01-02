Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2023 Altair Engineering Inc.
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
Chd|  HM_READ_INTER_LAGMUL_TYPE16   source/interfaces/int16/hm_read_inter_lagmul_type16.F
Chd|-- called by -----------
Chd|        HM_READ_INTER_LAGMUL          source/interfaces/reader/hm_read_inter_lagmul.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        HM_GET_INTV                   source/devtools/hm_reader/hm_get_intv.F
Chd|        NGR2USR                       source/system/nintrr.F        
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE HM_READ_INTER_LAGMUL_TYPE16(
     .           IPARI      ,STFAC      ,FRIGAP     ,NOINT     ,NI        ,
     .           IGRNOD     ,IGRBRIC    ,UNITAB    ,LSUBMODEL )
C============================================================================
C   M o d u l e s
C-----------------------------------------------
      USE UNITAB_MOD
      USE MESSAGE_MOD
      USE GROUPDEF_MOD
      USE SUBMODEL_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "scr17_c.inc"
#include      "sphcom.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER ISU1,ISU2,NOINT,NI
      INTEGER IPARI(*)
      my_real FRIGAP(*) , STFAC
      TYPE (GROUP_)  ,TARGET ,DIMENSION(NGRNOD)  :: IGRNOD
      TYPE (GROUP_)  ,TARGET, DIMENSION(NGRBRIC) :: IGRBRIC
      TYPE (UNIT_TYPE_),INTENT(IN) ::UNITAB 
      TYPE(SUBMODEL_DATA) LSUBMODEL(*)
C----------------------s-------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "scr05_c.inc"
#include      "scr06_c.inc"
#include      "units_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER :: ISU10,ISU20,IDUM,MULTIMP,ITIED,NTYP,IS1,IS2
      my_real :: STARTT,BUMULT,STOPT,GAP
      INTEGER, DIMENSION(:), POINTER :: INGR2USR
      LOGICAL IS_AVAILABLE
C-----------------------------------------------
C   E x t e r n a l   F u n c t i o n s
C-----------------------------------------------
      INTEGER NGR2USR
C=======================================================================
C     reading /INTER/LAGMUL/TYPE16
C=======================================================================

C Initializations
      NTYP = 16
      STARTT = ZERO
      STOPT  = EP20
      STFAC  = ONE_FIFTH
      GAP    = ZERO
c------------------------------------------------------------
c  Card1
      CALL HM_GET_INTV('secondaryentityids ',ISU10,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV('mainentityids',ISU20,IS_AVAILABLE,LSUBMODEL)
c
c  Card2
      CALL HM_GET_INTV('Itied' ,ITIED,IS_AVAILABLE,LSUBMODEL)
c------------------------------------------------------------
c     CHECKS
c------------------------------------------------------------
      IS1 = 0
      IS2 = 5
      INGR2USR => IGRNOD(1:NGRNOD)%ID
      ISU1 = NGR2USR(ISU10,INGR2USR,NGRNOD)
      INGR2USR => IGRBRIC(1:NGRBRIC)%ID
      ISU2 = NGR2USR(ISU20,INGR2USR,NGRBRIC)
c
      BUMULT = BMUL0
      IF (IMACH /= 3) THEN  ! SMP
        MULTIMP=4
      ELSE                 ! SPMD
        MULTIMP=12
      END IF
c
      IF (NSPMD > 1) THEN
          CALL ANCMSG(MSGID=755,MSGTYPE=MSGERROR,ANMODE=ANINFO,
     .                C1='TYPE 16 LAGRANGE INTERFACE')
      END IF
c------------------------------------------------------------
c     STORAGE
c------------------------------------------------------------
      IPARI(7)  = NTYP
      IPARI(13) = IS1*10+IS2
      IPARI(15) = NOINT
      IPARI(23) = MULTIMP
      IPARI(30) = ITIED
      IPARI(45) = ISU1
      IPARI(46) = ISU2
C
      FRIGAP(2) = GAP
      FRIGAP(3) = STARTT
      FRIGAP(4) = BUMULT
      FRIGAP(5) = ONE
      FRIGAP(11)= STOPT
C------------------------------------------------------------
C     PRINTOUT
C------------------------------------------------------------
      WRITE(IOUT,1000)ISU10,ISU20,ITIED,STARTT,STOPT 
C 
      IF(IS1 == 0)THEN
        WRITE(IOUT,'(6X,A)')'NO SECONDARY SURFACE INPUT'
      ELSEIF(IS1 == 1)THEN
        WRITE(IOUT,'(6X,A)')'SECONDARY SURFACE INPUT BY SEGMENTS'
      ELSEIF(IS1 == 2)THEN
        WRITE(IOUT,'(6X,A)')'SECONDARY SURFACE INPUT BY NODES'
      ELSEIF(IS1 == 3)THEN
        WRITE(IOUT,'(6X,A)')'SECONDARY SURFACE INPUT BY SEGMENTS'
      ELSEIF(IS1 == 4 )THEN
        WRITE(IOUT,'(6X,A)')'SECONDARY SIDE INPUT BY BRICKS'
      ELSEIF(IS1 == 5 )THEN
        WRITE(IOUT,'(6X,A)')'SECONDARY SIDE INPUT BY SOLID ELEMENTS'        
      ENDIF
      IF(IS2 == 0)THEN
        WRITE(IOUT,'(6X,A)')'NO MAIN SURFACE INPUT'
      ELSEIF(IS2 == 1)THEN
        WRITE(IOUT,'(6X,A)')'MAIN SURFACE INPUT BY SEGMENTS'
      ELSEIF(IS2 == 2)THEN
        WRITE(IOUT,'(6X,A)')'MAIN SURFACE INPUT BY NODES'
      ELSEIF(IS2 == 3)THEN
        WRITE(IOUT,'(6X,A)')'MAIN SURFACE INPUT BY SEGMENTS'
      ELSEIF(IS2 == 4)THEN
        WRITE(IOUT,'(6X,A)')'MAIN SURFACE REFERS ',
     .                      'TO HYPER-ELLIPSOIDAL SURFACE'
      ENDIF
      WRITE(IOUT,'(A)')' '
c-----------
      RETURN
c--------------------------------------------------------------
 1000 FORMAT(//
     .    '    TYPE 16   NODES TO 16 NODES THICK SHELL  ' //,
     .    '    NODE GROUP . . . . . . .  . . . . . . . . . ',I10/,
     .    '    SOLID ELEMENT GROUP. . . . . . .  . . . . . ',I10/,
     .    '    ITIED . . . . . . . . . . . . . . . . . . . ',I10/,
     .    '       0: SLIDING '/,
     .    '       1: TIED (during contact)'/,
     .    '       2: TIED (no rebound)'/,
     .    '    START TIME. . . . . . . . . . . . . . . . . ',1PG20.13/,
     .    '    STOP TIME . . . . . . . . . . . . . . . . . ',1PG20.13/)
c--------------------------------------------------------------
      END
