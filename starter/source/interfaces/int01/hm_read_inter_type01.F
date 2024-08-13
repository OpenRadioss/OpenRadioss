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
      !||====================================================================
      !||    hm_read_inter_type01   ../starter/source/interfaces/int01/hm_read_inter_type01.F
      !||--- called by ------------------------------------------------------
      !||    hm_read_inter_fsi      ../starter/source/interfaces/reader/hm_read_inter_fsi.F
      !||--- calls      -----------------------------------------------------
      !||    hm_get_intv            ../starter/source/devtools/hm_reader/hm_get_intv.F
      !||    ngr2usr                ../starter/source/system/nintrr.F
      !||--- uses       -----------------------------------------------------
      !||    message_mod            ../starter/share/message_module/message_mod.F
      !||    submodel_mod           ../starter/share/modules1/submodel_mod.F
      !||====================================================================
      SUBROUTINE HM_READ_INTER_TYPE01(IPARI, STFAC, FRIGAP, IGRSURF, LSUBMODEL, NSURF, NOINT, NPARI, NPARIR)
C-----------------------------------------------
C   D e s c r i p t i o n
C-----------------------------------------------
      ! Reader for interface type 1 (/INTER/TYPE1)
      !   FSI COUPLING INTERFACE : ALE (secondary side) versus LAGRANGE (main side)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
      USE GROUPDEF_MOD , ONLY : SURF_
      USE SUBMODEL_MOD , ONLY : NSUBMOD, SUBMODEL_DATA
      USE NAMES_AND_TITLES_MOD , ONLY : NCHARTITLE, NCHARKEY, NCHARFIELD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER,INTENT(IN) :: NPARI, NPARIR          !< array sizes IPARI and FRIGAP
      INTEGER,INTENT(IN) :: NSURF                  !< number of surfaces : array size for IGRSURF
      INTEGER,INTENT(IN) :: NOINT                  !< interface identifier
      INTEGER,INTENT(INOUT) :: IPARI(NPARI)        !< interface buffer (integer)
      my_real,INTENT(INOUT) :: FRIGAP(NPARIR)      !< interface buffer (real)
      my_real,INTENT(INOUT) :: STFAC               !< stiffness value or stiffness factor
      TYPE(SUBMODEL_DATA),INTENT(IN),DIMENSION(NSUBMOD) :: LSUBMODEL !< SUBMODEL data structure
      TYPE (SURF_),INTENT(IN),TARGET,DIMENSION(NSURF)   :: IGRSURF   !< surface sata structure
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "units_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      my_real FRIC,GAP,STARTT,STOPT !< interface parameters
      INTEGER ISU1,ISU2 !< surface identifiers
      INTEGER I,J,L, NTYP,IS1,IS2,IGSTI,ILEV,IVOID
      INTEGER, DIMENSION(:), POINTER :: INGR2USR
      LOGICAL :: IS_AVAILABLE
      CHARACTER(len=nchartitle) :: MSGTITL
C-----------------------------------------------
C   E x t e r n a l   F u n c t i o n s
C-----------------------------------------------
      INTEGER, EXTERNAL :: NGR2USR
C-----------------------------------------------

      ! Initializations
      IS1=0
      IS2=0
      IGSTI=0
      ILEV=0

      FRIC=ZERO
      GAP=ZERO
      STARTT=ZERO
      STOPT=EP20

      NTYP=1
      IPARI(15)=NOINT
      IPARI(7)=NTYP

      IF(STFAC == ZERO) STFAC = ONE_FIFTH

C------------------------------------------------------------
C  Card1 : surface identifiers
C------------------------------------------------------------

      CALL HM_GET_INTV('surf_IDA', ISU1, IS_AVAILABLE, LSUBMODEL)
      CALL HM_GET_INTV('surf_IDL', ISU2, IS_AVAILABLE, LSUBMODEL)
      !--- CHECKS
      IS1=1
      IS2=1
      INGR2USR => IGRSURF(1:NSURF)%ID
      ISU1=NGR2USR(ISU1,INGR2USR,NSURF)
      ISU2=NGR2USR(ISU2,INGR2USR,NSURF)

      !---Storage in data structure
      IPARI(45)=ISU1
      IPARI(46)=ISU2
      IPARI(13)=IS1*10+IS2
      IPARI(20)=ILEV
      FRIGAP(3)=STARTT
      FRIGAP(11)=STOPT
C
C------------------------------------------------------------
C     PRINTOUT
C------------------------------------------------------------
C
        WRITE(IOUT,1501)

C--------------------------------------------------------------
      IF(IS1==0)THEN
        WRITE(IOUT,'(6X,A)')'NO SECONDARY SURFACE INPUT (ALE)'
      ELSEIF(IS1==1)THEN
        WRITE(IOUT,'(6X,A)')'SECONDARY SURFACE INPUT BY SEGMENTS (ALE)'
      ELSEIF(IS1==2)THEN
        WRITE(IOUT,'(6X,A)')'SECONDARY SURFACE INPUT BY NODES (ALE)'
      ELSEIF(IS1==3)THEN
        WRITE(IOUT,'(6X,A)')'SECONDARY SURFACE INPUT BY SEGMENTS (ALE)'
      ELSEIF(IS1==4 )THEN
        WRITE(IOUT,'(6X,A)')'SECONDARY SIDE INPUT BY BRICKS (ALE)'
      ELSEIF(IS1==5 )THEN
        WRITE(IOUT,'(6X,A)')'SECONDARY SIDE INPUT BY SOLID ELEMENTS (ALE)'
      ENDIF
      IF(IS2==0)THEN
        WRITE(IOUT,'(6X,A)')'NO MAIN SURFACE INPUT (LAGRANGE)'
      ELSEIF(IS2==1)THEN
        WRITE(IOUT,'(6X,A)')'MAIN SURFACE INPUT BY SEGMENTS (LAGRANGE)'
      ELSEIF(IS2==2)THEN
        WRITE(IOUT,'(6X,A)')'MAIN SURFACE INPUT BY NODES (LAGRANGE)'
      ELSEIF(IS2==3)THEN
        WRITE(IOUT,'(6X,A)')'MAIN SURFACE INPUT BY SEGMENTS (LAGRANGE)'
      ELSEIF(IS2==4)THEN
        WRITE(IOUT,'(6X,A)')'MAIN SURFACE REFERS TO A HYPER-ELLIPSOIDAL SURFACE'
      ENDIF
C
C--------------------------------------------------------------
 1000 FORMAT(/1X,'  INTERFACE NUMBER :',I10,1X,A)
C------------
      RETURN

 1501 FORMAT(//
     .    '    TYPE==1   ALE(SECONDARY)/LAGRANGE(MAIN)   ' //)

      END
