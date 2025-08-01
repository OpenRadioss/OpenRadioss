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
!||    hm_read_inter_type03   ../starter/source/interfaces/int03/hm_read_inter_type03.F
!||--- called by ------------------------------------------------------
!||    hm_read_inter_struct   ../starter/source/interfaces/reader/hm_read_inter_struct.F
!||--- calls      -----------------------------------------------------
!||    hm_get_floatv          ../starter/source/devtools/hm_reader/hm_get_floatv.F
!||    hm_get_intv            ../starter/source/devtools/hm_reader/hm_get_intv.F
!||    ngr2usr                ../starter/source/system/nintrr.F
!||--- uses       -----------------------------------------------------
!||    message_mod            ../starter/share/message_module/message_mod.F
!||    submodel_mod           ../starter/share/modules1/submodel_mod.F
!||====================================================================
      SUBROUTINE HM_READ_INTER_TYPE03(
     1        IPARI      ,STFAC      ,FRIGAP      ,NOINT    ,
     2        IGRSURF    ,UNITAB     ,LSUBMODEL   ,NPARI    ,
     3        NPARIR     )
C============================================================================
C
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
      USE GROUPDEF_MOD
      USE SUBMODEL_MOD
      USE UNITAB_MOD
      USE NAMES_AND_TITLES_MOD , ONLY : NCHARTITLE, NCHARKEY, NCHARFIELD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER,INTENT(IN) :: NPARI, NPARIR !< array sizes (IPARI and FRIGAP)
      INTEGER ISU1,ISU2,ILAGM,NOINT
      INTEGER IPARI(NPARI)
      my_real STFAC
      my_real FRIGAP(NPARIR)
C-----------------------------------------------
      TYPE (SURF_)   ,TARGET , DIMENSION(NSURF)   :: IGRSURF
      TYPE(SUBMODEL_DATA) LSUBMODEL(*)
      TYPE (UNIT_TYPE_),INTENT(IN) ::UNITAB 
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "units_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,L, NTYP,IS1, IS2,IDELKEEP,ILEV,
     .    INACTI, IBC1, IBC2, IBC3,IBC1M, IBC2M, IBC3M,
     .    IGSTI,IDEL3,IVOID,IRS,IRM,INTKG
      my_real
     .   FRIC,GAP,STARTT,STOPT,VISC,VISCF,GAPSCALE,PTMAX
      CHARACTER(LEN=40)::MESS
      CHARACTER(LEN=NCHARTITLE)::MSGTITL
      CHARACTER(LEN=NCHARKEY)::OPT,KEY,KEY1
      CHARACTER(LEN=NCHARFIELD)::BCFLAG,BCFLAGM
!
      INTEGER, DIMENSION(:), POINTER :: INGR2USR
C-----------------------------------------------
C   E x t e r n a l   F u n c t i o n s
C-----------------------------------------------
      INTEGER NGR2USR
      LOGICAL IS_AVAILABLE
C-----------------------------------------------
C=======================================================================
C     READING PENALTY INTERFACE /INTER/TYPE3
C=======================================================================

C Initializations
      IS1=0
      IS2=0
      IDELKEEP=0
      IDEL3= 0
      INACTI = 0
      IGSTI = 0
      ILEV = 0
      IBC1=0
      IBC2=0
      IBC3=0
      IBC1M=0
      IBC2M=0
      IBC3M=0
      INTKG = 0   
C
      FRIC = ZERO
      GAP = ZERO
      GAPSCALE = ZERO
      STARTT = ZERO
      STOPT=EP30
      VISC = ZERO
      VISCF = ZERO
      PTMAX=EP30
C
      NTYP = 3
      IPARI(15)=NOINT
      IPARI(7)=NTYP
C
      IS_AVAILABLE = .FALSE.
C--------------------------------------------------
C EXTRACT DATAS (INTEGER VALUES)
C--------------------------------------------------
C
      CALL HM_GET_INTV('mainentityids',ISU1,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV('secondaryentityids',ISU2,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV('NodDel3',IDEL3,IS_AVAILABLE,LSUBMODEL)
C
      CALL HM_GET_INTV('Deactivate_X_BC',IBC1,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV('Deactivate_Y_BC',IBC2,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV('Deactivate_Z_BC',IBC3,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV('Gflag',IRS,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV('Vflag',IRM,IS_AVAILABLE,LSUBMODEL)
C
C--------------------------------------------------
C EXTRACT DATAS (REAL VALUES)
C--------------------------------------------------

      CALL HM_GET_FLOATV('TYPE3_SCALE',STFAC,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('FRIC',FRIC,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('GAP',GAP,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('TSTART',STARTT,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('TSTOP',STOPT,IS_AVAILABLE,LSUBMODEL,UNITAB)
C
      CALL HM_GET_FLOATV('Ptlim',PTMAX,IS_AVAILABLE,LSUBMODEL,UNITAB)
C
C--------------------------------------------------
C CHECKS And  Storage IPARI FRIGAP 
C--------------------------------------------------
C

C
C....* Card1 :flags  *.............
C

       IS1=1
       IS2=1
       INGR2USR => IGRSURF(1:NSURF)%ID
       ISU1=NGR2USR(ISU1,INGR2USR,NSURF)
       ISU2=NGR2USR(ISU2,INGR2USR,NSURF)
       IF (IDEL3 < 0) THEN
          IDELKEEP=1
          IDEL3=ABS(IDEL3)
       END IF
       IPARI(61)=IDELKEEP
       IF (IDEL3>2.OR.N2D==1) IDEL3 = 0
       IPARI(17)=IDEL3

C.......* Storage IPARI FRIGAP *........
      IPARI(45)=ISU1
      IPARI(46)=ISU2
      IPARI(13)=IS1*10+IS2
      IPARI(20)=ILEV

C
C....* Card2  *.............
C
       IF(STFAC==ZERO) STFAC=ONE_FIFTH

       IF (STOPT == ZERO) STOPT = EP30

C.....* Storage IPARI FRIGAP *.......
       FRIGAP(1)=FRIC
       FRIGAP(2)=GAP
       FRIGAP(3)=STARTT
       FRIGAP(11)=STOPT

C
C....* Card3   *.............
C
        IPARI(11)=4*IBC1+2*IBC2+IBC3 + 8 *(4*IBC1M+2*IBC2M+IBC3M)


       IPARI(24) = IRM
       IPARI(25) = IRS

       IPARI(11)=4*IBC1+2*IBC2+IBC3 + 8 *(4*IBC1M+2*IBC2M+IBC3M)
       FRIGAP(14)=VISC

C
C....* Card4   *.............
C
       IF(PTMAX==ZERO) PTMAX=EP30

       FRIGAP(16)=PTMAX
C
       IPARI(65) = INTKG

C
C------------------------------------------------------------
C     PRINTOUT
C------------------------------------------------------------
C
        WRITE(IOUT,1503)IBC1,IBC2,IBC3,STFAC,FRIC,GAP,STARTT,STOPT,
     .                 IRS,IRM,PTMAX
        IF(IDEL3/=0) THEN
          WRITE(IOUT,'(A,A,I5/)')
     . '    DELETION FLAG ON FAILURE OF ELEMENT',
     . ' (1:YES-ALL/2:YES-ANY) SET TO ',IDEL3
          IF(IDELKEEP == 1)THEN
            WRITE(IOUT,'(A/)')
     . '    IDEL: DO NOT REMOVE NON-CONNECTED NODES FROM SECONDARY SURFACE'
          ENDIF         
        ENDIF

C--------------------------------------------------------------
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
C--------------------------------------------------------------
 1000 FORMAT(/1X,'  INTERFACE NUMBER :',I10,1X,A)
C------------
      RETURN

 1503 FORMAT(//
     .    '    TYPE==3   SLIDING AND VOIDS             ' //,
     .    '    BOUND. COND. DELETED AFTER IMPACT IN X DIR  ',I1/,
     .    '       (1:YES 0:NO)                      Y DIR  ',I1/,
     .    '                                         Z DIR  ',I1/,
     .    '    STIFFNESS FACTOR. . . . . . . . . . . . . ',1PG20.13/,
     .    '    FRICTION FACTOR . . . . . . . . . . . . . ',1PG20.13/,
     .    '    INITIAL GAP . . . . . . . . . . . . . . . ',1PG20.13/,
     .    '    START TIME. . . . . . . . . . . . . . . . ',1PG20.13/,
     .    '    STOP TIME . . . . . . . . . . . . . . . . ',1PG20.13/,
     .    '    SECONDARY SURFACE REORDERING FLAG . . . . . . ',I1/,
     .    '    MAIN SURFACE REORDERING FLAG. . . . . . ',I1/,
     .    '    TANGENTIAL PRESSURE LIMIT. . .. . . . . . ',1PG20.13/)

      END
