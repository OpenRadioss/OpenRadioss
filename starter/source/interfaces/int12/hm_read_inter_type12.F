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
Chd|  HM_READ_INTER_TYPE12          source/interfaces/int12/hm_read_inter_type12.F
Chd|-- called by -----------
Chd|        HM_READ_INTER_FSI             source/interfaces/reader/hm_read_inter_fsi.F
Chd|-- calls ---------------
Chd|        HM_GET_FLOATV                 source/devtools/hm_reader/hm_get_floatv.F
Chd|        HM_GET_INTV                   source/devtools/hm_reader/hm_get_intv.F
Chd|        NGR2USR                       source/system/nintrr.F        
Chd|        USR2SYS                       source/system/sysfus.F        
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE HM_READ_INTER_TYPE12(
     1        IPARI      ,STFAC      ,FRIGAP     ,NOINT     ,NI       ,
     2        IGRSURF    ,ITAB       ,ITABM1     ,ISKN      ,TITR     ,
     3        LSUBMODEL  ,UNITAB)
C============================================================================
C
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
#include      "param_c.inc"
#include      "com04_c.inc"
#include      "com09_c.inc"
#include      "units_c.inc"
#include      "submod_c.inc"
#include      "scr17_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER ISU1,ISU2,NI,NOINT
      INTEGER IPARI(*),ISKN(LISKN,*),ITAB(*),ITABM1(*)
      my_real
     .   STFAC
      my_real
     .   FRIGAP(*)
      CHARACTER TITR*nchartitle,TITR1*nchartitle
      TYPE(SUBMODEL_DATA) LSUBMODEL(NSUBMOD)
      TYPE (UNIT_TYPE_),INTENT(IN) ::UNITAB 
C-----------------------------------------------
      TYPE (SURF_)   ,TARGET , DIMENSION(NSURF)   :: IGRSURF
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,L, NTYP,IS1, IS2,IGSTI,ILEV,ITIED,HIERA,
     .   BCOPT,ISKEW,ICENTER
      my_real
     .   FRIC,GAP,STARTT,STOPT,BID,XC,YC,ZC,XR,YR,ZR,TETA,
     .   XT,YT,ZT
      CHARACTER MESS*40, MSGTITL*nchartitle
      CHARACTER OPT*ncharkey,KEY*ncharkey,KEY1*ncharkey,
     .          BCFLAG*ncharfield, BCFLAGM*ncharfield
!
      INTEGER, DIMENSION(:), POINTER :: INGR2USR
      LOGICAL IS_AVAILABLE
C-----------------------------------------------
C   E x t e r n a l   F u n c t i o n s
C-----------------------------------------------
      INTEGER USR2SYS,SUR2USR,NGR2USR
C-----------------------------------------------
C=======================================================================
C     READING ALE INTERFACE /INTER/TYPE12
C=======================================================================

C Initializations
      IS1=0
      IS2=0
      IGSTI=0
      ILEV= 0
      HIERA=0
      BCOPT=0
C
      FRIC = ZERO
      GAP = ZERO
      STARTT = ZERO
      STOPT=EP30

C
      NTYP = 12
      IPARI(15)=NOINT
      IPARI(7)=NTYP
      
      IS_AVAILABLE=.FALSE.

C------------------------------------------------------------
C  Card1 :flags
C------------------------------------------------------------

      CALL HM_GET_INTV('secondaryentityids', ISU1, IS_AVAILABLE, LSUBMODEL)
      CALL HM_GET_INTV('mainentityids', ISU2, IS_AVAILABLE, LSUBMODEL)
      CALL HM_GET_INTV('type12_interpol', ILEV, IS_AVAILABLE, LSUBMODEL)
      IGSTI=0
      HIERA=0

C....* CHECKS *.............

      IS1=1
      IS2=1
      INGR2USR => IGRSURF(1:NSURF)%ID
      ISU1=NGR2USR(ISU1,INGR2USR,NSURF)
      ISU2=NGR2USR(ISU2,INGR2USR,NSURF)

C.......* Storage IPARI FRIGAP *........
      IPARI(45)=ISU1
      IPARI(46)=ISU2
      IPARI(13)=IS1*10+IS2

C------------------------------------------------------------
C  Card2
C------------------------------------------------------------
      CALL HM_GET_FLOATV('type12_tol', GAP, IS_AVAILABLE, LSUBMODEL, UNITAB)
      BID=ZERO
      STARTT=ZERO
      STOPT=ZERO
C
C.....* CHECKS *.....
C
      IF(GAP==0.)GAP=TWO*EM02
C------------------------------------------------------------
C  Card3
C------------------------------------------------------------
      CALL HM_GET_INTV('type12_itied', ITIED, IS_AVAILABLE, LSUBMODEL)
      CALL HM_GET_INTV('type12_bcopt', BCOPT, IS_AVAILABLE, LSUBMODEL)
      CALL HM_GET_INTV('SKEW_CSID', ISKEW, IS_AVAILABLE, LSUBMODEL)
      CALL HM_GET_INTV('Node_C', ICENTER, IS_AVAILABLE, LSUBMODEL)

C.....* CHECKS *.....

      IF(HIERA==0)HIERA=ITIED+1
      IF(BCOPT==0)BCOPT=2
      IPARI(26)=HIERA
      NHIN2=MAX(NHIN2,HIERA)

      IPARI(11)=BCOPT
C
C------------------------------------------------------------
C  Optional Card4 Card5 Card6 : Transformation parameters 
C------------------------------------------------------------
      IF(ITIED==2)THEN
         CALL HM_GET_FLOATV('type12_Xc', XC, IS_AVAILABLE, LSUBMODEL, UNITAB)
         CALL HM_GET_FLOATV('type12_Yc', YC, IS_AVAILABLE, LSUBMODEL, UNITAB)
         CALL HM_GET_FLOATV('type12_Zc', ZC, IS_AVAILABLE, LSUBMODEL, UNITAB)

         CALL HM_GET_FLOATV('type12_XN', XR, IS_AVAILABLE, LSUBMODEL, UNITAB)
         CALL HM_GET_FLOATV('type12_YN', YR, IS_AVAILABLE, LSUBMODEL, UNITAB)
         CALL HM_GET_FLOATV('type12_ZN', ZR, IS_AVAILABLE, LSUBMODEL, UNITAB)
         CALL HM_GET_FLOATV('type12_theta', TETA, IS_AVAILABLE, LSUBMODEL, UNITAB)

         CALL HM_GET_FLOATV('type12_XT', XT, IS_AVAILABLE, LSUBMODEL, UNITAB)
         CALL HM_GET_FLOATV('type12_YT', YT, IS_AVAILABLE, LSUBMODEL, UNITAB)
         CALL HM_GET_FLOATV('type12_ZT', ZT, IS_AVAILABLE, LSUBMODEL, UNITAB)

       ENDIF

C.....* Storage IPARI FRIGAP *.......
      FRIGAP(3)=STARTT
      IF (STOPT == ZERO) STOPT = EP30
      FRIGAP(11)=STOPT

C------------------------------------------------------------
C  General Storage IPARI FRIGAP 
C------------------------------------------------------------

      IF (STFAC == ZERO) STFAC = ONE_FIFTH
      FRIGAP(1)=ITIED+0.1
      FRIGAP(2)=GAP

C FRIGAP 5->14 EST MIS A JOUR DANS IN12R
      IF(ITIED==2) THEN
        FRIGAP(4)=TETA
        FRIGAP(5)=XT
        FRIGAP(6)=YT
        FRIGAP(7)=ZT
        FRIGAP(8)=XC
        FRIGAP(9)=YC
        FRIGAP(10)=ZC
        FRIGAP(12)=XR
        FRIGAP(13)=YR
        FRIGAP(14)=ZR
      ELSE
         IPARI(20)=ILEV
         IPARI(21)=0
         IF(ICENTER>0)THEN
           IPARI(22)=USR2SYS(ICENTER,ITABM1,MESS,IPARI(15))
         ELSE
           IPARI(22)=0
         ENDIF
         IF(ILEV==1)THEN
          IF(ISKEW>0)THEN
           DO 640 J=0,NUMSKW
            IF(ISKEW==ISKN(4,J+1)) THEN
              ISKEW=J
              GO TO 660
            ENDIF
 640       CONTINUE
           WRITE(ISTDO,641)
           WRITE(IOUT,641)
 641       FORMAT(' ** ERROR INTERF TYPE 12 WRONG SKEW SYSTEM NUMBER')
           IERR=IERR+1
 660       CONTINUE
           IF(ISKN(1,J+1)==0)THEN
             WRITE(ISTDO,'(a)')'** WARNING INTERFACE 12'
             IWARN=IWARN+1
             WRITE(IOUT,642) ICENTER
 642  FORMAT(' ** INTERF TYPE 12 SKEW SYSTEM IS FIXED,',
     &        ' USING CENTER NODE', I8,
     &        ' AND SKEW AXIS 1 FOR POLAR COORDINATE SYSTEM')
           ELSE
             ICENTER=ITAB(ISKN(1,J+1))
             IPARI(22)=ISKN(1,J+1)
           ENDIF
          ELSE
             WRITE(ISTDO,'(a)')'** WARNING INTERFACE 12'
             IWARN=IWARN+1
             WRITE(IOUT,643)
 643  FORMAT(' ** INTERF TYPE 12, USING ORIGIN AND X-AXIS',
     &        ' FOR POLAR COORDINATE SYSTEM')
          ENDIF
          IPARI(21)=ISKEW
         ENDIF
       ENDIF

C
C------------------------------------------------------------
C     PRINTOUT
C------------------------------------------------------------
C
        WRITE(IOUT,1512)GAP,ITIED,IPARI(11)
        IF(IPARI(20)==1)WRITE(IOUT,2512)IPARI(21),ICENTER
        IF(IPARI(20)==2)WRITE(IOUT,2513)IPARI(21)
        IF(ITIED==2) WRITE(IOUT,1513)XT,YT,ZT,XC,YC,ZC,XR,YR,ZR,TETA

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


 1512 FORMAT(//
     .    '    TYPE==12  FLUID/FLUID INTERFACE           ' //,
     .    '    TOLERANCE TO FIND MAIN SEGMENT  . . . . . ',1PG20.13/,
     .    '    ITIED . . . . . . . . . . . . . . . . . . . ',I1/,
     .    '       0: SLIDING (NOVOID)'/,
     .    '       1: TIED '/,
     .    '       2: PERIODIC BOUNDARY CONDITION '/,
     .    '       3: SLIDING NO FLUX '/,
     .    '    BCCOD (DEFAULT 2) . . . . . . . . . . . . . ',I1/,
     .    '       1: NORMAL CHECK '/,
     .    '       2: SECONDARY DEACTIVATION (RBY & INTER TYPE2) '/,
     .    '       3: SECONDARY DEACTIVATION (B.C., RBY & INTER TYPE2)'/)

 1513 FORMAT(
     .    '    TRANSLATION VECTOR  XT  . . . . . . . . . . ',1PG20.13/,
     .    '                        YT  . . . . . . . . . . ',1PG20.13/,
     .    '                        ZT  . . . . . . . . . . ',1PG20.13/,
     .    '    ROTATION CENTER     XC  . . . . . . . . . . ',1PG20.13/,
     .    '                        YC  . . . . . . . . . . ',1PG20.13/,
     .    '                        ZC  . . . . . . . . . . ',1PG20.13/,
     .    '    ROTATION VECTOR     XR  . . . . . . . . . . ',1PG20.13/,
     .    '                        YR  . . . . . . . . . . ',1PG20.13/,
     .    '                        ZR  . . . . . . . . . . ',1PG20.13/,
     .    '    ROTATION ANGLE    TETA  . . . . . . . . . . ',1PG20.13/)

 2512 FORMAT( '    POLAR INTERPOLATION : SKEW SYSTEM NUMBER  . ',I10/,
     .    '    CENTER NODE . . . . . . . . . . . . . . . . ',I10/)
 2513 FORMAT( '    SPHERICAL INTERPOLATION : CENTER NODE . . . ',I10/)


      END
