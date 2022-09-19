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
Chd|  READ_INTER_TYPE02             source/interfaces/int02/read_inter_type02.F
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        DEFINTER                      source/interfaces/interf1/definter.F
Chd|        FREERR                        source/starter/freform.F      
Chd|        NGR2USR                       source/system/nintrr.F        
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE READ_INTER_TYPE02(
     1        IPARI      ,STFAC      ,FRIGAP    ,NOINT    ,IGRNOD      ,
     2        IGRSURF    ,I2RUPT     ,FAC_L     ,FAC_M    ,FAC_T       ,
     3        AREASL     ,DEF_INTER  ,NPC1      ,TITR)
C============================================================================
C     
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
      USE GROUPDEF_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   A n a l y s e   M o d u l e
C-----------------------------------------------
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "scr17_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NOINT
      INTEGER IPARI(*),DEF_INTER(*),NPC1(*)
      my_real
     .   STFAC,AREASL,FAC_L,FAC_T,FAC_M
      my_real
     .   FRIGAP(*),I2RUPT(*)
      CHARACTER TITR*nchartitle
C-----------------------------------------------
      TYPE (GROUP_)  ,TARGET, DIMENSION(NGRNOD)  :: IGRNOD
      TYPE (SURF_)   ,TARGET , DIMENSION(NSURF)   :: IGRSURF
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
#include      "scr03_c.inc"
#include      "scr05_c.inc"
#include      "scr06_c.inc"
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "com09_c.inc"
#include      "units_c.inc"
#include      "warn_c.inc"
#include      "scr12_c.inc"
#include      "sysunit.inc"
#include      "fluid_scr_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER J,IBUC, NTYP, IGSTI,IS1, IS2,PID,IPRINT,
     .     ILEV, HIERA,IDEL2,INT2DEF,INTTH,IMOD,IFILTR,IGNORE,
     .     IFUNS,IFUNN,IFUNT,NUVAR,ISYM,INTKG,IPROJ,IASSIGN,
     .     PENFLAG,IROT,OK,ISU1,ISU2,ISI1,ISI2
      my_real
     .   STARTT,STOPT,KTHE,VISC,DNMAX,DTMAX,SCAL_F,
     .   SCAL_SR,SCAL_D,ALPHA,AREA
      CHARACTER KEY1*ncharkey
!
      INTEGER, DIMENSION(:), POINTER :: INGR2USR
C-----------------------------------------------
C   E x t e r n a l   F u n c t i o n s
C-----------------------------------------------
      INTEGER NGR2USR
C-----------------------------------------------
      DATA INT2DEF/5/
C=======================================================================
C     READING INTERFACE /INTER/TYPE02
C=======================================================================

C Initializations

      IS1=0
      IS2=0
      IBUC=0
      IMOD = 0
      IGSTI = 0
      INTTH = 0
      IDEL2 = 0
      INTKG = 0
      ILEV = 0

C
      STOPT=EP30
      STARTT = ZERO
      VISC = ZERO
      KTHE = ZERO
C
      IASSIGN = 1
C
      NTYP = 2
      IPARI(15)=NOINT
      IPARI(7)=NTYP

C------------------------------------------------------------
C  Card1 :flags
C------------------------------------------------------------

        READ(IIN,REC=IREC,ERR=999,FMT='(A)')LINE
        READ(LINE,ERR=999,FMT=FMT_8I_F)
     .      ISI1,ISI2,IGNORE,ILEV,HIERA,IBUC,IDEL2,PID,FRIGAP(4)

C.....* Default values definition *........

        IPRINT = 0
C
        KEY1='IGNORE'
        CALL DEFINTER(KEY1     ,IGNORE    ,IASSIGN   ,IPRINT   ,
     .                NTYP     ,DEF_INTER)
C
        KEY1='ILEV'
        CALL DEFINTER(KEY1     ,ILEV    ,IASSIGN   ,IPRINT   ,
     .                NTYP     ,DEF_INTER)
C
        KEY1='IBUC'
        CALL DEFINTER(KEY1     ,IBUC    ,IASSIGN   ,IPRINT   ,
     .                NTYP     ,DEF_INTER)
C
        KEY1='IDEL2'
        CALL DEFINTER(KEY1     ,IDEL2    ,IASSIGN   ,IPRINT   ,
     .                NTYP     ,DEF_INTER)

C....* CHECKS AND Storage IPARI FRIGAP *.............
C
        IS1=2
        IS2=1
        INGR2USR => IGRNOD(1:NGRNOD)%ID
        ISU1=NGR2USR(ISI1,INGR2USR,NGRNOD)
        INGR2USR => IGRSURF(1:NSURF)%ID
        ISU2=NGR2USR(ISI2,INGR2USR,NSURF)
C
C-------*CHECKS*-------------------

        IF (ISU1==0) THEN
          CALL ANCMSG(MSGID=1726,
     .               ANMODE=ANINFO_BLIND_1,
     .               MSGTYPE=MSGERROR,
     .               I1=NOINT,
     .               I2=ISI1,
     .               C1=TITR)
           IS1=0
        END IF
        IF(ISU2 == 0) THEN
          CALL ANCMSG(MSGID=1725,
     .               ANMODE=ANINFO_BLIND_1,
     .               MSGTYPE=MSGERROR,
     .               I1=NOINT,
     .               I2=ISI2,
     .               C1=TITR)
          IS2=0
        END IF
C
        IF ((ISU1 > 0).AND.(IGRNOD(ISU1)%NENTITY == 0)) THEN
           CALL ANCMSG(MSGID=1131,
     .   		ANMODE=ANINFO_BLIND_1,
     .  		MSGTYPE=MSGERROR,
     .    		I1=NOINT,
     .    		C1=TITR)
        END IF
C
        IPARI(45) = ISU1
        IPARI(46) = ISU2
        IPARI(13) = IS1*10+IS2
C
! ILEV
        IF (ILEV /= 0 .AND. ILEV /= 1 .AND. ILEV /= 2 .AND.
     .      ILEV /= 3 .AND .ILEV /= 4 .AND. ILEV /= 5 .AND.
     .      ILEV /= 10 .AND. ILEV /= 11 .AND. ILEV /= 12 .AND.
     .      ILEV /= 20 .AND. ILEV /= 21 .AND. ILEV /= 22 .AND.
     .      ILEV /= 25 .AND. ILEV /= 26 .AND. ILEV /= 27 .AND.
     .      ILEV /= 28 .AND. ILEV /= 30) THEN

            CALL ANCMSG(MSGID=1614,
     .                  MSGTYPE=MSGERROR,
     .                  ANMODE=ANINFO_BLIND_2,
     .                  I1=NOINT,
     .                  C1=TITR,
     .                  C2='SPOTFLAG',
     .                  C3='TYPE2',
     .                  I2=ILEV)
        ENDIF
        IF (ICAA == 1 .AND. ILEV == 0) ILEV=4
        IF (ILEV == 5) ILEV=0

! IDEL2
        IF ((ILEV/=0 .AND. ILEV/=1 .AND. ILEV/=5 .AND. ILEV/=25 .AND. ILEV/=26 .AND. ILEV/=27 .AND. ILEV/=28))
     .        IDEL2 = 0
        IF (N2D == 1)  IDEL2 = 0
        IPARI(17) = IDEL2

        IF(IBUC==0)IBUC=2
        IPARI(12) = IBUC

CC---     Hierarchy useless for interface type2 with penalty - hiera set to 0
        IF (ILEV == 25 .or. ILEV == 26 .or. ILEV == 27 .or. ILEV == 28) HIERA = 0

        IPARI(20)=ILEV

        NHIN2=MAX(NHIN2,HIERA)

C.....* Storage IPARI FRIGAP *.........
        IPARI(43) = PID
        IPARI(26) = HIERA
        IPARI(34) = IGNORE

        FRIGAP(4) = FRIGAP(4) * FAC_L

C------------------------------------------------------------
C  Optional Card2 :ILEV = 25,26,27,28
C------------------------------------------------------------
        IF (ILEV == 25 .or. ILEV == 26 .or. ILEV == 27 .or. ILEV == 28) THEN
           IREC=IREC+1
           READ(IIN,REC=IREC,ERR=999,FMT='(A)')LINE
           IF (LINE(1:1) == '/') THEN
              IREC=IREC-1
              I7STIFS=1
              STFAC = ONE
              VISC = FIVEEM2
              IGSTI = 0
              PENFLAG = 0
              IROT = 0
           ELSE
              I7STIFS=1
              READ(LINE,ERR=999,FMT=FMT_2F_3I) STFAC,VISC,PENFLAG,IROT,IGSTI
              IF (STFAC == ZERO) STFAC=ONE
              IF (VISC  == ZERO) VISC=FIVEEM2
           ENDIF
C.....* Default values definition *........
C
           KEY1='IGSTI'
           CALL DEFINTER(KEY1     ,IGSTI    ,IASSIGN   ,IPRINT   ,
     .                  NTYP     ,DEF_INTER)

           IF (IGSTI  == 0) IGSTI=2

C.....* Storage IPARI FRIGAP *.........
           IPARI(58) = IGSTI
           IPARI(43) = PENFLAG
           IPARI(48) = IROT

           FRIGAP(14)= VISC
        ENDIF
C
C------------------------------------------------------------
C  Optional Card3 Card4:ILEV = 20,21,22
C------------------------------------------------------------
        IF (ILEV==20 .OR. ILEV==21 .OR. ILEV==22) THEN
            IREC=IREC+1
Card3
            READ(IIN,REC=IREC,ERR=999,FMT='(A)')LINE
            READ(LINE,ERR=999,FMT=FMT_6I_2F)
     .           IMOD,IFILTR,IFUNS,IFUNN,IFUNT,ISYM,DNMAX,DTMAX
c---
            IREC=IREC+1
Card4
            READ(IIN,REC=IREC,ERR=999,FMT='(A)')LINE
            READ(LINE,ERR=999,FMT=FMT_5F)
     .           SCAL_F,SCAL_SR,SCAL_D,ALPHA,AREA

C.....* CHECK AND Storage IPARI FRIGAP *.........

            IF (IMOD == 0) IMOD = 2   ! default value
            IF (SCAL_F  == ZERO) SCAL_F  = ONE
            IF (SCAL_SR  == ZERO) SCAL_SR  = ONE
            IF (SCAL_D == ZERO) SCAL_D = ONE
C
	    DNMAX = DNMAX*FAC_L
	    DTMAX = DTMAX*FAC_L
            AREA = AREA*FAC_L*FAC_L
            SCAL_F = SCAL_F*FAC_M/(FAC_L*FAC_T**2)
            SCAL_SR = SCAL_SR*FAC_M/(FAC_L*FAC_T**3)
            SCAL_D = SCAL_D*FAC_L
C
            IF (DNMAX   == ZERO) DNMAX = EP20
            IF (DTMAX   == ZERO) DTMAX = EP20
C
            IF (IFILTR  == 1) THEN
              IF (ALPHA  == ZERO) ALPHA  = ONE
              ALPHA = MIN(ALPHA,ONE)
              ALPHA = MAX(ALPHA,ZERO)
            ENDIF
            IF (IFUNN == 0) THEN
              CALL ANCMSG(MSGID=848,
     .                    MSGTYPE=MSGERROR,
     .                    ANMODE=ANINFO,
     .                    I1=NOINT,
     .                    C1=TITR,
     .                    I2=IFUNN,
     .                    C2='Func_sn')
            ENDIF
            IF (IFUNT == 0) THEN
              CALL ANCMSG(MSGID=848,
     .                    MSGTYPE=MSGERROR,
     .                    ANMODE=ANINFO,
     .                    I1=NOINT,
     .                    C1=TITR,
     .                    I2=IFUNT,
     .                    C2='Func_st')
            ENDIF
            NUVAR = 2

C.....* Storage IPARI FRIGAP *.........
            IPARI(35) = NUVAR
            IPARI(43) = IMOD
            IPARI(44) = ISYM
            IPARI(59) = IFILTR
            IPARI(48) = IFUNS
            IPARI(49) = IFUNN
            IPARI(50) = IFUNT
            I2RUPT(1) = SCAL_F
            I2RUPT(2) = SCAL_D
            I2RUPT(3) = SCAL_SR
            I2RUPT(4) = ALPHA
            I2RUPT(5) = DNMAX
            I2RUPT(6) = DTMAX
            AREASL    = AREA
        ENDIF
C------------------------------------------------------------
C  Optional Card5 :THERMAL MODELLING & projection flag (hidden)
C------------------------------------------------------------
c
        IPROJ = 1
C
        IREC = IREC+1
        READ(IIN,REC=IREC,ERR=999,FMT='(A)')LINE
        IF (LINE(1:1) == '/') THEN
            IREC=IREC-1
        ELSE
            READ(LINE,ERR=999,FMT=FMT_I_F_6X_I)
     .       INTTH,KTHE,IPROJ
             IF (IPROJ==0) IPROJ = 1
        ENDIF
        IF(INTTH > 0 ) INTHEAT = 1

        KTHE = KTHE * FAC_M / FAC_T**3
        IPARI(47) = INTTH
        FRIGAP(15)  = KTHE
        IPARI(57) = IPROJ
C-----
        IF (STFAC == ZERO )STFAC = ONE_FIFTH

         FRIGAP(3)=STARTT
         IF (STOPT == ZERO) STOPT = EP30
         FRIGAP(11)=STOPT
         IPARI(65) = INTKG
C
C------------------------------------------------------------
C     RENUMBERING OF FUNCTIONS - USER TO INTERNAL ID
C------------------------------------------------------------

       IF (IPARI(20)==20.OR.IPARI(20)==21.OR.IPARI(20)==22) THEN
c         INTERFACE TYPE2 - sptflg 20/21/22
C--------------------------------
C
          OK = 0
          DO J=1,NFUNCT
            IF (IPARI(48) == NPC1(J)) THEN
              IPARI(48)=J
              EXIT
            ENDIF
          ENDDO
          DO J=1,NFUNCT
            IF (IPARI(49) == NPC1(J)) THEN
              IPARI(49)=J
              OK = 1
              EXIT
            ENDIF
          ENDDO
          DO J=1,NFUNCT
            IF (IPARI(50) == NPC1(J)) THEN
              IPARI(50)=J
              OK = 1
              EXIT
            ENDIF
          ENDDO
          IF (OK == 0) THEN
            CALL ANCMSG(MSGID=121,
     .                  MSGTYPE=MSGERROR,
     .                  ANMODE=ANINFO_BLIND_1,
     .                  I1=NOINT,
     .                  C1=TITR,
     .                  I2=IPARI(11))
          ENDIF
C
       ENDIF
C
C------------------------------------------------------------
C     PRINTOUT
C------------------------------------------------------------
C
       IPRINT = 1
C........* PRINT DEFAULT VALUES *.....................
C
        KEY1='IGNORE'
        CALL DEFINTER(KEY1     ,IGNORE   ,IASSIGN   ,IPRINT   ,
     .                NTYP     ,DEF_INTER)
C
        KEY1='ILEV'
        CALL DEFINTER(KEY1     ,ILEV     ,IASSIGN   ,IPRINT   ,
     .                NTYP     ,DEF_INTER)
C
        KEY1='IBUC'
        CALL DEFINTER(KEY1     ,IBUC     ,IASSIGN   ,IPRINT   ,
     .                NTYP     ,DEF_INTER)
C
        KEY1='IDEL2'
        CALL DEFINTER(KEY1     ,IDEL2    ,IASSIGN   ,IPRINT   ,
     .                NTYP     ,DEF_INTER)
C
        KEY1='IGSTI'
        CALL DEFINTER(KEY1     ,IGSTI    ,IASSIGN   ,IPRINT   ,
     .                NTYP     ,DEF_INTER)
C
C........* PRINT INTERFACES INPUT *.....................

        IF (ILEV == 20 .OR. ILEV == 21 .OR. ILEV == 22) THEN
            WRITE(IOUT,1502)ILEV,HIERA,IBUC,FRIGAP(4),IGNORE
            WRITE(IOUT,2502)SCAL_F,SCAL_D,SCAL_SR,ALPHA,AREASL,
     .            DNMAX,DTMAX,IFUNN,IFUNT,IFUNS,IMOD,ISYM,IFILTR
          ELSEIF (ILEV == 25 .or. ILEV == 26 .or. ILEV == 27 .or. ILEV == 28) THEN
            WRITE(IOUT,1532) ILEV,HIERA,IBUC,FRIGAP(4),STFAC,IGSTI,
     .                       VISC,IGNORE
          ELSEIF (ILEV /= 0) THEN
            WRITE(IOUT,1502)IPARI(20),HIERA,IBUC,FRIGAP(4),IGNORE
          ELSE
            WRITE(IOUT,1502)INT2DEF,HIERA,IBUC,FRIGAP(4),IGNORE
          END IF
          IF (ILEV/=1 .and. ILEV/=30 .and. ILEV/=26) THEN
            WRITE(IOUT,1533) IPROJ
          END IF
          WRITE(IOUT,'(A,A,I5/)')
     . '    DELETION FLAG CASE FAILURE OF MAIN ELEMENT',
     . ' SET TO ',IDEL2
          IF (INTTH > 0) THEN
            WRITE(IOUT,1534) KTHE
          END IF

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
 1300 FORMAT(    /1X,'   INTERFACES               ' /
     .            1X,' --------------             '// )
C------------
      RETURN
 999  CALL FREERR(3)
      RETURN
 1502 FORMAT(//
     .    '    TYPE==2   TIED SLIDING                  ' //
     .    '    FORMULATION LEVEL . . . . . . . . . . . . ',I5/,
     .    '    HIERARCHICAL SOLVING LEVEL. . . . . . . . ',I5/,
     .    '    SEARCH FORMULATION. . . . . . . . . . . . ',I5/,
     .    '    SEARCH DISTANCE . . . . . . . . . . . . . ',1PG20.13/,
     .    '    IGNORE FLAG . . . . . . . . . . . . . . . ',I5/)
 1532 FORMAT(//
     .    '    TYPE==2   TIED SLIDING                  ' //
     .    '    FORMULATION LEVEL . . . . . . . . . . . . ',I5/,
     .    '    HIERARCHICAL SOLVING LEVEL. . . . . . . . ',I5/,
     .    '    SEARCH FORMULATION. . . . . . . . . . . . ',I5/,
     .    '    SEARCH DISTANCE . . . . . . . . . . . . . ',1PG20.13/,
     .    '    STIFFNESS FACTOR. . . . . . . . . . . . . ',1PG20.13/,
     .    '    STIFFNESS FORMULATION . . . . . . . . . . ',I1/,
     .' 1       : STIFFNESS IS COMPUTED ON MAIN SEGMENT'/,
     .' 2,3,4,5 : STIFFNESS IS COMPUTED FROM MAIN SEGMENT'/,
     .'                                 AND SECONDARY NODE'/,
     .    '    CRITICAL DAMPING FACTOR . . . . . . . . . ',1PG20.13/,
     .    '    IGNORE FLAG . . . . . . . . . . . . . . . ',I5/)
 1533 FORMAT(
     .    '    PROJECTION FLAG . . . . . . . . . . . . . ',I1/)
 1534 FORMAT(
     .    '    THERMAL INTERFACE . . . . . . . . . . . . . ',//
     .    '    THERMAL heat exchange coefficient .. . . . .',1PG20.13/)
 2502 FORMAT('    RUPTURE PARAMETERS '
     .         /10X,'SCAL_F  . . . . . . . . . . . . . . ',1PG20.13
     .         /10X,'SCAL_DISP . . . . . . . . . . . . . ',1PG20.13
     .         /10X,'SCAL_SR . . . . . . . . . . . . . . ',1PG20.13
     .         /10X,'FILTERING COEFF . . . . . . . . . . ',1PG20.13
     .         /10X,'DEFAULT SECONDARY AREA. . . . . . . . . ',1PG20.13
     .         /10X,'DN_MAX  . . . . . . . . . . . . . . ',1PG20.13
     .         /10X,'DT_MAX  . . . . . . . . . . . . . . ',1PG20.13
     .         /10X,'IFUNN   . . . . . . . . . . . . . . ',I10
     .         /10X,'IFUNT   . . . . . . . . . . . . . . ',I10
     .         /10X,'IFUNS   . . . . . . . . . . . . . . ',I10
     .         /10X,'IMOD    . . . . . . . . . . . . . . ',I10
     .         /10X,'ISYM    . . . . . . . . . . . . . . ',I10
     .         /10X,'IFILTR  . . . . . . . . . . . . . . ',I10//)
      END
