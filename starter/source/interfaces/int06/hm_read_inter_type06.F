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
Chd|  HM_READ_INTER_TYPE06          source/interfaces/int06/hm_read_inter_type06.F
Chd|-- called by -----------
Chd|        HM_READ_INTER_STRUCT          source/interfaces/reader/hm_read_inter_struct.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        FREERR                        source/starter/freform.F      
Chd|        HM_GET_FLOATV                 source/devtools/hm_reader/hm_get_floatv.F
Chd|        HM_GET_INTV                   source/devtools/hm_reader/hm_get_intv.F
Chd|        NGR2USR                       source/system/nintrr.F        
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE HM_READ_INTER_TYPE06(
     1        IPARI      ,STFAC      ,FRIGAP     ,NOINT     ,NI       ,
     2        IGRSURF    ,NPC1       ,TITR       ,LSUBMODEL ,UNITAB   )
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
C   A n a l y s e   M o d u l e
C-----------------------------------------------
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "scr17_c.inc"
#include      "sphcom.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER ISU1,ISU2,NI, NOINT
      INTEGER IPARI(*),NPC1(*)
      my_real
     .   STFAC
      my_real
     .   FRIGAP(*)
      CHARACTER TITR*nchartitle,TITR1*nchartitle
      TYPE(SUBMODEL_DATA),INTENT(IN)::LSUBMODEL(*)
      TYPE (UNIT_TYPE_),INTENT(IN) ::UNITAB 
C-----------------------------------------------
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
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,L, NTYP, IBID,INACTI,IS1, IS2,ILEV,NCURS,NLO,       
     .    NFRIC, NDAMP1,NDAMP2,IRS,IRM,IFUN1,IFUN2,HFLAG,IKK,II,  
     .    INTKG, NFRIC1,NFRIC2,ICOR,IERR1,IERR2,IFRIC1,IFRIC2,
     .    IDAMP1,IDAMP2,IGSTI
      my_real
     .   FAC,FAC1,FAC2,FAC3,FACF,FACV,FRIC,GAP,STARTT,STOPT,SFRIC,
     .   VISC,FACX,STIFF
      CHARACTER MESS*40, MSGTITL*nchartitle
      CHARACTER OPT*ncharkey,KEY*ncharkey,KEY1*ncharkey,
     .          BCFLAG*ncharfield, BCFLAGM*ncharfield
!
      INTEGER, DIMENSION(:), POINTER :: INGR2USR
      LOGICAL IS_AVAILABLE
C-----------------------------------------------
C   E x t e r n a l   F u n c t i o n s
C-----------------------------------------------
      INTEGER NGR2USR
C=======================================================================
C     READING PENALTY INTERFACE /INTER/TYPE6
C=======================================================================
C Initializations
      IS1=0
      IS2=0
      IFUN1=0
      IFUN2=0         
      IGSTI = 0  
      INACTI = 0
      ILEV = 0      
      INTKG = 0    
C
      FRIC = ZERO
      GAP = ZERO
      STARTT = ZERO
      STOPT=EP30
      VISC = ZERO
      FACX = ZERO
C
      NTYP = 6
      IPARI(15)=NOINT
      IPARI(7)=NTYP
C
      IS_AVAILABLE = .FALSE.
C--------------------------------------------------
C EXTRACT DATAS (INTEGER VALUES)
C--------------------------------------------------
      CALL HM_GET_INTV('secondaryentityids',ISU1,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV('mainentityids',ISU2,IS_AVAILABLE,LSUBMODEL)

      CALL HM_GET_INTV('Gflag',IRS,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV('Vflag',IRM,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV('INACTIV',INACTI,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV('Crx_Fun',NFRIC1,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV('Cry_Fun',NFRIC2,IS_AVAILABLE,LSUBMODEL)

      CALL HM_GET_INTV('FUN_A1',IFUN1,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV('HFLAG1',HFLAG,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV('ISFLAG',ICOR,IS_AVAILABLE,LSUBMODEL)

      CALL HM_GET_INTV('FUNCT_ID',IFUN2,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV('Crz_Fun',NDAMP2,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV('Ctx_Fun',NDAMP1,IS_AVAILABLE,LSUBMODEL)
C--------------------------------------------------
C EXTRACT DATAS (REAL VALUES)
C--------------------------------------------------
      CALL HM_GET_FLOATV('Friction_phi',SFRIC,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('FRIC',FRIC,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('GAP',GAP,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('TSTART',STARTT,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('TSTOP',STOPT,IS_AVAILABLE,LSUBMODEL,UNITAB)

      CALL HM_GET_FLOATV('scale1',FACF,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('scale2',FACV,IS_AVAILABLE,LSUBMODEL,UNITAB)

      CALL HM_GET_FLOATV('FACX',FACX,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('FAC',FAC1,IS_AVAILABLE,LSUBMODEL,UNITAB)

      CALL HM_GET_FLOATV('STIFF1',STIFF,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('PFscale',FAC2,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('VISC',VISC,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('scale3',FAC3,IS_AVAILABLE,LSUBMODEL,UNITAB)

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
      IF (STOPT == ZERO) STOPT = EP30

C.....* Storage IPARI FRIGAP *.......
      FRIGAP(1)=FRIC
      FRIGAP(2)=GAP
      FRIGAP(3)=STARTT
      FRIGAP(11)=STOPT

C....* CHECKS *.............     
    
      IF (HFLAG > 0  .AND. IFUN2 == 0)    HFLAG = 2
      IF (HFLAG > 0  .AND. STIFF == ZERO) HFLAG = 0
      IF (HFLAG == 0 .AND. ICOR == 1)  ICOR = 0
      IF (FACX == ZERO)  FACX = UN
      IF (FAC1 == ZERO)  FAC1 = UN
      IF (FAC2 == ZERO)  FAC2 = UN
      IF (FAC3 == ZERO)  FAC3 = UN
      IF (FACF == ZERO)  FACF = UN
      IF (FACV == ZERO)  FACV = UN
      IF (STIFF == ZERO) STIFF = EP30
      FACX = UN / FACX
      FACF = UN / FACF
      FACV = UN / FACV
C
C.....* Storage IPARI FRIGAP *.......
      STFAC      = FAC1
      IF (STFAC == ZERO) STFAC = UNCINQ
      IPARI(11)  = IFUN1
      IPARI(22)  = INACTI
      IPARI(24)  = IRM
      IPARI(25)  = IRS
      IPARI(47)  = HFLAG
      IPARI(49)  = IFUN2
      IPARI(51)  = NFRIC1
      IPARI(52)  = NDAMP1
      IPARI(53)  = NDAMP2
      IPARI(54)  = NFRIC2
      IPARI(58)  = ICOR
      IPARI(20)  = ILEV
      IPARI(65)  = INTKG
      FRIGAP(5)  = SFRIC
      FRIGAP(19) = FACX
      FRIGAP(20) = FAC2
      FRIGAP(21) = STIFF
      FRIGAP(22) = FACF
      FRIGAP(23) = FACV
      FRIGAP(24) = FAC3
      FRIGAP(14)=VISC
C
C------------------------------------------------------------
C     RENUMBERING OF FUNCTIONS  - USER TO INTERNAL ID
C------------------------------------------------------------
      IERR1 = 1
      DO J=1,NFUNCT
        IF (IPARI(11) == NPC1(J)) THEN
          IPARI(11)=J
          IERR1 = 0
          EXIT
        ENDIF
      ENDDO
      IF (IERR1 == 1) THEN
        CALL ANCMSG(MSGID=121,
     .              MSGTYPE=MSGERROR,
     .              ANMODE=ANINFO_BLIND_1,
     .              I1=NOINT,
     .              C1=TITR,
     .              I2=IPARI(11))
      ENDIF
c
      IF (IPARI(47) > 0 .AND. IPARI(49) /= 0) THEN
        IERR2 = 1
        DO J=1,NFUNCT
          IF(IPARI(49) == NPC1(J)) THEN
            IPARI(49)=J
            IERR2 = 0
            EXIT
          ENDIF
        ENDDO
        IF (IERR2 == 1) THEN
          CALL ANCMSG(MSGID=121,
     .                MSGTYPE=MSGERROR,
     .                ANMODE=ANINFO_BLIND_1,
     .                I1=NOINT,
     .                C1=TITR,
     .                I2=IPARI(49))
        ENDIF
      ENDIF
c
      IFRIC1  = IPARI(51)
      IF (IFRIC1 /= 0) THEN  ! friction coefficient = f(Fn)  
        IERR1 = 1                                           
        DO J=1,NFUNCT                                   
          IF (IFRIC1 == NPC1(J)) THEN            
            IPARI(51) = J                                 
            IERR1 = 0                                       
            EXIT                                            
          ENDIF                                             
        ENDDO                                               
        IF (IERR1 == 1) CALL ANCMSG(MSGID=113,              
     .                              MSGTYPE=MSGERROR,       
     .                              ANMODE=ANINFO,          
     .                              I1=NOINT,               
     .                              C1=TITR,                
     .                              I2=IFRIC1)               
      ENDIF                                                 
c
      IDAMP1 = IPARI(52)
      IF (IDAMP1 /= 0) THEN  ! damping coefficient = f(Fn)  
        IERR1 = 1                                           
        DO J=1,NFUNCT                                   
          IF (IDAMP1 == NPC1(J)) THEN           
            IPARI(52) = J                                 
            IERR1 = 0                                       
            EXIT                                            
          ENDIF                                             
        ENDDO                                               
        IF (IERR1 == 1) CALL ANCMSG(MSGID=113,              
     .                              MSGTYPE=MSGERROR,       
     .                              ANMODE=ANINFO,          
     .                              I1=NOINT,               
     .                              C1=TITR,                
     .                              I2=IDAMP1)              
      ENDIF                                                 
c
      IDAMP2 = IPARI(53)
      IF (IDAMP2 /= 0) THEN  ! damping coefficient = f(Vn)  
        IERR1 = 1                                           
        DO J=1,NFUNCT                                   
          IF (IDAMP2 == NPC1(J)) THEN           
            IPARI(53) = J                                 
            IERR1 = 0                                       
            EXIT                                            
          ENDIF                                             
        ENDDO                                               
        IF (IERR1 == 1) CALL ANCMSG(MSGID=113,              
     .                              MSGTYPE=MSGERROR,       
     .                              ANMODE=ANINFO,          
     .                              I1=NOINT,               
     .                              C1=TITR,                
     .                              I2=IDAMP2)              
      ENDIF                                                 
c
      IFRIC2 = IPARI(54)
      IF (IFRIC2 /= 0) THEN  ! friction coefficient = f(Vn)  
        IERR1 = 1                                           
        DO J=1,NFUNCT                                   
          IF (IFRIC2 == NPC1(J)) THEN           
            IPARI(54) = J                                 
            IERR1 = 0                                       
            EXIT                                            
          ENDIF                                             
        ENDDO                                               
        IF (IERR1 == 1) CALL ANCMSG(MSGID=113,              
     .                              MSGTYPE=MSGERROR,       
     .                              ANMODE=ANINFO,          
     .                              I1=NOINT,               
     .                              C1=TITR,                
     .                              I2=IFRIC2)              
      ENDIF    
C
C------------------------------------------------------------
C     PRINTOUT
C------------------------------------------------------------
      WRITE(IOUT,1506) HFLAG,ICOR,IFUN1,IFUN2,FACX,STFAC,FAC2,
     .                 STIFF,SFRIC,FRIC,NFRIC1,NFRIC2,VISC,
     .                 NDAMP2,NDAMP1,INACTI,GAP,STARTT,STOPT,IRS,IRM                        
C--------------------------------------------------------------
      IF(IS1.EQ.0)THEN
        WRITE(IOUT,'(6X,A)')'NO SECONDARY SURFACE INPUT'
      ELSEIF(IS1.EQ.1)THEN
        WRITE(IOUT,'(6X,A)')'SECONDARY SURFACE INPUT BY SEGMENTS'
      ELSEIF(IS1.EQ.2)THEN
        WRITE(IOUT,'(6X,A)')'SECONDARY SURFACE INPUT BY NODES'
      ELSEIF(IS1.EQ.3)THEN
        WRITE(IOUT,'(6X,A)')'SECONDARY SURFACE INPUT BY SEGMENTS'
      ELSEIF(IS1.EQ.4 )THEN
        WRITE(IOUT,'(6X,A)')'SECONDARY SIDE INPUT BY BRICKS'
      ELSEIF(IS1.EQ.5 )THEN
        WRITE(IOUT,'(6X,A)')'SECONDARY SIDE INPUT BY SOLID ELEMENTS'        
      ENDIF
      IF(IS2.EQ.0)THEN
        WRITE(IOUT,'(6X,A)')'NO MAIN SURFACE INPUT'
      ELSEIF(IS2.EQ.1)THEN
        WRITE(IOUT,'(6X,A)')'MAIN SURFACE INPUT BY SEGMENTS'
      ELSEIF(IS2.EQ.2)THEN
        WRITE(IOUT,'(6X,A)')'MAIN SURFACE INPUT BY NODES'
      ELSEIF(IS2.EQ.3)THEN
        WRITE(IOUT,'(6X,A)')'MAIN SURFACE INPUT BY SEGMENTS'
      ELSEIF(IS2.EQ.4)THEN
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

 1506 FORMAT(//
     .    '    TYPE.EQ.6   RIGID BODY INTERFACE           ' //,
     .    '    FORMULATION FLAG                  . . . .  ',I10/,
     .    '    INITIAL PENETRATION FLAG          . . . .  ',I10/,
     .    '    LOADING FUNCTION ID               . . . .  ',I10/,
     .    '    UNLOADING FUNCTION ID             . . . .  ',I10/,
     .    '    ABSCISSA (DISPLACEMENT) SCALE FACTOR. . .  ',1PG20.13/,
     .    '    LOAD FUNCTION SCALE FACTOR  . . . . . . .  ',1PG20.13/,
     .    '    UNLOAD FUNCTION SCALE FACTOR  . . . . . .  ',1PG20.13/,
     .    '    ELASTIC MODULUS . . . . . . . . . . . . .  ',1PG20.13/,
     .    '    STATIC FRICTION FORCE . . . . . . . . . .  ',1PG20.13/,
     .    '    FRICTION FACTOR . . . . . . . . . . . . .  ',1PG20.13/,
     .    '    FRICTION FUNCTION OF NORMAL FORCE . . . . .',I10/,
     .    '    FRICTION FUNCTION OF SLIP VELOCITY. . . . .',I10/,
     .    '    DAMPING COEFFICIENT . . . . . . . . . . .  ',1PG20.13/,
     .    '    DAMPING AMPLIFIER FUNCTION VS NORMAL FORCE.',I10/,
     .    '    DAMPING FORCE FUNCTION VS VELOCITY. . . . .',I10/,
     .    '    DE-ACTIVATION OF INITIAL PENETRATIONS . . .',I10/,
     .    '    INITIAL GAP . . . . . . . . . . . . . . .  ',1PG20.13/,
     .    '    START TIME. . . . . . . . . . . . . . . .  ',1PG20.13/,
     .    '    STOP TIME . . . . . . . . . . . . . . . .  ',1PG20.13/,
     .    '    SECONDARY SURFACE  REORDERING FLAG . . . . . . ',I1/,
     .    '    MAIN SURFACE  REORDERING FLAG. . . . . . ',I1/)
      END
