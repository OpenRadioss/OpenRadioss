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
Chd|  HM_READ_INTER_TYPE22          source/interfaces/int22/hm_read_inter_type22.F
Chd|-- called by -----------
Chd|        HM_READ_INTER_FSI             source/interfaces/reader/hm_read_inter_fsi.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        HM_GET_INTV                   source/devtools/hm_reader/hm_get_intv.F
Chd|        NGR2USR                       source/system/nintrr.F        
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
       SUBROUTINE HM_READ_INTER_TYPE22(
     1        IPARI      ,STFAC      ,FRIGAP     ,NOINT     ,NI       ,
     2        IGRNOD     ,IGRSURF    ,IGRBRIC    ,IGRSH3N   ,IGRTRUSS ,
     3        FRIC_P     ,TITR       ,LSUBMODEL)  
C============================================================================
C     
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
      USE GROUPDEF_MOD
      USE SUBMODEL_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e 
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "scr17_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER ISU1,ISU2,NI,NOINT
      INTEGER IPARI(*)
      my_real STFAC
      my_real
     .   FRIGAP(*),FRIC_P(*)
      CHARACTER TITR*nchartitle,TITR1*nchartitle
C-----------------------------------------------
      TYPE (GROUP_)  ,TARGET, DIMENSION(NGRNOD)  :: IGRNOD
      TYPE (GROUP_)  ,TARGET, DIMENSION(NGRBRIC) :: IGRBRIC
      TYPE (GROUP_)  , DIMENSION(NGRSH3N) :: IGRSH3N
      TYPE (GROUP_)  , DIMENSION(NGRTRUS) :: IGRTRUSS
      TYPE (SURF_)   ,TARGET , DIMENSION(NSURF)   :: IGRSURF
      TYPE(SUBMODEL_DATA), DIMENSION(NSUBMOD), INTENT(IN) :: LSUBMODEL
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "scr06_c.inc"
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "units_c.inc"
#include      "inter22.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J, NTYP, IBID,INACTI,IGSTI, IVIS2,ILEV,INTKG,
     .    IS1, IS2, IGAP,MULTIMP,IFQ, KK,II,I22GRSH3N,I22GRNOD,
     .    IDELKEEP,INTTH,I22LEN1,I22GRTRUS,I22GRNOD2,I22GRNOD3
      my_real
     .    FRIC,GAP,STARTT,BUMULT,STOPT,VISC,VISCF,RATIO22_
      LOGICAL LOGI1, LOGI2, LOGI_I22GRSH3N, LOGI_I22GRTRUS,LOGI_I22GRNOD
      LOGICAL IS_AVAILABLE
      CHARACTER MESS*40, MSGTITL*nchartitle
      CHARACTER OPT*ncharkey,KEY*ncharkey,KEY1*ncharkey,
     .          BCFLAG*ncharfield, BCFLAGM*ncharfield
!
      INTEGER, DIMENSION(:), POINTER :: INGR2USR
C-----------------------------------------------
C   E x t e r n a l   F u n c t i o n s
C-----------------------------------------------
      INTEGER USR2SYS,SUR2USR,NGR2USR
C=======================================================================
C     READING ALE INTERFACE /INTER/TYPE22
C=======================================================================
C Initializations
      IS1=0
      IS2=0
      ILEV=0
      INTKG =0
C
      NTYP = 22
      IPARI(15)=NOINT
      IPARI(7)=NTYP
      
      IS_AVAILABLE=.FALSE.

C------------------------------------------------------------
C  Card1 
C------------------------------------------------------------

       CALL HM_GET_INTV('grbric_ID', ISU1, IS_AVAILABLE, LSUBMODEL)
       CALL HM_GET_INTV('surf_ID', ISU2, IS_AVAILABLE, LSUBMODEL)
       I22GRSH3N=0
       I22GRTRUS=0
       I22GRNOD=0
       I22GRNOD2=0
       I22GRNOD3=0
       IOUTP22=0
       IBID=0
C
C....* CHECKS *.............
C
       !IF main SIDE IS NOT GIVEN            
       IF(ISU2==0) THEN                      
          CALL ANCMSG(MSGID=119,               
     .                MSGTYPE=MSGERROR,        
     .                ANMODE=ANINFO,           
     .                I1=NOINT,                
     .                C1=TITR)                 
         IS2=0                                 
       ELSE                                    
         IS2=1                                 
         INGR2USR => IGRSURF(1:NSURF)%ID
         ISU2=NGR2USR(ISU2,INGR2USR,NSURF)
       ENDIF                                   
                                               
       !common value : IOUTP22                 
       IF(IOUTP22==0)THEN                    
         IOUTP22=1                             
       ELSEIF(IOUTP22/=1)THEN                
         IOUTP22=0                             
       ENDIF                                   

       ! IF SECONDARY ID SIDE IS GIVEN
       IF(ISU1/=0)THEN
          INGR2USR => IGRBRIC(1:NGRBRIC)%ID
          ISU1=NGR2USR(ISU1,INGR2USR,NGRBRIC)
          IS1 =4
       ELSE
             CALL ANCMSG(MSGID=114,
     .                   MSGTYPE=MSGERROR,
     .                   ANMODE=ANINFO,
     .                   I1=NOINT,
     .                   C1=TITR)
          IS1 =0
       ENDIF
C
C------------------------------------------------------------
C  Card2 
C------------------------------------------------------------
      !=======================================================!
      !   TEMPORARY : flags for equivalent type7 interface    !
      !=======================================================!

        JMULT22=ZERO
        RATIO22_=ZERO

C
C.....* CHECK AND Storage IPARI FRIGAP *.........
C

        IPARI(13)=IS1*10+IS2
        IPARI(45)=ISU1
        IPARI(46)=ISU2
C

        IF(JMULT22==ZERO)JMULT22=ONE
        IF(RATIO22_ == ZERO)RATIO22_ = ONE + TEN/HUNDRED
        RATIO22    = MIN(RATIO22,RATIO22_)
        GAP        = ZERO
        IGSTI      = 0
        INTTH      = 0
        IGAP       = 0
        MULTIMP    = 0
        NTYP       = 22
        STARTT     = ZERO
        STOPT      = ZERO
        IVIS2      = 0
        INACTI     = 0
        VISC       = ZERO
        VISCF      = ZERO
        BUMULT     = ZERO
        STFAC      = ONE
        VISC       = FIVEEM2
        FRIGAP(14) = VISC
        FRIGAP(16) = EP30
        FRIGAP(27) = ONE
        FRIGAP(28) = ZERO
        FRIGAP(24) = ONE
        FRIGAP(25) = ONE
        FRIGAP(20) = ONE/EP30
        FRIGAP(21) = ONE
        FRIGAP(22) = RATIO22_
        IPARI(39)  = 0
        IPARI(40)  = 0
        IPARI(41)  = 0
        IPARI(34)  = 0
        IPARI(47)  = 0
        FRIC       = ZERO
        FRIC_P(1)  = ZERO
        FRIC_P(2)  = ZERO
        FRIC_P(3)  = ZERO
        FRIC_P(4)  = ZERO
        FRIC_P(5)  = ZERO
        FRIC_P(6)  = ZERO
        IPARI(17)  = 0
        IPARI(14)  = 0
        IPARI(30)  = 0
        IPARI(31)  = 0
        IPARI(32)  = 0
        IPARI(44)  = 0 
        IPARI(48)  = I22GRSH3N
        IPARI(49)  = 0           !reserved GRSH3N  ---> sh3n group identifier
        IPARI(50)  = 0           !reserved GRSH3N  ---> sh3n group NEL
        IPARI(51)  = I22GRTRUS
        IPARI(52)  = 0           !reserved GRTRUS  ---> truss group identifier
        IPARI(53)  = 0           !reserved GRTRUS  ---> truss group NEL
        IPARI(35)  = IOUTP22
        IPARI(34)  = I22GRNOD    !orphan nodes
        IPARI(36)  = I22GRNOD2   !orphan nodes        
        IPARI(19)  = I22GRNOD3
        IPARI(70)  = 0           !reserved GRNOD_IID     !iad0 renumbered after ddsplit so retrieve it in engine from group internal id
        IPARI(81)  = 0           !reserved GRNOD_IID     !iad0 renumbered after ddsplit so retrieve it in engine from group internal id
        IPARI(82)  = 0           !reserved GRNOD_IID     !iad0 renumbered after ddsplit so retrieve it in engine from group internal id
        !IGR3SHN flag
        !
        !     IGRN(1,I) : IDENTIFICATEUR DE GROUP
        !     IGRN(2,I) : NOMBRE DE NOEUDS
        !     IGRN(3,I) : ADRESSE DES NOEUDS DANS IBUFSSG
        !     IGRN(4,I) : TYPE (0 POUR NOEUDS, 1 BRIC, 2 QUAD, 3 SHELL, 
        !                         4 TRUSS, 5 BEAM, 6 SPRINGS,7 SHELL_3N)
        !        
        LOGI_I22GRSH3N = .FALSE.
        DO J = 1,NGRSH3N
          IF (IGRSH3N(J)%ID == I22GRSH3N) THEN
           IPARI(49)     = J   ! J is the sh3n group number.
           IPARI(50)     = IGRSH3N(J)%NENTITY
           LOGI_I22GRSH3N = .TRUE.
           EXIT
          END IF
        END DO
        IF( (I22GRSH3N /=0) .AND. (LOGI_I22GRSH3N .EQV. .FALSE.))THEN
          I22GRSH3N    = 0
          IPARI(48:50) = 0          
c          print *, "WARNING : INTER22, GRSH3N ID NOT FOUND IN INPUT FILE"
        ENDIF

        LOGI_I22GRTRUS = .FALSE.
        DO J = 1,NGRTRUS
          IF (IGRTRUSS(J)%ID == I22GRTRUS) THEN
           IPARI(52)     = J
           IPARI(53)     = IGRTRUSS(J)%NENTITY
           LOGI_I22GRTRUS = .TRUE.
           EXIT
          END IF
        END DO
        IF( (I22GRTRUS /=0) .AND. (LOGI_I22GRTRUS .EQV. .FALSE.))THEN
          I22GRTRUS    = 0
          IPARI(51:53) = 0
c          print *, "WARNING : INTER22, GRTRUS ID NOT FOUND IN INPUT FILE"
        ENDIF        

        LOGI_I22GRNOD = .FALSE.
        DO J = 1,NGRNOD
          IF (IGRNOD(J)%ID == I22GRNOD) THEN
           IPARI(70)     = J
           LOGI_I22GRNOD = .TRUE.
           !WRITE(*,*)"IAD0,NODES", IPARI(35),IPARI(44)
           EXIT
          END IF
        END DO
        IF( (I22GRNOD /=0) .AND. (LOGI_I22GRNOD .EQV. .FALSE.))THEN
          I22GRNOD  = 0
          IPARI(70) = 0
c          print *, "WARNING : INTER22, GRNOD ID NOT FOUND IN INPUT FILE"
        ENDIF   
        !-------------!
        LOGI_I22GRNOD = .FALSE.
        DO J = 1,NGRNOD
          IF (IGRNOD(J)%ID == I22GRNOD2) THEN
           IPARI(81)     = J
           LOGI_I22GRNOD = .TRUE.
           !WRITE(*,*)"IAD0,NODES", IPARI(35),IPARI(44)
           EXIT
          END IF
        END DO
        IF( (I22GRNOD2 /=0) .AND. (LOGI_I22GRNOD .EQV. .FALSE.))THEN
          I22GRNOD2  = 0
          IPARI(81)  = 0
c          print *, "WARNING : INTER22, GRNOD ID NOT FOUND IN INPUT FILE"
        ENDIF      
        !-------------!
        LOGI_I22GRNOD = .FALSE.
        DO J = 1,NGRNOD
          IF (IGRNOD(J)%ID == I22GRNOD3) THEN
           IPARI(82)     = J
           LOGI_I22GRNOD = .TRUE.
           !!!!WRITE(*,*)"IAD0,NODES", IPARI(35),IPARI(44)
           EXIT
          END IF
        END DO
        IF( (I22GRNOD3 /=0) .AND. (LOGI_I22GRNOD .EQV. .FALSE.))THEN
          I22GRNOD3  = 0
          IPARI(82)  = 0
c          print *, "WARNING : INTER22, GRNOD ID NOT FOUND IN INPUT FILE"
        ENDIF                
        !-------------!
                 
        KCONTACT =MAX(KCONTACT,0,0)
        INTBAG = MAX(INTBAG,0)  
        
        IF(IS1*IS2/=0)THEN
          INT22  = INT22 + 1                                   !number of int22 interfaces.
          I22LEN1 = MAX(100     ,NINT(82*IGRBRIC(ISU1)%NENTITY**HALF))
          I22LEN1 = MIN(I22LEN1 ,IGRBRIC(ISU1)%NENTITY)
          I22LEN1 = NINT(JMULT22*I22LEN1)
          I22LEN  = MAX( I22LEN , I22LEN1 )                    !maximum nb of 3D elem.
        ENDIF

        !print *, "INTER22 BUFFER LENGTH, I22LEN =", I22LEN
        !print *, "INTER22 GRBRICK SIZE          =", IGRN(2,ISU1)
    
C------------------------------------------------------------
C  General Storage IPARI FRIGAP 
C------------------------------------------------------------
      IPARI(65) = INTKG

      IPARI(20)=ILEV
      IPARI(21)=IGAP
      IPARI(22)=INACTI

      FRIGAP(1)=FRIC
      FRIGAP(2)=GAP
      FRIGAP(3)=STARTT
      IF (STOPT == ZERO) STOPT = EP30
      FRIGAP(11)=STOPT
C BUMULT is increased for big models
      IF(BUMULT==ZERO) THEN
        BUMULT = BMUL0
        IF(NUMNOD > 2500000) THEN
          BUMULT = BMUL0*TWO
        ELSEIF(NUMNOD > 1500000) THEN
          BUMULT = BMUL0*THREE/TWO
        END IF
      END IF
      FRIGAP(4)=BUMULT

C FRIGAP(10) is initialized but used only in engine for storing number of couples candidates  
      FRIGAP(10)=FLOAT(0)

      MULTIMP = 4
      IPARI(23)=MULTIMP
C
C------------------------------------------------------------
C     PRINTOUT
C------------------------------------------------------------
C

      IF(I22GRSH3N>0)WRITE(IOUT,2207)I22GRSH3N, I22GRTRUS,I22GRNOD

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

 2207 FORMAT(//
     .    '    TYPE==22 FSI INTERFACE                    ' //,
     .    '    GRSH3N_ID. . . . . . . . . . . . . . . . . . ',I10/,
     .    '    GRTRUS_ID. . . . . . . . . . . . . . . . . . ',I10/,
     .    '    GRNOD_ID . . . . . . . . . . . . . . . . . . ',I10/)    
      END
