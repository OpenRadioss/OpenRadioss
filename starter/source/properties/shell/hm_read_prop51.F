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
Chd|  HM_READ_PROP51                source/properties/shell/hm_read_prop51.F
Chd|-- called by -----------
Chd|        HM_READ_PROPERTIES            source/properties/hm_read_properties.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        HM_GET_FLOATV                 source/devtools/hm_reader/hm_get_floatv.F
Chd|        HM_GET_FLOAT_ARRAY_2INDEXES   source/devtools/hm_reader/hm_get_float_array_2indexes.F
Chd|        HM_GET_FLOAT_ARRAY_INDEX      source/devtools/hm_reader/hm_get_float_array_index.F
Chd|        HM_GET_INTV                   source/devtools/hm_reader/hm_get_intv.F
Chd|        HM_GET_INT_ARRAY_2INDEXES     source/devtools/hm_reader/hm_get_int_array_2indexes.F
Chd|        HM_GET_INT_ARRAY_INDEX        source/devtools/hm_reader/hm_get_int_array_index.F
Chd|        HM_OPTION_IS_ENCRYPTED        source/devtools/hm_reader/hm_option_is_encrypted.F
Chd|        SUBROTVECT                    source/model/submodel/subrot.F
Chd|        ELBUFTAG_MOD                  share/modules1/elbuftag_mod.F 
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        STACK_MOD                     share/modules1/stack_mod.F    
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE HM_READ_PROP51(
     .           GEO      ,IGEO     ,PM       ,IPM       ,ISKN     ,  
     .           PROP_ID  ,PROP_TAG ,RTRANS   ,SUB_ID   ,STACK_INFO,
     .           TITR     ,UNITAB   ,LSUBMODEL)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE ELBUFTAG_MOD            
      USE UNITAB_MOD
      USE MESSAGE_MOD
      USE SUBMODEL_MOD
      USE STACK_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "units_c.inc"
#include      "com04_c.inc"
#include      "param_c.inc"
#include      "scr16_c.inc"
#include      "scr17_c.inc"
#include      "scr21_c.inc"
#include      "tablen_c.inc"
#include      "submod_c.inc"
#include      "sphcom.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER, INTENT(IN) :: PROP_ID,SUB_ID
      INTEGER, INTENT(INOUT) :: IGEO(NPROPGI)
      INTEGER, INTENT(IN) :: IPM(NPROPMI,*)
      INTEGER :: ISKN(LISKN,*)
      my_real, INTENT(INOUT) :: GEO(NPROPG)
      my_real, INTENT(IN) ::  PM(NPROPM,*),RTRANS(NTRANSF,*)
      CHARACTER(LEN = nchartitle) :: TITR
      TYPE (PROP_TAG_) , DIMENSION(0:MAXPROP) :: PROP_TAG
      TYPE (UNIT_TYPE_), INTENT(IN) :: UNITAB
      TYPE (SUBMODEL_DATA), DIMENSION(NSUBMOD), INTENT(IN) :: LSUBMODEL
      TYPE(STACK_INFO_) , TARGET :: STACK_INFO
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER :: I,J,K,KK,M1,ISHELL,ISH3N,ISMSTR,ISROT,ISTRAIN,IINT,ITHK,
     .   IORTH,IPOS,IGMAT,ISHEAR,IPLAST,NPLY,NP,NSUB,NISUB,IGTYP,IMID,
     .   ISK,IDSK,PLY_ID,IPID0,IDSUB,INTER,IS,LAMIN,IPID1,IPID2,NPT_SUB,
     .   IRP,IG
      my_real :: PTHK,ZSHIFT,HM,HF,HR,DM,DN,ASHEAR,VX,VY,VZ,FAILEXP,CVIS,
     .           NORM,ANG,POS,PTHKLY,WEIGHT
      LOGICAL :: IS_AVAILABLE, IS_ENCRYPTED, LFOUND
C=======================================================================
      IS_AVAILABLE = .FALSE.
      IS_ENCRYPTED = .FALSE.
c
      IGTYP   = 51
      IGMAT   = 1
      ISTRAIN = 1
      CVIS    = ZERO
      IRP = 0
      IDSK = 0
c--------------------------------------------
c     check encryption
c--------------------------------------------
c
      CALL HM_OPTION_IS_ENCRYPTED(IS_ENCRYPTED)
c  
c--------------------------------------------
c     Read input cards from prop_p51.cfg
c--------------------------------------------
card1
      CALL HM_GET_INTV('Ishell', ISHELL, IS_AVAILABLE, LSUBMODEL)
      CALL HM_GET_INTV('Ismstr', ISMSTR, IS_AVAILABLE, LSUBMODEL)
      CALL HM_GET_INTV('ISH3N' , ISH3N , IS_AVAILABLE, LSUBMODEL)
      CALL HM_GET_INTV('Idrill', ISROT , IS_AVAILABLE, LSUBMODEL)
      CALL HM_GET_FLOATV('P_Thick_Fail', PTHK, IS_AVAILABLE, LSUBMODEL, UNITAB)  
      CALL HM_GET_FLOATV('Z0'          , ZSHIFT, IS_AVAILABLE, LSUBMODEL, UNITAB)  
card2      
      CALL HM_GET_FLOATV('Hm', HM, IS_AVAILABLE, LSUBMODEL, UNITAB)  
      CALL HM_GET_FLOATV('Hf', HF, IS_AVAILABLE, LSUBMODEL, UNITAB)  
      CALL HM_GET_FLOATV('Hr', HR, IS_AVAILABLE, LSUBMODEL, UNITAB)  
      CALL HM_GET_FLOATV('Dm', DM, IS_AVAILABLE, LSUBMODEL, UNITAB)  
      CALL HM_GET_FLOATV('Dn', DN, IS_AVAILABLE, LSUBMODEL, UNITAB)  
card3      
c      CALL HM_GET_INTV  ('ISTRAIN'   ,ISTRAIN ,IS_AVAILABLE, LSUBMODEL) ! always = 1
      CALL HM_GET_FLOATV('AREA_SHEAR',ASHEAR  ,IS_AVAILABLE, LSUBMODEL, UNITAB)  
      CALL HM_GET_INTV  ('Iint'      ,IINT    ,IS_AVAILABLE, LSUBMODEL)
      CALL HM_GET_INTV  ('Ithick'    ,ITHK    ,IS_AVAILABLE, LSUBMODEL)
      CALL HM_GET_FLOATV('Fexp'      ,FAILEXP ,IS_AVAILABLE, LSUBMODEL, UNITAB)  
card4
      CALL HM_GET_FLOATV('V_X'       ,VX      ,IS_AVAILABLE, LSUBMODEL, UNITAB)  
      CALL HM_GET_FLOATV('V_Y'       ,VY      ,IS_AVAILABLE, LSUBMODEL, UNITAB)  
      CALL HM_GET_FLOATV('V_Z'       ,VZ      ,IS_AVAILABLE, LSUBMODEL, UNITAB)  
      CALL HM_GET_INTV('SKEW_CSID'   ,IDSK    ,IS_AVAILABLE, LSUBMODEL)
      CALL HM_GET_INTV('Iorth'       ,IORTH   ,IS_AVAILABLE, LSUBMODEL)
      CALL HM_GET_INTV('Ipos'        ,IPOS    ,IS_AVAILABLE, LSUBMODEL)
      CALL HM_GET_INTV('Ip',IRP,IS_AVAILABLE,LSUBMODEL)
c--------------------------------------------
c     Read ply input cards from laminate.cfg  & sub_laminate.cfg
c     either using list of plies or list of sub_stacks with interfaces
c     Fill up STACK_INFO data base
c--------------------------------------------
      CALL HM_GET_INTV('laminateconfig'    ,LAMIN, IS_AVAILABLE, LSUBMODEL)
c
      NSUB  = 0     ! nb of substacks
      NISUB = 0     ! nb of substack interfaces
      IF (LAMIN  > 0) THEN
        NPLY = 0
        CALL HM_GET_INTV('sublaminateidlistmax' ,NSUB,  IS_AVAILABLE, LSUBMODEL)
        CALL HM_GET_INTV('interfacepairsize'    ,NISUB, IS_AVAILABLE, LSUBMODEL)
c
        DO IS = 1,NSUB
          CALL HM_GET_INT_ARRAY_2INDEXES('plyidlistmax',NPT_SUB,IS,1,IS_AVAILABLE,LSUBMODEL)
          CALL HM_GET_INT_ARRAY_INDEX('DUMMY',IDSUB,IS,IS_AVAILABLE,LSUBMODEL)
          STACK_INFO%SUB(2*(IS-1) + 1) = IDSUB                      
          STACK_INFO%SUB(2*(IS-1) + 2) = NPT_SUB                    
c
          DO I = 1,NPT_SUB
            NPLY = NPLY + 1
            CALL HM_GET_INT_ARRAY_2INDEXES('plyidlist',PLY_ID,IS,I,IS_AVAILABLE,LSUBMODEL)
            CALL HM_GET_FLOAT_ARRAY_2INDEXES('Prop_phi',ANG,IS,I,IS_AVAILABLE,LSUBMODEL,UNITAB)
            CALL HM_GET_FLOAT_ARRAY_2INDEXES('Prop_Zi',POS,IS,I,IS_AVAILABLE,LSUBMODEL,UNITAB)
            CALL HM_GET_FLOAT_ARRAY_2INDEXES('P_thick_fail_lam',PTHKLY,IS,I,IS_AVAILABLE,LSUBMODEL,UNITAB)
            CALL HM_GET_FLOAT_ARRAY_2INDEXES('F_weight_i',WEIGHT,IS,I,IS_AVAILABLE,LSUBMODEL,UNITAB)
            IF (PTHKLY == ZERO) PTHKLY = ONE-EM06
            PTHKLY = MIN(PTHKLY, ONE)
            PTHKLY = MAX(PTHKLY,-ONE)
            IF (WEIGHT == ZERO) WEIGHT = ONE
            STACK_INFO%PID(NPLY) = PLY_ID
            STACK_INFO%ANG(NPLY) = ANG
            STACK_INFO%POS(NPLY) = POS
            STACK_INFO%THKLY(NPLY)  = PTHKLY
            STACK_INFO%WEIGHT(NPLY) = WEIGHT
          END DO
        END DO
c
        IF (NISUB > 0) THEN
          DO I = 1,NISUB
            CALL HM_GET_INT_ARRAY_2INDEXES('interfacepairplyids',IPID1,1,I,IS_AVAILABLE,LSUBMODEL)
            CALL HM_GET_INT_ARRAY_2INDEXES('interfacepairplyids',IPID2,2,I,IS_AVAILABLE,LSUBMODEL)
            STACK_INFO%ISUB(3*(I-1) + 1) = IPID1                    
            STACK_INFO%ISUB(3*(I-1) + 2) = IPID2                    
          END DO
        END IF  
      ELSE  ! property defined by a list of plies
        CALL HM_GET_INTV('plyidlistmax' ,NPLY ,IS_AVAILABLE ,LSUBMODEL)
        DO I=1,NPLY
          CALL HM_GET_INT_ARRAY_INDEX  ('plyidlist' ,PLY_ID,I,IS_AVAILABLE,LSUBMODEL)
          CALL HM_GET_FLOAT_ARRAY_INDEX('Prop_phi',ANG,I,IS_AVAILABLE,LSUBMODEL,UNITAB)   
          CALL HM_GET_FLOAT_ARRAY_INDEX('Prop_Zi' ,POS,I,IS_AVAILABLE,LSUBMODEL,UNITAB)   
          CALL HM_GET_FLOAT_ARRAY_INDEX('P_thick_fail_lam' ,PTHKLY,I,IS_AVAILABLE,LSUBMODEL,UNITAB)   
          CALL HM_GET_FLOAT_ARRAY_INDEX('F_weight_i' ,WEIGHT,I,IS_AVAILABLE,LSUBMODEL,UNITAB)   
c
          IF (PTHKLY == ZERO) PTHKLY = ONE-EM06
          PTHKLY = MIN(PTHKLY, ONE)
          PTHKLY = MAX(PTHKLY,-ONE)
          IF (WEIGHT == ZERO) WEIGHT = ONE
          STACK_INFO%PID(I) = PLY_ID
          STACK_INFO%ANG(I) = ANG
          STACK_INFO%POS(I) = POS
          STACK_INFO%THKLY(I)  = PTHKLY
          STACK_INFO%WEIGHT(I) = WEIGHT
          ! old format of interply not supported
        END DO
      END IF
c--------------------------------------------
c     Default values
c--------------------------------------------
      IF (PTHK == ZERO) PTHK = ONE-EM06
      PTHK = MIN(PTHK, ONE)
      PTHK = MAX(PTHK,-ONE)
      IF (ISHELL == 0) ISHELL = IHBE_D
c      IHBEOUTP = ISHELL
      IF (ISH3N  == 0) ISH3N = ISH3N_D
      IF (ITHK == 0)   ITHK = ITHK_D
      IF (ITHK_D==-2)  ITHK = -1
      ISHEAR = ISHEA_D
      IF (ISHEAR == 1) THEN
        ISHEAR = 1
      ELSEIF (ISHEAR==2) THEN
        ISHEAR = 0
      ENDIF
      IPLAST = IPLA_D
      IF (IPLA_D == -2) IPLAST = -1
c
      IF (ISROT == 0) ISROT = IDRIL_D
      IF (ISROT == 2) ISROT = 0
      IF (ISMSTR== 10 .AND. ISROT > 0 .AND. IDROT == 0) IDROT = 1  ! rotational dofs
      IF (ISMSTR == 0) ISMSTR = 2
      IF (ISMSTR == 3.AND. ISHELL /= 0 .AND. ISHELL /= 2) THEN
        ISMSTR = 2
        CALL ANCMSG(MSGID=319, MSGTYPE=MSGWARNING, ANMODE=ANINFO_BLIND_2,
     .              I1=PROP_ID,
     .              C1=TITR)
      ENDIF
      IF (FAILEXP == ZERO) FAILEXP = ONE
      IF (IINT /= 1 .AND. IINT /= 2) IINT = 1  ! by default - uniform distribution (integration)
C                                    IINT = 2  ! Gauss distribution (integration)
      IF (ASHEAR == ZERO) ASHEAR = FIVE_OVER_6
c--------------------------------------------
      IF (ISHELL == 4 .AND. ISH3N==0 .AND. ISH3N_D == 1) THEN
        CALL ANCMSG(MSGID=680, MSGTYPE=MSGWARNING, ANMODE=ANINFO_BLIND_1,
     .               I1=PROP_ID, C1=TITR)
      ENDIF
      IF (ISHELL==22 .OR. ISHELL==23) THEN
        CALL ANCMSG(MSGID=539, MSGTYPE=MSGWARNING, ANMODE=ANINFO_BLIND_1,
     .              I1=PROP_ID, C1=TITR)
        ISHELL = 24
      ENDIF         
c
      IF (ISHELL == 24) THEN
        IF (CVIS==ZERO) CVIS = ONE
        IF (DN == ZERO) DN = ZEP015
      ENDIF
c
      IF (ISHELL == 3) THEN
        IF (HM == ZERO) HM = EM01
        IF (HF == ZERO) HF = EM01
        IF (HR == ZERO) HR = EM02
      ELSE
        IF (HM == ZERO) HM = EM02
        IF (HF == ZERO) HF = EM02
        IF (HR == ZERO) HR = EM02
      ENDIF
      IF (ISHELL > 11 .AND. ISHELL < 29) THEN
        HM = DN
        DN = CVIS
      ENDIF
c
      NORM = SQRT(VX*VX+VY*VY+VZ*VZ)
      IF (NORM < EM10) THEN
        VX=ONE
        VY=ZERO
        VZ=ZERO
        IF (IRP==23) THEN
          CALL ANCMSG(MSGID=1922,
     .              MSGTYPE=MSGERROR,
     .              ANMODE=ANINFO,
     .              C1='PROPERTY',
     .              I1=PROP_ID,
     .              C2='PROPERTY',
     .              C3=TITR,
     .              I2=IRP)
        END IF
      ELSE
        VX=VX/NORM
        VY=VY/NORM
        VZ=VZ/NORM
      ENDIF
c------------------------------------------------------------------------------
c     Apply submodel offsets units submodel transform to V (VX,VY,VZ) if needed
c
      IF (SUB_ID > 0) CALL SUBROTVECT(VX,VY,VZ,RTRANS,SUB_ID,LSUBMODEL)
c
c------------------------------------------------------------------------------
c     Check skew ID
      ISK = 0
      IF (IDSK /= 0) THEN
        DO I=0,NUMSKW+MIN(1,NSPCOND)*NUMSPH+NSUBMOD
          IF (IDSK == ISKN(4,I+1)) THEN
            ISK = I+1
            EXIT
          ENDIF
        ENDDO
        IF (ISK == 0) THEN
          CALL ANCMSG(MSGID=184, MSGTYPE=MSGERROR, ANMODE=ANINFO,
     .              C1='PROPERTY',
     .              I1=PROP_ID,
     .              C2='PROPERTY',
     .              C3=TITR,
     .              I2=IDSK)
        ENDIF
      ENDIF
      IF (IRP==22.AND.ISK==0) THEN
        CALL ANCMSG(MSGID=1923,
     .              MSGTYPE=MSGERROR,
     .              ANMODE=ANINFO,
     .              C1='PROPERTY',
     .              I1=PROP_ID,
     .              C2='PROPERTY',
     .              C3=TITR,
     .              I2=IRP)
      END IF
c     check duplicated py IDs
      IPID0 =  STACK_INFO%PID(1)
      DO K=2,NPLY
        IF (STACK_INFO%PID(K) == IPID0) THEN
          CALL ANCMSG(MSGID=1584,MSGTYPE=MSGERROR,ANMODE=ANINFO_BLIND_2,
     .         I1=IDSUB,
     .         I2=IPID0)
        ENDIF      
      ENDDO 
c
      DO 250 K=1,NPLY
        IMID = STACK_INFO%MID_IP(K)
        DO J=1,NUMMAT
          IF (IPM(1,J) == IMID) THEN
            STACK_INFO%MID_IP(K) = J
            GO TO 250
          ENDIF
        ENDDO
        STACK_INFO%MID_IP(K) = 0
  250 CONTINUE
c
C     isub stack
      IF (NISUB > 0) THEN
        DO 300 K=1,NISUB
          IMID = STACK_INFO%ISUB (3*(K-1) + 3)
          DO J=1,NUMMAT
            IF (IPM(1,J) == IMID) THEN
              STACK_INFO%ISUB (3*(K-1) + 3) = J
              GO TO 300
            ENDIF
          ENDDO
          STACK_INFO%ISUB (3*(K-1) + 3) = 0
  300   CONTINUE
      ENDIF 
c--------------------------------------------
      IF (DM == ZERO) IGEO(31) = 1
      IGEO(1)  = PROP_ID
      IGEO(2)  = ISK
      IGEO(4)  = NPLY
      IGEO(5)  = ISMSTR
      IGEO(6)  = IORTH  ! IREP
      IGEO(10) = ISHELL
      IGEO(11) = IGTYP
      IGEO(18) = ISH3N
      IGEO(20) = ISROT
      IGEO(43) = NSUB   ! number of substack               
      IGEO(44) = NISUB  ! number of interface 
      IGEO(47) = IINT
      IGEO(48) = 0 
      IGEO(98) = IGMAT
      IGEO(99) = IPOS
      IGEO(14) = IRP
c
      GEO(3)  = ISMSTR
      GEO(6)  = NPLY    ! double stockage
      GEO(7)  = VX
      GEO(8)  = VY
      GEO(9)  = VZ
      GEO(11) = ISTRAIN
      GEO(12) = IGTYP
      GEO(13) = HM
      GEO(14) = HF
      GEO(15) = HR
      GEO(16) = DM
      GEO(17) = DN
      GEO(35) = ITHK
      GEO(37) = ISHEAR
      GEO(39) = IPLAST
      GEO(38) = ASHEAR
      GEO(42) = PTHK
      GEO(43) = FAILEXP
      GEO(199)= ZSHIFT
      GEO(212) = GEO(212) * PI / HUNDRED80
      IF (ISHELL==0) THEN
        GEO(171) = 0
      ELSEIF (ISHELL == 1) THEN
        GEO(171)=1
      ELSEIF (ISHELL == 2) THEN
        GEO(171)=0
      ELSEIF (ISHELL >= 3 .AND. ISHELL < 100 .AND. ISHELL /= 4) THEN
        GEO(171)=ISHELL-1
      ENDIF      
c--------------------------------------------
c     OUTPUT 
c--------------------------------------------
      IF (IS_ENCRYPTED) THEN
        WRITE(IOUT, 1000)
      ELSE
        WRITE(IOUT,1200) PROP_ID
        IF (ISK == 0) THEN
          IF (ISHELL > 11 .AND. ISHELL < 29) THEN
            WRITE(IOUT,2100)ISTRAIN,ISMSTR,ISHELL,ISH3N,ISROT,
     .               GEO(16),GEO(13),GEO(38),PTHK,FAILEXP,ISHEAR,ITHK,
     .               IPLAST,IORTH,GEO(7),GEO(8),GEO(9),IGEO(47),IGEO(14)
          ELSE 
            WRITE(IOUT,2200)ISTRAIN,ISMSTR,ISHELL,ISH3N,
     .               HM,HF,HR,DM,ASHEAR,
     .               PTHK,FAILEXP,ISHEAR,ITHK,IPLAST,IORTH,
     .               VX,VY,VZ,IINT,IGEO(14)
          ENDIF
        ELSE
          IF (ISHELL > 11 .AND. ISHELL < 29) THEN 
            WRITE(IOUT,2300)ISTRAIN,ISMSTR,ISHELL,ISH3N,ISROT,
     .               GEO(16),GEO(13),GEO(38),PTHK,FAILEXP,ISHEAR,ITHK,
     .               IPLAST,IORTH,IDSK,IGEO(47),IGEO(14)
          ELSE
            WRITE(IOUT,2400)ISTRAIN,ISMSTR,ISHELL,ISH3N,HM,HF,HR,DM,ASHEAR,
     .            PTHK,FAILEXP,ISHEAR,ITHK,IPLAST,IORTH,IDSK,IINT,IGEO(14)
          ENDIF
        ENDIF
c---
        IF (NSUB > 0) THEN
          KK = 0
          DO IS = 1,NSUB
            IDSUB   = STACK_INFO%SUB(2*(IS - 1) + 1) 
            NPT_SUB = STACK_INFO%SUB(2*(IS - 1) + 2) 
            WRITE(IOUT,3000) IS
            DO K=1,NPT_SUB
              M1 =  KK + K 
              WRITE(IOUT,2800)K,STACK_INFO%PID(M1),STACK_INFO%ANG(M1),!STACK_INFO%POS(M1),
     .                        STACK_INFO%THKLY(M1),STACK_INFO%WEIGHT(M1)
              STACK_INFO%ANG(M1)=STACK_INFO%ANG(M1)*PI/HUNDRED80
            ENDDO
            KK = KK + NPT_SUB
          ENDDO
c
          DO K=1,NISUB
            IPID1 = STACK_INFO%ISUB(3*(K - 1) + 1)
            IPID2 = STACK_INFO%ISUB(3*(K - 1) + 2)
            WRITE(IOUT,3100) K 
            WRITE(IOUT,3300) IPID1,IPID2
          ENDDO
c
        ELSE   ! NSUB = 0
c
          DO I=1,NPLY
            WRITE(IOUT,2800)I,STACK_INFO%PID(I),STACK_INFO%ANG(I),
     .                      STACK_INFO%THKLY(I),STACK_INFO%WEIGHT(I)
            STACK_INFO%ANG(I) = STACK_INFO%ANG(I)*PI/HUNDRED80
          ENDDO
        END IF  ! NSUB      
c
      END IF
c-----------------------------------------------------------------------
 1000 FORMAT(
     &    5X,' COMPOSITE STACK SHELL PROPERTY SET  '/,
     &    5X,' CONFIDENTIAL DATA'//)
 1200 FORMAT(
     & 5X,'COMPOSITE STACK SHELL PROPERTY TYPE 51  '/,
     & 5X,'WITH VARIABLE THICKNESSES AND MATERIALS '/,
     & 5X,'AND VARIABLE NUMBER OF INTEGRATION POINTS THROUGH EACH LAYER'/,
     & 5X,'PROPERTY SET NUMBER . . . . . . . . . .=',I10/)   
 2100 FORMAT(
     & 5X,'POST PROCESSING STRAIN FLAG . . . . . .=',I10/,
     & 5X,'SMALL STRAIN FLAG . . . . . . . . . . .=',I10/,
     & 5X,'SHELL FORMULATION FLAG. . . . . . . . .=',I10/,
     & 5X,'3NODE SHELL FORMULATION FLAG. . . . . .=',I10/,
     & 5X,'DRILLING D.O.F. FLAG  . . . . . . . . .=',I10/,
     & 5X,'SHELL MEMBRANE DAMPING. . . . . . . . .=',1PG20.13/,
     & 5X,'SHELL NUMERICAL DAMPING . . . . . . . .=',1PG20.13/,
     & 5X,'SHEAR AREA REDUCTION FACTOR . . . . . .=',1PG20.13/,
     & 5X,'ELEMENT DELETION PARAMETER. . . . . . .=',1PG20.13/,
     & 5X,' > 0.0 : FRACTION OF FAILED THICKNESS   ',/,
     & 5X,' < 0.0 : FRACTION OF FAILED LAYERS/PLYS ',/,
     & 5X,'COMPOSITE FAILURE EXPONENT. . . . . . .=',1PG20.13/,
     & 5X,'SHEAR FORMULATION FLAG. . . . . . . . .=',I10/,
     & 5X,'THICKNESS VARIATION FLAG. . . . . . . .=',I10/,
     & 5X,'PLASTICITY FORMULATION FLAG . . . . . .=',I10/,
     & 5X,'LOCAL ORTOTHROPY SYSTEM FLAG. . . . . .=',I10/,
     & 5X,'X COMPONENT OF DIR 1 OF ORTHOTROPY. . .=',1PG20.13/,
     & 5X,'Y COMPONENT OF DIR 1 OF ORTHOTROPY. . .=',1PG20.13/,
     & 5X,'Z COMPONENT OF DIR 1 OF ORTHOTROPY. . .=',1PG20.13/,
     & 5X,'INTEGRATION FORMULATION FLAG. . . . . .=',I10/,
     & 5X,'REFERENCE DIRECTION FLAG IN SHELL PLANE=',I10/)
 2200 FORMAT(
     & 5X,'POST PROCESSING STRAIN FLAG . . . . . .=',I10/,
     & 5X,'SMALL STRAIN FLAG . . . . . . . . . . .=',I10/,
     & 5X,'SHELL FORMULATION FLAG. . . . . . . . .=',I10/,
     & 5X,'3NODE SHELL FORMULATION FLAG. . . . . .=',I10/,
     & 5X,'SHELL HOURGLASS MEMBRANE DAMPING. . . .=',1PG20.13/,
     & 5X,'SHELL HOURGLASS FLEXURAL DAMPING. . . .=',1PG20.13/,
     & 5X,'SHELL HOURGLASS ROTATIONAL DAMPING. . .=',1PG20.13/,
     & 5X,'SHELL MEMBRANE DAMPING. . . . . . . . .=',1PG20.13/,
     & 5X,'SHEAR AREA REDUCTION FACTOR . . . . . .=',1PG20.13/,
     & 5X,'ELEMENT DELETION PARAMETER. . . . . . .=',1PG20.13/,
     & 5X,' > 0.0 : FRACTION OF FAILED THICKNESS   ',/,
     & 5X,' < 0.0 : FRACTION OF FAILED LAYERS/PLYS ',/,
     & 5X,'COMPOSITE FAILURE EXPONENT. . . . . . .=',1PG20.13/,
     & 5X,'SHEAR FORMULATION FLAG. . . . . . . . .=',I10/,
     & 5X,'THICKNESS VARIATION FLAG. . . . . . . .=',I10/,
     & 5X,'PLASTICITY FORMULATION FLAG . . . . . .=',I10/,
     & 5X,'LOCAL ORTOTHROPY SYSTEM FLAG. . . . . .=',I10/,
     & 5X,'X COMPONENT OF DIR 1 OF ORTHOTROPY. . .=',1PG20.13/,
     & 5X,'Y COMPONENT OF DIR 1 OF ORTHOTROPY. . .=',1PG20.13/,
     & 5X,'Z COMPONENT OF DIR 1 OF ORTHOTROPY. . .=',1PG20.13/,
     & 5X,'INTEGRATION FORMULATION FLAG. . . . . .=',I10/,
     & 5X,'REFERENCE DIRECTION FLAG IN SHELL PLANE=',I10/)
 2300 FORMAT(
     & 5X,'POST PROCESSING STRAIN FLAG . . . . . .=',I10/,
     & 5X,'SMALL STRAIN FLAG . . . . . . . . . . .=',I10/,
     & 5X,'SHELL FORMULATION FLAG. . . . . . . . .=',I10/,
     & 5X,'3NODE SHELL FORMULATION FLAG. . . . . .=',I10/,
     & 5X,'DRILLING D.O.F. FLAG  . . . . . . . . .=',I10/,
     & 5X,'SHELL MEMBRANE DAMPING. . . . . . . . .=',1PG20.13/,
     & 5X,'SHELL NUMERICAL DAMPING . . . . . . . .=',1PG20.13/,
     & 5X,'SHEAR AREA REDUCTION FACTOR . . . . . .=',1PG20.13/,
     & 5X,'ELEMENT DELETION PARAMETER. . . . . . .=',1PG20.13/,
     & 5X,' > 0.0 : FRACTION OF FAILED THICKNESS   ',/,
     & 5X,' < 0.0 : FRACTION OF FAILED LAYERS/PLYS ',/,
     & 5X,'COMPOSITE FAILURE EXPONENT. . . . . . .=',1PG20.13/,
     & 5X,'SHEAR FORMULATION FLAG. . . . . . . . .=',I10/,
     & 5X,'THICKNESS VARIATION FLAG. . . . . . . .=',I10/,
     & 5X,'PLASTICITY FORMULATION FLAG . . . . . .=',I10/,
     & 5X,'LOCAL ORTOTHROPY SYSTEM FLAG. . . . . .=',I10/,
     & 5X,'SKEW OF THE FIRST ORTHOTROPY DIRECTION.=',I10/,
     & 5X,'INTEGRATION FORMULATION FLAG. . . . . .=',I10/,
     & 5X,'REFERENCE DIRECTION FLAG IN SHELL PLANE=',I10/)
 2400 FORMAT(
     & 5X,'POST PROCESSING STRAIN FLAG . . . . . .=',I10/,
     & 5X,'SMALL STRAIN FLAG . . . . . . . . . . .=',I10/,
     & 5X,'SHELL FORMULATION FLAG. . . . . . . . .=',I10/,
     & 5X,'3NODE SHELL FORMULATION FLAG. . . . . .=',I10/,
     & 5X,'SHELL HOURGLASS MEMBRANE DAMPING. . . .=',1PG20.13/,
     & 5X,'SHELL HOURGLASS FLEXURAL DAMPING. . . .=',1PG20.13/,
     & 5X,'SHELL HOURGLASS ROTATIONAL DAMPING. . .=',1PG20.13/,
     & 5X,'SHELL MEMBRANE DAMPING. . . . . . . . .=',1PG20.13/,
     & 5X,'ELEMENT DELETION PARAMETER. . . . . . .=',1PG20.13/,
     & 5X,' > 0.0 : FRACTION OF FAILED THICKNESS   ',/,
     & 5X,' < 0.0 : FRACTION OF FAILED LAYERS/PLYS ',/,
     & 5X,'COMPOSITE FAILURE EXPONENT. . . . . . .=',1PG20.13/,
     & 5X,'SHEAR AREA REDUCTION FACTOR . . . . . .=',1PG20.13/,
     & 5X,'SHEAR FORMULATION FLAG. . . . . . . . .=',I10/,
     & 5X,'THICKNESS VARIATION FLAG. . . . . . . .=',I10/,
     & 5X,'PLASTICITY FORMULATION FLAG . . . . . .=',I10/,
     & 5X,'LOCAL ORTOTHROPY SYSTEM FLAG. . . . . .=',I10/,
     & 5X,'SKEW OF THE FIRST ORTHOTROPY DIRECTION.=',I10/,
     & 5X,'INTEGRATION FORMULATION FLAG. . . . . .=',I10/,
     & 5X,'REFERENCE DIRECTION FLAG IN SHELL PLANE=',I10/)
 2800 FORMAT(
     & 5X,'    PLY ',I3/,
     & 5X,'      PLY PID NUMBER  . . . . . . . . .=',I10/
     & 5X,'      ANGLE (DIR 1,PROJ(DIR 1 / SHELL).=',1PG20.13/,
     & 5X,'      PLY FAILURE PARAMETER . . . . . .=',1PG20.13/,
     & 5X,'        > 0.0 : FRACTION OF FAILED THICKNESS   ',/,
     & 5X,'        < 0.0 : FRACTION OF FAILED INTG. POINTS',/,
     & 5X,'      WEIGHT FACTOR FOR PLY FAILURE . .=',1PG20.13/)
 3000 FORMAT(
     & 5X,' COMPOSITE SUBSTACK SHELL NUMBER . . . =',I10/ ) 
 3100 FORMAT(
     & 5X,'  INTERFACE NUMBER BETWEEN-SUBSTACK . .:',I10/ )
 3300 FORMAT(
     & 5X,'      INTER-PLY_1 PID NUMBER  . . . . . =',I10/, 
     & 5X,'      INTER-PLY_2 PID NUMBER . . . . . .=',I10/)
c-----------------------------------------------------------------------
      RETURN
      END SUBROUTINE HM_READ_PROP51
