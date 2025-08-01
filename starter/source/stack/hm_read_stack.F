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
!||    hm_read_stack                 ../starter/source/stack/hm_read_stack.F
!||--- called by ------------------------------------------------------
!||    lecstack_ply                  ../starter/source/properties/composite_options/stack/lecstack_ply.F
!||--- calls      -----------------------------------------------------
!||    ancmsg                        ../starter/source/output/message/message.F
!||    hm_get_float_array_2indexes   ../starter/source/devtools/hm_reader/hm_get_float_array_2indexes.F
!||    hm_get_float_array_index      ../starter/source/devtools/hm_reader/hm_get_float_array_index.F
!||    hm_get_floatv                 ../starter/source/devtools/hm_reader/hm_get_floatv.F
!||    hm_get_int_array_2indexes     ../starter/source/devtools/hm_reader/hm_get_int_array_2indexes.F
!||    hm_get_int_array_index        ../starter/source/devtools/hm_reader/hm_get_int_array_index.F
!||    hm_get_intv                   ../starter/source/devtools/hm_reader/hm_get_intv.F
!||    hm_option_is_encrypted        ../starter/source/devtools/hm_reader/hm_option_is_encrypted.F
!||    subrotvect                    ../starter/source/model/submodel/subrot.F
!||--- uses       -----------------------------------------------------
!||    defaults_mod                  ../starter/source/modules/defaults_mod.F90
!||    elbuftag_mod                  ../starter/share/modules1/elbuftag_mod.F
!||    message_mod                   ../starter/share/message_module/message_mod.F
!||    stack_mod                     ../starter/share/modules1/stack_mod.F
!||    submodel_mod                  ../starter/share/modules1/submodel_mod.F
!||====================================================================
      SUBROUTINE HM_READ_STACK(
     .           GEO_STACK ,IGEO_STACK ,PM        ,IPM       ,ISKN     ,  
     .           PROP_ID   ,RTRANS     ,SUB_ID    ,STACK_INFO,
     .           TITR      ,UNITAB     ,LSUBMODEL ,DEFAULTS_SHELL)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE ELBUFTAG_MOD            
      USE UNITAB_MOD
      USE MESSAGE_MOD
      USE SUBMODEL_MOD
      USE STACK_MOD
      USE NAMES_AND_TITLES_MOD , ONLY : NCHARTITLE
      USE DEFAULTS_MOD
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
#include      "sphcom.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER, INTENT(IN) :: PROP_ID,SUB_ID
      INTEGER, INTENT(INOUT) :: IGEO_STACK(NPROPGI)
      INTEGER, INTENT(IN) :: IPM(NPROPMI,NUMMAT)
      INTEGER :: ISKN(LISKN,*)
      my_real, INTENT(INOUT) :: GEO_STACK(NPROPG)
      my_real, INTENT(IN) ::  PM(NPROPM,NUMMAT),RTRANS(NTRANSF,*)
      CHARACTER(LEN = NCHARTITLE) :: TITR
      TYPE (UNIT_TYPE_), INTENT(IN) :: UNITAB
      TYPE (SUBMODEL_DATA), DIMENSION(NSUBMOD), INTENT(IN) :: LSUBMODEL
      TYPE(STACK_INFO_) , TARGET :: STACK_INFO
      TYPE(SHELL_DEFAULTS_), INTENT(IN)  :: DEFAULTS_SHELL
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER :: I,J,K,KK,M1,ISHELL,ISH3N,ISMSTR,ISROT,ISTRAIN,IINT,ITHK,
     .   IORTH,IPOS,IGMAT,ISHEAR,IPLAST,NPLY,NP,NSUB,NISUB,IGTYP,IMID,
     .   ISK,IDSK,PLY_ID,IPID0,IDSUB,INTER,IS,LAMIN,IPID1,IPID2,NPT_SUB,IRP,
     .   NPLY_MAX
      INTEGER    IHBE_D,IPLA_D,ISTR_D,ITHK_D,ISHEA_D,ISST_D,
     .           ISH3N_D, ISTRA_D,NPTS_D,IDRIL_D
      my_real :: PTHK,ZSHIFT,HM,HF,HR,DM,DN,ASHEAR,VX,VY,VZ,FAILEXP,CVIS,
     .           NORM,ANG,POS,PTHKLY,WEIGHT
      LOGICAL :: IS_AVAILABLE, IS_ENCRYPTED, LFOUND
C=======================================================================
      IS_AVAILABLE = .FALSE.
      IS_ENCRYPTED = .FALSE.
c
      IGTYP   = 52
      IGMAT   = 1
      ISTRAIN = 1
      CVIS    = ZERO
      IRP = 0
!
      IHBE_D =  DEFAULTS_SHELL%ishell
      ISH3N_D=  DEFAULTS_SHELL%ish3n
      ISST_D =  DEFAULTS_SHELL%ismstr
      IPLA_D =  DEFAULTS_SHELL%iplas
      ITHK_D =  DEFAULTS_SHELL%ithick
      IDRIL_D=  DEFAULTS_SHELL%idrill
       ISHEA_D = 0
       NPTS_D = 0
       ISTRA_D = 1
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
      CALL HM_GET_INTV('LAM_Ishell', ISHELL, IS_AVAILABLE, LSUBMODEL)
      CALL HM_GET_INTV('LAM_Ismstr', ISMSTR, IS_AVAILABLE, LSUBMODEL)
      CALL HM_GET_INTV('LAM_ISH3N' , ISH3N , IS_AVAILABLE, LSUBMODEL)
      CALL HM_GET_INTV('LAM_Idrill', ISROT , IS_AVAILABLE, LSUBMODEL)
      CALL HM_GET_FLOATV('P_Thick_Fail', PTHK, IS_AVAILABLE, LSUBMODEL, UNITAB)  
      CALL HM_GET_FLOATV('LAM_Z0'          , ZSHIFT, IS_AVAILABLE, LSUBMODEL, UNITAB)
      
card2      
      CALL HM_GET_FLOATV('LAM_Hm', HM, IS_AVAILABLE, LSUBMODEL, UNITAB)  
      CALL HM_GET_FLOATV('LAM_Hf', HF, IS_AVAILABLE, LSUBMODEL, UNITAB)  
      CALL HM_GET_FLOATV('LAM_Hr', HR, IS_AVAILABLE, LSUBMODEL, UNITAB)  
      CALL HM_GET_FLOATV('LAM_Dm', DM, IS_AVAILABLE, LSUBMODEL, UNITAB)  
      CALL HM_GET_FLOATV('LAM_Dn', DN, IS_AVAILABLE, LSUBMODEL, UNITAB)  
card3      
c      CALL HM_GET_INTV  ('ISTRAIN'   ,ISTRAIN ,IS_AVAILABLE, LSUBMODEL) ! always = 1
      CALL HM_GET_FLOATV('LAM_Ashear',ASHEAR  ,IS_AVAILABLE, LSUBMODEL, UNITAB)  
      CALL HM_GET_INTV  ('LAM_Iint'      ,IINT    ,IS_AVAILABLE, LSUBMODEL)
      CALL HM_GET_INTV  ('LAM_Ithick'    ,ITHK    ,IS_AVAILABLE, LSUBMODEL)
!!      CALL HM_GET_FLOATV('Fexp'      ,FAILEXP ,IS_AVAILABLE, LSUBMODEL, UNITAB) 
card4
      CALL HM_GET_FLOATV('LAM_Vx'       ,VX      ,IS_AVAILABLE, LSUBMODEL, UNITAB)  
      CALL HM_GET_FLOATV('LAM_Vy'       ,VY      ,IS_AVAILABLE, LSUBMODEL, UNITAB)  
      CALL HM_GET_FLOATV('LAM_Vz'       ,VZ      ,IS_AVAILABLE, LSUBMODEL, UNITAB)  
      CALL HM_GET_INTV('LAM_SKEW_CSID'   ,IDSK    ,IS_AVAILABLE, LSUBMODEL)
      CALL HM_GET_INTV('LAM_Iorth'       ,IORTH   ,IS_AVAILABLE, LSUBMODEL)
      CALL HM_GET_INTV('LAM_Ipos'        ,IPOS    ,IS_AVAILABLE, LSUBMODEL)
      CALL HM_GET_INTV('LAM_Ip',IRP,IS_AVAILABLE,LSUBMODEL)
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
          CALL HM_GET_INT_ARRAY_INDEX('Nsub',IDSUB,IS,IS_AVAILABLE,LSUBMODEL)
          STACK_INFO%SUB(2*(IS-1) + 1) = IDSUB                      
          STACK_INFO%SUB(2*(IS-1) + 2) = NPT_SUB  
c
          DO I = 1,NPT_SUB
            CALL HM_GET_INT_ARRAY_2INDEXES('plyidlist',PLY_ID,IS,I,IS_AVAILABLE,LSUBMODEL)
            CALL HM_GET_FLOAT_ARRAY_2INDEXES('LAM_Stack_phi',ANG,IS,I,IS_AVAILABLE,LSUBMODEL,UNITAB)
            CALL HM_GET_FLOAT_ARRAY_2INDEXES('LAM_Stack_Zi',POS,IS,I,IS_AVAILABLE,LSUBMODEL,UNITAB)
            CALL HM_GET_FLOAT_ARRAY_2INDEXES('P_thick_fail_lam',PTHKLY,IS,I,IS_AVAILABLE,LSUBMODEL,UNITAB)
            CALL HM_GET_FLOAT_ARRAY_2INDEXES('F_weight_i',WEIGHT,IS,I,IS_AVAILABLE,LSUBMODEL,UNITAB)
            IF(PLY_ID > 0) THEN
                NPLY = NPLY + 1
               IF (PTHKLY == ZERO) PTHKLY = ONE-EM06
               PTHKLY = MIN(PTHKLY, ONE)
               PTHKLY = MAX(PTHKLY,-ONE)
               IF (WEIGHT == ZERO) WEIGHT = ONE
               STACK_INFO%PID(NPLY) = PLY_ID
               STACK_INFO%ANG(NPLY) = ANG
               STACK_INFO%POS(NPLY) = POS
               STACK_INFO%THKLY(NPLY)  = PTHKLY
               STACK_INFO%WEIGHT(NPLY) = WEIGHT
           ENDIF    
          END DO
        END DO
c
        IF (NISUB > 0) THEN
          DO I=1,NISUB
            CALL HM_GET_INT_ARRAY_INDEX('interfacepairplyids',IPID1 ,  2*(I - 1) + 1  ,IS_AVAILABLE,LSUBMODEL)
            CALL HM_GET_INT_ARRAY_INDEX('interfacepairplyids',IPID2 ,  2*I  ,IS_AVAILABLE,LSUBMODEL)
            STACK_INFO%ISUB(3*(I-1) + 1) = IPID1                    
            STACK_INFO%ISUB(3*(I-1) + 2) = IPID2    
          END DO
        END IF  
      ELSE  ! property defined by a list of plies
        CALL HM_GET_INTV('plyidlistmax' ,NPLY_MAX ,IS_AVAILABLE ,LSUBMODEL)
        NPLY =  0
        DO I=1,NPLY_MAX
          CALL HM_GET_INT_ARRAY_INDEX  ('plyidlist' ,PLY_ID,I,IS_AVAILABLE,LSUBMODEL)
          CALL HM_GET_FLOAT_ARRAY_INDEX('LAM_Stack_phi',ANG,I,IS_AVAILABLE,LSUBMODEL,UNITAB)   
          CALL HM_GET_FLOAT_ARRAY_INDEX('LAM_Stack_Zi' ,POS,I,IS_AVAILABLE,LSUBMODEL,UNITAB)   
          CALL HM_GET_FLOAT_ARRAY_INDEX('P_thick_fail_lam' ,PTHKLY,I,IS_AVAILABLE,LSUBMODEL,UNITAB)   
          CALL HM_GET_FLOAT_ARRAY_INDEX('F_weight_i' ,WEIGHT,I,IS_AVAILABLE,LSUBMODEL,UNITAB)   
c       
          IF(PLY_ID > 0) THEN
             NPLY = NPLY + 1
             IF (PTHKLY == ZERO) PTHKLY = ONE-EM06
             PTHKLY = MIN(PTHKLY, ONE)
             PTHKLY = MAX(PTHKLY,-ONE)
             IF (WEIGHT == ZERO) WEIGHT = ONE
             STACK_INFO%PID(NPLY) = PLY_ID
             STACK_INFO%ANG(NPLY) = ANG
             STACK_INFO%POS(NPLY) = POS
             STACK_INFO%THKLY(NPLY)  = PTHKLY
             STACK_INFO%WEIGHT(NPLY) = WEIGHT
          ENDIF
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
     .              C1='STACK',
     .              I1=PROP_ID,
     .              C2='STACK',
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
      IF (DM == ZERO) IGEO_STACK(31) = 1
      IGEO_STACK(1)  = PROP_ID
      IGEO_STACK(2)  = ISK
      IGEO_STACK(4)  = NPLY
      IGEO_STACK(5)  = ISMSTR
      IGEO_STACK(6)  = IORTH  ! IREP
      IGEO_STACK(10) = ISHELL
      IGEO_STACK(11) = IGTYP
      IGEO_STACK(18) = ISH3N
      IGEO_STACK(20) = ISROT
      IGEO_STACK(43) = NSUB   ! number of substack               
      IGEO_STACK(44) = NISUB  ! number of interface 
      IGEO_STACK(47) = IINT
      IGEO_STACK(48) = 0 
      IGEO_STACK(98) = IGMAT
      IGEO_STACK(99) = IPOS
      IGEO_STACK(14) = IRP
c
      GEO_STACK(3)  = ISMSTR
      GEO_STACK(6)  = NPLY    ! double stockage
      GEO_STACK(7)  = VX
      GEO_STACK(8)  = VY
      GEO_STACK(9)  = VZ
      GEO_STACK(11) = ISTRAIN
      GEO_STACK(12) = IGTYP
      GEO_STACK(13) = HM
      GEO_STACK(14) = HF
      GEO_STACK(15) = HR
      GEO_STACK(16) = DM
      GEO_STACK(17) = DN
      GEO_STACK(35) = ITHK
      GEO_STACK(37) = ISHEAR
      GEO_STACK(39) = IPLAST
      GEO_STACK(38) = ASHEAR
      GEO_STACK(42) = PTHK
      GEO_STACK(43) = ONE
      GEO_STACK(199)= ZSHIFT
      GEO_STACK(212) = GEO_STACK(212) * PI / HUNDRED80
      IF (ISHELL==0) THEN
        GEO_STACK(171) = 0
      ELSEIF (ISHELL == 1) THEN
        GEO_STACK(171)=1
      ELSEIF (ISHELL == 2) THEN
        GEO_STACK(171)=0
      ELSEIF (ISHELL >= 3 .AND. ISHELL < 100 .AND. ISHELL /= 4) THEN
        GEO_STACK(171)=ISHELL-1
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
     .               GEO_STACK(16),GEO_STACK(13),GEO_STACK(38),PTHK,ISHEAR,ITHK,
     .               IPLAST,IORTH,GEO_STACK(7),GEO_STACK(8),GEO_STACK(9),IGEO_STACK(47),IGEO_STACK(14)
          ELSE 
            WRITE(IOUT,2200)ISTRAIN,ISMSTR,ISHELL,ISH3N,ISROT,
     .               HM,HF,HR,DM,ASHEAR,
     .               PTHK,ISHEAR,ITHK,IPLAST,IORTH,
     .               VX,VY,VZ,IINT,IGEO_STACK(14)
          ENDIF
        ELSE
          IF (ISHELL > 11 .AND. ISHELL < 29) THEN 
            WRITE(IOUT,2300)ISTRAIN,ISMSTR,ISHELL,ISH3N,ISROT,
     .               GEO_STACK(16),GEO_STACK(13),GEO_STACK(38),PTHK,ISHEAR,ITHK,
     .               IPLAST,IORTH,IDSK,IGEO_STACK(47),IGEO_STACK(14)
          ELSE
            WRITE(IOUT,2400)ISTRAIN,ISMSTR,ISHELL,ISH3N,HM,HF,HR,DM,
     .            ASHEAR,PTHK,ISHEAR,ITHK,IPLAST,IORTH,IDSK,IINT,IGEO_STACK(14)
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
 1200  FORMAT (
     & 5X,'COMPOSITE STACK SHELL OBJECT SET ',
     &  'WITH VARIABLE THICKNESSES AND MATERIALS ',
     &  'AND VARIABLE NUMBER OF INTEGRATION POINTS THROUGH EACH LAYER'/,
     & 5X,'STACK SET NUMBER . . . . . . . . . .=',I10/)     
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
     & 5X,'DRILLING D.O.F. FLAG  . . . . . . . . .=',I10/,
     & 5X,'SHELL HOURGLASS MEMBRANE DAMPING. . . .=',1PG20.13/,
     & 5X,'SHELL HOURGLASS FLEXURAL DAMPING. . . .=',1PG20.13/,
     & 5X,'SHELL HOURGLASS ROTATIONAL DAMPING. . .=',1PG20.13/,
     & 5X,'SHELL MEMBRANE DAMPING. . . . . . . . .=',1PG20.13/,
     & 5X,'SHEAR AREA REDUCTION FACTOR . . . . . .=',1PG20.13/,
     & 5X,'ELEMENT DELETION PARAMETER. . . . . . .=',1PG20.13/,
     & 5X,' > 0.0 : FRACTION OF FAILED THICKNESS   ',/,
     & 5X,' < 0.0 : FRACTION OF FAILED LAYERS/PLYS ',/,
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
      END SUBROUTINE HM_READ_STACK
