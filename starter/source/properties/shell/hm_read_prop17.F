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
Chd|  HM_READ_PROP17                source/properties/shell/hm_read_prop17.F
Chd|-- called by -----------
Chd|        HM_READ_PROPERTIES            source/properties/hm_read_properties.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        FRETITL2                      source/starter/freform.F      
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
      SUBROUTINE HM_READ_PROP17(GEO       ,IGEO     ,PM       ,IPM      ,ISKN     ,
     .                   UNITAB    ,RTRANS   ,LSUBMODEL,SUB_ID   ,IDTITL   ,
     .                   PROP_ID   ,PROP_TAG , STACK_INFO)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE UNITAB_MOD
      USE ELBUFTAG_MOD  
      USE SUBMODEL_MOD
      USE MESSAGE_MOD
      USE STACK_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "units_c.inc"
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "param_c.inc"
#include      "scr16_c.inc"
#include      "scr17_c.inc"
#include      "scr21_c.inc"
#include      "submod_c.inc"
#include      "sphcom.inc"
#include      "tablen_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      TYPE (UNIT_TYPE_),INTENT(IN) ::UNITAB 
      INTEGER :: 
     .   IGEO(NPROPGI),IPM(NPROPMI,*),SUB_ID,ISKN(LISKN,*),
     .   PROP_ID
      my_real
     .   GEO(NPROPG), PM(NPROPM,*),RTRANS(NTRANSF,*)
      TYPE(SUBMODEL_DATA) LSUBMODEL(*)
      CHARACTER IDTITL*nchartitle
      TYPE(PROP_TAG_) , DIMENSION(0:MAXPROP) :: PROP_TAG
      TYPE(STACK_INFO_) , TARGET :: STACK_INFO
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      CHARACTER LAW_ID*4
      INTEGER :: IGMAT, IHBE , ISMSTR, ISROT, ISHXFEM  ,ISTRAIN, ITHK, IPLAST, IDSK,
     .           IORTH, IPOS , LAMIN , NSUB , NISUB , NN, NPLY, IDSUB , I, PLY_ID,
     .           IMID_PI,INTER ,IPID1 , IPID2 , IREP, ISHEAR ,IHBEOUTP ,IPID0,
     .           ISK , J, ISS, N1, IMID, K, NC, KK, II, NPT_SUB, M1, IMAT,
     .           N,IPID,ISHELL,ISH3N,ID,ID1,IUN,NPLY_SUB,IGTYP,FAIL_SHELL,IS,IRP
      my_real :: 
     .   AN, VX, VY, VZ, THICKT, DT, TMIN, TMAX,PTHK,CVIS,PUN,ZSHIFT, THK ,
     .   ASHEAR, ANG, POS, HM, HF, HR, DN, DM,VISC_INT
      INTEGER, DIMENSION(:),ALLOCATABLE :: IDMAT_INTP,IDMAT_SUB
      
      CHARACTER*nchartitle,
     .   TITR,TITR1
      LOGICAL :: IS_AVAILABLE, IS_ENCRYPTED
C-----------------------------------------------
C   D a t a
C-----------------------------------------------
      DATA IUN/1/
      DATA PUN/0.1/
C======================================================================|
C----------------------------------------------------------------
C      COMPOSITE LAYERED SHELL
C      LAYERS WITH : -VARIABLE THICKNESS
C                    -VARIABLE MATERIAL  (BUT LAW 25 OR 27 ONLY)
C----------------------------------------------------------------
C======================================================================|
      IS_AVAILABLE = .FALSE.
      IS_ENCRYPTED = .FALSE.
c
      IGTYP   = 17
      IGMAT   = 1
      CVIS    = ONE  
      ISTRAIN  = 1
      IRP = 0
      IDSK = 0
C--------------------------------------------------
C EXTRACT DATA (IS OPTION CRYPTED)
C--------------------------------------------------
      CALL HM_OPTION_IS_ENCRYPTED(IS_ENCRYPTED)
C======================================================================|
      ! Card 1
      CALL HM_GET_INTV('Ishell',IHBE,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV('Ismstr',ISMSTR,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV('ISH3N',ISH3N,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV('Idrill',ISROT,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV('plyxfem',ISHXFEM,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_FLOATV('Z0',ZSHIFT,IS_AVAILABLE,LSUBMODEL,UNITAB)  
      CALL HM_GET_FLOATV('Vinterply',VISC_INT,IS_AVAILABLE,LSUBMODEL,UNITAB) ! global interply viscosity for plyxfem formulation
       ! Card 2
      
      CALL HM_GET_FLOATV('Hm',HM,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('Hf',HF,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('Hr',HR,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('Dm',DM,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('Dn',DN,IS_AVAILABLE,LSUBMODEL,UNITAB)
      ! card3      
!!      CALL HM_GET_INTV  ('ISTRAIN'   ,ISTRAIN   ,IS_AVAILABLE, LSUBMODEL) ! removed 
      CALL HM_GET_FLOATV('THICK'     ,THK       ,IS_AVAILABLE, LSUBMODEL, UNITAB)  
      CALL HM_GET_FLOATV('AREA_SHEAR',ASHEAR    ,IS_AVAILABLE, LSUBMODEL, UNITAB)  
      CALL HM_GET_INTV  ('ITHICK'    ,ITHK      ,IS_AVAILABLE, LSUBMODEL)
      CALL HM_GET_INTV  ('IPLAS'     ,IPLAST    ,IS_AVAILABLE, LSUBMODEL)
!!      CALL HM_GET_FLOATV('Fexp'      ,FAIL_SHELL ,IS_AVAILABLE, LSUBMODEL, UNITAB)   not existing 
      ! card4
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
      NSUB = 0
      NISUB = 0 
      IF (LAMIN  > 0) THEN
        CALL HM_GET_INTV('sublaminateidlistmax' ,NSUB,  IS_AVAILABLE, LSUBMODEL)
        CALL HM_GET_INTV('interfacepairsize'    ,NISUB,  IS_AVAILABLE, LSUBMODEL)
        NPLY = 0
        DO IS = 1,NSUB
          CALL HM_GET_INT_ARRAY_INDEX('plyidlistmax',NPLY_SUB,IS,IS_AVAILABLE,LSUBMODEL)
          CALL HM_GET_INT_ARRAY_INDEX('DUMMY',IDSUB,IS,IS_AVAILABLE,LSUBMODEL) ! 
          !!
          STACK_INFO%SUB(2*(IS - 1) + 1) = IDSUB 
          STACK_INFO%SUB(2*(IS - 1) + 2) = NPLY_SUB
          DO I = 1,NPLY_SUB
            CALL HM_GET_INT_ARRAY_2INDEXES  ('plyidlist',PLY_ID,IS,I,IS_AVAILABLE,LSUBMODEL)
            CALL HM_GET_FLOAT_ARRAY_2INDEXES('Prop_phi' ,ANG   ,IS,I,IS_AVAILABLE,LSUBMODEL,UNITAB)
            CALL HM_GET_FLOAT_ARRAY_2INDEXES('Prop_Zi'  ,POS   ,IS,I,IS_AVAILABLE,LSUBMODEL,UNITAB) 
            CALL HM_GET_INT_ARRAY_2INDEXES  ('Prop_mi' ,IMID_PI,IS,I,IS_AVAILABLE,LSUBMODEL) 
            ! stockage
                  STACK_INFO%PID(NPLY + I) = PLY_ID
                  STACK_INFO%ANG(NPLY + I) = ANG
                  STACK_INFO%POS(NPLY + I) = POS  
                  STACK_INFO%MID_IP(NPLY + I) = IMID_PI
          END DO
           NPLY = NPLY + NPLY_SUB
        END DO
c
        IF (NISUB > 0) THEN
          DO INTER = 1,NISUB
             CALL HM_GET_INT_ARRAY_2INDEXES  ('interfacepairplyids'    ,IPID1  ,1 ,INTER,IS_AVAILABLE,LSUBMODEL)
             CALL HM_GET_INT_ARRAY_2INDEXES  ('interfacepairplyids'    ,IPID2  ,2 ,INTER,IS_AVAILABLE,LSUBMODEL)
             CALL HM_GET_INT_ARRAY_INDEX  ('interfacepair_Prop_mi'  ,IMID_PI,INTER,IS_AVAILABLE,LSUBMODEL)
              !!
              STACK_INFO%ISUB(3*(INTER - 1) + 1) = IPID1
              STACK_INFO%ISUB(3*(INTER - 1) + 2) = IPID2
              STACK_INFO%ISUB(3*(INTER - 1) + 3) = IMID_PI
          END DO
        END IF  
      ELSE  ! property defined by a list of plies
        CALL HM_GET_INTV('plyidlistmax' ,NPLY ,IS_AVAILABLE ,LSUBMODEL)
        DO I=1,NPLY
          CALL HM_GET_INT_ARRAY_INDEX  ('plyidlist' ,PLY_ID,I,IS_AVAILABLE,LSUBMODEL)
          CALL HM_GET_FLOAT_ARRAY_INDEX('Prop_phi',ANG,I,IS_AVAILABLE,LSUBMODEL,UNITAB)   
          CALL HM_GET_FLOAT_ARRAY_INDEX('Prop_Zi' ,POS,I,IS_AVAILABLE,LSUBMODEL,UNITAB)  
          CALL HM_GET_INT_ARRAY_INDEX  ('Prop_mi' ,IMID_PI,I,IS_AVAILABLE,LSUBMODEL) !
          !!
          STACK_INFO%PID(I) = PLY_ID
          STACK_INFO%ANG(I) = ANG
          STACK_INFO%POS(I) = POS     
          STACK_INFO%MID_IP(I) = IMID_PI
        END DO
      END IF
      PTHK = ZERO
c
      IREP = IORTH
C      IPOS = 0
C      IDSK = 0
C----------------------
C Default value
C----------------------
C ISHEAR NEVER USED
        ISHEAR = 0
C CVIS
       CVIS = ZERO

C     Double stockage temporaire - supprimer GEO(12,I)=IGTYP apres tests
      IGEO( 1)= PROP_ID
C        
C recommended formulation Iplyxfem=2  (old formulation iplyxfem=1)         
         IF(ISHXFEM > 0) IPLYXFEM = 2
         IF(ISHXFEM > 0 .AND. ISHXFEM /= 2 ) THEN
            CALL ANCMSG(MSGID=1607,
     .                  MSGTYPE=MSGWARNING,
     .                  ANMODE=ANINFO_BLIND_2,
     .                  I1=PROP_ID,
     .                  C1=IDTITL)
         ENDIF
        ISHXFEM = MIN(1,ISHXFEM)
C        
        IF(IHBE /= 12 .AND. ISHXFEM >0) THEN
             ISHXFEM = 0
             IPLYXFEM = 0
            CALL ANCMSG(MSGID=726,
     .                  MSGTYPE=MSGWARNING,
     .                  ANMODE=ANINFO_BLIND_2,
     .                  I1=PROP_ID,
     .                  C1=IDTITL)
       ENDIF  
       IF(IHBE==0)IHBE=IHBE_D
         IHBEOUTP=IHBE
         IF (IHBE == 4 .AND. ISH3N==0 .AND. ISH3N_D == 1) THEN
           CALL ANCMSG(MSGID=680,
     .                 MSGTYPE=MSGWARNING,
     .                 ANMODE=ANINFO_BLIND_1,
     .                  I1=PROP_ID,
     .                  C1=IDTITL)
         ENDIF
         IF (IHBE==22.OR.IHBE==23) THEN
           CALL ANCMSG(MSGID=539,
     .                 MSGTYPE=MSGWARNING,
     .                 ANMODE=ANINFO_BLIND_1,
     .                 I1=PROP_ID,
     .                 C1=IDTITL)
           IHBE=24
         ENDIF         
         IF(ISH3N==0) ISH3N = ISH3N_D
         IGEO(18) = ISH3N
         IF (DM == ZERO) IGEO(31) = 1
         
         IF (IHBE==24) THEN
           IF (CVIS==ZERO) CVIS = ONE
           IF (DN == ZERO) DN = ZEP015
           IF (DM == ZERO) THEN
C-------------remove in cgrtails            
C             IF (IGTYP==1.OR.IGTYP==9) GEO(16,I)=ZEP015
           END IF
         ENDIF
C---
         IF(ISMSTR==0)ISMSTR=ISST_D
         IF(IHBE==3)THEN
           IF(HM == ZERO)HM = EM01
           IF(HF == ZERO)HF = EM01
           IF(HR == ZERO)HR = EM02
         ELSE
           IF(HM == ZERO)HM = EM02
           IF(HF == ZERO)HF = EM02
           IF(HR == ZERO)HR = EM02
         ENDIF
         
         IF(ASHEAR == ZERO) ASHEAR=FIVE_OVER_6
C
         IF (IHBE>11.AND.IHBE<29) THEN
C---------GEO(13,I) est utilise pour porte dn;GEO(17,I),CVIS se change ----
           HM = DN
           DN = CVIS
C          IGEO(20,I)=ISROT
         ENDIF
C
C        Double stockage temporaire - supprimer GEO(12,I)=IGTYP apres tests
         IGEO(10)=IHBE
         GEO(171)=IHBE
C
         IF(ISROT==0)ISROT=IDRIL_D
         IF(ISROT==2) ISROT = 0
         IGEO(20)=ISROT
C-------to have DR----            
         IF (ISMSTR == 10 .AND. ISROT > 0 .AND. IDROT == 0 ) IDROT = 1
         IF(ISMSTR  == 0)ISMSTR=2
         IF(ISMSTR  == 3. AND.IHBE /= 0 .AND. IHBE /= 2) THEN
           ISMSTR = 2
           CALL ANCMSG(MSGID=319,
     .                 MSGTYPE=MSGWARNING,
     .                 ANMODE=ANINFO_BLIND_2,
     .                 I1=PROP_ID,
     .                 C1=IDTITL)
         ENDIF
         GEO(3) =ISMSTR
         IGEO(5)=ISMSTR
C      
       IF (SUB_ID /= 0)
     .       CALL SUBROTVECT(VX,VY,VZ,RTRANS,SUB_ID,LSUBMODEL)
C---
      GEO(1) = THK
      IGEO(32) =  0  ! ISLV not used (may be needed for plyxfem)
      IF(GEO(38) == ZERO) GEO(38) = FIVE_OVER_6
       !! IF(NLY == 1) GEO(38)= ZERO
      GEO(42) = EM03 ! default value  ! not used 
      FAIL_SHELL = 0
      AN=SQRT(VX*VX+VY*VY+VZ*VZ)
      IF(AN < EM10)THEN
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
        VX=VX/AN
        VY=VY/AN
        VZ=VZ/AN
      ENDIF
      ISK = 0
      IF (IDSK/=0) THEN
        DO J=0,NUMSKW+MIN(IUN,NSPCOND)*NUMSPH+NSUBMOD
          IF(IDSK == ISKN(4,J+1)) THEN
            ISK=J+1
            GO TO 10
          ENDIF
        END DO
        CALL FRETITL2(TITR,IGEO(NPROPGI-LTITR+1),LTITR)
        CALL ANCMSG(MSGID=184,
     .              MSGTYPE=MSGERROR,
     .              ANMODE=ANINFO,
     .              C1='PROPERTY',
     .              I1=PROP_ID,
     .              C2='PROPERTY',
     .              C3=TITR,
     .              I2=IDSK)
 10     CONTINUE
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
     .         I1=PROP_ID,
     .         I2=IPID0)
        ENDIF      
      ENDDO 
c
      IF(ITHK == 0)ITHK=ITHK_D
      IF(ISHEAR == 0)ISHEAR=ISHEA_D
      IF(IPLAST == 0)IPLAST=IPLA_D
!      
      IHBE = IGEO(10)
      ISS  = IGEO(5)
C======================================================================|
      IF (DM == ZERO) IGEO(31) = 1
      IGEO( 1) = PROP_ID
      IGEO(2)  = ISK
      IGEO(5)  = ISMSTR
      IGEO(6)  = IORTH  ! IREP
      IGEO(11) = IGTYP
      IGEO(18) = ISH3N
      IGEO(19) = ISHXFEM
      IGEO(20) = ISROT
      IGEO(14)  = IRP
!!      IGEO(47) = IINT
      !! is not necessary it done in translation : before 2017 igmat=-1
      !! the new global mat is by default since 2017 version IGMAT IS USED GENERALLY > 0 
      IGEO(98) = IGMAT  
c
      GEO(3)  = ISMSTR
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
      GEO(20) = VISC_INT ! for ply-xfem formulation
      GEO(35) = ITHK
      GEO(37) = ISHEAR
      GEO(38) = ASHEAR
      GEO(39) = IPLAST
      GEO(171)= IHBE  ! double stockage
      GEO(199)= ZSHIFT  
C      
      IHBE=NINT(GEO(171))                                 
      IF(IHBE==0)THEN                                       
        GEO(171)=0                                        
      ELSEIF(IHBE==1)THEN                                   
        GEO(171)= 1                                        
      ELSEIF(IHBE==2)THEN                                   
        GEO(171)=0                                        
      ELSEIF(IHBE>=3.AND.IHBE<100.AND.IHBE/=4)THEN   
        GEO(171)=IHBE-1                                   
      ENDIF                                                 

      ISTRAIN=NINT(GEO(11))                               
      IF(ISTRAIN==0)THEN                                    
        GEO(11)=0                                         
      ELSEIF(ISTRAIN==1)THEN                                
        GEO(11)=1                                         
      ELSEIF(ISTRAIN==2)THEN                                
        GEO(11)=0                                         
      ENDIF                                                 

      ISHEAR = GEO(37)                                    
      IF(ISHEAR==0)THEN                                     
        GEO(37)=0                                         
      ELSEIF(ISHEAR==1)THEN                                 
        GEO(37)=1                                         
      ELSEIF(ISHEAR==2)THEN                                 
        GEO(37)=0                                         
      ENDIF                                                 
!! N1 it not used  only with old input format     
!!      IGEO(IPPID + N1) = IPOS
      CALL FRETITL2(TITR,IGEO(NPROPGI-LTITR+1),LTITR)
C-----------------------------------------------------------------
      IF(IS_ENCRYPTED)THEN
         WRITE(IOUT,1000)PROP_ID
 1000    FORMAT(
     &    5X,'COMPOSITE LAYERED SHELL PROPERTY SET'/,
     &    5X,'------------------------------------'/,
     &    5X,'PROPERTY SET NUMBER . . . . . . . . . .=',I10/,
     &    5X,'CONFIDENTIAL DATA'//)
      ELSE
       IF(ISK==0)THEN
        IF (IHBE > 11.AND.IHBE < 29) THEN
          WRITE(IOUT,2112)PROP_ID,ISTRAIN,GEO(1),ISS,IHBE,
     .               ISH3N,IGEO(20),
     .               GEO(16),GEO(13),GEO(38),FAIL_SHELL,ISHEAR,ITHK,
     .               IPLAST,IREP,GEO(7),GEO(8),GEO(9),IGEO(14)
         ELSE
          WRITE(IOUT,2110)PROP_ID,ISTRAIN,GEO(1),ISS,IHBE, ! nly de moins
     .               ISH3N,
     .               GEO(13),GEO(14),GEO(15),GEO(16),
     .               GEO(38),FAIL_SHELL,ISHEAR,ITHK,IPLAST,IREP,
     .               GEO(7),GEO(8),GEO(9),IGEO(14)
         ENDIF
       ELSE
        IF (IHBE > 11.AND.IHBE < 29) THEN
          WRITE(IOUT,2113)PROP_ID,ISTRAIN,GEO(1),ISS,IHBE,
     .               ISH3N,IGEO(20),
     .               GEO(16),GEO(13),GEO(38),FAIL_SHELL,ISHEAR,ITHK,
     .               IPLAST,IREP,IDSK,IGEO(14)
         ELSE
           WRITE(IOUT,2111)PROP_ID,ISTRAIN,GEO(1),ISS,IHBE,
     .               ISH3N,
     .               GEO(13),GEO(14),GEO(15),GEO(16),GEO(38),
     .               FAIL_SHELL,ISHEAR,ITHK,IPLAST,IREP,IDSK,IGEO(14)
        ENDIF
       ENDIF
       IF(ISHXFEM > 0) WRITE(IOUT, 2114)
      ENDIF
      
C---
!!!      IF (NLY>NLYMAX) THEN
!!         CALL ANCMSG(MSGID=28,
!!     .               MSGTYPE=MSGERROR,
!!     .               ANMODE=ANINFO_BLIND_1,
!!     .               I1=IG,
!!     .               C1=TITR,
!!     .               I2=NLYMAX)
!!      ENDIF
C          
        NC = NPLY        
        ALLOCATE( IDMAT_INTP(NPLY),IDMAT_SUB(NISUB))
        IDMAT_INTP = 0
        IDMAT_SUB = 0   
        N1 = NPLY
        GEO(6)=N1 + EM01
        IGEO(4) = N1
C
        IGEO(43) = NSUB ! number of substack               
        IGEO(44) = NISUB ! number of interface 
         DO 250 K = 1, NPLY 
           IMID = STACK_INFO%MID_IP(K)
           IDMAT_INTP(K) = IMID
           IF( IMID == 0 .AND. ISHXFEM > 0) THEN
             CALL FRETITL2(TITR,IGEO(NPROPGI-LTITR+1),LTITR)
             CALL ANCMSG(MSGID=2019,
     .                   MSGTYPE=MSGERROR,
     .                   ANMODE=ANINFO_BLIND_2,
     .                   I1=PROP_ID,
     .                   C1=TITR,
     .                   I2=IMID)
           ENDIF
           DO J=1,NUMMAT
            IF(IPM(1,J) == IMID) THEN
              STACK_INFO%MID_IP(K) = J
             GO TO 250
            ENDIF
           ENDDO
           IF(ISHXFEM > 0) THEN
             CALL FRETITL2(TITR,IGEO(NPROPGI-LTITR+1),LTITR)
             CALL ANCMSG(MSGID=2019,
     .                   MSGTYPE=MSGERROR,
     .                   ANMODE=ANINFO_BLIND_2,
     .                   I1=PROP_ID,
     .                   C1=TITR,
     .                   I2=IMID)
           ENDIF
           STACK_INFO%MID_IP(K) = 0
  250    CONTINUE
C isub stack
       IF(IGEO(44) > 0) THEN
         DO 300 K = 1, NISUB
           IMID = STACK_INFO%ISUB (3*(K-1) + 3)
           IDMAT_SUB(K) = IMID
           IF( IMID == 0 .AND. ISHXFEM > 0) THEN
             CALL FRETITL2(TITR,IGEO(NPROPGI-LTITR+1),LTITR)
             CALL ANCMSG(MSGID=31,
     .                   MSGTYPE=MSGERROR,
     .                   ANMODE=ANINFO_BLIND_2,
     .                   I1=PROP_ID,
     .                   C1=TITR,
     .                   I2=IMID)
           ENDIF
           DO J=1,NUMMAT
            IF(IPM(1,J) == IMID) THEN
             STACK_INFO%ISUB (3*(K-1) + 3) = J
             GO TO 300
            ENDIF
           ENDDO
           IF(ISHXFEM > 0) THEN
             CALL FRETITL2(TITR,IGEO(NPROPGI-LTITR+1),LTITR)
             CALL ANCMSG(MSGID=31,
     .                   MSGTYPE=MSGERROR,
     .                   ANMODE=ANINFO_BLIND_2,
     .                   I1=PROP_ID,
     .                   C1=TITR,
     .                   I2=IMID)
           ENDIF
           STACK_INFO%ISUB (3*(K-1) + 3) = 0
  300    CONTINUE
       ENDIF 
       IGEO(99)  = IPOS
       IF(NSUB > 0) THEN
         KK = 0
         DO  II = 1,NSUB
            IDSUB   = STACK_INFO%SUB ( 2*(II - 1) + 1)  
            NPT_SUB = STACK_INFO%SUB ( 2*(II - 1) + 2) 
C            
             IF(IS_ENCRYPTED .EQV. .FALSE. )WRITE(IOUT,3000)IDSUB
             DO K=1,NPT_SUB
               M1=  KK + K 
               IF(IS_ENCRYPTED .EQV. .FALSE. ) THEN
                   WRITE(IOUT,2121)K,STACK_INFO%PID(M1),STACK_INFO%ANG(M1),
     .                                                  STACK_INFO%POS(M1)
                   IF(ISHXFEM > 0) WRITE(IOUT,2122)IDMAT_INTP(M1),GEO(20)
               ENDIF  
               STACK_INFO%ANG(M1)=STACK_INFO%ANG(M1)*PI/HUNDRED80                              
             ENDDO
             KK = KK + NPT_SUB
         ENDDO
         NISUB=  IGEO(44)
         DO K=1,NISUB
            IPID1  =STACK_INFO%ISUB (3*(K - 1) + 1)
            IPID2  =STACK_INFO%ISUB (3*(K - 1) + 2)
            IMAT   =STACK_INFO%ISUB (3*(K - 1) + 3)
            IF( IS_ENCRYPTED .EQV. .FALSE.  ) THEN
              WRITE(IOUT,3100)K 
              IF(ISHXFEM > 0) THEN
                 WRITE(IOUT,3200)IPID1,IPID2,IMAT
              ELSE  
                 WRITE(IOUT,3300)IPID1,IPID2
              ENDIF
           ENDIF 
         ENDDO
       ELSE
           DO N=1,N1 
              IF(IS_ENCRYPTED .EQV. .FALSE. )THEN
                WRITE(IOUT,2121)N,STACK_INFO%PID(N),STACK_INFO%ANG(N),
     .                            STACK_INFO%POS(N)
                IF(N < N1 .AND. ISHXFEM > 0) WRITE(IOUT,2122)IDMAT_INTP(N),GEO(20)
              ENDIF
              STACK_INFO%ANG(N)=STACK_INFO%ANG(N)*PI/HUNDRED80
           ENDDO
      ENDIF     
C      
      DEALLOCATE (IDMAT_INTP,IDMAT_SUB)
C----
      RETURN
 2110 FORMAT(
     & 5X,'COMPOSITE STACK SHELL PROPERTY SET ',
     &    'WITH VARIABLE THICKNESSES AND MATERIALS'/,
     & 5X,'PROPERTY SET NUMBER . . . . . . . . . .=',I10/,
!!     & 5X,'NUMBER OF LAYERS. . . . . . . . . . . .=',I10/,
     & 5X,'POST PROCESSING STRAIN FLAG . . . . . .=',I10/,
     & 5X,'SHELL THICKNESS . . . . . . . . . . . .=',1PG20.13/,
     & 5X,'SMALL STRAIN FLAG . . . . . . . . . . .=',I10/,
     & 5X,'SHELL FORMULATION FLAG. . . . . . . . .=',I10/,
     & 5X,'3NODE SHELL FORMULATION FLAG. . . . . .=',I10/,
     & 5X,'SHELL HOURGLASS MEMBRANE DAMPING. . . .=',1PG20.13/,
     & 5X,'SHELL HOURGLASS FLEXURAL DAMPING. . . .=',1PG20.13/,
     & 5X,'SHELL HOURGLASS ROTATIONAL DAMPING. . .=',1PG20.13/,
     & 5X,'SHELL MEMBRANE DAMPING. . . . . . . . .=',1PG20.13/,
     & 5X,'SHEAR AREA REDUCTION FACTOR . . . . . .=',1PG20.13/,
     & 5X,'SHELL FAILURE FLAG. . . . . . . . . . .=',I10/,
     & 5X,'SHEAR FORMULATION FLAG. . . . . . . . .=',I10/,
     & 5X,'THICKNESS VARIATION FLAG. . . . . . . .=',I10/,
     & 5X,'PLASTICITY FORMULATION FLAG . . . . . .=',I10/,
     & 5X,'LOCAL ORTOTHROPY SYSTEM FLAG. . . . . .=',I10/,
     & 5X,'X COMPONENT OF DIR 1 OF ORTHOTROPY. . .=',1PG20.13/,
     & 5X,'Y COMPONENT OF DIR 1 OF ORTHOTROPY. . .=',1PG20.13/,
     & 5X,'Z COMPONENT OF DIR 1 OF ORTHOTROPY. . .=',1PG20.13/,
     & 5X,'REFERENCE DIRECTION FLAG IN SHELL PLANE=',I10/)
 2111 FORMAT(
     & 5X,'COMPOSITE STACK SHELL PROPERTY SET ',
     &    'WITH VARIABLE THICKNESSES AND MATERIALS'/,
     & 5X,'PROPERTY SET NUMBER . . . . . . . . . .=',I10/,
!!     & 5X,'NUMBER OF LAYERS. . . . . . . . . . . .=',I10/,
     & 5X,'POST PROCESSING STRAIN FLAG . . . . . .=',I10/,
     & 5X,'SHELL THICKNESS . . . . . . . . . . . .=',1PG20.13/,
     & 5X,'SMALL STRAIN FLAG . . . . . . . . . . .=',I10/,
     & 5X,'SHELL FORMULATION FLAG. . . . . . . . .=',I10/,
     & 5X,'3NODE SHELL FORMULATION FLAG. . . . . .=',I10/,
     & 5X,'SHELL HOURGLASS MEMBRANE DAMPING. . . .=',1PG20.13/,
     & 5X,'SHELL HOURGLASS FLEXURAL DAMPING. . . .=',1PG20.13/,
     & 5X,'SHELL HOURGLASS ROTATIONAL DAMPING. . .=',1PG20.13/,
     & 5X,'SHELL MEMBRANE DAMPING. . . . . . . . .=',1PG20.13/,
     & 5X,'SHEAR AREA REDUCTION FACTOR . . . . . .=',1PG20.13/,
     & 5X,'SHELL FAILURE FLAG. . . . . . . . . . .=',I10/,
     & 5X,'SHEAR FORMULATION FLAG. . . . . . . . .=',I10/,
     & 5X,'THICKNESS VARIATION FLAG. . . . . . . .=',I10/,
     & 5X,'PLASTICITY FORMULATION FLAG . . . . . .=',I10/,
     & 5X,'LOCAL ORTOTHROPY SYSTEM FLAG. . . . . .=',I10/,
     & 5X,'SKEW OF THE FIRST ORTHOTROPY DIRECTION.=',I10/,
     & 5X,'REFERENCE DIRECTION FLAG IN SHELL PLANE=',I10/)
 2112 FORMAT(
     & 5X,'COMPOSITE STACK SHELL PROPERTY SET ',
     &    'WITH VARIABLE THICKNESSES AND MATERIALS'/,
     & 5X,'PROPERTY SET NUMBER . . . . . . . . . .=',I10/,
!!     & 5X,'NUMBER OF LAYERS. . . . . . . . . . . .=',I10/,
     & 5X,'POST PROCESSING STRAIN FLAG . . . . . .=',I10/,
     & 5X,'SHELL THICKNESS . . . . . . . . . . . .=',1PG20.13/,
     & 5X,'SMALL STRAIN FLAG . . . . . . . . . . .=',I10/,
     & 5X,'SHELL FORMULATION FLAG. . . . . . . . .=',I10/,
     & 5X,'3NODE SHELL FORMULATION FLAG. . . . . .=',I10/,
     & 5X,'DRILLING D.O.F. FLAG  . . . . . . . . .=',I10/,
     & 5X,'SHELL MEMBRANE DAMPING. . . . . . . . .=',1PG20.13/,
     & 5X,'SHELL NUMERICAL DAMPING . . . . . . . .=',1PG20.13/,
     & 5X,'SHEAR AREA REDUCTION FACTOR . . . . . .=',1PG20.13/,
     & 5X,'SHELL FAILURE FLAG. . . . . . . . . . .=',I10/,
     & 5X,'SHEAR FORMULATION FLAG. . . . . . . . .=',I10/,
     & 5X,'THICKNESS VARIATION FLAG. . . . . . . .=',I10/,
     & 5X,'PLASTICITY FORMULATION FLAG . . . . . .=',I10/,
     & 5X,'LOCAL ORTOTHROPY SYSTEM FLAG. . . . . .=',I10/,
     & 5X,'X COMPONENT OF DIR 1 OF ORTHOTROPY. . .=',1PG20.13/,
     & 5X,'Y COMPONENT OF DIR 1 OF ORTHOTROPY. . .=',1PG20.13/,
     & 5X,'Z COMPONENT OF DIR 1 OF ORTHOTROPY. . .=',1PG20.13/,
     & 5X,'REFERENCE DIRECTION FLAG IN SHELL PLANE=',I10/)
 2113 FORMAT(
     & 5X,'COMPOSITE STACK SHELL PROPERTY SET ',
     &    'WITH VARIABLE THICKNESSES AND MATERIALS'/,
     & 5X,'PROPERTY SET NUMBER . . . . . . . . . .=',I10/,
!!     & 5X,'NUMBER OF LAYERS. . . . . . . . . . . .=',I10/,
     & 5X,'POST PROCESSING STRAIN FLAG . . . . . .=',I10/,
     & 5X,'SHELL THICKNESS . . . . . . . . . . . .=',1PG20.13/,
     & 5X,'SMALL STRAIN FLAG . . . . . . . . . . .=',I10/,
     & 5X,'SHELL FORMULATION FLAG. . . . . . . . .=',I10/,
     & 5X,'3NODE SHELL FORMULATION FLAG. . . . . .=',I10/,
     & 5X,'DRILLING D.O.F. FLAG  . . . . . . . . .=',I10/,
     & 5X,'SHELL MEMBRANE DAMPING. . . . . . . . .=',1PG20.13/,
     & 5X,'SHELL NUMERICAL DAMPING . . . . . . . .=',1PG20.13/,
     & 5X,'SHEAR AREA REDUCTION FACTOR . . . . . .=',1PG20.13/,
     & 5X,'SHELL FAILURE FLAG. . . . . . . . . . .=',I10/,
     & 5X,'SHEAR FORMULATION FLAG. . . . . . . . .=',I10/,
     & 5X,'THICKNESS VARIATION FLAG. . . . . . . .=',I10/,
     & 5X,'PLASTICITY FORMULATION FLAG . . . . . .=',I10/,
     & 5X,'LOCAL ORTOTHROPY SYSTEM FLAG. . . . . .=',I10/,
     & 5X,'SKEW OF THE FIRST ORTHOTROPY DIRECTION.=',I10/,
     & 5X,'REFERENCE DIRECTION FLAG IN SHELL PLANE=',I10/)
 2114 FORMAT(
     & 5X,'PLY XFEM SHELL COMPOSITE FORMULATION IS ACTIVED '/)
 2121 FORMAT(
     & 5X,'    PLY ',I3/,
     & 5X,'      PLY PID NUMBER  . . . . . . . . .=',I10/
     & 5X,'      ANGLE (DIR 1,PROJ(DIR 1 / SHELL).=',1PG20.13/,
     & 5X,'      POSITION. . . . . . . . . . . . .=',1PG20.13/)
 2122 FORMAT(
     & 5X,'      INTER-PLY MID NUMBER  . . . . . .=',I10/ 
     & 5X,'      INTER-PLY SHELL DAMPING . . . . . .=',1PG20.13/)
 3000 FORMAT(
     & 5X,' COMPOSITE SUBSTACK SHELL ID . . . . . . =',I10/ ) 
 3100 FORMAT(
     & 5X,'  INTERFACE NUMBER BETWEEN-SUBSTACK . . .:',I10/ )
 3200 FORMAT(
     & 5X,'      INTER-PLY_1 PID NUMBER  . . . . . =',I10/, 
     & 5X,'      INTER-PLY_2 PID NUMBER . . . . . .=',I10/,   
     & 5X,'      INTER-PLY MID NUMBER  . . . . . . =',I10/) 
 3300 FORMAT(
     & 5X,'      INTER-PLY_1 PID NUMBER  . . . . . =',I10/, 
     & 5X,'      INTER-PLY_2 PID NUMBER . . . . . .=',I10/)
      END
