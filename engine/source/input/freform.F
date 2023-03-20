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
Chd|  FREFORM                       source/input/freform.F        
Chd|-- called by -----------
Chd|        LECINP                        source/input/lecinp.F         
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        ARRET                         source/system/arret.F         
Chd|        CONNECTION_SOCK_INIT_C        source/coupling/rad2rad/rad2rad_c.c
Chd|        DELETE_USER_FILE              source/user_interface/dyn_userlib.c
Chd|        ERRMSG                        source/input/errmsg.F         
Chd|        FRALEONOFF                    source/input/fraleonoff.F     
Chd|        FRALNK                        source/input/fralnk.F         
Chd|        FRALNK2                       source/input/fralnk.F         
Chd|        FREABF                        source/input/freabf.F         
Chd|        FREANIM                       source/output/anim/reader/freanim.F
Chd|        FREBCS                        source/input/frebcs.F         
Chd|        FRECPL                        source/input/frecpl.F         
Chd|        FREDAMP                       source/input/fredamp.F        
Chd|        FREDEBUG                      source/input/fredebug.F       
Chd|        FREDLI                        source/input/fredli.F         
Chd|        FREDLI7                       source/input/fredli7.F        
Chd|        FREDYNAIN                     source/input/fredynain.F      
Chd|        FREEIG                        source/input/freeig.F         
Chd|        FREFLW                        source/input/freflw.F         
Chd|        FREFUNC                       source/input/frefunc.F        
Chd|        FREFVBAG                      source/input/frefvbag.F       
Chd|        FREFXINP                      source/input/frefxinp.F       
Chd|        FREIMPL                       source/input/freimpl.F        
Chd|        FREINIV                       source/input/freiniv.F        
Chd|        FREINT                        source/input/freint.F         
Chd|        FRELNK                        source/input/frelnk.F         
Chd|        FRENOIS                       source/input/frenois.F        
Chd|        FREOUTP                       source/input/freoutp.F        
Chd|        FRERBO                        source/input/frerbo.F         
Chd|        FRESTAT                       source/input/frestat.F        
Chd|        FRETHERMAL                    source/constraints/thermic/frethermal.F
Chd|        FREUPWIND                     source/input/freupwind.F      
Chd|        FREUPWM                       source/input/freupwm.F        
Chd|        H3D_INI                       source/output/h3d/h3d_build_fortran/h3d_ini.F
Chd|        H3D_READ                      source/output/h3d/h3d_build_fortran/h3d_read.F
Chd|        OPEN_F_SCRATCH_FILE           source/system/open_tempfile.F 
Chd|        RDELE                         source/input/rdele.F          
Chd|        REDKEY0                       source/input/redkey0.F        
Chd|        REDKEY1                       source/input/redkey1.F        
Chd|        REDKEY1_H3D                   source/input/redkey1_h3d.F    
Chd|        WRIUSC2                       source/input/wriusc2.F        
Chd|        NVAR                          source/input/nvar.F           
Chd|        CHECK_MOD                     ../common_source/modules/check_mod.F
Chd|        FVBAG_MOD                     share/modules/fvbag_mod.F     
Chd|        H3D_MOD                       share/modules/h3d_mod.F       
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        METRIC_MOD                    ../common_source/modules/metric_mod.F
Chd|        STATE_MOD                     ../common_source/modules/state_mod.F
Chd|        TH_MOD                        share/modules/th_mod.F        
Chd|====================================================================
      SUBROUTINE FREFORM(IRUNN,IRFL,IRFE,H3D_DATA,FLAG_CST_AMS,DYNAIN_DATA)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
      USE H3D_MOD
      USE METRIC_MOD
      USE CHECK_MOD
      USE FVBAG_MOD
      USE TH_MOD
      USE STATE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IRUNN, IRFL, IRFE, FLAG_CST_AMS
      TYPE(H3D_DATABASE), INTENT(INOUT) :: H3D_DATA
      TYPE (DYNAIN_DATABASE), INTENT(INOUT) :: DYNAIN_DATA
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "task_c.inc"
#include      "units_c.inc"
#include      "warn_c.inc"
#include      "lon_c.inc"
#include      "scr02_c.inc"
#include      "scr03_c.inc"
#include      "scr05_c.inc"
#include      "scr06_c.inc"
#include      "scr07_c.inc"
#include      "scr17_c.inc"
#include      "scr18_c.inc"
#include      "stati_c.inc"
#include      "statr_c.inc"
#include      "cong1_c.inc"
#include      "cong2_c.inc"
#include      "chara_c.inc"
#include      "scrfs_c.inc"
#include      "parit_c.inc"
#include      "couple_c.inc"
#include      "rad2r_c.inc"
#include      "timeri_c.inc"
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "com08_c.inc"
#include      "check.inc"
#include      "subc_c.inc"
#include      "scr19_c.inc"
#include      "ddspmd_c.inc"
#include      "sms_c.inc"
#include      "scr_thermal_c.inc"
#include      "alesolver.inc"
#include      "scrupw_c.inc"
#include      "ale_param.inc"
#include      "alemuscl.inc"
C-----------------------------------------------
C   L o c a l   P a r a m e t e r s
C-----------------------------------------------
      INTEGER NKEY,IRUNR
      PARAMETER (NKEY = 91)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER IKAD(0:NKEY+1),
     .   KDTIX, KIMPL, KFLSEC, KFUNC, KKEREL, KDYREL, KRUN, KTITLE,
     .   KDT, KTFILE, KPRINT, KRFILE, KVERS, KALE, KDEL,
     .   KVEL, KBCS, KBCSR, KDEBUG,KOUTP,KDTNOD,KALESUB,KPROC,
     .   KNOISE,KDELIN,KPARIT,KINIV,KRBODY,KANIM,KINTER,
     .   KUPWM, KMON,KDT1,KINCMP,KTH, KSTOP, KKILL,
     .   KMADYM,KSPMD,KRAD2R,KDAMP,N_DAMP, KATFIL,KBTFIL,KCTFIL,KDTFIL,
     .   KETFIL,KFTFIL,KGTFIL,KHTFIL,KITFIL, KRERUN, KQEPH, KFXINP,
     .   NFXINP,KOSHSUB,KEIG,NEIGOFF,NEOFF,KFVBAG,
     .   NFVMESH,KSTATF,KSHVER,KDT1HEPH,KABF,KDTSDE,
     .   KAABF,KBABF,KCABF,KDABF,KEABF,KFABF,KGABF,KHABF,KIABF,NFVMODI,
     .   KUPWIND,KTHERMAL,KH3D,KREPORT,KNEGVOL,KADYREL,KDT1TET10,KPERF,
     .   KDYNAIN,KDTTSH,KMASS
      INTEGER
     .   IKEY, I, II, I1, J1, J2, K,KK, M, N, NCT, IKOLD, NBC, K3, K4,
     .   NUBCS1,NUBCS2, IR,IDT,NELTGOF,
     .   IAL, NINTOF, NELSOF, NDEL, NOF,ITTYP, NCPRI0,NCPRI_HEAD0,
     .   NELQOF, NELCOF, NELTOF, NELPOF, NELROF, NUM_BCS, NINTCH,
     .   NRBYNF, NBCS1, NBCS2, NRBYON, NRBYOF, NINIV,NINIVT,
     .   INTCH_IFOP,IUSC_TMP,INTCH_ITOP,NBTOPFOP,KONOFF,NINTNOF,NINTSOF,
     .   NSPHOF,NALEOF,NEULEROF,NTHERMOF, IDTGRX,ibid, M1, M2, IFORM,
     .   GR_ID, NLINKOFF,IS_PREVKEYA_NCHARKEY,IS_KEYA_NCHARKEY, IFLAG,
     .   DT_CTRL_COMPATIBILITY(3), LOWMACH_OPT,IO_ERR, NITERP, IOK_READ,
     .   ID_BAG, IERR1, ID_DT_OPTION, FVMBAG_L_TYPE
      my_real
     .   TSTOP0,DTFAC0, DTMIN0, DTHIS0, ALP, GAM,
     .   A1, A2, A3, VM, DTFACA,DTMINA, DTHIS01(9),vt, TMP,DTFAC_AMS,
     .   LAMBDA,L_TYPE
      CHARACTER KEY(9)*5, TITLE*81,
     .  KEY1*5, KEY2*5,KEY3*5,   CH4*4, KEY4*5, KEY5*5, StrIAL*32
      CHARACTER(LEN=ncharline) :: KEYA
      CHARACTER(LEN=ncharline) :: CARTE,KEYW,MOT1,LINE
      CHARACTER(LEN=ncharkey), DIMENSION(9) :: KEY_H3D
      CHARACTER(LEN=5), DIMENSION(NKEY) :: KEY0
      CHARACTER(LEN=5), DIMENSION(24)   ::  KEYALE
      CHARACTER(len=ncharline) :: STRING_RESTART
      INTEGER BCS_SK_FR(10000),VEL_SK_FR(10000),NSLASH(NKEY)
      INTEGER :: NBALEOFF, NBALEON, NBPARTALEON, NBPARTALEOFF
      INTEGER :: NUM_ALE_KEY2, INUM
      LOGICAL :: lFOUND
C-----------------------------------------------
      INTEGER LEN_IUSC1_FN,LEN_IUSC2_FN,LEN_IUSCTMP_FN
      CHARACTER(LEN=10192) :: IUSC1_FN,IUSC2_FN,IUSCTMP_FN
      

C-----------------------------------------------
C   E x t e r n a l   F u n c t i o n s
C-----------------------------------------------
      INTEGER NVAR,INTDT
C-----------------------------------------------
      DATA KEY0/
     .     'RUN  '   ,'TITLE'   ,'DT   '   ,'TFILE'   ,'PRINT'   ,
     .     'RFILE'   ,'VERS '   ,''        ,''        ,'ALE  '   ,
     .     'DEL  '   ,'VEL  '   ,'BCS  '   ,'BCSR '   ,'INTER'   ,
     .     'DTIX '   ,'IMPL '   ,'FUNCT'   ,'FLSEC'   ,'DYREL'   ,
     .     'KEREL'   ,'RBODY'   ,'ANIM'    ,''        ,'DEBUG'   ,
     .     'OUTP '   ,'DTNOD'   ,'ALESU'   ,'PROC'    ,''        ,
     .     '@TFIL'   ,'DELIN'   ,''        ,'PARIT'   ,'INIV '   ,
     .     'UPWM '   ,'MON'     ,''        ,'DT1'     ,'INCMP'   ,
     .     ''        ,'TH'      ,'STOP'    ,'KILL'    ,'MADYM'   ,
     .     'SPMD '   ,'RAD2R'   ,'DAMP'    ,'ATFIL'   ,'BTFIL'   ,
     .     'CTFIL'   ,'DTFIL'   ,'ETFIL'   ,'FTFIL'   ,'GTFIL'   ,
     .     'HTFIL'   ,'ITFIL'   ,'RERUN'   ,'QEPH '   ,'FXINP'   ,
     .     'SHSUB'   ,''        ,'EIG'     ,''        ,'FVMBA'   ,
     .     ''        ,'STATE'   ,'SHVER'   ,'DT1HE'   ,'ABF'     ,
     .     'DTSDE'   ,'AABF'    ,'BABF'    ,'CABF'    ,'DABF'    ,
     .     'EABF '   ,'FABF'    ,'GABF'    ,'HABF'    ,'IABF'    ,
     .     'UPWIN'   ,'THERM'   ,'H3D'     ,'REPOR'   ,'NEGVO'  ,
     .     'ADYRE'   ,'DT1TE'   ,'PERF'    ,'DYNAI'   ,'DTTSH'   ,
     .     'MASS '    /

      DATA KRUN/1/    ,KTITLE/2/   ,KDT/3/       ,KTFILE/4/   ,KPRINT/5/ ,
     .     KRFILE/6/  ,KVERS/7/    ,                           KALE/10/  ,
     .     KDEL/11/   ,KVEL/12/    ,KBCS/13/     ,KBCSR/14/   ,KINTER/15/,
     .     KDTIX/16/  ,KIMPL/17/   ,KFUNC/18/    ,KFLSEC/19/  ,KDYREL/20/,
     .     KKEREL/21/ ,KRBODY/22/  ,KANIM/23/                 ,KDEBUG/25/,
     .     KOUTP/26/  ,KDTNOD/27/  ,KALESUB/28/   ,KPROC/29/  ,
     .     KNOISE/31/ ,KDELIN/32/                ,KPARIT/34/  ,KINIV/35/ ,
     .     KUPWM/36/  ,KMON/37/    ,              KDT1/39/    ,KINCMP/40/,
     .                 KTH/42/     ,KSTOP/43/    ,KKILL/44/   ,KMADYM/45/,
     .     KSPMD/46/  ,KRAD2R/47/  ,KDAMP/48/    ,KATFIL/49/  ,KBTFIL/50/,
     .     KCTFIL/51/ ,KDTFIL/52/  ,KETFIL/53/   ,KFTFIL/54/  ,KGTFIL/55/,
     .     KHTFIL/56/ ,KITFIL/57/  ,KRERUN/58/   ,KQEPH/59/   ,KFXINP/60/,
     .     KOSHSUB/61/,             KEIG/63/     ,             KFVBAG/65/,
     .                 KSTATF/67/  ,KSHVER/68/   ,KDT1HEPH/69/,KABF/70/  ,
     .     KDTSDE/71/ ,KAABF/72/   ,KBABF/73/    ,KCABF/74/   ,KDABF/75/ ,
     .     KEABF/76/  ,KFABF/77/   ,KGABF/78/    ,KHABF/79/   ,KIABF/80/ ,
     .     KUPWIND/81/,KTHERMAL/82/,KH3D/83/     ,KREPORT/84/ ,
     .     KNEGVOL/85/,KADYREL/86/ ,KDT1TET10/87/,KPERF/88/,
     .     KDYNAIN/89/,KDTTSH/90/  ,KMASS/91/

      DATA KEYALE/
     .     'BCS  '   ,'CLOSE'   ,'GRID '   ,'LINK '   ,'MAT  '   ,
     .     'ON   '   ,'OFF  '   ,'MUSCL'   ,'SOLVE'   ,'ZERO '   ,
     .     'DONEA'   ,'STAND'   ,'SPRIN'   ,'DISP '   ,'SUPG '   ,
     .     '0    '   ,'1    '   ,'2    '   ,'3    '   ,'4    '   ,
     .     'LOWMA'   ,'LAPLA'   ,'     '   ,'     '   /
      NUM_ALE_KEY2=21

C------------------------------------------------------------------
      KEY=" "
      KEY2=" "
      KEY3=" "
      KEY4=" "
      
      CALL OPEN_F_SCRATCH_FILE(IUSC1,2,ROOTN,LENROOTN,IUSC1_FN)
      LEN_IUSC1_FN=LEN_TRIM(IUSC1_FN)

      CALL OPEN_F_SCRATCH_FILE(IUSC2,1,ROOTN,LENROOTN,IUSC2_FN)
      LEN_IUSC2_FN=LEN_TRIM(IUSC2_FN)

C attention IBM/MPI ne supporte pas de REWIND sur STDIN !
      IUSC_TMP=60
      CALL OPEN_F_SCRATCH_FILE(IUSC_TMP,1,ROOTN,LENROOTN,IUSCTMP_FN)
      LEN_IUSCTMP_FN=LEN_TRIM(IUSCTMP_FN)
C
      ROOTNAM=' '
      ROOTLEN = 0
      IERR=0
      NCT=0
      J1 = 0
      J2 = 0
      NBC = 0
      IKAD(0:NKEY+1)=0
      NSLASH(1:NKEY) = 0
      IKEY = 0
      
      DTFACA = EP30
      DTMINA = EP30
      LAMBDA = ZERO
      L_TYPE = ZERO
      ID_BAG = 0
      
      PERCENT_ADDMASS = ZERO
      DT_STOP_PERCENT_ADDMASS = ZERO
      IDT_PERCENT_ADDMASS = 0
      
      TH_TITLES = 0
           
      IERR1=0
      IF(.NOT.ALLOCATED(FVMBAG_INPUT_OPTIONS)) ALLOCATE(FVMBAG_INPUT_OPTIONS(8192),STAT=IERR1)
      IF (IERR1/=0) GOTO 1000
      DO II=1,8192
        FVMBAG_INPUT_OPTIONS(II)%LAMBDA = ZERO 
        FVMBAG_INPUT_OPTIONS(II)%ID_BAG = 0
        FVMBAG_INPUT_OPTIONS(II)%L_type = 0
        FVMBAG_INPUT_OPTIONS(II)%ID_DT_OPTION = -1
      ENDDO
      NUM_OPT_DT_FVMBAG = 0
      NUM_OPT_DT_FVMBAG_0 = 0
      NUM_OPT_DT_FVMBAG_1 = 0
      NUM_OPT_DT_FVMBAG_2 = 0
      NUM_OPT_DT_FVMBAG_3 = 0
C
C     DT_CTRL_COMPATIBILITY(1) = 1 -> if /DT/NODA/CST is present
C     DT_CTRL_COMPATIBILITY(2) = 1 -> if /DT/AMS is present
C     DT_CTRL_COMPATIBILITY(3) = 1 -> if /DT/CST_AMS is present
      DT_CTRL_COMPATIBILITY = 0

      !ALE TIME STEP
      IDT_ALE     = 0
C     FLAG for Hybrid CST + AMS
      FLAG_CST_AMS = 0
C---------------------------------------------------
C     ENUMERATION OF CARDS BY OPTION
C---------------------------------------------------
   10 READ(ISTDI,'(A)',END=20) KEYA(1:ncharline)
        WRITE(IUSC_TMP,'(A)') KEYA(1:ncharline)
        IF(KEYA(1:4)=='/END')GOTO 20
        NCT=NCT+1
        IF(KEYA(1:1)=='#')GOTO 10
        IF(KEYA(1:1)=='$')GOTO 10
        IF(KEYA==' ')     GOTO 10
        IF(KEYA(1:1)=='/') CALL REDKEY0(KEY0,NKEY,KEYA,IKEY,NSLASH)
        IF(IKEY==0)     GOTO 10
        IKAD(IKEY)=IKAD(IKEY)+1
        IF(KEYA(1:4)=='/BCS' .AND. KEYA(1:5)/='/BCSR') THEN
          I=2
          J1 = J1 +1
          DO WHILE(KEYA(I:I)/='/'.AND.I<LEN_TRIM(KEYA))
            I=I+1
          ENDDO
          I=I+1
          DO WHILE(KEYA(I:I)/='/'.AND.I<LEN_TRIM(KEYA))
            I=I+1
          ENDDO
          I=I+1
          DO WHILE(KEYA(I:I)/='/'.AND.I<LEN_TRIM(KEYA))
            I=I+1
          ENDDO
          I=I+1
          I1=I
          IF (J1>1000) THEN
            CALL ANCMSG(MSGID=74,ANMODE=ANINFO,C1=KEYA(1:I1-1),C2='/BCS OR /BCSR')
            CALL ARRET(2)
          END IF
          IF(I < LEN_TRIM(KEYA)) THEN
            DO WHILE(KEYA(I1:I1)/='/'.AND.I1<LEN_TRIM(KEYA))
              I1=I1+1
            ENDDO
            MOT1=KEYA(I:I1)
            READ(MOT1,FMT='(I10)')BCS_SK_FR(J1)
          ELSE
            BCS_SK_FR(J1) = 0
          ENDIF
        ENDIF
C---
        IF(KEYA(1:4)=='/VEL') THEN
          I=2
          J2 = J2 +1
          DO WHILE(KEYA(I:I)/='/'.AND.I<LEN_TRIM(KEYA))
            I=I+1
          ENDDO
          I=I+1
          DO WHILE(KEYA(I:I)/='/'.AND.I<LEN_TRIM(KEYA))
            I=I+1
          ENDDO
          I=I+1
          DO WHILE(KEYA(I:I)/='/'.AND.I<LEN_TRIM(KEYA))
            I=I+1
          ENDDO
          I=I+1
          I1=I
          IF (J2>1000) THEN
            CALL ANCMSG(MSGID=74,ANMODE=ANINFO,
     .                  C1=KEYA(1:I1-1),C2='/VEL')
            CALL ARRET(2)
          END IF
          IF(I < LEN_TRIM(KEYA)) THEN
            DO WHILE(KEYA(I1:I1)/='/'.AND.I1<LEN_TRIM(KEYA))
              I1=I1+1
            ENDDO
            IF(KEYA(I1:I1)=='/')I1=I1-1 !set position to the first character before '/'
            MOT1=KEYA(I:I1)
            READ(MOT1,FMT='(I10)')VEL_SK_FR(J2)
          ELSE
            VEL_SK_FR(J2) = 0
          ENDIF
        ENDIF
C---
      GO TO 10
  20  CONTINUE
      ISTDI=IUSC_TMP
      REWIND(ISTDI)
C---------------------------------------------------
C     CALCUL DES RECORDS
C---------------------------------------------------
      IKAD(0)=1
      DO I=1,NKEY+1
        IKAD(I)=IKAD(I)+IKAD(I-1)
      ENDDO
      DO I=NKEY+1,1,-1
        IKAD(I)=IKAD(I-1)
      ENDDO
C---------------------------------------------------
C     ECRITURE CLASSEE(SUR PREMIER MOT CLEF) EN ACCES DIRECTE
C---------------------------------------------------
      IKOLD=0
      IS_KEYA_NCHARKEY = 0
      IS_PREVKEYA_NCHARKEY = 0
      DO N=1,NCT
       READ(ISTDI,'(A)') KEYA
       IF(KEYA(1:1)=='#')CYCLE
       IF(KEYA(1:1)=='$')CYCLE
       IF(KEYA==' ')     CYCLE
c
       IF(KEYA(1:4)=='/H3D' .OR. KEYA(1:6)=='/STATE'.OR. KEYA(1:7)==
     .      '/DYNAIN'.OR.KEYA(1:5)=='/ANIM')THEN
         IS_KEYA_NCHARKEY = 1
       ELSE
         IS_KEYA_NCHARKEY = 0
       ENDIF
c
       IF(IS_KEYA_NCHARKEY == 1)THEN
         IF(IS_PREVKEYA_NCHARKEY == 1 .AND. IKOLD /=0)THEN
           WRITE(IUSC1,REC=IKOLD,FMT='(A,I10)') KEYW,NBC
         ELSEIF(IKOLD/=0)THEN
           WRITE(IUSC1,REC=IKOLD,FMT='(17A,I10)')'/',(KEY(I),'/',I=1,8),NBC
         ENDIF
         KEYW = KEYA
         IS_PREVKEYA_NCHARKEY = 1
         CALL REDKEY1_H3D(KEY_H3D,KEY0,NKEY,KEYA,IKEY,IR)
         IF(IR/=0) CYCLE
         NBC=0
         IKOLD=IKAD(IKEY)

       ELSEIF(KEYA(1:1)=='/')THEN
         IF(IS_PREVKEYA_NCHARKEY == 1 .AND. IKOLD /=0)THEN
           WRITE(IUSC1,REC=IKOLD,FMT='(A,I10)') KEYW,NBC
         ELSEIF(IKOLD/=0)THEN
           WRITE(IUSC1,REC=IKOLD,FMT='(17A,I10)')'/',(KEY(I),'/',I=1,8),NBC
         ENDIF
         IS_PREVKEYA_NCHARKEY = 0
         CALL REDKEY1(KEY,KEY0,NKEY,KEYA,IKEY,IR)
         IF(IR/=0) CYCLE
         NBC=0
         IKOLD=IKAD(IKEY)

       ELSEIF( IS_PREVKEYA_NCHARKEY == 1) THEN
         NBC=NBC+1
         WRITE(IUSC1,REC=IKAD(IKEY),FMT='(A)')KEYA

       ELSE
         NBC=NBC+1
         WRITE(IUSC1,REC=IKAD(IKEY),FMT='(A)')KEYA
       ENDIF
       IKAD(IKEY)=IKAD(IKEY)+1


      ENDDO !next N
c
      IF( IS_PREVKEYA_NCHARKEY == 1) THEN
        WRITE(IUSC1,REC=IKOLD,FMT='(A,I10)') KEYW,NBC
      ELSE
        WRITE(IUSC1,REC=IKOLD,FMT='(17A,I10)')'/',(KEY(I),'/',I=1,8),NBC
      ENDIF
C
      DO I=NKEY+1,1,-1
        IKAD(I)=IKAD(I-1)
      ENDDO
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7--
        CLOSE(UNIT=IUSC_TMP)
        CALL DELETE_USER_FILE(IUSCTMP_FN,LEN_IUSCTMP_FN)
        ISTDI = 5
       IF (GOT_INPUT==1) THEN
        ISTDI=80
        CLOSE(UNIT=ISTDI, STATUS='DELETE', IOSTAT=IO_ERR)
       ELSE
        CLOSE (UNIT=ISTDI)
       ENDIF
C========================================================================
C---- Distinction d'un fichier de relance de calcul
C   /RERUN/ROOTNAME/IRUN
C---------------
C RERUN OPTION
C
      IKEY=KRERUN
      IRUNN=1
      MCHECK =0
      TSTOP0= ZERO
      IF(IKAD(IKEY)/=IKAD(IKEY+1))THEN
       IRUNN=0
       READ(IUSC1,REC=IKAD(IKEY),FMT='(7X,A4,2X,I5)',ERR=9990)CH4,IRUNN

       IF (GOT_INPUT == 0)THEN
C RUNN is -input run number,
C if RUNN = 0 (common starter / engine input file) RUNN is set to 1 in Engine.
        IF (RUNN==0 ) RUNN=1
C IRUNN is the run number read from input.
C If IRUNN was set in input and is different from -input run number,
C than we print out a message.
        IF (IRUNN /= 0)THEN
          IF (IRUNN /= RUNN)THEN
           WRITE(ISTDO,'(A,A)') ' ** WARNING ** : RUN NUMBER FOUND IN ',
     *               '/RUN CARD FROM INPUT FILE'
            WRITE(ISTDO,'(A)')   '                 RUN NUMBER IGNORED '
          ENDIF
        ENDIF
        IRUNN = RUNN
       ENDIF
C
       IF(ROOTLEN==0) THEN
        CALL ANCMSG(MSGID=194,ANMODE=ANINFO)
        IERR=IERR+1
       ENDIF
       IF(IRUNN<=0) THEN
        CALL ANCMSG(MSGID=195,ANMODE=ANINFO)
        IERR=IERR+1
        IRUNN=1
       ENDIF
       MCHECK = 1
      ENDIF

C--------------------
C     INPUT VERSION
C--------------------
      INVERS=0
      IKEY=KVERS
      IF (GOT_INPUT/=0) THEN
        INVERS=CODVERS
      END IF
      IF(IKAD(IKEY)/=IKAD(IKEY+1))THEN
       READ(IUSC1,REC=IKAD(IKEY),FMT='(7X,I5)',ERR=9990)INVERS
      ENDIF
      IF (GOT_INPUT==0) THEN
        IF(INVERS<18.AND.MCHECK==0) THEN
          CALL ANCMSG(MSGID=196,ANMODE=ANINFO)
          IERR=IERR+1
        ENDIF
      END IF
C--------------------
C     OPTION V181 ET +
C--------------------
      IPARIT = -1
      IKEY=KPARIT
      IF(IKAD(IKEY)/=IKAD(IKEY+1))THEN
       READ(IUSC1,REC=IKAD(IKEY),FMT='(7X,A,1X,I5)',
     .        ERR=9990)KEY2,K4
       IF(KEY2=='OFF')THEN
         IPARIT = 0
       ELSEIF(KEY2=='ON')THEN
         IPARIT = K4 + 1
       ENDIF
      ENDIF
C
      IKEY=KDTNOD
      NODADT = 0
      IF(IKAD(IKEY)/=IKAD(IKEY+1)) NODADT = 1
      IKEY=KDTIX
      IF(IKAD(IKEY)/=IKAD(IKEY+1))THEN
       CALL WRIUSC2(IKAD(IKEY)+1,1,KEY0(IKEY))
       READ(IUSC2,*,ERR=9990,END=9990)DTIN,DTMX
      ELSE
       DTIN=ZERO
       DTMX=EP30
      ENDIF
      IMPL=0
C
      NSFLSW=0
      IKEY=KFLSEC
      IF(IKAD(IKEY)/=IKAD(IKEY+1))THEN
       K=0
 110   READ(IUSC1,REC=IKAD(IKEY)+K,FMT='(49X,I10)',
     .      ERR=9990)NBC
       K=K+NBC+1
       NSFLSW=NSFLSW+1
       IF(IKAD(IKEY)+K/=IKAD(IKEY+1))GO TO 110
      ENDIF
C
      NFCT=0
      IKEY=KFUNC
      IF(IKAD(IKEY)/=IKAD(IKEY+1))THEN
       K=0
 140   READ(IUSC1,REC=IKAD(IKEY)+K,FMT='(49X,I10)',
     .      ERR=9990)NBC
       K=K+NBC+1
       NFCT=NFCT+1
       IF(IKAD(IKEY)+K/=IKAD(IKEY+1))GO TO 140
      ENDIF
C-----------------------
C     //
C-----------------------
      IKEY=KPROC
      IF(IKAD(IKEY)/=IKAD(IKEY+1))THEN
      ENDIF
C-----------------------
C     DYNAMIC RELAXATION
C-----------------------
      ISTAT = 0
      ISTATG = 0
      IKEY=KKEREL
c------- add T_START,T_STOP
      TST_START = ZERO
      TST_STOP = ZERO
      IF(IKAD(IKEY)/=IKAD(IKEY+1))THEN
       READ(IUSC1,REC=IKAD(IKEY),FMT='(7X,I5)',ERR=9990)K4
       CALL WRIUSC2(IKAD(IKEY)+1,1,KEY0(IKEY))
       IF(K4==1)THEN
         READ(IUSC2,*)ISTATG
         CALL WRIUSC2(IKAD(IKEY)+2,1,KEY0(IKEY))
       ENDIF
       READ(IUSC2,*,ERR=310,END=320)TST_START ,TST_STOP
        GOTO 320
 310    READ(IUSC2,*,ERR=320,END=320)TST_START
 320    CONTINUE
       BETA = ZERO
       PERIOD = ZERO
       ISTAT=2
      ENDIF
      IKEY=KDYREL
      IF(IKAD(IKEY)/=IKAD(IKEY+1))THEN
       IF(ISTAT==0)ISTAT=1
       CALL WRIUSC2(IKAD(IKEY)+1,1,KEY0(IKEY))
       READ(IUSC1,REC=IKAD(IKEY),FMT='(7X,I5)',ERR=9990)K4
       IF(K4==1)THEN
         READ(IUSC2,*)ISTATG
         CALL WRIUSC2(IKAD(IKEY)+2,1,KEY0(IKEY))
       ENDIF
       READ(IUSC2,*)BETA,PERIOD
      ENDIF
      ISTATG = -IABS(ISTATG)
      IF (BETA==ZERO) BETA=ONE
      IF(PERIOD/=ZERO)THEN
       BETATE = BETA / PERIOD
      ELSE
       BETATE = ZERO
      ENDIF
C
      IKEY=KADYREL
      IF(IKAD(IKEY)/=IKAD(IKEY+1))THEN
       ISTATG = 0
       FREQ_C =ZERO
       TST_START = ZERO
       TST_STOP = ZERO
       READ(IUSC1,REC=IKAD(IKEY),FMT='(7X,A)',ERR=9990)KEY2
       READ(IUSC1,REC=IKAD(IKEY)+1,FMT='(A)',ERR=520)KEY3
       CALL WRIUSC2(IKAD(IKEY)+1,1,KEY0(IKEY))
       IF (KEY2=='FREQ_') THEN
         READ(IUSC2,*)FREQ_C
         READ(IUSC1,REC=IKAD(IKEY)+2,FMT='(A)',ERR=520)KEY2
         CALL WRIUSC2(IKAD(IKEY)+2,1,KEY0(IKEY))
       ELSE
        READ(IUSC1,REC=IKAD(IKEY),FMT='(7X,I5)',ERR=9990)K4
        IF(K4==1)THEN
         READ(IUSC2,*)ISTATG
         READ(IUSC1,REC=IKAD(IKEY)+2,FMT='(A)',ERR=520)KEY2
         CALL WRIUSC2(IKAD(IKEY)+2,1,KEY0(IKEY))
        ENDIF
       END IF
       READ(IUSC2,*,ERR=510,END=520)TST_START ,TST_STOP
        GOTO 520
 510    READ(IUSC2,*,ERR=520,END=520)TST_START
 520    CONTINUE
       BETA=ONE
       PERIOD = ZERO
       BETATE = ZERO
       ISTAT=3
       ISTATG = -IABS(ISTATG)
      ENDIF
C-----------------------
C     ALE SUB-CYCLING
C-----------------------
      IALESUB = 0
      IKEY=KALESUB
      IF(IKAD(IKEY)/=IKAD(IKEY+1))THEN
       IALESUB=2
       CALL WRIUSC2(IKAD(IKEY)+1,1,KEY0(IKEY))
       READ(IUSC1,REC=IKAD(IKEY),FMT='(7X,I5)',ERR=9990)K4
       IF(K4==1)IALESUB=1
       READ(IUSC2,*)DTFSUB,DTMSUB
      ENDIF
      IF(DTFSUB==ZERO)DTFSUB=ZEP9
      IF (IALESUB==1) THEN
        CALL ANCMSG(MSGID=197,ANMODE=ANINFO)
        IERR=IERR+1
      ENDIF
C-----------------------
C     STOP/KILL
C-----------------------
C         /STOP/LSENSOR
C------------------------
      NSTOPSENS = 0
      LSTOPSENS(1:NSENSOR) = 0
C
      IKEY=KSTOP
      IF(IKAD(IKEY)/=IKAD(IKEY+1))THEN
        K=0
 1150   READ(IUSC1,REC=IKAD(IKEY)+K,FMT='(7X,A,37X,I10)',
     .      ERR=9990)KEY2,NBC
        K=K+1
        IF(KEY2=='LSENS')THEN    ! IF STOP BASED ON SENSOR ACTIVATION
c        First lign
          DO I=1,NBC-1
            READ(IUSC1,REC=IKAD(IKEY)+K,FMT='(A)',ERR=9990)CARTE
            CALL WRIUSC2(IKAD(IKEY)+K,1,KEY0(IKEY))
           K=K+1
            READ(IUSC2,*,ERR=9990,END=9990)
     .          (LSTOPSENS(N),N=1,NVAR(CARTE))
              NSTOPSENS=NSTOPSENS+NVAR(CARTE)
          ENDDO
c        Second lign
          IOK_READ = 0
          CALL WRIUSC2(IKAD(IKEY)+K,1,KEY0(IKEY))
          READ(IUSC2,*,ERR=148,END=148)
     .        NSANIM,NSTH,NSSTAT,NSOUTP,NSABF,NSH3D,NSDYNANIN
          IOK_READ = 1
c        Ensure Backward compatibility : without NSH3D
148       IF (IOK_READ == 0) THEN
            CALL WRIUSC2(IKAD(IKEY)+K,1,KEY0(IKEY))
            READ(IUSC2,*,ERR=9990,END=9990)
     .        NSANIM,NSTH,NSSTAT,NSOUTP,NSABF
          ENDIF
C
           K=K+1
        ELSE
          CALL WRIUSC2(IKAD(IKEY)+K,1,KEY0(IKEY))
          READ(IUSC2,*,ERR=150,END=149)
     .       DEMXS,DMTMXS,DMNMXS,NTH,NANIM,NERR_POSIT
          GOTO 151
 149      CONTINUE
          READ(IUSC2,*,ERR=150,END=150)
     .       DEMXS,DMTMXS,DMNMXS,NTH,NANIM
          GOTO 151
 150      CONTINUE
          CALL WRIUSC2(IKAD(IKEY)+K,1,KEY0(IKEY))
          READ(IUSC2,*)DEMXS,DMTMXS,DMNMXS
 151      CONTINUE
C
          K=K+NBC
        ENDIF
        IF(IKAD(IKEY)+K/=IKAD(IKEY+1))GO TO 1150
      ENDIF
      IF(DMTMXS==ZERO)DMTMXS=EP30
      IF(DMNMXS==ZERO)DMNMXS=EP30
      IF(DEMXS==ZERO) DEMXS =EP30
      IKEY=KKILL
      IF(IKAD(IKEY)/=IKAD(IKEY+1))THEN
        CALL WRIUSC2(IKAD(IKEY)+1,1,KEY0(IKEY))
        READ(IUSC2,*,ERR=155,END=154)
     .       DEMXK,DMTMXK,DMNMXK,NTH,NANIM,NERR_POSIT
        GOTO 156
 154    CONTINUE
        READ(IUSC2,*,ERR=155,END=155)
     .       DEMXK,DMTMXK,DMNMXK,NTH,NANIM
        GOTO 156
 155    CONTINUE
        CALL WRIUSC2(IKAD(IKEY)+1,1,KEY0(IKEY))
        READ(IUSC2,*)DEMXK,DMTMXK,DMNMXK
 156    CONTINUE
      ENDIF
      IF(DMTMXK==ZERO)DMTMXK=EP20
      IF(DMNMXK==ZERO)DMNMXK=EP20
      IF(DEMXK==ZERO) DEMXK =EP20
C========================================================================
C--------------------
C     CONTROL CARD 1
C--------------------
      IF (MCHECK == 0) THEN
      IKEY=KRUN
      IRUNN=1
      IRUNR=0
      CHRUN0='  '
      IF(IKAD(IKEY)/=IKAD(IKEY+1))THEN
       IRUNN=0
       READ(IUSC1,REC=IKAD(IKEY),FMT='(7X,A4,2X,I5,1X,A1)',
     .                           ERR=9990)CH4,IRUNN,CHRUN0(2:2)
       IF(CHRUN0(2:2)/=' ')CHRUN0(1:1)='_'
       CALL WRIUSC2(IKAD(IKEY)+1,1,KEY0(IKEY))
       READ(IUSC2,*)TSTOP0
      ENDIF

      IF (GOT_INPUT == 1) THEN
C RUNN is -input run number,
C if RUNN = 0 (common starter / engine input file) RUNN is set to 1 in Engine.
        IF (RUNN==0 ) RUNN=1
C IRUNN is the run number read from input.
C If IRUNN was set in input and is different from -input run number,
C than we print out a message.
        IF (IRUNN /= 0)THEN
          IF (IRUNN /= RUNN)THEN
           WRITE(ISTDO,'(A,A)') ' ** WARNING ** : RUN NUMBER FOUND IN ',
     *               '/RUN CARD FROM INPUT FILE'
            WRITE(ISTDO,'(A)')   '                 RUN NUMBER IGNORED '
          ENDIF
        ENDIF

        IRUNN = RUNN
        IF (IRUNN == 0) IRUNN = 1
      ENDIF
      IF(ROOTLEN==0) THEN
       CALL ANCMSG(MSGID=194,ANMODE=ANINFO)
       IERR=IERR+1
      ENDIF
      IF(IRUNN<=0) THEN
       CALL ANCMSG(MSGID=195,ANMODE=ANINFO)
       IERR=IERR+1
       IRUNN=1
      ENDIF
      ENDIF ! MCHECK == 0

      IKEY=KTITLE
      IF(IKAD(IKEY)/=IKAD(IKEY+1))THEN
       READ(IUSC1,REC=IKAD(IKEY)+1,FMT='(A)',ERR=9990)TITLE
       IF(TITLE(1:1)=='/')TITLE=' '
      ELSE
       TITLE=' '
      ENDIF
      WRITE(IIN,'(2A,I2.2,A)')ROOTNAM(1:ROOTLEN),'D',IRUNN,TITLE
      WRITE(CHRUN,'(I4.4)')IRUNN
C-----------------------
C  OPTION /QEPH/V4 pour re-emuler le calcul de l'hourglass en flexion
C  en version 44
C  par defaut : calcul plus robuste de l'hourglass en flexion
C                             (cas des elements tres gauches)
C--------------------------------------------------------
      IKEY   = KQEPH
      IQEPHV5=11
      IF(IKAD(IKEY)/=IKAD(IKEY+1))THEN
         READ(IUSC1,REC=IKAD(IKEY),FMT='(7X,A)',
     .                           ERR=9990)KEY2
         IF(KEY2(1:2)=='V4') THEN
          IQEPHV5=0
         ELSEIF(KEY2(1:3)=='V10') THEN
          IQEPHV5=10
         END IF
      END IF
C-----------------------
C  OPTION /SHVER/V51 pour revenir aux anciens vers sans correction
C  par defaut : Correction 2nd order rigid rotation pour QEPH,QBAT,DKT18,S3N6
C--------------------------------------------------------
      IKEY   = KSHVER
      ISHV51 = 52
      IF(IKAD(IKEY)/=IKAD(IKEY+1))THEN
         READ(IUSC1,REC=IKAD(IKEY),FMT='(7X,A)',
     .                           ERR=9990)KEY2
         IF(KEY2(1:3)=='V51')ISHV51=51
      END IF
C-----------------------
C  OPTION /NEGVOL/keyword SMSTR: INEG_V=1;STOP: INEG_V=0;
C--------------------------------------------------------
      IKEY   = KNEGVOL
      INEG_V = 1
      IF(IKAD(IKEY)/=IKAD(IKEY+1))THEN
         READ(IUSC1,REC=IKAD(IKEY),FMT='(7X,A)',
     .                           ERR=9990)KEY2
         IF(KEY2(1:4)=='STOP') INEG_V = 0
      END IF
C-----------------------
C  OPTION /DTSDE pour dt for degenerated solid element
C----change to default; to return back use /DTSDE/OFF
C----reput /DTSDE/OFF as default after instability issue; re-use /DTSDE if needed
C--------------------------------------------------------
      IKEY   = KDTSDE
      IDTS6 = 0
      IF(IKAD(IKEY)/=IKAD(IKEY+1))THEN
          IDTS6=1
         READ(IUSC1,REC=IKAD(IKEY),FMT='(7X,A)',
     .                           ERR=9990)KEY2
         IF(KEY2(1:3)=='OFF') IDTS6 = 0
      END IF
C--------------------
C     CONTROL CARD 2 ET 2BIS
C--------------------
      IKEY=KDT
      DTFAC0=ZERO
      DTMIN0=ZERO
      INTDT = 0
      IDTGR(11)  = 0
C      KDTINT=1
C     temporaire :
      KDTINT=0
      KDTSMSTR=1
      NODSUBDT=0
C
      TOL_SMS= ZERO
      NCPRISMS= 0
      M_VS_SMS=0
C
      IDTMINS_INT= 0
      DTFACS_INT = ZERO
      DTMINS_INT = ZERO
      IDT_THERM = 0
C
C
      IF(IKAD(IKEY)/=IKAD(IKEY+1))THEN
       K=0
 160   READ(IUSC1,REC=IKAD(IKEY)+K,FMT='(7X,A)',ERR=9990)KEY2
       IF(KEY2(1:4)=='NODA')THEN
         READ(IUSC1,REC=IKAD(IKEY)+K,FMT='(7X,A,1X,A)', ERR=9990)KEY2,KEY3
         IF(KEY3(1:4)/='STOP' .AND. KEY3(1:3)/='DEL'   .AND.
     .      KEY3(1:3)/='CST'  .AND. KEY3(1:3)/='SET'   .AND.
     .      KEY3(1:4)/='KILL' .AND. KEY3(1:3)/='LAG'   .AND.
     .      KEY3(1:3)/='AMS'  .AND. KEY3(1:3)/='ALE'   )THEN     !ALEON hidden option to keep compatibility of DT NODA * with ALE
           READ(IUSC1,REC=IKAD(IKEY)+K,FMT='(7X,A,1X,I5)',ERR=9990)KEY2,K4
         ELSEIF(KEY3(1:3)=='DEL')THEN
           CALL ANCMSG(MSGID=246,ANMODE=ANINFO,C1='/DT/NODA/DEL')
           IERR=IERR+1
         ELSE
           READ(IUSC1,REC=IKAD(IKEY)+K,FMT='(7X,A,1X,A,1X,I5)',ERR=9990)KEY2,KEY3,K4
        ENDIF
       ELSEIF(KEY2=='AMS')THEN
         READ(IUSC1,REC=IKAD(IKEY)+K,FMT='(7X,A,1X,I5)',ERR=9990)KEY2,K4
       ELSEIF(KEY2=='CST_A')THEN
         ! /DT/CST_AMS -> hybride /DT/NODA/CST + /DT/AMS
         READ(IUSC1,REC=IKAD(IKEY)+K,FMT='(7X,A,1X,I5)',ERR=9990)KEY2,K4
       ELSEIF(KEY2=='GLOB')THEN
         READ(IUSC1,REC=IKAD(IKEY)+K,FMT='(7X,A,1X,I5,1X,A)',ERR=9990)KEY2,K4,KEY5
       ELSEIF(KEY2(1:4)=='NODE')THEN
           CALL ANCMSG(MSGID=246,ANMODE=ANINFO,C1='/DT/NODE')
           IERR=IERR+1
       ELSE
           READ(IUSC1,REC=IKAD(IKEY)+K,FMT='(7X,A,1X,A,1X,I5)',ERR=9990)KEY2,KEY3,K4
       END IF

       K=K+1
       IF(KEY2=='INTER'.AND.KEY3(1:2)=='V5')THEN
         KDTINT=1
       ELSEIF(KEY2=='INTER'.AND.KEY3(1:2)=='V4')THEN
         KDTINT=0
       ELSEIF(KEY2=='SMSTR'.AND.KEY3(1:2)=='V4') THEN
         KDTSMSTR=0
       ELSE IF(KEY2=='THERM')THEN
          IDT_THERM = 1
          DTFACTHERM = ZEP9
          IF(IKAD(IKEY)+K/=IKAD(IKEY+1))THEN
             READ(IUSC1,REC=IKAD(IKEY)+K,FMT='(1X,A)',ERR=9990)KEY4
             IF(KEY4(1:2)/='DT') THEN
               CALL WRIUSC2(IKAD(IKEY)+K,1,KEY0(IKEY))
               READ(IUSC2,*,ERR=9990,END=9990)DTFACTHERM
               IF (DTFACTHERM == ZERO) DTFACA = ZEP9
               K=K+1
             ENDIF
          ENDIF
       ELSE IF(KEY2(1:4)=='IGEO')THEN
        CALL WRIUSC2(IKAD(IKEY)+K,1,KEY0(IKEY))
        READ(IUSC2,*,ERR=9990,END=9990)DTFACA,DTMINA
        K=K+1
        IF(KEY3(1:4)=='ELEM')THEN
           IDT = 1
        ELSEIF(KEY3(1:4)=='LCAR')THEN
           IDT = 2
        ELSE
           IDT = 0
        ENDIF
        DTFAC1(101) = DTFACA
        DTMIN1(101) = DTMINA
        IDTMIN(101) = IDT

       ELSEIF(KEY2(1:4)=='GLOB')THEN
         WRITE(6,*) "/DT/GLOB is a deprecated feature"
         CALL ARRET(5)
       ELSE
        IF(KEY3(1:3)=='ALE')THEN
          CALL WRIUSC2(IKAD(IKEY)+K,1,KEY0(IKEY))
          IFLAG = 0
          I_DT_NODA_ALE_ON_KEY = 0
          READ(IUSC2,*,ERR=9990,END=9990)IFLAG
          IF(IFLAG==1)I_DT_NODA_ALE_ON_KEY = 1
          K = K + 1
          GOTO 2003
        ELSEIF (KEY2(1:5)=='CST_A') THEN
          DTFAC_AMS = ZERO
          CALL WRIUSC2(IKAD(IKEY)+K,1,KEY0(IKEY))
          READ(IUSC2,*,ERR=9001,END=9001) DTFACA,DTMINA,DTFAC_AMS
          GOTO 9002
 9001     CONTINUE
          CALL WRIUSC2(IKAD(IKEY)+K,1,KEY0(IKEY))
          READ(IUSC2,*,ERR=9990,END=9990) DTFACA,DTMINA
 9002     CONTINUE
          IF (DTFACA == ZERO) DTFACA = ZEP9
          IF (DTFAC_AMS == ZERO) DTFAC_AMS = ZEP67
        ELSEIF(KEY2(1:5) == 'FVMBA')THEN
          ! ---/DT/FVMBAG/. ---                                                 
          !     dtfaca dtmina lambda l_type
          !     fvbag_id
          IERR1=0                                                               
          CALL WRIUSC2(IKAD(IKEY)+K,1,KEY0(IKEY))                               
          READ(IUSC2,'(A)',ERR=9990,END=9990)LINE                                                 
          READ(LINE,*,iostat=IERR1)DTFACA,DTMINA,LAMBDA,L_TYPE                  
          IF(IERR1 /= 0)THEN                                                    
            !l_type left blank (default 0)                                      
            L_TYPE=ZERO                                                         
            IERR1=0                                                             
            READ(LINE,*,iostat=IERR1)DTFACA,DTMINA,LAMBDA                       
            IF(IERR1 /= 0)THEN                                                  
              !param 3 also left blank (default 0.0)                            
              LAMBDA=ZERO                                                       
              IERR1=0                                                           
              ! this is minimanl input, goto to error message if reading fails  
              READ(LINE,*,ERR=9990,END=9990)DTFACA,DTMINA                       
            ENDIF                                                               
          ENDIF                                                                 
          ID_BAG = 0                                                            
          IF(KEY3(1:1)=='2')THEN                                                
            K=K+1                                                               
            CALL WRIUSC2(IKAD(IKEY)+K,1,KEY0(IKEY))
            READ(IUSC2,'(A)',ERR=9990,END=9990)LINE                                              
            READ(LINE,*,ERR=9990,END=9990)ID_BAG 
            IF(ID_BAG <= 0)THEN
                CALL ANCMSG(MSGID=299,ANMODE=ANINFO,I1=ID_BAG)
                CALL ARRET(0)         
            ENDIF                            
          ENDIF                                                                 
        ELSE
          ! --- /DT/NODA/CST, and others ... ---
          !     dtfaca dtmina percent_addmass  
          CALL WRIUSC2(IKAD(IKEY)+K,1,KEY0(IKEY))
          READ(IUSC2,*,ERR=9003,END=9003)DTFACA,DTMINA,PERCENT_ADDMASS
          ! Read of target % of added mass
          IF (PERCENT_ADDMASS > ZERO) IDT_PERCENT_ADDMASS = 1
          GOTO 9004
 9003     CONTINUE
          CALL WRIUSC2(IKAD(IKEY)+K,1,KEY0(IKEY))
          READ(IUSC2,*,ERR=9990,END=9990) DTFACA,DTMINA
 9004     CONTINUE
        ENDIF
        K=K+1
C
        IF(KEY3(1:4)=='STOP')THEN
           IDT = 1
           IF((KEY2(1:4)=='NODA').AND.(IDT_PERCENT_ADDMASS == 1)) THEN
C--       case DT/NODA/STOP + % of added mass - IDTMIN set to 8
             IDT = 8
             IDT_PERCENT_ADDMASS = 2
             DT_STOP_PERCENT_ADDMASS = DTMINA
           ENDIF
        ELSEIF(KEY3(1:3)=='DEL')THEN
           IDT = 2
        ELSEIF(KEY3(1:3)=='CST')THEN
           IDT = 3
           IF((KEY2(1:4)=='NODA'.OR.KEY2(1:5)=='INTER').AND.KEY3(1:4)/='CST1')IDT=8
        ELSEIF(KEY3(1:3)=='SET')THEN
           IDT = 4
        ELSEIF(KEY3(1:4)=='KILL')THEN
           IDT = 5
        ELSEIF(KEY3(1:3)=='LAG')THEN
           IDT = 6
        ELSEIF(KEY3(1:3)=='AMS')THEN
           IDT = 7
        ELSEIF(KEY3(1:3)=='COD')THEN
           IDT = 3
        ELSE
           IDT = 0
        ENDIF
C
        IF(KEY2=='BRICK')THEN
         DTFAC1(1) = DTFACA
         DTMIN1(1) = DTMINA
         IDTMIN(1) = IDT
         MIN_ASPECT = ZERO
         MIN_DEFV = ZERO
         IF(K4==1)THEN
           CALL WRIUSC2(IKAD(IKEY)+K,1,KEY0(IKEY))
           READ(IUSC2,*,ERR=1001,END=1001)MIN_ASPECT,MIN_DEFV
           GO TO 1002
 1001      CONTINUE
           CALL WRIUSC2(IKAD(IKEY)+K,1,KEY0(IKEY))
           READ(IUSC2,*,ERR=9990,END=9990)MIN_ASPECT
 1002      CONTINUE
           K=K+1
         ENDIF
        ELSEIF(KEY2(1:4)=='QUAD')THEN
         DTFAC1(2) = DTFACA
         DTMIN1(2) = DTMINA
         IDTMIN(2) = IDT
        ELSEIF(KEY2=='SHELL')THEN
         DTFAC1(3) = DTFACA
         DTMIN1(3) = DTMINA
         IDTMIN(3) = IDT
        ELSEIF(KEY2=='SH_3N'.OR.KEY2(1:4)=='SH3N')THEN
         DTFAC1(7) = DTFACA
         DTMIN1(7) = DTMINA
         IDTMIN(7) = IDT
        ELSEIF(KEY2=='TRUSS')THEN
         DTFAC1(4) = DTFACA
         DTMIN1(4) = DTMINA
         IDTMIN(4) = IDT
        ELSEIF(KEY2(1:4)=='BEAM')THEN
         DTFAC1(5) = DTFACA
         DTMIN1(5) = DTMINA
         IDTMIN(5) = IDT
        ELSEIF(KEY2=='SPRIN')THEN
         DTFAC1(6) = DTFACA
         DTMIN1(6) = DTMINA
         IDTMIN(6) = IDT
        ELSEIF(KEY2=='AIRBA')THEN
         DTFAC1(9) = DTFACA
         DTMIN1(9) = DTMINA
         IDTMIN(9) = IDT
        ELSEIF(KEY2=='INTER')THEN
         IF(INTDT==0.and.(IDT/=3.and.IDT/=8.and.IDT/=7))THEN
C           si /DT/INTER/DEL lu + tard ce qui suit sera ecrase
            DTFAC1(10) = DTFACA
            DTMIN1(10) = DTMINA
            IDTMIN(10) = IDT
         ENDIF
         IF(IDT==2)INTDT=1
         IF(NODADT==0.AND.(IDT==3.OR.IDT==8))THEN
C           si /DT/NODA lu + tard ce qui suit sera ecrase
            DTFAC1(11) = DTFACA
            DTMIN1(11) = DTMINA
            IDTMIN(11) = IDT
         ELSEIF(IDT==7)THEN
            DTFACS_INT = DTFACA
            DTMINS_INT = DTMINA
            IDTMINS_INT= 1
C           si /DT/AMS lu + tard ce qui suit sera ecrase
            IF(K4>=1)THEN
              CALL WRIUSC2(IKAD(IKEY)+K,1,KEY0(IKEY))
              READ(IUSC2,*,ERR=3001,END=3001)TOL_SMS,M_VS_SMS
              GO TO 3002
 3001         CONTINUE
              CALL WRIUSC2(IKAD(IKEY)+K,1,KEY0(IKEY))
              READ(IUSC2,*,ERR=9990,END=9990)TOL_SMS
 3002         CONTINUE
              K=K+1
            ENDIF
            IF(K4>=2)THEN
              CALL WRIUSC2(IKAD(IKEY)+K,1,KEY0(IKEY))
              READ(IUSC2,*,ERR=9990,END=9990)NSMSPCG,NCPRISMS
              K=K+1
            ENDIF
         ENDIF
        ELSEIF(KEY2(1:4)=='NODA' .AND. KEY3(1:3)/='ALE')THEN
         IF(IDT/=7)THEN
           DTFAC1(11) = DTFACA
           DTMIN1(11) = DTMINA
           IDTMIN(11) = IDT
           IDTGR(11) = 0
           IF ((IDT == 8).OR.(IDT == 3)) DT_CTRL_COMPATIBILITY(1) = 1
           IF(K4==1)THEN
             CALL WRIUSC2(IKAD(IKEY)+K,1,KEY0(IKEY))
             READ(IUSC2,*,ERR=9990,END=9990)IDTGR(11)
             IDTGR(11) = -IDTGR(11)
             K=K+1
           ENDIF
         ELSE
           DTFACS = DTFACA
           DTMINS = DTMINA
           IDTMINS= 1
           TOL_SMS = ZERO
           NSMSPCG = 0
           NCPRISMS= 0
           IF(K4>=1)THEN
             CALL WRIUSC2(IKAD(IKEY)+K,1,KEY0(IKEY))
             READ(IUSC2,*,ERR=100,END=100)IDTGRX,TOL_SMS
             IDTGRS = -IDTGRX
             GO TO 101
 100         CONTINUE
             CALL WRIUSC2(IKAD(IKEY)+K,1,KEY0(IKEY))
             READ(IUSC2,*,ERR=9990,END=9990)TOL_SMS
 101         CONTINUE
             K=K+1
           ENDIF
           IF(K4>=2)THEN
             CALL WRIUSC2(IKAD(IKEY)+K,1,KEY0(IKEY))
             READ(IUSC2,*,ERR=9990,END=9990)NSMSPCG,NCPRISMS
             K=K+1
           ENDIF
         END IF
         NODADT = 1
        ELSEIF(KEY2(1:5)=='SPCEL'.OR.KEY2(1:5)=='SPHCE')THEN
         DTFAC1(51) = DTFACA
         DTMIN1(51) = DTMINA
         IDTMIN(51) = IDT
        ELSEIF(KEY2(1:5)=='SHNOD')THEN
         DTFSUB = DTFACA
         DTMSUB = DTMINA
         NODSUBDT=1
         IF(IDT==3)NODSUBDT=2
        ELSEIF(KEY2(1:5)=='FVMBA') THEN
         IF(KEY3(1:1)=='0')THEN
           ID_DT_OPTION = 0
           DTFAC1(52) = DTFACA
           DTMIN1(52) = DTMINA    
           IDTMIN(52) = ID_DT_OPTION        
         ELSEIF(KEY3(1:1)=='1')THEN
           ID_DT_OPTION = 1
           DTFAC1(52) = DTFACA
           DTMIN1(52) = DTMINA    
           IDTMIN(52) = ID_DT_OPTION   
         ELSEIF(KEY3(1:1)=='2')THEN
           ID_DT_OPTION = 2
           !option requires an airbag_id.
         ELSE
           ID_DT_OPTION = 1
           DTFAC1(52) = DTFACA
           DTMIN1(52) = DTMINA 
           IDTMIN(52) = ID_DT_OPTION                      
         ENDIF

         LAMBDA = MAX(ZERO, LAMBDA)
         FVMBAG_L_TYPE = INT(L_TYPE)
         FVMBAG_L_TYPE = MIN(FVMBAG_L_TYPE,2) !input check:upper bound
         FVMBAG_L_TYPE = MAX(0,FVMBAG_L_TYPE) !input check:lower bound
         !storing in temporary data structure. It will be used to fill the final data structure when restart file is read (not yet allocated)         
         IF(ID_DT_OPTION == 0)NUM_OPT_DT_FVMBAG_0 = NUM_OPT_DT_FVMBAG_0 + 1
         IF(ID_DT_OPTION == 1)NUM_OPT_DT_FVMBAG_1 = NUM_OPT_DT_FVMBAG_1 + 1
         IF(ID_DT_OPTION == 2)NUM_OPT_DT_FVMBAG_2 = NUM_OPT_DT_FVMBAG_2 + 1
         IF(ID_DT_OPTION == 3)NUM_OPT_DT_FVMBAG_3 = NUM_OPT_DT_FVMBAG_3 + 1         
         NUM_OPT_DT_FVMBAG = NUM_OPT_DT_FVMBAG + 1
         FVMBAG_INPUT_OPTIONS(NUM_OPT_DT_FVMBAG)%CFL_COEF = DTFACA 
         FVMBAG_INPUT_OPTIONS(NUM_OPT_DT_FVMBAG)%DTMIN = DTMINA 
         FVMBAG_INPUT_OPTIONS(NUM_OPT_DT_FVMBAG)%LAMBDA = LAMBDA 
         FVMBAG_INPUT_OPTIONS(NUM_OPT_DT_FVMBAG)%ID_BAG = ID_BAG
         FVMBAG_INPUT_OPTIONS(NUM_OPT_DT_FVMBAG)%L_type = FVMBAG_L_TYPE
         FVMBAG_INPUT_OPTIONS(NUM_OPT_DT_FVMBAG)%ID_DT_OPTION = ID_DT_OPTION
         !Input check is done later in rdresb.F>FVRREST once all keywords are finished to be read & before deallocating (FVMBAG_INPUT_OPTIONS)
         
        ELSEIF(KEY2(1:3)=='AMS')THEN
          DT_CTRL_COMPATIBILITY(2) = 1
          DTFACS = DTFACA
          DTMINS = DTMINA
          IDTMINS = 2
          TOL_SMS = ZERO
          NSMSPCG = 0
          NCPRISMS= 0
C
          IF(K4>=1)THEN
            CALL WRIUSC2(IKAD(IKEY)+K,1,KEY0(IKEY))
              READ(IUSC2,*,ERR=2001,END=2001)TOL_SMS,M_VS_SMS
              GO TO 2002
 2001         CONTINUE
              CALL WRIUSC2(IKAD(IKEY)+K,1,KEY0(IKEY))
              READ(IUSC2,*,ERR=9990,END=9990)TOL_SMS
 2002         CONTINUE
            K=K+1
          ENDIF
          IF(K4>=2)THEN
            CALL WRIUSC2(IKAD(IKEY)+K,1,KEY0(IKEY))
            READ(IUSC2,*,ERR=9990,END=9990)NSMSPCG,NCPRISMS
            K=K+1
          ENDIF
        ELSEIF(KEY2(1:5)=='CST_A')THEN
C--    Cas hybride CST_AMS : /DT/NODA/CST + /DT/AMS
          DT_CTRL_COMPATIBILITY(3) = 1
C        Set of /DT/NODA/CST
          DTFAC1(11) = DTFACA
          DTMIN1(11) = DTMINA
          IDTMIN(11) = 8
          IDTGR(11) = 0
          NODADT = 1
C        Set of /DT/AMS
          DTFACS = DTFAC_AMS
          DTMINS = DTMINA
          IDTMINS = 2
          TOL_SMS = ZERO
          NSMSPCG = 0
          NCPRISMS= 0
C
          FLAG_CST_AMS = 1
C
          IF(K4>=1)THEN
            CALL WRIUSC2(IKAD(IKEY)+K,1,KEY0(IKEY))
              READ(IUSC2,*,ERR=2011,END=2011)TOL_SMS,M_VS_SMS
              GO TO 2012
 2011         CONTINUE
              CALL WRIUSC2(IKAD(IKEY)+K,1,KEY0(IKEY))
              READ(IUSC2,*,ERR=9990,END=9990)TOL_SMS
 2012         CONTINUE
            K=K+1
          ENDIF
          IF(K4>=2)THEN
            CALL WRIUSC2(IKAD(IKEY)+K,1,KEY0(IKEY))
            READ(IUSC2,*,ERR=9990,END=9990)NSMSPCG,NCPRISMS
            K=K+1
          ENDIF
        ELSEIF(KEY2(1:3)=='ALE'.OR. KEY2(1:4)=='EULER')THEN
         DTFAC1(102) = DTFACA
         DTMIN1(102) = DTMINA
         IDTMIN(102) = IDT
         If(IDT_ALE==0)IDT_ALE     = 1
        ELSE
         DTFAC0=DTFACA
         DTMIN0=DTMINA
         IDT_ALE     = -1
        ENDIF
       ENDIF
 2003  CONTINUE
       IF(IKAD(IKEY)+K/=IKAD(IKEY+1))GO TO 160
      ENDIF
C
C   Check of compatibility of time step control options - /DT/CST_AMS can not be used with /DT/AMS or /DT/NODA/CST
      IF (DT_CTRL_COMPATIBILITY(3) == 1) THEN
        IF ((DT_CTRL_COMPATIBILITY(1)+DT_CTRL_COMPATIBILITY(2)) > 0) THEN
          CALL ANCMSG(MSGID=279,ANMODE=ANINFO,
     .            C1=KEY0(IKEY))
          CALL ARRET(0)
        ENDIF
      ENDIF
C AMS is not compatible with thermal time step
      IF (IDT_THERM == 1.AND.IDTMINS/=0) THEN
          CALL ANCMSG(MSGID=301,ANMODE=ANINFO,
     .            C1=KEY0(IKEY))
          CALL ARRET(0)
      ENDIF
C
      IKEY=KDT1
      IDT1SH = 0
      IDT1SOL= 0
      IF(IKAD(IKEY)/=IKAD(IKEY+1))THEN
       K=0
 161   READ(IUSC1,REC=IKAD(IKEY)+K,FMT='(7X,A,1X,A,1X,I5)',
     .                              ERR=9990)KEY2,KEY3,K4
c       IF(KEY2=='BRICK')THEN
c         READ(IUSC1,REC=IKAD(IKEY)+K,FMT='(7X,A,1X,A,1X,I5)',ERR=9990)KEY2,KEY3,K4
c       ENDIF

       K=K+1
       CALL WRIUSC2(IKAD(IKEY)+K,1,KEY0(IKEY))
       READ(IUSC2,*,ERR=9990,END=9990)DTFACA,DTMINA

       K=K+1
       IF(KEY3(1:4)=='STOP')THEN
           IDT = 1
       ELSEIF(KEY3(1:3)=='DEL')THEN
           IDT = 2
       ELSEIF(KEY3(1:3)=='CST')THEN
           IDT = 3
       ELSEIF(KEY3(1:4)=='KILL')THEN
           IDT = 5
       ELSE
           IDT = 0
       ENDIF

       IF(KEY2=='SHELL')THEN
         IDT1SH = 1
         DTFAC1(3) = DTFACA
         DTMIN1(3) = DTMINA
         IDTMIN(3) = IDT
       ELSEIF(KEY2=='BRICK')THEN
         IDT1SOL= 1
         DTFAC1(1) = DTFACA
         DTMIN1(1) = DTMINA
         IDTMIN(1) = IDT
            MIN_ASPECT = ZERO
            MIN_DEFV = ZERO
            IF(K4==1)THEN
              CALL WRIUSC2(IKAD(IKEY)+K,1,KEY0(IKEY))
              READ(IUSC2,*,ERR=1101,END=1101)MIN_ASPECT,MIN_DEFV
              GO TO 1102
 1101         CONTINUE
              CALL WRIUSC2(IKAD(IKEY)+K,1,KEY0(IKEY))
              READ(IUSC2,*,ERR=9990,END=9990)MIN_ASPECT
 1102         CONTINUE
              K=K+1
            ENDIF
       ELSE
         IDT1SH = 1
         IDT1SOL= 1
         DTFAC0=DTFACA
         DTMIN0=DTMINA
       ENDIF
       IF(IKAD(IKEY)+K/=IKAD(IKEY+1))GO TO 161
      ENDIF
C
      ITTYP = 0
      IKEY=KTFILE
      IF(IKAD(IKEY)/=IKAD(IKEY+1))THEN
       READ(IUSC1,REC=IKAD(IKEY),FMT='(7X,I5)',ERR=9990)ITTYP
       CALL WRIUSC2(IKAD(IKEY)+1,1,KEY0(IKEY))
       READ(IUSC2,*,ERR=9990,END=9990)DTHIS0
      ELSE
       ITTYP=0
       DTHIS0=EP30
      ENDIF
       IKEY=KATFIL
      IF(IKAD(IKEY)/=IKAD(IKEY+1))THEN
       READ(IUSC1,REC=IKAD(IKEY),FMT='(7X,I5)',ERR=9990)AFORM(1)
       CALL WRIUSC2(IKAD(IKEY)+1,1,KEY0(IKEY))
       READ(IUSC2,*,ERR=9990,END=9990)DTHIS01(1)
      ELSE
       DTHIS01(1)=EP30
       THIS1(1)= EP30
      ENDIF
      IKEY=KBTFIL
      IF(IKAD(IKEY)/=IKAD(IKEY+1))THEN
       READ(IUSC1,REC=IKAD(IKEY),FMT='(7X,I5)',ERR=9990)AFORM(2)
       CALL WRIUSC2(IKAD(IKEY)+1,1,KEY0(IKEY))
       READ(IUSC2,*,ERR=9990,END=9990)DTHIS01(2)
      ELSE
       DTHIS01(2)=EP30
       THIS1(2)=EP30
      ENDIF
      IKEY=KCTFIL
      IF(IKAD(IKEY)/=IKAD(IKEY+1))THEN
       READ(IUSC1,REC=IKAD(IKEY),FMT='(7X,I5)',ERR=9990)AFORM(3)
       CALL WRIUSC2(IKAD(IKEY)+1,1,KEY0(IKEY))
       READ(IUSC2,*,ERR=9990,END=9990)DTHIS01(3)
      ELSE
       DTHIS01(3)=EP30
       THIS1(3)=EP30
      ENDIF
      IKEY=KDTFIL
      IF(IKAD(IKEY)/=IKAD(IKEY+1))THEN
       READ(IUSC1,REC=IKAD(IKEY),FMT='(7X,I5)',ERR=9990)AFORM(4)
       CALL WRIUSC2(IKAD(IKEY)+1,1,KEY0(IKEY))
       READ(IUSC2,*,ERR=9990,END=9990)DTHIS01(4)
      ELSE
       DTHIS01(4)=EP30
       THIS1(4)=EP30
      ENDIF
      IKEY=KETFIL
      IF(IKAD(IKEY)/=IKAD(IKEY+1))THEN
       READ(IUSC1,REC=IKAD(IKEY),FMT='(7X,I5)',ERR=9990)AFORM(5)
       CALL WRIUSC2(IKAD(IKEY)+1,1,KEY0(IKEY))
       READ(IUSC2,*,ERR=9990,END=9990)DTHIS01(5)
      ELSE
       DTHIS01(5)=EP30
       THIS1(5)=EP30
      ENDIF
      IKEY=KFTFIL
      IF(IKAD(IKEY)/=IKAD(IKEY+1))THEN
       READ(IUSC1,REC=IKAD(IKEY),FMT='(7X,I5)',ERR=9990)AFORM(6)
       CALL WRIUSC2(IKAD(IKEY)+1,1,KEY0(IKEY))
       READ(IUSC2,*,ERR=9990,END=9990)DTHIS01(6)
      ELSE
       DTHIS01(6)=EP30
       THIS1(6)=EP30
      ENDIF
      IKEY=KGTFIL
      IF(IKAD(IKEY)/=IKAD(IKEY+1))THEN
       READ(IUSC1,REC=IKAD(IKEY),FMT='(7X,I5)',ERR=9990)AFORM(7)
       CALL WRIUSC2(IKAD(IKEY)+1,1,KEY0(IKEY))
       READ(IUSC2,*,ERR=9990,END=9990)DTHIS01(7)
      ELSE
       DTHIS01(7)=EP30
       THIS1(7)=EP30
      ENDIF
      IKEY=KHTFIL
      IF(IKAD(IKEY)/=IKAD(IKEY+1))THEN
       READ(IUSC1,REC=IKAD(IKEY),FMT='(7X,I5)',ERR=9990)AFORM(8)
       CALL WRIUSC2(IKAD(IKEY)+1,1,KEY0(IKEY))
       READ(IUSC2,*,ERR=9990,END=9990)DTHIS01(8)
      ELSE
       DTHIS01(8)=EP30
       THIS1(8)=EP30
      ENDIF
      IKEY=KITFIL
      IF(IKAD(IKEY)/=IKAD(IKEY+1))THEN
       READ(IUSC1,REC=IKAD(IKEY),FMT='(7X,I5)',ERR=9990)AFORM(9)
       CALL WRIUSC2(IKAD(IKEY)+1,1,KEY0(IKEY))
       READ(IUSC2,*,ERR=9990,END=9990)DTHIS01(9)
      ELSE
       DTHIS01(9)=EP30
       THIS1(9)=EP30
      ENDIF
      IKEY=KTH
      TH_VERS=40
      IF(IKAD(IKEY)/=IKAD(IKEY+1))THEN
       K=0
 1160  READ(IUSC1,REC=IKAD(IKEY)+K,
     .      FMT='(7X,A,1X,A,1X,A,1X,A,19X,I10)',
     .      ERR=9990)KEY2,KEY3,KEY4,KEY5,NBC
       K=K+1
       IF(KEY2(1:4)=='VERS')THEN
         READ(KEY3,'(I4)')TH_VERS
       ELSEIF(KEY2(1:5)=='TITLE')THEN
         TH_TITLES = 1
       ELSE
         GOTO 9990
       ENDIF
       IF (TH_VERS<40) THEN
          CALL ANCMSG(MSGID=198,ANMODE=ANINFO)
        IERR=IERR+1
       ENDIF
       K=K+NBC
       IF(IKAD(IKEY)+K/=IKAD(IKEY+1))GO TO 1160
      ENDIF
C
      IKEY=KPRINT
      IF(IKAD(IKEY)/=IKAD(IKEY+1))THEN
       READ(IUSC1,REC=IKAD(IKEY),FMT='(7X,I5,X,I5)',ERR=9990)NCPRI0,NCPRI_HEAD0
      ELSE
       NCPRI0=-100
       NCPRI_HEAD0=0
      ENDIF
      RF_CHECK = 0
      IKEY=KRFILE
      IF(IKAD(IKEY)/=IKAD(IKEY+1))THEN
        READ(IUSC1,REC=IKAD(IKEY),FMT='(7X,A)',ERR=9990) KEY2
        !    /RFILE/OFF or -norst command line --> avoid to write restart files during a run
        !    default = write restart files
        IF(KEY2(1:3)=='OFF') THEN
            RESTART_FILE = 0
            MULTIREST=0
            NCRST=0
            IRLTYP=0
            IRETYP=0
        ELSEIF(RESTART_FILE==0) THEN
        !   /RFILE/NCYCLE is written in the *1.rad but the user
        !   uses the -NORST command line
            MULTIREST=0
            NCRST=0
            IRLTYP=0
            IRETYP=0
        ELSE
        !    /RFILE/
        !    NCYCLE
            RESTART_FILE = 1
            READ(IUSC1,REC=IKAD(IKEY),FMT='(7X,I5)',ERR=9990)MULTIREST
            CALL WRIUSC2(IKAD(IKEY)+1,1,KEY0(IKEY))
            READ(IUSC2,*,ERR=9990,END=9990)NCRST
            IRLTYP=0
            IRETYP=0
            MULTIREST=MIN(MULTIREST,26)
            RF_CHECK = 1
        ENDIF
      ELSE
        MULTIREST=0
        NCRST=0
        IRLTYP=0
        IRETYP=0
      ENDIF

c      WRITE(IIN,'(65X,I5,1X,3I1)')INVERS,ITTYP,IRLTYP,IRETYP
      WRITE(IIN,'(1P4E16.9,I8,I10,2I8)')
     .    TSTOP0,DTHIS0,DTFAC0,DTMIN0,NCPRI0,NCRST,MULTIREST,NCPRI_HEAD0
C     .    TSTOP,DTHIS0,DTHIS01,DTFAC0,DTMIN0,NCPRI,NCRST,MULTIREST
       WRITE(IIN,'(1P5E16.9)')(DTHIS01(I), I=1,5)
       WRITE(IIN,'(1P4E16.9)')(DTHIS01(I+5), I=1,4)

c     on met la valeur de ITTYP a 4 (Binary IEEE 32 bits) par defaut
      IF(ITTYP==0) ITTYP=4

      ITFORM=ITTYP-1
      DO I=1,9
        IF (AFORM(I)==0) AFORM(I)=4
        AFORM(I)=AFORM(I)-1
      END DO
C
C     IRFE=IRFORM/3
C     IRFL=IRFORM-3*IRFE
      IRFE=IRFORM/5
      IRFL=IRFORM-5*IRFE
c      IF(IRETYP/=0) IRFE=IRETYP-1
c      IF(IRLTYP/=0) IRFL=IRLTYP-1
C     IRFORM=3*IRFE+IRFL
      IRFORM=5*IRFE+IRFL
C--------------------
C     CONTROL CARD 5 '/ALE/GRID/*'
C--------------------
      IKEY=KALE
      ALP=ZERO
      GAM=ZERO
      A1=ZERO
      A2=ZERO
      A3=ZERO
      VM=ZERO
      IAL=-1
      NWALE_ENGINE=-1
      IF(IKAD(IKEY)/=IKAD(IKEY+1))THEN
       READ(IUSC1,REC=IKAD(IKEY),FMT='(7X,A32)',ERR=9990)StrIAL
       IF(StrIAL(1:4)=='GRID')StrIAL(1:5)=StrIAL(7:11)          !Shift String "/ALE  /GRID /*****    => StrIAL = '*****' , len=5
       CALL WRIUSC2(IKAD(IKEY)+1,1,KEY0(IKEY))                    !                     12345678901
       !---------------------------------------!
       !    '/ALE/GRID/DONEA'                  !
       !---------------------------------------!
       IF(StrIAL(1:5)=='DONEA'.OR.StrIAL(1:1)=='0')THEN
        IAL=0
        READ(IUSC2,*,ERR=9990,END=9990)ALP,GAM,A1,A2,A3,VM
       !---------------------------------------!
       !    '/ALE/GRID/DISP'                   !
       !---------------------------------------!
       ELSEIF(StrIAL(1:4)=='DISP'.OR.StrIAL(1:1)=='1')THEN
        IAL=1
        READ(IUSC2,*,ERR=1161,END=1161)ALP,A1,A2,A3,VM  !format kept for backward compatibility
        IF(ALP/=ZERO.OR.VM/=ZERO
     .     .OR.A1/=ZERO.OR.A2/=ZERO.OR.A3/=ZERO)GOTO 1162
 1161   REWIND(IUSC2)
        A1=ZERO; A2=ZERO; A3=ZERO;
        READ(IUSC2,*,ERR=9990,END=9990)ALP,VM           !new format scale factor removed (hidden flags)
 1162   CONTINUE
        GAM=ZERO
       !---------------------------------------!
       !    '/ALE/GRID/SPRING'                 !
       !---------------------------------------!
       ELSEIF(StrIAL(1:5)=='SPRIN'.OR.StrIAL(1:1)=='2')THEN
        IAL=2
        READ(IUSC2,*,ERR=9990,END=9990)ALP,GAM,A1,A2,VM
        A3=ZERO
       !---------------------------------------!
       !    '/ALE/GRID/ZERO'                   !
       !---------------------------------------!
       ELSEIF(StrIAL(1:4)=='ZERO'.OR.StrIAL(1:1)=='3')THEN
        IAL=3
        !READ(IUSC2,*,ERR=9990,END=9990)ALP
       !---------------------------------------!
       !    '/ALE/GRID/STANDARD'               !
       !---------------------------------------!
       ELSEIF(StrIAL(1:5)=='STAND'.OR.StrIAL(1:1)=='4')THEN
        IAL=4
        READ(IUSC2,*,ERR=9990,END=9990)ALP,GAM,A1,A2
        A3=ZERO
        VM=ZERO
       !---------------------------------------!
       !    '/ALE/GRID/LAPLACIAN               !
       !---------------------------------------!
       ELSEIF(StrIAL(1:5)=='LAPLA'.OR.StrIAL(1:1)=='5')THEN
        IAL=1
        READ(IUSC2,*,ERR=9990,END=9990)ALP,A1,A2
        IF(ALP==ZERO)ALP=ONE
        IF(NINT(A1)<=ZERO)A1=ONE
        IF(A2/=ONE .AND. A2/=TWO)A2=ONE
       ENDIF
       !defining new grid formulation
       IF(IAL>=0)NWALE_ENGINE=IAL
      ELSE  !nwale<0 nwale is unchanged. its value will be set to TABVINT(264) in rdcomm sub.
        NWALE_ENGINE=-1
      ENDIF
      WRITE(IIN,'(1P6E10.3)')ALP,GAM,A1,A2,A3,VM


      !---------------------------------------!
      !    '/ALE/LOWMACH                      !
      !---------------------------------------!
      IKEY          = KALE
      K             = 0
      LOWMACH_OPT   = 0
      DO WHILE (IKAD(IKEY) + K /= IKAD(IKEY + 1))
       READ(IUSC1,REC=IKAD(IKEY)+K,FMT='(7X,A,1X,A,31X,I10)',ERR=9990)KEY2,KEY3,NBC
       !---------------------------------------!
       !    '/ALE/LOWMACH'                     !
       !---------------------------------------!
       IF (KEY2(1:5) == 'LOWMA') THEN
          LOWMACH_OPT = 1
          EXIT
       ENDIF
       K = K + 1 + NBC
      ENDDO
      WRITE(IIN,'(I8)') LOWMACH_OPT


C--------------------
C     CONTROL CARD 6(DEL)
C--------------------
      NINTOF=0
      NELSOF=0
      NELQOF=0
      NELCOF=0
      NELTGOF=0
      NELTOF=0
      NELPOF=0
      NELROF=0
      NSPHOF=0
      IKEY=KDEL
      NDEL=0
      NINTNOF=0
      NINTSOF=0
      NALEOF=0
      NEULEROF=0
      NTHERMOF=0
      IF(IKAD(IKEY)/=IKAD(IKEY+1))THEN
       K=0
 200   READ(IUSC1,REC=IKAD(IKEY)+K,FMT='(7X,A,1X,A,31X,I10)',
     .      ERR=9990)KEY2,KEY3,NBC
       K3 = 0
       IF(KEY3(1:3)/='NOD'.AND.KEY3(1:3)/='SEG')
     .      READ(KEY3,FMT='(I5)',ERR=9990)K3
       K=K+1
       NOF=0
       DO 210 I=1,NBC
        READ(IUSC1,REC=IKAD(IKEY)+K,FMT='(A)',ERR=9990)CARTE
        K=K+1
        NOF=NOF+NVAR(CARTE)
 210   CONTINUE
       NDEL=NDEL+1
       IF(K3==1)NOF=NOF/2
       IF(KEY2=='INTER'.AND.KEY3(1:3)=='NOD')THEN
        NINTNOF=NINTNOF+1
       ELSEIF(KEY2=='INTER'.AND.KEY3(1:3)=='SEG')THEN
        NINTSOF=NINTSOF+1
       ELSEIF(KEY2=='INTER')THEN
        NINTOF=NINTOF+NOF
       ELSEIF(KEY2=='BRICK')THEN
        NELSOF=NELSOF+NOF
       ELSEIF(KEY2=='QUAD ')THEN
        NELQOF=NELQOF+NOF
       ELSEIF(KEY2=='SHELL')THEN
        NELCOF=NELCOF+NOF
       ELSEIF(KEY2=='TRUSS')THEN
        NELTOF=NELTOF+NOF
       ELSEIF(KEY2=='BEAM ')THEN
        NELPOF=NELPOF+NOF
       ELSEIF(KEY2=='SPRIN')THEN
        NELROF=NELROF+NOF
       ELSEIF(KEY2=='SH_3N'.OR.KEY2(1:4)=='SH3N')THEN
        NELTGOF=NELTGOF+NOF
       ELSEIF(KEY2=='SPCEL'.OR.KEY2=='SPHCE')THEN
        NSPHOF=NSPHOF+NOF
       ELSEIF(KEY2=='ALE  ')THEN
         NALEOF=1
       ELSEIF(KEY2=='EULER')THEN
         NEULEROF=1
       ELSEIF(KEY2=='THERM')THEN
         NTHERMOF=1
       ELSE
        CALL ERRMSG(KEY2)
       ENDIF
       IF(IKAD(IKEY)+K/=IKAD(IKEY+1))GO TO 200
      ENDIF
      WRITE(IIN,'(10I8)')NINTOF,NELSOF,NELQOF,NELCOF,NELTOF,
     .                  NELPOF,NELROF,NELTGOF,NINTNOF,NINTSOF
      WRITE(IIN,'(I8)')NSPHOF
      WRITE(IIN,'(3I8)')NALEOF,NEULEROF,NTHERMOF
C--------------------
C     CONTROL CARD 7
C       ALE LINKS (OLD FORMAT)
C--------------------
      NRLINK=0
      NALELK=0
      IKEY=KVEL
      K=0
      DO WHILE (IKAD(IKEY) + K /= IKAD(IKEY + 1))
       READ(IUSC1,REC=IKAD(IKEY)+K,FMT='(7X,A,37X,I10)', ERR=9990)KEY2,NBC
       IF(KEY2=='TRA  ')THEN
        NRLINK=NRLINK+1
       ELSEIF(KEY2=='ROT  ')THEN
        NRLINK=NRLINK+1
       ELSEIF(KEY2=='ALE  ')THEN
        NALELK=NALELK+1
       ELSE
        CALL ERRMSG(KEY2)
       ENDIF
       K=K+NBC+1
      ENDDO
      WRITE(IIN,'(2I8)')NRLINK,NALELK
C-----------------------
C     ALE LINKS
C-----------------------
      IKEY          = KALE
      NALELINK      = 0
      K=0
      DO WHILE (IKAD(IKEY) + K /= IKAD(IKEY + 1))
        READ(IUSC1,REC=IKAD(IKEY)+K,FMT='(7X,A,1X,A,31X,I10)',ERR=9990)KEY2,KEY3,NBC
         IF(KEY2(1:5)=='LINK ')THEN
            IF(KEY3(1:5)=='VEL  ')THEN
               NALELINK=NALELINK+1
            ELSEIF(KEY3(1:5)=='OFF  ')THEN
               NALELINK=NALELINK+1

            ELSEIF(KEY3(1:5)=='ON   ')THEN
               NALELINK=NALELINK+1
            ELSE
               CALL ERRMSG('/ALE/LINK')
            ENDIF
         ENDIF
         K=K+NBC+1
      ENDDO
      WRITE(IIN,'(I8)')NALELINK

C-----------------------
C     /ALE/SUPG
C-----------------------
      UPWM          = 3
      UPW_UPDATE    = 0
      UPWMG2        = ZERO
      UPWOG2        = ZERO
      UPWSM2        = ZERO
      CUPWM         = ONE     !SUPG enabled
      I_SUPG_ON_OFF = 0      !test if card is present to check user input in case of both /ALE/SUPG & /UPWM/SUPG
                             !  1 : ON
                             !  2 : OFF
      IKEY          = KALE
      K             = 0
      DO WHILE (IKAD(IKEY) + K /= IKAD(IKEY + 1))
       READ(IUSC1,REC=IKAD(IKEY)+K,FMT='(7X,A,1X,A,31X,I10)',ERR=9990)KEY2,KEY3,NBC
       !---------------------------------------!
       !    '/ALE/SUPG/OFF - ON                !
       !---------------------------------------!
       IF(KEY2(1:4)=='SUPG')THEN
         IF(KEY3(1:3)=='OFF')THEN
           UPWM            = 0
           UPW_UPDATE      = 0
           I_SUPG_ON_OFF   = 2
         ELSEIF(KEY3(1:5)=='ON')THEN
           UPWM            = 3
           UPW_UPDATE      = 0
           I_SUPG_ON_OFF   = 1
         ELSE
           IF(N2D==1)THEN
           !axisymmetric case
             UPWM          = 0
             UPW_UPDATE    = 0
           ELSE
           !3D & 2D planar
             UPWM          = 3
           ENDIF
           UPW_UPDATE      = 0
           I_SUPG_ON_OFF   = 0
         ENDIF
       ENDIF
       K = K + 1 + NBC
      ENDDO

C-----------------------
C      /ALE/MUSCL
C-----------------------
      I_MUSCL_OFF = 0
      IKEY          = KALE
      K             = 0
      DO WHILE (IKAD(IKEY) + K /= IKAD(IKEY + 1))
       READ(IUSC1,REC=IKAD(IKEY)+K,FMT='(7X,A,1X,A,31X,I10)',ERR=9990)KEY2,KEY3,NBC
       !---------------------------------------!
       !    '/ALE/MUSCL/OFF'                   !
       !---------------------------------------!
       IF (KEY2(1:5) == 'MUSCL') THEN
          IF(KEY3(1:3)=='OFF')THEN
              I_MUSCL_OFF = 1
          ELSEIF(KEY3(1:2)=='ON')THEN
              I_MUSCL_OFF = 0
              IALEMUSCL = 1
              I_BETA = TWO
          ELSE
            GOTO 9990
          ENDIF
       ENDIF
       K = K + 1 + NBC
      ENDDO

C--------------------
C   /ALE/ON  /ALE/OFF
C--------------------
      IKEY         = KALE
      NBALEOFF     = 0
      NBALEON      = 0
      NBPARTALEON  = 0
      NBPARTALEOFF = 0
      K            = 0
      DO WHILE (IKAD(IKEY) + K /= IKAD(IKEY + 1))
         READ (IUSC1, REC = IKAD(IKEY) + K, FMT = '(7X,A,37X,I10)') KEY2, NBC
         IF (KEY2(1:2) == 'ON') THEN
            NBALEON = NBALEON + 1
            NBPARTALEON = NBPARTALEON + NBC
         ENDIF
         IF (KEY2(1:3) == 'OFF') THEN
            NBALEOFF = NBALEOFF + 1
            NBPARTALEOFF = NBPARTALEOFF + NBC
         ENDIF
         K = K + 1 + NBC
      ENDDO
      WRITE (IIN, '(I8, I8)') NBPARTALEON, NBPARTALEOFF
C-----------------------
C     /ALE/SOLVER/FINT
C-----------------------
      IKEY          = KALE
      K             = 0
      ISFINT        = 0 !not yet read communicated in engine
      DO WHILE (IKAD(IKEY) + K /= IKAD(IKEY + 1))

       READ(IUSC1,REC=IKAD(IKEY)+K,FMT='(7X,A32)',ERR=9990)StrIAL
       IF(StrIAL(1:5)/='SOLVE')EXIT
       READ(IUSC1,REC=IKAD(IKEY)+K,FMT='(13X,A32)',ERR=9990)StrIAL
       IF(StrIAL(1:4)/='FINT')EXIT
       READ(IUSC1,REC=IKAD(IKEY)+K,FMT='(A)',ERR=9990)LINE
       READ(LINE,FMT='(7X,A5,1X,A5,1X,I5)',ERR=9990)KEY2,KEY3,IFORM
       IF(IFORM==2)THEN
         ISFINT          = 2  !FEM INT : surface integration
       ELSEIF(IFORM==1)THEN
         ISFINT          = 1  !FVM INT : volume integration
       ELSEIF(IFORM==3)THEN
         ISFINT          = 3  !FVM INT : volume integration
       ELSE
         !default
         ISFINT          = 1
       ENDIF
       K = K + 1
      ENDDO
C--------------------
C   /ALE/ CHECK KEY2
C--------------------
      IKEY         = KALE
      K            = 0
      DO WHILE (IKAD(IKEY) + K /= IKAD(IKEY + 1))
         lFOUND=.FALSE.
         READ (IUSC1, REC = IKAD(IKEY) + K, FMT = '(A32)', ERR=998) KEYA
         READ (IUSC1, REC = IKAD(IKEY) + K, FMT = '(X,A5,X,A5)', ERR=998) KEY1  ,KEY2
         IF(KEY2(1:5)=='     ')KEY2(1:1)='0'
         IF(KEY1(1:5)=='ALE  ')THEN
           DO INUM=1,NUM_ALE_KEY2
             IF (KEY2(1:5) == KEYALE(INUM)) THEN
              lFOUND = .TRUE.
              EXIT
             ENDIF
           ENDDO
           IF(lFOUND.EQV. .FALSE.)THEN
             CALL ANCMSG(MSGID=51,ANMODE=ANINFO,C1=KEYA(1:12))
             IERR = IERR + 1
           ENDIF
         ENDIF
 998     CONTINUE
      ! ENDIF
       K = K + 1
      ENDDO


C--------------------
C     CONTROL CARD 8(DEF CHANGES)
C--------------------
      NUBCS1=0
      NBCS1=0
      NBCS2=0
      IKEY=KBCS
      IF(IKAD(IKEY)/=IKAD(IKEY+1))THEN
       K=0
 400   READ(IUSC1,REC=IKAD(IKEY)+K,FMT='(7X,A,37X,I10)',
     .      ERR=9990)KEY2,NBC
       K=K+1
       DO 410 I=1,NBC
        READ(IUSC1,REC=IKAD(IKEY)+K,FMT='(A)',ERR=9990)CARTE
        K=K+1
        NUBCS1=NUBCS1+NVAR(CARTE)
 410   CONTINUE
       NBCS1 = NBCS1 + 1
       IF(IKAD(IKEY)+K/=IKAD(IKEY+1))GO TO 400
      ENDIF
      NUBCS2=0
      IKEY=KBCSR
      IF(IKAD(IKEY)/=IKAD(IKEY+1))THEN
       K=0
 420   READ(IUSC1,REC=IKAD(IKEY)+K,FMT='(7X,A,37X,I10)',
     .      ERR=9990)KEY2,NBC
       K=K+1
       DO I=1,NBC
        READ(IUSC1,REC=IKAD(IKEY)+K,FMT='(A)',ERR=9990)CARTE
        K=K+1
        NUBCS2=NUBCS2+NVAR(CARTE)
       ENDDO
       NBCS2 = NBCS2 + 1
       IF(IKAD(IKEY)+K/=IKAD(IKEY+1))GO TO 420
      ENDIF
      NUM_BCS=NUBCS1+NUBCS2
C
      NINIV = 0
      NINIVT= 0
      IKEY=KINIV
      IF(IKAD(IKEY)/=IKAD(IKEY+1))THEN
       K=0
 440   READ(IUSC1,REC=IKAD(IKEY)+K,FMT='(7X,A,7X,I5,25X,I10)',
     .      ERR=9990)KEY2,K3,NBC
       K=K+1
       IF (KEY2(1:4)=="AXIS") THEN
         M=2
       ELSE
         M=1
       ENDIF
       K=K+M
       IF (K3==2) THEN
        READ(IUSC1,REC=IKAD(IKEY)+K,FMT='(A)',ERR=9990)CARTE
C-----------  NVAR(CARTE) should be =1
        K=K+1
        NINIVT=NINIVT+(NBC-M)*2*NVAR(CARTE)
       ELSE
         DO I=1,NBC-M
          READ(IUSC1,REC=IKAD(IKEY)+K,FMT='(A)',ERR=9990)CARTE
          K=K+1
          NINIVT=NINIVT+(2-K3)*NVAR(CARTE)
         ENDDO
       END IF
       NINIV=NINIV + 1
       IF(IKAD(IKEY)+K/=IKAD(IKEY+1))GO TO 440
      ENDIF
      WRITE(IIN,'(2I8)')NUM_BCS,NINIVT/2
C--------------------
C     CONTROL CARD 9(TEMP CHANGES)
C--------------------
      NINTCH=0
      INTCH_IFOP = 0
      IFOP  =1
      INTCH_ITOP = 0
      ITOP = 1
      NBTOPFOP = 0
      IKEY=KINTER
      IF(IKAD(IKEY)/=IKAD(IKEY+1))THEN
       K=0
C      options /inter/fop , /inter/top, /inter/nofop, /inter/notop
 500   READ(IUSC1,REC=IKAD(IKEY)+K,FMT='(7X,A,37X,I10)',
     .      ERR=9990)KEY2,NBC
       K=K+NBC+1
       NINTCH=NINTCH+1
       IF(KEY2=='FOP') THEN
C        IFOP = 1
         IF (INTCH_IFOP/=0) THEN
           GO TO 9990
         ELSE
           INTCH_IFOP=NINTCH
           NBTOPFOP = NBTOPFOP + 1
         ENDIF
       ELSEIF(KEY2=='NOFOP') THEN
C        IFOP = 0
         IF (INTCH_IFOP/=0) THEN
           GO TO 9990
         ELSE
           INTCH_IFOP=NINTCH
           NBTOPFOP = NBTOPFOP + 1
         ENDIF
       ELSEIF(KEY2=='TOP') THEN
C        ITOP = 1
         IF (INTCH_ITOP/=0) THEN
           GO TO 9990
         ELSE
           INTCH_ITOP=NINTCH
           NBTOPFOP = NBTOPFOP + 1
         ENDIF
       ELSEIF(KEY2=='NOTOP') THEN
C        ITOP = 0
         IF (INTCH_ITOP/=0) THEN
           GO TO 9990
         ELSE
           INTCH_ITOP=NINTCH
           NBTOPFOP = NBTOPFOP + 1
         ENDIF
       ENDIF
       IF(IKAD(IKEY)+K/=IKAD(IKEY+1))GO TO 500
      ENDIF
C
      IKEY=KRBODY
      NRBYON=0
      NRBYOF=0
      NRBYNF=0
      IF(IKAD(IKEY)/=IKAD(IKEY+1))THEN
       K=0
 550   READ(IUSC1,REC=IKAD(IKEY)+K,FMT='(7X,A,37X,I10)',
     .      ERR=9990)KEY2,NBC
       K=K+1
       NOF=0
       DO 560 I=1,NBC
        READ(IUSC1,REC=IKAD(IKEY)+K,FMT='(A)',ERR=9990)CARTE
        K=K+1
        NOF=NOF+NVAR(CARTE)
 560   CONTINUE
       NRBYNF=NRBYNF+1
       IF(KEY2=='ON')THEN
        NRBYON=NRBYON+NOF
       ELSEIF(KEY2=='OFF')THEN
        NRBYOF=NRBYOF+NOF
       ELSE
        CALL ERRMSG(KEY2)
       ENDIF
       IF(IKAD(IKEY)+K/=IKAD(IKEY+1))GO TO 550
      ENDIF
C
C prise en compte des cas /inter/fop et /inter/top
      WRITE(IIN,'(10I8)')NINTCH-NBTOPFOP,NRBYON,NRBYOF
C--------------------
C     COUPLAGE MADYMO/RADIOSS.
C--------------------
      IKEY=KMADYM
      IMADCPL = 0
C--------------------
      IF (IKAD(IKEY)/=IKAD(IKEY+1)) THEN
       K=0
 650   READ(IUSC1,REC=IKAD(IKEY)+K,FMT='(7X,A,37X,I10)',
     .      ERR=9990) KEY2,NBC
       IF(KEY2/='ON'  .AND.
     *    KEY2/='ON2' .AND.
     *    KEY2/='MPP' )THEN
        CALL ERRMSG(KEY2)
       ELSEIF(KEY2=='ON2')THEN
        IMADCPL = 1
       ELSEIF(KEY2=='MPP')THEN
        IMADCPL = 1
       ENDIF
       K=K+1
       DO 660 I=1,NBC
        READ(IUSC1,REC=IKAD(IKEY)+K,FMT='(A)',ERR=9990) CARTE
        K=K+1
 660   CONTINUE
       IF(IKAD(IKEY)+K/=IKAD(IKEY+1)) GO TO 650
      ENDIF
C--------------------
C     EXTERNAL PROCESS COUPLING (rad2rad)
C--------------------
      IKEY=KRAD2R
      IRAD2R=0
C--------------------
      IF (IKAD(IKEY)/=IKAD(IKEY+1)) THEN
        K=0
 670    READ(IUSC1,REC=IKAD(IKEY)+K,FMT='(7X,A,37X,I10)',
     .      ERR=9990) KEY2,NBC
        IF(KEY2/='ON')THEN
          CALL ERRMSG(KEY2)
        ELSEIF(KEY2=='ON')THEN
          IRAD2R = 1
C     Connection initale avec rad2rad  C
          IF (ISPMD==0) CALL CONNECTION_SOCK_INIT_C(SOCK0)
        ENDIF
        K=K+1
        DO 680 I=1,NBC
          READ(IUSC1,REC=IKAD(IKEY)+K,FMT='(A)',ERR=9990) CARTE
          K=K+1
 680    CONTINUE
        IF(IKAD(IKEY)+K/=IKAD(IKEY+1)) GO TO 670
      ENDIF
C--------------------------------------------------
C     /PERF
C-------------------------------------------------
      IKEY=KPERF
      GLOBAL_INTERFACE_SORTING_ALGO = 1
      IF (IKAD(IKEY)/=IKAD(IKEY+1)) THEN
        K=0
 700    READ(IUSC1,REC=IKAD(IKEY)+K,FMT='(7X,A,37X,I10)',
     .      ERR=9990) KEY2,NBC
        IF(KEY2=='SORT1') THEN
          GLOBAL_INTERFACE_SORTING_ALGO = 1
        ELSEIF(KEY2=='SORT2') THEN
          GLOBAL_INTERFACE_SORTING_ALGO = 2
         ELSEIF(KEY2=='SORT3') THEN
          GLOBAL_INTERFACE_SORTING_ALGO = 0
        ENDIF
        K = K +1
        IF(IKAD(IKEY)+K/=IKAD(IKEY+1)) GO TO 700
      ENDIF
C--------------------
C     RADIOSS SPMD specific options
C--------------------
      IKEY=KSPMD
      IDDW=0
      IPREAD=0
      IDDWSTAT=0
C
      IF (IKAD(IKEY)/=IKAD(IKEY+1)) THEN
       K=0
 750   READ(IUSC1,REC=IKAD(IKEY)+K,FMT='(7X,A,37X,I10)',
     .      ERR=9990) KEY2,NBC
       IF(KEY2=='DDW')THEN
         IDDW=1
         CALL WRIUSC2(IKAD(IKEY)+1,1,KEY0(IKEY))
         READ(IUSC2,*) TPSENG

       ELSEIF(KEY2=='PREAD')THEN
         IPREAD = 1
       ELSEIF(KEY2=='DDWST')THEN
        IDDW=1
        IDDWSTAT=1
       ELSE
         CALL ERRMSG(KEY2)
       ENDIF
       K=K+1
       DO 760 I=1,NBC
        READ(IUSC1,REC=IKAD(IKEY)+K,FMT='(A)',ERR=9990) CARTE
        K=K+1
 760   CONTINUE
       IF(IKAD(IKEY)+K/=IKAD(IKEY+1)) GO TO 750
      ENDIF
C
C--------------------
C     RAYLEIGH DAMPING
C--------------------
      IDAMP0=0
      N_DAMP=0
      IKEY=KDAMP
      IF(IKAD(IKEY)/=IKAD(IKEY+1))THEN
        K=0
 770    READ(IUSC1,REC=IKAD(IKEY)+K,FMT='(49X,I10)',ERR=9990) NBC
        IF (NBC == 2) IDAMP0=1
        K=K+NBC+1
        N_DAMP=N_DAMP+1
        IF(IKAD(IKEY)+K/=IKAD(IKEY+1))GO TO 770
      ENDIF
      WRITE(IIN,'(I8)')N_DAMP
C--------------------
C    FLEXIBLE BODY INPUT FILES
C--------------------
      IKEY=KFXINP
      NFXINP=0
      IF (IKAD(IKEY)/=IKAD(IKEY+1)) THEN
         K=0
  780    CALL WRIUSC2(IKAD(IKEY)+1,1,KEY0(IKEY))
         READ(IUSC1,REC=IKAD(IKEY)+K,FMT='(49X,I10)',ERR=9990) NBC
         K=K+NBC+1
         NFXINP=NFXINP+NBC
         IF (IKAD(IKEY)+K/=IKAD(IKEY+1)) GO TO 780
      ENDIF
      WRITE(IIN,'(I8)')NFXINP
C-----------------------
C     SHELLS SUB-CYCLING
C-----------------------
      ISHSUB = 0
      IKEY=KOSHSUB
      IF(IKAD(IKEY)/=IKAD(IKEY+1))THEN
       ISHSUB=1
       READ(IUSC1,REC=IKAD(IKEY),FMT='(7X,I5,X,I5)',ERR=9990)
     .      NCPRISH,NCTRLSH
       WRITE(IIN,'(2I8)')NCPRISH,NCTRLSH
      ELSE
       NCPRISH=0
       NCTRLSH=0
      ENDIF
C--------------------
C    EIGENPROBLEM DEACTIVATION
C--------------------
      IKEY=KEIG
      NEIGOFF=0
      NEOFF=0
      IF (IKAD(IKEY)/=IKAD(IKEY+1)) THEN
         K=0
 790     READ(IUSC1,REC=IKAD(IKEY)+K,FMT='(7X,A,37X,I10)',
     .        ERR=9990)KEY2,NBC
         K=K+1
         NOF=0
         DO 800 I=1,NBC
            READ(IUSC1,REC=IKAD(IKEY)+K,FMT='(A)',ERR=9990)CARTE
            K=K+1
            NOF=NOF+NVAR(CARTE)
 800     CONTINUE
         NEIGOFF=NEIGOFF+1
         NEOFF=NEOFF+NOF
         IF (KEY2/='OFF') CALL ERRMSG(KEY2)
         IF (IKAD(IKEY)+K/=IKAD(IKEY+1)) GOTO 790
      ENDIF
      WRITE(IIN,'(2I8)')NEIGOFF, NEOFF
C-----------------------
C    FVMBAG REMESH/MODIF
C-----------------------
      IKEY=KFVBAG
      NFVMESH=0
      NFVMODI=0
      IF (IKAD(IKEY)/=IKAD(IKEY+1)) THEN
         K=0
  810    READ(IUSC1,REC=IKAD(IKEY)+K,FMT='(7X,A5,37X,I10)',ERR=9990)
     .      KEY2, NBC
         K=K+NBC+1
C Le mot clef doit etre repete (FA)
C        IF (KEY2=='REMES') NFVMESH=NFVMESH+NBC/6
         IF (KEY2=='REMES') NFVMESH=NFVMESH+1
         IF (KEY2=='MODIF') NFVMODI=NFVMODI+1
         IF (IKAD(IKEY)+K/=IKAD(IKEY+1)) GO TO 810
      ENDIF
      WRITE(IIN,'(2I8)')NFVMESH,NFVMODI
C-----------------------
C  OPTION /DT1HEPH use full integrated solid timestep.
C--------------------------------------------------------
      IKEY   = KDT1HEPH
      IDTHEPH=0
      IF(IKAD(IKEY)/=IKAD(IKEY+1))THEN
        IDTHEPH=1
      END IF
          IF (IDTHEPH==0.AND.IDT1SOL==1) IDTHEPH=1
C-----------------------
C  OPTION /DT1TET10
C--------------------------------------------------------
      IKEY   = KDT1TET10
      IDT1TET10=0
      IF(IKAD(IKEY)/=IKAD(IKEY+1))THEN
        NITERP=0
        READ(IUSC1,REC=IKAD(IKEY),FMT='(7X,I5)',ERR=9990)NITERP
        IF(NITERP == 0)NITERP=1
        IDT1TET10=1+NITERP
      END IF
C-----------------------
C  OPTION /DTTSH use full shell timestep for thick shells.
C--------------------------------------------------------
      IKEY   = KDTTSH
      IDTTSH=0
      IF(IKAD(IKEY)/=IKAD(IKEY+1))THEN
        IDTTSH=1
      END IF
C-----------------------
C  OPTION /MASS/RESET
C--------------------------------------------------------
      IKEY   = KMASS
      IMASSI = 0
      IF(IKAD(IKEY)/=IKAD(IKEY+1))THEN
        READ(IUSC1,REC=IKAD(IKEY),FMT='(7X,A)',ERR=9990)KEY2
        IF(KEY2(1:5)=='RESET'.AND.IRUNN>1.AND.MCHECK==0) IMASSI = 1
      END IF
C--------------------
C     DELETE INTERFACE 7 AFTER SHELL DELETE
C--------------------
      CALL FREDLI7(IKAD(0),KEY0,KDELIN)
C--------------------
C     DELETE INTERFACES
C--------------------
      CALL FREDLI(IKAD(0),KEY0,KDEL,NDEL)
C--------------------
C     DELETE ELEMENTS
C--------------------
      CALL RDELE(NDEL,'BRICK',IKAD(KDEL),KEY0(IKEY))
      CALL RDELE(NDEL,'QUAD ',IKAD(KDEL),KEY0(IKEY))
      CALL RDELE(NDEL,'SHELL',IKAD(KDEL),KEY0(IKEY))
      CALL RDELE(NDEL,'TRUSS',IKAD(KDEL),KEY0(IKEY))
      CALL RDELE(NDEL,'BEAM ',IKAD(KDEL),KEY0(IKEY))
      CALL RDELE(NDEL,'SPRIN',IKAD(KDEL),KEY0(IKEY))
      CALL RDELE(NDEL,'SH_3N',IKAD(KDEL),KEY0(IKEY))
      CALL RDELE(NDEL,'SH3N ',IKAD(KDEL),KEY0(IKEY))
      CALL RDELE(NDEL,'SPCEL',IKAD(KDEL),KEY0(IKEY))
      CALL RDELE(NDEL,'SPHCE',IKAD(KDEL),KEY0(IKEY))
C--------------------
C     LINK
C--------------------
      CALL FRELNK(IKAD(0),KEY0,KVEL,NRLINK,IFID,VEL_SK_FR)
C--------------------
C     ALE LINK
C--------------------
      LLINAL = 0
      IF(NALELK/=0)  CALL FRALNK (IKAD(0),KEY0,KVEL,NALELK,IFID)       !format v5.1
      IF(NALELINK/=0)CALL FRALNK2(IKAD(0),KEY0,KALE,NALELINK,IFID) !format v12
      !LLINAL is now LINALE() dimension for engine ALE links definitions.
C--------------------
C     ALE ON / OFF
C--------------------
      IF (NBALEON + NBALEOFF /= 0) CALL FRALEONOFF(IKAD(0), KEY0, KALE, NBALEON + NBALEOFF)
C--------------------
C     BCS
C--------------------
      CALL FREBCS(IKAD(0),KEY0,KBCS,KBCSR,NBCS1,NBCS2,BCS_SK_FR)
C--------------------
C     CHANGE INTERF
C--------------------
      CALL FREINT(IKAD(0)   ,KEY0,KINTER,NINTCH,INVERS,
     .            INTCH_IFOP,INTCH_ITOP)
C--------------------
C     RBODY ON OFF
C--------------------
      CALL FRERBO(IKAD(0),KEY0,KRBODY,NRBYNF)
C--------------------
C     OPTION V181
C--------------------
C--------------------
C     SECTIONS FLUIDE
C--------------------
      NTFLSW=0
      IF(NSFLSW/=0)CALL FREFLW(
     .      IKAD(0),KEY0,KFLSEC,NSFLSW,NTFLSW,IFID,MFID)
C   Sections fluides non portees en SPMD
      IF (NSFLSW>0.AND.IMACH==3) THEN
       CALL ANCMSG(MSGID=201,ANMODE=ANINFO)
       IERR=IERR+1
      ENDIF
C--------------------
C     CHANGE FUNCTION
C--------------------
      CALL FREFUNC(IKAD(0),KEY0,KFUNC,NFCT)
C--------------------
C     REINITIALISATION DES VITESSES
C--------------------
      CALL FREINIV(IKAD(0),KEY0,KINIV,NINIV)
C-----------------------
C     ANIMATION FILES
C-----------------------
      CALL FREANIM(IKAD(0),KEY0,KANIM,IFID,MFID)
C-----------------------
C     OUTPUT FILES
C-----------------------
      CALL FREOUTP(IKAD(0),KEY0,KOUTP)
C-----------------------
C     DEBUG PRINT
C-----------------------
      CALL FREDEBUG(IKAD(0),KEY0,KDEBUG)
C--------------------
C     NOISE
C--------------------
      CALL FRENOIS(IKAD(0),KEY0,KNOISE,IFID,MFID)
C--------------------
C     UPWIND PARAMETERS (ETA1,ETA2,ETA3)
C--------------------
      CALL FREUPWIND(IKAD(0),KEY0,KUPWIND)
C--------------------
C     UPWIND METHOD FOR MOMENTUM ADVECTION
C--------------------
      CALL FREUPWM(IKAD(0),KEY0,KUPWM)
      !print *, "UPWM      ",UPWM
      !print *, "CUPWM     ",CUPWM
      !print *, "UPWMG2    ",UPWMG2
      !print *, "UPWOG2    ",UPWOG2
      !print *, "UPWSM2    ",UPWSM2
      !print *, "UPW_UPDATE",UPW_UPDATE
C--------------------
C     INSTRUMENTATION DU CODE
C--------------------
      IKEY=KMON
      IMON = 1
      IMONM = 0
      IMON_MAT = 0
      IF(IKAD(IKEY)/=IKAD(IKEY+1))THEN
       READ(IUSC1,REC=IKAD(IKEY),FMT='(7X,A,1X,I5)',
     .        ERR=9990)KEY2,K4
       IF(KEY2=='OFF')THEN
         IMON = 0
         IMONM = 0
       ELSEIF(KEY2=='ON')THEN
         IMON = K4 + 1
         IMONM = 0
       ELSEIF(KEY2=='FULL')THEN
         IMON = K4 + 1
         IMONM = 1
       ELSEIF(KEY2=='SYNC')THEN
         IMON = K4 + 1
         IMONM = 2
       ELSEIF(KEY2=='MAT')THEN
         IMON = K4 + 1
         IMON_MAT = 1
c       ELSEIF(KEY2=='GRAPH')THEN
c         IMON = K4 + 1
c         IMONM = 3
       ENDIF
      ENDIF
C--------------------
C     INCOMPRESSIBLE
C--------------------
      INCOMP = 0
      IKEY=KINCMP
      IF(IKAD(IKEY)/=IKAD(IKEY+1))THEN
       INCOMP=1
      ENDIF
C--------------------
C     COUPLAGE MADYMO/RADIOSS
C--------------------
      IF (IMADCPL/=0) CALL FRECPL(IKAD,KEY0,KMADYM)
C--------------------
C     DAMPING
C--------------------
      CALL FREDAMP(IKAD(0),KEY0,KDAMP,N_DAMP)
C-----------------------
C     IMPLICIT OPTIONS
C-----------------------
      CALL FREIMPL(IKAD(0),KEY0,KIMPL)
C--------------------
C    OUTPUT FILES FOR FLEXIBLE BODIES
C--------------------
      CALL FREFXINP(IKAD(0),KEY0,KFXINP,NFXINP)
C--------------------
C    EIGENPROBLEM DEACTIVATION
C--------------------
      CALL FREEIG(IKAD(0),KEY0,KEIG,NEIGOFF)
C--------------------
C     FVMBAG REMESH
C--------------------
      CALL FREFVBAG(IKAD(0),KEY0,KFVBAG)
C-----------------------
C     STATE FILES
C-----------------------
      CALL FRESTAT(IKAD(0),KEY0,KSTATF)
C-----------------------
C     DYNAIN FILES
C-----------------------
      CALL FREDYNAIN(IKAD(0),KEY0,KDYNAIN,DYNAIN_DATA)
C-----------------------
C     ABF FILES
C-----------------------
      ABFILE = 0
      NABFILE = 0
#ifdef DNC
      CALL FREABF(IKAD(0),KEY0,KABF,ABFILE,NABFILE,1)
      CALL FREABF(IKAD(0),KEY0,KAABF,ABFILE,NABFILE,2)
      CALL FREABF(IKAD(0),KEY0,KBABF,ABFILE,NABFILE,3)
      CALL FREABF(IKAD(0),KEY0,KCABF,ABFILE,NABFILE,4)
      CALL FREABF(IKAD(0),KEY0,KDABF,ABFILE,NABFILE,5)
      CALL FREABF(IKAD(0),KEY0,KEABF,ABFILE,NABFILE,6)
      CALL FREABF(IKAD(0),KEY0,KFABF,ABFILE,NABFILE,7)
      CALL FREABF(IKAD(0),KEY0,KGABF,ABFILE,NABFILE,8)
      CALL FREABF(IKAD(0),KEY0,KHABF,ABFILE,NABFILE,9)
      CALL FREABF(IKAD(0),KEY0,KIABF,ABFILE,NABFILE,10)
      IF (NABFILE/=0 .AND. ABFILE(1) == 0) THEN
        NABFILE = NABFILE + 1
        ABFILE(1) = ABFILE(1) + 1
      ENDIF
#endif
C-----------------------
C     THERMAL ANALYSIS
C-----------------------
      CALL FRETHERMAL(IKAD(0),KEY0,KTHERMAL)
C-----------------------
C     H3DFILES
C-----------------------
      CALL H3D_INI(H3D_DATA,KH3D,NSLASH)
      CALL H3D_READ(IKAD(0),KEY0,KH3D,NSLASH,H3D_DATA)
C-----------------------
C     REPORT
C-----------------------
      IKEY=KREPORT
      IREPORT = 0
      IH3DREADER = 0
      IMVW = 0
      IMVW_REF = 0
      K = 0

      DO WHILE(IKAD(IKEY)+K/=IKAD(IKEY+1))
        READ(IUSC1,REC=IKAD(IKEY)+K,
     .      FMT='(7X,A,1X,A,1X,A,1X,A,19X,I10)',
     .      ERR=9990)KEY2,KEY3,KEY4,KEY5,NBC

        IREPORT=1
        IF(KEY2=='HTML')THEN
          IHTML = 1
          IF(KEY3=='H3D' .AND. NSLASH(KH3D) /= 0) THEN
            IH3DREADER = 1
            IF(KEY4 =='EXT') IH3DREADER = 2
          ENDIF
        ELSEIF(KEY2=='MVW')THEN
          IMVW=1
          IF(KEY3=='REF')THEN
            IMVW_REF = 1
            K = K + 1
            READ(IUSC1,REC=IKAD(IKEY)+K,FMT='(5X,A)',ERR=9990)IMVW_REFPATH
          ELSEIF(KEY3=='ANIM')THEN
            IF(KEY4=='TH')THEN
              IMVW=1
            ELSEIF(KEY4=='ABF')THEN
              IMVW=2
            ENDIF
          ELSEIF(KEY3=='H3D')THEN
            IF(KEY4=='TH')THEN
              IMVW=3
            ELSEIF(KEY4=='ABF')THEN
              IMVW=4
            ENDIF
          ELSE
c autoselect of files format
            IF(NSLASH(KANIM) /= 0)THEN
              IF(NSLASH(KTFILE) /= 0)THEN
                IMVW=1
              ELSEIF(NSLASH(KABF) /= 0)THEN
                IMVW=2
              ENDIF
            ELSEIF(NSLASH(KH3D) /= 0) THEN
              IF(NSLASH(KTFILE) /= 0)THEN
                IMVW=3
              ELSEIF(NSLASH(KABF) /= 0)THEN
                IMVW=4
              ENDIF
            ENDIF
          ENDIF
        ENDIF
        K = K + 1
      ENDDO
C========================================================================
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7--
      CLOSE(UNIT=IUSC1)
      CLOSE(UNIT=IUSC2)
      CALL DELETE_USER_FILE(IUSC1_FN,LEN_IUSC1_FN)
      CALL DELETE_USER_FILE(IUSC2_FN,LEN_IUSC2_FN)

      IF(IERR==0) RETURN
      CALL ARRET(0)
C
 9990 CONTINUE
C
      CALL ANCMSG(MSGID=73,ANMODE=ANINFO,
     .            C1=KEY0(IKEY))
      CALL ARRET(0)
      RETURN
      
C-----------------------------------------------
 1000 CONTINUE
       IF (IERR1/=0) THEN
         WRITE(IOUT,*)' ** ERROR IN MEMORY ALLOCATION WHILE READING ENGINE FILE'
         WRITE(ISTDO,*)' ** ERROR IN MEMORY ALLOCATION WHILE READING ENGINE FILE'
         CALL ARRET(2)
       END IF
C-----------------------------------------------
      
      END
Chd|====================================================================
Chd|  NGR2USR                       source/input/freform.F        
Chd|-- called by -----------
Chd|        LECDAMP                       source/input/lecdamp.F        
Chd|        LECINV                        source/input/lecinv.F         
Chd|        LECTUR                        source/input/lectur.F         
Chd|        S10CNISTAT                    source/elements/solid/solide10/s10cndf.F
Chd|        SMS_MASS_SCALE_2              source/ams/sms_mass_scale_2.F 
Chd|        STATIC                        source/general_controls/damping/static.F
Chd|-- calls ---------------
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|====================================================================
      INTEGER FUNCTION NGR2USR(IU,IGRNOD,NGRNOD)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE GROUPDEF_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "comlock.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "units_c.inc"
#include      "com08_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IU,NGRNOD
C-----------------------------------------------
      TYPE (GROUP_)  , DIMENSION(NGRNOD) :: IGRNOD
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, IE
      IF(IU==0)THEN
        NGR2USR=0
        RETURN
      ENDIF
      DO I=1,NGRNOD
      IE=IGRNOD(I)%ID
      IF(IE==IU)THEN
        NGR2USR=I
        RETURN
      ENDIF
      ENDDO
       TSTOP = TT
#include "lockon.inc"
       WRITE(IOUT,*) ' ** ERROR ** : NODE GROUP',IU,' NOT FOUND'
       WRITE(ISTDO,*) ' ** ERROR ** : NODE GROUP',IU,' NOT FOUND'
       NGR2USR=0
#include "lockoff.inc"
      RETURN
      END
C
Chd|====================================================================
Chd|  FRETITL                       source/input/freform.F        
Chd|-- called by -----------
Chd|        GENH3D                        source/output/h3d/h3d_results/genh3d.F
Chd|        WRITE_SENSOR_TAB              source/output/restart/write_sensor_tab.F
Chd|        W_GR_ENTITY                   source/output/restart/w_gr_entity.F
Chd|        W_LINE_STR                    source/output/restart/w_line_str.F
Chd|        W_SUBSET_STR                  source/output/restart/w_subset_str.F
Chd|        W_SURF_STR                    source/output/restart/w_surf_str.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE FRETITL(TITR,IASC,L)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
      INTEGER L,IASC(*)
      CHARACTER TITR*(*)
      INTEGER I,J,LTITR
C
      J=1
      LTITR=LEN(TITR)
      DO I=1,L
        IASC(I)=  ICHAR(TITR(J:J))*65536
        J = J+1
        IF(J>LTITR) GOTO 10
        IASC(I)=  IASC(I) + ICHAR(TITR(J:J))*256
        J = J+1
        IF(J>LTITR) GOTO 10
        IASC(I)=  IASC(I) + ICHAR(TITR(J:J))
        J = J+1
        IF(J>LTITR) GOTO 10
      ENDDO
 10   CONTINUE
C
      RETURN
      END
Chd|====================================================================
Chd|  FRETITL2                      source/input/freform.F        
Chd|-- called by -----------
Chd|        ALLOC_GROUP_STR               source/groups/alloc_group_str.F
Chd|        ALLOC_LINE_STR                source/groups/alloc_line_str.F
Chd|        ALLOC_SUBSET_STR              source/groups/alloc_subset_str.F
Chd|        ALLOC_SURF_STR                source/groups/alloc_surf_str.F
Chd|        GENANI                        source/output/anim/generate/genani.F
Chd|        HIST1                         source/output/th/hist1.F      
Chd|        OUTP_MT                       source/output/sty/outp_mt.F   
Chd|        READ_SENSOR_TAB               source/output/restart/read_sensor_tab.F
Chd|        THMONV                        source/output/th/thmonv.F     
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE FRETITL2(TITR,IASC,L)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
      INTEGER L,IASC(*)
      CHARACTER TITR*(*)
      INTEGER I,J,LTITR
C
      J=1
      LTITR=LEN(TITR)
      DO I=1,L
        TITR(J:J)=CHAR(IASC(I)/65536)
        J=J+1
        IF(J>LTITR) GOTO 10
        TITR(J:J)=CHAR(MOD(IASC(I),65536)/256)
        J=J+1
        IF(J>LTITR) GOTO 10
        TITR(J:J)=CHAR(MOD(IASC(I),256))
        J=J+1
        IF(J>LTITR) GOTO 10
      ENDDO
 10   CONTINUE
      RETURN
      END
Chd|====================================================================
Chd|  FRETITLR                      source/input/freform.F        
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE FRETITLR(TITR,ASC,L)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
      my_real
     . ASC(*)
      INTEGER L
      CHARACTER TITR*(*)
      INTEGER I,J,LTITR
C
      J=1
      LTITR=LEN(TITR)
      DO I=1,L
        ASC(I)=  ICHAR(TITR(J:J))*65536
        J = J+1
        IF(J>LTITR) GOTO 10
        ASC(I)=  ASC(I) + ICHAR(TITR(J:J))*256
        J = J+1
        IF(J>LTITR) GOTO 10
        ASC(I)=  ASC(I) + ICHAR(TITR(J:J))
        J = J+1
        IF(J>LTITR) GOTO 10
      ENDDO
 10   CONTINUE
C
      RETURN
      END
Chd|====================================================================
Chd|  FRETITL2R                     source/input/freform.F        
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE FRETITL2R(TITR,ASC,L)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
      my_real
     . ASC(*)
      INTEGER L
      CHARACTER TITR*(*)
      INTEGER I,J,LTITR
C
      J=1
      LTITR=LEN(TITR)
      DO I=1,L
        TITR(J:J)=CHAR(NINT(ASC(I))/65536)
        J=J+1
        IF(J>LTITR) GOTO 10
        TITR(J:J)=CHAR(MOD(NINT(ASC(I)),65536)/256)
        J=J+1
        IF(J>LTITR) GOTO 10
        TITR(J:J)=CHAR(MOD(NINT(ASC(I)),256))
        J=J+1
        IF(J>LTITR) GOTO 10
      ENDDO
 10   CONTINUE
      RETURN
      END
