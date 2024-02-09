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
Chd|====================================================================
Chd|  HM_READ_PART                  source/model/assembling/hm_read_part.F
Chd|-- called by -----------
Chd|        LECTUR                        source/starter/lectur.F       
Chd|-- calls ---------------
Chd|        ALE_EULER_INIT                source/materials/ale/ale_euler_init.F
Chd|        ANCMSG                        source/output/message/message.F
Chd|        FRETITL                       source/starter/freform.F      
Chd|        FRETITL2                      source/starter/freform.F      
Chd|        HM_GET_FLOATV                 source/devtools/hm_reader/hm_get_floatv.F
Chd|        HM_GET_INTV                   source/devtools/hm_reader/hm_get_intv.F
Chd|        HM_OPTION_READ_KEY            source/devtools/hm_reader/hm_option_read_key.F
Chd|        HM_OPTION_START               source/devtools/hm_reader/hm_option_start.F
Chd|        UDOUBLE                       source/system/sysfus.F        
Chd|        GET_U_GEO                     source/user_interface/uaccess.F
Chd|        NINTRI                        source/system/nintrr.F        
Chd|        ALE_MOD                       ../common_source/modules/ale/ale_mod.F
Chd|        ELBUFTAG_MOD                  share/modules1/elbuftag_mod.F 
Chd|        HM_OPTION_READ_MOD            share/modules1/hm_option_read_mod.F
Chd|        MAT_ELEM_MOD                  ../common_source/modules/mat_elem/mat_elem_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        MULTI_FVM_MOD                 ../common_source/modules/ale/multi_fvm_mod.F
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE HM_READ_PART(IPART,PM,GEO,IPM,IGEO,IWA,THK_PART,
     .                        UNITAB,LSUBMODEL,MULTI_FVM,MLAW_TAG,
     .                        MAT_PARAM)
C-----------------------------------------------
C   ROUTINE DESCRIPTION :
C   ===================
C   READ /PART USING HM_READER
C-----------------------------------------------
C   DUMMY ARGUMENTS DESCRIPTION:
C   ===================
C
C     NAME            DESCRIPTION                         
C
C     IPART           PART ARRAY 
C     PM              MATERIAL ARRAY(REAL)
C     GEO             PROPERTY ARRAY(REAL)
C     IPM             MATERIAL ARRAY(INTEGER)
C     IGEO            PROPERTY ARRAY(INTEGER)
C     THK_PART        VIRTUAL THICKNESS FOR PART ( USE BY CONTACT INTERFACES )
C     UNITAB          UNITS ARRAY
C     LSUBMODEL       SUBMODEL STRUCTURE    
C============================================================================
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE UNITAB_MOD
      USE MESSAGE_MOD           
      USE SUBMODEL_MOD           
      USE HM_OPTION_READ_MOD
      USE MULTI_FVM_MOD
      USE ELBUFTAG_MOD 
      USE ALE_MOD
      USE MAT_ELEM_MOD            
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "scr17_c.inc"
#include      "units_c.inc"
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "com_xfem1.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
C INPUT ARGUMENTS
      TYPE (UNIT_TYPE_),INTENT(IN) ::UNITAB 
      my_real,INTENT(IN)::GEO(NPROPG,NUMGEO)
      TYPE(SUBMODEL_DATA),INTENT(IN)::LSUBMODEL(*)
C OUTPUT ARGUMENTS
      INTEGER,INTENT(OUT)::IPART(LIPART1,*)
      INTEGER,INTENT(OUT)::IWA(*)
      my_real,INTENT(OUT)::THK_PART(*)
C MODIFIED ARGUMENT
      INTEGER,INTENT(INOUT)::IGEO(NPROPGI,NUMGEO)
      INTEGER,INTENT(INOUT)::IPM(NPROPMI,NUMMAT)
      my_real,INTENT(INOUT)::PM(NPROPM,NUMMAT)
      TYPE(MULTI_FVM_STRUCT),INTENT(INOUT)::MULTI_FVM     
      TYPE(MLAW_TAG_) , DIMENSION(NUMMAT) , INTENT(INOUT) :: MLAW_TAG      
      TYPE (MATPARAM_STRUCT_) ,DIMENSION(NUMMAT) ,INTENT(INOUT) :: MAT_PARAM
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      CHARACTER MESS*40
      CHARACTER*nchartitle,TITR,TITR1,TITR2,LINE1
      CHARACTER*5 CHAR_PROP,CHAR_MAT
      CHARACTER*7::CHAR_MAT_TYPE,CHAR_PROP_TYPE
      LOGICAL IS_AVAILABLE, USER_LAW, IS_ASSOCIATED_LAW51
      INTEGER PID,MID,SID,ID,ID1,ID2,I,IMID,IPID,ISID,K,ITH, IGTYP,XFEMFLG,
     .        IXFEM,IHBE,ILAW,UID,IFLAGUNIT,J,IDMAT_PLY,
     .        ILAW_PLY,IPMAT,NPT,IDPARTSPH,SUB_INDEX,SIZE, IDS, CNT, 
     .        IFIX_TMP,STAT,JALE_FROM_PROP,JALE_FROM_MAT
      my_real BID, THICK,FAC_L,MP,VOL,DIAM
C-----------------------------------------------
C   E x t e r n a l   F u n c t i o n s
C-----------------------------------------------
      my_real GET_U_GEO
      EXTERNAL GET_U_GEO
      INTEGER NINTRI
      DATA MESS/' PART DEFINITION                        '/
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------

      TITR1 = REPEAT(" ",nchartitle)
      TITR2 = REPEAT(" ",nchartitle)
      TITR = REPEAT(" ",nchartitle)
      LINE1 = REPEAT(" ",nchartitle)
      CHAR_PROP = REPEAT(" ",5)
      CHAR_MAT = REPEAT(" ",5)
      CHAR_MAT_TYPE = REPEAT(" ",7)
      CHAR_PROP_TYPE = REPEAT(" ",7)


      IS_ASSOCIATED_LAW51 = .FALSE.
      IS_AVAILABLE = .FALSE.
      SUB_INDEX = 0
      UID = 0
      FAC_L = ONE
      XFEMFLG = 0
      IXFEM   = 0

      WRITE(IOUT,'(//A)')'       PARTS' 
      WRITE(IOUT,'(A//)')'       -----' 

      DO I=1,NUMGEO
         IWA(I) = 0
      ENDDO
      DO I=1,NUMMAT
         IWA(NUMGEO+I) = 0
      ENDDO
C--------------------------------------------------
C ALE or EULER CONVECTION CODES (CONVECTION/REZONING/EBCS)
C--------------------------------------------------      
        ALE%GLOBAL%CODV(1:ALE%GLOBAL%LCONV)=0      
C--------------------------------------------------
C START BROWSING MODEL PARTS
C--------------------------------------------------
      CALL HM_OPTION_START('PART')
C--------------------------------------------------
C BROWSING MODEL PARTS 1->NPART
C--------------------------------------------------
      DO I=1,NPART
        TITR = ''
C--------------------------------------------------
C EXTRACT DATAS OF /PART/... LINE
C--------------------------------------------------
        CALL HM_OPTION_READ_KEY(LSUBMODEL,
     .                          OPTION_ID = ID,
     .                          UNIT_ID = UID,
     .                          SUBMODEL_INDEX = SUB_INDEX,
     .                          OPTION_TITR = TITR)
C--------------------------------------------------
C EXTRACT DATAS (INTEGER VALUES)
C--------------------------------------------------
        CALL HM_GET_INTV('propertyid',PID,IS_AVAILABLE,LSUBMODEL)
        CALL HM_GET_INTV('materialid',MID,IS_AVAILABLE,LSUBMODEL)
        CALL HM_GET_INTV('subsetid',SID,IS_AVAILABLE,LSUBMODEL)
C--------------------------------------------------
C EXTRACT DATAS (REAL VALUES)
C--------------------------------------------------
        CALL HM_GET_FLOATV('THICK',THICK,IS_AVAILABLE,LSUBMODEL,UNITAB)
C--------------------------------------------------

        CALL FRETITL(TITR,IPART(LIPART1-LTITR+1,I),LTITR)

        THK_PART(I) = THICK
        
C--------------------------------------------------
C MATERIAL & PROPERTY CHECKS
C--------------------------------------------------        
        IPID = NINTRI(PID,IGEO,NPROPGI,NUMGEO,1)
        IF(IPID == 0) THEN
           IPID=1
           CALL ANCMSG(MSGID=178,MSGTYPE=MSGERROR,ANMODE=ANINFO,I1=ID,C1=TITR,I2=PID)
           TITR1=' '
        ELSE
            CALL FRETITL2(TITR1,IGEO(NPROPGI-LTITR+1,IPID),LTITR)
        ENDIF

        IGTYP=NINT(GEO(12,IPID))
        IF(IGTYP == 17 .OR. IGTYP == 51) IPART_STACK = 1
        IF(IGTYP ==  52) IPART_PCOMPP = 1
        IF( (IGTYP == 0).OR.
     .      (IGTYP == 1).OR.(IGTYP == 2).OR.(IGTYP == 3).OR.
     .      (IGTYP == 6).OR.(IGTYP == 9).OR.(IGTYP == 10).OR.
     .      (IGTYP == 11).OR.(IGTYP == 14).OR.(IGTYP == 16).OR.
     .      (IGTYP == 18).OR.(IGTYP == 20).OR.(IGTYP == 21).OR.
     .      (IGTYP == 22).OR.(IGTYP == 34).OR.(IGTYP == 11).OR.
     .      (IGTYP == 17).OR.(IGTYP == 51).OR.(IGTYP == 52).OR. 
     .      (IGTYP == 23).OR.(IGTYP == 43)) THEN
            IF(MID == 0) THEN
               CALL ANCMSG(MSGID=179,
     .                     MSGTYPE=MSGERROR,
     .                     ANMODE=ANINFO,
     .                     I1=ID,
     .                     C1=TITR,
     .                     I2=MID)
            ENDIF          
        ENDIF
        !--- check material identifier
        IF(MID == 0) THEN
         !fictitious material law for spring elements
         IMID=NUMMAT
         ILAW=IPM(2,IMID)
        ELSE
         IMID = NINTRI(MID,IPM,NPROPMI,NUMMAT,1)
         IF(IMID == 0) THEN
            CALL ANCMSG(MSGID=179,
     .                  MSGTYPE=MSGERROR,
     .                  ANMODE=ANINFO,
     .                  I1=ID,
     .                  C1=TITR,
     .                  I2=MID)
           ILAW=0
         ELSE
           ILAW  = IPM(2,IMID)
           IXFEM = MAT_PARAM(IMID)%IXFEM
           CALL FRETITL2(TITR2,IPM(NPROPMI-LTITR+1,IMID),LTITR)
         ENDIF
         !check if law151 is used
         IF(ILAW == 151)IS_ASSOCIATED_LAW51=.TRUE.
                
         !--- check property identifier
         IGTYP=0
         IF(IPID > 0) IGTYP=IGEO(11,IPID)
         IF (IXFEM > 0 .and. (IGTYP==1  .or. IGTYP==9 .or. IGTYP==10 .or. 
     .                        IGTYP==11 .or. IGTYP==51)) THEN
           XFEMFLG = XFEMFLG + IXFEM
         END IF
         IF (ILAW == 99.AND.IGTYP == 14) THEN
           IHBE=IGEO(10,IPID)
           IF (IHBE == 12) THEN
            CALL ANCMSG(MSGID=768,
     .                  MSGTYPE=MSGERROR,
     .                  ANMODE=ANINFO,
     .                  I1=ID,
     .                  C1=TITR,
     .                  I2=PID,
     .                  C2=TITR1,
     .                  I3=MID,
     .                  C3=TITR2,
     .                  C4='SOLID',
     .                  I4=IHBE)
           END IF
         END IF
         !tag for user material law
         IF (ILAW==29 .or. ILAW==30 .or. ILAW==31 .or. ILAW==99) THEN
           USER_LAW = .true.
         ELSE
           USER_LAW = .false.
         ENDIF
         
         !check compatibility between material law and property
         IF (((IGTYP==43) .and. ((ILAW/=59 .and. ILAW/=83 .and. ILAW/=116 .and. ILAW/=117 .AND. ILAW /=120) .and. 
     .       (USER_LAW .eqv. .false. ) ).eqv. .true.) .or.
     .       ((ILAW==59 .or. ILAW==83 .or. ILAW==116 .or. ILAW==117) .and. IGTYP/=43) .or.
     .        (ILAW==1 .and. (IGTYP==9.OR.IGTYP==10.OR.IGTYP==11.OR.IGTYP==16.OR.
     .                        IGTYP==17.OR.IGTYP==51.OR.IGTYP==52) .eqv. .true.) .eqv. .true.) THEN
           CALL ANCMSG(MSGID=658,
     .                 MSGTYPE=MSGERROR,
     .                 ANMODE=ANINFO_BLIND_2,
     .                 I1=PID,
     .                 C1=TITR1,
     .                 I2=ILAW,
     .                 I3=IGTYP)
         ENDIF
         
         !anisotropic material law not compatible with isotropic property
         IF (ILAW == 87 .AND. IGTYP /= 9) THEN
           CALL ANCMSG(MSGID=1110,
     .                 MSGTYPE=MSGWARNING,
     .                 ANMODE=ANINFO_BLIND_1,
     .                 I1=ID,
     .                 C1=TITR,
     .                 I2=ILAW,
     .                 I3=IGTYP)
         ENDIF
         IF (ILAW == 187 .AND. IGTYP /= 6) THEN
           CALL ANCMSG(MSGID=1110,
     .                 MSGTYPE=MSGWARNING,
     .                 ANMODE=ANINFO_BLIND_1,
     .                 I1=ID,
     .                 C1=TITR,
     .                 I2=ILAW,
     .                 I3=IGTYP)
         ENDIF

         !rigid material law (obsolete)
         IF(ILAW == 13 .AND. IRODDL == 0) IRODDL = 1
         
        ENDIF

        ! compatibility of global material and ply material for type11
        IF(IGTYP == 11) THEN
            NPT=IGEO(4,IPID)
            IPMAT = 100
           DO J=1,NPT
             IDMAT_PLY= IGEO(IPMAT+J,IPID)
             ILAW_PLY = IPM(2,IDMAT_PLY)
             IF(ILAW_PLY /= ILAW) THEN
                 CALL ANCMSG(MSGID=1083,
     .                  MSGTYPE=MSGERROR,
     .                  ANMODE=ANINFO,
     .                  I1=ID,
     .                  C1=TITR,
     .                  I2=PID,
     .                  C2=TITR1,
     .                  I3=MID,
     .                  C3=TITR2)
             ENDIF
           ENDDO
        ENDIF  
              
        !spring type 23 & material compatibility
        IF(IGTYP == 23) THEN
           IMID = NINTRI(MID,IPM,NPROPMI,NUMMAT,1)
           ILAW=IPM(2,IMID)
           IF(ILAW /= 108 .AND. ILAW /=113.AND. ILAW /=114 .AND. ILAW /= 0 ) THEN
             CALL ANCMSG(MSGID = 1715,
     .                  MSGTYPE=MSGERROR,
     .                  ANMODE=ANINFO,
     .                  I1=ID,
     .                  C1=TITR)
           ENDIF
        ENDIF

        ! law70 (/MAT/FOAM_TAB)
        IF(ILAW == 70 .AND. IGEO(31,IPID) == 1) WRITE(IOUT,2000)

c-------------------------------------------------------------------
c      ALE EULER SPECIFIC TREATMENTS
c-------------------------------------------------------------------
        !SSP BUFFER + UPWIND + TURB + CHECK + CONVECTION FLAGS
        CALL ALE_EULER_INIT( MLAW_TAG,IPM,PM,IGEO,TITR,TITR1,TITR2,IGTYP,ID,ILAW,MID,IMID,PID,IPID,JALE_FROM_PROP,JALE_FROM_MAT)           

c-------------------------------------------------------------------
c      STARTER PRINTOUT
c-------------------------------------------------------------------
        WRITE(IOUT,'(/A,I10,2A)')'PART:',ID,',',TRIM(TITR)
        WRITE(IOUT,'(A)')        '----'
        
C----PROPERTY OUTPUT  
        CHAR_PROP_TYPE='TYPE ? '
        IF(IPID>0)THEN
          WRITE(CHAR_PROP_TYPE(5:7),FMT='(I3)')IGTYP
          IF(IGTYP<10)WRITE(CHAR_PROP_TYPE(6:6),FMT='(A1)') '0'
        ENDIF
        WRITE(IOUT,'(A,I10,4A)')'     PROPERTY    :',PID,' (',TRIM(CHAR_PROP_TYPE),'),',TRIM(TITR1)
        
C----MATERIAL OUTPUT
        CHAR_MAT_TYPE='LAW  ? '
        IF(IMID>0)THEN
          WRITE(CHAR_MAT_TYPE(5:7),FMT='(I3)')ILAW
          IF(ILAW<10)WRITE(CHAR_MAT_TYPE(6:6),FMT='(A1)') '0'
        ENDIF         
        IF( IMID /= 0) WRITE(IOUT,'(A,I10,4A)')'     MATERIAL    :',MID,' (',TRIM(CHAR_MAT_TYPE),'),',TRIM(TITR2)
                 
C----SUBSET OUTPUT            
        WRITE(IOUT,'(A,I10,2A)')'     SUBSET      :',SID
      
C----FRAMEWORK OUTPUT        
        IF(JALE_FROM_PROP==1 .OR. JALE_FROM_MAT==1)THEN
          WRITE(IOUT,'(A)')'     FRAMEWORK   :         ALE'
        ELSEIF(JALE_FROM_PROP==2 .OR. JALE_FROM_MAT==2)THEN       
          WRITE(IOUT,'(A)')'     FRAMEWORK   :         EULER'    
        ELSE
          WRITE(IOUT,'(A)')'     FRAMEWORK   :         LAGRANGE'
        ENDIF    

C----VIRTUAL THICKNESS OUTPUT  (For properties which are compatible with shell elements : /SHELL and /SH3N) 
        IF( (IGTYP == 0).OR.
     .      (IGTYP == 1).OR.(IGTYP == 9).OR.(IGTYP == 10).OR.
     .      (IGTYP == 11).OR.(IGTYP == 16).OR.(IGTYP == 17).OR.
     .      (IGTYP == 19).OR.(IGTYP == 51).OR.(IGTYP == 52)) THEN      
          WRITE(IOUT,'(A,1PG20.13,2A)')'     VIRT. THICKN:       ',THK_PART(I)
        ENDIF
C----VIRTUAL THICKNESS OUTPUT  (extended to /BEAM /TRUSS /SPRING) 
        IF( THK_PART(I)>ZERO .AND. ((IGTYP == 3).OR.(IGTYP == 2).OR.
     .      (IGTYP == 18).OR.(IGTYP == 4).OR.(IGTYP == 8).OR.
     .      (IGTYP == 12).OR.(IGTYP == 13).OR.(IGTYP == 23).OR.
     .      (IGTYP == 25).OR.(IGTYP == 26).OR.(IGTYP == 27))) THEN      
          WRITE(IOUT,'(A,1PG20.13,2A)')'     VIRT. THICKN:       ',THK_PART(I)
        ENDIF

C----SPH SMOOTHING LENGTH OUTPUT ( /PROP/SPH (Type34) )   
        IF (IGEO(11,IPID) == 34) THEN
          DIAM =GET_U_GEO(6,IPID)
          IF(DIAM == ZERO) THEN
            MP  = GET_U_GEO(1,IPID)
            VOL = MP/PM(1,IMID)
            DIAM= (SQR2*VOL)**THIRD
            WRITE(IOUT,'(A,1PG20.13,2A)')' SPH SMOOTHING LENGTH:   ',DIAM
          ENDIF
        ENDIF

c-------------------------------------------------------------------
c      STORAGE
c-------------------------------------------------------------------      
        IPART(1,I)=IMID
        IPART(2,I)=IPID
        ISID=0
        IPART(3,I)=ISID
        IPART(4,I)=ID
        IPART(5,I)=MID
        IPART(6,I)=PID
        IPART(7,I)=SID
        ITH=0
        IPART(8,I)=ITH
        IPART(9,I)=SUB_INDEX

        !ensure positive identifier
        IF(IPART(4,I) == 0) THEN
          CALL ANCMSG(MSGID=494,MSGTYPE=MSGERROR,ANMODE=ANINFO_BLIND_1,C1=LINE1)
        ENDIF

      ENDDO ! NPART
      
C--------------------------------------------------
C ALE or EULER CONVECTION CODES (CONVECTION/REZONING/EBCS)
C--------------------------------------------------   
      ALE%GLOBAL%NVCONV=0
      DO I=1,ALE%GLOBAL%LCONV
        IF(ALE%GLOBAL%CODV(I) == 1)THEN
          ALE%GLOBAL%NVCONV=ALE%GLOBAL%NVCONV+1
          ALE%GLOBAL%CODV(I)=ALE%GLOBAL%NVCONV
        ENDIF
      ENDDO      
C--------------------------------------------------
C LAW151 - MULTIFLUID
C--------------------------------------------------   
      MULTI_FVM%IS_USED = IS_ASSOCIATED_LAW51
      IMULTI_FVM = 0
      IF (MULTI_FVM%IS_USED) THEN
         IMULTI_FVM = 1     
         IF (N2D == 0) THEN
            ALLOCATE(MULTI_FVM%VEL(3, NUMELS), STAT=stat)
         ELSE
            ALLOCATE(MULTI_FVM%VEL(3, NUMELQ + NUMELTG), STAT=stat)
         ENDIF
         IF(STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,MSGTYPE=MSGERROR,C1='MULTI_FVM%VEL')
         MULTI_FVM%VEL(: ,:) = ZERO
      ENDIF      
C--------------------------------------------------         
      IF (XFEMFLG == 0) ICRACK3D = 0
C--------------------------------------------------   
      DO I=1,NPART
         IWA(IPART(2,I)) = 1
         IWA(NUMGEO+IPART(1,I)) = 1
      ENDDO
C--------------------------------------------------   
      CNT = 0
      DO I=1,NUMGEO
        IF (IWA(I) ==  0) CNT = CNT+1
      ENDDO
      IDS = 52
      CNT = 0
      DO I=1,NUMMAT
        IF (IWA(NUMGEO+I) ==  0) CNT = CNT+1
      ENDDO
      IDS = 3
C-------------------------------------        
C     SOL2SPH : Orthotropic flag transferred to SPH property
C-------------------------------------
      DO I=1,NPART
        IDPARTSPH = IGEO(38,IPART(2,I))
        IF (IDPARTSPH > 0) THEN
          IGEO(17,IPART(2,IDPARTSPH)) = IGEO(17,IPART(2,I))
        ENDIF
      ENDDO
C-------------------------------------
C     DUPLICATED IDs
C-------------------------------------
      CALL UDOUBLE(IPART(4,1),LIPART1,NPART,MESS,0,BID)
C-------------------------------------
      RETURN
 2000 FORMAT(5X,'FOR LAW 70 THE DEFAULT VALUE OF Qa and Qb IS 0' )
C
      END
Chd|====================================================================
Chd|  HM_PREREAD_PART               source/model/assembling/hm_read_part.F
Chd|-- called by -----------
Chd|        CONTRL                        source/starter/contrl.F       
Chd|        LECTUR                        source/starter/lectur.F       
Chd|-- calls ---------------
Chd|        HM_GET_INTV                   source/devtools/hm_reader/hm_get_intv.F
Chd|        HM_OPTION_READ_KEY            source/devtools/hm_reader/hm_option_read_key.F
Chd|        HM_OPTION_START               source/devtools/hm_reader/hm_option_start.F
Chd|        NINTRI                        source/system/nintrr.F        
Chd|        HM_OPTION_READ_MOD            share/modules1/hm_option_read_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE HM_PREREAD_PART(IPART,IGEO,LSUBMODEL)
C============================================================================
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD       
      USE SUBMODEL_MOD
      USE HM_OPTION_READ_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "scr17_c.inc"
#include      "com04_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IPART(LIPART1,*),IGEO(NPROPGI,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER PID,ID,I,IPID,UID,SUB_INDEX
      CHARACTER MESS*40
      INTEGER  IFIX_TMP
      CHARACTER*nchartitle,TITR
      TYPE(SUBMODEL_DATA) LSUBMODEL(NSUBMOD)
      LOGICAL IS_AVAILABLE
C-----------------------------------------------
C   E x t e r n a l   F u n c t i o n s
C-----------------------------------------------
      INTEGER NINTRI
      DATA MESS/' PART PRE-READING                       '/
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------
      IS_AVAILABLE = .FALSE.
      CALL HM_OPTION_START('PART')
C--------------------------------------------------
      DO I=1,NPART
        TITR = ''
        CALL HM_OPTION_READ_KEY(LSUBMODEL,
     .                       OPTION_ID = ID,
     .                       UNIT_ID = UID,
     .                       SUBMODEL_INDEX = SUB_INDEX,
     .                       OPTION_TITR = TITR)
        CALL HM_GET_INTV('propertyid',PID,IS_AVAILABLE,LSUBMODEL)
        IPID = NINTRI(PID,IGEO,NPROPGI,NUMGEO,1)
        IPART(2,I)=IPID
        IPART(4,I)=ID
        IPART(9,I)=SUB_INDEX
      ENDDO

      RETURN

      END
