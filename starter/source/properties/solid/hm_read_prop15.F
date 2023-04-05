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
Chd|  HM_READ_PROP15                source/properties/solid/hm_read_prop15.F
Chd|-- called by -----------
Chd|        HM_READ_PROPERTIES            source/properties/hm_read_properties.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        HM_GET_FLOATV                 source/devtools/hm_reader/hm_get_floatv.F
Chd|        HM_GET_INTV                   source/devtools/hm_reader/hm_get_intv.F
Chd|        HM_OPTION_IS_ENCRYPTED        source/devtools/hm_reader/hm_option_is_encrypted.F
Chd|        HM_READ_ALE_CLOSE             source/ale/hm_read_ale_close.F
Chd|        USR2SYS                       source/system/sysfus.F        
Chd|        ALE_MOD                       ../common_source/modules/ale/ale_mod.F
Chd|        ELBUFTAG_MOD                  share/modules1/elbuftag_mod.F 
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE HM_READ_PROP15 (IG , IGTYP , GEO   ,  IGEO   ,PROP_TAG  ,UNITAB ,
     .                           LSUBMODEL,IDTITL ,ISKN  ,ITABM1    )
C============================================================================
C   M o d u l e s
C-----------------------------------------------
      USE UNITAB_MOD
      USE ELBUFTAG_MOD            
      USE SUBMODEL_MOD
      USE MESSAGE_MOD
      USE ALE_MOD
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
#include      "scr17_c.inc"
#include      "scr21_c.inc"
#include      "tablen_c.inc"
#include      "submod_c.inc"
#include      "sphcom.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      TYPE (UNIT_TYPE_),INTENT(IN) ::UNITAB 
      INTEGER IGTYP , IGEO(*) ,ISKN(LISKN,*) ,ITABM1(*)
      TYPE(SUBMODEL_DATA) LSUBMODEL(*)
      my_real GEO(*)
      CHARACTER IDTITL*nchartitle
      TYPE(PROP_TAG_) , DIMENSION(0:MAXPROP) :: PROP_TAG
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER IG, ISMSTR, NIP, J,K ,NPTS , 
     .        IHBE,ISH3N,ISROT ,I8PT ,ISK,IHON ,ITU ,IRB,
     .        IGFLU ,IHBE_OLD
     
      my_real  ANGL,PUN,CVIS,RBID,VX,VY,VZ,FAC_L,FAC_T,FAC_M, PTHK, AN, PHI
      CHARACTER*nchartitle,TITR
      CHARACTER MESS*40,KEY*ncharkey
      LOGICAL IS_AVAILABLE, IS_ENCRYPTED
C-----------------------------------------------
C   E x t e r n a l   F u n c t i o n s
C-----------------------------------------------
      INTEGER USR2SYS
      DATA PUN/0.1/
      DATA MESS/'PID DEFINITION                          '/
C------------------------------
C      POROUS SOLID
c------------------------------
        IHBE=0
        ISMSTR=0
        ISROT=0
        IGFLU=1
        CVIS =ZERO

        IS_ENCRYPTED = .FALSE.
        IS_AVAILABLE = .FALSE.
C--------------------------------------------------
C EXTRACT DATA (IS OPTION CRYPTED)
C--------------------------------------------------
      CALL HM_OPTION_IS_ENCRYPTED(IS_ENCRYPTED)
C--------------------------------------------------
C EXTRACT DATAS (INTEGER VALUES)
C--------------------------------------------------
      CALL HM_GET_INTV('SKEW_CSID',ISK,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV('MAT_Iflag',IHON,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV('I_TH',ITU,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV('IRBY',IRB,IS_AVAILABLE,LSUBMODEL)

C--------------------------------------------------
C EXTRACT DATAS (REAL VALUES)
C--------------------------------------------------
      CALL HM_GET_FLOATV('qa_l',GEO(14),IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('qb_l',GEO(15),IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('h_l',GEO(13),IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('MAT_POROS',GEO(21),IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('MAT_PDIR1',GEO(24),IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('MAT_PDIR2',GEO(25),IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('MAT_PDIR3',GEO(26),IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('ALPHA1',GEO(22),IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('THICK',GEO(23),IS_AVAILABLE,LSUBMODEL,UNITAB)

c         CALL FRETITL(IDTITL,IGEO(NPROPGI-LTITR+1),LTITR)
c         WRITE(IOUT,'(A40)') IDTITL
C        Double stockage temporaire - supprimer GEO(12,I)=IGTYP apres tests
         IGEO(1) = IG
         IGEO(11)= IGTYP
         GEO(12) = IGTYP+PUN

	 IF(ALE%GLOBAL%ICAA==0 .AND. IGFLU==0)THEN
	   IF(GEO(14)==ZERO) GEO(14)=ONEP1
	   IF(GEO(15)==ZERO) GEO(15)=FIVEEM2
	 ENDIF
	 IF(GEO(13)==ZERO)GEO(13)=EM01
	 IF(IHBE==0)THEN
	   IHBE=IHBE_DS
	 ENDIF
	 I8PT=0
C
	 IF(ISMSTR==0)ISMSTR=ISST_DS
         IF (ISMSTR < 0.OR.ISST_DS==-2) ISMSTR=4                            
         IF(ISMSTR==0)ISMSTR=4
         IF(ISMSTR==3)GEO(5)=EP06
         GEO(3)   =ISMSTR
         IGEO(5)  = ISMSTR
C        Double stockage temporaire - supprimer GEO(12,I)=IGTYP apres tests
         IGEO(10)=IHBE
         GEO(171)=IHBE
c
         IF(IHBE==12)THEN
           I8PT=1
           IHBE=0
         ELSEIF(IHBE==13)THEN
           I8PT=1
         ELSEIF(IHBE==112)THEN
           I8PT=1
         ELSEIF(IHBE>=222)THEN
           I8PT=1
         ENDIF
         GEO(1)=1
         IF(N2D==0.AND.I8PT==1) GEO(1)=8
         IF(IABS(IHBE)>=222) GEO(1)=IHBE
         IF(N2D>0.AND.I8PT==1)THEN
           GEO(1)=4
           CALL ANCMSG(MSGID=323,
     .                 MSGTYPE=MSGWARNING,
     .                 ANMODE=ANINFO_BLIND_2,
     .                 I1=IG,
     .                 C1=IDTITL)
         ENDIF
         IF(N2D>0.AND.IHBE/=0.AND.IHBE/=2)THEN
            IHBE_OLD=IHBE
            IHBE=0
            CALL ANCMSG(MSGID=324,
     .                  MSGTYPE=MSGWARNING,
     .                  ANMODE=ANINFO_BLIND_2,
     .                  I1=IG,
     .                  C1=IDTITL,
     .                  I2=IHBE_OLD,
     .                  I3=IHBE)
         ENDIF
         IF(IHBE>=3.AND.IHBE<13.AND.IHBE/=4) IHBE=1
         GEO(171)=IHBE
         IF(IHBE>1000.AND.IHBE<1050) THEN
          NPTS=IHBE-1000
         ELSEIF(IABS(IHBE)>=222) THEN
          NPTS=IABS(IHBE)/100*MOD(IABS(IHBE)/10,10)*MOD(IABS(IHBE),10)
         ELSE
          NPTS=NINT(GEO(1))
         ENDIF
         IGEO(4)  = NPTS
         IGEO(10) = IHBE
C----------------------
         IF(GEO(21)==0.) GEO(21)=ONE
         ITU=MIN(ITU,1)
         IF(ITU==1)THEN
           IF(GEO(22)==ZERO)GEO(22)=EM01
           IF(GEO(23)==ZERO)THEN
            GEO(23)=EM20
            IWARN = IWARN + 1
            WRITE(IOUT,*)
     .      ' MIXING LENGTH REQUIRED IF TURBULENCE',
     .      ' IS IMPOSED BY POROUS MEDIUM'
           ENDIF
         ENDIF
C
         DO K=0,NUMSKW+MIN(1,NSPCOND)*NUMSPH+NSUBMOD
           IF(ISK == ISKN(4,K+1)) THEN
             ISK=K+1
             GO TO 10
           ENDIF
         ENDDO
         CALL ANCMSG(MSGID=137,ANMODE=ANINFO,MSGTYPE=MSGERROR,
     .               C1='PROPERTY',
     .               C2='PROPERTY',
     .               I1=IG,I2=ISK,C3=IDTITL)
10       CONTINUE
C
         GEO(27)=ISK + EM01
         GEO(28)=ITU + EM01
         IF(IRB/=0)THEN
          GEO(29)=USR2SYS(IRB,ITABM1,MESS,IG)+PUN
         ELSE
          GEO(29)=0
         ENDIF
         GEO(30)=IHON+EM01
         IF(GEO(24)+GEO(25)+GEO(26)==ZERO)GEO(20)=ONEP1
         WRITE(IOUT,1800)IG,NINT(GEO(1)),IHBE,GEO(14),GEO(15),
     .         GEO(13),GEO(21),(GEO(J),J=24,26),ISKN(4,ISK),
     .         IHON,IRB
         IF(ITU==1) WRITE(IOUT,1850)GEO(22),GEO(23)

         IF(GEO( 3)/=ZERO.AND.IGEO( 5)== 0)IGEO( 5)=NINT(GEO( 3))
         IF(GEO(39)/=ZERO.AND.IGEO( 9)== 0)IGEO( 9)=NINT(GEO(39))
         IF(GEO(171)/=ZERO.AND.IGEO(10)== 0)
     .      IGEO(10)=NINT(GEO(171))

         IF (GEO(16) /= ZERO .OR. GEO(17) /= ZERO) THEN
           IGEO(33) = 1   ! ISVIS flag
         ENDIF         

!     /ALE/CLOSE
!     ----------
         CALL HM_READ_ALE_CLOSE(UNITAB, LSUBMODEL, GEO)

C--------   Variables stored in element buffer 
c----   Solids
          PROP_TAG(IGTYP)%G_SIG  = 6
          PROP_TAG(IGTYP)%G_VOL  = 1
          PROP_TAG(IGTYP)%G_EINT = 1
          PROP_TAG(IGTYP)%G_QVIS = 1
          PROP_TAG(IGTYP)%L_SIG  = 6
          PROP_TAG(IGTYP)%L_EINT = 1
          PROP_TAG(IGTYP)%L_VOL  = 1
          PROP_TAG(IGTYP)%L_QVIS = 1
          PROP_TAG(IGTYP)%G_FILL = 1
          PROP_TAG(IGTYP)%L_STRA = 6 
C-----------
      RETURN
C-----------
 1800 FORMAT(
     & 5X,'POROUS FLUID PROPERTY SET'/,
     & 5X,'PROPERTY SET NUMBER . . . . . . . . . .=',I10/,
     & 5X,'NUMBER OF GAUSS POINT . . . . . . . . .=',I10/,
     & 5X,'HOURGLASS BELYTSHKO . . . . . . . . . .=',I10/,
     & 5X,'QUADRATIC BULK VISCOSITY. . . . . . . .=',1PG20.13/,
     & 5X,'LINEAR BULK VISCOSITY . . . . . . . . .=',1PG20.13/,
     & 5X,'HOURGLASS VISCOSITY . . . . . . . . . .=',1PG20.13/,
     & 5X,'POROSITY  . . . . . . . . . . . . . . .=',1PG20.13/,
     & 5X,'RESISTANCE FACTOR DIR 1 . . . . . . . .=',1PG20.13/,
     & 5X,'RESISTANCE FACTOR DIR 2 . . . . . . . .=',1PG20.13/,
     & 5X,'RESISTANCE FACTOR DIR 3 . . . . . . . .=',1PG20.13/,
     & 5X,'SKEW NUMBER AS REFERENCE FRAME  . . . .=',I10/,
     & 5X,'FLAG FOR HONEYCOMB IN DIR 1 . . . . . .=',I10/,
     & 5X,'RIGID BODY NUMBER TO WHICH',/,
     & 5X,'      SUBSTRATE REACTION IS APPLIED . .=',I10/)
 1850 FORMAT(
     & 5X,'TURBULENCE IS IMPOSED BY POROUS MEDIUM'/,
     & 5X,'TURBULENT FLUCTUATION COEFF . . . . . .=',1PG20.13/,
     & 5X,'MIXING LENGTH . . . . . . . . . . . . .=',1PG20.13/)
C-----------

      END




