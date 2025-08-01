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
!||    hm_read_gauge          ../starter/source/output/gauge/hm_read_gauge.F
!||--- called by ------------------------------------------------------
!||    lectur                 ../starter/source/starter/lectur.F
!||--- calls      -----------------------------------------------------
!||    ancmsg                 ../starter/source/output/message/message.F
!||    fretitl                ../starter/source/starter/freform.F
!||    hm_get_floatv          ../starter/source/devtools/hm_reader/hm_get_floatv.F
!||    hm_get_intv            ../starter/source/devtools/hm_reader/hm_get_intv.F
!||    hm_option_count        ../starter/source/devtools/hm_reader/hm_option_count.F
!||    hm_option_read_key     ../starter/source/devtools/hm_reader/hm_option_read_key.F
!||    hm_option_start        ../starter/source/devtools/hm_reader/hm_option_start.F
!||    sz_r2r                 ../starter/source/coupling/rad2rad/routines_r2r.F
!||    usr2sys                ../starter/source/system/sysfus.F
!||    vdouble                ../starter/source/system/sysfus.F
!||--- uses       -----------------------------------------------------
!||    hm_option_read_mod     ../starter/share/modules1/hm_option_read_mod.F
!||    message_mod            ../starter/share/message_module/message_mod.F
!||    r2r_mod                ../starter/share/modules1/r2r_mod.F
!||    submodel_mod           ../starter/share/modules1/submodel_mod.F
!||====================================================================
      SUBROUTINE HM_READ_GAUGE(LGAUGE,GAUGE,ITABM1,UNITAB,IXC,NOM_OPT,LSUBMODEL)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE UNITAB_MOD
      USE MESSAGE_MOD
      USE R2R_MOD
      USE HM_OPTION_READ_MOD    
      USE SUBMODEL_MOD
      USE NAMES_AND_TITLES_MOD , ONLY : NCHARTITLE, NCHARKEY
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "scr17_c.inc"
#include      "units_c.inc"
#include      "com04_c.inc"
#include      "param_c.inc"
#include      "r2r_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      TYPE (UNIT_TYPE_),INTENT(IN) ::UNITAB 
      INTEGER,INTENT(IN) :: IXC(NIXC,*),ITABM1(*)
      INTEGER,INTENT(INOUT) ::  LGAUGE(3,*)
      INTEGER NOM_OPT(LNOPT1,*)
      my_real GAUGE(LLGAUGE,*)
      TYPE(SUBMODEL_DATA) LSUBMODEL(*)
C-----------------------------------------------
      LOGICAL IS_AVAILABLE
C-----------------------------------------------
C   E x t e r n a l   F u n c t i o n s
C-----------------------------------------------
      INTEGER USR2SYS
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J,ID, NGAU, NOD, ISK, UID, IFLAGUNIT, IG, L,NBGAUGE_SPH,NBGAUGE_POINT
      INTEGER N,NS,NG,OFFS
      my_real FF,BID,DIST,Xgauge,Ygauge,Zgauge
      CHARACTER MESS*40
      CHARACTER(LEN=NCHARTITLE) :: TITR
      CHARACTER(LEN=NCHARKEY) :: KEY
      DATA MESS/'GAUGE DEFINITION                        '/
C=======================================================================
      IS_AVAILABLE = .FALSE.
C-----------------------------------------------
C---------------------------------------------
C     LECTURE DES PARAMETRES DE GAUGES
C-----------------------------------------------
c     LGAUGE(3,*)
c 1:  -Isolid                              -(NUMELS_G+1) if SPH gauge
c 2:  GaugeId
c 3:  +Node or -Shell
c
c     => GAUGE(LLGAUGE,*), LLGAUGE = 37
c 1:  Dist (distance from Shell)           Dist (distance from Shell)
c 2:  XG                                   XG
c 3:  YG                                   YG
c 4:  ZG                                   ZG
c 5:  Alpha (Solid penetration ratio)      not yet used
c 6:                                       XSAV (SPH sorting)
c 7:                                       YSAV (SPH sorting)
c 8:                                       ZSAV (SPH sorting)
c 9:                                       FF (sph only)
c 10:                                      intantaneous Pressure
c 11:                                      intantaneous PA
c 12:                                      intantaneous Rho
c 13:                                      intantaneous E
c 14:                                      ! Butterworth !
c 15:                                      ! Butterworth !
c 16:                                      ! Butterworth !
c 17:                                      ! Butterworth !
c 18:                                      ! Butterworth !
c 19:                                      ! Butterworth !
c 20:                                      ! Butterworth !
c 21:                                      ! Butterworth !
c 22:                                      ! Butterworth !
c 23:                                      ! Butterworth !
c 24:                                      ! Butterworth !
c 25:                                      ! Butterworth !
c 26:                                      ! Butterworth !
c 27:                                      ! Butterworth !
c 28:                                      ! Butterworth !
c 29:                                      ! Butterworth !
c 30:  Pressure                            filtered Pressure
c 31:  PA                                  filtered PA
c 32:  Rho                                 filtered Rho
c 33:  E                                   filtered E
c 34:                                      ! Xpoint      !
c 35:                                      ! Ypoint      !
c 36:                                      ! Zpoint      !
c 37:                                      ! Butterworth !
C=======================================================================
      NG = 0
      FF = 0
      CALL HM_OPTION_COUNT('/GAUGE/SPH', NBGAUGE_SPH)
C----------------------------------------------------------------- 
      IF ( NBGAUGE_SPH > 0)THEN
C----------------------------------------------------------------- 
       CALL HM_OPTION_START('/GAUGE/SPH')       
       DO I=1,NBGAUGE_SPH
        NG=NG+1
        !---Multidomaines --> on ignore les sections non tagees----
        IF(NSUBDOM > 0) THEN
          IF(TAGGAU(NG) == 0) CALL SZ_R2R(TAGGAU,NG)
        ENDIF
        !---------------------------------------------------------- 
        KEY=''
        CALL HM_OPTION_READ_KEY(LSUBMODEL,OPTION_ID=NGAU,KEYWORD2=KEY,OPTION_TITR=TITR)
        CALL FRETITL(TITR,NOM_OPT(LNOPT1-LTITR+1,I),LTITR)


        NOM_OPT(1,I)=NGAU
   
        LGAUGE(1,I)=-(NUMELS+1)
        DIST = ZERO
        CALL HM_GET_INTV   ('NODE1'    ,NOD   ,IS_AVAILABLE,LSUBMODEL)
        CALL HM_GET_INTV   ('shell_ID' ,NS    ,IS_AVAILABLE,LSUBMODEL)
        CALL HM_GET_FLOATV ('DIST'     ,DIST  ,IS_AVAILABLE, LSUBMODEL, UNITAB)
        CALL HM_GET_FLOATV ('Fcut'     ,FF    ,IS_AVAILABLE, LSUBMODEL, UNITAB)

        GAUGE(1,I) =DIST
        GAUGE(9,I) =FF
        LGAUGE(2,I)=NGAU

        WRITE (IOUT,'(///,A)')'      SPH GAUGE'
        WRITE (IOUT,'(A/)')   '      ---------'
        WRITE (IOUT,'(A,I10)')' SPH GAUGE NUMBER . . . . . . . . . . .',NGAU

        IF(NOD /= 0)THEN
          LGAUGE(3,I)=USR2SYS(NOD,ITABM1,MESS,NGAU)
          WRITE (IOUT,'(A,I10)')' NODE NUMBER. . . . . . . . . . . . . .',NOD
        ELSEIF(NS /= 0)THEN
            DO J=1,NUMELC
              IF(IXC(NIXC,J)==NS)THEN
                LGAUGE(3,I)=-J
                EXIT
              ENDIF
            ENDDO
            WRITE (IOUT,'(A,I10)')     ' SHELL NUMBER . . . . . . . . . . . . .',NS
            WRITE (IOUT,'(A,1PG20.13)')' DISTANCE . . . . . . . . . . . . . . .',DIST
        ENDIF
        WRITE (IOUT,'(A,1PG20.13)')    ' 4-POLE BUTTERWORTH CORNER FREQUENCY. .',FF
C-------------------------------------

       ENDDO ! NBGAUGE_SPH

      ENDIF

C-----------------------------------------------
c--------/GAUGE/POINT
C-----------------------------------------------
      CALL HM_OPTION_COUNT('/GAUGE/POINT', NBGAUGE_POINT)
C----------------------------------------------------------------- 
      IF ( NBGAUGE_POINT > 0)THEN
        CALL HM_OPTION_START('/GAUGE/POINT')       
          DO I=1,NBGAUGE_POINT
            KEY=''
            CALL HM_OPTION_READ_KEY(LSUBMODEL,OPTION_ID=NGAU,KEYWORD2=KEY,OPTION_TITR=TITR)
            NOM_OPT(1,I)=NGAU
            CALL FRETITL(TITR,NOM_OPT(LNOPT1-LTITR+1,I),LTITR)
            NG=NG+1
            !Multidomaines --> on ignore les sections non tagees----
            IF(NSUBDOM > 0) THEN
              IF(TAGGAU(NG) == 0) CALL SZ_R2R(TAGGAU,NG)
            ENDIF
            !----------------------------------------------------------------- 
            LGAUGE(1,NG)=0
            CALL HM_GET_FLOATV ('Xi'     ,Xgauge  ,IS_AVAILABLE, LSUBMODEL, UNITAB)
            CALL HM_GET_FLOATV ('Yi'     ,Ygauge  ,IS_AVAILABLE, LSUBMODEL, UNITAB)
            CALL HM_GET_FLOATV ('Zi'     ,Zgauge  ,IS_AVAILABLE, LSUBMODEL, UNITAB)
            LGAUGE(2,NG)=NGAU
            LGAUGE(3,NG)=0  ! ID shell or node only
            GAUGE(1,NG)=ZERO ! DIST
            GAUGE(9,NG)=ZERO ! FF
            GAUGE(34,NG)=Xgauge
            GAUGE(35,NG)=Ygauge
            GAUGE(36,NG)=Zgauge
            WRITE (IOUT,'(///,A)')'          GAUGE'
            WRITE (IOUT,'(A/)')   '          -----'
            WRITE (IOUT,'(A,I10)')' GAUGE NUMBER . . . . . . . . . . . . .',NGAU
            WRITE (IOUT,'(A,I10)')' GAUGE POINT coordinate:'
            WRITE (IOUT,'(A,/1P3G20.13/)')'        Xg                  Yg                  Zg',Xgauge, Ygauge, Zgauge
            WRITE (IOUT,'(A,1PG20.13)')' 4-POLE BUTTERWORTH CORNER FREQUENCY. .',FF
          ENDDO ! DO I=1,NBGAUGE_POINT
      ENDIF ! IF ( NBGAUGE_POINT > 0)
C-----------------------------------------------
c--------/GAUGE
C-----------------------------------------------
      CALL HM_OPTION_START('/GAUGE')       
C-----------------------------------------------
       DO I=1,NBGAUGE
        KEY=''
        CALL HM_OPTION_READ_KEY(LSUBMODEL,OPTION_ID=NGAU,KEYWORD2=KEY,OPTION_TITR=TITR)
        NOM_OPT(1,I)=NGAU
        CALL FRETITL(TITR,NOM_OPT(LNOPT1-LTITR+1,I),LTITR)

        IF (KEY == 'SPH' .OR. KEY == 'POINT') CYCLE
        NG=NG+1
        !Multidomaines --> on ignore les sections non tagees----
        IF(NSUBDOM > 0) THEN
          IF(TAGGAU(NG) == 0) CALL SZ_R2R(TAGGAU,NG)
        ENDIF
        !----------------------------------------------------------------- 
        LGAUGE(1,NG)=0

        DIST = ZERO
        CALL HM_GET_INTV   ('NODE1'    ,NOD   ,IS_AVAILABLE,LSUBMODEL)
        CALL HM_GET_INTV   ('shell_ID' ,NS    ,IS_AVAILABLE,LSUBMODEL)
        CALL HM_GET_FLOATV ('DIST'     ,DIST  ,IS_AVAILABLE, LSUBMODEL, UNITAB)

        GAUGE(1,NG)=DIST
        GAUGE(9,NG)=ZERO
        LGAUGE(2,NG)=NGAU

        WRITE (IOUT,'(///,A)')'          GAUGE'
        WRITE (IOUT,'(A/)')   '          -----'
        WRITE (IOUT,'(A,I10)')' GAUGE NUMBER . . . . . . . . . . . . .',NGAU
        IF(NOD /= 0)THEN
          LGAUGE(3,NG)=USR2SYS(NOD,ITABM1,MESS,NGAU)
          WRITE (IOUT,'(A,I10)')' NODE NUMBER. . . . . . . . . . . . . .',NOD
        ELSEIF(NS /= 0)THEN
            DO J=1,NUMELC
              IF(IXC(NIXC,J)==NS)THEN
                LGAUGE(3,NG)=-J
                EXIT
              ENDIF
            ENDDO
            IF (LGAUGE(3,NG) == 0)CALL ANCMSG(MSGID=3013,MSGTYPE=MSGERROR,ANMODE=ANINFO,I1=NGAU,C1=TITR,I2=NS)
            WRITE (IOUT,'(/,A,I10)')' SHELL NUMBER . . . . . . . . . . . . .',NS
            WRITE (IOUT,'(A,1PG20.13)')' DISTANCE . . . . . . . . . . . . . . .',DIST
        ENDIF
        WRITE (IOUT,'(A,1PG20.13)')' 4-POLE BUTTERWORTH CORNER FREQUENCY. .',FF
C-------------------------------------
      ENDDO ! NBGAUGE

C-------------------------------------
C Recherche des ID doubles
C-------------------------------------
      CALL VDOUBLE(NOM_OPT,LNOPT1,NBGAUGE,MESS,0,BID)
C----
      RETURN
      END
