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
Chd|  LECDAMP                       source/input/lecdamp.F        
Chd|-- called by -----------
Chd|        LECTUR                        source/input/lectur.F         
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        ARRET                         source/system/arret.F         
Chd|        NGR2USR                       source/input/freform.F        
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE LECDAMP(ND, DAMPR, IGRNOD)
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
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "scr07_c.inc"
#include      "stati_c.inc"
#include      "statr_c.inc"
#include      "com01_c.inc"
#include      "units_c.inc"
#include      "com04_c.inc"
#include      "warn_c.inc"
#include      "param_c.inc"
#include      "task_c.inc"
#include      "scr17_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER ND
      my_real DAMPR(NRDAMP,*)
      TYPE(GROUP_)  ,DIMENSION(NGRNOD)  :: IGRNOD
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER K, L, ID, IDNEW, NGR, OK, KORTH
C     REAL
      my_real
     .   DAMPA,DAMPB,DAMPAY,DAMPBY,DAMPAZ,DAMPBZ,FACTB,
     .   DAMPARX,DAMPBRX,DAMPARY,DAMPBRY,DAMPARZ,DAMPBRZ
c     .   TSTART,TSTOP
C-----------------------------------------------
C   E x t e r n a l   F u n c t i o n s
C-----------------------------------------------
      INTEGER  NGR2USR
      EXTERNAL NGR2USR
C-----------------------------------------------
      IF (ND>0) THEN
        IF (IDAMP0 == 1) THEN
          IF (ND>1) THEN
           IF (ISPMD==0)THEN
            WRITE(ISTDO,*)' ** ERROR:  INPUT ERROR IN OPTION DAMP'
            WRITE(IOUT,* )' ** ERROR:  INPUT ERROR IN OPTION DAMP'
            WRITE(IOUT,*) '    V41 FORMAT ALLOWS ONLY ONE /DAMP OPTION'
           END IF
           CALL ARRET(2)
          ENDIF
          DAMPA0 = 0.
          DAMPB0 = 0.
          READ (IIN,'(I8,2E16.9,I8)') IDNEW,DAMPA0,DAMPB0,NGR
          IF (MCHECK==0) THEN
            IDAMPG = NGR2USR(NGR,IGRNOD,NGRNOD)
            IF (IDAMPG==0) NGR = 0
            DAMPA  = DAMPA0
            DAMPB  = DAMPB0
            IF (ISPMD==0) WRITE(IOUT,2241) NGR,DAMPA,DAMPB
          ENDIF
        ELSEIF (NDAMP>0) THEN
          IF (ISPMD==0) WRITE (IOUT,2000) ND
          DO K=1,ND
            READ (IIN,'(I10,2E20.0,2I10,E20.0)') IDNEW,DAMPA,DAMPB,NGR,
     .                                           KORTH,FACTB
c            READ (IIN,'(I10,2E20.0,2I10,2E20.0)') IDNEW,DAMPA,DAMPB,NGR,
c     .                                           KORTH,TSTART,TSTOP
c            FACTB = ZERO
c            IF(TSTOP == ZERO) TSTOP = EP30
            IF (NGR/=0) THEN
             IF (ISPMD==0) THEN
              WRITE(ISTDO,*)' ** ERROR:  INPUT ERROR IN OPTION DAMP'
              WRITE(IOUT,* )' ** ERROR:  INPUT ERROR IN OPTION DAMP'
              WRITE(IOUT,*) '            NOT A V44 FORMAT '
             END IF
             CALL ARRET(2)
            ENDIF
            IF (FACTB == ZERO) FACTB = ONE
            IF(KORTH==0)THEN
              IF (ISPMD==0) WRITE (IOUT,2200) IDNEW,DAMPA,DAMPB,FACTB
c              IF (ISPMD==0) WRITE (IOUT,2200) IDNEW,DAMPA,DAMPB,FACTB,
c     .                                          TSTART,TSTOP
              OK=0
              IF (NDAMP == 1 .AND. IDNEW == 0) THEN
                ID = DAMPR(1,1)
                DAMPR(3,1) = DAMPA
                DAMPR(4,1) = DAMPB
                DAMPR(16,1)= FACTB
                OK=1
              ELSE
                DO L=1,NDAMP
                  ID = DAMPR(1,L)
                  IF(ID==IDNEW) THEN
                    DAMPR(3,L) = DAMPA
                    DAMPR(4,L) = DAMPB
                    DAMPR(5,L)  = DAMPA
                    DAMPR(6,L)  = DAMPB
                    DAMPR(7,L)  = DAMPA
                    DAMPR(8,L)  = DAMPB
                    DAMPR(9,L)  = DAMPA
                    DAMPR(10,L) = DAMPB
                    DAMPR(11,L) = DAMPA
                    DAMPR(12,L) = DAMPB
                    DAMPR(13,L) = DAMPA
                    DAMPR(14,L) = DAMPB
                    DAMPR(16,L) = FACTB
c                    DAMPR(17,L) = TSTART
c                    DAMPR(18,L) = TSTOP
                    OK=1
                  ENDIF
                ENDDO
              ENDIF
            ELSE
              IF(NRDAMP<15)THEN
               IERR=IERR+1
               IF (ISPMD==0) THEN
                 CALL ANCMSG(MSGID=203,ANMODE=ANINFO,
     .                       I1=IDNEW)
               END IF
               CALL ARRET(2)
              END IF
              READ (IIN,'(2E20.0)') DAMPAY,DAMPBY
              READ (IIN,'(2E20.0)') DAMPAZ,DAMPBZ
              READ (IIN,'(2E20.0)') DAMPARX,DAMPBRX
              READ (IIN,'(2E20.0)') DAMPARY,DAMPBRY
              READ (IIN,'(2E20.0)') DAMPARZ,DAMPBRZ
              IF (ISPMD==0) WRITE (IOUT,2250) IDNEW,
     .                    DAMPA,DAMPB,DAMPAY,DAMPBY,DAMPAZ,DAMPBZ,
     .            DAMPARX,DAMPBRX,DAMPARY,DAMPBRY,DAMPARZ,DAMPBRZ
c              IF (ISPMD==0) WRITE (IOUT,2250) IDNEW,
c     .                    DAMPA,DAMPB,DAMPAY,DAMPBY,DAMPAZ,DAMPBZ,
c     .            DAMPARX,DAMPBRX,DAMPARY,DAMPBRY,DAMPARZ,DAMPBRZ,
c     .            TSTART,TSTOP
              OK=0
              DO L=1,NDAMP
                ID = DAMPR(1,L)
                IF(ID==IDNEW) THEN
                  DAMPR(3,L)  = DAMPA
                  DAMPR(4,L)  = DAMPB
                  DAMPR(5,L)  = DAMPAY
                  DAMPR(6,L)  = DAMPBY
                  DAMPR(7,L)  = DAMPAZ
                  DAMPR(8,L)  = DAMPBZ
                  DAMPR(9,L)  = DAMPARX
                  DAMPR(10,L) = DAMPBRX
                  DAMPR(11,L) = DAMPARY
                  DAMPR(12,L) = DAMPBRY
                  DAMPR(13,L) = DAMPARZ
                  DAMPR(14,L) = DAMPBRZ
                  DAMPR(16,L) = FACTB
c                  DAMPR(17,L) = TSTART
c                  DAMPR(18,L) = TSTOP
                  OK=1
                ENDIF
              ENDDO
            END IF
            IF(OK==0)THEN
             IERR=IERR+1
             IF (ISPMD==0) THEN
               CALL ANCMSG(MSGID=203,ANMODE=ANINFO,
     .                     I1=IDNEW)
             END IF
             CALL ARRET(2)
            ENDIF
          ENDDO
        ELSE
         IF (ISPMD==0) THEN
          CALL ANCMSG(MSGID=204,ANMODE=ANINFO)
         END IF
         CALL ARRET(2)
        ENDIF
      ENDIF
C
      RETURN
C
 2000  FORMAT(' RAYLEIGH DAMPING'  /
     .  '  NUMBER OF NEW DAMPING PARAMETERS. . . . . . =' ,I8 /)
 2050  FORMAT(' ** ERROR:  INPUT ERROR IN OPTION DAMP',/
     .'    WRONG NUMBER OF DAMPING PARAMETERS FOR /DAMP/ID NUMBER:',
     .   I10/)
 2100  FORMAT(' ** ERROR:  INPUT ERROR IN OPTION DAMP',/
     .        '    /DAMP/ID NUMBER:',I10,' NOT FOUND'/)
 2200  FORMAT('DAMPING ID . . . . . . . . . . . . .',I10
     .        /5X,'ALPHA. . . . . . . . . . . . . .',1PG20.13
     .        /5X,'BETA . . . . . . . . . . . . . .',1PG20.13
     .        /5X,'MAX TIME STEP FACTOR . . . . . .',1PG20.13/)
c     .        /5X,'START TIME . . . . . . . . . . .',1PG20.13
c     .        /5X,'STOP TIME  . . . . . . . . . . .',1PG20.13/)
 2250  FORMAT('DAMPING ID . . . . . . . . . . . . .',I10
     .        /5X,'ALPHAX . . . . . . . . . . . . .',1PG20.13
     .        /5X,'BETAX. . . . . . . . . . . . . .',1PG20.13
     .        /5X,'ALPHAY . . . . . . . . . . . . .',1PG20.13
     .        /5X,'BETAY. . . . . . . . . . . . . .',1PG20.13
     .        /5X,'ALPHAZ . . . . . . . . . . . . .',1PG20.13
     .        /5X,'BETAZ. . . . . . . . . . . . . .',1PG20.13
     .        /5X,'ALPHARX. . . . . . . . . . . . .',1PG20.13
     .        /5X,'BETARX . . . . . . . . . . . . .',1PG20.13
     .        /5X,'ALPHARY. . . . . . . . . . . . .',1PG20.13
     .        /5X,'BETARY . . . . . . . . . . . . .',1PG20.13
     .        /5X,'ALPHARZ. . . . . . . . . . . . .',1PG20.13
     .        /5X,'BETARZ . . . . . . . . . . . . .',1PG20.13/)
c     .        /5X,'START TIME . . . . . . . . . . .',1PG20.13
c     .        /5X,'STOP TIME  . . . . . . . . . . .',1PG20.13/)
 2241 FORMAT(/
     .  ' RAYLEIGH DAMPING                        '/
     .  ' NODE GROUP ID (=0 ALL NODES) . . . . . . . . . . ',I5/
     .  ' ALPHA  . . . . . . . . . . . . . . . . . . . . . ',G14.7/
     .  ' BETA . . . . . . . . . . . . . . . . . . . . . . ',G14.7/)
      END
