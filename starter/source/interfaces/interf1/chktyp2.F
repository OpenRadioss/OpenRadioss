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
Chd|  CHKTYP2                       source/interfaces/interf1/chktyp2.F
Chd|-- called by -----------
Chd|        LECTUR                        source/starter/lectur.F       
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        FRETITL2                      source/starter/freform.F      
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE CHKTYP2(IPARI   ,ITAB,
     .                   NOM_OPT ,INTBUF_TAB, NATIV_SMS)
      USE MESSAGE_MOD
      USE INTBUFDEF_MOD 
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   A n a l y s e   M o d u l e
C-----------------------------------------------
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IPARI(NPARI,*), ITAB(*)
      INTEGER NOM_OPT(LNOPT1,*),NATIV_SMS(*)

      TYPE(INTBUF_STRUCT_) INTBUF_TAB(*)
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "scr15_c.inc"
#include      "scr17_c.inc"
#include      "units_c.inc"
#include      "sms_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER N, NTY
      INTEGER K10,K11,K12,K13,K14,ILEV,II,J,NMN,NSN,NRTS,NRTM,
     .        NLINS,NLINM,IWOUT
      INTEGER, DIMENSION(:),ALLOCATABLE :: TAGHIER
      INTEGER ID
      CHARACTER*nchartitle,
     .   TITR
C-----------------------------------------------
      ALLOCATE(TAGHIER(NUMNOD))
      TAGHIER(1:NUMNOD) = 0
      DO N=1,NINTER
         NTY =I PARI(7,N)
         IF (NTY == 2) THEN
          NRTS  =IPARI(3,N)
          NRTM  =IPARI(4,N)
          NSN   =IPARI(5,N)
          NMN   =IPARI(6,N)
          ILEV  =IPARI(20,N)
C
          DO II=1,NSN        
            J = INTBUF_TAB(N)%NSV(II)
            IF (ILEV /=25 .and. ILEV/=26 .and. ILEV/=27 .and. ILEV/=28) TAGHIER(J) = ONE
          ENDDO
         ENDIF
      ENDDO
      DO N=1,NINTER
        NTY = IPARI(7,N)
        ID  = NOM_OPT(1,N)
        CALL FRETITL2(TITR, NOM_OPT(LNOPT1-LTITR+1,N),LTITR)
        IF (NTY == 2) THEN
          ILEV = IPARI(20,N)
          IF (ILEV == 0 .OR. ILEV == 1 .OR. ILEV == 27 .OR. ILEV == 28) THEN
            NMN  = IPARI(6,N)
            DO II=1,NMN        
              J=INTBUF_TAB(N)%MSR(II)
              IF (TAGHIER(J) == ONE) THEN
                CALL ANCMSG(MSGID=556,
     .                      MSGTYPE=MSGERROR,
     .                      ANMODE=ANINFO_BLIND_1,
     .                      I1=ID,
     .                      C1=TITR,
     .                      I2=ITAB(J))
              ENDIF
            ENDDO
          ENDIF
        ENDIF
      ENDDO
C
C-----
C-- Check of secnd nodes of type2 + AMS + contact
C
      IF (ISMS /=0 ) THEN
C
        TAGHIER(1:NUMNOD) = 0
        DO N=1,NINTER
          NTY  = IPARI(7,N)
          ILEV = IPARI(20,N)
          IF (NTY == 2 .AND. ILEV/=25 .and. ILEV /= 26) THEN
            NSN   =IPARI(5,N)
            DO II=1,NSN        
              J = INTBUF_TAB(N)%NSV(II)
C-- Only secnds nodes with AMS are checked
              IF (NATIV_SMS(J)/=0) TAGHIER(J) = ONE
            ENDDO
          ENDIF
        ENDDO
C
        DO N=1,NINTER
           NTY=IPARI(7,N)
           NSN   =IPARI(5,N)
           NRTS  =IPARI(3,N)
           NRTM  =IPARI(4,N)

           ID=NOM_OPT(1,N)
           CALL FRETITL2(TITR,
     .          NOM_OPT(LNOPT1-LTITR+1,N),LTITR)
           IWOUT=0
           IF(NTY == 7 .OR. NTY == 10 .OR. NTY == 20 .OR. NTY == 22)THEN
             DO II=1,NSN
               J=INTBUF_TAB(N)%NSV(II)
               IF(TAGHIER(J) == ONE) THEN
                 IF(IWOUT==0)THEN
                   CALL ANCMSG(MSGID=852,
     .                         MSGTYPE=MSGWARNING,
     .                         ANMODE=ANINFO_BLIND_1,
     .                         I1=ID,
     .                         C1=TITR)
                   IWOUT=1
                 END IF
                 CALL ANCMSG(MSGID=849,
     .                       MSGTYPE=MSGWARNING,
     .                       ANMODE=ANINFO_BLIND_2,
     .                       I1=ID,
     .                       C1=TITR,
     .                       I2=ITAB(J))
               END IF
             END DO
             DO II=1,NRTM
               J=INTBUF_TAB(N)%IRECTM(1+4*(II-1))
               IF(TAGHIER(J) == ONE)THEN
                 IF(IWOUT==0)THEN
                   CALL ANCMSG(MSGID=852,
     .                         MSGTYPE=MSGWARNING,
     .                         ANMODE=ANINFO_BLIND_1,
     .                         I1=ID,
     .                         C1=TITR)
                   IWOUT=1
                 END IF
                 CALL ANCMSG(MSGID=850,
     .                       MSGTYPE=MSGWARNING,
     .                       ANMODE=ANINFO_BLIND_2,
     .                       I1=ID,
     .                       C1=TITR,
     .                       I2=ITAB(J))
               END IF
               J=INTBUF_TAB(N)%IRECTM(4*(II-1)+2)
               IF(TAGHIER(J) == ONE)THEN
                 IF(IWOUT==0)THEN
                   CALL ANCMSG(MSGID=852,
     .                         MSGTYPE=MSGWARNING,
     .                         ANMODE=ANINFO_BLIND_1,
     .                         I1=ID,
     .                         C1=TITR)
                   IWOUT=1
                 END IF
                 CALL ANCMSG(MSGID=850,
     .                       MSGTYPE=MSGWARNING,
     .                       ANMODE=ANINFO_BLIND_2,
     .                       I1=ID,
     .                       C1=TITR,
     .                       I2=ITAB(J))
               END IF
               J=INTBUF_TAB(N)%IRECTM(4*(II-1)+3)
               IF(TAGHIER(J) == ONE)THEN
                 IF(IWOUT==0)THEN
                   CALL ANCMSG(MSGID=852,
     .                         MSGTYPE=MSGWARNING,
     .                         ANMODE=ANINFO_BLIND_1,
     .                         I1=ID,
     .                         C1=TITR)
                   IWOUT=1
                 END IF
                 CALL ANCMSG(MSGID=850,
     .                       MSGTYPE=MSGWARNING,
     .                       ANMODE=ANINFO_BLIND_2,
     .                       I1=ID,
     .                       C1=TITR,
     .                       I2=ITAB(J))
               END IF
               J=INTBUF_TAB(N)%IRECTM(4*(II-1)+4)
               IF(TAGHIER(J) == ONE)THEN
                 IF(IWOUT==0)THEN
                   CALL ANCMSG(MSGID=852,
     .                         MSGTYPE=MSGWARNING,
     .                         ANMODE=ANINFO_BLIND_1,
     .                         I1=ID,
     .                         C1=TITR)
                   IWOUT=1
                 END IF
                 CALL ANCMSG(MSGID=850,
     .                       MSGTYPE=MSGWARNING,
     .                       ANMODE=ANINFO_BLIND_2,
     .                       I1=ID,
     .                       C1=TITR,
     .                       I2=ITAB(J))
               END IF
             END DO
             IF(NTY == 20)THEN
               NLINS  =IPARI(51,N)
               NLINM  =IPARI(52,N)
               IF(NLINS+NLINM /= 0)THEN
                 DO II=1,NLINS
                   J=INTBUF_TAB(N)%IXLINS(2*(II-1)+1)
                   IF(TAGHIER(J) == ONE)THEN
                     IF(IWOUT==0)THEN
                       CALL ANCMSG(MSGID=852,
     .                             MSGTYPE=MSGWARNING,
     .                             ANMODE=ANINFO_BLIND_1,
     .                             I1=ID,
     .                             C1=TITR)
                       IWOUT=1
                     END IF
                     CALL ANCMSG(MSGID=851,
     .                           MSGTYPE=MSGWARNING,
     .                           ANMODE=ANINFO_BLIND_2,
     .                           I1=ID,
     .                           C1=TITR,
     .                           I2=ITAB(J))
                   END IF
                   J=INTBUF_TAB(N)%IXLINS(2*(II-1)+2)
                   IF(TAGHIER(J) == ONE)THEN
                     IF(IWOUT==0)THEN
                       CALL ANCMSG(MSGID=852,
     .                             MSGTYPE=MSGWARNING,
     .                             ANMODE=ANINFO_BLIND_1,
     .                             I1=ID,
     .                             C1=TITR)
                       IWOUT=1
                     END IF
                     CALL ANCMSG(MSGID=851,
     .                           MSGTYPE=MSGWARNING,
     .                           ANMODE=ANINFO_BLIND_2,
     .                           I1=ID,
     .                           C1=TITR,
     .                           I2=ITAB(J))
                   END IF
                 END DO
                 DO II=1,NLINM
                   J=INTBUF_TAB(N)%IXLINM(2*(II-1)+1)
                   IF(TAGHIER(J) == ONE)THEN
                     IF(IWOUT==0)THEN
                       CALL ANCMSG(MSGID=852,
     .                             MSGTYPE=MSGWARNING,
     .                             ANMODE=ANINFO_BLIND_1,
     .                             I1=ID,
     .                             C1=TITR)
                       IWOUT=1
                     END IF
                     CALL ANCMSG(MSGID=851,
     .                           MSGTYPE=MSGWARNING,
     .                           ANMODE=ANINFO_BLIND_2,
     .                           I1=ID,
     .                           C1=TITR,
     .                           I2=ITAB(J))
                   END IF
                   J=INTBUF_TAB(N)%IXLINM(2*(II-1)+2)
                   IF(TAGHIER(J) == ONE)THEN
                     IF(IWOUT==0)THEN
                       CALL ANCMSG(MSGID=852,
     .                             MSGTYPE=MSGWARNING,
     .                             ANMODE=ANINFO_BLIND_1,
     .                             I1=ID,
     .                             C1=TITR)
                       IWOUT=1
                     END IF
                     CALL ANCMSG(MSGID=851,
     .                           MSGTYPE=MSGWARNING,
     .                           ANMODE=ANINFO_BLIND_2,
     .                           I1=ID,
     .                           C1=TITR,
     .                           I2=ITAB(J))
                   END IF
                 END DO
               END IF
             END IF
           ELSEIF(NTY == 11)THEN
             DO II=1,NRTS
               J=INTBUF_TAB(N)%IRECTS(2*(II-1)+1)
               IF(TAGHIER(J) == ONE)THEN
                 IF(IWOUT==0)THEN
                   CALL ANCMSG(MSGID=852,
     .                         MSGTYPE=MSGWARNING,
     .                         ANMODE=ANINFO_BLIND_1,
     .                         I1=ID,
     .                         C1=TITR)
                   IWOUT=1
                 END IF
                 CALL ANCMSG(MSGID=851,
     .                       MSGTYPE=MSGWARNING,
     .                       ANMODE=ANINFO_BLIND_2,
     .                       I1=ID,
     .                       C1=TITR,
     .                       I2=ITAB(J))
               END IF
               J=INTBUF_TAB(N)%IRECTS(2*(II-1)+2)
               IF(TAGHIER(J) == ONE)THEN
                 IF(IWOUT==0)THEN
                   CALL ANCMSG(MSGID=852,
     .                         MSGTYPE=MSGWARNING,
     .                         ANMODE=ANINFO_BLIND_1,
     .                         I1=ID,
     .                         C1=TITR)
                   IWOUT=1
                 END IF
                 CALL ANCMSG(MSGID=851,
     .                       MSGTYPE=MSGWARNING,
     .                       ANMODE=ANINFO_BLIND_2,
     .                       I1=ID,
     .                       C1=TITR,
     .                       I2=ITAB(J))
               END IF
             END DO
             DO II=1,NRTM
               J=INTBUF_TAB(N)%IRECTM(2*(II-1)+1)
               IF(TAGHIER(J) == ONE)THEN
                 IF(IWOUT==0)THEN
                   CALL ANCMSG(MSGID=852,
     .                         MSGTYPE=MSGWARNING,
     .                         ANMODE=ANINFO_BLIND_1,
     .                         I1=ID,
     .                         C1=TITR)
                   IWOUT=1
                 END IF
                 CALL ANCMSG(MSGID=851,
     .                       MSGTYPE=MSGWARNING,
     .                       ANMODE=ANINFO_BLIND_2,
     .                       I1=ID,
     .                       C1=TITR,
     .                       I2=ITAB(J))
               END IF
               J=INTBUF_TAB(N)%IRECTM(2*(II-1)+2)
               IF(TAGHIER(J) == ONE)THEN
                 IF(IWOUT==0)THEN
                   CALL ANCMSG(MSGID=852,
     .                         MSGTYPE=MSGWARNING,
     .                         ANMODE=ANINFO_BLIND_1,
     .                         I1=ID,
     .                         C1=TITR)
                   IWOUT=1
                 END IF
                 CALL ANCMSG(MSGID=851,
     .                       MSGTYPE=MSGWARNING,
     .                       ANMODE=ANINFO_BLIND_2,
     .                       I1=ID,
     .                       C1=TITR,
     .                       I2=ITAB(J))
               END IF
             END DO
           ELSEIF(NTY == 21)THEN
             DO II=1,NSN
               J=INTBUF_TAB(N)%NSV(II)
               IF(TAGHIER(J) == ONE) THEN
                 IF(IWOUT==0)THEN
                   CALL ANCMSG(MSGID=852,
     .                         MSGTYPE=MSGWARNING,
     .                         ANMODE=ANINFO_BLIND_1,
     .                         I1=ID,
     .                         C1=TITR)
                   IWOUT=1
                 END IF
                 CALL ANCMSG(MSGID=849,
     .                       MSGTYPE=MSGWARNING,
     .                       ANMODE=ANINFO_BLIND_2,
     .                       I1=ID,
     .                       C1=TITR,
     .                       I2=ITAB(J))
               END IF
             END DO
           END IF
        END DO
      END IF
      DEALLOCATE(TAGHIER)
C
      RETURN
      END
