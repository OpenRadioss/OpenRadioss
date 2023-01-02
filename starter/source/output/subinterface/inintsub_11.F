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
Chd|  ININTSUB_11                   source/output/subinterface/inintsub_11.F
Chd|-- called by -----------
Chd|        ININTSUB                      source/interfaces/interf1/inintsub.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        FRETITL2                      source/starter/freform.F      
Chd|        BITSET                        source/interfaces/inter3d1/bitget.F
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE ININTSUB_11(ITAB      ,IGRSLIN ,IGRSURF ,NOM_OPT ,INTBUF_TAB,
     .                       NRTM      ,NRTM0  ,NSN     ,NISUBS  ,NISUBM    ,
     .                       NOINT     ,NI     ,NOD2RTMS,NOD2RTMM,KAD       ,
     .                       TAGLINS   ,TAGLINM ,IADD   ,NT19    ,MAXRTMS   ,
     .                       NRTS      ,NTY     )      

C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD  
      USE INTBUFDEF_MOD
      USE GROUPDEF_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   A n a l y s e   M o d u l e
C-----------------------------------------------
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "scr03_c.inc"
#include      "scr17_c.inc"
#include      "com04_c.inc"
#include      "units_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER ITAB(*) , NOD2RTMS(*), NOD2RTMM(*), KAD(*), TAGLINS(*), TAGLINM(*), 
     .        IADD(*) 
      INTEGER NRTM, NRTM0, NSN, NISUBS, NISUBM, NOINT, NI, NT19, NRTS, NTY, MAXRTMS
      INTEGER NOM_OPT(LNOPT1,*)

      TYPE(INTBUF_STRUCT_) INTBUF_TAB(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,K,JGRN,ISU,ISU1,ISU2,
     .   JSUB, KSUB, NNE, IS, ISV, CUR, ID1,
     .   NEXT, IM, KM, JAD, IN, II, N,STAT,K1,K2,INOD,IFNRT
      CHARACTER*nchartitle,
     .   TITR,TITR1
      INTEGER ISLIN1,ISLIN2,FOUND,FOUND_LINE
C-----------------------------------------------
      TYPE (GROUP_)  , DIMENSION(NGRNOD)  :: IGRNOD
      TYPE (SURF_)   , DIMENSION(NSURF)   :: IGRSURF
      TYPE (SURF_)   , DIMENSION(NSLIN)   :: IGRSLIN
C-----------------------------------------------
C   E x t e r n a l   F u n c t i o n s
C-----------------------------------------------
      INTEGER BITSET
      EXTERNAL BITSET
C=======================================================================

C

      INTBUF_TAB(NI)%ADDSUBS(1:NRTS+1) = 0 ! address of different subinter related to secondary line
      INTBUF_TAB(NI)%ADDSUBM(1:NRTM+1) = 0 ! address of different subinter related to main line

      INTBUF_TAB(NI)%INFLG_SUBS(1:NISUBS)=0  ! Flags for determining what is surface Surf1 or Surf2
      INTBUF_TAB(NI)%INFLG_SUBM(1:NISUBM)=0


      IF ((NTY==11).AND.(NT19==0)) THEN
C----------------------------------------
C  TAG nodes second calculate addresses
C---------------------------------------------

C
        IADD(1:NUMNOD+1) = 0
        DO IS=1,NRTS
           IN       =INTBUF_TAB(NI)%IRECTS(2*(IS-1)+1)
           IADD(IN) =IADD(IN)+1
           IN       =INTBUF_TAB(NI)%IRECTS(2*(IS-1)+2)
           IADD(IN) =IADD(IN)+1
        END DO
C
        CUR=1
        DO I=1,NUMNOD
           NEXT     =CUR+IADD(I)
           IADD(I)  =CUR
           CUR      =NEXT
        END DO
        IADD(NUMNOD+1)=CUR
C
        DO I=1,NUMNOD
           KAD(I)=IADD(I)
        END DO
C
        DO IS=1,NRTS
           IN       =INTBUF_TAB(NI)%IRECTS(2*(IS-1)+1)
           NOD2RTMS(KAD(IN)) = IS
           KAD(IN) = KAD(IN) + 1
           IN       =INTBUF_TAB(NI)%IRECTS(2*(IS-1)+2)
           NOD2RTMS(KAD(IN)) = IS
           KAD(IN) = KAD(IN) + 1
        ENDDO
C
        KSUB=0
        DO JSUB=1,NINTSUB
           ID1=NOM_OPT(1,NINTER+JSUB)
           CALL FRETITL2(TITR1,
     .              NOM_OPT(LNOPT1-LTITR+1,NINTER+JSUB),LTITR)
           IF(NOM_OPT(2,NINTER+JSUB)==NOINT
     .           .AND.NOM_OPT(5,NINTER+JSUB)==1)THEN
               KSUB=KSUB+1
C      LISUB(KSUB)=JSUB no interne de la sous-interface
               INTBUF_TAB(NI)%LISUB(KSUB) = JSUB
               INTBUF_TAB(NI)%TYPSUB(KSUB) = 1
C
               ISLIN1  =NOM_OPT(4,NINTER+JSUB)
               NNE  =IGRSLIN(ISLIN1)%NSEG
C
               DO I=1,NNE
                  IN=IGRSLIN(ISLIN1)%NODES(I,1)
                  KM=0
C
                  DO JAD=IADD(IN),IADD(IN+1)-1
                    IS = NOD2RTMS(JAD)
                    FOUND_LINE = 0
                    DO J=1,2
                      II=IGRSLIN(ISLIN1)%NODES(I,J)
                      FOUND=0
                      DO K=1,2
                         IF(INTBUF_TAB(NI)%IRECTS(2*(IS-1)+K)==II) FOUND=1
                      END DO
                      FOUND_LINE = FOUND_LINE + FOUND
                    END DO
                    IF (FOUND_LINE==2) THEN
                      KM=IS
                      EXIT
                    ENDIF
                  ENDDO
C
                  IF(KM==0)THEN
                    CALL ANCMSG(MSGID=1188,
     .                          MSGTYPE=MSGERROR,
     .                          ANMODE=ANINFO_BLIND_1,
     .                          I1=ID1,
     .                          C1=TITR1,
     .                          I2=ITAB(IGRSLIN(ISLIN1)%NODES(I,1)),
     .                          I3=ITAB(IGRSLIN(ISLIN1)%NODES(I,2)),
     .                          I4=NOINT)
                  ELSE
                    INTBUF_TAB(NI)%ADDSUBS(KM)=INTBUF_TAB(NI)%ADDSUBS(KM)+1
                  END IF
               END DO

C
C---------Case of subinter defined with inter 0 -------------
C
           ELSEIF(NOM_OPT(2,NINTER+JSUB) == 0 
     .         .AND. NOM_OPT(5,NINTER+JSUB) == 1) THEN

               KSUB=KSUB+1
C
C      LISUB(KSUB)=JSUB no interne de la sous-interface
               INTBUF_TAB(NI)%LISUB (KSUB) = JSUB
C
C      prepare ADDSUBS , LISUBS (K31) : 
C      LISUBS(ADDSUBS(IS):ADDSUBS(IS+1)-1) SS. INTERF CONTENANT LE ND SECOND. IS
C
               TAGLINS(1:MAXRTMS) = 0

               ISU2 =NOM_OPT(6,NINTER+JSUB)  ! SURFACE ID 2
               IF(ISU2/=0)THEN
                  INTBUF_TAB(NI)%TYPSUB(KSUB) = 2
                  DO I=1,IGRSURF(ISU2)%NSEG

                     DO INOD=1,2
                       IN=IGRSURF(ISU2)%NODES(I,2*INOD)
                       DO JAD=IADD(IN),IADD(IN+1)-1
                          IS = NOD2RTMS(JAD)
                          FOUND_LINE = 0
                          DO J=1,2
                             II=INTBUF_TAB(NI)%IRECTS(2*(IS-1)+J)
                             FOUND=0
                             DO K=1,4
                                IF(IGRSURF(ISU2)%NODES(I,K)==II) FOUND=1
                             END DO
                             FOUND_LINE = FOUND_LINE + FOUND
                          END DO
                          IF ((FOUND_LINE==2).AND.(TAGLINS(IS)==0)) THEN
                             INTBUF_TAB(NI)%ADDSUBS(IS)=INTBUF_TAB(NI)%ADDSUBS(IS)+1
C                          Line must be taken into account only one time for one subinterface
                             TAGLINS(IS) = 1
                          ENDIF
                       ENDDO
                     ENDDO

                  ENDDO 
               ENDIF

               ISU1 =NOM_OPT(3,NINTER+JSUB)  ! SURFACE ID 1
               IF(ISU1/=0)THEN
                  INTBUF_TAB(NI)%TYPSUB(KSUB) = 3
                  DO I=1,IGRSURF(ISU1)%NSEG

                     DO INOD=1,2
                       IN=IGRSURF(ISU1)%NODES(I,2*INOD)
                       DO JAD=IADD(IN),IADD(IN+1)-1
                          IS = NOD2RTMS(JAD)
                          FOUND_LINE = 0
                          DO J=1,2
                             II=INTBUF_TAB(NI)%IRECTS(2*(IS-1)+J)
                             FOUND=0
                             DO K=1,4
                                IF(IGRSURF(ISU1)%NODES(I,K)==II) FOUND=1
                             END DO
                             FOUND_LINE = FOUND_LINE + FOUND
                          END DO
                          IF ((FOUND_LINE==2).AND.(TAGLINS(IS)==0)) THEN
                             INTBUF_TAB(NI)%ADDSUBS(IS)=INTBUF_TAB(NI)%ADDSUBS(IS)+1
C                          Line must be taken into account only one time for one subinterface
                             TAGLINS(IS) = 1
                          ENDIF
                       ENDDO
                     ENDDO

                  ENDDO 
               ENDIF
C
C

           END IF
        END DO
C
C  --------Skyline tabs ADDSUBS-----------
C
         CUR=1
         DO IS=1,NRTS
            NEXT	         =CUR+INTBUF_TAB(NI)%ADDSUBS(IS)
            INTBUF_TAB(NI)%ADDSUBS(IS)=CUR
            CUR	                =NEXT
         END DO
         INTBUF_TAB(NI)%ADDSUBS(NRTS+1)=CUR
C
C           utilise KAD(1:NRTM)
         DO IS=1,NRTS
            KAD(IS)=INTBUF_TAB(NI)%ADDSUBS(IS)
         END DO
C
C      prepare LISUBS :
C
         KSUB=0
         DO JSUB=1,NINTSUB
            IF(NOM_OPT(2,NINTER+JSUB)==NOINT
     .         .AND.NOM_OPT(5,NINTER+JSUB)==1)THEN
               KSUB=KSUB+1
C
               ISLIN1  =NOM_OPT(4,NINTER+JSUB)
               NNE  =IGRSLIN(ISLIN1)%NSEG
C
               DO I=1,NNE
                  IN=IGRSLIN(ISLIN1)%NODES(I,1)
                  KM=0
C
                  DO JAD=IADD(IN),IADD(IN+1)-1
                    IS = NOD2RTMS(JAD)
                    FOUND_LINE = 0
                    DO J=1,2
                      II=IGRSLIN(ISLIN1)%NODES(I,J)
                      FOUND=0
                      DO K=1,2
                         IF(INTBUF_TAB(NI)%IRECTS(2*(IS-1)+K)==II) FOUND=1
                      END DO
                      FOUND_LINE = FOUND_LINE + FOUND
                    END DO
                    IF (FOUND_LINE==2) THEN
                      KM=IS
                      EXIT
                    ENDIF
                  ENDDO
C
                  IF(KM/=0)THEN
                    INTBUF_TAB(NI)%LISUBS(KAD(IS))=KSUB
                    KAD(IS)=KAD(IS)+1
                  END IF
               END DO
C
C---------Case of subinter defined with inter 0 -------------
C
           ELSEIF(NOM_OPT(2,NINTER+JSUB) == 0 
     .         .AND. NOM_OPT(5,NINTER+JSUB) == 1) THEN

               KSUB=KSUB+1
C
C      LISUB(KSUB)=JSUB no interne de la sous-interface
               INTBUF_TAB(NI)%LISUB (KSUB) = JSUB
C
C      prepare ADDSUBS , LISUBS (K31) : 
C      LISUBS(ADDSUBS(IS):ADDSUBS(IS+1)-1) SS. INTERF CONTENANT LE ND SECOND. IS
C
               TAGLINS(1:MAXRTMS) = 0

               ISU2 =NOM_OPT(6,NINTER+JSUB)  ! SURFACE ID 2
               IF(ISU2/=0)THEN
                  DO I=1,IGRSURF(ISU2)%NSEG

                     DO INOD=1,2
                       IN=IGRSURF(ISU2)%NODES(I,2*INOD)
                       DO JAD=IADD(IN),IADD(IN+1)-1
                          IS = NOD2RTMS(JAD)
                          FOUND_LINE = 0
                          DO J=1,2
                             II=INTBUF_TAB(NI)%IRECTS(2*(IS-1)+J)
                             FOUND=0
                             DO K=1,4
                                IF(IGRSURF(ISU2)%NODES(I,K)==II) FOUND=1
                             END DO
                             FOUND_LINE = FOUND_LINE + FOUND
                          END DO

                          IF (FOUND_LINE==2)THEN
                            IF(TAGLINS(IS)==0) THEN
                              INTBUF_TAB(NI)%INFLG_SUBS(KAD(IS))=
     .                          BITSET(INTBUF_TAB(NI)%INFLG_SUBS(KAD(IS)),0)
                              INTBUF_TAB(NI)%LISUBS(KAD(IS))=KSUB
                              KAD(IS)=KAD(IS)+1
C                          Line must be taken into account only one time for one subinterface
                              TAGLINS(IS) = 1                     
                            ELSE                   
                              INTBUF_TAB(NI)%INFLG_SUBS(KAD(IS)-1)=
     .                           BITSET(INTBUF_TAB(NI)%INFLG_SUBS(KAD(IS)-1),0)
                            ENDIF
                          ENDIF
                       ENDDO
                     ENDDO

                  ENDDO 
               ENDIF
C
               ISU1 =NOM_OPT(3,NINTER+JSUB)  ! SURFACE ID 1
               IF(ISU1/=0)THEN
                  DO I=1,IGRSURF(ISU1)%NSEG

                     DO INOD=1,2
                       IN=IGRSURF(ISU1)%NODES(I,2*INOD)
                       DO JAD=IADD(IN),IADD(IN+1)-1
                          IS = NOD2RTMS(JAD)
                          FOUND_LINE = 0
                          DO J=1,2
                             II=INTBUF_TAB(NI)%IRECTS(2*(IS-1)+J)
                             FOUND=0
                             DO K=1,4
                                IF(IGRSURF(ISU1)%NODES(I,K)==II) FOUND=1
                             END DO
                             FOUND_LINE = FOUND_LINE + FOUND
                          END DO   
                          IF (FOUND_LINE==2)THEN
                            IF(TAGLINS(IS)==0) THEN
                              INTBUF_TAB(NI)%INFLG_SUBS(KAD(IS))=
     .                          BITSET(INTBUF_TAB(NI)%INFLG_SUBS(KAD(IS)),1)
                              INTBUF_TAB(NI)%LISUBS(KAD(IS))=KSUB
                              KAD(IS)=KAD(IS)+1
C                          Line must be taken into account only one time for one subinterface
                              TAGLINS(IS) = 1                     
                            ELSE                   
                              INTBUF_TAB(NI)%INFLG_SUBS(KAD(IS)-1)=
     .                           BITSET(INTBUF_TAB(NI)%INFLG_SUBS(KAD(IS)-1),1)
                            ENDIF
                          ENDIF
                       ENDDO

                     ENDDO

                  ENDDO 
               ENDIF
C

            END IF
         END DO
C
C--- MAIN Side--
C
         IADD(1:NUMNOD+1) = 0
         DO IM=1,NRTM
            IN       =INTBUF_TAB(NI)%IRECTM(2*(IM-1)+1)
            IADD(IN) =IADD(IN)+1
            IN       =INTBUF_TAB(NI)%IRECTM(2*(IM-1)+2)
            IADD(IN) =IADD(IN)+1
         END DO
C
         CUR=1
         DO I=1,NUMNOD
            NEXT     =CUR+IADD(I)
            IADD(I)  =CUR
            CUR      =NEXT
         END DO
         IADD(NUMNOD+1)=CUR
C
         DO I=1,NUMNOD
            KAD(I)=IADD(I)
         END DO
C
         DO IM=1,NRTM
            IN       =INTBUF_TAB(NI)%IRECTM(2*(IM-1)+1)
            NOD2RTMM(KAD(IN)) = IM
            KAD(IN) = KAD(IN) + 1
            IN       =INTBUF_TAB(NI)%IRECTM(2*(IM-1)+2)
            NOD2RTMM(KAD(IN)) = IM
            KAD(IN) = KAD(IN) + 1
         ENDDO
C
         KSUB=0
         DO JSUB=1,NINTSUB
            ID1=NOM_OPT(1,NINTER+JSUB)
            CALL FRETITL2(TITR1,
     .               NOM_OPT(LNOPT1-LTITR+1,NINTER+JSUB),LTITR)
            IF(NOM_OPT(2,NINTER+JSUB)==NOINT
     .           .AND.NOM_OPT(5,NINTER+JSUB)==1)THEN
               KSUB=KSUB+1
C
               ISLIN2  =NOM_OPT(3,NINTER+JSUB)
               NNE  =IGRSLIN(ISLIN2)%NSEG
C
               DO I=1,NNE
                  IN=IGRSLIN(ISLIN2)%NODES(I,1)
                  KM=0
C
                  DO JAD=IADD(IN),IADD(IN+1)-1
                    IM = NOD2RTMM(JAD)
                    FOUND_LINE = 0
                    DO J=1,2
                      II=IGRSLIN(ISLIN2)%NODES(I,J)
                      FOUND=0
                      DO K=1,2
                         IF(INTBUF_TAB(NI)%IRECTM(2*(IM-1)+K)==II) FOUND=1
                      END DO
                      FOUND_LINE = FOUND_LINE + FOUND
                    END DO
                    IF (FOUND_LINE==2) THEN
                      KM=IM
                      EXIT
                    ENDIF
                  ENDDO
C
                  IF(KM==0)THEN
                    CALL ANCMSG(MSGID=1189,
     .                          MSGTYPE=MSGERROR,
     .                          ANMODE=ANINFO_BLIND_1,
     .                          I1=ID1,
     .                          C1=TITR1,
     .                          I2=ITAB(IGRSLIN(ISLIN2)%NODES(I,1)),
     .                          I3=ITAB(IGRSLIN(ISLIN2)%NODES(I,2)),
     .                          I4=NOINT)
                  ELSE
                    INTBUF_TAB(NI)%ADDSUBM(KM)=INTBUF_TAB(NI)%ADDSUBM(KM)+1
                  END IF
               END DO

C
C---------Case of subinter defined with inter 0 -------------
C
           ELSEIF(NOM_OPT(2,NINTER+JSUB) == 0 
     .         .AND. NOM_OPT(5,NINTER+JSUB) == 1) THEN

               KSUB=KSUB+1
C
C      LISUB(KSUB)=JSUB no interne de la sous-interface
               INTBUF_TAB(NI)%LISUB (KSUB) = JSUB
C
C      prepare ADDSUBS , LISUBS (K31) : 
C      LISUBS(ADDSUBS(IS):ADDSUBS(IS+1)-1) SS. INTERF CONTENANT LE ND SECOND. IS
C
               TAGLINM(1:MAXRTMS) = 0

               ISU1 =NOM_OPT(3,NINTER+JSUB)  ! SURFACE ID 1
               IF(ISU1/=0)THEN
                  DO I=1,IGRSURF(ISU1)%NSEG

                     DO INOD=1,2
                       IN=IGRSURF(ISU1)%NODES(I,2*INOD)
                       DO JAD=IADD(IN),IADD(IN+1)-1
                          IM = NOD2RTMM(JAD)
                          FOUND_LINE = 0
                          DO J=1,2
                             II=INTBUF_TAB(NI)%IRECTM(2*(IS-1)+J)
                             FOUND=0
                             DO K=1,4
                                IF(IGRSURF(ISU1)%NODES(I,K)==II) FOUND=1
                             END DO
                             FOUND_LINE = FOUND_LINE + FOUND
                          END DO
                          IF ((FOUND_LINE==2).AND.(TAGLINM(IM)==0)) THEN
                             INTBUF_TAB(NI)%ADDSUBM(IM)=INTBUF_TAB(NI)%ADDSUBM(IM)+1
C                          Line must be taken into account only one time for one subinterface
                             TAGLINM(IM) = 1
                          ENDIF
                       ENDDO
                     ENDDO

                  ENDDO 
 
               ENDIF
C
               ISU2 =NOM_OPT(6,NINTER+JSUB)  ! SURFACE ID 2
               IF(ISU2/=0)THEN
                  DO I=1,IGRSURF(ISU2)%NSEG

                     DO INOD=1,2
                       IN=IGRSURF(ISU2)%NODES(I,2*INOD)
                       DO JAD=IADD(IN),IADD(IN+1)-1
                          IM = NOD2RTMM(JAD)
                          FOUND_LINE = 0
                          DO J=1,2
                             II=INTBUF_TAB(NI)%IRECTM(2*(IS-1)+J)
                             FOUND=0
                             DO K=1,4
                                IF(IGRSURF(ISU2)%NODES(I,K)==II) FOUND=1
                             END DO
                             FOUND_LINE = FOUND_LINE + FOUND
                          END DO
                          IF ((FOUND_LINE==2).AND.(TAGLINM(IM)==0)) THEN
                             INTBUF_TAB(NI)%ADDSUBM(IM)=INTBUF_TAB(NI)%ADDSUBM(IM)+1
C                          Line must be taken into account only one time for one subinterface
                             TAGLINM(IM) = 1
                          ENDIF
                       ENDDO
                     ENDDO

                  ENDDO 
 
               ENDIF
C

            END IF
         END DO
C
         CUR=1
         DO IM=1,NRTM
            NEXT	                =CUR+INTBUF_TAB(NI)%ADDSUBM(IM)
            INTBUF_TAB(NI)%ADDSUBM(IM)=CUR
            CUR	                =NEXT
         END DO
         INTBUF_TAB(NI)%ADDSUBM(NRTM+1)=CUR
C
C        utilise KAD(1:NRTM)
         DO IM=1,NRTM
            KAD(IM)=INTBUF_TAB(NI)%ADDSUBM(IM)
         END DO
C
C      prepare LISUBM : 
C
         KSUB=0
         DO JSUB=1,NINTSUB
            IF(NOM_OPT(2,NINTER+JSUB)==NOINT
     .         .AND.NOM_OPT(5,NINTER+JSUB)==1)THEN
               KSUB=KSUB+1
C
               ISLIN2  =NOM_OPT(3,NINTER+JSUB)
               NNE  =IGRSLIN(ISLIN2)%NSEG
C
               DO I=1,NNE
                  IN=IGRSLIN(ISLIN2)%NODES(I,1)
                  KM=0
C
                  DO JAD=IADD(IN),IADD(IN+1)-1
                    IM = NOD2RTMM(JAD)
                    FOUND_LINE = 0
                    DO J=1,2
                      II=IGRSLIN(ISLIN2)%NODES(I,J)
                      FOUND=0
                      DO K=1,2
                         IF(INTBUF_TAB(NI)%IRECTM(2*(IM-1)+K)==II) FOUND=1
                      END DO
                      FOUND_LINE = FOUND_LINE + FOUND
                    END DO
                    IF (FOUND_LINE==2) THEN
                      KM=IM
                      EXIT
                    ENDIF
                  ENDDO
C
                  IF(KM/=0)THEN
                    INTBUF_TAB(NI)%LISUBM(KAD(IM))=KSUB
                    KAD(IM)=KAD(IM)+1
                  END IF
               END DO


C
C---------Case of subinter defined with inter 0 -------------
C
           ELSEIF(NOM_OPT(2,NINTER+JSUB) == 0 
     .         .AND. NOM_OPT(5,NINTER+JSUB) == 1) THEN

               KSUB=KSUB+1
C
C      LISUB(KSUB)=JSUB no interne de la sous-interface
               INTBUF_TAB(NI)%LISUB (KSUB) = JSUB
C
C      prepare ADDSUBS , LISUBS (K31) : 
C      LISUBS(ADDSUBS(IS):ADDSUBS(IS+1)-1) SS. INTERF CONTENANT LE ND SECOND. IS
C
               TAGLINM(1:MAXRTMS) = 0

               ISU1 =NOM_OPT(3,NINTER+JSUB)  ! SURFACE ID 1
               IF(ISU1/=0)THEN
                  DO I=1,IGRSURF(ISU1)%NSEG

                     DO INOD=1,2
                       IN=IGRSURF(ISU1)%NODES(I,2*INOD)
                       DO JAD=IADD(IN),IADD(IN+1)-1
                          IM = NOD2RTMM(JAD)
                          FOUND_LINE = 0
                          DO J=1,2
                             II=INTBUF_TAB(NI)%IRECTM(2*(IS-1)+J)
                             FOUND=0
                             DO K=1,4
                                IF(IGRSURF(ISU1)%NODES(I,K)==II) FOUND=1
                             END DO
                             FOUND_LINE = FOUND_LINE + FOUND
                          END DO

                          IF (FOUND_LINE==2)THEN
                            IF(TAGLINM(IM)==0) THEN
                              INTBUF_TAB(NI)%INFLG_SUBM(KAD(IM))=
     .                          BITSET(INTBUF_TAB(NI)%INFLG_SUBM(KAD(IM)),1)
                              INTBUF_TAB(NI)%LISUBM(KAD(IM))=KSUB
                              KAD(IM)=KAD(IM)+1
C                          Line must be taken into account only one time for one subinterface
                              TAGLINM(IM) = 1                     
                            ELSE                   
                              INTBUF_TAB(NI)%INFLG_SUBM(KAD(IM)-1)=
     .                           BITSET(INTBUF_TAB(NI)%INFLG_SUBM(KAD(IM)-1),1)
                            ENDIF
                          ENDIF
                       ENDDO
                     ENDDO

                  ENDDO 
               ENDIF

               ISU2 =NOM_OPT(6,NINTER+JSUB)  ! SURFACE ID 1
               IF(ISU2/=0)THEN
                  DO I=1,IGRSURF(ISU2)%NSEG

                     DO INOD=1,2
                       IN=IGRSURF(ISU2)%NODES(I,2*INOD)
                       DO JAD=IADD(IN),IADD(IN+1)-1
                          IM = NOD2RTMM(JAD)
                          FOUND_LINE = 0
                          DO J=1,2
                             II=INTBUF_TAB(NI)%IRECTM(2*(IS-1)+J)
                             FOUND=0
                             DO K=1,4
                                IF(IGRSURF(ISU2)%NODES(I,K)==II) FOUND=1
                             END DO
                             FOUND_LINE = FOUND_LINE + FOUND
                          END DO

                          IF (FOUND_LINE==2)THEN
                            IF(TAGLINM(IM)==0) THEN
                              INTBUF_TAB(NI)%INFLG_SUBM(KAD(IM))=
     .                          BITSET(INTBUF_TAB(NI)%INFLG_SUBM(KAD(IM)),0)
                              INTBUF_TAB(NI)%LISUBM(KAD(IM))=KSUB
                              KAD(IM)=KAD(IM)+1
C                          Line must be taken into account only one time for one subinterface
                              TAGLINM(IM) = 1                     
                            ELSE                   
                              INTBUF_TAB(NI)%INFLG_SUBM(KAD(IM)-1)=
     .                           BITSET(INTBUF_TAB(NI)%INFLG_SUBM(KAD(IM)-1),0)
                            ENDIF
                          ENDIF
                       ENDDO
                     ENDDO

                  ENDDO 
               ENDIF
C
C
          END IF
         END DO
C

C
      ELSEIF ((NTY==11).AND.(NT19>0)) THEN
C   interface type11 of interface type19 - subinterface input by surfaces
C
C
C--- SECONDARY Side--
C
        IADD(1:NUMNOD+1) = 0
        DO IS=1,NRTS
           IN       =INTBUF_TAB(NI)%IRECTS(2*(IS-1)+1)
           IADD(IN) =IADD(IN)+1
           IN       =INTBUF_TAB(NI)%IRECTS(2*(IS-1)+2)
           IADD(IN) =IADD(IN)+1
         END DO
C
         CUR=1
         DO I=1,NUMNOD
            NEXT     =CUR+IADD(I)
            IADD(I)  =CUR
            CUR      =NEXT
         END DO
         IADD(NUMNOD+1)=CUR
C
         DO I=1,NUMNOD
            KAD(I)=IADD(I)
         END DO
C
         DO IS=1,NRTS
            IN       =INTBUF_TAB(NI)%IRECTS(2*(IS-1)+1)
            NOD2RTMS(KAD(IN)) = IS
            KAD(IN) = KAD(IN) + 1
            IN       =INTBUF_TAB(NI)%IRECTS(2*(IS-1)+2)
            NOD2RTMS(KAD(IN)) = IS
            KAD(IN) = KAD(IN) + 1
         ENDDO
C
         KSUB=0
         DO JSUB=1,NINTSUB
            ID1=NOM_OPT(1,NINTER+JSUB)
            CALL FRETITL2(TITR1,
     .              NOM_OPT(LNOPT1-LTITR+1,NINTER+JSUB),LTITR)
            IF(NOM_OPT(2,NINTER+JSUB)==NOINT
     .         .AND.NOM_OPT(5,NINTER+JSUB)==1)THEN
               KSUB=KSUB+1
C      LISUB(KSUB)=JSUB no interne de la sous-interface
               INTBUF_TAB(NI)%LISUB(KSUB) = JSUB
                  INTBUF_TAB(NI)%TYPSUB(KSUB) = 1
C
               ISU  =NOM_OPT(4,NINTER+JSUB)
               NNE  =IGRSURF(ISU)%NSEG
               TAGLINS(1:MAXRTMS) = 0
C
               DO I=1,NNE
                  DO INOD=1,2
                    IN=IGRSURF(ISU)%NODES(I,2*INOD)
                    DO JAD=IADD(IN),IADD(IN+1)-1
                      IS = NOD2RTMS(JAD)
                      FOUND_LINE = 0
                      DO J=1,2
                        II=INTBUF_TAB(NI)%IRECTS(2*(IS-1)+J)
                        FOUND=0
                        DO K=1,4
                          IF(IGRSURF(ISU)%NODES(I,K)==II) FOUND=1
                        END DO
                        FOUND_LINE = FOUND_LINE + FOUND
                      END DO
                      IF ((FOUND_LINE==2).AND.(TAGLINS(IS)==0)) THEN
                        INTBUF_TAB(NI)%ADDSUBS(IS)=INTBUF_TAB(NI)%ADDSUBS(IS)+1
C                       Line must be taken into account only one time for one subinterface
                        TAGLINS(IS) = 1
                      ENDIF
                    ENDDO
                  ENDDO
                END DO
C
C---------Case of subinter defined with inter 0 -------------
C
            ELSEIF(NOM_OPT(2,NINTER+JSUB) == 0 
     .         .AND. NOM_OPT(5,NINTER+JSUB) == 1) THEN

               KSUB=KSUB+1
C
C      LISUB(KSUB)=JSUB no interne de la sous-interface
               INTBUF_TAB(NI)%LISUB (KSUB) = JSUB
C
C      prepare ADDSUBS , LISUBS (K31) : 
C      LISUBS(ADDSUBS(IS):ADDSUBS(IS+1)-1) SS. INTERF CONTENANT LE ND SECOND. IS
C
               TAGLINS(1:MAXRTMS) = 0

               ISU2 =NOM_OPT(6,NINTER+JSUB)  ! SURFACE ID 2
               IF(ISU2/=0)THEN
                  INTBUF_TAB(NI)%TYPSUB(KSUB) = 2
                  DO I=1,IGRSURF(ISU2)%NSEG

                     DO INOD=1,2
                       IN=IGRSURF(ISU2)%NODES(I,2*INOD)
                       DO JAD=IADD(IN),IADD(IN+1)-1
                          IS = NOD2RTMS(JAD)
                          FOUND_LINE = 0
                          DO J=1,2
                             II=INTBUF_TAB(NI)%IRECTS(2*(IS-1)+J)
                             FOUND=0
                             DO K=1,4
                                IF(IGRSURF(ISU2)%NODES(I,K)==II) FOUND=1
                             END DO
                             FOUND_LINE = FOUND_LINE + FOUND
                          END DO
                          IF ((FOUND_LINE==2).AND.(TAGLINS(IS)==0)) THEN
                             INTBUF_TAB(NI)%ADDSUBS(IS)=INTBUF_TAB(NI)%ADDSUBS(IS)+1
C                          Line must be taken into account only one time for one subinterface
                             TAGLINS(IS) = 1
                          ENDIF
                       ENDDO
                     ENDDO

                  ENDDO 

               ENDIF

               ISU1 =NOM_OPT(3,NINTER+JSUB)  ! SURFACE ID 1
               IF(ISU1/=0)THEN
                  INTBUF_TAB(NI)%TYPSUB(KSUB) = 3
                  DO I=1,IGRSURF(ISU1)%NSEG

                     DO INOD=1,2
                       IN=IGRSURF(ISU1)%NODES(I,2*INOD)
                       DO JAD=IADD(IN),IADD(IN+1)-1
                          IS = NOD2RTMS(JAD)
                          FOUND_LINE = 0
                          DO J=1,2
                             II=INTBUF_TAB(NI)%IRECTS(2*(IS-1)+J)
                             FOUND=0
                             DO K=1,4
                                IF(IGRSURF(ISU1)%NODES(I,K)==II) FOUND=1
                             END DO
                             FOUND_LINE = FOUND_LINE + FOUND
                          END DO
                          IF ((FOUND_LINE==2).AND.(TAGLINS(IS)==0)) THEN
                             INTBUF_TAB(NI)%ADDSUBS(IS)=INTBUF_TAB(NI)%ADDSUBS(IS)+1
C                          Line must be taken into account only one time for one subinterface
                             TAGLINS(IS) = 1
                          ENDIF
                       ENDDO
                     ENDDO

                  ENDDO 

               ENDIF
C
C

            END IF
          END DO
C
          CUR=1
          DO IS=1,NRTS
             NEXT	                =CUR+INTBUF_TAB(NI)%ADDSUBS(IS)
             INTBUF_TAB(NI)%ADDSUBS(IS)=CUR
             CUR	                =NEXT
          END DO
          INTBUF_TAB(NI)%ADDSUBS(NRTS+1)=CUR
C
C           utilise KAD(1:NRTM)
          DO IS=1,NRTS
             KAD(IS)=INTBUF_TAB(NI)%ADDSUBS(IS)
          END DO
C
C      prepare LISUBS :
C
          KSUB=0
          DO JSUB=1,NINTSUB
             IF(NOM_OPT(2,NINTER+JSUB)==NOINT
     .          .AND.NOM_OPT(5,NINTER+JSUB)==1)THEN
               KSUB=KSUB+1
C
               ISU  =NOM_OPT(4,NINTER+JSUB)
               NNE  =IGRSURF(ISU)%NSEG
               TAGLINS(1:MAXRTMS) = 0
C
               DO I=1,NNE
                  DO INOD=1,2
                    IN=IGRSURF(ISU)%NODES(I,2*INOD)
                    DO JAD=IADD(IN),IADD(IN+1)-1
                      IS = NOD2RTMS(JAD)
                      FOUND_LINE = 0
                      DO J=1,2
                        II=INTBUF_TAB(NI)%IRECTS(2*(IS-1)+J)
                        FOUND=0
                        DO K=1,4
                          IF(IGRSURF(ISU)%NODES(I,K)==II) FOUND=1
                        END DO
                        FOUND_LINE = FOUND_LINE + FOUND
                      END DO
                      IF ((FOUND_LINE==2).AND.(TAGLINS(IS)==0)) THEN
                        INTBUF_TAB(NI)%LISUBS(KAD(IS))=KSUB
                        KAD(IS)=KAD(IS)+1
C                       Line must be taken into account only one time for one subinterface
                        TAGLINS(IS) = 1
                      ENDIF
                    ENDDO
                  ENDDO
               END DO
C---------Case of subinter defined with inter 0 -------------
C
           ELSEIF(NOM_OPT(2,NINTER+JSUB) == 0 
     .         .AND. NOM_OPT(5,NINTER+JSUB) == 1) THEN

               KSUB=KSUB+1
C
C      LISUB(KSUB)=JSUB no interne de la sous-interface
               INTBUF_TAB(NI)%LISUB (KSUB) = JSUB
C
C      prepare ADDSUBS , LISUBS (K31) : 
C      LISUBS(ADDSUBS(IS):ADDSUBS(IS+1)-1) SS. INTERF CONTENANT LE ND SECOND. IS
C
               TAGLINS(1:MAXRTMS) = 0

               ISU2 =NOM_OPT(6,NINTER+JSUB)  ! SURFACE ID 2
               IF(ISU2/=0)THEN
                  DO I=1,IGRSURF(ISU2)%NSEG

                     DO INOD=1,2
                       IN=IGRSURF(ISU2)%NODES(I,2*INOD)
                       DO JAD=IADD(IN),IADD(IN+1)-1
                          IS = NOD2RTMS(JAD)
                          FOUND_LINE = 0
                          DO J=1,2
                             II=INTBUF_TAB(NI)%IRECTS(2*(IS-1)+J)
                             FOUND=0
                             DO K=1,4
                                IF(IGRSURF(ISU2)%NODES(I,K)==II) FOUND=1
                             END DO
                             FOUND_LINE = FOUND_LINE + FOUND
                          END DO

                          IF (FOUND_LINE==2)THEN
                            IF(TAGLINS(IS)==0) THEN
                              INTBUF_TAB(NI)%INFLG_SUBS(KAD(IS))=
     .                          BITSET(INTBUF_TAB(NI)%INFLG_SUBS(KAD(IS)),0)
                              INTBUF_TAB(NI)%LISUBS(KAD(IS))=KSUB
                              KAD(IS)=KAD(IS)+1
C                          Line must be taken into account only one time for one subinterface
                              TAGLINS(IS) = 1                     
                            ELSE                   
                              INTBUF_TAB(NI)%INFLG_SUBS(KAD(IS)-1)=
     .                           BITSET(INTBUF_TAB(NI)%INFLG_SUBS(KAD(IS)-1),0)
                            ENDIF
                          ENDIF
                       ENDDO
                     ENDDO

                  ENDDO 

              ENDIF
C
              ISU1 =NOM_OPT(3,NINTER+JSUB)  ! SURFACE ID 1
               IF(ISU1/=0)THEN
                  DO I=1,IGRSURF(ISU1)%NSEG

                     DO INOD=1,2
                       IN=IGRSURF(ISU1)%NODES(I,2*INOD)
                       DO JAD=IADD(IN),IADD(IN+1)-1
                          IS = NOD2RTMS(JAD)
                          FOUND_LINE = 0
                          DO J=1,2
                             II=INTBUF_TAB(NI)%IRECTS(2*(IS-1)+J)
                             FOUND=0
                             DO K=1,4
                                IF(IGRSURF(ISU1)%NODES(I,K)==II) FOUND=1
                             END DO
                             FOUND_LINE = FOUND_LINE + FOUND
                          END DO
  
                          IF (FOUND_LINE==2)THEN
                            IF(TAGLINS(IS)==0) THEN
                              INTBUF_TAB(NI)%INFLG_SUBS(KAD(IS))=
     .                          BITSET(INTBUF_TAB(NI)%INFLG_SUBS(KAD(IS)),1)
                              INTBUF_TAB(NI)%LISUBS(KAD(IS))=KSUB
                              KAD(IS)=KAD(IS)+1
C                          Line must be taken into account only one time for one subinterface
                              TAGLINS(IS) = 1                     
                            ELSE                   
                              INTBUF_TAB(NI)%INFLG_SUBS(KAD(IS)-1)=
     .                           BITSET(INTBUF_TAB(NI)%INFLG_SUBS(KAD(IS)-1),1)
                            ENDIF
                          ENDIF
                       ENDDO
                     ENDDO

                  ENDDO 

              ENDIF
            END IF
          END DO
C
C--- MAIN Side--
C
          IADD(1:NUMNOD+1) = 0
          DO IM=1,NRTM
             IN       =INTBUF_TAB(NI)%IRECTM(2*(IM-1)+1)
             IADD(IN) =IADD(IN)+1
             IN       =INTBUF_TAB(NI)%IRECTM(2*(IM-1)+2)
             IADD(IN) =IADD(IN)+1
          END DO
C
          CUR=1
          DO I=1,NUMNOD
             NEXT     =CUR+IADD(I)
             IADD(I)  =CUR
             CUR      =NEXT
          END DO
          IADD(NUMNOD+1)=CUR
C
          DO I=1,NUMNOD
             KAD(I)=IADD(I)
          END DO
C
          DO IM=1,NRTM
             IN       =INTBUF_TAB(NI)%IRECTM(2*(IM-1)+1)
             NOD2RTMM(KAD(IN)) = IM
             KAD(IN) = KAD(IN) + 1
             IN       =INTBUF_TAB(NI)%IRECTM(2*(IM-1)+2)
             NOD2RTMM(KAD(IN)) = IM
             KAD(IN) = KAD(IN) + 1
           ENDDO
C
           KSUB=0
           DO JSUB=1,NINTSUB
              ID1=NOM_OPT(1,NINTER+JSUB)
              CALL FRETITL2(TITR1,
     .                NOM_OPT(LNOPT1-LTITR+1,NINTER+JSUB),LTITR)
              IF(NOM_OPT(2,NINTER+JSUB)==NOINT
     .           .AND.NOM_OPT(5,NINTER+JSUB)==1)THEN
                KSUB=KSUB+1
C
                ISU  =NOM_OPT(3,NINTER+JSUB)
                NNE  =IGRSURF(ISU)%NSEG
                TAGLINM(1:MAXRTMS) = 0
C
                DO I=1,NNE
                  DO INOD=1,2
                    IN=IGRSURF(ISU)%NODES(I,2*INOD)
                    DO JAD=IADD(IN),IADD(IN+1)-1
                      IM = NOD2RTMM(JAD)
                      FOUND_LINE = 0
                      DO J=1,2
                        II=INTBUF_TAB(NI)%IRECTM(2*(IM-1)+J)
                        FOUND=0
                        DO K=1,4
                          IF(IGRSURF(ISU)%NODES(I,K)==II) FOUND=1
                        END DO
                        FOUND_LINE = FOUND_LINE + FOUND
                      END DO
                      IF ((FOUND_LINE==2).AND.(TAGLINM(IM)==0)) THEN
                        INTBUF_TAB(NI)%ADDSUBM(IM)=INTBUF_TAB(NI)%ADDSUBM(IM)+1
C                       Line must be taken into account only one time for one subinterface
                        TAGLINM(IM) = 1
                      ENDIF
                    ENDDO
                  ENDDO
                END DO
C---------Case of subinter defined with inter 0 -------------
C
           ELSEIF(NOM_OPT(2,NINTER+JSUB) == 0 
     .         .AND. NOM_OPT(5,NINTER+JSUB) == 1) THEN

               KSUB=KSUB+1
C
C      LISUB(KSUB)=JSUB no interne de la sous-interface
               INTBUF_TAB(NI)%LISUB (KSUB) = JSUB
C
C      prepare ADDSUBS , LISUBS (K31) : 
C      LISUBS(ADDSUBS(IS):ADDSUBS(IS+1)-1) SS. INTERF CONTENANT LE ND SECOND. IS
C
               TAGLINM(1:MAXRTMS) = 0

               ISU1 =NOM_OPT(3,NINTER+JSUB)  ! SURFACE ID 1
               IF(ISU1/=0)THEN
                  DO I=1,IGRSURF(ISU1)%NSEG

                     DO INOD=1,2
                       IN=IGRSURF(ISU1)%NODES(I,2*INOD)
                       DO JAD=IADD(IN),IADD(IN+1)-1
                          IM = NOD2RTMM(JAD)
                          FOUND_LINE = 0
                          DO J=1,2
                             II=INTBUF_TAB(NI)%IRECTM(2*(IS-1)+J)
                             FOUND=0
                             DO K=1,4
                                IF(IGRSURF(ISU1)%NODES(I,K)==II) FOUND=1
                             END DO
                             FOUND_LINE = FOUND_LINE + FOUND
                          END DO
                          IF ((FOUND_LINE==2).AND.(TAGLINM(IM)==0)) THEN
                             INTBUF_TAB(NI)%ADDSUBM(IM)=INTBUF_TAB(NI)%ADDSUBM(IM)+1
C                          Line must be taken into account only one time for one subinterface
                             TAGLINM(IM) = 1
                          ENDIF

                       ENDDO
                     ENDDO

                  ENDDO 

                ENDIF


               ISU2 =NOM_OPT(6,NINTER+JSUB)  ! SURFACE ID 2
               IF(ISU2/=0)THEN
                  DO I=1,IGRSURF(ISU2)%NSEG

                     DO INOD=1,2
                       IN=IGRSURF(ISU2)%NODES(I,2*INOD)
                       DO JAD=IADD(IN),IADD(IN+1)-1
                          IM = NOD2RTMM(JAD)
                          FOUND_LINE = 0
                          DO J=1,2
                             II=INTBUF_TAB(NI)%IRECTM(2*(IS-1)+J)
                             FOUND=0
                             DO K=1,4
                                IF(IGRSURF(ISU2)%NODES(I,K)==II) FOUND=1
                             END DO
                             FOUND_LINE = FOUND_LINE + FOUND
                          END DO
                          IF ((FOUND_LINE==2).AND.(TAGLINM(IM)==0)) THEN
                             INTBUF_TAB(NI)%ADDSUBM(IM)=INTBUF_TAB(NI)%ADDSUBM(IM)+1
C                          Line must be taken into account only one time for one subinterface
                             TAGLINM(IM) = 1
                          ENDIF
                       ENDDO
                     ENDDO

                  ENDDO 

                ENDIF
C
C
             END IF
           END DO
C
           CUR=1
           DO IM=1,NRTM
              NEXT	                =CUR+INTBUF_TAB(NI)%ADDSUBM(IM)
              INTBUF_TAB(NI)%ADDSUBM(IM)=CUR
              CUR	                =NEXT
           END DO
           INTBUF_TAB(NI)%ADDSUBM(NRTM+1)=CUR
C
           DO IM=1,NRTM
              KAD(IM)=INTBUF_TAB(NI)%ADDSUBM(IM)
           END DO
C
C      prepare LISUBM : 
C
           KSUB=0
           DO JSUB=1,NINTSUB
              IF(NOM_OPT(2,NINTER+JSUB)==NOINT
     .           .AND.NOM_OPT(5,NINTER+JSUB)==1)THEN
                KSUB=KSUB+1
C
                ISU  =NOM_OPT(3,NINTER+JSUB)
                NNE  =IGRSURF(ISU)%NSEG
                TAGLINM(1:MAXRTMS) = 0
C
                DO I=1,NNE
                  DO INOD=1,2
                    IN=IGRSURF(ISU)%NODES(I,2*INOD)
                    DO JAD=IADD(IN),IADD(IN+1)-1
                      IM = NOD2RTMM(JAD)
                      FOUND_LINE = 0
                      DO J=1,2
                        II=INTBUF_TAB(NI)%IRECTM(2*(IM-1)+J)
                        FOUND=0
                        DO K=1,4
                          IF(IGRSURF(ISU)%NODES(I,K)==II) FOUND=1
                        END DO
                        FOUND_LINE = FOUND_LINE + FOUND
                      END DO
                      IF ((FOUND_LINE==2).AND.(TAGLINM(IM)==0)) THEN
                        INTBUF_TAB(NI)%LISUBM(KAD(IM))=KSUB
                        KAD(IM)=KAD(IM)+1
C                       Line must be taken into account only one time for one subinterface
                        TAGLINM(IM) = 1
                      ENDIF
                    ENDDO
                  ENDDO
                END DO
C
C---------Case of subinter defined with inter 0 -------------
C
           ELSEIF(NOM_OPT(2,NINTER+JSUB) == 0 
     .         .AND. NOM_OPT(5,NINTER+JSUB) == 1) THEN

               KSUB=KSUB+1
C
C      LISUB(KSUB)=JSUB no interne de la sous-interface
               INTBUF_TAB(NI)%LISUB (KSUB) = JSUB
C
C      prepare ADDSUBS , LISUBS (K31) : 
C      LISUBS(ADDSUBS(IS):ADDSUBS(IS+1)-1) SS. INTERF CONTENANT LE ND SECOND. IS
C
               TAGLINM(1:MAXRTMS) = 0

               ISU1 =NOM_OPT(3,NINTER+JSUB)  ! SURFACE ID 1
               IF(ISU1/=0)THEN
                  INTBUF_TAB(NI)%TYPSUB(KSUB) = 3
                  DO I=1,IGRSURF(ISU1)%NSEG

                     DO INOD=1,2
                       IN=IGRSURF(ISU1)%NODES(I,2*INOD)
                       DO JAD=IADD(IN),IADD(IN+1)-1
                          IM = NOD2RTMM(JAD)
                          FOUND_LINE = 0
                          DO J=1,2
                             II=INTBUF_TAB(NI)%IRECTM(2*(IS-1)+J)
                             FOUND=0
                             DO K=1,4
                                IF(IGRSURF(ISU1)%NODES(I,K)==II) FOUND=1
                             END DO
                             FOUND_LINE = FOUND_LINE + FOUND
                          END DO
                          IF (FOUND_LINE==2)THEN
                            IF(TAGLINM(IM)==0) THEN
                              INTBUF_TAB(NI)%INFLG_SUBM(KAD(IM))=
     .                          BITSET(INTBUF_TAB(NI)%INFLG_SUBM(KAD(IM)),1)
                              INTBUF_TAB(NI)%LISUBM(KAD(IM))=KSUB
                              KAD(IM)=KAD(IM)+1
C                          Line must be taken into account only one time for one subinterface
                              TAGLINM(IM) = 1                     
                            ELSE                   
                              INTBUF_TAB(NI)%INFLG_SUBM(KAD(IM)-1)=
     .                           BITSET(INTBUF_TAB(NI)%INFLG_SUBM(KAD(IM)-1),1)
                            ENDIF
                          ENDIF
                       ENDDO
                     ENDDO

                  ENDDO 
               ENDIF
C
               ISU2 =NOM_OPT(6,NINTER+JSUB)  ! SURFACE ID 2
               IF(ISU2/=0)THEN
                  INTBUF_TAB(NI)%TYPSUB(KSUB) = 3
                  DO I=1,IGRSURF(ISU2)%NSEG

                     DO INOD=1,2
                       IN=IGRSURF(ISU2)%NODES(I,2*INOD)
                       DO JAD=IADD(IN),IADD(IN+1)-1
                          IM = NOD2RTMM(JAD)
                          FOUND_LINE = 0
                          DO J=1,2
                             II=INTBUF_TAB(NI)%IRECTM(2*(IS-1)+J)
                             FOUND=0
                             DO K=1,4
                                IF(IGRSURF(ISU2)%NODES(I,K)==II) FOUND=1
                             END DO
                             FOUND_LINE = FOUND_LINE + FOUND
                          END DO
                          IF (FOUND_LINE==2)THEN
                            IF(TAGLINM(IM)==0) THEN
                              INTBUF_TAB(NI)%INFLG_SUBM(KAD(IM))=
     .                          BITSET(INTBUF_TAB(NI)%INFLG_SUBM(KAD(IM)),0)
                              INTBUF_TAB(NI)%LISUBM(KAD(IM))=KSUB
                              KAD(IM)=KAD(IM)+1
C                          Line must be taken into account only one time for one subinterface
                              TAGLINM(IM) = 1                     
                            ELSE                   
                              INTBUF_TAB(NI)%INFLG_SUBM(KAD(IM)-1)=
     .                           BITSET(INTBUF_TAB(NI)%INFLG_SUBM(KAD(IM)-1),0)
                            ENDIF
                          ENDIF
                       ENDDO
                     ENDDO

                  ENDDO 
               ENDIF
C
              END IF
           END DO

        ENDIF

C-------------------------------------
      RETURN
      END

