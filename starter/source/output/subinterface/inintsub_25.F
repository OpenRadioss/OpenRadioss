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
Chd|  ININTSUB_25                   source/output/subinterface/inintsub_25.F
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
      SUBROUTINE ININTSUB_25(ITAB      ,IGRNOD ,IGRSURF ,NOM_OPT ,INTBUF_TAB,
     .                       NRTM      ,NRTM0  ,NSN     ,NISUBS  ,NISUBM    ,
     .                       NOINT     ,NI     ,NOD2NSV ,NOD2RTM ,KAD       ,
     .                       TAGNOD    ,TAGRTM ,IADD    )   

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
      INTEGER ITAB(*), NOD2NSV(*), NOD2RTM(*), KAD(*), TAGNOD(*), TAGRTM(*), 
     .        IADD(*) 
      INTEGER NRTM, NRTM0, NSN, NISUBS, NISUBM, NOINT, NI
      INTEGER NOM_OPT(LNOPT1,*)

      TYPE(INTBUF_STRUCT_) INTBUF_TAB(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,K,JGRN,ISU,ISU1,ISU2,
     .   JSUB, KSUB, NNE, IS, ISV, CUR, ID1,
     .   NEXT, IM, KM, JAD, IN, II, N,STAT,K1,K2,NT19,INOD,IFNRT
      CHARACTER MESS*40
      CHARACTER*nchartitle,
     .   TITR,TITR1
C-----------------------------------------------
      TYPE (GROUP_)  , DIMENSION(NGRNOD)  :: IGRNOD
      TYPE (SURF_)   , DIMENSION(NSURF)   :: IGRSURF
      TYPE (SURF_)   , DIMENSION(NSLIN)   :: IGRSLIN
C-----------------------------------------------
C   D a t a
C-----------------------------------------------
      DATA MESS/'SUB-INTERFACES FOR TH INITIALIZATIONS   '/
C-----------------------------------------------
C   E x t e r n a l   F u n c t i o n s
C-----------------------------------------------
      INTEGER BITSET
      EXTERNAL BITSET
C=======================================================================

C
        INTBUF_TAB(NI)%ADDSUBS(1:NSN+1)  = 0   ! address of different subinter related to secondary node
        INTBUF_TAB(NI)%ADDSUBM(1:NRTM+1) = 0   ! address of different subinter related to main segment

        INTBUF_TAB(NI)%INFLG_SUBS(1:NISUBS)=0  ! Flags for determing what is surface Surf1 or Surf2
        INTBUF_TAB(NI)%INFLG_SUBM(1:NISUBM)=0

C----------------------------------------
C  TAG nodes second calculate addresses
C---------------------------------------------

        NOD2NSV(1:NUMNOD) = 0
        DO IS=1,NSN
           ISV         = INTBUF_TAB(NI)%NSV(IS)
           NOD2NSV(ISV)=IS
        END DO
C
        KSUB=0
        DO JSUB=1,NINTSUB
           ID1=NOM_OPT(1,NINTER+JSUB)
           CALL FRETITL2(TITR1,
     .          NOM_OPT(LNOPT1-LTITR+1,NINTER+JSUB),LTITR)

C---------Case of subinter defined with inter -------------

           IF(NOM_OPT(2,NINTER+JSUB)==NOINT
     .           .AND.NOM_OPT(5,NINTER+JSUB)==1)THEN
               KSUB=KSUB+1
C
C      LISUB(KSUB)=JSUB no interne de la sous-interface
               INTBUF_TAB(NI)%LISUB(KSUB) = JSUB
               INTBUF_TAB(NI)%TYPSUB(KSUB) = 1
C
C      prepare ADDSUBS , LISUBS (K31) : 
C      LISUBS(ADDSUBS(IS):ADDSUBS(IS+1)-1) SS. INTERF CONTENANT LE ND SECOND. IS
C
               TAGNOD(1:NUMNOD) = 0

               JGRN =NOM_OPT(4,NINTER+JSUB)  ! GRNOD ID
               IF(JGRN/=0)THEN
                  NNE  =IGRNOD(JGRN)%NENTITY
                  DO I=1,NNE
                    IN=IGRNOD(JGRN)%ENTITY(I)
                    IS =NOD2NSV(IN)
                    IF(IS==0)THEN
                      CALL ANCMSG(MSGID=580,
     .                          MSGTYPE=MSGWARNING,
     .                          ANMODE=ANINFO_BLIND_1,
     .                          I1=ID1,
     .                          C1=TITR1,
     .                          I2=ITAB(IN),
     .                          I3=NOINT)
                    ELSEIF(TAGNOD(IN)==0) THEN ! beware to GRNOD NODENS
                      INTBUF_TAB(NI)%ADDSUBS(IS) = 
     .                         INTBUF_TAB(NI)%ADDSUBS(IS)+1
                      TAGNOD(IN) = 1
                    END IF
                  END DO
               END IF
C
               ISU2  =NOM_OPT(6,NINTER+JSUB) ! SURFACE ID 2
               IF(ISU2/=0)THEN
C
                  ISU1  =NOM_OPT(3,NINTER+JSUB) ! SURFACE ID 1
                  NNE  =IGRSURF(ISU1)%NSEG
                  DO I=1,NNE
                     DO J=1,4
                      IN = IGRSURF(ISU1)%NODES(I,J)
                      IS = NOD2NSV(IN)
                      IF (IS==0) THEN
                        CALL ANCMSG(MSGID=580,
     .                            MSGTYPE=MSGWARNING,
     .                            ANMODE=ANINFO_BLIND_1,
     .                            I1=ID1,
     .                            C1=TITR1,
     .                            I2=ITAB(IN),
     .                            I3=NOINT)
                      ELSEIF (TAGNOD(IN)==0) THEN
                        INTBUF_TAB(NI)%ADDSUBS(IS) = INTBUF_TAB(NI)%ADDSUBS(IS)+1
                        TAGNOD(IN) = 1
                      ENDIF
                    END DO
                 END DO

                 NNE  =IGRSURF(ISU2)%NSEG
                 DO I=1,NNE
                    DO J=1,4
                      IN = IGRSURF(ISU2)%NODES(I,J)
                      IS = NOD2NSV(IN)
                      IF (IS==0) THEN
                        CALL ANCMSG(MSGID=580,
     .                            MSGTYPE=MSGWARNING,
     .                            ANMODE=ANINFO_BLIND_1,
     .                            I1=ID1,
     .                            C1=TITR1,
     .                            I2=ITAB(IN),
     .                            I3=NOINT)
                      ELSEIF (TAGNOD(IN)==0) THEN
                        INTBUF_TAB(NI)%ADDSUBS(IS) = INTBUF_TAB(NI)%ADDSUBS(IS)+1
                        TAGNOD(IN) = 1
                      ENDIF
                    END DO
                 END DO
               ENDIF

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
               TAGNOD(1:NUMNOD) = 0

               ISU2 =NOM_OPT(6,NINTER+JSUB)  ! SURFACE ID 2
               IF(ISU2/=0)THEN
                  INTBUF_TAB(NI)%TYPSUB(KSUB) = 2
                  DO I=1,IGRSURF(ISU2)%NSEG
                   DO K=1,4
                     IN=IGRSURF(ISU2)%NODES(I,K)
                     IS =NOD2NSV(IN)
                     IF(IS/=0.AND.TAGNOD(IN)==0) THEN 
                      INTBUF_TAB(NI)%ADDSUBS(IS) = 
     .                         INTBUF_TAB(NI)%ADDSUBS(IS)+1
                      TAGNOD(IN) = 1
                     END IF
                   ENDDO
                  ENDDO 
               ENDIF

               ISU1 =NOM_OPT(3,NINTER+JSUB)  ! SURFACE ID 1
               IF(ISU1/=0)THEN
                  INTBUF_TAB(NI)%TYPSUB(KSUB) = 3
                  DO I=1,IGRSURF(ISU1)%NSEG
                   DO K=1,4
                     IN=IGRSURF(ISU1)%NODES(I,K)
                     IS =NOD2NSV(IN)
                     IF(IS/=0.AND.TAGNOD(IN)==0) THEN 
                      INTBUF_TAB(NI)%ADDSUBS(IS) = 
     .                         INTBUF_TAB(NI)%ADDSUBS(IS)+1
                      TAGNOD(IN) = 1
                     END IF
                   ENDDO
                  ENDDO 
               ENDIF
C
C
           END IF
        END DO
C
C  --------Skyline tabs ADDSUBS-----------

        CUR=1
        DO IS=1,NSN
           NEXT	                = CUR+INTBUF_TAB(NI)%ADDSUBS(IS)
           INTBUF_TAB(NI)%ADDSUBS(IS)= CUR
            CUR	                = NEXT
        END DO
        INTBUF_TAB(NI)%ADDSUBS(1+NSN)=CUR
C
C           utilise KAD(1:NSN)
        DO IS=1,NSN
           KAD(IS)=INTBUF_TAB(NI)%ADDSUBS(IS)
        END DO
C

C----------------------------------------
C  Secondary side : After ADRESS STORE SUBINTER in INTBUF_TAB(NI)%LISUBS
C---------------------------------------------
        KSUB=0
        DO JSUB=1,NINTSUB
C
            IF(NOM_OPT(2,NINTER+JSUB)==NOINT
     .           .AND.NOM_OPT(5,NINTER+JSUB)==1)THEN
               KSUB=KSUB+1
C
C      prepare LISUBS : 
C      LISUBS(ADDSUBS(IS):ADDSUBS(IS+1)-1) SS. INTERF CONTENANT LE ND SECOND. IS
C
               TAGNOD(1:NUMNOD) = 0

               JGRN =NOM_OPT(4,NINTER+JSUB)  ! GRNOD ID
               IF(JGRN/=0)THEN
                  NNE  =IGRNOD(JGRN)%NENTITY
                  DO I=1,NNE
                    IN=IGRNOD(JGRN)%ENTITY(I)
                    IS =NOD2NSV(IN)
                    IF(IS/=0)THEN
                      IF(TAGNOD(IN)==0) THEN ! beware to GRNOD NODENS
                        INTBUF_TAB(NI)%INFLG_SUBS(KAD(IS))=
     .                    BITSET(INTBUF_TAB(NI)%INFLG_SUBS(KAD(IS)),2)
                        INTBUF_TAB(NI)%LISUBS(KAD(IS))=KSUB
                        KAD(IS)=KAD(IS)+1
                      ELSE                   
                        INTBUF_TAB(NI)%INFLG_SUBS(KAD(IS)-1)=
     .                    BITSET(INTBUF_TAB(NI)%INFLG_SUBS(KAD(IS)-1),2)
                      END IF
                      TAGNOD(IN) = 1                     
                    END IF
                  END DO
               ENDIF
C
               ISU2  =NOM_OPT(6,NINTER+JSUB) ! SURFACE ID 2
               IF(ISU2/=0)THEN
                  ISU1  =NOM_OPT(3,NINTER+JSUB) ! SURFACE ID 1
                  NNE  =IGRSURF(ISU1)%NSEG
                  DO I=1,NNE
                    DO J=1,4
                      IN = IGRSURF(ISU1)%NODES(I,J)
                      IS = NOD2NSV(IN)
                      IF (IS/=0)THEN
                        IF(TAGNOD(IN)==0) THEN
                          INTBUF_TAB(NI)%INFLG_SUBS(KAD(IS))=
     .                      BITSET(INTBUF_TAB(NI)%INFLG_SUBS(KAD(IS)),0)
                          INTBUF_TAB(NI)%LISUBS(KAD(IS))=KSUB
                          KAD(IS)=KAD(IS)+1
                          TAGNOD(IN) = 1  
                        ELSE                   
                          INTBUF_TAB(NI)%INFLG_SUBS(KAD(IS)-1)=
     .                      BITSET(INTBUF_TAB(NI)%INFLG_SUBS(KAD(IS)-1),0)
                        ENDIF
                      ENDIF
                    END DO
                  ENDDO

                  NNE  =IGRSURF(ISU2)%NSEG
                  DO I=1,NNE
                    DO J=1,4
                      IN = IGRSURF(ISU2)%NODES(I,J)
                      IS = NOD2NSV(IN)
                      IF (IS/=0)THEN
                        IF(TAGNOD(IN)==0) THEN
                          INTBUF_TAB(NI)%INFLG_SUBS(KAD(IS))=
     .                      BITSET(INTBUF_TAB(NI)%INFLG_SUBS(KAD(IS)),1)
                          INTBUF_TAB(NI)%LISUBS(KAD(IS))=KSUB
                          KAD(IS)=KAD(IS)+1
                          TAGNOD(IN) = 1                     
                        ELSE                   
                          INTBUF_TAB(NI)%INFLG_SUBS(KAD(IS)-1)=
     .                      BITSET(INTBUF_TAB(NI)%INFLG_SUBS(KAD(IS)-1),1)
                        ENDIF
                      ENDIF
                    END DO
                  ENDDO
               ENDIF
C  

C
C---------Case of subinter defined with inter 0 -------------
C
            ELSEIF(NOM_OPT(2,NINTER+JSUB) == 0 
     .      .AND. NOM_OPT(5,NINTER+JSUB) == 1) THEN

               KSUB=KSUB+1
C
C      prepare ADDSUBS , LISUBS (K31) : 
C      LISUBS(ADDSUBS(IS):ADDSUBS(IS+1)-1) SS. INTERF CONTENANT LE ND SECOND. IS
C
               TAGNOD(1:NUMNOD) = 0

               ISU2 =NOM_OPT(6,NINTER+JSUB)  ! SURFACE ID 2
               IF(ISU2/=0)THEN

                  DO I=1,IGRSURF(ISU2)%NSEG
                   DO K=1,4
                     IN=IGRSURF(ISU2)%NODES(I,K)
                     IS =NOD2NSV(IN)
                     IF (IS/=0)THEN
                        IF(TAGNOD(IN)==0) THEN
                          INTBUF_TAB(NI)%INFLG_SUBS(KAD(IS))=
     .                      BITSET(INTBUF_TAB(NI)%INFLG_SUBS(KAD(IS)),0)
                          INTBUF_TAB(NI)%LISUBS(KAD(IS))=KSUB
                          KAD(IS)=KAD(IS)+1
                          TAGNOD(IN) = 1                     
                        ELSE                   
                          INTBUF_TAB(NI)%INFLG_SUBS(KAD(IS)-1)=
     .                      BITSET(INTBUF_TAB(NI)%INFLG_SUBS(KAD(IS)-1),0)
                        ENDIF
                     ENDIF

                   ENDDO
                  ENDDO 
               ENDIF

               ISU1 =NOM_OPT(3,NINTER+JSUB)  ! SURFACE ID 1
               IF(ISU1/=0)THEN

                  DO I=1,IGRSURF(ISU1)%NSEG
                   DO K=1,4
                     IN=IGRSURF(ISU1)%NODES(I,K)
                     IS =NOD2NSV(IN)
                     IF (IS/=0)THEN
                       IF(TAGNOD(IN)==0) THEN
                          INTBUF_TAB(NI)%INFLG_SUBS(KAD(IS))=
     .                      BITSET(INTBUF_TAB(NI)%INFLG_SUBS(KAD(IS)),1)
                          INTBUF_TAB(NI)%LISUBS(KAD(IS))=KSUB
                          KAD(IS)=KAD(IS)+1
                          TAGNOD(IN) = 1                     
                       ELSE                   
                          INTBUF_TAB(NI)%INFLG_SUBS(KAD(IS)-1)=
     .                      BITSET(INTBUF_TAB(NI)%INFLG_SUBS(KAD(IS)-1),1)
                       ENDIF
                     ENDIF
                   ENDDO
                  ENDDO 
               ENDIF

            END IF
        END DO


C----------------------------------------
C  TAG main segments calculate addresses
C---------------------------------------------

C
C           utilise IADD(1:NUMNOD+1)
        IADD(1:NUMNOD+1) = 0

        DO IM=1,NRTM0
           IN       =INTBUF_TAB(NI)%IRECTM(4*(IM-1)+1)
           IADD(IN) =IADD(IN)+1
           IN       =INTBUF_TAB(NI)%IRECTM(4*(IM-1)+2)
           IADD(IN) =IADD(IN)+1
           IN       =INTBUF_TAB(NI)%IRECTM(4*(IM-1)+3)
           IADD(IN) =IADD(IN)+1
           IN       =INTBUF_TAB(NI)%IRECTM(4*(IM-1)+4)
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
C           utilise KAD(NUMNOD)
        DO I=1,NUMNOD
           KAD(I)=IADD(I)
        END DO
C
C           utilise NOD2RTM(4*NRTM0)
        DO IM=1,NRTM0
           IN       =INTBUF_TAB(NI)%IRECTM(4*(IM-1)+1)
           NOD2RTM(KAD(IN)) = IM
           KAD(IN) = KAD(IN) + 1
           IN       =INTBUF_TAB(NI)%IRECTM(4*(IM-1)+2)
           NOD2RTM(KAD(IN)) = IM
           KAD(IN) = KAD(IN) + 1
           IN       =INTBUF_TAB(NI)%IRECTM(4*(IM-1)+3)
           NOD2RTM(KAD(IN)) = IM
           KAD(IN) = KAD(IN) + 1
           IN       =INTBUF_TAB(NI)%IRECTM(4*(IM-1)+4)
           NOD2RTM(KAD(IN)) = IM
              KAD(IN) = KAD(IN) + 1
        ENDDO

C
C      prepare ADDSUBM : 
        KSUB=0
        DO JSUB=1,NINTSUB
           ID1=NOM_OPT(1,NINTER+JSUB)
           CALL FRETITL2(TITR1,
     .                NOM_OPT(LNOPT1-LTITR+1,NINTER+JSUB),LTITR)

           IF(NOM_OPT(2,NINTER+JSUB)==NOINT
     .           .AND.NOM_OPT(5,NINTER+JSUB)==1)THEN
                KSUB=KSUB+1
C
                TAGRTM(1:NRTM0) = 0
C
                ISU1  =NOM_OPT(3,NINTER+JSUB)
                NNE  =IGRSURF(ISU1)%NSEG
                DO I=1,NNE
                  IN=IGRSURF(ISU1)%NODES(I,1)
                  KM=0

                  DO 310 JAD=IADD(IN),IADD(IN+1)-1
                    IM = NOD2RTM(JAD)
                    DO 300 J=1,4
                      II=IGRSURF(ISU1)%NODES(I,J)
                      IF(J==4.AND.II==0)THEN
                        GO TO 300
                      ELSE
                        DO K=1,4
                           IF(INTBUF_TAB(NI)%IRECTM(4*(IM-1)+K)==II) GOTO 300
                        END DO
                        GOTO 310
                      END IF
  300               CONTINUE
                    KM=IM
                    GO TO 320
  310             CONTINUE
  320             CONTINUE


                  IF(KM==0)THEN
                     CALL ANCMSG(MSGID=581,
     .                        MSGTYPE=MSGWARNING,
     .                        ANMODE=ANINFO_BLIND_1,
     .                        I1=ID1,
     .                        C1=TITR1,
     .                        I2=ITAB(IGRSURF(ISU1)%NODES(I,1)),
     .                        I3=ITAB(IGRSURF(ISU1)%NODES(I,2)),
     .                        I4=ITAB(IGRSURF(ISU1)%NODES(I,3)),
     .                        I5=ITAB(IGRSURF(ISU1)%NODES(I,4)),
     .                        I6=NOINT)
                  ELSEIF(TAGRTM(KM)==0)THEN
                    INTBUF_TAB(NI)%ADDSUBM(KM)=INTBUF_TAB(NI)%ADDSUBM(KM)+1
                    TAGRTM(KM)=1
                  END IF
                END DO
C
                ISU2  =NOM_OPT(6,NINTER+JSUB)
                IF(ISU2/=0)THEN
                  NNE  =IGRSURF(ISU2)%NSEG
                  DO I=1,NNE
                    IN=IGRSURF(ISU2)%NODES(I,1)
                    KM=0
                    DO 360 JAD=IADD(IN),IADD(IN+1)-1
                      IM = NOD2RTM(JAD)
                      DO 350 J=1,4
                        II=IGRSURF(ISU2)%NODES(I,J)
                        IF(J==4.AND.II==0)THEN
                          GO TO 350
                        ELSE
                          DO K=1,4
                             IF(INTBUF_TAB(NI)%IRECTM(4*(IM-1)+K)==II) GOTO 350
                          END DO
                          GOTO 360
                        END IF
  350                 CONTINUE
                      KM=IM
                      GO TO 370
  360               CONTINUE
  370               CONTINUE

                    IF(KM==0)THEN
                       CALL ANCMSG(MSGID=581,
     .                          MSGTYPE=MSGWARNING,
     .                          ANMODE=ANINFO_BLIND_1,
     .                          I1=ID1,
     .                          C1=TITR1,
     .                          I2=ITAB(IGRSURF(ISU2)%NODES(I,1)),
     .                          I3=ITAB(IGRSURF(ISU2)%NODES(I,2)),
     .                          I4=ITAB(IGRSURF(ISU2)%NODES(I,3)),
     .                          I5=ITAB(IGRSURF(ISU2)%NODES(I,4)),
     .                          I6=NOINT)
                    ELSEIF(TAGRTM(KM)==0)THEN
                      INTBUF_TAB(NI)%ADDSUBM(KM)=INTBUF_TAB(NI)%ADDSUBM(KM)+1
                      TAGRTM(KM)=1
                    END IF
                  END DO
                END IF
C
C


C
C---------Case of subinter defined with inter 0 -------------
C
           ELSEIF(NOM_OPT(2,NINTER+JSUB) == 0 
     .      .AND. NOM_OPT(5,NINTER+JSUB) == 1) THEN

                KSUB=KSUB+1
C
                TAGRTM(1:NRTM0) = 0
C
                ISU1  =NOM_OPT(3,NINTER+JSUB)

                IF(ISU1 > 0 ) THEN

                  NNE  =IGRSURF(ISU1)%NSEG
                  DO I=1,NNE
                     IN=IGRSURF(ISU1)%NODES(I,1)
                     KM=0

                     DO JAD=IADD(IN),IADD(IN+1)-1
                       IM = NOD2RTM(JAD)
                       IFNRT = 0
                       DO J=1,4
                         II=IGRSURF(ISU1)%NODES(I,J)
                         IF(J/=4.OR.II/=0)THEN
                           DO K=1,4
                              IF(INTBUF_TAB(NI)%IRECTM(4*(IM-1)+K)==II) IFNRT = IFNRT + 1
                           END DO
                         ENDIF
                       ENDDO
                       IF(IFNRT >= 3) THEN
                          KM=IM 
                          EXIT
                       ENDIF
                    ENDDO

                    IF(KM/=0)THEN
                      IF(TAGRTM(KM)==0)THEN
                        INTBUF_TAB(NI)%ADDSUBM(KM)=INTBUF_TAB(NI)%ADDSUBM(KM)+1
                        TAGRTM(KM)=1
                      ENDIF
                    END IF
                  END DO
                ENDIF
C
                ISU2  =NOM_OPT(6,NINTER+JSUB)

                IF(ISU2 > 0 ) THEN

                  NNE  =IGRSURF(ISU2)%NSEG
                  DO I=1,NNE
                     IN=IGRSURF(ISU2)%NODES(I,1)
                     KM=0

                     DO JAD=IADD(IN),IADD(IN+1)-1
                       IM = NOD2RTM(JAD)
                       IFNRT = 0
                       DO J=1,4
                         II=IGRSURF(ISU2)%NODES(I,J)
                         IF(J/=4.OR.II/=0)THEN
                           DO K=1,4
                              IF(INTBUF_TAB(NI)%IRECTM(4*(IM-1)+K)==II) IFNRT = IFNRT + 1
                           END DO
                         ENDIF
                       ENDDO
                       IF(IFNRT >= 3) THEN
                          KM=IM 
                          EXIT
                       ENDIF
                    ENDDO

                    IF(KM/=0)THEN
                      IF(TAGRTM(KM)==0) THEN
                         INTBUF_TAB(NI)%ADDSUBM(KM)=INTBUF_TAB(NI)%ADDSUBM(KM)+1
                         TAGRTM(KM)=1
                      ENDIF
                    END IF
                  END DO
                ENDIF
C
           END IF

        END DO
C
        CUR=1
        DO IM=1,NRTM0
           NEXT	                =CUR+INTBUF_TAB(NI)%ADDSUBM(IM)
           INTBUF_TAB(NI)%ADDSUBM(IM)=CUR
           CUR	                =NEXT
        END DO
        INTBUF_TAB(NI)%ADDSUBM(NRTM0+1:NRTM+1)=CUR
C
C           utilise KAD(1:NRTM0)
        DO IM=1,NRTM0
           KAD(IM)=INTBUF_TAB(NI)%ADDSUBM(IM)
        END DO
C
C      prepare LISUBM : 
C      LISUBM(ADDSUBM(IM):ADDSUBM(IM+1)-1) SS. INTERF CONTENANT LE SEG.MAIN IM
        KSUB=0
        DO JSUB=1,NINTSUB
           IF(NOM_OPT(2,NINTER+JSUB)==NOINT
     .           .AND.NOM_OPT(5,NINTER+JSUB)==1)THEN
                KSUB=KSUB+1
C
                TAGRTM(1:NRTM0) = 0
C
                ISU1 =NOM_OPT(3,NINTER+JSUB)
                NNE  =IGRSURF(ISU1)%NSEG
                DO I=1,NNE
                  IN=IGRSURF(ISU1)%NODES(I,1)
                  KM=0
                  DO 410 JAD=IADD(IN),IADD(IN+1)-1
                    IM = NOD2RTM(JAD)
                    DO 400 J=1,4
                      II=IGRSURF(ISU1)%NODES(I,J)
                      IF(J==4.AND.II==0)THEN
                        GO TO 400
                      ELSE
                        DO K=1,4
                          IF(INTBUF_TAB(NI)%IRECTM(4*(IM-1)+K)==II) GOTO 400
                        END DO
                        GOTO 410
                      END IF
  400               CONTINUE
                    KM=IM
                    GOTO 420
  410             CONTINUE
  420             CONTINUE
                  IF(KM/=0)THEN
                    IF(TAGRTM(IM)==0)THEN
                      INTBUF_TAB(NI)%INFLG_SUBM(KAD(IM))=
     .                  BITSET(INTBUF_TAB(NI)%INFLG_SUBM(KAD(IM)),0)
                      INTBUF_TAB(NI)%LISUBM(KAD(IM))=KSUB
                      KAD(IM)=KAD(IM)+1
                      TAGRTM(IM)=1
C                   ELSE
C                     INTBUF_TAB(NI)%INFLG_SUBM(KAD(IM)-1)=
C    .                  BITSET(INTBUF_TAB(NI)%INFLG_SUBM(KAD(IM)-1),0)
                    END IF
                  END IF
                END DO

                ISU2  =NOM_OPT(6,NINTER+JSUB)
                IF(ISU2/=0)THEN
                  NNE  =IGRSURF(ISU2)%NSEG
                  DO I=1,NNE
                    IN=IGRSURF(ISU2)%NODES(I,1)
                    KM=0
                    DO 460 JAD=IADD(IN),IADD(IN+1)-1
                      IM = NOD2RTM(JAD)
                      DO 450 J=1,4
                        II=IGRSURF(ISU2)%NODES(I,J)
                        IF(J==4.AND.II==0)THEN
                          GO TO 450
                        ELSE
                          DO K=1,4
                            IF(INTBUF_TAB(NI)%IRECTM(4*(IM-1)+K)==II) GOTO 450
                          END DO
                          GOTO 460
                        END IF
  450                 CONTINUE
                      KM=IM
                      GOTO 470
  460               CONTINUE
  470               CONTINUE
                    IF(KM/=0)THEN
                      IF(TAGRTM(IM)==0)THEN
                        INTBUF_TAB(NI)%INFLG_SUBM(KAD(IM))=
     .                    BITSET(INTBUF_TAB(NI)%INFLG_SUBM(KAD(IM)),1)
                        INTBUF_TAB(NI)%LISUBM(KAD(IM))=KSUB
                        KAD(IM)=KAD(IM)+1
                        TAGRTM(IM)=1
                      ELSE
                        INTBUF_TAB(NI)%INFLG_SUBM(KAD(IM)-1)=
     .                    BITSET(INTBUF_TAB(NI)%INFLG_SUBM(KAD(IM)-1),1)
                      END IF
                    END IF
                  END DO
                END IF



C
C---------Case of subinter defined with inter 0 -------------
C
           ELSEIF(NOM_OPT(2,NINTER+JSUB) == 0 
     .         .AND. NOM_OPT(5,NINTER+JSUB) == 1) THEN

                KSUB=KSUB+1
C
                TAGRTM(1:NRTM0) = 0
C

                ISU1 =NOM_OPT(3,NINTER+JSUB)
                IF(ISU1 > 0 ) THEN

                  NNE  =IGRSURF(ISU1)%NSEG
                  DO I=1,NNE
                    IN=IGRSURF(ISU1)%NODES(I,1)
                    KM=0

                    DO JAD=IADD(IN),IADD(IN+1)-1
                      IM = NOD2RTM(JAD)
                      IFNRT = 0
                      DO J=1,4
                        II=IGRSURF(ISU1)%NODES(I,J)
                        IF(J/=4.OR.II/=0)THEN
                          DO K=1,4
                             IF(INTBUF_TAB(NI)%IRECTM(4*(IM-1)+K)==II) IFNRT = IFNRT + 1
                          END DO
                        ENDIF
                      ENDDO
                      IF(IFNRT >= 3) THEN
                         KM=IM 
                         EXIT
                      ENDIF
                    ENDDO

                    IF(KM/=0)THEN
                      IF(TAGRTM(IM)==0)THEN
                        INTBUF_TAB(NI)%INFLG_SUBM(KAD(IM))=
     .                    BITSET(INTBUF_TAB(NI)%INFLG_SUBM(KAD(IM)),1)
                        INTBUF_TAB(NI)%LISUBM(KAD(IM))=KSUB
                        KAD(IM)=KAD(IM)+1
                        TAGRTM(IM)=1
                      ENDIF
                    END IF

                  END DO
C
                ENDIF
C
                ISU2 =NOM_OPT(6,NINTER+JSUB)
                IF(ISU2 > 0 ) THEN

                  NNE  =IGRSURF(ISU2)%NSEG
                  DO I=1,NNE
                    IN=IGRSURF(ISU2)%NODES(I,1)
                    KM=0

                    DO JAD=IADD(IN),IADD(IN+1)-1
                      IM = NOD2RTM(JAD)
                      IFNRT = 0
                      DO J=1,4
                        II=IGRSURF(ISU2)%NODES(I,J)
                        IF(J/=4.OR.II/=0)THEN
                          DO K=1,4
                             IF(INTBUF_TAB(NI)%IRECTM(4*(IM-1)+K)==II) IFNRT = IFNRT + 1
                          END DO
                        ENDIF
                      ENDDO
                      IF(IFNRT >= 3) THEN
                         KM=IM 
                         EXIT
                      ENDIF
                    ENDDO

                    IF(KM/=0)THEN
                      IF(TAGRTM(IM)==0)THEN
                        INTBUF_TAB(NI)%INFLG_SUBM(KAD(IM))=
     .                    BITSET(INTBUF_TAB(NI)%INFLG_SUBM(KAD(IM)),0)
                        INTBUF_TAB(NI)%LISUBM(KAD(IM))=KSUB
                        KAD(IM)=KAD(IM)+1
                        TAGRTM(IM)=1
                      ELSE
                        INTBUF_TAB(NI)%INFLG_SUBM(KAD(IM)-1)=
     .                    BITSET(INTBUF_TAB(NI)%INFLG_SUBM(KAD(IM)-1),0)
                      END IF
                    END IF

                  END DO
C
                ENDIF
C
           END IF
        END DO
C-------------------------------------
      RETURN
      END

