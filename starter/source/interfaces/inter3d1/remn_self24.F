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
Chd|  REMN_SELF24                   source/interfaces/inter3d1/remn_self24.F
Chd|-- called by -----------
Chd|        ININTR                        source/interfaces/interf1/inintr.F
Chd|        ININTR2                       source/interfaces/inter3d1/inintr2.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        FRETITL2                      source/starter/freform.F      
Chd|        INSOL3ET                      source/interfaces/inter3d1/i24sti3.F
Chd|        UPGRADE_REMNODE2              source/interfaces/interf1/upgrade_remnode.F
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE REMN_SELF24(
     .               X   ,IXS   ,IXS10 ,IXS16,IXS20   ,
     .               KNOD2ELS,NOD2ELS,IPARI ,INTBUF_TAB ,
     .               ITAB , NOM_OPT,NREMOV,S_NOD2ELS,IDDLEVEL)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MY_ALLOC_MOD
      USE MESSAGE_MOD
      USE INTBUFDEF_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
#include      "com04_c.inc"
#include      "scr17_c.inc"
#include      "tabsiz_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER, DIMENSION(NPARI,NINTER)    ,INTENT(INOUT) :: IPARI
      INTEGER, DIMENSION(NIXS,NUMELS)     ,INTENT(IN) :: IXS
      INTEGER, DIMENSION(6,NUMELS10)      ,INTENT(IN) :: IXS10
      INTEGER, DIMENSION(8,NUMELS16)      ,INTENT(IN) :: IXS16
      INTEGER, DIMENSION(12,NUMELS20)     ,INTENT(IN) :: IXS20
      INTEGER, DIMENSION(NUMNOD+1)        ,INTENT(IN) :: KNOD2ELS
      INTEGER,                             INTENT(IN) :: S_NOD2ELS
      INTEGER, DIMENSION(S_NOD2ELS)       ,INTENT(IN) :: NOD2ELS
      INTEGER, DIMENSION(NUMNOD)          ,INTENT(IN) :: ITAB
      INTEGER, DIMENSION(LNOPT1,SNOM_OPT) ,INTENT(IN) :: NOM_OPT
      INTEGER, DIMENSION(NINTER)      ,INTENT(INOUT) :: NREMOV
      my_real, DIMENSION(NUMNOD*3)       ,INTENT(IN) :: X
      TYPE(INTBUF_STRUCT_), DIMENSION(NINTER),INTENT(IN):: INTBUF_TAB
      INTEGER, INTENT(in) :: IDDLEVEL !< flag : 0 for the 1rst step, 1 for the 2nd step
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER N,NTY,FLAGREMNODE,I,NI
      INTEGER ILEV,II,J,NMN,NSN,NRTS,NRTM,LREMNORMAX,K,
     .        NLINS,NLINM,IWOUT,INCOM,NM,N2,IFLAG,NRE,ip,IACT,
     .        IF7,IF24,IF25,NN2,NNOD,M1,M2,M3,M4,NNREM,IBIT,NEW,
     .        KI,KL,JJ,IEDGE,NEDGE,NREMOV1(NINTER),NS,MAXNM
      INTEGER, DIMENSION(:),ALLOCATABLE :: TAGD,TAGNOD
      INTEGER ID,NC(20),NMC(4)
      CHARACTER*nchartitle,
     .   TITR


      INTEGER :: III,JJJ,NNOD_2,NOINT,E_ID,IADA
      INTEGER :: FIRST,LAST,NNREM_SAVE,FLAGREMNODE_SAV
      INTEGER :: OFFSET, NBR_INTRA,NBR_EXTRA,TOTAL_INSERTED
      INTEGER :: SIZE_INSERTED_NODE,OLDSIZE,MAX_INSERTED_NODE
      INTEGER, DIMENSION(:), ALLOCATABLE :: NBR_INSERT_II,ADRESS_II
      INTEGER, DIMENSION(:), ALLOCATABLE :: KREMNODE_SAVE,INSERTED_NODE,REMNODE,TMP
      my_real
     .   AREA
!       -------------------------------
!       FIRST : integer , first block of inserted nodes
!       LAST : integer , last block of inserted nodes
!       NNREM_SAVE : integer , internal counter
!       OFFSET : integer , internal offset for the REMNODE array
!       NBR_INTRA : integer , number of old nodes between 2 blocks
!       NBR_EXTRA : integer , number of old remaining nodes
!       TOTAL_INSERTED : integer , total number of inserted nodes
!       NBR_INSERT_II : integer, dimension = NRTM , number of inserted nodes for each II segment
!       ADRESS_II : integer, dimension = NRTM , adress of the first inserted nodes for each II segment
!       KREMNODE_SAVE : integer, dimension = NRTM+1 , list of old nodes
!       SIZE_INSERTED_NODE : integer, size of the INSERTED_NODE array ; SIZE_INSERTED_NODE is an upper bound,
!                            can be optimized!
!       INSERTED_NODE : integer, dimension = SIZE_INSERTED_NODE, list inserted nodes
!       REMNODE : integer, dimension = NRTM + TOTAL_INSERTED, new array with old and inserted nodes
!       -------------------------------
C-----------------------------------------------
C----creat list of SECONDARY nodes of self-contact to be removed per M_seg
      MAX_INSERTED_NODE = 1
      ALLOCATE(TAGNOD(NUMNOD),TAGD(NUMNOD))
      DO N=1,NINTER
        NTY=IPARI(7,N)
        NREMOV1(N)=0
        IF (NTY/=24) CYCLE
        NSN   =IPARI(5,N)
        NRTM  =IPARI(4,N)
        NOINT =IPARI(15,N)
        TAGNOD(1:NUMNOD)=0
        DO JJ=1,NSN
          NS = INTBUF_TAB(N)%NSV(JJ)
          IF (NS<=NUMNOD) TAGNOD(NS)=1
        ENDDO
C----- dimensioning
        DO II=1,NRTM
          CALL INSOL3ET(X   ,INTBUF_TAB(N)%IRECTM,IXS   ,
     .                  N   ,E_ID,II ,AREA ,
     .                  NOINT ,KNOD2ELS,NOD2ELS,IXS10 ,
     .                  IXS16,IXS20 ,NNOD)
          SELECT CASE (NNOD)
           CASE(8)
            NC(1:8)=IXS(2:9,E_ID)
           CASE(10)
            NC(1) =IXS(2,E_ID)
            NC(2) =IXS(4,E_ID)
            NC(3) =IXS(7,E_ID)
            NC(4) =IXS(6,E_ID)
            NC(5:10)=IXS10(1:6,E_ID-NUMELS8)
           CASE(20)
            NC(1:8)=IXS(2:9,E_ID)
            NC(9:20)=IXS20(1:12,E_ID-NUMELS8-NUMELS10)
           CASE(16)
            NC(1:8)=IXS(2:9,E_ID)
            NC(9:16)=IXS16(1:8,E_ID-NUMELS8-NUMELS10-NUMELS20)
          END SELECT
C
          NMC(1:4)=INTBUF_TAB(N)%IRECTM(4*(II-1)+1:4)
          MAXNM = 0
          DO I = 1,NNOD
            NI= NC(I)
            IF (NI==0) CYCLE
            IF (TAGNOD(NI)>0 .AND.NI/=NMC(1).AND.NI/=NMC(2)
     .                     .AND.NI/=NMC(3).AND.NI/=NMC(4)) THEN
              NREMOV1(N) =  NREMOV1(N)+ 1
              MAXNM = MAXNM + 1
            END IF
          END DO
          MAX_INSERTED_NODE = MAX(MAX_INSERTED_NODE,MAXNM)
        END DO
      END DO !N=1,NINTER



!   ------------------------------------------------
C---------
      DO N=1,NINTER
        IF(NREMOV1(N)==0) CYCLE
        NTY=IPARI(7,N)
        NSN   =IPARI(5,N)
        NRTM  =IPARI(4,N)

        ALLOCATE( NBR_INSERT_II(NRTM) )
        ALLOCATE( ADRESS_II(NRTM) )
        ALLOCATE( KREMNODE_SAVE(NRTM+1) )
        NBR_INSERT_II(1:NRTM) = 0
        ADRESS_II(1:NRTM) = 0
        KREMNODE_SAVE(1:NRTM+1) = 0

C----- --
        TAGNOD(1:NUMNOD)=0
        TAGD(1:NUMNOD)=2
        JJJ = 0
C--------dim first
        NNREM = 0
        DO JJ=1,NSN
          NS = INTBUF_TAB(N)%NSV(JJ)
          IF (NS<=NUMNOD) TAGD(NS)=0
          IF (NS<=NUMNOD) TAGNOD(NS)=1
        ENDDO
        IFLAG =0
        NREMOV(N) = IPARI(62,N)
        FLAGREMNODE=IPARI(63,N)
        IF(IDDLEVEL==0.AND.FLAGREMNODE==1.AND.NREMOV(N)>0) FLAGREMNODE = 2
        IADA= 1
        IF(NREMOV(N)>0) KREMNODE_SAVE(1:NRTM+1) = INTBUF_TAB(N)%KREMNODE(1:NRTM+1)

        SIZE_INSERTED_NODE = MAX_INSERTED_NODE*NRTM
        CALL MY_ALLOC(INSERTED_NODE,SIZE_INSERTED_NODE)

        DO II=1,NRTM
          NNREM_SAVE = NNREM
C
          IF (FLAGREMNODE==2)THEN
            KI = INTBUF_TAB(N)%KREMNODE(II)+1
            KL = INTBUF_TAB(N)%KREMNODE(II+1)
            DO J=KI,KL
              NS = INTBUF_TAB(N)%REMNODE(J)
              TAGD(NS)=1
            END DO
          END IF !IF(FLAGREMNODE==2)THEN
C
          CALL INSOL3ET(X   ,INTBUF_TAB(N)%IRECTM,IXS   ,
     .                  N   ,E_ID,II,AREA ,
     .                  NOINT ,KNOD2ELS,NOD2ELS,IXS10 ,
     .                  IXS16,IXS20 ,NNOD)
          SELECT CASE (NNOD)
           CASE(8)
            NC(1:8)=IXS(2:9,E_ID)
           CASE(10)
            NC(1) =IXS(2,E_ID)
            NC(2) =IXS(4,E_ID)
            NC(3) =IXS(7,E_ID)
            NC(4) =IXS(6,E_ID)
            NC(5:10)=IXS10(1:6,E_ID-NUMELS8)
           CASE(20)
            NC(1:8)=IXS(2:9,E_ID)
            NC(9:20)=IXS20(1:12,E_ID-NUMELS8-NUMELS10)
           CASE(16)
            NC(1:8)=IXS(2:9,E_ID)
            NC(9:16)=IXS16(1:8,E_ID-NUMELS8-NUMELS10-NUMELS20)
          END SELECT
C
          NMC(1:4)=INTBUF_TAB(N)%IRECTM(4*(II-1)+1:4)
          DO I = 1,NNOD
            NI= NC(I)
            IF (NI==0) CYCLE
            IF (TAGNOD(NI)>0 .AND.NI/=NMC(1).AND.NI/=NMC(2)
     .                     .AND.NI/=NMC(3).AND.NI/=NMC(4)) THEN
              IF(TAGD(NI)==0) THEN
                NNREM = NNREM + 1
                TAGD(NI)=1
                JJJ = JJJ + 1
                INSERTED_NODE(JJJ) = NI
              ENDIF
            END IF
          END DO
          !   -------------------
          !   number of inserted nodes
          NBR_INSERT_II(II) = NNREM - NNREM_SAVE
          KREMNODE_SAVE(II) = KREMNODE_SAVE(II+1) - KREMNODE_SAVE(II)
          IADA = IADA + KREMNODE_SAVE(II)
          !   adress of the first inserted node
          ADRESS_II(II) = IADA
          KREMNODE_SAVE(II) = IADA + NBR_INSERT_II(II) - 1
          IADA = IADA + NBR_INSERT_II(II)
          !   -------------------

C-----reset    TAGD=0
          DO I = 1,NNOD
            NI= NC(I)
            IF (NI==0) CYCLE
            IF (TAGNOD(NI)>0 .AND.NI/=NMC(1).AND.NI/=NMC(2)
     .                     .AND.NI/=NMC(3).AND.NI/=NMC(4)) THEN
              IF(TAGD(NI)==1) TAGD(NI)=0
            END IF
          END DO
          IF (FLAGREMNODE==2)THEN
            DO J=KI,KL
              NS = INTBUF_TAB(N)%REMNODE(J)
              TAGD(NS)=0
            END DO
          END IF 
C
        END DO !II=1,NRTM


        IF(NNREM>0) THEN

          ! get the first and the last inserted node
          FIRST = 0
          LAST = 0
          DO II = 1,NRTM
            IF(FIRST==0) THEN
              IF( NBR_INSERT_II(II)/=0 ) FIRST = II
            ENDIF
            IF(LAST==0) THEN
              IF( NBR_INSERT_II(NRTM+1-II)/=0 ) LAST = NRTM+1-II
            ENDIF
          ENDDO
          !       count the total number of inserted nodes
          TOTAL_INSERTED = 0
          DO II=1,NRTM
            TOTAL_INSERTED = TOTAL_INSERTED + NBR_INSERT_II(II)
          ENDDO
          !       allocate the buffer array
          ALLOCATE( REMNODE(NREMOV(N)+TOTAL_INSERTED) )

          J = 0
          I = 0
          OFFSET = 0
          IF( FIRST>0 ) THEN
            !   insertion of the first chunk of node : if ADRESS_II(FIRST) > 1
            !   --> need to copy the old nodes
            IF( ADRESS_II(FIRST)>1 ) THEN
              REMNODE(1:ADRESS_II(FIRST)-1) = INTBUF_TAB(N)%REMNODE(1:ADRESS_II(FIRST)-1)
              OFFSET = OFFSET + ADRESS_II(FIRST)-1
              I = I + ADRESS_II(FIRST)-1
            ENDIF

            DO II=FIRST,LAST
              !       insertion of the nodes
              IF( NBR_INSERT_II(II)>0 ) THEN
                DO JJ = 1,NBR_INSERT_II(II)
                  J = J + 1
                  REMNODE(OFFSET+NBR_INSERT_II(II)+1-JJ) = INSERTED_NODE(J)
                ENDDO
                OFFSET = OFFSET + NBR_INSERT_II(II)
              ENDIF
              IF(II<LAST.AND.NREMOV(N)>0) THEN
                ! copy of the old nodes
                NBR_INTRA = ADRESS_II(II+1) - ADRESS_II(II)-NBR_INSERT_II(II)
                IF( NBR_INTRA>0 )THEN
                  DO JJ = 1,NBR_INTRA
                    I = I + 1
                    REMNODE(JJ+OFFSET) = INTBUF_TAB(N)%REMNODE(I)
                  ENDDO
                  OFFSET = OFFSET + NBR_INTRA
                ENDIF
              ENDIF
            ENDDO
          ENDIF
          !       copy of the old nodes for the LAST chunk

          IF( I<NREMOV(N) ) THEN
            NBR_EXTRA = NREMOV(N) - I
            REMNODE(OFFSET+1:OFFSET+NBR_EXTRA) = INTBUF_TAB(N)%REMNODE(I+1:NREMOV(N))
          ENDIF
          ID=NOM_OPT(1,N)
          CALL FRETITL2(TITR,NOM_OPT(LNOPT1-LTITR+1,N),LTITR)
          ! -----------------
          ! only print the message for the 2nd sorting
          IF(IDDLEVEL>0) THEN
            CALL ANCMSG(MSGID=3014,
     .              MSGTYPE=MSGINFO,
     .              ANMODE=ANINFO_BLIND_1,
     .              I1=ID,
     .              C1=TITR,
     .              I2=NNREM)
          ENDIF
          ! -----------------
          !       update of NNREM and deallocation / allocation of the new array

          NNREM = NNREM + NREMOV(N)
C----         no need, done in UPGRADE_REMNODE2 IPARI(63,N) = 2
          CALL UPGRADE_REMNODE2(N,NNREM,INTBUF_TAB(N),NTY)
          INTBUF_TAB(N)%REMNODE(1:NNREM) = REMNODE(1:NNREM)
          INTBUF_TAB(N)%KREMNODE(2:NRTM+1) = KREMNODE_SAVE(1:NRTM)
          INTBUF_TAB(N)%KREMNODE(1)=0
C----------used for Iedge=1
          NREMOV(N) = NNREM
        END IF !IF (NNREM>0) THEN
        IF(ALLOCATED(REMNODE)) DEALLOCATE( REMNODE )
        IF(ALLOCATED(INSERTED_NODE)) DEALLOCATE( INSERTED_NODE )

C

        DEALLOCATE( NBR_INSERT_II )
        DEALLOCATE( ADRESS_II )
        DEALLOCATE( KREMNODE_SAVE )
      END DO

      DEALLOCATE(TAGD,TAGNOD)
C----
      RETURN
      END
