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
Chd|  SPMD_TRI25VOX                 source/mpi/interfaces/spmd_tri25vox.F
Chd|-- called by -----------
Chd|        I25MAIN_TRI                   source/interfaces/intsort/i25main_tri.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        ARRET                         source/system/arret.F         
Chd|        STARTIME                      source/system/timer.F         
Chd|        STOPTIME                      source/system/timer.F         
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        TRI25EBOX                     share/modules/tri25ebox.F     
Chd|        TRI7BOX                       share/modules/tri7box.F       
Chd|====================================================================
      SUBROUTINE SPMD_TRI25VOX(
     1   NSV     ,NSN      ,X      ,V     ,MS     ,
     2   BMINMAL ,WEIGHT   ,STIFN  ,NIN   ,ISENDTO,
     3   IRCVFROM,IAD_ELEM ,FR_ELEM,NSNR  ,IGAP   ,
     4   GAP_S   ,ITAB     ,KINET  ,IFQ   ,INACTI ,
     5   NSNFIOLD,INTTH    ,IELES  ,AREAS ,TEMP   ,
     6   NUM_IMP ,NODNX_SMS,GAP_S_L       ,ITYP,
     7   IRTLM   ,I24_TIME_S,I24_FRFI,I24_PENE_OLD,
     8   I24_STIF_OLD ,NBINFLG,ILEV ,I24_ICONT_I  ,
     9   INTFRIC ,IPARTFRICS,ITIED  ,IVIS2, IF_ADH,
     A   LEDGE, NEDGE, LNDEDGE, STFM, NEDGE_LOCAL,
     B   GAPE,
     C   GAP_E_L,
     D   STFE,
     E   EDG_BISECTOR,
     F   VTX_BISECTOR,
     G   ADMSR,
     H   IRECT,
     I   EBINFLG,
     J   MVOISIN, 
     K   IEDGE, 
     L   ICODT,
     M   ISKEW,IPARTFRIC_E,
     N   E2S_NOD_NORMAL)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE TRI7BOX
      USE TRI25EBOX
      USE MESSAGE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "r4r8_p.inc"
C-----------------------------------------------
C   M e s s a g e   P a s s i n g
C-----------------------------------------------
#ifdef MPI
#include "mpif.h"
#endif
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "my_allocate.inc"
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "scr02_c.inc"
#include      "scr18_c.inc"
#include      "task_c.inc"
#include      "units_c.inc"
#include      "warn_c.inc"
#include      "timeri_c.inc"
#include      "sms_c.inc"
#include      "i25edge_c.inc"
#include      "assert.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NIN, NSN, IFQ, INACTI, IGAP,INTTH,NSNR,INTFRIC,
     .        ITIED, IVIS2,
     .        NSNFIOLD(*), NSV(*), WEIGHT(*),
     .        ISENDTO(NINTER+1,*), IRCVFROM(NINTER+1,*),
     .        IAD_ELEM(2,*), FR_ELEM(*), ITAB(*), KINET(*),
     .        IELES(*),NUM_IMP, NODNX_SMS(*),IRTLM(*),ITYP,
     .        NBINFLG(*),ILEV,I24_ICONT_I(*),IPARTFRICS(*),IF_ADH(*),
     .        IPARTFRIC_E(*)
      INTEGER :: NEDGE, LNDEDGE, LEDGE(LNDEDGE,NEDGE) 
      INTEGER :: ADMSR(4,*),IRECT(4,*)
      INTEGER, INTENT(IN) :: EBINFLG(*)
      INTEGER, INTENT(IN) :: NEDGE_LOCAL
      INTEGER, INTENT(IN) :: MVOISIN(4,*)
      INTEGER, INTENT(IN) :: IEDGE
      INTEGER, INTENT(IN) :: ICODT(*)
      INTEGER, INTENT(IN) :: ISKEW(*)

C     INTEGER :: NSNFIEOLD(*)

      my_real
     .        X(3,*), V(3,*), MS(*), BMINMAL(*), STIFN(*), GAP_S(*),
     .        AREAS(*),TEMP(*),GAP_S_L(*),I24_TIME_S(*),I24_FRFI(6,*),
     .        I24_PENE_OLD(5,*),I24_STIF_OLD(2,*),STFM(*),
     .        GAPE(*),
     .        GAP_E_L(*),
     .        STFE(*)
      REAL*4  EDG_BISECTOR(3,4,*),VTX_BISECTOR(3,2,*),E2S_NOD_NORMAL(3,*)

C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
#ifdef MPI
      INTEGER MSGTYP,INFO,I,NOD, DT_CST, LOC_PROC,P,IDEB,
     .        SIZ,J, L, BUFSIZ, LEN, NB, IERROR1, IAD,
     .        STATUS(MPI_STATUS_SIZE),IERROR,REQ_SB(NSPMD),
     .        REQ_RB(NSPMD),KK,NBIRECV,IRINDEXI(NSPMD),
     .        REQ_RD(NSPMD),REQ_SD(NSPMD),REQ_SD2(NSPMD),
     .        REQ_RC(NSPMD),REQ_SC(NSPMD),
     .        INDEXI,ISINDEXI(NSPMD),INDEX(NUMNOD),                    
     .        NBOX2(2,NSPMD),NBOX(2,NSPMD),
     .        NBX,NBY,NBZ,IX,IY,IZ,
     .        MSGOFF, MSGOFF2, MSGOFF3, MSGOFF4, MSGOFF5,MSGOFF6,
     .        MSGOFF7,
     .        RSIZ, ISIZ, L2, REQ_SD3(NSPMD),REQ_RD2(NSPMD),
     .        REQ_SD4(NSPMD),REQ_RD4(NSPMD),
     .        REQ_SD5(NSPMD),REQ_RD5(NSPMD),
     .        LEN2, RSHIFT, ISHIFT, ND, JDEB, Q, NBB,
     .        NB_EDGE, IDEB_EDGE,
     .        ISIZ_EDGE

      my_real:: XMINS,YMINS,ZMINS
      my_real:: XMAXS,YMAXS,ZMAXS
      INTEGER :: N1,N2 ,NN1,NN2
      INTEGER :: IX1,IX2,IY1,IY2,IZ1,IZ2
      INTEGER :: IE,JE,I1,I2
     
      my_real :: DX,DY,DZ
      my_real :: STF

      DATA MSGOFF/6000/
      DATA MSGOFF2/6001/
      DATA MSGOFF3/6002/
      DATA MSGOFF4/6003/ 
      DATA MSGOFF5/6004/ 
      DATA MSGOFF6/6006/ 
      DATA MSGOFF7/6007/ 

      my_real
     .        BMINMA(6,NSPMD),
     .        XMAXB,YMAXB,ZMAXB,XMINB,YMINB,ZMINB
     
      TYPE(real_pointer), DIMENSION(NSPMD) :: RBUF
      TYPE(int_pointer) , DIMENSION(NSPMD) :: IBUF   
      TYPE(int_pointer) , DIMENSION(NSPMD) :: IBUF_EDGE   
      TYPE(real_pointer), DIMENSION(NSPMD) :: RBUF_EDGE   

      INTEGER, DIMENSION(:), ALLOCATABLE :: ITAGNSNFI  
      INTEGER, DIMENSION(:), ALLOCATABLE :: INDEX_EDGE 

      INTEGER :: NBIRECV_NODE,NBIRECV_EDGE
      INTEGER :: IAM,JAM,IM,M1,M2

C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
C
C================================================================
C     tag des boites contenant des facettes
C     et creation des candidats
C================================================================
      LOC_PROC = ISPMD + 1
      NBX = LRVOXEL
      NBY = LRVOXEL
      NBZ = LRVOXEL
C
C Sauvegarde valeur ancienne des nsn frontieres
C
      IF(INACTI.EQ.5.OR.INACTI.EQ.6.OR.INACTI.EQ.7.OR.IFQ.GT.0
     .   .OR.NUM_IMP.GT.0.OR.ITIED.NE.0.OR.ITYP.EQ.23.OR.ITYP.EQ.24   
     .   .OR.ITYP.EQ.25) THEN
         DO P = 1, NSPMD
           NSNFIOLD(P) = NSNFI(NIN)%P(P)
           IF(IEDGE > 0) THEN
             NSNFIEOLD(P) = NSNFIE(NIN)%P(P)
           ENDIF
         END DO
      END IF
C
Cel boite minmax pour le tri provenant de i7buce BMINMA
C
      IF(IRCVFROM(NIN,LOC_PROC).EQ.0.AND.
     .   ISENDTO(NIN,LOC_PROC).EQ.0) RETURN
      IF (IMONM > 0) CALL STARTIME(25,1)
      BMINMA(1,LOC_PROC) = BMINMAL(1)
      BMINMA(2,LOC_PROC) = BMINMAL(2)
      BMINMA(3,LOC_PROC) = BMINMAL(3)
      BMINMA(4,LOC_PROC) = BMINMAL(4)
      BMINMA(5,LOC_PROC) = BMINMAL(5)
      BMINMA(6,LOC_PROC) = BMINMAL(6)
C
Cel envoi voxel + boite min/max
C
      IF(IRCVFROM(NIN,LOC_PROC).NE.0) THEN
        DO P = 1, NSPMD
          IF(ISENDTO(NIN,P).NE.0) THEN
            IF(P.NE.LOC_PROC) THEN
              MSGTYP = MSGOFF 
              CALL MPI_ISEND(
     .          CRVOXEL25(0,0,1,LOC_PROC),
     .          2*(LRVOXEL25+1)*(LRVOXEL25+1),
     .          MPI_INTEGER,
     .          IT_SPMD(P),MSGTYP,MPI_COMM_WORLD,REQ_SC(P),IERROR)
              MSGTYP = MSGOFF2 
              CALL MPI_ISEND(
     .          BMINMA(1,LOC_PROC),6        ,REAL  ,IT_SPMD(P),MSGTYP,
     .          MPI_COMM_WORLD    ,REQ_SB(P),IERROR)
            ENDIF
          ENDIF
        ENDDO
      ENDIF
C
Cel reception voxel + boites min-max
C
      IF(ISENDTO(NIN,LOC_PROC).NE.0) THEN
        NBIRECV=0
        DO P = 1, NSPMD
          IF(IRCVFROM(NIN,P).NE.0) THEN
            IF(LOC_PROC.NE.P) THEN
              NBIRECV=NBIRECV+1
              IRINDEXI(NBIRECV)=P
              MSGTYP = MSGOFF 
              CALL MPI_IRECV(
     .          CRVOXEL25(0,0,1,P),
     .         2*(LRVOXEL+1)*(LRVOXEL+1),
     .          MPI_INTEGER,
     .          IT_SPMD(P),MSGTYP,MPI_COMM_WORLD,REQ_RC(NBIRECV),IERROR)
              MSGTYP = MSGOFF2 
              CALL MPI_IRECV(
     .          BMINMA(1,P)   ,6              ,REAL  ,IT_SPMD(P),MSGTYP,
     .          MPI_COMM_WORLD,REQ_RB(NBIRECV),IERROR)
            ENDIF
          ENDIF
        ENDDO
      ENDIF
C
Cel envoi de XREM
C
C computation of real and integer sending buffers sizes
c general case
      RSIZ = 8    
      ISIZ = 6

      IF(.TRUE.) THEN
        ISIZ = ISIZ + 2 
      ENDIF

c specific cases 
c IGAP=1 or IGAP=2
      IF(IGAP.EQ.1 .OR. IGAP.EQ.2)THEN
        RSIZ = RSIZ + 1
c IGAP=3        
      ELSEIF(IGAP.EQ.3)THEN
        RSIZ = RSIZ + 2
      ENDIF

C thermic      
      IF(INTTH > 0 ) THEN    
        RSIZ = RSIZ + 2
        ISIZ = ISIZ + 1
      ENDIF

C Interface Adhesion      
      IF(ITYP==25.AND.(IVIS2==-1.AND.INTTH == 0) ) THEN    
        RSIZ = RSIZ + 1 ! areas
        ISIZ = ISIZ + 2 ! if_adh+ioldnsnfi
      ENDIF

C Friction      
      IF(INTFRIC > 0 ) THEN    
        ISIZ = ISIZ + 1
      ENDIF

C -- IDTMINS==2      
      IF(IDTMINS == 2)THEN     
        ISIZ = ISIZ + 2
C -- IDTMINS_INT /= 0           
      ELSEIF(IDTMINS_INT/=0)THEN    
        ISIZ = ISIZ + 1
      END IF

c INT24      
      IF(ITYP==24)THEN
        RSIZ = RSIZ + 8
        ISIZ = ISIZ + 3
C-----for   NBINFLG      
        IF (ILEV==2) ISIZ = ISIZ + 1

      ENDIF    

c INT25     
      IF(ITYP==25)THEN
        RSIZ = RSIZ + 3
        ISIZ = ISIZ + 6
C-----for   NBINFLG      
        IF (ILEV==2) ISIZ = ISIZ + 1
      ENDIF    
      IDEB = 1
      REQ_SD4(1:NSPMD) = MPI_REQUEST_NULL
      REQ_SD5(1:NSPMD) = MPI_REQUEST_NULL
      REQ_RD(1:NSPMD)  = MPI_REQUEST_NULL
      REQ_RD2(1:NSPMD) = MPI_REQUEST_NULL
      REQ_RD4(1:NSPMD) = MPI_REQUEST_NULL
      REQ_RD5(1:NSPMD) = MPI_REQUEST_NULL



      JDEB = 0
      IF(ITYP.EQ.25)THEN
        ALLOCATE(ITAGNSNFI(NUMNOD),STAT=IERROR)
        ITAGNSNFI(1:NUMNOD) = 0
        ALLOCATE(INDEX_EDGE(NEDGE),STAT=IERROR)
        INDEX_EDGE(1:NEDGE) = 0 
      END IF
      
      IF(ISENDTO(NIN,LOC_PROC).NE.0) THEN
        DO KK = 1, NBIRECV
          CALL MPI_WAITANY(NBIRECV,REQ_RB,INDEXI,STATUS,IERROR)
          P=IRINDEXI(INDEXI)
          CALL MPI_WAIT(REQ_RC(INDEXI),STATUS,IERROR)
C Traitement special sur d.d. ne consever que les noeuds internes
          DO J = IAD_ELEM(1,P), IAD_ELEM(1,P+1)-1
            NOD = FR_ELEM(J)
C weight < 0 temporairement pour ne conserver que les noeuds non frontiere
            WEIGHT(NOD) = WEIGHT(NOD)*(-1)
          ENDDO
C
          L = IDEB
          NBOX(2,P) = 0
          NB = 0
          XMAXB = BMINMA(1,P)
          YMAXB = BMINMA(2,P)
          ZMAXB = BMINMA(3,P)
          XMINB = BMINMA(4,P)
          YMINB = BMINMA(5,P)
          ZMINB = BMINMA(6,P)
C ==================== Secnd nodes ============================= 
          DO I=1,NSN
            NOD = NSV(I)
            IF(WEIGHT(NOD).EQ.1)THEN
             IF(STIFN(I).GT.ZERO)THEN
               IF(ITYP==25.AND.IRTLM(4*(I-1)+4)==P)THEN
                 NB = NB + 1
                 INDEX(NB) = I
               ELSEIF(ITIED/=0.AND.ITYP==7.AND.CANDF_SI(NIN)%P(I)/=0) THEN
                 NB = NB + 1
                 INDEX(NB) = I
               ELSE
                 IF(X(1,NOD) < XMINB)  CYCLE
                 IF(X(1,NOD) > XMAXB)  CYCLE
                 IF(X(2,NOD) < YMINB)  CYCLE
                 IF(X(2,NOD) > YMAXB)  CYCLE
                 IF(X(3,NOD) < ZMINB)  CYCLE
                 IF(X(3,NOD) > ZMAXB)  CYCLE
                 IX=INT(NBX*(X(1,NOD)-XMINB)/(XMAXB-XMINB))
                 IF(IX >= 0 .AND. IX <= NBX) THEN
                   IY=INT(NBY*(X(2,NOD)-YMINB)/(YMAXB-YMINB))
                   IF(IY >= 0 .AND. IY <= NBY) THEN
                     IZ=INT(NBZ*(X(3,NOD)-ZMINB)/(ZMAXB-ZMINB))
                     IF(IZ >= 0 .AND. IZ <= NBZ) THEN
                       IF(BTEST(CRVOXEL25(IY,IZ,1,P),IX)) THEN
                         NB = NB + 1
                         INDEX(NB) = I
                       ENDIF
                     ENDIF
                   ENDIF
                 ENDIF
               ENDIF
             ENDIF
            ENDIF
          ENDDO
          NBOX(1,P) = NB
          DO J = IAD_ELEM(1,P), IAD_ELEM(1,P+1)-1
            NOD = FR_ELEM(J)
C remise de weight > 0
            WEIGHT(NOD) = WEIGHT(NOD)*(-1)
          ENDDO

C ==================== Secnd edges ============================= 
          DX=XMAXB-XMINB
          DY=YMAXB-YMINB
          DZ=ZMAXB-ZMINB
          NB_EDGE = 0
! loop over edge that are local to ISPMD (ISPMD is main of the edge,
! even if it is a boundary edge
          IF(IEDGE /= 0) THEN
          DO I=1,NEDGE_LOCAL
           ASSERT(LEDGE(9,I) == 1) 
           N1=LEDGE(5,I)
           N2=LEDGE(6,I)
           ASSERT(N1 > 0) 
           ASSERT(N2 > 0)
           ASSERT(N1 <= NUMNOD)
           ASSERT(N2 <= NUMNOD)
 
           IF(LEDGE(1,I) > 0) THEN
C            First segment is local
             STF = STFM(LEDGE(1,I))
           ELSEIF (LEDGE(3,I) > 0) THEN
C            First segment is on the other side of the boundary
             STF = UN
             IF(MVOISIN(LEDGE(4,I),LEDGE(3,I)) == 0) STF = 0
           ELSE ! 
             
             ! ISPMD owns a boundary edge, but the local segment is deleted
             STF = UN
           ENDIF
           DEBUG_E2E(LEDGE(8,I) == D_ES,P-1)
           DEBUG_E2E(LEDGE(8,I) == D_ES,STF)
           DEBUG_E2E(LEDGE(8,I) == D_ES,LEDGE(7,I))


           IF( STF > ZERO .AND. LEDGE(7,I) >= 0)  THEN
C
C            GAPE inutile ici (Redondant avec BGAPEMAX cote main) !
             XMINS = MIN(X(1,N1),X(1,N2))!- GAPE(I)      
             YMINS = MIN(X(2,N1),X(2,N2))!- GAPE(I)      
             ZMINS = MIN(X(3,N1),X(3,N2))!- GAPE(I)      
             XMAXS = MAX(X(1,N1),X(1,N2))!+ GAPE(I)      
             YMAXS = MAX(X(2,N1),X(2,N2))!+ GAPE(I)      
             ZMAXS = MAX(X(3,N1),X(3,N2))!+ GAPE(I)      

             DEBUG_E2E(LEDGE(8,I) == D_ES, XMINS)
             DEBUG_E2E(LEDGE(8,I) == D_ES, YMINS)
             DEBUG_E2E(LEDGE(8,I) == D_ES, ZMINS)
             DEBUG_E2E(LEDGE(8,I) == D_ES, XMAXS)
             DEBUG_E2E(LEDGE(8,I) == D_ES, YMAXS)
             DEBUG_E2E(LEDGE(8,I) == D_ES, ZMAXS)

             IX1=INT(NBX*(XMINS-XMINB)/DX)
             IX2=INT(NBX*(XMAXS-XMINB)/DX)       

             IF(IX2>=0.AND.IX1<=NBX)THEN
              IY1=INT(NBY*(YMINS-YMINB)/DY)
              IY2=INT(NBY*(YMAXS-YMINB)/DY) 

               IF(IY2>=0.AND.IY1<=NBY)THEN
               IZ1=INT(NBZ*(ZMINS-ZMINB)/DZ)
               IZ2=INT(NBZ*(ZMAXS-ZMINB)/DZ)

                IF(IZ2>=0.AND.IZ1<=NBZ)THEN
                IX1=MAX(IX1,0)
                IX2=MIN(IX2,NBX)
                IY1=MAX(IY1,0)
                IY2=MIN(IY2,NBX)
                IZ1=MAX(IZ1,0)
                IZ2=MIN(IZ2,NBX) 
                DO IX=IX1,IX2
                 DO IY=IY1,IY2
                  DO IZ=IZ1,IZ2
                   IF(BTEST(CRVOXEL25(IY,IZ,1,P),IX)) THEN
                    NB_EDGE = NB_EDGE + 1
                    INDEX_EDGE(NB_EDGE) = I
                    DEBUG_E2E(LEDGE(8,I)==D_ES,NB_EDGE)
                    GOTO 111 !next I
                   END IF
                  END DO
                 END DO
                END DO
               ENDIF
              ENDIF
             ENDIF
  111      CONTINUE       
           ENDIF !
          ENDDO
          ENDIF ! IEDGE

          NBOX(2,P) = NB_EDGE
C         WRITE(6,*) ISPMD,"sends ",NB_EDGE,"to,",P-1
          IF(ITYP.EQ.25)THEN
            JDEB = 0
            DO Q=1,P-1
              JDEB = JDEB + NSNSI(NIN)%P(Q)
            END DO
            NBB = NSNSI(NIN)%P(P)
            DO J = 1, NBB
              ND = NSVSI(NIN)%P(JDEB+J)
              NOD= NSV(ND)
              ITAGNSNFI(NOD)=J
            END DO
          END IF
C
C Envoi taille msg
C
          MSGTYP = MSGOFF3 
          CALL MPI_ISEND(NBOX(1,P),2,MPI_INTEGER,IT_SPMD(P),MSGTYP,
     .                 MPI_COMM_WORLD,REQ_SD(P),IERROR)
C
C Alloc buffer
C
          IF( NB_EDGE > 0) THEN
            MY_ALLOCATE(IBUF_EDGE(P)%P,E_IBUF_SIZE*NB_EDGE)
            MY_ALLOCATE(RBUF_EDGE(P)%P,E_RBUF_SIZE*NB_EDGE)

            L = 0
            DO J=1,NB_EDGE
               I = INDEX_EDGE(J)
               ASSERT(I > 0) 
               ASSERT(I <= NEDGE)
               IBUF_EDGE(P)%p(E_GLOBAL_ID    + L) = LEDGE(8,I)
               IBUF_EDGE(P)%p(E_LEFT_SEG     + L) = LEDGE(1,I)
               IBUF_EDGE(P)%p(E_LEFT_ID      + L) = LEDGE(2,I)
               IBUF_EDGE(P)%p(E_RIGHT_SEG    + L) = LEDGE(3,I)
               IBUF_EDGE(P)%p(E_RIGHT_ID     + L) = LEDGE(4,I)
               IBUF_EDGE(P)%p(E_NODE1_ID     + L) = LEDGE(5,I)
               IBUF_EDGE(P)%p(E_NODE2_ID     + L) = LEDGE(6,I)
               IBUF_EDGE(P)%p(E_TYPE         + L) = LEDGE(7,I) 
! It is possible that one of the node is not sent 
               IBUF_EDGE(P)%p(E_NODE1_GLOBID + L) = ITAB((LEDGE(5,I))) 
               IBUF_EDGE(P)%p(E_NODE2_GLOBID + L) = ITAB((LEDGE(6,I))) 
               IBUF_EDGE(P)%p(E_LOCAL_ID     + L) = I 
               IF(ILEV == 2) THEN
                 IBUF_EDGE(P)%p(E_EBINFLG      + L) = EBINFLG(I)
               ELSE
                 IBUF_EDGE(P)%p(E_EBINFLG      + L) = 0
               ENDIF
               IAM= LEDGE(1,I)
               JAM= LEDGE(2,I)
               M1 = LEDGE(5,I)
               M2 = LEDGE(6,I)
               IM = LEDGE(10,I)
               IBUF_EDGE(P)%p(E_IM     + L) = IM 
               IF(IDTMINS /= 0) THEN
                 IF(IDTMINS/=2 .AND. IDTMINS_INT == 0) THEN
                 ELSEIF(IDTMINS==2) THEN
                   IBUF_EDGE(P)%p(E_NODNX1  + L) = NODNX_SMS(M1)
                   IBUF_EDGE(P)%p(E_NODAMS1 + L) = M1 
                   IBUF_EDGE(P)%p(E_NODNX2  + L) = NODNX_SMS(M2)
                   IBUF_EDGE(P)%p(E_NODAMS2 + L) = M2 
                 ELSE ! IDTMINS_INT == 0
                   IBUF_EDGE(P)%p(E_NODNX1  + L) = 0
                   IBUF_EDGE(P)%p(E_NODAMS1 + L) = M1 
                   IBUF_EDGE(P)%p(E_NODNX2  + L) = 0
                   IBUF_EDGE(P)%p(E_NODAMS2 + L) = M2 
                 ENDIF
                   ASSERT(NODNX_SMS(M1) >=0)
                   ASSERT(NODNX_SMS(M2) >=0)
                   DEBUG_E2E(NODNX_SMS(M1) < 0,NODNX_SMS(M1))
                   DEBUG_E2E(NODNX_SMS(M2) < 0,NODNX_SMS(M2))
               ENDIF ! IDTMINS /= 0
               IF(INTFRIC > 0) THEN
                  IBUF_EDGE(P)%p(E_IPARTFRIC_E  + L) = IPARTFRIC_E(I)
               ELSE
                  IBUF_EDGE(P)%p(E_IPARTFRIC_E  + L) = 0
               ENDIF
               L = L + E_IBUF_SIZE
            ENDDO

            L = 0
            DO J=1,NB_EDGE
               I = INDEX_EDGE(J)
               RBUF_EDGE(P)%p(E_X1+ L) =  X(1,(LEDGE(5,I))) 
               RBUF_EDGE(P)%p(E_Y1+ L) =  X(2,(LEDGE(5,I))) 
               RBUF_EDGE(P)%p(E_Z1+ L) =  X(3,(LEDGE(5,I))) 
               RBUF_EDGE(P)%p(E_X2+ L) =  X(1,(LEDGE(6,I))) 
               RBUF_EDGE(P)%p(E_Y2+ L) =  X(2,(LEDGE(6,I))) 
               RBUF_EDGE(P)%p(E_Z2+ L) =  X(3,(LEDGE(6,I))) 
               RBUF_EDGE(P)%p(E_VX1+ L) = V(1,(LEDGE(5,I))) 
               RBUF_EDGE(P)%p(E_VY1+ L) = V(2,(LEDGE(5,I))) 
               RBUF_EDGE(P)%p(E_VZ1+ L) = V(3,(LEDGE(5,I))) 
               RBUF_EDGE(P)%p(E_VX2+ L) = V(1,(LEDGE(6,I))) 
               RBUF_EDGE(P)%p(E_VY2+ L) = V(2,(LEDGE(6,I))) 
               RBUF_EDGE(P)%p(E_VZ2+ L) = V(3,(LEDGE(6,I))) 
               RBUF_EDGE(P)%p(E_MS1+ L) = MS((LEDGE(5,I))) 
               RBUF_EDGE(P)%p(E_MS2+ L) = MS((LEDGE(6,I))) 
               RBUF_EDGE(P)%p(E_GAP+ L) = GAPE(I) 
               IF(IGAP == 3) THEN 
                 RBUF_EDGE(P)%p(E_GAPL+ L) = GAP_E_L(I) 
               ELSE
                 RBUF_EDGE(P)%p(E_GAPL+ L) = 0 
               ENDIF
               ASSERT(NOT(ISNAN( RBUF_EDGE(P)%p(E_GAPL+ L))))

C              RBUF_EDGE(P)%p(E_STIFE+ L) = STFM(LEDGE(1,I)) 
               RBUF_EDGE(P)%p(E_STIFE+ L) = STFE(I) 
               ASSERT(NOT(isnan(STFE(I))))

C TO DO: single precision
               L2 = E_EDG_BIS + L 

               IE  = ABS(LEDGE(1,I))
               JE  = LEDGE(2,I)
               IAM = LEDGE(1,I)
               JAM = LEDGE(2,I)
               M1  = LEDGE(5,I)
               M2  = LEDGE(6,I)
               IM  = LEDGE(10,I)
               I1  = LEDGE(11,I)
               I2  = LEDGE(12,I)
               NN1 = ADMSR(JE,IE)
               NN2 = ADMSR(MOD(JE,4)+1,IE)


               RBUF_EDGE(P)%p(L2:L2+2) = EDG_BISECTOR(1:3,JE,IE)

               L2 = E_VTX_BIS + L
               RBUF_EDGE(P)%p(L2:L2+2) = VTX_BISECTOR(1:3,1,I1)

               L2 = L2 + 3
               RBUF_EDGE(P)%p(L2:L2+2) = VTX_BISECTOR(1:3,2,I1)

               L2 = L2 + 3
               RBUF_EDGE(P)%p(L2:L2+2) = VTX_BISECTOR(1:3,1,I2)

               L2 = L2 + 3
               RBUF_EDGE(P)%p(L2:L2+2) = VTX_BISECTOR(1:3,2,I2)

               L2 = L2 + 3
               RBUF_EDGE(P)%p(L2:L2+2) = E2S_NOD_NORMAL(1:3,NN1)

               L2 = L2 + 3
               RBUF_EDGE(P)%p(L2:L2+2) = E2S_NOD_NORMAL(1:3,NN2)

               L = L + E_RBUF_SIZE
            ENDDO
            
c           DO J = 1, L-1
c             IF(ISNAN(RBUF_EDGE(P)%p(J))) THEN
c               WRITE(6,*) ISPMD,"NaN found",J,"/",L
c             ENDIF
C           ENDDO
          ENDIF

          IF (NB > 0) THEN
            ALLOCATE(RBUF(P)%P(RSIZ*NB),STAT=IERROR)
            ALLOCATE(IBUF(P)%P(ISIZ*NB),STAT=IERROR)
            L = 0
            L2= 0           
              
#include      "vectorize.inc"
            DO J = 1, NB
               I = INDEX(J)
               NOD = NSV(I)
               RBUF(P)%p(L+1) = X(1,NOD)
               RBUF(P)%p(L+2) = X(2,NOD)
               RBUF(P)%p(L+3) = X(3,NOD)
               RBUF(P)%p(L+4) = V(1,NOD)
               RBUF(P)%p(L+5) = V(2,NOD)
               RBUF(P)%p(L+6) = V(3,NOD)
               RBUF(P)%p(L+7) = MS(NOD)
               RBUF(P)%p(L+8) = STIFN(I)          
               IBUF(P)%p(L2+1) = I
               IBUF(P)%p(L2+2) = ITAB(NOD)      
               IBUF(P)%p(L2+3) = KINET(NOD)
!     save specifics IREM and XREM indexes for INT24 sorting
               IBUF(P)%p(L2+4) = 0 !IGAPXREMP
               IBUF(P)%p(L2+5) = 0 !I24XREMP    
               IBUF(P)%p(L2+6) = 0 !I24IREMP
               L = L + RSIZ
               L2 = L2 + ISIZ
            END DO

c shift for real variables (prepare for next setting)       
            RSHIFT = 9
c shift for integer variables (prepare for next setting) 
            ISHIFT = 7 

C  symmetric plane
          IF(.TRUE. )THEN
            L = 0        
#include    "vectorize.inc"
            DO J = 1, NB
              I = INDEX(J)
              NOD = NSV(I)   
              IBUF(P)%p(L+ISHIFT+0)= ICODT(NOD) 
              IBUF(P)%p(L+ISHIFT+1)= ISKEW(NOD) 
              L = L + ISIZ    
            ENDDO
            ISHIFT = ISHIFT + 2
          ENDIF





c specific cases
c IGAP=1 or IGAP=2               
            IF(IGAP.EQ.1 .OR. IGAP.EQ.2)THEN
               L = 0        
               IGAPXREMP = RSHIFT       
#include      "vectorize.inc"          
               DO J = 1, NB
                 I = INDEX(J)   
                 RBUF(P)%p(L+RSHIFT)= GAP_S(I)
                 L = L + RSIZ    
               ENDDO
               RSHIFT = RSHIFT + 1    
                  
c IGAP=3                       
            ELSEIF(IGAP.EQ.3)THEN 
               L = 0     
               IGAPXREMP = RSHIFT       
#include      "vectorize.inc"            
               DO J = 1, NB
                 I = INDEX(J)
                 RBUF(P)%p(L+RSHIFT)  = GAP_S(I)
                 RBUF(P)%p(L+RSHIFT+1)= GAP_S_L(I)
                 L = L + RSIZ
               END DO
               RSHIFT = RSHIFT + 2
            ENDIF
                     
C thermic
            IF(INTTH.GT.0)THEN
               L = 0
               L2 = 0           
#include      "vectorize.inc"                       
               DO J = 1, NB
                 I = INDEX(J)
                 NOD = NSV(I)
                 RBUF(P)%p(L+RSHIFT)   = TEMP(NOD)
                 RBUF(P)%p(L+RSHIFT+1) = AREAS(I)
                 IBUF(P)%p(L2+ISHIFT) = IELES(I)
                 L = L + RSIZ
                 L2 = L2 + ISIZ
               END DO
               RSHIFT = RSHIFT + 2
               ISHIFT = ISHIFT + 1             
            ENDIF

C Interface Adhesion
            IF(ITYP==25.AND.IVIS2==-1.AND.INTTH==0)THEN                                                  
              L = 0
              L2 = 0
#include      "vectorize.inc"                       
              DO J = 1, NB
                I = INDEX(J)
                NOD = NSV(I)
                RBUF(P)%p(L+RSHIFT) = AREAS(I)
                IBUF(p)%p(L2+ISHIFT) = IF_ADH(I)
                IBUF(P)%p(L2+ISHIFT+1)=ITAGNSNFI(NOD)
                L = L + RSIZ
                L2 = L2 + ISIZ
              END DO
              RSHIFT = RSHIFT + 1
              ISHIFT = ISHIFT + 2 
            ENDIF 

C Friction
            IF(INTFRIC.GT.0)THEN
               L2 = 0           
#include      "vectorize.inc"                       
               DO J = 1, NB
                 I = INDEX(J)
                 IBUF(P)%p(L2+ISHIFT) = IPARTFRICS(I)
                 L2 = L2 + ISIZ
               END DO
               ISHIFT = ISHIFT + 1             
            ENDIF
               
C -- IDTMINS==2
            IF(IDTMINS==2)THEN
               L2 = 0
#include      "vectorize.inc"                  
               DO J = 1, NB
                 I = INDEX(J)
                 NOD = NSV(I)
                 IBUF(P)%p(L2+ISHIFT)  = NODNX_SMS(NOD)
                 IBUF(P)%p(L2+ISHIFT+1)= NOD
                 L2 = L2 + ISIZ
               END DO
               ISHIFT = ISHIFT + 2
               
C -- IDTMINS_INT /= 0          
            ELSEIF(IDTMINS_INT.NE.0)THEN
              L2 = 0             
#include      "vectorize.inc"         
              DO J = 1, NB
                I = INDEX(J)
                NOD = NSV(I)
                IBUF(P)%p(L2+ISHIFT)= NOD
                L2 = L2 + ISIZ
              END DO
              ISHIFT = ISHIFT + 1             
            ENDIF
             
c INT24
            IF(ITYP.EQ.24)THEN

              L = 0
              I24XREMP = RSHIFT
#include      "vectorize.inc"
              DO J = 1, NB
                I = INDEX(J)
                RBUF(P)%p(L+RSHIFT)    =I24_TIME_S(I)
                RBUF(P)%p(L+RSHIFT+1)  =I24_FRFI(1,I)
                RBUF(P)%p(L+RSHIFT+2)  =I24_FRFI(2,I)
                RBUF(P)%p(L+RSHIFT+3)  =I24_FRFI(3,I)
                RBUF(P)%p(L+RSHIFT+4)  =I24_PENE_OLD(1,I)
                RBUF(P)%p(L+RSHIFT+5)  =I24_STIF_OLD(1,I) 
                RBUF(P)%p(L+RSHIFT+6)  =I24_PENE_OLD(3,I)
                RBUF(P)%p(L+RSHIFT+7)  =I24_PENE_OLD(5,I)
                L = L + RSIZ
              END DO          
              RSHIFT = RSHIFT + 8  
                
              L2 = 0      
              I24IREMP = ISHIFT 
#include      "vectorize.inc"
              DO J = 1, NB
                I = INDEX(J)
C               IRTLM(2,NSN) in TYPE24
                IBUF(P)%p(L2+ISHIFT)  =IRTLM(2*(I-1)+1)
                IBUF(P)%p(L2+ISHIFT+1)=IRTLM(2*(I-1)+2)
                IBUF(P)%p(L2+ISHIFT+2)=I24_ICONT_I(I)
                L2 = L2 + ISIZ
              END DO          
              ISHIFT = ISHIFT + 3
C---pay attention in i24sto.F IREM(I24IREMP+3,N-NSN) is used, 
C----change the shift value when new tabel was added like I24_ICONT_I           
              IF (ILEV==2) THEN  
                L2 = 0                
#include      "vectorize.inc"       
                DO J = 1, NB
                  I = INDEX(J)
                  IBUF(P)%p(L2+ISHIFT)=NBINFLG(I)
                  L2 = L2 + ISIZ
                END DO        
              END IF
              ISHIFT = ISHIFT + 1

            END IF !(ITYP.EQ.24)                
             
c INT25
            IF(ITYP.EQ.25)THEN
              L = 0
              I24XREMP = RSHIFT
#include      "vectorize.inc"
              DO J = 1, NB
                I = INDEX(J)
                RBUF(P)%p(L+RSHIFT)    =I24_TIME_S(2*(I-1)+1)
                RBUF(P)%p(L+RSHIFT+1)  =I24_TIME_S(2*(I-1)+2)
                RBUF(P)%p(L+RSHIFT+2)  =I24_PENE_OLD(5,I) !  used only at time=0
                L = L + RSIZ
              END DO          
              RSHIFT = RSHIFT + 3 
                
              L2 = 0      
              I24IREMP = ISHIFT 

#include      "vectorize.inc"
              DO J = 1, NB
                I = INDEX(J)
                NOD = NSV(I)
C               IRTLM(3,NSN) en TYPE25 / IRTLM(3,-) inutile ici 
                IBUF(P)%p(L2+ISHIFT)  =IRTLM(4*(I-1)+1)
                IBUF(P)%p(L2+ISHIFT+1)=IRTLM(4*(I-1)+2)
C
C               IRTLM(3,I) == local n    of the impacted segment is shared but only valid on proc == IRTLM(4,I)
                IBUF(P)%p(L2+ISHIFT+2)=IRTLM(4*(I-1)+3)
                IBUF(P)%p(L2+ISHIFT+3)=IRTLM(4*(I-1)+4)
                IBUF(P)%p(L2+ISHIFT+4)=I24_ICONT_I(I)
                IBUF(P)%p(L2+ISHIFT+5)=ITAGNSNFI(NOD)
                L2 = L2 + ISIZ
              END DO          
              ISHIFT = ISHIFT + 6
C---pay attention in i25sto.F IREM(I24IREMP+4,N-NSN) is used, 
C----change the shift value when new tabel was added like IRTLM(3*(I-1)+2)              
              IF (ILEV==2) THEN  
                L2 = 0                
#include      "vectorize.inc"       
                DO J = 1, NB
                  I = INDEX(J)
                  IBUF(P)%p(L2+ISHIFT)=NBINFLG(I)
                  L2 = L2 + ISIZ
                END DO        
              END IF
              ISHIFT = ISHIFT + 1

            END IF !(ITYP.EQ.25)                
C
            !save specifics IREM and XREM indexes for INT24 sorting
            L2 = 0
#include      "vectorize.inc"
            DO J = 1, NB
              I = INDEX(J)
              NOD = NSV(I)
              !save specifics IREM and XREM indexes for INT24 sorting
              IBUF(P)%p(L2+4) = IGAPXREMP
              IBUF(P)%p(L2+5) = I24XREMP
              IBUF(P)%p(L2+6) = I24IREMP
              L2 = L2 + ISIZ
            END DO
          ENDIF ! NB > 0

            IF( NB > 0 ) THEN
C             WRITE(6,*) "Sends",NB,"nodes to",P-1
              MSGTYP = MSGOFF4
              CALL MPI_ISEND(
     1          RBUF(P)%P(1),NB*RSIZ,REAL,IT_SPMD(P),MSGTYP,
     2          MPI_COMM_WORLD,REQ_SD2(P),ierror)
              
              MSGTYP = MSGOFF5
              CALL MPI_ISEND(
     1          IBUF(P)%P(1),NB*ISIZ,MPI_INTEGER,IT_SPMD(P),MSGTYP,
     2          MPI_COMM_WORLD,REQ_SD3(P),ierror)
            ENDIF
            IF(NB_EDGE > 0) THEN

              MSGTYP = MSGOFF6
              CALL MPI_ISEND(
     1          IBUF_EDGE(P)%P(1),E_IBUF_SIZE*NB_EDGE ,MPI_INTEGER,IT_SPMD(P),MSGTYP,
     2          MPI_COMM_WORLD,REQ_SD4(P),ierror)
              
              MSGTYP = MSGOFF7
              CALL MPI_ISEND(
     1          RBUF_EDGE(P)%P(1),E_RBUF_SIZE*NB_EDGE ,REAL,IT_SPMD(P),MSGTYP,
     2          MPI_COMM_WORLD,REQ_SD5(P),ierror)
            ENDIF  ! NB_EDGE > 0
c         ENDIF
C
C reset old tag for next P
          IF(ITYP.EQ.25)THEN
C reset 
            NBB = NSNSI(NIN)%P(P)
            DO J = 1, NBB
              ND = NSVSI(NIN)%P(JDEB+J)
              NOD= NSV(ND)
              ITAGNSNFI(NOD)=0
            END DO
          END IF
        ENDDO
      ENDIF       
C
      IF(ITYP.EQ.25) THEN
        DEALLOCATE(ITAGNSNFI)
        DEALLOCATE(INDEX_EDGE) 
      ENDIF
C
Cel reception  des donnees XREM
C
      NEDGE_REMOTE = 0
      IF(IRCVFROM(NIN,LOC_PROC).NE.0) THEN
C       WRITE(6,*) __FILE__,__LINE__,ISPMD
        NSNR = 0
        L=0
        DO P = 1, NSPMD
          NSNFI(NIN)%P(P) = 0
          IF(IEDGE /= 0) NSNFIE(NIN)%P(P) = 0
          IF(ISENDTO(NIN,P).NE.0) THEN
C           WRITE(6,*) __FILE__,__LINE__,ISPMD
            IF(LOC_PROC.NE.P) THEN
              MSGTYP = MSGOFF3 
              CALL MPI_RECV(NBOX2(1,P),2,MPI_INTEGER,IT_SPMD(P),
     .                      MSGTYP,MPI_COMM_WORLD,STATUS,IERROR)
              NSNFI(NIN)%P(P) = NBOX2(1,P)

              IF(IEDGE /= 0) THEN
                NEDGE_REMOTE = NEDGE_REMOTE + NBOX2(2,P)
                EDGE_FI(NIN)%P(P) = NBOX2(2,P)
                NSNFIE(NIN)%P(P) = NBOX2(2,P)
              ELSE
C               EDGE_FI(NIN)%P(P) = 0 
C               NSNFIE(NIN)%P(P) = 0
              ENDIF

              IF(NSNFI(NIN)%P(P)> 0 .OR. NBOX2(2,P) > 0) THEN
                L=L+1
                ISINDEXI(L)=P
                NSNR = NSNR + NSNFI(NIN)%P(P)
              ENDIF
            ENDIF
          ENDIF
        ENDDO
        NBIRECV=L
C
C Allocate total size
C

       IF(NSNR > 0 .OR. NEDGE_REMOTE > 0 ) THEN
          ALLOCATE(XREM(RSIZ,NSNR),STAT=IERROR)
          IF(IERROR.NE.0) THEN
            CALL ANCMSG(MSGID=20,ANMODE=ANINFO)
            CALL ARRET(2)
          ENDIF
          ALLOCATE(IREM(ISIZ,NSNR),STAT=IERROR)   
          IF(IERROR.NE.0) THEN
            CALL ANCMSG(MSGID=20,ANMODE=ANINFO)
            CALL ARRET(2)
          ENDIF
          IF(IEDGE /= 0) THEN
            ALLOCATE(IREM_EDGE(E_IBUF_SIZE,NEDGE_REMOTE),STAT=IERROR)   
            IF(IERROR.NE.0) THEN
              CALL ANCMSG(MSGID=20,ANMODE=ANINFO)
              CALL ARRET(2)
            ENDIF
            ALLOCATE(XREM_EDGE(E_RBUF_SIZE,NEDGE_REMOTE),STAT=IERROR)   
            IF(IERROR.NE.0) THEN
              CALL ANCMSG(MSGID=20,ANMODE=ANINFO)
              CALL ARRET(2)
            ENDIF
          ENDIF
          IDEB = 1
          IDEB_EDGE = 1
          NBIRECV_EDGE = 0
          NBIRECV_NODE = 0
          DO L = 1, NBIRECV
            P = ISINDEXI(L)
            IF(NSNFI(NIN)%P(P) > 0 ) THEN
              LEN = NSNFI(NIN)%P(P)*RSIZ
              MSGTYP = MSGOFF4 
              NBIRECV_NODE = NBIRECV_NODE + 1
              CALL MPI_IRECV(
     1          XREM(1,IDEB),LEN,REAL,IT_SPMD(P),
     2          MSGTYP,MPI_COMM_WORLD,REQ_RD(NBIRECV_NODE),IERROR)
              
              LEN2 = NSNFI(NIN)%P(P)*ISIZ
              MSGTYP = MSGOFF5 
              CALL MPI_IRECV(
     1          IREM(1,IDEB),LEN2,MPI_INTEGER,IT_SPMD(P),
     2          MSGTYP,MPI_COMM_WORLD,REQ_RD2(NBIRECV_NODE),IERROR)
              IDEB = IDEB + NSNFI(NIN)%P(P)                   
            ENDIF

            IF(IEDGE /= 0) THEN
            IF(EDGE_FI(NIN)%P(P) > 0 ) THEN
              MSGTYP = MSGOFF6 
              LEN2 = EDGE_FI(NIN)%P(P)*E_IBUF_SIZE
              NBIRECV_EDGE = NBIRECV_EDGE + 1

              CALL MPI_IRECV(
     1          IREM_EDGE(1,IDEB_EDGE),LEN2,MPI_INTEGER,IT_SPMD(P),
     2          MSGTYP,MPI_COMM_WORLD,REQ_RD4(NBIRECV_EDGE),IERROR)
              
              MSGTYP = MSGOFF7 
              LEN2 = EDGE_FI(NIN)%P(P)*E_RBUF_SIZE
              CALL MPI_IRECV(
     1          XREM_EDGE(1,IDEB_EDGE),LEN2,REAL,IT_SPMD(P),
     2          MSGTYP,MPI_COMM_WORLD,REQ_RD5(NBIRECV_EDGE),IERROR)
                IDEB_EDGE = IDEB_EDGE + EDGE_FI(NIN)%P(p)
             ENDIF
             ENDIF
          ENDDO


          
          CALL MPI_WAITALL(NBIRECV_NODE,REQ_RD ,MPI_STATUSES_IGNORE,IERROR)
          CALL MPI_WAITALL(NBIRECV_NODE,REQ_RD2,MPI_STATUSES_IGNORE,IERROR)
          CALL MPI_WAITALL(NBIRECV_EDGE,REQ_RD4,MPI_STATUSES_IGNORE,IERROR)
          CALL MPI_WAITALL(NBIRECV_EDGE,REQ_RD5,MPI_STATUSES_IGNORE,IERROR)

          !set specifics IREM and XREM indexes for INT24 sorting
          IGAPXREMP = IREM(4,1)
          I24XREMP  = IREM(5,1)
          I24IREMP  = IREM(6,1)
        ENDIF
      ENDIF
C
      IF(IRCVFROM(NIN,LOC_PROC).NE.0) THEN
        DO P = 1, NSPMD
          IF(ISENDTO(NIN,P).NE.0) THEN
            IF(P.NE.LOC_PROC) THEN
              CALL MPI_WAIT(REQ_SB(P),STATUS,IERROR)
              CALL MPI_WAIT(REQ_SC(P),STATUS,IERROR)
            ENDIF
          ENDIF
        ENDDO
      ENDIF
C
      IF(ISENDTO(NIN,LOC_PROC).NE.0) THEN
        DO P = 1, NSPMD
          IF(IRCVFROM(NIN,P).NE.0) THEN
            IF(P.NE.LOC_PROC) THEN
              CALL MPI_WAIT(REQ_SD(P),STATUS,IERROR)
              IF(NBOX(1,P) > 0) THEN
                CALL MPI_WAIT(REQ_SD2(P),STATUS,IERROR)
                DEALLOCATE(RBUF(P)%p)
                CALL MPI_WAIT(REQ_SD3(P),STATUS,IERROR)
                DEALLOCATE(IBUF(P)%p)                   
              ENDIF
              IF(NBOX(2,P) > 0) THEN
                CALL MPI_WAIT(REQ_SD4(P),STATUS,IERROR)
                DEALLOCATE(IBUF_EDGE(P)%p)                   
                CALL MPI_WAIT(REQ_SD5(P),STATUS,IERROR)
                DEALLOCATE(RBUF_EDGE(P)%p)                   
              END IF
            ENDIF
          ENDIF
        ENDDO
      ENDIF
C
      IF (IMONM > 0) CALL STOPTIME(25,1)
C
#endif
      RETURN
      END

