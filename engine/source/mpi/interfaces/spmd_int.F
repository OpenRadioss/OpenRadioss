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
C Message TAGS Rules
C TAGS are represented by variable integer MSGOFF
C MSGOFF is a 4 digit integer of the form
C DATA MSGOFF/ABCD/ 
C MSGTYP = MSGOFF

C TAG ID series for different message files:
C    1  ->  999  => spmd_mach.F 
C 1000  -> 1999  => send_cand.F
C 2000  -> 2999  => spmd_sph.F
C 3000  -> 3999  => spmd_cfd.F
C 4000  -> 4999  => spmd_section.F
C 5000  -> 5999  => spmd_r2r.F
C 6000  -> 6999  => spmd_int.F
C 7000  -> 7999  => spmd_anim.F
C 8000  -> 8999  => spmd_th.F
C 9000  -> 9999  => spmd_outp.F
C 10000 -> 10999 => spmd_stat.F
C 11000 -> 11999 => spmd_rest.F
C 12000 -> 12999 => spmd_lag.F
C 13000 -> 13999 => spmd_dsreso.F

#include "hardware.inc"
C
Chd|====================================================================
Chd|  SPMD_TRI7VOX0                 source/mpi/interfaces/spmd_int.F
Chd|-- called by -----------
Chd|        I7MAIN_TRI                    source/interfaces/intsort/i7main_tri.F
Chd|-- calls ---------------
Chd|        TRI7BOX                       share/modules/tri7box.F       
Chd|====================================================================
      SUBROUTINE SPMD_TRI7VOX0(
     1   X      ,BMINMAL ,IGAP   ,NRTM  ,STF   ,
     2   TZINF  ,CURV_MAX,GAPMIN ,GAPMAX,GAP_M ,
     3   IRECT  ,GAP     ,BGAPSMX,DRAD  ,DGAPLOAD)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE TRI7BOX
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "comlock.inc"
#include      "r4r8_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "task_c.inc"
#include      "timeri_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IGAP, NRTM, IRECT(4,*)
      my_real
     .        X(3,*), BMINMAL(*),
     .        STF(*), GAP_M(*), BGAPSMX,
     .        TZINF,GAPMIN,GAPMAX,GAP,CURV_MAX(NRTM)
      my_real , INTENT(IN) :: DRAD,DGAPLOAD
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
      INTEGER LOC_PROC,
     .        NBX,NBY,NBZ,NE,M1,M2,M3,M4,
     .        IX1,IY1,IZ1,IX2,IY2,IZ2,IX,IY,IZ
      my_real
     .        RATIO, AAA, MARGE,
     .        XMAXB,YMAXB,ZMAXB,XMINB,YMINB,ZMINB,
     .        XMINE,YMINE,ZMINE,XMAXE,YMAXE,ZMAXE,
     .        XX1,XX2,XX3,XX4,YY1,YY2,YY3,YY4,ZZ1,ZZ2,ZZ3,ZZ4
c      DATA IPWR2/1,2,4,8,16,32,64,128,256,512,1024,2048,4096,8192,
c     .           16384,32768,65536,131072,262144,524288,1048576,
c     .           2097152,4194304,8388608,16777216,33554432,67108864,
c     .           134217728,268435456,536870912,1073741824,2147483648/
       INTEGER TMP
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
C
C=======================================================================
C     tag des boites contenant des facettes
C     et creation des candidats
C=======================================================================

      LOC_PROC = ISPMD + 1
      MARGE = TZINF-MAX(GAP+DGAPLOAD,DRAD)

      NBX = LRVOXEL
      NBY = LRVOXEL
      NBZ = LRVOXEL

      XMAXB = BMINMAL(1)
      YMAXB = BMINMAL(2)
      ZMAXB = BMINMAL(3)
      XMINB = BMINMAL(4)
      YMINB = BMINMAL(5)
      ZMINB = BMINMAL(6)

      DO NE=1,NRTM
C on ne retient pas les facettes detruites
        IF(STF(NE) == ZERO)CYCLE

         IF(IGAP == 0)THEN
           AAA = TZINF+CURV_MAX(NE)
         ELSE
           AAA = MARGE+CURV_MAX(NE)+
     .           MAX(MIN(GAPMAX,MAX(GAPMIN,BGAPSMX+GAP_M(NE)))+DGAPLOAD,DRAD)
         ENDIF

c     il est possible d'ameliorer l'algo en decoupant la facette
c     en 2(4,3,6,9...) si la facette est grande devant AAA et inclinee

         M1 = IRECT(1,NE)
         M2 = IRECT(2,NE)
         M3 = IRECT(3,NE)
         M4 = IRECT(4,NE)

         XX1=X(1,M1)
         XX2=X(1,M2)
         XX3=X(1,M3)
         XX4=X(1,M4)
         XMAXE=MAX(XX1,XX2,XX3,XX4)
         XMINE=MIN(XX1,XX2,XX3,XX4)

         YY1=X(2,M1)
         YY2=X(2,M2)
         YY3=X(2,M3)
         YY4=X(2,M4)
         YMAXE=MAX(YY1,YY2,YY3,YY4)
         YMINE=MIN(YY1,YY2,YY3,YY4)

         ZZ1=X(3,M1)
         ZZ2=X(3,M2)
         ZZ3=X(3,M3)
         ZZ4=X(3,M4)
         ZMAXE=MAX(ZZ1,ZZ2,ZZ3,ZZ4)
         ZMINE=MIN(ZZ1,ZZ2,ZZ3,ZZ4)

c        indice des voxels occupes par la facette

         IX1=INT(NBX*(XMINE-AAA-XMINB)/(XMAXB-XMINB))
         IY1=INT(NBY*(YMINE-AAA-YMINB)/(YMAXB-YMINB))
         IZ1=INT(NBZ*(ZMINE-AAA-ZMINB)/(ZMAXB-ZMINB))

         IX1=MAX(0,MIN(NBX,IX1))
         IY1=MAX(0,MIN(NBY,IY1))
         IZ1=MAX(0,MIN(NBZ,IZ1))

         IX2=INT(NBX*(XMAXE+AAA-XMINB)/(XMAXB-XMINB))
         IY2=INT(NBY*(YMAXE+AAA-YMINB)/(YMAXB-YMINB))
         IZ2=INT(NBZ*(ZMAXE+AAA-ZMINB)/(ZMAXB-ZMINB))

         IX2=MAX(0,MIN(NBX,IX2))
         IY2=MAX(0,MIN(NBY,IY2))
         IZ2=MAX(0,MIN(NBZ,IZ2))

         DO IZ = IZ1, IZ2
           DO IY = IY1, IY2
             TMP = 0
             DO IX = IX1, IX2
               TMP=IBSET(TMP,IX)
             END DO
#include "atomic.inc"
             CRVOXEL(IY,IZ,LOC_PROC)=IOR(CRVOXEL(IY,IZ,LOC_PROC),TMP)
           END DO
         END DO
      ENDDO

C
      RETURN
      END
C
C END TRI7VOX0
Chd|====================================================================
Chd|  SPMD_TRI7VOX                  source/mpi/interfaces/spmd_int.F
Chd|-- called by -----------
Chd|        I23MAIN_TRI                   source/interfaces/intsort/i23main_tri.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        ARRET                         source/system/arret.F         
Chd|        STARTIME                      source/system/timer.F         
Chd|        STOPTIME                      source/system/timer.F         
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        TRI7BOX                       share/modules/tri7box.F       
Chd|====================================================================
      SUBROUTINE SPMD_TRI7VOX(
     1   NSV     ,NSN      ,X      ,V     ,MS     ,
     2   BMINMAL ,WEIGHT   ,STIFN  ,NIN   ,ISENDTO,
     3   IRCVFROM,IAD_ELEM ,FR_ELEM,NSNR  ,IGAP   ,
     4   GAP_S   ,ITAB     ,KINET  ,IFQ   ,INACTI ,
     5   NSNFIOLD,INTTH    ,IELEC  ,AREAS ,TEMP   ,
     6   NUM_IMP ,NODNX_SMS,GAP_S_L       ,ITYP,
     7   IRTLM   ,I24_TIME_S,I24_FRFI,I24_PENE_OLD,
     8   I24_STIF_OLD ,NBINFLG,ILEV ,I24_ICONT_I  ,
     9   INTFRIC ,IPARTFRICS,ITIED  ,IVIS2, IF_ADH)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE TRI7BOX
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
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "scr02_c.inc"
#include      "scr18_c.inc"
#include      "task_c.inc"
#include      "units_c.inc"
#include      "warn_c.inc"
#include      "timeri_c.inc"
#include      "sms_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NIN, NSN, IFQ, INACTI, IGAP,INTTH,NSNR,INTFRIC,
     .        ITIED, IVIS2,
     .        NSNFIOLD(*), NSV(*), WEIGHT(*),
     .        ISENDTO(NINTER+1,*), IRCVFROM(NINTER+1,*),
     .        IAD_ELEM(2,*), FR_ELEM(*), ITAB(*), KINET(*),
     .        IELEC(*),NUM_IMP, NODNX_SMS(*),IRTLM(*),ITYP,
     .        NBINFLG(*),ILEV,I24_ICONT_I(*),IPARTFRICS(*),IF_ADH(*)

      my_real
     .        X(3,*), V(3,*), MS(*), BMINMAL(*), STIFN(*), GAP_S(*),
     .        AREAS(*),TEMP(*),GAP_S_L(*),I24_TIME_S(*),I24_FRFI(6,*),
     .        I24_PENE_OLD(5,*),I24_STIF_OLD(2,*)
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
     .        INDEXI,ISINDEXI(NSPMD),INDEX(NUMNOD),NBOX(NSPMD),
     .        NBX,NBY,NBZ,IX,IY,IZ,
     .        MSGOFF, MSGOFF2, MSGOFF3, MSGOFF4, MSGOFF5,
     .        RSIZ, ISIZ, L2, REQ_SD3(NSPMD),REQ_RD2(NSPMD),
     .        LEN2, RSHIFT, ISHIFT, ND, JDEB, Q, NBB
     
      DATA MSGOFF/6000/
      DATA MSGOFF2/6001/
      DATA MSGOFF3/6002/
      DATA MSGOFF4/6003/ 
      DATA MSGOFF5/6004/ 
        
      my_real
     .        BMINMA(6,NSPMD),
     .        XMAXB,YMAXB,ZMAXB,XMINB,YMINB,ZMINB
     
      TYPE(real_pointer), DIMENSION(NSPMD) :: RBUF
      TYPE(int_pointer) , DIMENSION(NSPMD) :: IBUF   
      INTEGER, DIMENSION(:), ALLOCATABLE :: ITAGNSNFI  

C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
C
C=======================================================================
C     tag des boites contenant des facettes
C     et creation des candidats
C=======================================================================
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
     .          CRVOXEL(0,0,LOC_PROC),
     .          (LRVOXEL+1)*(LRVOXEL+1),
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
     .          CRVOXEL(0,0,P),
     .         (LRVOXEL+1)*(LRVOXEL+1),
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

      JDEB = 0
      IF(ITYP.EQ.25)THEN
        ALLOCATE(ITAGNSNFI(NUMNOD),STAT=IERROR)
        ITAGNSNFI(1:NUMNOD) = 0
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
          NBOX(P) = 0
          NB = 0
          XMAXB = BMINMA(1,P)
          YMAXB = BMINMA(2,P)
          ZMAXB = BMINMA(3,P)
          XMINB = BMINMA(4,P)
          YMINB = BMINMA(5,P)
          ZMINB = BMINMA(6,P)

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
                       IF(BTEST(CRVOXEL(IY,IZ,P),IX)) THEN
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
          NBOX(P) = NB
C
          DO J = IAD_ELEM(1,P), IAD_ELEM(1,P+1)-1
            NOD = FR_ELEM(J)
C remise de weight > 0
            WEIGHT(NOD) = WEIGHT(NOD)*(-1)
          ENDDO
C old tag
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
          CALL MPI_ISEND(NBOX(P),1,MPI_INTEGER,IT_SPMD(P),MSGTYP,
     .                 MPI_COMM_WORLD,REQ_SD(P),IERROR)
C
C Alloc buffer
C
          IF (NB.GT.0) THEN
            ALLOCATE(RBUF(P)%P(RSIZ*NB),STAT=IERROR)
	      ALLOCATE(IBUF(P)%P(ISIZ*NB),STAT=IERROR)
            IF(IERROR.NE.0) THEN
              CALL ANCMSG(MSGID=20,ANMODE=ANINFO)
              CALL ARRET(2)
            ENDIF
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
		 IBUF(P)%p(L2+ISHIFT) = IELEC(I)
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

            MSGTYP = MSGOFF4

            CALL MPI_ISEND(
     1        RBUF(P)%P(1),NB*RSIZ,REAL,IT_SPMD(P),MSGTYP,
     2        MPI_COMM_WORLD,REQ_SD2(P),ierror)
     
            MSGTYP = MSGOFF5
            CALL MPI_ISEND(
     1        IBUF(P)%P(1),NB*ISIZ,MPI_INTEGER,IT_SPMD(P),MSGTYP,
     2        MPI_COMM_WORLD,REQ_SD3(P),ierror)
         
          ENDIF
C
C reset old tag for next P
          IF(ITYP.EQ.25)THEN
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
      IF(ITYP.EQ.25) DEALLOCATE(ITAGNSNFI)
C
Cel reception  des donnees XREM
C
      IF(IRCVFROM(NIN,LOC_PROC).NE.0) THEN
        NSNR = 0
        L=0
        DO P = 1, NSPMD
          NSNFI(NIN)%P(P) = 0
          IF(ISENDTO(NIN,P).NE.0) THEN
            IF(LOC_PROC.NE.P) THEN
              MSGTYP = MSGOFF3 
              CALL MPI_RECV(NSNFI(NIN)%P(P),1,MPI_INTEGER,IT_SPMD(P),
     .                      MSGTYP,MPI_COMM_WORLD,STATUS,IERROR)
              IF(NSNFI(NIN)%P(P).GT.0) THEN
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
       IF(NSNR.GT.0) THEN
	
          ALLOCATE(XREM(RSIZ,NSNR),STAT=IERROR)
          ALLOCATE(IREM(ISIZ,NSNR),STAT=IERROR)	  
	
          IF(IERROR.NE.0) THEN
            CALL ANCMSG(MSGID=20,ANMODE=ANINFO)
            CALL ARRET(2)
          ENDIF
          IDEB = 1
          DO L = 1, NBIRECV
            P = ISINDEXI(L)
            LEN = NSNFI(NIN)%P(P)*RSIZ
            MSGTYP = MSGOFF4 
	    
            CALL MPI_IRECV(
     1        XREM(1,IDEB),LEN,REAL,IT_SPMD(P),
     2        MSGTYP,MPI_COMM_WORLD,REQ_RD(L),IERROR)
     
            LEN2 = NSNFI(NIN)%P(P)*ISIZ
            MSGTYP = MSGOFF5 
            CALL MPI_IRECV(
     1        IREM(1,IDEB),LEN2,MPI_INTEGER,IT_SPMD(P),
     2        MSGTYP,MPI_COMM_WORLD,REQ_RD2(L),IERROR)
            IDEB = IDEB + NSNFI(NIN)%P(P)	   	    
          ENDDO
          DO L = 1, NBIRECV
            CALL MPI_WAITANY(NBIRECV,REQ_RD,INDEXI,STATUS,IERROR)
            CALL MPI_WAITANY(NBIRECV,REQ_RD2,INDEXI,STATUS,IERROR)
          ENDDO

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
              IF(NBOX(P).NE.0) THEN
                CALL MPI_WAIT(REQ_SD2(P),STATUS,IERROR)
                DEALLOCATE(RBUF(P)%p)
                CALL MPI_WAIT(REQ_SD3(P),STATUS,IERROR)
                DEALLOCATE(IBUF(P)%p)			
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
C
c END TRI7VOX
Chd|====================================================================
Chd|  SPMD_TRI18_151VOX             source/mpi/interfaces/spmd_int.F
Chd|-- called by -----------
Chd|        I7MAIN_TRI                    source/interfaces/intsort/i7main_tri.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        ARRET                         source/system/arret.F         
Chd|        STARTIME                      source/system/timer.F         
Chd|        STOPTIME                      source/system/timer.F         
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        MULTI_FVM_MOD                 ../common_source/modules/multi_fvm_mod.F
Chd|        TRI7BOX                       share/modules/tri7box.F       
Chd|====================================================================
      SUBROUTINE SPMD_TRI18_151VOX(
     1   NSV     ,NSN      ,X      ,V     ,MS     ,
     2   BMINMAL ,WEIGHT   ,STIFN  ,NIN   ,ISENDTO,
     3   IRCVFROM,IAD_ELEM ,FR_ELEM,NSNR  ,IGAP   ,
     4   GAP_S   ,ITAB     ,KINET  ,IFQ   ,INACTI ,
     5   NSNFIOLD,INTTH    ,IELEC  ,AREAS ,TEMP   ,
     6   NUM_IMP ,NODNX_SMS,GAP_S_L       ,ITYP,
     7   IRTLM   ,I24_TIME_S,I24_FRFI,I24_PENE_OLD,
     8   I24_STIF_OLD ,NBINFLG,ILEV ,I24_ICONT_I, 
     8   IXS, MULTI_FVM,INTFRIC ,IPARTFRICS)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE TRI7BOX
      USE MESSAGE_MOD
      USE MULTI_FVM_MOD
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
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "scr02_c.inc"
#include      "scr18_c.inc"
#include      "task_c.inc"
#include      "units_c.inc"
#include      "warn_c.inc"
#include      "timeri_c.inc"
#include      "sms_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NIN, NSN, IFQ, INACTI, IGAP,INTTH,INTFRIC,
     .        NSNFIOLD(*), NSV(*), WEIGHT(*),
     .        ISENDTO(NINTER+1,*), IRCVFROM(NINTER+1,*),
     .        IAD_ELEM(2,*), FR_ELEM(*), ITAB(*), KINET(*),
     .        IELEC(*),NUM_IMP, NODNX_SMS(*),IRTLM(*),ITYP,
     .        NBINFLG(*),ILEV,I24_ICONT_I(*),NSNR,IXS(NIXS, *),
     .        IPARTFRICS(*)

      my_real
     .        X(3,*), V(3,*), MS(*), BMINMAL(*), STIFN(*), GAP_S(*),
     .        AREAS(*),TEMP(*),GAP_S_L(*),I24_TIME_S(*),I24_FRFI(6,*),
     .        I24_PENE_OLD(5,*),I24_STIF_OLD(2,*)

      TYPE (MULTI_FVM_STRUCT), INTENT(IN) :: MULTI_FVM
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
     .        INDEXI,ISINDEXI(NSPMD),INDEX(NUMNOD),NBOX(NSPMD),
     .        NBX,NBY,NBZ,IX,IY,IZ,
     .        MSGOFF, MSGOFF2, MSGOFF3, MSGOFF4, MSGOFF5,
     .        RSIZ, ISIZ, L2, REQ_SD3(NSPMD),REQ_RD2(NSPMD),
     .        LEN2, RSHIFT, ISHIFT, ND, JDEB, Q, NBB
     
      DATA MSGOFF/6000/
      DATA MSGOFF2/6001/
      DATA MSGOFF3/6002/
      DATA MSGOFF4/6003/ 
      DATA MSGOFF5/6004/ 
        
      my_real
     .        BMINMA(6,NSPMD),
     .        XMAXB,YMAXB,ZMAXB,XMINB,YMINB,ZMINB
     
      TYPE(real_pointer), DIMENSION(NSPMD) :: RBUF
      TYPE(int_pointer) , DIMENSION(NSPMD) :: IBUF   
      INTEGER, DIMENSION(:), ALLOCATABLE :: ITAGNSNFI  

C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
C
C=======================================================================
C     tag des boites contenant des facettes
C     et creation des candidats
C=======================================================================
      LOC_PROC = ISPMD + 1

      NBX = LRVOXEL
      NBY = LRVOXEL
      NBZ = LRVOXEL
C
C Sauvegarde valeur ancienne des nsn frontieres
C
      IF(INACTI.EQ.5.OR.INACTI.EQ.6.OR.INACTI.EQ.7.OR.IFQ.GT.0
     .   .OR.NUM_IMP.GT.0.OR.ITYP.EQ.23.OR.ITYP.EQ.24   
     .   .OR.ITYP.EQ.25) THEN
         DO P = 1, NSPMD
           NSNFIOLD(P) = NSNFI(NIN)%P(P)
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
     .          CRVOXEL(0,0,LOC_PROC),
     .          (LRVOXEL+1)*(LRVOXEL+1),
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
     .          CRVOXEL(0,0,P),
     .         (LRVOXEL+1)*(LRVOXEL+1),
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


      IDEB = 1

      JDEB = 0
      
      IF(ISENDTO(NIN,LOC_PROC).NE.0) THEN
        DO KK = 1, NBIRECV
          CALL MPI_WAITANY(NBIRECV,REQ_RB,INDEXI,STATUS,IERROR)
          P=IRINDEXI(INDEXI)
          CALL MPI_WAIT(REQ_RC(INDEXI),STATUS,IERROR)

          L = IDEB
          NBOX(P) = 0
          NB = 0
          XMAXB = BMINMA(1,P)
          YMAXB = BMINMA(2,P)
          ZMAXB = BMINMA(3,P)
          XMINB = BMINMA(4,P)
          YMINB = BMINMA(5,P)
          ZMINB = BMINMA(6,P)
          DO I=1,NSN
             NOD = NSV(I)
             IF(STIFN(I).GT.ZERO)THEN
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
                         IF(BTEST(CRVOXEL(IY,IZ,P),IX)) THEN
                            NB = NB + 1
                            INDEX(NB) = I
                         ENDIF
                      ENDIF
                   ENDIF
                ENDIF
             ENDIF
          ENDDO
          NBOX(P) = NB
C
C Envoi taille msg
C
          MSGTYP = MSGOFF3 
          CALL MPI_ISEND(NBOX(P),1,MPI_INTEGER,IT_SPMD(P),MSGTYP,
     .                 MPI_COMM_WORLD,REQ_SD(P),IERROR)
C
C Alloc buffer
C
          IF (NB.GT.0) THEN
            ALLOCATE(RBUF(P)%P(RSIZ*NB),STAT=IERROR)
	      ALLOCATE(IBUF(P)%P(ISIZ*NB),STAT=IERROR)
            IF(IERROR.NE.0) THEN
              CALL ANCMSG(MSGID=20,ANMODE=ANINFO)
              CALL ARRET(2)
            ENDIF
            L = 0
            L2= 0	    
              
c general case
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
               IBUF(P)%p(L2+2) = IXS(NIXS, NOD - NUMNOD)	
               IBUF(P)%p(L2+3) = KINET(NOD)
!     save specifics IREM and XREM indexes for INT24 sorting
               IBUF(P)%p(L2+4) = 0 !IGAPXREMP
               IBUF(P)%p(L2+5) = 0 !I24XREMP	
               IBUF(P)%p(L2+6) = 0 !I24IREMP
               L = L + RSIZ
               L2 = L2 + ISIZ 
            ENDDO

c shift for real variables (prepare for next setting)	    
	    RSHIFT = 9
c shift for integer variables (prepare for next setting) 
	    ISHIFT = 7 

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
		 IBUF(P)%p(L2+ISHIFT) = IELEC(I)
                 L = L + RSIZ
                 L2 = L2 + ISIZ
               END DO
	       RSHIFT = RSHIFT + 2
	       ISHIFT = ISHIFT + 1	       
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

            MSGTYP = MSGOFF4

            CALL MPI_ISEND(
     1        RBUF(P)%P(1),NB*RSIZ,REAL,IT_SPMD(P),MSGTYP,
     2        MPI_COMM_WORLD,REQ_SD2(P),ierror)
     
            MSGTYP = MSGOFF5
            CALL MPI_ISEND(
     1        IBUF(P)%P(1),NB*ISIZ,MPI_INTEGER,IT_SPMD(P),MSGTYP,
     2        MPI_COMM_WORLD,REQ_SD3(P),ierror)
         
          ENDIF
        ENDDO
      ENDIF       
C
C
Cel reception  des donnees XREM
C
      IF(IRCVFROM(NIN,LOC_PROC).NE.0) THEN
        NSNR = 0
        L=0
        DO P = 1, NSPMD
          NSNFI(NIN)%P(P) = 0
          IF(ISENDTO(NIN,P).NE.0) THEN
            IF(LOC_PROC.NE.P) THEN
              MSGTYP = MSGOFF3 
              CALL MPI_RECV(NSNFI(NIN)%P(P),1,MPI_INTEGER,IT_SPMD(P),
     .                      MSGTYP,MPI_COMM_WORLD,STATUS,IERROR)
              IF(NSNFI(NIN)%P(P).GT.0) THEN
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
       IF(NSNR.GT.0) THEN
	
          ALLOCATE(XREM(RSIZ,NSNR),STAT=IERROR)
          ALLOCATE(IREM(ISIZ,NSNR),STAT=IERROR)	  
	
          IF(IERROR.NE.0) THEN
            CALL ANCMSG(MSGID=20,ANMODE=ANINFO)
            CALL ARRET(2)
          ENDIF
          IDEB = 1
          DO L = 1, NBIRECV
            P = ISINDEXI(L)
            LEN = NSNFI(NIN)%P(P)*RSIZ
            MSGTYP = MSGOFF4 
	    
            CALL MPI_IRECV(
     1        XREM(1,IDEB),LEN,REAL,IT_SPMD(P),
     2        MSGTYP,MPI_COMM_WORLD,REQ_RD(L),IERROR)
     
            LEN2 = NSNFI(NIN)%P(P)*ISIZ
            MSGTYP = MSGOFF5 
            CALL MPI_IRECV(
     1        IREM(1,IDEB),LEN2,MPI_INTEGER,IT_SPMD(P),
     2        MSGTYP,MPI_COMM_WORLD,REQ_RD2(L),IERROR)
            IDEB = IDEB + NSNFI(NIN)%P(P)	   	    
          ENDDO
          DO L = 1, NBIRECV
            CALL MPI_WAITANY(NBIRECV,REQ_RD,INDEXI,STATUS,IERROR)
            CALL MPI_WAITANY(NBIRECV,REQ_RD2,INDEXI,STATUS,IERROR)
          ENDDO

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
              IF(NBOX(P).NE.0) THEN
                CALL MPI_WAIT(REQ_SD2(P),STATUS,IERROR)
                DEALLOCATE(RBUF(P)%p)
                CALL MPI_WAIT(REQ_SD3(P),STATUS,IERROR)
                DEALLOCATE(IBUF(P)%p)			
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
Chd|====================================================================
Chd|  SPMD_TRI24VOX                 source/mpi/interfaces/spmd_int.F
Chd|-- called by -----------
Chd|        I24MAIN_TRI                   source/interfaces/intsort/i24main_tri.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        ARRET                         source/system/arret.F         
Chd|        STARTIME                      source/system/timer.F         
Chd|        STOPTIME                      source/system/timer.F         
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        TRI7BOX                       share/modules/tri7box.F       
Chd|====================================================================
      SUBROUTINE SPMD_TRI24VOX(
     1   NSV     ,NSN      ,X      ,V     ,MS     ,
     2   BMINMAL ,WEIGHT   ,STIFN  ,NIN   ,ISENDTO,
     3   IRCVFROM,IAD_ELEM ,FR_ELEM,NSNR  ,IGAP   ,
     4   GAP_S   ,ITAB     ,KINET  ,IFQ   ,INACTI ,
     5   NSNFIOLD,INTTH    ,IELEC  ,AREAS ,TEMP   ,
     6   NUM_IMP ,NODNX_SMS,GAP_S_L       ,ITYP,
     7   I24_IRTLM,I24_TIME_S,I24_FRFI,I24_PENE_OLD,
     8   I24_STIF_OLD ,NBINFLG,ILEV ,I24_ICONT_I ,
     9   XFIC         ,VFIC   ,IEDGE4 ,NSNE,IS2SE,
     A   IRTSE,IS2PT,ISEGPT,MSFIC,NRTSE,IS2ID,ISPT2,
     B   INTFRIC,IPARTFRICS,T2MAIN_SMS,INTNITSCHE,FORNEQS,
     C   T2FAC_SMS)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE TRI7BOX
      USE MESSAGE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "r4r8_p.inc"
C-----------------------------------------------
C   M e s s a g e   P a s s i n g
C-----------------------------------------------
#ifdef  MPI
#include "mpif.h"
#endif
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "scr02_c.inc"
#include      "scr18_c.inc"
#include      "task_c.inc"
#include      "units_c.inc"
#include      "warn_c.inc"
#include      "timeri_c.inc"
#include      "sms_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NIN, NSN, IFQ, INACTI, IGAP,INTTH,INTFRIC,INTNITSCHE,
     .        NSNFIOLD(*), NSV(*), WEIGHT(*),
     .        ISENDTO(NINTER+1,*), IRCVFROM(NINTER+1,*),
     .        IAD_ELEM(2,*), FR_ELEM(*), ITAB(*), KINET(*),
     .        IELEC(*),NUM_IMP, NODNX_SMS(*),I24_IRTLM(2,*),ITYP,
     .        NBINFLG(*),ILEV,I24_ICONT_I(*),IEDGE4,NSNE,IS2SE(2,*),IRTSE(5,*),
     .        IS2PT(*),ISEGPT(*),NRTSE, NSNR,IS2ID(*),ISPT2(*),IPARTFRICS(*),T2MAIN_SMS(6,*)

      my_real
     .        X(3,*), V(3,*), MS(*), BMINMAL(*), STIFN(*), GAP_S(*),
     .        AREAS(*),TEMP(*),GAP_S_L(*),I24_TIME_S(*),I24_FRFI(6,*),
     .        I24_PENE_OLD(5,*),I24_STIF_OLD(2,*),XFIC(3,*),VFIC(3,*),MSFIC(*),
     .        FORNEQS(3,*),T2FAC_SMS(*)
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
#ifdef  MPI
      INTEGER MSGTYP,INFO,I,NOD, DT_CST, LOC_PROC,P,IDEB,
     .        SIZ,J, L, BUFSIZ, LEN, NB, IERROR1, IAD,
     .        STATUS(MPI_STATUS_SIZE),IERROR,REQ_SB(NSPMD),
     .        REQ_RB(NSPMD),KK,NBIRECV,IRINDEXI(NSPMD),
     .        REQ_RD(NSPMD),REQ_SD(NSPMD),REQ_SD2(NSPMD),
     .        REQ_RC(NSPMD),REQ_SC(NSPMD),
     .        INDEXI,ISINDEXI(NSPMD),INDEX(NUMNOD+NSNE),NBOX(NSPMD),
     .        NBX,NBY,NBZ,IX,IY,IZ,
     .        MSGOFF, MSGOFF2, MSGOFF3, MSGOFF4, MSGOFF5,
     .        RSIZ, ISIZ, L2, REQ_SD3(NSPMD),REQ_RD2(NSPMD),
     .        LEN2, RSHIFT, ISHIFT,BOXR,NBE,ND,SURF,N1,N2,N3,N4,
     .        SE,N
     
      DATA MSGOFF/6016/
      DATA MSGOFF2/6017/
      DATA MSGOFF3/6018/
      DATA MSGOFF4/6019/ 
      DATA MSGOFF5/6020/ 
        
      my_real
     .        BMINMA(6,NSPMD),
     .        XMAXB,YMAXB,ZMAXB,XMINB,YMINB,ZMINB
     
      TYPE(real_pointer), DIMENSION(NSPMD) :: RBUF
      TYPE(int_pointer) , DIMENSION(NSPMD) :: IBUF     

      INTEGER, DIMENSION(:), ALLOCATABLE :: TAG_SN,INDEXE,TAG_2RY,NSV_INV
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
C
      IF(IEDGE4 /=0)THEN 
           ALLOCATE(TAG_SN(NUMNOD))
           ALLOCATE(INDEXE(NUMNOD+NSNE))
           ALLOCATE(TAG_2RY(NSN))
           ALLOCATE(NSV_INV(NUMNOD))
      ELSE
           ALLOCATE(TAG_SN(0))
           ALLOCATE(INDEXE(0))
           ALLOCATE(TAG_2RY(0))
           ALLOCATE(NSV_INV(0))
      ENDIF
C=======================================================================
C     tag des boites contenant des facettes
C     et creation des candidats
C=======================================================================
      LOC_PROC = ISPMD + 1

      NBX = LRVOXEL
      NBY = LRVOXEL
      NBZ = LRVOXEL
C
C Sauvegarde valeur ancienne des nsn frontieres
C
      IF(INACTI.EQ.5.OR.INACTI.EQ.6.OR.INACTI.EQ.7.OR.IFQ.GT.0
     .   .OR.NUM_IMP.GT.0.OR.ITYP.EQ.23.OR.ITYP==24) THEN
         DO P = 1, NSPMD
           NSNFIOLD(P) = NSNFI(NIN)%P(P)
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
     .          CRVOXEL(0,0,LOC_PROC),
     .          (LRVOXEL+1)*(LRVOXEL+1),
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
     .          CRVOXEL(0,0,P),
     .         (LRVOXEL+1)*(LRVOXEL+1),
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
      ISIZ = 8

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
C Friction      
      IF(INTFRIC > 0 ) THEN    
	ISIZ = ISIZ + 1
      ENDIF

C -- IDTMINS==2      
      IF(IDTMINS == 2)THEN
        RSIZ = RSIZ + 1     
        ISIZ = ISIZ + 8
C -- IDTMINS_INT /= 0		
      ELSEIF(IDTMINS_INT/=0)THEN
        RSIZ = RSIZ + 1    
        ISIZ = ISIZ + 7
      END IF

c INT24      
      IF(ITYP==24)THEN
        RSIZ = RSIZ + 8
        ISIZ = ISIZ + 3
C-----for   NBINFLG      
        IF (ILEV==2) ISIZ = ISIZ + 1
        IF(IEDGE4 > 0)ISIZ = ISIZ + 8
      ENDIF    
C
C---Nitsche
        IF(INTNITSCHE > 0) RSIZ = RSIZ + 3

      IDEB = 1
      
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
          IF(IEDGE4 /=0)THEN 
             TAG_SN(1:NUMNOD)=0
             TAG_2RY(1:NSN)=0
          ENDIF
          
          L = IDEB
          NBOX(P) = 0
          NB = 0
          XMAXB = BMINMA(1,P)
          YMAXB = BMINMA(2,P)
          ZMAXB = BMINMA(3,P)
          XMINB = BMINMA(4,P)
          YMINB = BMINMA(5,P)
          ZMINB = BMINMA(6,P)
          DO I=1,NSN-NSNE
            NOD = NSV(I)
            IF(IEDGE4 >0)THEN
C Need an inverted NSV to add some Edge Nodes
               NSV_INV(NOD)=I
            ENDIF
            IF (NOD <= NUMNOD)THEN
              IF(WEIGHT(NOD).EQ.1)THEN
               IF(STIFN(I).GT.ZERO)THEN
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
                       IF(BTEST(CRVOXEL(IY,IZ,P),IX)) THEN
                         NB = NB + 1
                         INDEX(NB) = I
                         IF(IEDGE4>0) THEN
                               TAG_SN(NOD)=NB
                               TAG_2RY(I)=NB
                         ENDIF
                       ENDIF
                     ENDIF
                   ENDIF
                 ENDIF
               ENDIF
             ENDIF
            ENDIF
          ENDDO

            NBE = 0
            DO I=NSN-NSNE+1,NSN
            NOD = NSV(I)         
               IF(STIFN(I).GT.ZERO)THEN
                 ND = NOD-NUMNOD
                 IF(XFIC(1,ND) < XMINB)  CYCLE
                 IF(XFIC(1,ND) > XMAXB)  CYCLE
                 IF(XFIC(2,ND) < YMINB)  CYCLE
                 IF(XFIC(2,ND) > YMAXB)  CYCLE
                 IF(XFIC(3,ND) < ZMINB)  CYCLE
                 IF(XFIC(3,ND) > ZMAXB)  CYCLE

                 IX=INT(NBX*(XFIC(1,ND)-XMINB)/(XMAXB-XMINB))
                 IF(IX >= 0 .AND. IX <= NBX) THEN
                   IY=INT(NBY*(XFIC(2,ND)-YMINB)/(YMAXB-YMINB))
                   IF(IY >= 0 .AND. IY <= NBY) THEN
                     IZ=INT(NBZ*(XFIC(3,ND)-ZMINB)/(ZMAXB-ZMINB))
                     IF(IZ >= 0 .AND. IZ <= NBZ) THEN
                       IF(BTEST(CRVOXEL(IY,IZ,P),IX)) THEN
                         NBE = NBE + 1
                         INDEXE(NBE) = I
                         SURF=IS2SE(1,ND)

                         N1 = IRTSE(1,SURF)
                         IF( TAG_SN(N1)==0)THEN
                            NB = NB + 1
                            INDEX(NB) = NSV_INV(N1)
                            TAG_SN(N1)=-NB                ! Tag SN is tagged negatively - this will set ISEDGE_FI to -1
                         ENDIF                            ! in order to remove it from sorting. 
                         N2 = IRTSE(2,SURF)
                         IF( TAG_SN(N2)==0)THEN
                            NB = NB + 1
                            INDEX(NB) = NSV_INV(N2)
                            TAG_SN(N2)=-NB
                         ENDIF
                         N3 = IRTSE(3,SURF)
                         IF( TAG_SN(N3)==0)THEN
                            NB = NB + 1
                            INDEX(NB) = NSV_INV(N3)
                            TAG_SN(N3)=-NB
                         ENDIF
                         N4 = IRTSE(4,SURF)
                         IF( TAG_SN(N4)==0)THEN
                            NB = NB + 1
                            INDEX(NB) = NSV_INV(N4)
                            TAG_SN(N4)=-NB
                         ENDIF
                       ENDIF
                     ENDIF
                   ENDIF
                 ENDIF
               ENDIF     ! IF(STIFN(I).GT.ZERO)THEN
              
          ENDDO
C Have the E2E Fictive node at the end
          DO I=1,NBE
            NB = NB + 1
            INDEX(NB) = INDEXE(I)
            TAG_2RY(INDEXE(I))=NB
          ENDDO
C
          NBOX(P) = NB

          DO J = IAD_ELEM(1,P), IAD_ELEM(1,P+1)-1
            NOD = FR_ELEM(J)
C remise de weight > 0
            WEIGHT(NOD) = WEIGHT(NOD)*(-1)
          ENDDO
C
C Envoi taille msg
C
          MSGTYP = MSGOFF3 
          CALL MPI_ISEND(NBOX(P),1,MPI_INTEGER,IT_SPMD(P),MSGTYP,
     .                 MPI_COMM_WORLD,REQ_SD(P),IERROR)
C
C Alloc buffer
C
          IF (NB.GT.0) THEN
            ALLOCATE(RBUF(P)%P(RSIZ*NB),STAT=IERROR)
	      ALLOCATE(IBUF(P)%P(ISIZ*NB),STAT=IERROR)
            IF(IERROR.NE.0) THEN
              CALL ANCMSG(MSGID=20,ANMODE=ANINFO)
              CALL ARRET(2)
            ENDIF
            L = 0
	      L2= 0	    

c general case
#include      "vectorize.inc"
            DO J = 1, NB
              I = INDEX(J)
              NOD = NSV(I)
              IF(NOD <=NUMNOD)THEN
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
                IF(IEDGE4 >0)THEN
C Local Node : 0, local sleeping node : -1 or E2E Node : 1
C Local sleeping nodes are nodes which are not candidated but stays
C in E2E IRTS Secnd surface. Thay must be shipped, but removed from sorting
C IBUF(8,ND) ->ISEDGE_FI
                  IF(TAG_SN(NOD)<0)THEN
 	            IBUF(P)%p(L2+8) = -1
                  ELSE
	            IBUF(P)%p(L2+8) = 0
                  ENDIF
                ELSE
                  IBUF(P)%p(L2+8) = 0
                ENDIF
               ELSE
                ND=NOD-NUMNOD
                RBUF(P)%p(L+1) = XFIC(1,ND)
                RBUF(P)%p(L+2) = XFIC(2,ND)
                RBUF(P)%p(L+3) = XFIC(3,ND)
                RBUF(P)%p(L+4) = VFIC(1,ND)
                RBUF(P)%p(L+5) = VFIC(2,ND)
                RBUF(P)%p(L+6) = VFIC(3,ND)
                RBUF(P)%p(L+7) = MSFIC(ND)
                RBUF(P)%p(L+8) = STIFN(I)	  
	        IBUF(P)%p(L2+1) = I
	        IBUF(P)%p(L2+2) = IS2ID(ND)
	        IBUF(P)%p(L2+3) = 0
C Local Node : 0, local sleeping node : -1 or E2E Node : 1
	        IBUF(P)%p(L2+8) = 1
               ENDIF
              !save specifics IREM and XREM indexes for INT24 sorting
	        IBUF(P)%p(L2+4) = 0 !IGAPXREMP
	        IBUF(P)%p(L2+5) = 0 !I24XREMP	
	        IBUF(P)%p(L2+6) = 0 !I24IREMP
	        IBUF(P)%p(L2+7) = 0 !I24IREMPNSNE
              L = L + RSIZ
	        L2 = L2 + ISIZ
            END DO

c shift for real variables (prepare for next setting)	    
	    RSHIFT = 9
c shift for integer variables (prepare for next setting) 
	    ISHIFT = 9

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
		 IBUF(P)%p(L2+ISHIFT) = IELEC(I)
                 L = L + RSIZ
                 L2 = L2 + ISIZ
               END DO
	       RSHIFT = RSHIFT + 2
	       ISHIFT = ISHIFT + 1	       
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
               L = 0
               L2 = 0
#include      "vectorize.inc"	       	       
               DO J = 1, NB
                 I = INDEX(J)
                 NOD = NSV(I)
                 IF(NOD.LE.NUMNOD)THEN
                   RBUF(P)%p(L+RSHIFT)   = T2FAC_SMS(NOD)
                   IBUF(P)%p(L2+ISHIFT)  = NODNX_SMS(NOD)
                   IBUF(P)%p(L2+ISHIFT+1)= NOD
                   IBUF(P)%p(L2+ISHIFT+2)= T2MAIN_SMS(1,NOD)
                   IBUF(P)%p(L2+ISHIFT+3)= T2MAIN_SMS(2,NOD)
                   IBUF(P)%p(L2+ISHIFT+4)= T2MAIN_SMS(3,NOD)
                   IBUF(P)%p(L2+ISHIFT+5)= T2MAIN_SMS(4,NOD)
                   IBUF(P)%p(L2+ISHIFT+6)= T2MAIN_SMS(5,NOD)
                   IBUF(P)%p(L2+ISHIFT+7)= T2MAIN_SMS(6,NOD)           
                 ELSE
C E2E nodes (> NUMNOD) should not be need
C IRTSE Nodes are used.
                   RBUF(P)%p(L+RSHIFT)   = ONE
                   IBUF(P)%p(L2+ISHIFT)  = 0
                   IBUF(P)%p(L2+ISHIFT+1)= 0
                   IBUF(P)%p(L2+ISHIFT+2)= 0
                   IBUF(P)%p(L2+ISHIFT+3)= 0
                   IBUF(P)%p(L2+ISHIFT+4)= 0
                   IBUF(P)%p(L2+ISHIFT+5)= 0
                   IBUF(P)%p(L2+ISHIFT+6)= 0
                   IBUF(P)%p(L2+ISHIFT+7)= 0
                 ENDIF
                 L = L + RSIZ
                 L2 = L2 + ISIZ
               END DO
	       RSHIFT = RSHIFT + 1
	       ISHIFT = ISHIFT + 8
	       
C -- IDTMINS_INT /= 0	       
            ELSEIF(IDTMINS_INT.NE.0)THEN
              L = 0
              L2 = 0	         
#include      "vectorize.inc"	      
              DO J = 1, NB
                I = INDEX(J)
                NOD = NSV(I)
                RBUF(P)%p(L+RSHIFT)   = T2FAC_SMS(NOD)
                IBUF(P)%p(L2+ISHIFT)= NOD
                IBUF(P)%p(L2+ISHIFT+1)= T2MAIN_SMS(1,NOD)
                IBUF(P)%p(L2+ISHIFT+2)= T2MAIN_SMS(2,NOD)
                IBUF(P)%p(L2+ISHIFT+3)= T2MAIN_SMS(3,NOD)
                IBUF(P)%p(L2+ISHIFT+4)= T2MAIN_SMS(4,NOD)
                IBUF(P)%p(L2+ISHIFT+5)= T2MAIN_SMS(5,NOD)
                IBUF(P)%p(L2+ISHIFT+6)= T2MAIN_SMS(6,NOD)
                L = L + RSIZ    
                L2 = L2 + ISIZ
              END DO
	      RSHIFT = RSHIFT + 1
              ISHIFT = ISHIFT + 7 	      
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

                IBUF(P)%p(L2+ISHIFT)  =I24_IRTLM(1,I)
                IBUF(P)%p(L2+ISHIFT+1)=I24_IRTLM(2,I)
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
	      ISHIFT = ISHIFT + 1
              END IF

C E2E IRTS
            I24IREMPNSNE=ISHIFT
            IF(IEDGE4>0)THEN
    	      L2 = 0  	      
              DO J = 1, NB
               I = INDEX(J)
               NOD = NSV(I)
               IF(NOD > NUMNOD)THEN

                 ND = NOD-NUMNOD
C IRTS
                 SE=IS2SE(1,ND)
                 N =  IRTSE(1,SE)
	         IBUF(P)%p(L2+ISHIFT) = ABS(TAG_SN(N))
                 N =  IRTSE(2,SE)
	         IBUF(P)%p(L2+ISHIFT+1) = ABS(TAG_SN(N))
                 N =  IRTSE(3,SE)
	         IBUF(P)%p(L2+ISHIFT+2) = ABS(TAG_SN(N))
                 N =  IRTSE(4,SE)
	         IBUF(P)%p(L2+ISHIFT+3) = ABS(TAG_SN(N))
	         IBUF(P)%p(L2+ISHIFT+4) = IRTSE(5,SE)
                 IBUF(P)%p(L2+ISHIFT+5) = IS2PT(ND)
                 IBUF(P)%p(L2+ISHIFT+7) = ISPT2(I)
                 IBUF(P)%p(L2+ISHIFT+6) = ISEGPT(I)
               ELSE
C Is not an Edge
	         IBUF(P)%p(L2+ISHIFT)   = 0
	         IBUF(P)%p(L2+ISHIFT+1) = 0
	         IBUF(P)%p(L2+ISHIFT+2) = 0
	         IBUF(P)%p(L2+ISHIFT+3) = 0
	         IBUF(P)%p(L2+ISHIFT+4) = 0
	         IBUF(P)%p(L2+ISHIFT+5) = 0
                 IBUF(P)%p(L2+ISHIFT+7) = ISPT2(I)
                 IBUF(P)%p(L2+ISHIFT+6) = TAG_2RY(I)
               ENDIF

c                IF( ISEGPT(ND) < 0)THEN
c                    IBUF(P)%p(L2+ISHIFT+6) = ISEGPT(ND)
c                ELSE
c                    IBUF(P)%p(L2+ISHIFT+6) = TAG_2RY(I)
c                ENDIF

 
	         L2 = L2 + ISIZ
             END DO
       
	      ISHIFT = ISHIFT + 8
            ENDIF

	    END IF !(ITYP.EQ.24)	        
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
	      IBUF(P)%p(L2+7) = I24IREMPNSNE
	      L2 = L2 + ISIZ
            END DO

C NITSCHE      
             IF(INTNITSCHE > 0 ) THEN    
                L = 0
#include      "vectorize.inc"
                DO J = 1, NB
                   I = INDEX(J)
                   NOD = NSV(I)
                   RBUF(P)%p(L+RSHIFT)    =FORNEQS(1,NOD)
                   RBUF(P)%p(L+RSHIFT+1)  =FORNEQS(2,NOD)
                   RBUF(P)%p(L+RSHIFT+2)  =FORNEQS(3,NOD)
                   L = L + RSIZ
                END DO 	      
	        RSHIFT = RSHIFT + 3 
              ENDIF

            MSGTYP = MSGOFF4
            CALL MPI_ISEND(
     1        RBUF(P)%P(1),NB*RSIZ,REAL,IT_SPMD(P),MSGTYP,
     2        MPI_COMM_WORLD,REQ_SD2(P),ierror)
     
            MSGTYP = MSGOFF5
            CALL MPI_ISEND(
     1        IBUF(P)%P(1),NB*ISIZ,MPI_INTEGER,IT_SPMD(P),MSGTYP,
     2        MPI_COMM_WORLD,REQ_SD3(P),ierror)
         
          ENDIF
        ENDDO
      ENDIF       
C
Cel reception  des donnees XREM
C
      IF(IRCVFROM(NIN,LOC_PROC).NE.0) THEN
        NSNR = 0
        L=0
        DO P = 1, NSPMD
          NSNFI(NIN)%P(P) = 0
          IF(ISENDTO(NIN,P).NE.0) THEN
            IF(LOC_PROC.NE.P) THEN
              MSGTYP = MSGOFF3 
              CALL MPI_RECV(NSNFI(NIN)%P(P),1,MPI_INTEGER,IT_SPMD(P),
     .                      MSGTYP,MPI_COMM_WORLD,STATUS,IERROR)

              IF(NSNFI(NIN)%P(P).GT.0) THEN
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

       IF(NSNR.GT.0) THEN
	
          ALLOCATE(XREM(RSIZ,NSNR),STAT=IERROR)
          ALLOCATE(IREM(ISIZ,NSNR),STAT=IERROR)	
        
          IF(IERROR.NE.0) THEN
            CALL ANCMSG(MSGID=20,ANMODE=ANINFO)
            CALL ARRET(2)
          ENDIF
          IDEB = 1
          DO L = 1, NBIRECV
            P = ISINDEXI(L)
            LEN = NSNFI(NIN)%P(P)*RSIZ
            MSGTYP = MSGOFF4 
	    
            CALL MPI_IRECV(
     1        XREM(1,IDEB),LEN,REAL,IT_SPMD(P),
     2        MSGTYP,MPI_COMM_WORLD,REQ_RD(L),IERROR)
     
            LEN2 = NSNFI(NIN)%P(P)*ISIZ
            MSGTYP = MSGOFF5 
            CALL MPI_IRECV(
     1        IREM(1,IDEB),LEN2,MPI_INTEGER,IT_SPMD(P),
     2        MSGTYP,MPI_COMM_WORLD,REQ_RD2(L),IERROR)
     	    
            IDEB = IDEB + NSNFI(NIN)%P(P)	   	    
          ENDDO
          DO L = 1, NBIRECV
            CALL MPI_WAITANY(NBIRECV,REQ_RD,INDEXI,STATUS,IERROR)
            CALL MPI_WAITANY(NBIRECV,REQ_RD2,INDEXI,STATUS,IERROR)
          ENDDO

          !set specifics IREM and XREM indexes for INT24 sorting
          IGAPXREMP = IREM(4,1)
          I24XREMP  = IREM(5,1)
          I24IREMP  = IREM(6,1)
          I24IREMPNSNE = IREM(7,1)
C with E2E fictive nodes
C One needs to shift the IRTSE in order to be found (SHIFT to IDEB)
          IF(IEDGE4 >0)THEN
            IDEB=0

            DO L = 1, NBIRECV
               P = ISINDEXI(L)
               LEN = NSNFI(NIN)%P(P)
               DO I=1,LEN
                 IF(IREM(8,I+IDEB)==1)THEN
                    IREM(I24IREMPNSNE  ,I+IDEB)=IREM(I24IREMPNSNE  ,I+IDEB) + IDEB
                    IREM(I24IREMPNSNE+1,I+IDEB)=IREM(I24IREMPNSNE+1,I+IDEB) + IDEB
                    IREM(I24IREMPNSNE+2,I+IDEB)=IREM(I24IREMPNSNE+2,I+IDEB) + IDEB
                    IREM(I24IREMPNSNE+3,I+IDEB)=IREM(I24IREMPNSNE+3,I+IDEB) + IDEB
                 ENDIF
               ENDDO
               IDEB = IDEB + LEN
            ENDDO
          ENDIF
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
              IF(NBOX(P).NE.0) THEN
                CALL MPI_WAIT(REQ_SD2(P),STATUS,IERROR)
                DEALLOCATE(RBUF(P)%p)
                CALL MPI_WAIT(REQ_SD3(P),STATUS,IERROR)
                DEALLOCATE(IBUF(P)%p)			
              END IF
            ENDIF
          ENDIF
        ENDDO
      ENDIF
C
      IF(ALLOCATED(TAG_SN)) DEALLOCATE(TAG_SN)
      IF(ALLOCATED(TAG_SN)) DEALLOCATE(INDEXE)

      IF (IMONM > 0) CALL STOPTIME(25,1)
#endif
C
      RETURN
      END
C
c END TRI24VOX
Chd|====================================================================
Chd|  SPMD_TRI24GAT                 source/mpi/interfaces/spmd_int.F
Chd|-- called by -----------
Chd|        I24MAIN_TRI                   source/interfaces/intsort/i24main_tri.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        ARRET                         source/system/arret.F         
Chd|        H3D_MOD                       share/modules/h3d_mod.F       
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        TRI7BOX                       share/modules/tri7box.F       
Chd|====================================================================
      SUBROUTINE SPMD_TRI24GAT(RESULT,NSN ,CAND_N ,I_STOK,NIN,
     2                        IGAP  ,NSNR,MULTIMP,ITY,INTTH ,
     3                        ILEV  ,IEDGE4, H3D_DATA,INTFRIC,
     4                        INTNITSCHE)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE TRI7BOX
      USE MESSAGE_MOD
      USE H3D_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "r4r8_p.inc"
C-----------------------------------------------
C   M e s s a g e   P a s s i n g
C-----------------------------------------------
#ifdef  MPI
#include "mpif.h"
#endif
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "task_c.inc"
#include      "units_c.inc"
#include      "scr14_c.inc"
#include      "scr16_c.inc"
#include      "scr18_c.inc"
#include      "parit_c.inc"
#include      "spmd_c.inc"
#include      "sms_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER RESULT, NIN, NSN, I_STOK, IGAP, NSNR, MULTIMP, ITY,
     .        CAND_N(*),INTTH,ILEV,IEDGE4,INTFRIC,INTNITSCHE
      TYPE(H3D_DATABASE) :: H3D_DATA
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
#ifdef  MPI
      INTEGER OLDNSNR,NODFI,NNP,LSKYFI,
     .        NOD, LOC_PROC, I, N, NN, P, IDEB, J, K,
     .        IERROR1,IERROR2,IERROR3,IERROR4,IERROR5,IERROR6,IERROR7,
     .        IERROR8,IERROR9,IERROR0,IERROR11,IERROR12,
     .        IERROR13,IERROR14,IERROR15,IERROR16,IERROR17,IERROR18,
     .        INDEX(NSNR),NN2,RSHIFT,ISHIFT,ND
     
      INTEGER, DIMENSION(:), ALLOCATABLE :: IAUX,IAUXINV
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
      LOC_PROC = ISPMD + 1
C
C
      NODFI = 0
      LSKYFI= 0
      IF(RESULT.EQ.0) THEN
C
C Reperage des candidats
C
        NODFI = 0    
        DO I = 1, I_STOK
          N = CAND_N(I)
          NN = N-NSN
          IF(NN.GT.0)THEN
            IF(IREM(1,NN).GT.0)THEN
              NODFI = NODFI + 1
              IREM(1,NN) = -IREM(1,NN)
            ENDIF
          ENDIF
        ENDDO

C E2E Node : Ensure that all E2E IRTS Nodes are retained when 
C E2E Node is candidate
        IF(IEDGE4 >0)THEN
          NN2 = 0  
          IDEB = 0
          DO P = 1, NSPMD
            NN = 0
            OLDNSNR = NSNFI(NIN)%P(P)	
            DO I = 1, OLDNSNR
              IF(IREM(1,I+IDEB).LT.0 .AND.IREM(8,I+IDEB)==1 ) THEN
C go to where the Secnd surfaces are stored
                 I24IREMPNSNE = IREM(7,I+IDEB)

                 ND = IREM(I24IREMPNSNE,I+IDEB)

                 IF (IREM(1,ND) >0) THEN
                     IREM(1,ND)=IREM(1,ND)*(-1)
                     NODFI = NODFI + 1
                 ENDIF

                 ND = IREM(I24IREMPNSNE+1,I+IDEB)
                 IF (IREM(1,ND) >0) THEN
                     IREM(1,ND)=IREM(1,ND)*(-1)
                     NODFI = NODFI + 1
                 ENDIF

                 ND = IREM(I24IREMPNSNE+2,I+IDEB)
                 IF (IREM(1,ND) >0) THEN
                     IREM(1,ND)=IREM(1,ND)*(-1)
                     NODFI = NODFI + 1
                 ENDIF

                 ND = IREM(I24IREMPNSNE+3,I+IDEB)
                 IF (IREM(1,ND) >0) THEN
                     IREM(1,ND)=IREM(1,ND)*(-1)
                     NODFI = NODFI + 1
                 ENDIF

              ENDIF

            ENDDO
            IDEB = IDEB + OLDNSNR    
          ENDDO

        ENDIF
    
cccc          DO I = 1, I_STOK
cccc            N = CAND_N(I)
cccc            NN = N-NSN
cccc            IF(NN.GT.0)THEN
cccc
cccc             IF(IREM(1,NN).LT.0 .AND.IREM(8,NN)==1) THEN
cccc                 I24IREMPNSNE = IREM(7,NN)
cccc
cccc                 ND = IREM(I24IREMPNSNE,NN)
cccc                 IF(IREM(1,ND) >0) THEN
cccc                    IREM(1,ND)=IREM(1,ND)*(-1)
cccc                    NODFI = NODFI + 1
cccc                 ENDIF
cccc
cccc                 ND = IREM(I24IREMPNSNE+1,NN)
cccc                 IF(IREM(1,ND) >0) THEN
cccc                    IREM(1,ND)=IREM(1,ND)*(-1)
cccc                    NODFI = NODFI + 1
cccc                 ENDIF
cccc
cccc                 ND = IREM(I24IREMPNSNE+2,NN)
cccc                 IF(IREM(1,ND) >0) THEN
cccc                    IREM(1,ND)=IREM(1,ND)*(-1)
cccc                    NODFI = NODFI + 1
cccccccc                 ENDIF
cccc                    
cccc                 ND = IREM(I24IREMPNSNE+3,NN)
cccc                 IF(IREM(1,ND) >0) THEN
cccc                    IREM(1,ND)=IREM(1,ND)*(-1)
cccc                    NODFI = NODFI + 1
cccc                 ENDIF
cccc             ENDIF
cccc            ENDIF
cccc          ENDDO
cccc        ENDIF


C
C Allocation des tableaux de frontieres interfaces
C
        IERROR1 = 0
        IERROR2 = 0
        IERROR3 = 0
        IERROR4 = 0
        IERROR5 = 0
        IERROR6 = 0
        IERROR7 = 0
        IERROR8 = 0
        IERROR9 = 0
        IERROR0 = 0
        IERROR11 = 0
        IERROR12 = 0
        IERROR13 = 0
        IERROR14 = 0
        IERROR15 = 0
        IERROR16 = 0
        IERROR17 = 0
        IERROR18 = 0
        IF(ASSOCIATED(NSVFI(NIN)%P)) DEALLOCATE(NSVFI(NIN)%P)
        ALLOCATE(NSVFI(NIN)%P(NODFI),STAT=IERROR1)
        IF(ASSOCIATED(XFI(NIN)%P)) DEALLOCATE(XFI(NIN)%P)
        ALLOCATE(XFI(NIN)%P(3,NODFI),STAT=IERROR2)
        IF(ASSOCIATED(VFI(NIN)%P)) DEALLOCATE(VFI(NIN)%P)
        ALLOCATE(VFI(NIN)%P(3,NODFI),STAT=IERROR3)
        IF(ASSOCIATED(MSFI(NIN)%P)) DEALLOCATE(MSFI(NIN)%P)
        ALLOCATE(MSFI(NIN)%P(NODFI),STAT=IERROR4)
        IF(ASSOCIATED(STIFI(NIN)%P)) DEALLOCATE(STIFI(NIN)%P)
        ALLOCATE(STIFI(NIN)%P(NODFI),STAT=IERROR5)
        IF(ASSOCIATED(ITAFI(NIN)%P)) DEALLOCATE(ITAFI(NIN)%P)
        ALLOCATE(ITAFI(NIN)%P(NODFI),STAT=IERROR6)
        IF(ITY.EQ.7.OR.ITY.EQ.22.OR.ITY.EQ.23.OR.ITY.EQ.24) THEN
          IF(ASSOCIATED(KINFI(NIN)%P)) DEALLOCATE(KINFI(NIN)%P)
          ALLOCATE(KINFI(NIN)%P(NODFI),STAT=IERROR8)
          IF(INTTH > 0 ) THEN
           IF(ASSOCIATED(TEMPFI(NIN)%P)) DEALLOCATE(TEMPFI(NIN)%P)
           ALLOCATE(TEMPFI(NIN)%P(NODFI),STAT=IERROR9)
           IF(ASSOCIATED(MATSFI(NIN)%P)) DEALLOCATE(MATSFI(NIN)%P)
           ALLOCATE(MATSFI(NIN)%P(NODFI),STAT=IERROR0)
           IF(ASSOCIATED(AREASFI(NIN)%P)) DEALLOCATE(AREASFI(NIN)%P)
           ALLOCATE(AREASFI(NIN)%P(NODFI),STAT=IERROR11)
          ENDIF 
        ENDIF
        IF(IDTMINS == 2) THEN
         IF(ASSOCIATED(NODNXFI(NIN)%P)) DEALLOCATE(NODNXFI(NIN)%P)
         ALLOCATE(NODNXFI(NIN)%P(NODFI),STAT=IERROR12)
         IF(ASSOCIATED(NODAMSFI(NIN)%P)) DEALLOCATE(NODAMSFI(NIN)%P)
         ALLOCATE(NODAMSFI(NIN)%P(NODFI),STAT=IERROR13)
         IF(ASSOCIATED(PROCAMSFI(NIN)%P)) DEALLOCATE(PROCAMSFI(NIN)%P)
         ALLOCATE(PROCAMSFI(NIN)%P(NODFI),STAT=IERROR14)
         IF(ASSOCIATED(T2MAIN_SMS_FI(NIN)%P)) DEALLOCATE(T2MAIN_SMS_FI(NIN)%P)
         ALLOCATE(T2MAIN_SMS_FI(NIN)%P(6,NODFI),STAT=IERROR14)
         IF(ASSOCIATED(T2FAC_SMS_FI(NIN)%P)) DEALLOCATE(T2FAC_SMS_FI(NIN)%P)
         ALLOCATE(T2FAC_SMS_FI(NIN)%P(NODFI),STAT=IERROR14)
        ELSEIF(IDTMINS_INT /= 0) THEN
         IF(ASSOCIATED(NODAMSFI(NIN)%P)) DEALLOCATE(NODAMSFI(NIN)%P)
         ALLOCATE(NODAMSFI(NIN)%P(NODFI),STAT=IERROR13)
         IF(ASSOCIATED(PROCAMSFI(NIN)%P)) DEALLOCATE(PROCAMSFI(NIN)%P)
         ALLOCATE(PROCAMSFI(NIN)%P(NODFI),STAT=IERROR14)
         IF(ASSOCIATED(T2MAIN_SMS_FI(NIN)%P)) DEALLOCATE(T2MAIN_SMS_FI(NIN)%P)
         ALLOCATE(T2MAIN_SMS_FI(NIN)%P(6,NODFI),STAT=IERROR14)
         IF(ASSOCIATED(T2FAC_SMS_FI(NIN)%P)) DEALLOCATE(T2FAC_SMS_FI(NIN)%P)
         ALLOCATE(T2FAC_SMS_FI(NIN)%P(NODFI),STAT=IERROR14)
        ENDIF 
        IF(IGAP.NE.0) THEN
          IF(ASSOCIATED(GAPFI(NIN)%P)) DEALLOCATE(GAPFI(NIN)%P)
          ALLOCATE(GAPFI(NIN)%P(NODFI),STAT=IERROR7)
          IF(IGAP.EQ.3) THEN
            IF(ASSOCIATED(GAP_LFI(NIN)%P)) DEALLOCATE(GAP_LFI(NIN)%P)
            ALLOCATE(GAP_LFI(NIN)%P(NODFI),STAT=IERROR7)
          ENDIF
        ENDIF
        IF(ITY.EQ.24)THEN
          IF(ASSOCIATED(IRTLM_FI(NIN)%P)) DEALLOCATE(IRTLM_FI(NIN)%P)
          ALLOCATE(IRTLM_FI(NIN)%P(2,NODFI),STAT=IERROR15)

          IF(ASSOCIATED(TIME_SFI(NIN)%P)) DEALLOCATE(TIME_SFI(NIN)%P)
          ALLOCATE(TIME_SFI(NIN)%P(NODFI),STAT=IERROR16)

          IF(ASSOCIATED(SECND_FRFI(NIN)%P)) DEALLOCATE(SECND_FRFI(NIN)%P)
          ALLOCATE(SECND_FRFI(NIN)%P(6,NODFI),STAT=IERROR16)

          IF(ASSOCIATED(PENE_OLDFI(NIN)%P))DEALLOCATE(PENE_OLDFI(NIN)%P)
          ALLOCATE(PENE_OLDFI(NIN)%P(5,NODFI),STAT=IERROR16)

          IF(ASSOCIATED(STIF_OLDFI(NIN)%P))DEALLOCATE(STIF_OLDFI(NIN)%P)
          ALLOCATE(STIF_OLDFI(NIN)%P(2,NODFI),STAT=IERROR16)
	 	
          IF(ASSOCIATED(ICONT_I_FI(NIN)%P))DEALLOCATE(ICONT_I_FI(NIN)%P)
          ALLOCATE(ICONT_I_FI(NIN)%P(NODFI),STAT=IERROR16)

C E2E //          
          IF(ASSOCIATED(ISEDGE_FI(NIN)%P))DEALLOCATE(ISEDGE_FI(NIN)%P)
          ALLOCATE(ISEDGE_FI(NIN)%P(NODFI),STAT=IERROR16)

          IF(IEDGE4 >0)THEN
            IF(ASSOCIATED(IRTSE_FI(NIN)%P))DEALLOCATE(IRTSE_FI(NIN)%P)
            ALLOCATE(IRTSE_FI(NIN)%P(5,NODFI),STAT=IERROR16)

            IF(ASSOCIATED(IS2PT_FI(NIN)%P))DEALLOCATE(IS2PT_FI(NIN)%P)
            ALLOCATE(IS2PT_FI(NIN)%P(NODFI),STAT=IERROR16)

            IF(ASSOCIATED(ISPT2_FI(NIN)%P))DEALLOCATE(ISPT2_FI(NIN)%P)
            ALLOCATE(ISPT2_FI(NIN)%P(NODFI),STAT=IERROR16)

            IF(ASSOCIATED(ISEGPT_FI(NIN)%P))DEALLOCATE(ISEGPT_FI(NIN)%P) 
            ALLOCATE(ISEGPT_FI(NIN)%P(NODFI),STAT=IERROR16)

            IF(ASSOCIATED(IS2SE_FI(NIN)%P))DEALLOCATE(IS2SE_FI(NIN)%P) 
            ALLOCATE(IS2SE_FI(NIN)%P(2,NODFI),STAT=IERROR16)

           ENDIF

        ENDIF
        IF(INTFRIC > 0 ) THEN
           IF(ASSOCIATED(IPARTFRICSFI(NIN)%P)) DEALLOCATE(IPARTFRICSFI(NIN)%P)
           ALLOCATE(IPARTFRICSFI(NIN)%P(NODFI),STAT=IERROR17)
        ENDIF

        IF(INTNITSCHE > 0 ) THEN 
           IF(ASSOCIATED(FORNEQSFI(NIN)%P))DEALLOCATE(FORNEQSFI(NIN)%P) 
           ALLOCATE(FORNEQSFI(NIN)%P(3,NODFI),STAT=IERROR18) 
        ENDIF  

C
        IF(IERROR1+IERROR2+IERROR3+IERROR4+IERROR5+
     +     IERROR6+IERROR7+IERROR8 + IERROR9 + IERROR0 + 
     +     IERROR11+IERROR12+IERROR13+IERROR14+IERROR15+
     +     IERROR16+IERROR17+IERROR18 .NE. 0) THEN
          CALL ANCMSG(MSGID=20,ANMODE=ANINFO)
          CALL ARRET(2)
        ENDIF
C
C Compactage des candidats
C
        IDEB = 0
        NN2 = 0  
	
        DO P = 1, NSPMD
          NN = 0
          OLDNSNR = NSNFI(NIN)%P(P)	    
	    
          IF(OLDNSNR.NE.0) THEN
	   
	    ALLOCATE(IAUX(OLDNSNR),STAT=IERROR17)
	    ALLOCATE(IAUXINV(OLDNSNR),STAT=IERROR17)
            IAUXINV(1:OLDNSNR)=0
            IF(IERROR17.NE.0) THEN
              CALL ANCMSG(MSGID=20,ANMODE=ANINFO)
              CALL ARRET(2)
            ENDIF
	   	   
            NNP = NN2     

            DO I = 1, OLDNSNR
              IF(IREM(1,I+IDEB).LT.0) THEN	    
                NN = NN + 1
                IAUX(NN) = I
              ENDIF
            ENDDO		

c general case 
#include      "vectorize.inc"		
            DO J = 1, NN
	      I = IAUX(J)	    	
              INDEX(I+IDEB) = NN2+J
              XFI(NIN)%P(1,NN2+J) = XREM(1,I+IDEB)
              XFI(NIN)%P(2,NN2+J) = XREM(2,I+IDEB)
              XFI(NIN)%P(3,NN2+J) = XREM(3,I+IDEB)
              VFI(NIN)%P(1,NN2+J) = XREM(4,I+IDEB)
              VFI(NIN)%P(2,NN2+J) = XREM(5,I+IDEB)
              VFI(NIN)%P(3,NN2+J) = XREM(6,I+IDEB)
              MSFI(NIN)%P(NN2+J)  = XREM(7,I+IDEB)
              STIFI(NIN)%P(NN2+J) = XREM(8,I+IDEB)
              NSVFI(NIN)%P(NN2+J) = -IREM(1,I+IDEB)
              ITAFI(NIN)%P(NN2+J) = IREM(2,I+IDEB)
              KINFI(NIN)%P(NN2+J) = IREM(3,I+IDEB)
              ISEDGE_FI(NIN)%P(NN2+J) = IREM(8,I+IDEB)
              !ignore specifics IREM and XREM indexes for INT24 sorting
              !IGAPXREMP = IREM(4,I+IDEB)
              !I24XREMP  = IREM(5,I+IDEB)
              !I24IREMP  = IREM(6,I+IDEB)
              !I24IREMPNSNE = IREM(7,I+IDEB)
            ENDDO
	    
c shift for real variables (prepare for next setting)	    
            RSHIFT = 9
c shift for integer variables (prepare for next setting)  	    
            ISHIFT = 9
	    
c IGAP=1 or IGAP=2	
            IF(IGAP.EQ.1 .OR. IGAP.EQ.2)THEN 	    
#include      "vectorize.inc"	      
              DO J = 1, NN
	        I = IAUX(J)
                GAPFI(NIN)%P(NN2+J) = XREM(RSHIFT,I+IDEB)
              ENDDO   
              RSHIFT = RSHIFT + 1 		              	    
c IGAP=3	   
            ELSEIF(IGAP.EQ.3)THEN
#include      "vectorize.inc"	      	 	         
              DO J = 1, NN	       
	        I = IAUX(J)
	        GAPFI(NIN)%P(NN2+J)   = XREM(RSHIFT,I+IDEB)	  
                GAP_LFI(NIN)%P(NN2+J) = XREM(RSHIFT+1,I+IDEB)
              ENDDO
	      RSHIFT = RSHIFT + 2
         ENDIF	
	        
C thermic
            IF(INTTH.GT.0)THEN    
#include      "vectorize.inc"	      
              DO J = 1, NN
	        I = IAUX(J)
                TEMPFI(NIN)%P(NN2+J)  = XREM(RSHIFT,I+IDEB)
                AREASFI(NIN)%P(NN2+J) = XREM(RSHIFT+1,I+IDEB)
                MATSFI(NIN)%P(NN2+J)  = IREM(ISHIFT,I+IDEB)		  
              ENDDO
	      RSHIFT = RSHIFT + 2
	      ISHIFT = ISHIFT + 1	      
         ENDIF
C Friction model
            IF(INTFRIC.GT.0)THEN    
#include      "vectorize.inc"	      
              DO J = 1, NN
	        I = IAUX(J)
                IPARTFRICSFI(NIN)%P(NN2+J)  = IREM(ISHIFT,I+IDEB)		  
              ENDDO
	      ISHIFT = ISHIFT + 1	      
         ENDIF
	    
C -- IDTMINS==2	    
            IF(IDTMINS.EQ.2)THEN    
#include      "vectorize.inc"	      
              DO J = 1, NN
	        I = IAUX(J)
                T2FAC_SMS_FI(NIN)%P(NN2+J) = XREM(RSHIFT,I+IDEB)
                NODNXFI(NIN)%P(NN2+J)   = IREM(ISHIFT,I+IDEB)
                NODAMSFI(NIN)%P(NN2+J)  = IREM(ISHIFT+1,I+IDEB)
                T2MAIN_SMS_FI(NIN)%P(1,NN2+J)  = IREM(ISHIFT+2,I+IDEB)
                T2MAIN_SMS_FI(NIN)%P(2,NN2+J)  = IREM(ISHIFT+3,I+IDEB)
                T2MAIN_SMS_FI(NIN)%P(3,NN2+J)  = IREM(ISHIFT+4,I+IDEB)
                T2MAIN_SMS_FI(NIN)%P(4,NN2+J)  = IREM(ISHIFT+5,I+IDEB)
                T2MAIN_SMS_FI(NIN)%P(5,NN2+J)  = IREM(ISHIFT+6,I+IDEB)
                T2MAIN_SMS_FI(NIN)%P(6,NN2+J)  = IREM(ISHIFT+7,I+IDEB)
                PROCAMSFI(NIN)%P(NN2+J) = P		 
              ENDDO
	      RSHIFT = RSHIFT + 1
	      ISHIFT = ISHIFT + 8
	      
C -- IDTMINS_INT /= 0	      
            ELSEIF(IDTMINS_INT.NE.0)THEN	    
#include      "vectorize.inc"	      	     
              DO J = 1, NN
	        I = IAUX(J)
                T2FAC_SMS_FI(NIN)%P(NN2+J) = XREM(RSHIFT,I+IDEB)
		NODAMSFI(NIN)%P(NN2+J)  = IREM(ISHIFT,I+IDEB)
                T2MAIN_SMS_FI(NIN)%P(1,NN2+J)  = IREM(ISHIFT+1,I+IDEB)
                T2MAIN_SMS_FI(NIN)%P(2,NN2+J)  = IREM(ISHIFT+2,I+IDEB)
                T2MAIN_SMS_FI(NIN)%P(3,NN2+J)  = IREM(ISHIFT+3,I+IDEB)
                T2MAIN_SMS_FI(NIN)%P(4,NN2+J)  = IREM(ISHIFT+4,I+IDEB)
                T2MAIN_SMS_FI(NIN)%P(5,NN2+J)  = IREM(ISHIFT+5,I+IDEB)
                T2MAIN_SMS_FI(NIN)%P(6,NN2+J)  = IREM(ISHIFT+6,I+IDEB)  
		PROCAMSFI(NIN)%P(NN2+J) = P	   
              ENDDO
	      RSHIFT = RSHIFT + 1
	      ISHIFT = ISHIFT + 7 	      
            ENDIF

c INT24
	    IF(ITY.EQ.24)THEN    
#include      "vectorize.inc"	      	    	    
              DO J = 1, NN
                I = IAUX(J)
                IRTLM_FI(NIN)%P(1,NN2+J)  =IREM(ISHIFT,I+IDEB)
                IRTLM_FI(NIN)%P(2,NN2+J)  =IREM(ISHIFT+1,I+IDEB)
                ICONT_I_FI(NIN)%P(NN2+J) = IREM(ISHIFT+2,I+IDEB)
                TIME_SFI(NIN)%P(NN2+J )   =XREM(RSHIFT,I+IDEB)
                SECND_FRFI(NIN)%P(1,NN2+J) =ZERO
                SECND_FRFI(NIN)%P(2,NN2+J) =ZERO
                SECND_FRFI(NIN)%P(3,NN2+J) =ZERO
                SECND_FRFI(NIN)%P(4,NN2+J) =XREM(RSHIFT+1,I+IDEB)
                SECND_FRFI(NIN)%P(5,NN2+J) =XREM(RSHIFT+2,I+IDEB)
                SECND_FRFI(NIN)%P(6,NN2+J) =XREM(RSHIFT+3,I+IDEB)
                PENE_OLDFI(NIN)%P(1,NN2+J)=ZERO
                STIF_OLDFI(NIN)%P(1,NN2+J)=ZERO
                PENE_OLDFI(NIN)%P(2,NN2+J)=XREM(RSHIFT+4,I+IDEB)
                STIF_OLDFI(NIN)%P(2,NN2+J)=XREM(RSHIFT+5,I+IDEB)
C
C We Store PENE_OLD(3 in PENE_OLD(4 during sorting
                PENE_OLDFI(NIN)%P(4,NN2+J)=XREM(RSHIFT+6,I+IDEB)
                PENE_OLDFI(NIN)%P(5,NN2+J)=XREM(RSHIFT+7,I+IDEB)
              ENDDO
	      RSHIFT = RSHIFT + 8 
	      ISHIFT = ISHIFT + 3
              IF (ILEV==2) ISHIFT = ISHIFT + 1       

              IF (IEDGE4 > 0)THEN
                 DO J = 1, NN
                    I = IAUX(J)
                    IF( IREM(8,I+IDEB)==1)THEN
                      ND = IREM(ISHIFT,I+IDEB)
                      
                      IRTSE_FI(NIN)%P(1,NN2+J) = INDEX(ND)
                      ND = IREM(ISHIFT+1,I+IDEB)
                      IRTSE_FI(NIN)%P(2,NN2+J) = INDEX(ND)
                    
                      ND = IREM(ISHIFT+2,I+IDEB)
                      IRTSE_FI(NIN)%P(3,NN2+J) = INDEX(ND)
                    
                      ND = IREM(ISHIFT+3,I+IDEB)
                      IRTSE_FI(NIN)%P(4,NN2+J) = INDEX(ND)

                      IRTSE_FI(NIN)%P(5,NN2+J) = IREM(ISHIFT+4,I+IDEB)

                      IS2PT_FI(NIN)%P(NN2+J) =  IREM(ISHIFT+5,I+IDEB)
                      ISPT2_FI(NIN)%P(NN2+J) =  IREM(ISHIFT+7,I+IDEB)
                      IS2SE_FI(NIN)%P(1,NN2+J) = NN2+J
                      IS2SE_FI(NIN)%P(2,NN2+J) = 0
                    ELSE
                      IRTSE_FI(NIN)%P(1:5,NN2+J) = 0
                      IS2PT_FI(NIN)%P(NN2+J) = 0
                      ISEGPT_FI(NIN)%P(NN2+J) = 0
                      IS2SE_FI(NIN)%P(1,NN2+J) =0
                      IS2SE_FI(NIN)%P(2,NN2+J) = 0
                      ISPT2_FI(NIN)%P(NN2+J) =  IREM(ISHIFT+7,I+IDEB)
                    ENDIF
                      IF(IREM(ISHIFT+6,I+IDEB) > 0)THEN
c                        ND = IREM(ISHIFT+6,I+IDEB)
C                        ISEGPT_FI(NIN)%P(NN2+J)=  IAUXINV(ND)
                         ISEGPT_FI(NIN)%P(NN2+J)= 0
                      ELSE
                         ISEGPT_FI(NIN)%P(NN2+J)= 0
                      ENDIF
                 ENDDO 
	      ISHIFT = ISHIFT + 8
              ENDIF   	      	      
	    ENDIF
	    
C NITSCHE    
  
          IF(INTNITSCHE > 0 ) THEN 

#include      "vectorize.inc"		
            DO J = 1, NN
	        I = IAUX(J)	    	
                FORNEQSFI(NIN)%P(1,NN2+J) = XREM(RSHIFT,I+IDEB)
                FORNEQSFI(NIN)%P(2,NN2+J) = XREM(RSHIFT+1,I+IDEB)
                FORNEQSFI(NIN)%P(3,NN2+J) = XREM(RSHIFT+2,I+IDEB)
            ENDDO
	      RSHIFT = RSHIFT + 3   

          ENDIF
          
          NN2 = NN2 + NN	            
          IDEB = IDEB + OLDNSNR    
          NSNFI(NIN)%P(P) = NN2-NNP
	   
	    DEALLOCATE(IAUX)
	    DEALLOCATE(IAUXINV)
	   	    
          ENDIF !IF(OLDNSNR.NE.0) 
	   
        ENDDO  ! end do NSPMD        	
	
        LSKYFI = NN2*MULTIMAX
Cel nsnr nouveau utile pour inacti
        NSNR = NN2
      ENDIF
C
C Deallocation de XREM IREM
C
      IF(ALLOCATED(XREM)) DEALLOCATE(XREM)
      IF(ALLOCATED(IREM)) DEALLOCATE(IREM)

C
      IERROR1=0 
      IERROR2=0 
      IERROR3=0 
      IERROR4=0 
      IF(INTTH == 0 ) THEN
C
C Allocation Parith/OFF
C
        IF(IPARIT.EQ.0) THEN
         
          IF(ASSOCIATED(AFI(NIN)%P)) THEN
              DEALLOCATE(AFI(NIN)%P)
              NULLIFY(AFI(NIN)%P)
          ENDIF
          IF(ASSOCIATED(STNFI(NIN)%P)) THEN
              DEALLOCATE(STNFI(NIN)%P)
              NULLIFY(AFI(NIN)%P) 
          ENDIF

          IF(NODFI.GT.0)ALLOCATE(AFI(NIN)%P(3,NODFI*NTHREAD),STAT=IERROR1)
          IF(NODFI.GT.0)ALLOCATE(STNFI(NIN)%P(NODFI*NTHREAD),STAT=IERROR2)
C Init a 0
          DO I = 1, NODFI*NTHREAD
            AFI(NIN)%P(1,I) = ZERO
            AFI(NIN)%P(2,I) = ZERO
            AFI(NIN)%P(3,I) = ZERO
            STNFI(NIN)%P(I) = ZERO
          ENDDO
C
          IF(KDTINT.NE.0)THEN
            IF(ASSOCIATED(VSCFI(NIN)%P)) DEALLOCATE(VSCFI(NIN)%P)
            IF(NODFI.GT.0)ALLOCATE(VSCFI(NIN)%P(NODFI*NTHREAD),STAT=IERROR3)
C Init a 0
            DO I = 1, NODFI*NTHREAD
              VSCFI(NIN)%P(I) = ZERO
            ENDDO
          ENDIF
          NLSKYFI(NIN) = NODFI
C
        ELSE
C
C Allocation Parith/ON
C
          IF(ASSOCIATED(FSKYFI(NIN)%P)) DEALLOCATE(FSKYFI(NIN)%P)
          IF(ASSOCIATED(ISKYFI(NIN)%P)) DEALLOCATE(ISKYFI(NIN)%P)
          NLSKYFI(NIN) = LSKYFI
          IF(LSKYFI.GT.0) THEN
            ALLOCATE(ISKYFI(NIN)%P(LSKYFI),STAT=IERROR1)
            IF(KDTINT.EQ.0) THEN
              ALLOCATE(FSKYFI(NIN)%P(4,LSKYFI),STAT=IERROR2)
            ELSE
              ALLOCATE(FSKYFI(NIN)%P(5,LSKYFI),STAT=IERROR2)
            ENDIF
          ENDIF
        ENDIF
      ELSE
C
C Allocation Parith/OFF
C
        IF(IPARIT.EQ.0) THEN
          IF(ASSOCIATED(AFI(NIN)%P)) DEALLOCATE(AFI(NIN)%P)
          IF(ASSOCIATED(STNFI(NIN)%P)) DEALLOCATE(STNFI(NIN)%P)
          IF(ASSOCIATED(FTHEFI(NIN)%P)) DEALLOCATE(FTHEFI(NIN)%P)
          IF(NODFI.GT.0)ALLOCATE(AFI(NIN)%P(3,NODFI*NTHREAD),STAT=IERROR1)
          IF(NODFI.GT.0)ALLOCATE(STNFI(NIN)%P(NODFI*NTHREAD),STAT=IERROR2)
          IF(NODFI.GT.0)ALLOCATE(FTHEFI(NIN)%P(NODFI*NTHREAD),STAT=IERROR3)
C
          IF(NODADT_THERM ==1) THEN
            IF(ASSOCIATED(CONDNFI(NIN)%P)) DEALLOCATE(CONDNFI(NIN)%P)
            IF(NODFI.GT.0.AND.NODADT_THERM ==1)ALLOCATE(CONDNFI(NIN)%P(NODFI*NTHREAD),STAT=IERROR4)
          ENDIF
C

        
C Init a 0

          DO I = 1, NODFI*NTHREAD
            AFI(NIN)%P(1,I) = ZERO
            AFI(NIN)%P(2,I) = ZERO
            AFI(NIN)%P(3,I) = ZERO
            STNFI(NIN)%P(I) = ZERO
            FTHEFI(NIN)%P(I) = ZERO
          ENDDO
          IF(NODADT_THERM ==1) THEN
            DO I = 1, NODFI
               CONDNFI(NIN)%P(I) = ZERO
            ENDDO
          ENDIF
C
          IF(KDTINT.NE.0)THEN
            IF(ASSOCIATED(VSCFI(NIN)%P)) DEALLOCATE(VSCFI(NIN)%P)
            IF(NODFI.GT.0)ALLOCATE(VSCFI(NIN)%P(NODFI),STAT=IERROR4)
C Init a 0
            DO I = 1, NODFI
              VSCFI(NIN)%P(I) = ZERO
            ENDDO
          ENDIF
C
        ELSE
C
C Allocation Parith/ON
C
          IF(ASSOCIATED(FSKYFI(NIN)%P)) DEALLOCATE(FSKYFI(NIN)%P)
          IF(ASSOCIATED(ISKYFI(NIN)%P)) DEALLOCATE(ISKYFI(NIN)%P)
          IF(ASSOCIATED(FTHESKYFI(NIN)%P)) DEALLOCATE(FTHESKYFI(NIN)%P)
          NLSKYFI(NIN) = LSKYFI
          IF(LSKYFI.GT.0) THEN
            ALLOCATE(ISKYFI(NIN)%P(LSKYFI),STAT=IERROR1)
            IF(KDTINT.EQ.0) THEN
              ALLOCATE(FSKYFI(NIN)%P(4,LSKYFI),STAT=IERROR2)
              ALLOCATE(FTHESKYFI(NIN)%P(LSKYFI),STAT=IERROR3)
            ELSE
              ALLOCATE(FSKYFI(NIN)%P(5,LSKYFI),STAT=IERROR2)
              ALLOCATE(FTHESKYFI(NIN)%P(LSKYFI),STAT=IERROR3)
            ENDIF

            ENDIF
C
          IF(NODADT_THERM ==1) THEN
            IF(ASSOCIATED(CONDNSKYFI(NIN)%P)) DEALLOCATE(CONDNSKYFI(NIN)%P)
            IF(LSKYFI.GT.0) ALLOCATE(CONDNSKYFI(NIN)%P(LSKYFI),STAT=IERROR4)
          ENDIF
C

        ENDIF        
      ENDIF    
C
      IF(IERROR1+IERROR2+IERROR3+IERROR4.NE.0) THEN
        CALL ANCMSG(MSGID=20,ANMODE=ANINFO)
        CALL ARRET(2)
      ENDIF
C
C allocations conditionnelles output pression
C
      IF(ANIM_V(12)+OUTP_V(12)+H3D_DATA%N_VECT_PCONT .GT.0)THEN
        IF(ASSOCIATED(FNCONTI(NIN)%P)) DEALLOCATE(FNCONTI(NIN)%P)
        IF(ASSOCIATED(FTCONTI(NIN)%P)) DEALLOCATE(FTCONTI(NIN)%P)
        ALLOCATE(FNCONTI(NIN)%P(3,NODFI),STAT=IERROR1)
        ALLOCATE(FTCONTI(NIN)%P(3,NODFI),STAT=IERROR2)
        IF(IERROR1+IERROR2.NE.0) THEN
          CALL ANCMSG(MSGID=20,ANMODE=ANINFO)
          CALL ARRET(2)
        ELSE
          DO J = 1, NODFI
            FNCONTI(NIN)%P(1,J)=ZERO
            FNCONTI(NIN)%P(2,J)=ZERO
            FNCONTI(NIN)%P(3,J)=ZERO
            FTCONTI(NIN)%P(1,J)=ZERO
            FTCONTI(NIN)%P(2,J)=ZERO
            FTCONTI(NIN)%P(3,J)=ZERO
          END DO                  
        END IF  		      
      END IF
C
C
C Renumerotation des candidats
C
      DO I = 1, I_STOK
        N = CAND_N(I)
        NN = N-NSN
        IF(NN.GT.0)THEN
          CAND_N(I) = INDEX(NN)+NSN
        ENDIF
      ENDDO
C
#endif
      RETURN
      END

C END TRI24GAT
Chd|====================================================================
Chd|  SPMD_TRI25GAT                 source/mpi/interfaces/spmd_int.F
Chd|-- called by -----------
Chd|        I25MAIN_TRI                   source/interfaces/intsort/i25main_tri.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        ARRET                         source/system/arret.F         
Chd|        H3D_MOD                       share/modules/h3d_mod.F       
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        TRI7BOX                       share/modules/tri7box.F       
Chd|====================================================================
      SUBROUTINE SPMD_TRI25GAT(RESULT,NSN ,CAND_N ,I_STOK,NIN,
     2                        IGAP  ,NSNR,MULTIMP,ITY,INTTH ,
     3                        ILEV  ,NSNFIOLD,IPARI,NSNROLD ,
     4                        RENUM ,H3D_DATA,INTFRIC,FLAGREMN,
     5                        LREMNORMAX,NRTM,KREMNOD,REMNOD,
     6                        IVIS2)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE TRI7BOX
      USE MESSAGE_MOD
      USE H3D_MOD
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
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "com08_c.inc"
#include      "task_c.inc"
#include      "units_c.inc"
#include      "scr14_c.inc"
#include      "scr16_c.inc"
#include      "scr18_c.inc"
#include      "param_c.inc"
#include      "parit_c.inc"
#include      "spmd_c.inc"
#include      "sms_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER RESULT, NIN, NSN, I_STOK, IGAP, NSNR, MULTIMP, ITY,
     .        FLAGREMN,LREMNORMAX,NRTM,
     .        CAND_N(*),INTTH,ILEV, INTFRIC, IVIS2,
     .        NSNFIOLD(*), IPARI(NPARI,NINTER), NSNROLD, RENUM(*),
     .        KREMNOD(*), REMNOD(*)
      TYPE(H3D_DATABASE) :: H3D_DATA
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
#ifdef MPI
      INTEGER OLDNSNR,NODFI,NNP,LSKYFI,
     .        NOD, LOC_PROC, I, N, NN, P, IDEB, J, K, NI,
     .        IERROR1,IERROR2,IERROR3,IERROR4,IERROR5,IERROR6,IERROR7,
     .        IERROR8,IERROR9,IERROR0,IERROR11,IERROR12,
     .        IERROR13,IERROR14,IERROR15,IERROR16,IERROR17,IERROR18,IERROR19,IERROR20,
     .        IERROR21,INDEX(NSNR),NN2,RSHIFT,ISHIFT, IOLDNSNFI, ND, JDEB, NSNR_OLD, Q,
     .        KK ,SIZREMNORFI, NE, KI, KM, LL
     
      INTEGER, DIMENSION(:), ALLOCATABLE :: IAUX, IFFI_ADH
      my_real,
     .    DIMENSION(:), ALLOCATABLE :: STIFFI_OLD
      my_real,
     .     DIMENSION(:,:), ALLOCATABLE :: PENEFI_OLD, SECND_FRFI_OLD
      INTEGER, DIMENSION(:), ALLOCATABLE :: REMNOR_FI_TMP
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
      LOC_PROC = ISPMD + 1
C
C
      NODFI = 0
      LSKYFI= 0

      IF(.NOT.(ALLOCATED(CURRENT_FI_SIZE))) ALLOCATE(CURRENT_FI_SIZE(NINTER))
      IF(.NOT.(ALLOCATED(CURRENT_NODFI))) ALLOCATE(CURRENT_NODFI(NINTER))


      IF(RESULT.EQ.0) THEN
C
C Reperage des candidats
C
        DO NI = 1, NSNROLD
C         STIFI a ete mis a jour dans SPMD_GET_STIF !
C         STIFI was not set at time=0 (not read from restart)
          IF((TT==ZERO.OR.STIFI(NIN)%P(NI).GT.ZERO).AND.IRTLM_FI(NIN)%P(4,NI)==LOC_PROC)THEN
            NODFI = NODFI + 1
            NN = RENUM(NI)
            IREM(1,NN) = -IREM(1,NN)
          END IF
        END DO
C
        DO I = 1, I_STOK
          N = CAND_N(I)
          NN = N-NSN
          IF(NN.GT.0)THEN
            IF(IREM(1,NN).GT.0)THEN
              NODFI = NODFI + 1
              IREM(1,NN) = -IREM(1,NN)
            ENDIF
          ENDIF
        ENDDO
C
C Allocation des tableaux de frontieres interfaces
C
        IERROR1 = 0
        IERROR2 = 0
        IERROR3 = 0
        IERROR4 = 0
        IERROR5 = 0
        IERROR6 = 0
        IERROR7 = 0
        IERROR8 = 0
        IERROR9 = 0
        IERROR0 = 0
        IERROR11 = 0
        IERROR12 = 0
        IERROR13 = 0
        IERROR14 = 0
        IERROR15 = 0
        IERROR16 = 0
        IERROR17 = 0
        IERROR18 = 0
        IERROR19 = 0
        IERROR20 = 0
        IERROR21 = 0


        ! margin can be implemented here in order to avoid 
        ! allocation / deallocation for type 25 in case of sliding
        CURRENT_FI_SIZE(NIN) = NODFI
        CURRENT_NODFI(NIN) = NODFI

        IF(ASSOCIATED(NSVFI(NIN)%P)) DEALLOCATE(NSVFI(NIN)%P)
        ALLOCATE(NSVFI(NIN)%P(NODFI),STAT=IERROR1)
        IF(ASSOCIATED(PMAINFI(NIN)%P)) DEALLOCATE(PMAINFI(NIN)%P)
        ALLOCATE(PMAINFI(NIN)%P(NODFI),STAT=IERROR2)
        IERROR1 = IERROR2 + IERROR1
        IF(ASSOCIATED(XFI(NIN)%P)) DEALLOCATE(XFI(NIN)%P)
        ALLOCATE(XFI(NIN)%P(3,NODFI),STAT=IERROR2)
        IF(ASSOCIATED(VFI(NIN)%P)) DEALLOCATE(VFI(NIN)%P)
        ALLOCATE(VFI(NIN)%P(3,NODFI),STAT=IERROR3)
        IF(ASSOCIATED(MSFI(NIN)%P)) DEALLOCATE(MSFI(NIN)%P)
        ALLOCATE(MSFI(NIN)%P(NODFI),STAT=IERROR4)
        IF(ASSOCIATED(STIFI(NIN)%P)) DEALLOCATE(STIFI(NIN)%P)
        ALLOCATE(STIFI(NIN)%P(NODFI),STAT=IERROR5)
        IF(ASSOCIATED(ITAFI(NIN)%P)) DEALLOCATE(ITAFI(NIN)%P)
        ALLOCATE(ITAFI(NIN)%P(NODFI),STAT=IERROR6)
        IF(ITY.EQ.7.OR.ITY.EQ.22.OR.ITY.EQ.23.OR.ITY.EQ.24.OR.
     +     ITY.EQ.25) THEN
          IF(ASSOCIATED(KINFI(NIN)%P)) DEALLOCATE(KINFI(NIN)%P)
          ALLOCATE(KINFI(NIN)%P(NODFI),STAT=IERROR8)
          IF(INTTH > 0 ) THEN
           IF(ASSOCIATED(TEMPFI(NIN)%P)) DEALLOCATE(TEMPFI(NIN)%P)
           ALLOCATE(TEMPFI(NIN)%P(NODFI),STAT=IERROR9)
           IF(ASSOCIATED(MATSFI(NIN)%P)) DEALLOCATE(MATSFI(NIN)%P)
           ALLOCATE(MATSFI(NIN)%P(NODFI),STAT=IERROR0)
           IF(ASSOCIATED(AREASFI(NIN)%P)) DEALLOCATE(AREASFI(NIN)%P)
           ALLOCATE(AREASFI(NIN)%P(NODFI),STAT=IERROR11)
          ENDIF
          IF(INTTH==0.AND.IVIS2==-1) THEN
           IF(ASSOCIATED(AREASFI(NIN)%P)) DEALLOCATE(AREASFI(NIN)%P)
           ALLOCATE(AREASFI(NIN)%P(NODFI),STAT=IERROR11)
c IF_ADHFI allocation is done later once NSNR_OLD is calculated
          ENDIF 
        ENDIF
        IF(IDTMINS == 2) THEN
         IF(ASSOCIATED(NODNXFI(NIN)%P)) DEALLOCATE(NODNXFI(NIN)%P)
         ALLOCATE(NODNXFI(NIN)%P(NODFI),STAT=IERROR12)
         IF(ASSOCIATED(NODAMSFI(NIN)%P)) DEALLOCATE(NODAMSFI(NIN)%P)
         ALLOCATE(NODAMSFI(NIN)%P(NODFI),STAT=IERROR13)
         IF(ASSOCIATED(PROCAMSFI(NIN)%P)) DEALLOCATE(PROCAMSFI(NIN)%P)
         ALLOCATE(PROCAMSFI(NIN)%P(NODFI),STAT=IERROR14)
        ELSEIF(IDTMINS_INT /= 0) THEN
         IF(ASSOCIATED(NODAMSFI(NIN)%P)) DEALLOCATE(NODAMSFI(NIN)%P)
         ALLOCATE(NODAMSFI(NIN)%P(NODFI),STAT=IERROR13)
         IF(ASSOCIATED(PROCAMSFI(NIN)%P)) DEALLOCATE(PROCAMSFI(NIN)%P)
         ALLOCATE(PROCAMSFI(NIN)%P(NODFI),STAT=IERROR14)
        ENDIF 
        IF(IGAP.NE.0) THEN
          IF(ASSOCIATED(GAPFI(NIN)%P)) DEALLOCATE(GAPFI(NIN)%P)
          ALLOCATE(GAPFI(NIN)%P(NODFI),STAT=IERROR7)
          IF(IGAP.EQ.3) THEN
            IF(ASSOCIATED(GAP_LFI(NIN)%P)) DEALLOCATE(GAP_LFI(NIN)%P)
            ALLOCATE(GAP_LFI(NIN)%P(NODFI),STAT=IERROR7)
          ENDIF
        ENDIF
C
        NSNR_OLD=IPARI(24,NIN)
        ALLOCATE(SECND_FRFI_OLD(3,NSNR_OLD),PENEFI_OLD(4,NSNR_OLD),
     .           STIFFI_OLD(NSNR_OLD),
     .           STAT=IERROR16)
        SECND_FRFI_OLD(1:3,1:NSNR_OLD)=ZERO
        PENEFI_OLD(1:4,1:NSNR_OLD)   =ZERO
        STIFFI_OLD(1:NSNR_OLD)       =ZERO

C create a temporary array IFFI_ADH to copy old values

	IF(ITY.EQ.25.AND.IVIS2==-1.AND.INTTH==0) THEN 
	  ALLOCATE(IFFI_ADH(NSNR_OLD), STAT=IERROR16)
	  IFFI_ADH(1:NSNR_OLD) = 0
	ENDIF				
C
        IF(ASSOCIATED(IRTLM_FI(NIN)%P)) DEALLOCATE(IRTLM_FI(NIN)%P)
        ALLOCATE(IRTLM_FI(NIN)%P(4,NODFI),STAT=IERROR15)
C
        IF(ASSOCIATED(TIME_SFI(NIN)%P)) DEALLOCATE(TIME_SFI(NIN)%P)
        ALLOCATE(TIME_SFI(NIN)%P(2*NODFI),STAT=IERROR16)
C
        IF(ASSOCIATED(SECND_FRFI(NIN)%P)) THEN
          SECND_FRFI_OLD(1:3,1:NSNR_OLD)=SECND_FRFI(NIN)%P(1:3,1:NSNR_OLD)
          DEALLOCATE(SECND_FRFI(NIN)%P)
        END IF
        ALLOCATE(SECND_FRFI(NIN)%P(6,NODFI),STAT=IERROR16)
C
        IF(ASSOCIATED(PENE_OLDFI(NIN)%P)) THEN
          PENEFI_OLD(1,1:NSNR_OLD)=PENE_OLDFI(NIN)%P(1,1:NSNR_OLD)
          PENEFI_OLD(2,1:NSNR_OLD)=PENE_OLDFI(NIN)%P(5,1:NSNR_OLD)
          PENEFI_OLD(3,1:NSNR_OLD)=PENE_OLDFI(NIN)%P(3,1:NSNR_OLD)
          PENEFI_OLD(4,1:NSNR_OLD)=PENE_OLDFI(NIN)%P(4,1:NSNR_OLD)
          DEALLOCATE(PENE_OLDFI(NIN)%P)
        END IF
        ALLOCATE(PENE_OLDFI(NIN)%P(5,NODFI),STAT=IERROR16)
C
        IF(ASSOCIATED(STIF_OLDFI(NIN)%P)) THEN
          STIFFI_OLD(1:NSNR_OLD)=STIF_OLDFI(NIN)%P(1,1:NSNR_OLD)
          DEALLOCATE(STIF_OLDFI(NIN)%P)
        END IF
        ALLOCATE(STIF_OLDFI(NIN)%P(2,NODFI),STAT=IERROR16)
C
C copy old values of if_adh
	IF(IVIS2==-1.AND.INTTH==0) THEN
	  IF(ASSOCIATED(IF_ADHFI(NIN)%P)) THEN
            IFFI_ADH(1:NSNR_OLD)=IF_ADHFI(NIN)%P(1:NSNR_OLD)
            DEALLOCATE(IF_ADHFI(NIN)%P)
          END IF
        ALLOCATE(IF_ADHFI(NIN)%P(NODFI),STAT=IERROR16)
        ENDIF
C	
        IF(ASSOCIATED(ICONT_I_FI(NIN)%P))DEALLOCATE(ICONT_I_FI(NIN)%P)
        ALLOCATE(ICONT_I_FI(NIN)%P(NODFI),STAT=IERROR16)

        IF(ASSOCIATED(ISKEW_FI(NIN)%P))DEALLOCATE(ISKEW_FI(NIN)%P)
        ALLOCATE(ISKEW_FI(NIN)%P(NODFI),STAT=IERROR17)

        IF(ASSOCIATED(ICODT_FI(NIN)%P))DEALLOCATE(ICODT_FI(NIN)%P)
        ALLOCATE(ICODT_FI(NIN)%P(NODFI),STAT=IERROR17)
C	
        IF(ASSOCIATED(ISLIDE_FI(NIN)%P))DEALLOCATE(ISLIDE_FI(NIN)%P)
        ALLOCATE(ISLIDE_FI(NIN)%P(4,NODFI),STAT=IERROR17)
C
C Friction model
        IF(INTFRIC > 0 ) THEN
           IF(ASSOCIATED(IPARTFRICSFI(NIN)%P)) DEALLOCATE(IPARTFRICSFI(NIN)%P)
           ALLOCATE(IPARTFRICSFI(NIN)%P(NODFI),STAT=IERROR18)
        ENDIF
C REMOVE NODES FLAGREMNOD
        IF(FLAGREMN == 2 ) THEN
           IF(ASSOCIATED(KREMNOR_FI(NIN)%P)) DEALLOCATE(KREMNOR_FI(NIN)%P)
           ALLOCATE(KREMNOR_FI(NIN)%P(NODFI+1),STAT=IERROR19)
           KREMNOR_FI(NIN)%P(1:NODFI+1) = 0
           IF(ASSOCIATED(REMNOR_FI(NIN)%P)) DEALLOCATE(REMNOR_FI(NIN)%P)
           NULLIFY(KREMNOR_FI(NIN)%P) 
        ENDIF
C
        IF(IERROR1+IERROR2+IERROR3+IERROR4+IERROR5+
     +     IERROR6+IERROR7+IERROR8 + IERROR9 + IERROR0 + 
     +     IERROR11+IERROR12+IERROR13+IERROR14+IERROR15+
     +     IERROR16+IERROR17+IERROR18+IERROR19.NE. 0) THEN
          CALL ANCMSG(MSGID=20,ANMODE=ANINFO)
          CALL ARRET(2)
        ENDIF
C
        SECND_FRFI(NIN)%P (1:6,1:NODFI)=ZERO
        PENE_OLDFI(NIN)%P(1:5,1:NODFI)=ZERO
        STIF_OLDFI(NIN)%P(1:2,1:NODFI)=ZERO
C
C reset FI
        IF(IVIS2==-1.AND.INTTH==0) IF_ADHFI(NIN)%P(1:NODFI) = 0
C
        IF(FLAGREMN == 2 ) THEN
           ALLOCATE(REMNOR_FI_TMP(NODFI*LREMNORMAX),STAT=IERROR20)
           IF(IERROR20.NE. 0) THEN
             CALL ANCMSG(MSGID=20,ANMODE=ANINFO)
             CALL ARRET(2)
           ENDIF
C
        ENDIF
C
C Compactage des candidats
C
        IDEB = 0
        NN2 = 0  

        JDEB = 0
	
        DO P = 1, NSPMD
          NN = 0
          OLDNSNR = NSNFI(NIN)%P(P)	    
         	    
          IF(OLDNSNR.NE.0) THEN
	   
	    ALLOCATE(IAUX(OLDNSNR),STAT=IERROR17)
            IF(IERROR17.NE.0) THEN
              CALL ANCMSG(MSGID=20,ANMODE=ANINFO)
              CALL ARRET(2)
            ENDIF
	   	   
            NNP = NN2     

            DO I = 1, OLDNSNR
              IF(IREM(1,I+IDEB).LT.0) THEN	    
                NN = NN + 1
                IAUX(NN) = I
              ENDIF
            ENDDO		

c general case 
#include      "vectorize.inc"		
            DO J = 1, NN
	      I = IAUX(J)	    	
              INDEX(I+IDEB) = NN2+J
              XFI(NIN)%P(1,NN2+J) = XREM(1,I+IDEB)
              XFI(NIN)%P(2,NN2+J) = XREM(2,I+IDEB)
              XFI(NIN)%P(3,NN2+J) = XREM(3,I+IDEB)
              VFI(NIN)%P(1,NN2+J) = XREM(4,I+IDEB)
              VFI(NIN)%P(2,NN2+J) = XREM(5,I+IDEB)
              VFI(NIN)%P(3,NN2+J) = XREM(6,I+IDEB)
              MSFI(NIN)%P(NN2+J)  = XREM(7,I+IDEB)
              STIFI(NIN)%P(NN2+J) = XREM(8,I+IDEB)
              NSVFI(NIN)%P(NN2+J) = -IREM(1,I+IDEB)
              ITAFI(NIN)%P(NN2+J) = IREM(2,I+IDEB)
              KINFI(NIN)%P(NN2+J) = IREM(3,I+IDEB)
              PMAINFI(NIN)%P(NN2+J) = P 

            
              !ignore specifics IREM and XREM indexes for INT24 sorting
              !IGAPXREMP = IREM(4,I+IDEB)
              !I24XREMP  = IREM(5,I+IDEB)
              !I24IREMP  = IREM(6,I+IDEB)
            ENDDO
	    
c shift for real variables (prepare for next setting)	    
            RSHIFT = 9
c shift for integer variables (prepare for next setting)  	    
            ISHIFT = 7
	    
            IF(.TRUE.)THEN    
#include      "vectorize.inc"         
              DO J = 1, NN
                I = IAUX(J)
                ICODT_FI(NIN)%P(NN2+J)  = IREM(ISHIFT+0,I+IDEB)               
                ISKEW_FI(NIN)%P(NN2+J)  = IREM(ISHIFT+1,I+IDEB)               
              ENDDO
              ISHIFT = ISHIFT + 2             
            ENDIF


c IGAP=1 or IGAP=2	
            IF(IGAP.EQ.1 .OR. IGAP.EQ.2)THEN 	    
#include      "vectorize.inc"	      
              DO J = 1, NN
	        I = IAUX(J)
                GAPFI(NIN)%P(NN2+J) = XREM(RSHIFT,I+IDEB)
              ENDDO   
              RSHIFT = RSHIFT + 1 		              	    
c IGAP=3	   
            ELSEIF(IGAP.EQ.3)THEN
#include      "vectorize.inc"	      	 	         
              DO J = 1, NN	       
	        I = IAUX(J)
	        GAPFI(NIN)%P(NN2+J)   = XREM(RSHIFT,I+IDEB)	  
                GAP_LFI(NIN)%P(NN2+J) = XREM(RSHIFT+1,I+IDEB)
              ENDDO
	      RSHIFT = RSHIFT + 2
         ENDIF	
	        
C thermic
            IF(INTTH.GT.0)THEN    
#include      "vectorize.inc"	      
              DO J = 1, NN
	        I = IAUX(J)
                TEMPFI(NIN)%P(NN2+J)  = XREM(RSHIFT,I+IDEB)
                AREASFI(NIN)%P(NN2+J) = XREM(RSHIFT+1,I+IDEB)
                MATSFI(NIN)%P(NN2+J)  = IREM(ISHIFT,I+IDEB)		  
              ENDDO
	      RSHIFT = RSHIFT + 2
	      ISHIFT = ISHIFT + 1	      
            ENDIF


C Interface Adhesion           

            IF(IVIS2==-1.AND.INTTH==0)THEN
              JDEB = 0
              DO Q=1,P-1
                JDEB = JDEB + NSNFIOLD(Q)
              END DO  
              IF(TT==0) THEN
#include      "vectorize.inc"                
                DO J = 1, NN
                  I = IAUX(J)
                  AREASFI(NIN)%P(NN2+J) = XREM(RSHIFT,I+IDEB)
                  IF_ADHFI(NIN)%P(NN2+J) = IREM(ISHIFT,I+IDEB)
                  IOLDNSNFI = IREM(ISHIFT+1,I+IDEB)
                ENDDO
              ELSE
#include      "vectorize.inc"
                DO J = 1, NN
                  I = IAUX(J)
                  AREASFI(NIN)%P(NN2+J) = XREM(RSHIFT,I+IDEB)
                  IOLDNSNFI = IREM(ISHIFT+1,I+IDEB)
                  IF(IOLDNSNFI /= 0)THEN 
                    IF_ADHFI(NIN)%P(NN2+J)=IFFI_ADH(IOLDNSNFI+JDEB)
                  ELSE
                    IF_ADHFI(NIN)%P(NN2+J)=0
                  ENDIF
                ENDDO            
              ENDIF
              RSHIFT = RSHIFT + 1
              ISHIFT = ISHIFT + 2
            ENDIF

C Friction model
         IF(INTFRIC.GT.0)THEN    
#include      "vectorize.inc"	      
            DO J = 1, NN
	       I = IAUX(J)
               IPARTFRICSFI(NIN)%P(NN2+J)  = IREM(ISHIFT,I+IDEB)		  
            ENDDO
	    ISHIFT = ISHIFT + 1	      
          ENDIF    
C -- IDTMINS==2	    
            IF(IDTMINS.EQ.2)THEN    
#include      "vectorize.inc"	      
              DO J = 1, NN
	        I = IAUX(J)
                NODNXFI(NIN)%P(NN2+J)   = IREM(ISHIFT,I+IDEB)
                NODAMSFI(NIN)%P(NN2+J)  = IREM(ISHIFT+1,I+IDEB)
                PROCAMSFI(NIN)%P(NN2+J) = P		 
              ENDDO
	      ISHIFT = ISHIFT + 2
	      
C -- IDTMINS_INT /= 0	      
            ELSEIF(IDTMINS_INT.NE.0)THEN	    
#include      "vectorize.inc"	      	     
              DO J = 1, NN
	        I = IAUX(J)
		NODAMSFI(NIN)%P(NN2+J)  = IREM(ISHIFT,I+IDEB) 
		PROCAMSFI(NIN)%P(NN2+J) = P	   
              ENDDO
	      ISHIFT = ISHIFT + 1 	      
            ENDIF

            JDEB = 0
            DO Q=1,P-1
              JDEB = JDEB + NSNFIOLD(Q)
            END DO

            IF(TT==ZERO)THEN
#include    "vectorize.inc"   
              DO J = 1, NN
             	I = IAUX(J)
             	IRTLM_FI(NIN)%P(1,NN2+J) = IREM(ISHIFT,I+IDEB)
             	IRTLM_FI(NIN)%P(2,NN2+J) = IREM(ISHIFT+1,I+IDEB)
             	IRTLM_FI(NIN)%P(3,NN2+J) = IREM(ISHIFT+2,I+IDEB)
             	IRTLM_FI(NIN)%P(4,NN2+J) = IREM(ISHIFT+3,I+IDEB)
             	ICONT_I_FI(NIN)%P(NN2+J) = IREM(ISHIFT+4,I+IDEB)

             	TIME_SFI(NIN)%P(2*(NN2+J-1)+1) =XREM(RSHIFT,I+IDEB)
             	TIME_SFI(NIN)%P(2*(NN2+J-1)+2) =XREM(RSHIFT+1,I+IDEB)
             	PENE_OLDFI(NIN)%P(5,NN2+J)     =XREM(RSHIFT+2,I+IDEB)
             	IOLDNSNFI = IREM(ISHIFT+5,I+IDEB)

             	IF(IOLDNSNFI /= 0)THEN
             	  SECND_FRFI(NIN)%P(1,NN2+J) =SECND_FRFI_OLD(1,IOLDNSNFI+JDEB)
             	  SECND_FRFI(NIN)%P(2,NN2+J) =SECND_FRFI_OLD(2,IOLDNSNFI+JDEB)
             	  SECND_FRFI(NIN)%P(3,NN2+J) =SECND_FRFI_OLD(3,IOLDNSNFI+JDEB)
             	  PENE_OLDFI(NIN)%P(1,NN2+J)=PENEFI_OLD(1,IOLDNSNFI+JDEB)
             	  STIF_OLDFI(NIN)%P(1,NN2+J)=STIFFI_OLD(IOLDNSNFI+JDEB)
             	ELSE
             	  SECND_FRFI(NIN)%P(1,NN2+J) =ZERO
             	  SECND_FRFI(NIN)%P(2,NN2+J) =ZERO
             	  SECND_FRFI(NIN)%P(3,NN2+J) =ZERO
             	  PENE_OLDFI(NIN)%P(1,NN2+J)=ZERO
             	  STIF_OLDFI(NIN)%P(1,NN2+J)=ZERO
             	END IF
c            	SECND_FRFI(NIN)%P(4,NN2+J) =ZERO
c            	SECND_FRFI(NIN)%P(5,NN2+J) =ZERO
c            	SECND_FRFI(NIN)%P(6,NN2+J) =ZERO
             	PENE_OLDFI(NIN)%P(2,NN2+J)=ZERO
             	STIF_OLDFI(NIN)%P(2,NN2+J)=ZERO
              ENDDO
            ELSE
#include    "vectorize.inc"   
              DO J = 1, NN
             	I = IAUX(J)
             	IRTLM_FI(NIN)%P(1,NN2+J) = IREM(ISHIFT,I+IDEB)
             	IRTLM_FI(NIN)%P(2,NN2+J) = IREM(ISHIFT+1,I+IDEB)
             	IRTLM_FI(NIN)%P(3,NN2+J) = IREM(ISHIFT+2,I+IDEB)
             	IRTLM_FI(NIN)%P(4,NN2+J) = IREM(ISHIFT+3,I+IDEB)
             	ICONT_I_FI(NIN)%P(NN2+J) = IREM(ISHIFT+4,I+IDEB)

             	TIME_SFI(NIN)%P(2*(NN2+J-1)+1) =XREM(RSHIFT,I+IDEB)
             	TIME_SFI(NIN)%P(2*(NN2+J-1)+2) =XREM(RSHIFT+1,I+IDEB)
             	IOLDNSNFI = IREM(ISHIFT+5,I+IDEB)

             	IF(IOLDNSNFI /= 0)THEN
             	  SECND_FRFI(NIN)%P(1,NN2+J) =SECND_FRFI_OLD(1,IOLDNSNFI+JDEB)
             	  SECND_FRFI(NIN)%P(2,NN2+J) =SECND_FRFI_OLD(2,IOLDNSNFI+JDEB)
             	  SECND_FRFI(NIN)%P(3,NN2+J) =SECND_FRFI_OLD(3,IOLDNSNFI+JDEB)
             	  PENE_OLDFI(NIN)%P(1,NN2+J)=PENEFI_OLD(1,IOLDNSNFI+JDEB)
             	  STIF_OLDFI(NIN)%P(1,NN2+J)=STIFFI_OLD(IOLDNSNFI+JDEB)
             	  PENE_OLDFI(NIN)%P(5,NN2+J)=PENEFI_OLD(2,IOLDNSNFI+JDEB)
             	  PENE_OLDFI(NIN)%P(3,NN2+J)=PENEFI_OLD(3,IOLDNSNFI+JDEB)
             	  PENE_OLDFI(NIN)%P(4,NN2+J)=PENEFI_OLD(4,IOLDNSNFI+JDEB)
             	ELSE
             	  SECND_FRFI(NIN)%P(1,NN2+J) =ZERO
             	  SECND_FRFI(NIN)%P(2,NN2+J) =ZERO
             	  SECND_FRFI(NIN)%P(3,NN2+J) =ZERO
             	  PENE_OLDFI(NIN)%P(1,NN2+J)=ZERO
             	  STIF_OLDFI(NIN)%P(1,NN2+J)=ZERO
             	  PENE_OLDFI(NIN)%P(5,NN2+J)=ZERO
             	  PENE_OLDFI(NIN)%P(3,NN2+J)=ZERO
             	  PENE_OLDFI(NIN)%P(4,NN2+J)=ZERO
             	END IF
c            	SECND_FRFI(NIN)%P(4,NN2+J) =ZERO
c            	SECND_FRFI(NIN)%P(5,NN2+J) =ZERO
c            	SECND_FRFI(NIN)%P(6,NN2+J) =ZERO
             	PENE_OLDFI(NIN)%P(2,NN2+J)=ZERO
             	STIF_OLDFI(NIN)%P(2,NN2+J)=ZERO
              ENDDO
            END IF
            RSHIFT = RSHIFT + 3 
            ISHIFT = ISHIFT + 6
            IF (ILEV==2) ISHIFT = ISHIFT + 1  

          
          NN2 = NN2 + NN	            
          IDEB = IDEB + OLDNSNR    
          NSNFI(NIN)%P(P) = NN2-NNP
	   
	    DEALLOCATE(IAUX)
	   	    
          ENDIF !IF(OLDNSNR.NE.0) 
	   
        ENDDO  ! end do NSPMD        	
	
        LSKYFI = NN2*MULTIMAX
Cel nsnr nouveau utile pour inacti
        NSNR = NN2
      ENDIF
C-------------------------------------------------------------------------------
C FLAGREMN REMOVE main SEGMENTS : no reception but reconstruction of the tab REMNOR_FI
C-------------------------------------------------------------------------------
      IF(FLAGREMN == 2 ) THEN
         KI = 0
!       this loop can't be vectorizd = not an inner loop and there is a dependency in the inner one !!!!! 
!#include      "vectorize.inc"	
         DO N = 1, NODFI
           DO NE=1,NRTM
              KK = KREMNOD(2*(NE-1)+2) + 1
              LL = KREMNOD(2*(NE-1)+3)  
              DO KM=KK,LL
                 IF(REMNOD(KM) == -ITAFI(NIN)%P(N) ) THEN
                    KREMNOR_FI(NIN)%P(N)=KREMNOR_FI(NIN)%P(N)+1
                    KI = KI+1
                    REMNOR_FI_TMP(KI) = NE
                 ENDIF
               ENDDO
            ENDDO
         ENDDO  
c
!       this loop can't be vectorizd = N/N+1 dependency !!!!! 
!#include      "vectorize.inc"	

         DO N=1,NODFI
           KREMNOR_FI(NIN)%P(N+1) = KREMNOR_FI(NIN)%P(N+1) + KREMNOR_FI(NIN)%P(N)
         END DO
C
!       this loop can't be vectorizd = N/N+1 dependency !!!!! 
!#include      "vectorize.inc"		
         DO N=NODFI,1,-1
           KREMNOR_FI(NIN)%P(N+1)=KREMNOR_FI(NIN)%P(N)
         END DO
         KREMNOR_FI(NIN)%P(1)=0  

         SIZREMNORFI = KREMNOR_FI(NIN)%P(NODFI+1)
         ALLOCATE(REMNOR_FI(NIN)%P(SIZREMNORFI),STAT=IERROR21)
         IF(SIZREMNORFI /= 0) THEN
c           IF(ASSOCIATED(REMNOR_FI(NIN)%P)) DEALLOCATE(REMNOR_FI(NIN)%P)
           IF(IERROR21.NE. 0) THEN
             CALL ANCMSG(MSGID=20,ANMODE=ANINFO)
             CALL ARRET(2)
           ENDIF
#include      "vectorize.inc"	
           DO N=1,SIZREMNORFI
             REMNOR_FI(NIN)%P(N) =REMNOR_FI_TMP(N)
           ENDDO
         ENDIF
         DEALLOCATE(REMNOR_FI_TMP)
      ENDIF



C
C Deallocation de XREM IREM
C
      IF(ALLOCATED(XREM)) DEALLOCATE(XREM)
      IF(ALLOCATED(IREM)) DEALLOCATE(IREM)

      IF(ITY.EQ.25)THEN
        DEALLOCATE(SECND_FRFI_OLD,PENEFI_OLD,STIFFI_OLD)
        IF(IVIS2==-1) DEALLOCATE(IFFI_ADH)
      END IF

C
      IERROR1=0 
      IERROR2=0 
      IERROR3=0 
      IERROR4=0 
      IF(INTTH == 0 ) THEN
C
C Allocation Parith/OFF
C
        IF(IPARIT.EQ.0) THEN
         
          IF(ASSOCIATED(AFI(NIN)%P)) THEN
              DEALLOCATE(AFI(NIN)%P)
              NULLIFY(AFI(NIN)%P)
          ENDIF
          IF(ASSOCIATED(STNFI(NIN)%P)) THEN
              DEALLOCATE(STNFI(NIN)%P)
              NULLIFY(AFI(NIN)%P) 
          ENDIF

          IF(NODFI.GT.0)ALLOCATE(AFI(NIN)%P(3,NODFI*NTHREAD),STAT=IERROR1)
          IF(NODFI.GT.0)ALLOCATE(STNFI(NIN)%P(NODFI*NTHREAD),STAT=IERROR2)
C Init a 0
          DO I = 1, NODFI*NTHREAD
            AFI(NIN)%P(1,I) = ZERO
            AFI(NIN)%P(2,I) = ZERO
            AFI(NIN)%P(3,I) = ZERO
            STNFI(NIN)%P(I) = ZERO
          ENDDO
C
          IF(KDTINT.NE.0)THEN
            IF(ASSOCIATED(VSCFI(NIN)%P)) DEALLOCATE(VSCFI(NIN)%P)
            IF(NODFI.GT.0)ALLOCATE(VSCFI(NIN)%P(NODFI*NTHREAD),STAT=IERROR3)
C Init a 0
            DO I = 1, NODFI*NTHREAD
              VSCFI(NIN)%P(I) = ZERO
            ENDDO
          ENDIF
          NLSKYFI(NIN) = NODFI
C
        ELSE
C
C Allocation Parith/ON Done in upgrade_rem_slv
C
        ENDIF
      ELSE
C
C Allocation Parith/OFF
C
        IF(IPARIT.EQ.0) THEN
          IF(ASSOCIATED(AFI(NIN)%P)) DEALLOCATE(AFI(NIN)%P)
          IF(ASSOCIATED(STNFI(NIN)%P)) DEALLOCATE(STNFI(NIN)%P)
          IF(ASSOCIATED(FTHEFI(NIN)%P)) DEALLOCATE(FTHEFI(NIN)%P)
          IF(NODFI.GT.0)ALLOCATE(AFI(NIN)%P(3,NODFI*NTHREAD),STAT=IERROR1)
          IF(NODFI.GT.0)ALLOCATE(STNFI(NIN)%P(NODFI*NTHREAD),STAT=IERROR2)
          IF(NODFI.GT.0)ALLOCATE(FTHEFI(NIN)%P(NODFI*NTHREAD),STAT=IERROR3)
C
          IF(NODADT_THERM ==1) THEN
            IF(ASSOCIATED(CONDNFI(NIN)%P)) DEALLOCATE(CONDNFI(NIN)%P)
            IF(NODFI.GT.0.AND.NODADT_THERM ==1)ALLOCATE(CONDNFI(NIN)%P(NODFI*NTHREAD),STAT=IERROR4)
          ENDIF
C

        
C Init a 0

          DO I = 1, NODFI*NTHREAD
            AFI(NIN)%P(1,I) = ZERO
            AFI(NIN)%P(2,I) = ZERO
            AFI(NIN)%P(3,I) = ZERO
            STNFI(NIN)%P(I) = ZERO
            FTHEFI(NIN)%P(I) = ZERO
          ENDDO
          IF(NODADT_THERM ==1) THEN
            DO I = 1, NODFI
               CONDNFI(NIN)%P(I) = ZERO
            ENDDO
          ENDIF
C
          IF(KDTINT.NE.0)THEN
            IF(ASSOCIATED(VSCFI(NIN)%P)) DEALLOCATE(VSCFI(NIN)%P)
            IF(NODFI.GT.0)ALLOCATE(VSCFI(NIN)%P(NODFI),STAT=IERROR4)
C Init a 0
            DO I = 1, NODFI
              VSCFI(NIN)%P(I) = ZERO
            ENDDO
          ENDIF
C
        ELSE
C
C Allocation Parith/ON
C

C Done in upgrade_rem_slv

        ENDIF        
      ENDIF    
C
      IF(IERROR1+IERROR2+IERROR3+IERROR4.NE.0) THEN
        CALL ANCMSG(MSGID=20,ANMODE=ANINFO)
        CALL ARRET(2)
      ENDIF
C
C allocations conditionnelles output pression
C
      IF(ANIM_V(12)+OUTP_V(12)+H3D_DATA%N_VECT_PCONT .GT.0)THEN
        IF(ASSOCIATED(FNCONTI(NIN)%P)) DEALLOCATE(FNCONTI(NIN)%P)
        IF(ASSOCIATED(FTCONTI(NIN)%P)) DEALLOCATE(FTCONTI(NIN)%P)
        ALLOCATE(FNCONTI(NIN)%P(3,NODFI),STAT=IERROR1)
        ALLOCATE(FTCONTI(NIN)%P(3,NODFI),STAT=IERROR2)
        IF(IERROR1+IERROR2.NE.0) THEN
          CALL ANCMSG(MSGID=20,ANMODE=ANINFO)
          CALL ARRET(2)
        ELSE
          DO J = 1, NODFI
            FNCONTI(NIN)%P(1,J)=ZERO
            FNCONTI(NIN)%P(2,J)=ZERO
            FNCONTI(NIN)%P(3,J)=ZERO
            FTCONTI(NIN)%P(1,J)=ZERO
            FTCONTI(NIN)%P(2,J)=ZERO
            FTCONTI(NIN)%P(3,J)=ZERO
          END DO                  
        END IF  		      
      END IF
C
C
C Renumerotation des candidats
C
      DO I = 1, I_STOK
        N = CAND_N(I)
        NN = N-NSN
        IF(NN.GT.0)THEN
          CAND_N(I) = INDEX(NN)+NSN
        ENDIF
      ENDDO
C
#endif
      RETURN
      END

C END TRI25GAT
Chd|====================================================================
Chd|  SPMD_TRI7GAT                  source/mpi/interfaces/spmd_int.F
Chd|-- called by -----------
Chd|        I22MAIN_TRI                   source/interfaces/intsort/i22main_tri.F
Chd|        I23MAIN_TRI                   source/interfaces/intsort/i23main_tri.F
Chd|        I7MAIN_TRI                    source/interfaces/intsort/i7main_tri.F
Chd|        INTER_SORT_07                 source/interfaces/int07/inter_sort_07.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        ARRET                         source/system/arret.F         
Chd|        H3D_MOD                       share/modules/h3d_mod.F       
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        MULTI_FVM_MOD                 ../common_source/modules/multi_fvm_mod.F
Chd|        TRI7BOX                       share/modules/tri7box.F       
Chd|====================================================================
      SUBROUTINE SPMD_TRI7GAT(RESULT ,NSN    ,CAND_N  ,I_STOK  ,NIN    ,
     2                        IGAP   ,NSNR   ,MULTIMP ,ITY     ,INTTH  ,
     3                        ILEV   ,NSNFIOLD,IPARI  ,H3D_DATA,INTFRIC,
     4                        MULTI_FVM)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE TRI7BOX
      USE MESSAGE_MOD
      USE H3D_MOD
      USE MULTI_FVM_MOD
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
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "task_c.inc"
#include      "units_c.inc"
#include      "scr14_c.inc"
#include      "scr16_c.inc"
#include      "scr18_c.inc"
#include      "param_c.inc"
#include      "parit_c.inc"
#include      "spmd_c.inc"
#include      "sms_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER RESULT, NIN, NSN, I_STOK, IGAP, NSNR, MULTIMP, ITY,
     .        CAND_N(*),INTTH,ILEV, INTFRIC,
     .        NSNFIOLD(*), IPARI(NPARI,NINTER)
      TYPE(H3D_DATABASE) :: H3D_DATA
      TYPE(MULTI_FVM_STRUCT) :: MULTI_FVM
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
#ifdef MPI
      INTEGER OLDNSNR,NODFI,NNP,LSKYFI,
     .        NOD, LOC_PROC, I, N, NN, P, IDEB, J, K,
     .        IERROR1,IERROR2,IERROR3,IERROR4,IERROR5,IERROR6,IERROR7,
     .        IERROR8,IERROR9,IERROR0,IERROR11,IERROR12,
     .        IERROR13,IERROR14,IERROR15,IERROR16,IERROR17,INDEX(NSNR),
     .        NN2,RSHIFT,ISHIFT, IOLDNSNFI, ND, JDEB, NSNR_OLD, Q
     
      INTEGER, DIMENSION(:), ALLOCATABLE :: IAUX
      my_real,
     .    DIMENSION(:), ALLOCATABLE :: PENEFI_OLD, STIFFI_OLD
      my_real,
     .     DIMENSION(:,:), ALLOCATABLE :: SECND_FRFI_OLD
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
      LOC_PROC = ISPMD + 1
C
C
      NODFI = 0
      LSKYFI= 0

      IF(RESULT.EQ.0) THEN
C
C Reperage des candidats
C
        DO I = 1, I_STOK
          N = CAND_N(I)
          NN = N-NSN
          IF(NN.GT.0)THEN
            IF(IREM(1,NN).GT.0)THEN
              NODFI = NODFI + 1
              IREM(1,NN) = -IREM(1,NN)
            ENDIF
          ENDIF
        ENDDO
C
C Allocation des tableaux de frontieres interfaces
C
        IERROR1 = 0
        IERROR2 = 0
        IERROR3 = 0
        IERROR4 = 0
        IERROR5 = 0
        IERROR6 = 0
        IERROR7 = 0
        IERROR8 = 0
        IERROR9 = 0
        IERROR0 = 0
        IERROR11 = 0
        IERROR12 = 0
        IERROR13 = 0
        IERROR14 = 0
        IERROR15 = 0
        IERROR16 = 0
        IERROR17 = 0

        IF(ASSOCIATED(NSVFI(NIN)%P)) DEALLOCATE(NSVFI(NIN)%P)
        ALLOCATE(NSVFI(NIN)%P(NODFI),STAT=IERROR1)
        IF(ASSOCIATED(PMAINFI(NIN)%P)) DEALLOCATE(PMAINFI(NIN)%P)
        ALLOCATE(PMAINFI(NIN)%P(NODFI),STAT=IERROR2)
        IERROR1 = IERROR2 + IERROR1
        IF(ASSOCIATED(XFI(NIN)%P)) DEALLOCATE(XFI(NIN)%P)
        ALLOCATE(XFI(NIN)%P(3,NODFI),STAT=IERROR2)
        IF(ASSOCIATED(VFI(NIN)%P)) DEALLOCATE(VFI(NIN)%P)
        ALLOCATE(VFI(NIN)%P(3,NODFI),STAT=IERROR3)
        IF(ASSOCIATED(MSFI(NIN)%P)) DEALLOCATE(MSFI(NIN)%P)
        ALLOCATE(MSFI(NIN)%P(NODFI),STAT=IERROR4)
        IF(ASSOCIATED(STIFI(NIN)%P)) DEALLOCATE(STIFI(NIN)%P)
        ALLOCATE(STIFI(NIN)%P(NODFI),STAT=IERROR5)
        IF(ASSOCIATED(ITAFI(NIN)%P)) DEALLOCATE(ITAFI(NIN)%P)
        ALLOCATE(ITAFI(NIN)%P(NODFI),STAT=IERROR6)
        IF(ITY.EQ.7.OR.ITY.EQ.22.OR.ITY.EQ.23.OR.ITY.EQ.24) THEN
          IF(ASSOCIATED(KINFI(NIN)%P)) DEALLOCATE(KINFI(NIN)%P)
          ALLOCATE(KINFI(NIN)%P(NODFI),STAT=IERROR8)
          IF(INTTH > 0 ) THEN
           IF(ASSOCIATED(TEMPFI(NIN)%P)) DEALLOCATE(TEMPFI(NIN)%P)
           ALLOCATE(TEMPFI(NIN)%P(NODFI),STAT=IERROR9)
           IF(ASSOCIATED(MATSFI(NIN)%P)) DEALLOCATE(MATSFI(NIN)%P)
           ALLOCATE(MATSFI(NIN)%P(NODFI),STAT=IERROR0)
           IF(ASSOCIATED(AREASFI(NIN)%P)) DEALLOCATE(AREASFI(NIN)%P)
           ALLOCATE(AREASFI(NIN)%P(NODFI),STAT=IERROR11)
          ENDIF 
        ENDIF
        IF(IDTMINS == 2) THEN
         IF(ASSOCIATED(NODNXFI(NIN)%P)) DEALLOCATE(NODNXFI(NIN)%P)
         ALLOCATE(NODNXFI(NIN)%P(NODFI),STAT=IERROR12)
         IF(ASSOCIATED(NODAMSFI(NIN)%P)) DEALLOCATE(NODAMSFI(NIN)%P)
         ALLOCATE(NODAMSFI(NIN)%P(NODFI),STAT=IERROR13)
         IF(ASSOCIATED(PROCAMSFI(NIN)%P)) DEALLOCATE(PROCAMSFI(NIN)%P)
         ALLOCATE(PROCAMSFI(NIN)%P(NODFI),STAT=IERROR14)
        ELSEIF(IDTMINS_INT /= 0) THEN
         IF(ASSOCIATED(NODAMSFI(NIN)%P)) DEALLOCATE(NODAMSFI(NIN)%P)
         ALLOCATE(NODAMSFI(NIN)%P(NODFI),STAT=IERROR13)
         IF(ASSOCIATED(PROCAMSFI(NIN)%P)) DEALLOCATE(PROCAMSFI(NIN)%P)
         ALLOCATE(PROCAMSFI(NIN)%P(NODFI),STAT=IERROR14)
        ENDIF 
        IF(IGAP.NE.0) THEN
          IF(ASSOCIATED(GAPFI(NIN)%P)) DEALLOCATE(GAPFI(NIN)%P)
          ALLOCATE(GAPFI(NIN)%P(NODFI),STAT=IERROR7)
          IF(IGAP.EQ.3) THEN
            IF(ASSOCIATED(GAP_LFI(NIN)%P)) DEALLOCATE(GAP_LFI(NIN)%P)
            ALLOCATE(GAP_LFI(NIN)%P(NODFI),STAT=IERROR7)
          ENDIF
        ENDIF
        IF(ITY.EQ.24)THEN
          IF(ASSOCIATED(IRTLM_FI(NIN)%P)) DEALLOCATE(IRTLM_FI(NIN)%P)
          ALLOCATE(IRTLM_FI(NIN)%P(2,NODFI),STAT=IERROR15)

          IF(ASSOCIATED(TIME_SFI(NIN)%P)) DEALLOCATE(TIME_SFI(NIN)%P)
          ALLOCATE(TIME_SFI(NIN)%P(NODFI),STAT=IERROR16)

          IF(ASSOCIATED(SECND_FRFI(NIN)%P)) DEALLOCATE(SECND_FRFI(NIN)%P)
          ALLOCATE(SECND_FRFI(NIN)%P(6,NODFI),STAT=IERROR16)

          IF(ASSOCIATED(PENE_OLDFI(NIN)%P))DEALLOCATE(PENE_OLDFI(NIN)%P)
          ALLOCATE(PENE_OLDFI(NIN)%P(5,NODFI),STAT=IERROR16)

          IF(ASSOCIATED(STIF_OLDFI(NIN)%P))DEALLOCATE(STIF_OLDFI(NIN)%P)
          ALLOCATE(STIF_OLDFI(NIN)%P(2,NODFI),STAT=IERROR16)
	 	
          IF(ASSOCIATED(ICONT_I_FI(NIN)%P))DEALLOCATE(ICONT_I_FI(NIN)%P)
          ALLOCATE(ICONT_I_FI(NIN)%P(NODFI),STAT=IERROR16)
        ENDIF
c
        IF(ITY.EQ.7) THEN
          IF(INTFRIC > 0 ) THEN
           IF(ASSOCIATED(IPARTFRICSFI(NIN)%P)) DEALLOCATE(IPARTFRICSFI(NIN)%P)
           ALLOCATE(IPARTFRICSFI(NIN)%P(NODFI),STAT=IERROR0)
          ENDIF
        ENDIF
C
        ! ----------------------
        ! /TYPE18 + /LAW 151
        IF( MULTI_FVM%IS_INT18_LAW151.AND.IPARIT/=0 ) THEN
            ! -----------
            ! check if the present interface is a TYPE18+LAW151
            IF( MULTI_FVM%INT18_GLOBAL_LIST(NIN) ) THEN
                IF( ALLOCATED( MULTI_FVM%R_AFI(NIN)%R_FORCE_INT ) ) DEALLOCATE( MULTI_FVM%R_AFI(NIN)%R_FORCE_INT )
                MULTI_FVM%R_AFI(NIN)%NODFI = NODFI
                ALLOCATE( MULTI_FVM%R_AFI(NIN)%R_FORCE_INT(3,6,NODFI*NTHREAD) )
                MULTI_FVM%R_AFI(NIN)%R_FORCE_INT(1:3,1:6,1:NODFI*NTHREAD) = 0d+00
            ENDIF
        ENDIF
        ! ----------------------

C
        IF(IERROR1+IERROR2+IERROR3+IERROR4+IERROR5+
     +     IERROR6+IERROR7+IERROR8 + IERROR9 + IERROR0 + 
     +     IERROR11+IERROR12+IERROR13+IERROR14+IERROR15+
     +     IERROR16+IERROR17.NE. 0) THEN
          CALL ANCMSG(MSGID=20,ANMODE=ANINFO)
          CALL ARRET(2)
        ENDIF
C
C Compactage des candidats
C
        IDEB = 0
        NN2 = 0  

        JDEB = 0
	
        DO P = 1, NSPMD
          NN = 0
          OLDNSNR = NSNFI(NIN)%P(P)	    
	    
          IF(OLDNSNR.NE.0) THEN
	   
	    ALLOCATE(IAUX(OLDNSNR),STAT=IERROR17)
            IF(IERROR17.NE.0) THEN
              CALL ANCMSG(MSGID=20,ANMODE=ANINFO)
              CALL ARRET(2)
            ENDIF
	   	   
            NNP = NN2     

            DO I = 1, OLDNSNR
              IF(IREM(1,I+IDEB).LT.0) THEN	    
                NN = NN + 1
                IAUX(NN) = I
              ENDIF
            ENDDO		

c general case 
#include      "vectorize.inc"		
            DO J = 1, NN
	      I = IAUX(J)	    	
              INDEX(I+IDEB) = NN2+J
              XFI(NIN)%P(1,NN2+J) = XREM(1,I+IDEB)
              XFI(NIN)%P(2,NN2+J) = XREM(2,I+IDEB)
              XFI(NIN)%P(3,NN2+J) = XREM(3,I+IDEB)
              VFI(NIN)%P(1,NN2+J) = XREM(4,I+IDEB)
              VFI(NIN)%P(2,NN2+J) = XREM(5,I+IDEB)
              VFI(NIN)%P(3,NN2+J) = XREM(6,I+IDEB)
              MSFI(NIN)%P(NN2+J)  = XREM(7,I+IDEB)
              STIFI(NIN)%P(NN2+J) = XREM(8,I+IDEB)
              NSVFI(NIN)%P(NN2+J) = -IREM(1,I+IDEB)
              ITAFI(NIN)%P(NN2+J) = IREM(2,I+IDEB)
              KINFI(NIN)%P(NN2+J) = IREM(3,I+IDEB)
              PMAINFI(NIN)%P(NN2+J) = P 

            
              !ignore specifics IREM and XREM indexes for INT24 sorting
              !IGAPXREMP = IREM(4,I+IDEB)
              !I24XREMP  = IREM(5,I+IDEB)
              !I24IREMP  = IREM(6,I+IDEB)
            ENDDO
	    
c shift for real variables (prepare for next setting)	    
            RSHIFT = 9
c shift for integer variables (prepare for next setting)  	    
            ISHIFT = 7
	    
c IGAP=1 or IGAP=2	
            IF(IGAP.EQ.1 .OR. IGAP.EQ.2)THEN 	    
#include      "vectorize.inc"	      
              DO J = 1, NN
	        I = IAUX(J)
                GAPFI(NIN)%P(NN2+J) = XREM(RSHIFT,I+IDEB)
              ENDDO   
              RSHIFT = RSHIFT + 1 		              	    
c IGAP=3	   
            ELSEIF(IGAP.EQ.3)THEN
#include      "vectorize.inc"	      	 	         
              DO J = 1, NN	       
	        I = IAUX(J)
	        GAPFI(NIN)%P(NN2+J)   = XREM(RSHIFT,I+IDEB)	  
                GAP_LFI(NIN)%P(NN2+J) = XREM(RSHIFT+1,I+IDEB)
              ENDDO
	      RSHIFT = RSHIFT + 2
         ENDIF	
	        
C thermic
            IF(INTTH.GT.0)THEN    
#include      "vectorize.inc"	      
              DO J = 1, NN
	        I = IAUX(J)
                TEMPFI(NIN)%P(NN2+J)  = XREM(RSHIFT,I+IDEB)
                AREASFI(NIN)%P(NN2+J) = XREM(RSHIFT+1,I+IDEB)
                MATSFI(NIN)%P(NN2+J)  = IREM(ISHIFT,I+IDEB)		  
              ENDDO
	      RSHIFT = RSHIFT + 2
	      ISHIFT = ISHIFT + 1	      
         ENDIF

C Friction model
            IF(INTFRIC.GT.0)THEN    
#include      "vectorize.inc"	      
              DO J = 1, NN
	        I = IAUX(J)
                IPARTFRICSFI(NIN)%P(NN2+J)  = IREM(ISHIFT,I+IDEB)		  
              ENDDO
	      ISHIFT = ISHIFT + 1	      
         ENDIF
	    
C -- IDTMINS==2	    
            IF(IDTMINS.EQ.2)THEN    
#include      "vectorize.inc"	      
              DO J = 1, NN
	        I = IAUX(J)
                NODNXFI(NIN)%P(NN2+J)   = IREM(ISHIFT,I+IDEB)
                NODAMSFI(NIN)%P(NN2+J)  = IREM(ISHIFT+1,I+IDEB)
                PROCAMSFI(NIN)%P(NN2+J) = P		 
              ENDDO
	      ISHIFT = ISHIFT + 2
	      
C -- IDTMINS_INT /= 0	      
            ELSEIF(IDTMINS_INT.NE.0)THEN	    
#include      "vectorize.inc"	      	     
              DO J = 1, NN
	        I = IAUX(J)
		NODAMSFI(NIN)%P(NN2+J)  = IREM(ISHIFT,I+IDEB) 
		PROCAMSFI(NIN)%P(NN2+J) = P	   
              ENDDO
	      ISHIFT = ISHIFT + 1 	      
            ENDIF

c INT24
	    IF(ITY.EQ.24)THEN    
#include      "vectorize.inc"	      	    	    
              DO J = 1, NN
                I = IAUX(J)
                IRTLM_FI(NIN)%P(1,NN2+J)  =IREM(ISHIFT,I+IDEB)
                IRTLM_FI(NIN)%P(2,NN2+J)  =IREM(ISHIFT+1,I+IDEB)
                ICONT_I_FI(NIN)%P(NN2+J) = IREM(ISHIFT+2,I+IDEB)
                TIME_SFI(NIN)%P(NN2+J )   =XREM(RSHIFT,I+IDEB)
                SECND_FRFI(NIN)%P(1,NN2+J) =ZERO
                SECND_FRFI(NIN)%P(2,NN2+J) =ZERO
                SECND_FRFI(NIN)%P(3,NN2+J) =ZERO
                SECND_FRFI(NIN)%P(4,NN2+J) =XREM(RSHIFT+1,I+IDEB)
                SECND_FRFI(NIN)%P(5,NN2+J) =XREM(RSHIFT+2,I+IDEB)
                SECND_FRFI(NIN)%P(6,NN2+J) =XREM(RSHIFT+3,I+IDEB)
                PENE_OLDFI(NIN)%P(1,NN2+J)=ZERO
                STIF_OLDFI(NIN)%P(1,NN2+J)=ZERO
                PENE_OLDFI(NIN)%P(2,NN2+J)=XREM(RSHIFT+4,I+IDEB)
                STIF_OLDFI(NIN)%P(2,NN2+J)=XREM(RSHIFT+5,I+IDEB)
C
C We Store PENE_OLD(3 in PENE_OLD(4 during sorting
                PENE_OLDFI(NIN)%P(4,NN2+J)=XREM(RSHIFT+6,I+IDEB)
                PENE_OLDFI(NIN)%P(5,NN2+J)=XREM(RSHIFT+7,I+IDEB)
              ENDDO
	      RSHIFT = RSHIFT + 8 
	      ISHIFT = ISHIFT + 3
              IF (ILEV==2) ISHIFT = ISHIFT + 1          	      	      
	    ENDIF
	    
          
          NN2 = NN2 + NN	            
          IDEB = IDEB + OLDNSNR    
          NSNFI(NIN)%P(P) = NN2-NNP
	   
	    DEALLOCATE(IAUX)
	   	    
          ENDIF !IF(OLDNSNR.NE.0) 
	   
        ENDDO  ! end do NSPMD        	
	
        LSKYFI = NN2*MULTIMAX
Cel nsnr nouveau utile pour inacti
        NSNR = NN2
      ENDIF
C
C Deallocation de XREM IREM
C
      IF(ALLOCATED(XREM)) DEALLOCATE(XREM)
      IF(ALLOCATED(IREM)) DEALLOCATE(IREM)

C
      IERROR1=0 
      IERROR2=0 
      IERROR3=0 
      IERROR4=0 
      IF(INTTH == 0 ) THEN
C
C Allocation Parith/OFF
C
        IF(IPARIT.EQ.0) THEN
         
          IF(ASSOCIATED(AFI(NIN)%P)) THEN
              DEALLOCATE(AFI(NIN)%P)
              NULLIFY(AFI(NIN)%P)
          ENDIF
          IF(ASSOCIATED(STNFI(NIN)%P)) THEN
              DEALLOCATE(STNFI(NIN)%P)
              NULLIFY(AFI(NIN)%P) 
          ENDIF

          IF(NODFI.GT.0)ALLOCATE(AFI(NIN)%P(3,NODFI*NTHREAD),STAT=IERROR1)
          IF(NODFI.GT.0)ALLOCATE(STNFI(NIN)%P(NODFI*NTHREAD),STAT=IERROR2)
C Init a 0
          DO I = 1, NODFI*NTHREAD
            AFI(NIN)%P(1,I) = ZERO
            AFI(NIN)%P(2,I) = ZERO
            AFI(NIN)%P(3,I) = ZERO
            STNFI(NIN)%P(I) = ZERO
          ENDDO
C
          IF(KDTINT.NE.0)THEN
            IF(ASSOCIATED(VSCFI(NIN)%P)) DEALLOCATE(VSCFI(NIN)%P)
            IF(NODFI.GT.0)ALLOCATE(VSCFI(NIN)%P(NODFI*NTHREAD),STAT=IERROR3)
C Init a 0
            DO I = 1, NODFI*NTHREAD
              VSCFI(NIN)%P(I) = ZERO
            ENDDO
          ENDIF
          NLSKYFI(NIN) = NODFI
C
        ELSE
C
C Allocation Parith/ON Done in upgrade_rem_slv
C
        ENDIF
      ELSE
C
C Allocation Parith/OFF
C
        IF(IPARIT.EQ.0) THEN
          IF(ASSOCIATED(AFI(NIN)%P)) DEALLOCATE(AFI(NIN)%P)
          IF(ASSOCIATED(STNFI(NIN)%P)) DEALLOCATE(STNFI(NIN)%P)
          IF(ASSOCIATED(FTHEFI(NIN)%P)) DEALLOCATE(FTHEFI(NIN)%P)
          IF(NODFI.GT.0)ALLOCATE(AFI(NIN)%P(3,NODFI*NTHREAD),STAT=IERROR1)
          IF(NODFI.GT.0)ALLOCATE(STNFI(NIN)%P(NODFI*NTHREAD),STAT=IERROR2)
          IF(NODFI.GT.0)ALLOCATE(FTHEFI(NIN)%P(NODFI*NTHREAD),STAT=IERROR3)
C
          IF(NODADT_THERM ==1) THEN
            IF(ASSOCIATED(CONDNFI(NIN)%P)) DEALLOCATE(CONDNFI(NIN)%P)
            IF(NODFI.GT.0.AND.NODADT_THERM ==1)ALLOCATE(CONDNFI(NIN)%P(NODFI*NTHREAD),STAT=IERROR4)
          ENDIF
C

        
C Init a 0

          DO I = 1, NODFI*NTHREAD
            AFI(NIN)%P(1,I) = ZERO
            AFI(NIN)%P(2,I) = ZERO
            AFI(NIN)%P(3,I) = ZERO
            STNFI(NIN)%P(I) = ZERO
            FTHEFI(NIN)%P(I) = ZERO
          ENDDO
          IF(NODADT_THERM ==1) THEN
            DO I = 1, NODFI
               CONDNFI(NIN)%P(I) = ZERO
            ENDDO
          ENDIF
C
          IF(KDTINT.NE.0)THEN
            IF(ASSOCIATED(VSCFI(NIN)%P)) DEALLOCATE(VSCFI(NIN)%P)
            IF(NODFI.GT.0)ALLOCATE(VSCFI(NIN)%P(NODFI),STAT=IERROR4)
C Init a 0
            DO I = 1, NODFI
              VSCFI(NIN)%P(I) = ZERO
            ENDDO
          ENDIF
C
        ELSE
C
C Allocation Parith/ON Upgrade_rem_slv
C


        ENDIF        
      ENDIF    
C
      IF(IERROR1+IERROR2+IERROR3+IERROR4.NE.0) THEN
        CALL ANCMSG(MSGID=20,ANMODE=ANINFO)
        CALL ARRET(2)
      ENDIF
C
C allocations conditionnelles output pression
C
      IF(ANIM_V(12)+OUTP_V(12)+H3D_DATA%N_VECT_PCONT .GT.0)THEN
        IF(ASSOCIATED(FNCONTI(NIN)%P)) DEALLOCATE(FNCONTI(NIN)%P)
        IF(ASSOCIATED(FTCONTI(NIN)%P)) DEALLOCATE(FTCONTI(NIN)%P)
        ALLOCATE(FNCONTI(NIN)%P(3,NODFI),STAT=IERROR1)
        ALLOCATE(FTCONTI(NIN)%P(3,NODFI),STAT=IERROR2)
        IF(IERROR1+IERROR2.NE.0) THEN
          CALL ANCMSG(MSGID=20,ANMODE=ANINFO)
          CALL ARRET(2)
        ELSE
          DO J = 1, NODFI
            FNCONTI(NIN)%P(1,J)=ZERO
            FNCONTI(NIN)%P(2,J)=ZERO
            FNCONTI(NIN)%P(3,J)=ZERO
            FTCONTI(NIN)%P(1,J)=ZERO
            FTCONTI(NIN)%P(2,J)=ZERO
            FTCONTI(NIN)%P(3,J)=ZERO
          END DO                  
        END IF  		      
      END IF
C
C
C Renumerotation des candidats
C
      DO I = 1, I_STOK
        N = CAND_N(I)
        NN = N-NSN
        IF(NN.GT.0)THEN
          CAND_N(I) = INDEX(NN)+NSN
        ENDIF
      ENDDO
C
#endif
      RETURN
      END

C END TRI7GAT
Chd|====================================================================
Chd|  SPMD_TRI10BOX                 source/mpi/interfaces/spmd_int.F
Chd|-- called by -----------
Chd|        I10MAIN_TRI                   source/interfaces/intsort/i10main_tri.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        ARRET                         source/system/arret.F         
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        TRI7BOX                       share/modules/tri7box.F       
Chd|====================================================================
      SUBROUTINE SPMD_TRI10BOX(NSV     ,NSN     ,X      ,V   ,MS     ,
     2                         BMINMAL ,WEIGHT  ,STIFN  ,NIN ,ISENDTO,
     3                         IRCVFROM,IAD_ELEM,FR_ELEM,NSNR,IGAP   ,
     4                         GAP_S   ,NSNFIOLD,NODNX_SMS,ITAB,ITIED)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE TRI7BOX
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
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "scr02_c.inc"
#include      "scr18_c.inc"
#include      "sms_c.inc"
#include      "task_c.inc"
#include      "units_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NIN, NSN, IGAP,
     .        NSNFIOLD(*), NSV(*), WEIGHT(*), ITAB(*),
     .        ISENDTO(NINTER+1,*), IRCVFROM(NINTER+1,*),
     .        IAD_ELEM(2,*), FR_ELEM(*), NODNX_SMS(*),NSNR
      INTEGER, INTENT(IN) :: ITIED
      my_real
     . X(3,*), V(3,*), MS(*), BMINMAL(*), STIFN(*), GAP_S(*)
      
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
#ifdef MPI
      INTEGER MSGTYP,INFO,I,NOD, DT_CST, LOC_PROC,P,IDEB,
     .        J, L, BUFSIZ, LEN, NB, IERROR1,
     .        STATUS(MPI_STATUS_SIZE),IERROR,REQ_SB(NSPMD),
     .        REQ_RB(NSPMD),KK,NBIRECV,IRINDEXI(NSPMD),
     .        REQ_RD(NSPMD),REQ_SD(NSPMD),REQ_SD2(NSPMD),
     .        INDEXI,ISINDEXI(NSPMD),INDEX(NUMNOD),NBOX(NSPMD),
     .        MSGOFF, MSGOFF2, MSGOFF3, MSGOFF4,
     .        REQ_RD2(NSPMD), REQ_SD3(NSPMD),
     .        RSIZ, ISIZ,RSHIFT,ISHIFT,LEN2,L2
               
      DATA MSGOFF/6005/
      DATA MSGOFF2/6006/
      DATA MSGOFF3/6007/
      DATA MSGOFF4/6008/      
     
      my_real BMINMA(6,NSPMD), RATIO
      TYPE(real_pointer), DIMENSION(NSPMD) :: RBUF
      TYPE(int_pointer) , DIMENSION(NSPMD) :: IBUF 

C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
C
C Sauvegarde valeur ancienne des nsn frontieres
C
      !WRITE(6,*) __FILE__,__LINE__
      DO P = 1, NSPMD
        NSNFIOLD(P) = NSNFI(NIN)%P(P)
      END DO
C
      LOC_PROC = ISPMD + 1
C
Cel boite minmax pour le tri provenant de i10buce BMINMA
C
      IF(IRCVFROM(NIN,LOC_PROC).EQ.0.AND.
     .   ISENDTO(NIN,LOC_PROC).EQ.0) RETURN
      BMINMA(1,LOC_PROC) = BMINMAL(1)
      BMINMA(2,LOC_PROC) = BMINMAL(2)
      BMINMA(3,LOC_PROC) = BMINMAL(3)
      BMINMA(4,LOC_PROC) = BMINMAL(4)
      BMINMA(5,LOC_PROC) = BMINMAL(5)
      BMINMA(6,LOC_PROC) = BMINMAL(6)
C
Cel envoi boite
C
      IF(IRCVFROM(NIN,LOC_PROC).NE.0) THEN
        DO P = 1, NSPMD
          IF(ISENDTO(NIN,P).NE.0) THEN
            IF(P.NE.LOC_PROC) THEN
              MSGTYP = MSGOFF
              CALL MPI_ISEND(
     .          BMINMA(1,LOC_PROC),6        ,REAL  ,IT_SPMD(P),MSGTYP,
     .          MPI_COMM_WORLD    ,REQ_SB(P),IERROR)
            ENDIF
          ENDIF
        ENDDO
      ENDIF
C
Cel reception des boites min-max
C
      IF(ISENDTO(NIN,LOC_PROC).NE.0) THEN
        NBIRECV=0
        DO P = 1, NSPMD
          IF(IRCVFROM(NIN,P).NE.0) THEN
            IF(LOC_PROC.NE.P) THEN
              MSGTYP = MSGOFF
              NBIRECV=NBIRECV+1
              IRINDEXI(NBIRECV)=P
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
c general case
      RSIZ = 8    
      ISIZ = 2

c IGAP > 0      
      IF(IGAP.GT.0) THEN
        RSIZ = RSIZ + 1
      ENDIF

c IDTMINS = 2      
      IF(IDTMINS == 2)THEN
        ISIZ = ISIZ + 2
c IDTMINS_INT /= 0	
      ELSEIF(IDTMINS_INT/=0)THEN
        ISIZ = ISIZ + 1
      END IF
      
      IDEB = 1
      IF(ISENDTO(NIN,LOC_PROC).NE.0) THEN
        DO KK = 1, NBIRECV
          CALL MPI_WAITANY(NBIRECV,REQ_RB,INDEXI,STATUS,IERROR)
          P=IRINDEXI(INDEXI)
C Traitement special sur d.d. ne consever que les noeuds internes
          DO J = IAD_ELEM(1,P), IAD_ELEM(1,P+1)-1
            NOD = FR_ELEM(J)
C weight < 0 temporairement pour ne conserver que les noeuds non frontiere
            WEIGHT(NOD) = WEIGHT(NOD)*(-1)
          ENDDO
C
          L = IDEB
          NBOX(P) = 0
          NB = 0
          DO I=1,NSN
            NOD = NSV(I)
            IF(WEIGHT(NOD).EQ.1)THEN
             IF(CANDF_SI(NIN)%P(I)/=0) THEN
               NB = NB + 1
               INDEX(NB) = I
               !WRITE(6,*) "Force send of",ITAB(NOD),"TO",P-1
             ELSE
             IF(STIFN(I).GT.ZERO)THEN
              IF(X(1,NOD).LE.BMINMA(1,P)) THEN
               IF(X(1,NOD).GE.BMINMA(4,P)) THEN
                IF(X(2,NOD).LE.BMINMA(2,P)) THEN
                 IF(X(2,NOD).GE.BMINMA(5,P)) THEN
                  IF(X(3,NOD).LE.BMINMA(3,P)) THEN
                   IF(X(3,NOD).GE.BMINMA(6,P)) THEN
                     NB = NB + 1
                     INDEX(NB) = I
                   ENDIF
                  ENDIF
                 ENDIF
                ENDIF
               ENDIF
              ENDIF
             ENDIF
            ENDIF
            ENDIF
          ENDDO
          NBOX(P) = NB
C
          DO J = IAD_ELEM(1,P), IAD_ELEM(1,P+1)-1
            NOD = FR_ELEM(J)
C remise de weight > 0
            WEIGHT(NOD) = WEIGHT(NOD)*(-1)
          ENDDO
C
C Envoi taille msg
C
          MSGTYP = MSGOFF2 
          CALL MPI_ISEND(NBOX(P),1,MPI_INTEGER,IT_SPMD(P),MSGTYP,
     .                 MPI_COMM_WORLD,REQ_SD(P),IERROR)
C
C Alloc buffer
C
          IF (NB.GT.0) THEN
            ALLOCATE(RBUF(P)%P(RSIZ*NB),STAT=IERROR)
	    ALLOCATE(IBUF(P)%P(ISIZ*NB),STAT=IERROR)
            IF(IERROR.NE.0) THEN
              CALL ANCMSG(MSGID=20,ANMODE=ANINFO)
              CALL ARRET(2)
            ENDIF
            L = 0
	    L2 = 0 

c general case
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
              L = L + RSIZ
	      L2 = L2 + ISIZ
            END DO

c shift for real variables	    
	    RSHIFT = 8
c shift for integer variables    
	    ISHIFT = 2

c specific cases
c IGAP=1 or IGAP=2		 
            IF(IGAP.GT.0)THEN
	      L = 0	    
	      RSHIFT = RSHIFT + 1
#include     "vectorize.inc"	       
              DO J = 1, NB
                I = INDEX(J)   
                RBUF(P)%p(L+RSHIFT)= GAP_S(I)
                L = L + RSIZ  	 
	      ENDDO
	    ENDIF
 
C -- IDTMINS==2
            IF(IDTMINS==2)THEN
               L2 = 0
	       ISHIFT = ISHIFT + 1
#include      "vectorize.inc"	       	       
               DO J = 1, NB
                 I = INDEX(J)
                 NOD = NSV(I)
                 IBUF(P)%p(L2+ISHIFT)  = NODNX_SMS(NOD)
                 IBUF(P)%p(L2+ISHIFT+1)= NOD
                 L2 = L2 + ISIZ
               END DO
	       
C -- IDTMINS_INT /= 0	       
            ELSEIF(IDTMINS_INT.NE.0)THEN
              L2 = 0	    
	      ISHIFT = ISHIFT + 1      
#include      "vectorize.inc"	      
              DO J = 1, NB
                I = INDEX(J)
                NOD = NSV(I)
		IBUF(P)%p(L2+ISHIFT)= NOD
                L2 = L2 + ISIZ
              END DO
	    ENDIF

            MSGTYP = MSGOFF3 
            CALL MPI_ISEND(
     1        RBUF(P)%P(1),NB*RSIZ,REAL,IT_SPMD(P),
     2        MSGTYP,MPI_COMM_WORLD,REQ_SD2(P),ierror)
     
            MSGTYP = MSGOFF4
            CALL MPI_ISEND(
     1        IBUF(P)%P(1),NB*ISIZ,MPI_INTEGER,IT_SPMD(P),MSGTYP,
     2        MPI_COMM_WORLD,REQ_SD3(P),ierror) 
         
          ENDIF
        ENDDO
      ENDIF
C
Cel reception  des donnees XREM
C
      IF(IRCVFROM(NIN,LOC_PROC).NE.0) THEN
        NSNR = 0
        L=0
        DO P = 1, NSPMD
          NSNFI(NIN)%P(P) = 0
          IF(ISENDTO(NIN,P).NE.0) THEN
            IF(LOC_PROC.NE.P) THEN
              MSGTYP = MSGOFF2 
              CALL MPI_RECV(NSNFI(NIN)%P(P),1,MPI_INTEGER,IT_SPMD(P),
     .                      MSGTYP,MPI_COMM_WORLD,STATUS,IERROR)
              IF(NSNFI(NIN)%P(P).GT.0) THEN
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
        IF(NSNR.GT.0) THEN
          ALLOCATE(XREM(RSIZ,NSNR),STAT=IERROR)
	  
          ALLOCATE(IREM(ISIZ,NSNR),STAT=IERROR1)
          IERROR=IERROR+IERROR1
	  
          IF(IERROR.NE.0) THEN
            CALL ANCMSG(MSGID=20,ANMODE=ANINFO)
            CALL ARRET(2)
          ENDIF
	  
          IDEB = 1
          DO L = 1, NBIRECV
            P = ISINDEXI(L)
            LEN = NSNFI(NIN)%P(P)*RSIZ
            MSGTYP = MSGOFF3 
	    
            CALL MPI_IRECV(
     1        XREM(1,IDEB),LEN,REAL,IT_SPMD(P),
     2        MSGTYP,MPI_COMM_WORLD,REQ_RD(L),IERROR)
     
            LEN2 = NSNFI(NIN)%P(P)*ISIZ
            MSGTYP = MSGOFF4 
            CALL MPI_IRECV(
     1        IREM(1,IDEB),LEN2,MPI_INTEGER,IT_SPMD(P),
     2        MSGTYP,MPI_COMM_WORLD,REQ_RD2(L),IERROR) 
         
            IDEB = IDEB + NSNFI(NIN)%P(P)
	    
          ENDDO
          DO L = 1, NBIRECV
            CALL MPI_WAITANY(NBIRECV,REQ_RD,INDEXI,STATUS,IERROR)
	    CALL MPI_WAITANY(NBIRECV,REQ_RD2,INDEXI,STATUS,IERROR)
          ENDDO

        ENDIF
      ENDIF
C
      IF(IRCVFROM(NIN,LOC_PROC).NE.0) THEN
        DO P = 1, NSPMD
          IF(ISENDTO(NIN,P).NE.0) THEN
            IF(P.NE.LOC_PROC) THEN
              CALL MPI_WAIT(REQ_SB(P),STATUS,IERROR)
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
              IF(NBOX(P).NE.0) THEN
                CALL MPI_WAIT(REQ_SD2(P),STATUS,IERROR)
                DEALLOCATE(RBUF(P)%p)
	        CALL MPI_WAIT(REQ_SD3(P),STATUS,IERROR)
                DEALLOCATE(IBUF(P)%p)		
              END IF
            ENDIF
          ENDIF
        ENDDO
      ENDIF
C
#endif
      RETURN
      END
C      
C END SPMD_TRI10BOX
Chd|====================================================================
Chd|  SPMD_TRI10GAT                 source/mpi/interfaces/spmd_int.F
Chd|-- called by -----------
Chd|        I10MAIN_TRI                   source/interfaces/intsort/i10main_tri.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        ARRET                         source/system/arret.F         
Chd|        H3D_MOD                       share/modules/h3d_mod.F       
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        TRI7BOX                       share/modules/tri7box.F       
Chd|====================================================================
      SUBROUTINE SPMD_TRI10GAT(RESULT,NSN ,CAND_N ,I_STOK,NIN,
     2                        IGAP  ,NSNR,MULTIMP,ITY,INTTH,H3D_DATA)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE TRI7BOX
      USE MESSAGE_MOD
      USE H3D_MOD
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
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "task_c.inc"
#include      "units_c.inc"
#include      "scr14_c.inc"
#include      "scr16_c.inc"
#include      "scr18_c.inc"
#include      "parit_c.inc"
#include      "spmd_c.inc"
#include      "sms_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER RESULT, NIN, NSN, I_STOK, IGAP, NSNR, MULTIMP, ITY,
     .        CAND_N(*),INTTH
      TYPE(H3D_DATABASE) :: H3D_DATA
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
#ifdef MPI
      INTEGER OLDNSNR,NODFI,NNP,LSKYFI,
     .        NOD, LOC_PROC, I, N, NN, P, IDEB, J, K,
     .        IERROR1,IERROR2,IERROR3,IERROR4,IERROR5,IERROR6,IERROR7,
     .        IERROR8,IERROR9,IERROR10,IERROR11,IERROR12,IERROR13,
     .        INDEX(NSNR),NN2,RSHIFT,ISHIFT
     
      INTEGER, DIMENSION(:), ALLOCATABLE :: IAUX
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
      LOC_PROC = ISPMD + 1
C
C
      NODFI = 0
      LSKYFI= 0
      IF(RESULT.EQ.0) THEN
C
C Reperage des candidats
C
        NODFI = 0
        DO I = 1, I_STOK
          N = CAND_N(I)
          NN = N-NSN
          IF(NN.GT.0)THEN
            IF(IREM(1,NN).GT.0)THEN
              NODFI = NODFI + 1
              IREM(1,NN) = -IREM(1,NN)
            ENDIF
          ENDIF
        ENDDO
C
C Allocation des tableaux de frontieres interfaces
C
        IERROR1 = 0
        IERROR2 = 0
        IERROR3 = 0
        IERROR4 = 0
        IERROR5 = 0
        IERROR6 = 0
        IERROR7 = 0
        IERROR8 = 0
        IERROR9 = 0
        IERROR10 = 0
        IERROR11 = 0
        IERROR12 = 0
        IERROR13 = 0	

        IF(ASSOCIATED(NSVFI(NIN)%P)) DEALLOCATE(NSVFI(NIN)%P)
        ALLOCATE(NSVFI(NIN)%P(NODFI),STAT=IERROR1)
        IF(ASSOCIATED(XFI(NIN)%P)) DEALLOCATE(XFI(NIN)%P)
        ALLOCATE(XFI(NIN)%P(3,NODFI),STAT=IERROR2)
        IF(ASSOCIATED(VFI(NIN)%P)) DEALLOCATE(VFI(NIN)%P)
        ALLOCATE(VFI(NIN)%P(3,NODFI),STAT=IERROR3)
        IF(ASSOCIATED(MSFI(NIN)%P)) DEALLOCATE(MSFI(NIN)%P)
        ALLOCATE(MSFI(NIN)%P(NODFI),STAT=IERROR4)
        IF(ASSOCIATED(STIFI(NIN)%P)) DEALLOCATE(STIFI(NIN)%P)
        ALLOCATE(STIFI(NIN)%P(NODFI),STAT=IERROR5)
        IF(ASSOCIATED(ITAFI(NIN)%P)) DEALLOCATE(ITAFI(NIN)%P)
        ALLOCATE(ITAFI(NIN)%P(NODFI),STAT=IERROR6)
        IF(IDTMINS == 2) THEN
         IF(ASSOCIATED(NODNXFI(NIN)%P)) DEALLOCATE(NODNXFI(NIN)%P)
         ALLOCATE(NODNXFI(NIN)%P(NODFI),STAT=IERROR7)
         IF(ASSOCIATED(NODAMSFI(NIN)%P)) DEALLOCATE(NODAMSFI(NIN)%P)
         ALLOCATE(NODAMSFI(NIN)%P(NODFI),STAT=IERROR8)
         IF(ASSOCIATED(PROCAMSFI(NIN)%P)) DEALLOCATE(PROCAMSFI(NIN)%P)
         ALLOCATE(PROCAMSFI(NIN)%P(NODFI),STAT=IERROR9)
        ELSEIF(IDTMINS_INT /= 0) THEN
         IF(ASSOCIATED(NODAMSFI(NIN)%P)) DEALLOCATE(NODAMSFI(NIN)%P)
         ALLOCATE(NODAMSFI(NIN)%P(NODFI),STAT=IERROR10)
         IF(ASSOCIATED(PROCAMSFI(NIN)%P)) DEALLOCATE(PROCAMSFI(NIN)%P)
         ALLOCATE(PROCAMSFI(NIN)%P(NODFI),STAT=IERROR11)
        ENDIF 
        IF(IGAP.NE.0) THEN
          IF(ASSOCIATED(GAPFI(NIN)%P)) DEALLOCATE(GAPFI(NIN)%P)
          ALLOCATE(GAPFI(NIN)%P(NODFI),STAT=IERROR12)
          IF(IGAP.EQ.3) THEN
            IF(ASSOCIATED(GAP_LFI(NIN)%P)) DEALLOCATE(GAP_LFI(NIN)%P)
            ALLOCATE(GAP_LFI(NIN)%P(NODFI),STAT=IERROR12)
          ENDIF
        ENDIF
C
        IF((IERROR1+IERROR2+IERROR3+IERROR4+IERROR5+
     +     IERROR6+IERROR7+IERROR8 + IERROR9 + IERROR10 + 
     +     IERROR11+IERROR12)>0) THEN
          CALL ANCMSG(MSGID=20,ANMODE=ANINFO)
          CALL ARRET(2)
        ENDIF
C
C Compactage des candidats
C
        IDEB = 0
        NN2 = 0      
  
          DO P = 1, NSPMD
	   NN = 0
           OLDNSNR = NSNFI(NIN)%P(P)	    
	    
           IF(OLDNSNR.NE.0) THEN
	   
	    ALLOCATE(IAUX(OLDNSNR),STAT=IERROR13)
            IF(IERROR13.NE.0) THEN
              CALL ANCMSG(MSGID=20,ANMODE=ANINFO)
              CALL ARRET(2)
            ENDIF
	   	   
            NNP = NN2     

            DO I = 1, OLDNSNR
              IF(IREM(1,I+IDEB).LT.0) THEN	    
	        NN = NN + 1
		IAUX(NN) = I
              ENDIF
            ENDDO		

c general case 
#include      "vectorize.inc"		
            DO J = 1, NN
	      I = IAUX(J)	    	
              INDEX(I+IDEB) = NN2+J
              XFI(NIN)%P(1,NN2+J) = XREM(1,I+IDEB)
              XFI(NIN)%P(2,NN2+J) = XREM(2,I+IDEB)
              XFI(NIN)%P(3,NN2+J) = XREM(3,I+IDEB)
              VFI(NIN)%P(1,NN2+J) = XREM(4,I+IDEB)
              VFI(NIN)%P(2,NN2+J) = XREM(5,I+IDEB)
              VFI(NIN)%P(3,NN2+J) = XREM(6,I+IDEB)
              MSFI(NIN)%P(NN2+J)  = XREM(7,I+IDEB)
              STIFI(NIN)%P(NN2+J) = XREM(8,I+IDEB)
              NSVFI(NIN)%P(NN2+J) = -IREM(1,I+IDEB)
              ITAFI(NIN)%P(NN2+J) = IREM(2,I+IDEB)
            ENDDO
	    
	    RSHIFT = 8
	    ISHIFT = 2
	    
c IGAP=1 or IGAP=2	
            IF(IGAP.GT.0)THEN 	    
              RSHIFT = RSHIFT + 1 
#include      "vectorize.inc"	      
              DO J = 1, NN
	        I = IAUX(J)
                GAPFI(NIN)%P(NN2+J) = XREM(RSHIFT,I+IDEB)
              ENDDO     
	    ENDIF	    

C -- IDTMINS==2	    
            IF(IDTMINS.EQ.2)THEN    
	      ISHIFT = ISHIFT + 1 
#include      "vectorize.inc"	      
              DO J = 1, NN
	        I = IAUX(J)
                NODNXFI(NIN)%P(NN2+J)   = IREM(ISHIFT,I+IDEB)
                NODAMSFI(NIN)%P(NN2+J)  = IREM(ISHIFT+1,I+IDEB)
                PROCAMSFI(NIN)%P(NN2+J) = P		 
              ENDDO
	      
C -- IDTMINS_INT /= 0	      
            ELSEIF(IDTMINS_INT.NE.0)THEN	    
	      ISHIFT = ISHIFT + 1 
#include      "vectorize.inc"	      	     
              DO J = 1, NN
	        I = IAUX(J)
		NODAMSFI(NIN)%P(NN2+J)  = IREM(ISHIFT,I+IDEB) 
		PROCAMSFI(NIN)%P(NN2+J) = P	   
              ENDDO
            ENDIF
	    
	    NN2 = NN2 + NN	            
            IDEB = IDEB + OLDNSNR    
            NSNFI(NIN)%P(P) = NN2-NNP
	   
	    DEALLOCATE(IAUX)
	   	    
           ENDIF !IF(OLDNSNR.NE.0) 
	   
          ENDDO  ! fin do NSPMD   	
	
        LSKYFI = NN2*MULTIMAX
Cel nsnr nouveau utile pour inacti
        NSNR = NN2
      ENDIF
C
C Deallocation de XREM
C
      IF(ALLOCATED(XREM)) DEALLOCATE(XREM)
      IF(ALLOCATED(IREM)) DEALLOCATE(IREM)
C
      IERROR1=0 
      IERROR2=0 
      IERROR3=0 
      IERROR4=0 
C
C Allocation Parith/OFF
C
      IF(IPARIT.EQ.0) THEN
        IF(ASSOCIATED(AFI(NIN)%P)) DEALLOCATE(AFI(NIN)%P)
        IF(ASSOCIATED(STNFI(NIN)%P)) DEALLOCATE(STNFI(NIN)%P)
        IF(NODFI.GT.0)ALLOCATE(AFI(NIN)%P(3,NODFI*NTHREAD),STAT=IERROR1)
        IF(NODFI.GT.0)ALLOCATE(STNFI(NIN)%P(NODFI*NTHREAD),STAT=IERROR2)
C Init a 0
        DO I = 1, NODFI*NTHREAD
          AFI(NIN)%P(1,I) = ZERO
          AFI(NIN)%P(2,I) = ZERO
          AFI(NIN)%P(3,I) = ZERO
          STNFI(NIN)%P(I) = ZERO
        ENDDO
C
        IF(KDTINT.NE.0)THEN
          IF(ASSOCIATED(VSCFI(NIN)%P)) DEALLOCATE(VSCFI(NIN)%P)
          IF(NODFI.GT.0)ALLOCATE(VSCFI(NIN)%P(NODFI*NTHREAD),STAT=IERROR3)
C Init a 0
          DO I = 1, NODFI*NTHREAD
            VSCFI(NIN)%P(I) = ZERO
          ENDDO
        ENDIF
C
        NLSKYFI(NIN) = NODFI
C
      ELSE
C
C Allocation Parith/ON
C
        IF(ASSOCIATED(FSKYFI(NIN)%P)) DEALLOCATE(FSKYFI(NIN)%P)
        IF(ASSOCIATED(ISKYFI(NIN)%P)) DEALLOCATE(ISKYFI(NIN)%P)
        NLSKYFI(NIN) = LSKYFI
        IF(LSKYFI.GT.0) THEN
          ALLOCATE(ISKYFI(NIN)%P(LSKYFI),STAT=IERROR1)
          IF(KDTINT.EQ.0) THEN
            ALLOCATE(FSKYFI(NIN)%P(4,LSKYFI),STAT=IERROR2)
          ELSE
            ALLOCATE(FSKYFI(NIN)%P(5,LSKYFI),STAT=IERROR2)
          ENDIF
        ENDIF
       ENDIF            
C
      IF(IERROR1+IERROR2+IERROR3+IERROR4.NE.0) THEN
        CALL ANCMSG(MSGID=20,ANMODE=ANINFO)
        CALL ARRET(2)
      ENDIF
C
C allocations conditionnelles output pression
C
      IF(ANIM_V(12)+OUTP_V(12)+H3D_DATA%N_VECT_PCONT .GT.0)THEN
        IF(ASSOCIATED(FNCONTI(NIN)%P)) DEALLOCATE(FNCONTI(NIN)%P)
        IF(ASSOCIATED(FTCONTI(NIN)%P)) DEALLOCATE(FTCONTI(NIN)%P)
        ALLOCATE(FNCONTI(NIN)%P(3,NODFI),STAT=IERROR1)
        ALLOCATE(FTCONTI(NIN)%P(3,NODFI),STAT=IERROR2)
        IF(IERROR1+IERROR2.NE.0) THEN
          CALL ANCMSG(MSGID=20,ANMODE=ANINFO)
          CALL ARRET(2)
        ELSE
          DO J = 1, NODFI
            FNCONTI(NIN)%P(1,J)=ZERO
            FNCONTI(NIN)%P(2,J)=ZERO
            FNCONTI(NIN)%P(3,J)=ZERO
            FTCONTI(NIN)%P(1,J)=ZERO
            FTCONTI(NIN)%P(2,J)=ZERO
            FTCONTI(NIN)%P(3,J)=ZERO
          END DO                  
        END IF  		      
      END IF

C
C Renumerotation des candidats
C
      DO I = 1, I_STOK
        N = CAND_N(I)
        NN = N-NSN
        IF(NN.GT.0)THEN
          CAND_N(I) = INDEX(NN)+NSN
        ENDIF
      ENDDO
C
#endif
      RETURN
      END

C FIN TRI10GAT
Chd|====================================================================
Chd|  SPMD_TRI11VOX0                source/mpi/interfaces/spmd_int.F
Chd|-- called by -----------
Chd|        I11MAIN_TRI                   source/interfaces/intsort/i11main_tri.F
Chd|-- calls ---------------
Chd|        TRI7BOX                       share/modules/tri7box.F       
Chd|====================================================================
      SUBROUTINE SPMD_TRI11VOX0(
     1   X      ,BMINMAL ,IGAP   ,NRTM  ,STF   ,
     2   TZINF  ,IRECTM  ,GAP, GAP_M,
     3   GAPMIN,BGAPSMX,DRAD ,DGAPLOAD) 
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE TRI7BOX
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "comlock.inc"
#include      "r4r8_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "task_c.inc"
#include      "timeri_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IGAP, NRTM, IRECTM(2,NRTM)
      my_real
     .        X(3,*), BMINMAL(6),GAP_M(*),GAPMIN,BGAPSMX,
     .        STF(NRTM),
     .        TZINF,GAP
      my_real , INTENT(IN) :: DRAD,DGAPLOAD
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
      INTEGER LOC_PROC,
     .        NBX,NBY,NBZ,NEDG,M1,M2,M3,M4,
     .        IX1,IY1,IZ1,IX2,IY2,IZ2,IX,IY,IZ,I
      my_real
     .        RATIO, AAA, MARGE,
     .        XMAXB,YMAXB,ZMAXB,XMINB,YMINB,ZMINB,
     .        XMINE,YMINE,ZMINE,XMAXE,YMAXE,ZMAXE,
     .        XX1,XX2,XX3,XX4,YY1,YY2,YY3,YY4,ZZ1,ZZ2,ZZ3,ZZ4
      INTEGER TMP
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
C
C=======================================================================
C     tag des boites contenant des facettes
C     et creation des candidats
C=======================================================================

      LOC_PROC = ISPMD + 1
      !MARGE = TZINF-GAP
      MARGE =  TZINF-MAX(GAP+DGAPLOAD,DRAD)
      

      NBX = LRVOXEL
      NBY = LRVOXEL
      NBZ = LRVOXEL

      XMAXB = BMINMAL(1)
      YMAXB = BMINMAL(2)
      ZMAXB = BMINMAL(3)
      XMINB = BMINMAL(4)
      YMINB = BMINMAL(5)
      ZMINB = BMINMAL(6)

      AAA = 0
      DO NEDG=1,NRTM
C on ne retient pas les facettes detruites
        IF(STF(NEDG) == ZERO)CYCLE

          AAA = TZINF
c            AAA = ZERO

           IF(IGAP == 0)THEN
             AAA = TZINF          
           ELSE
             AAA = MARGE+
     .           MAX(MAX(GAPMIN,BGAPSMX+GAP_M(NEDG))+DGAPLOAD,DRAD)
           ENDIF


         M1 = IRECTM(1,NEDG)
         M2 = IRECTM(2,NEDG)

         XX1=X(1,M1)
         XX2=X(1,M2)
         XMAXE=MAX(XX1,XX2)
         XMINE=MIN(XX1,XX2)

         YY1=X(2,M1)
         YY2=X(2,M2)
         YMAXE=MAX(YY1,YY2)
         YMINE=MIN(YY1,YY2)

         ZZ1=X(3,M1)
         ZZ2=X(3,M2)
         ZMAXE=MAX(ZZ1,ZZ2)
         ZMINE=MIN(ZZ1,ZZ2)

c        indice des voxels occupes par la facette

         IX1=INT(NBX*(XMINE-AAA-XMINB)/(XMAXB-XMINB))
         IY1=INT(NBY*(YMINE-AAA-YMINB)/(YMAXB-YMINB))
         IZ1=INT(NBZ*(ZMINE-AAA-ZMINB)/(ZMAXB-ZMINB))

         IX1=MAX(0,MIN(NBX,IX1))
         IY1=MAX(0,MIN(NBY,IY1))
         IZ1=MAX(0,MIN(NBZ,IZ1))

         IX2=INT(NBX*(XMAXE+AAA-XMINB)/(XMAXB-XMINB))
         IY2=INT(NBY*(YMAXE+AAA-YMINB)/(YMAXB-YMINB))
         IZ2=INT(NBZ*(ZMAXE+AAA-ZMINB)/(ZMAXB-ZMINB))

         IX2=MAX(0,MIN(NBX,IX2))
         IY2=MAX(0,MIN(NBY,IY2))
         IZ2=MAX(0,MIN(NBZ,IZ2))

         DO IZ = IZ1, IZ2
           DO IY = IY1, IY2
             TMP = 0
             DO IX = IX1, IX2
               TMP=IBSET(TMP,IX)
             END DO
#include "atomic.inc"
             CRVOXEL(IY,IZ,LOC_PROC)=IOR(CRVOXEL(IY,IZ,LOC_PROC),TMP)
           END DO
         END DO


       ENDDO
C
      RETURN
      END
C END TRI11VOX0
Chd|====================================================================
Chd|  SPMD_TRI11VOX                 source/mpi/interfaces/spmd_int.F
Chd|-- called by -----------
Chd|        I11MAIN_TRI                   source/interfaces/intsort/i11main_tri.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        ARRET                         source/system/arret.F         
Chd|        STARTIME                      source/system/timer.F         
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        TRI7BOX                       share/modules/tri7box.F       
Chd|====================================================================
      SUBROUTINE SPMD_TRI11VOX(
     1   IRECTS  ,NRTS     ,X      ,V     ,MS     ,
     2   BMINMAL ,WEIGHT   ,STIFS  ,NIN   ,ISENDTO,
     3   IRCVFROM,IAD_ELEM ,FR_ELEM,NRTSR ,INACTI ,
     4   GAP_S   ,PENIS    ,ITAB   ,IGAP  ,TZINF  ,
     5   NODNX_SMS,GAP_S_L ,NSNFIOLD,IFORM,INTTH  ,
     6   IELEC   , AREAS   ,TEMP    ,NISUB,ADDSUBS,
     7   LISUBS  ,INTFRIC  ,IPARTFRICS,INFLG_SUBS)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE TRI7BOX
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
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "scr02_c.inc"
#include      "scr18_c.inc"
#include      "task_c.inc"
#include      "units_c.inc"
#include      "warn_c.inc"
#include      "timeri_c.inc"
#include      "sms_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NIN, INACTI, IGAP, NRTS,NRTSR, INTFRIC,
     .        WEIGHT(*),IRECTS(2,NRTS),
     .        ISENDTO(NINTER+1,*), IRCVFROM(NINTER+1,*),
     .        IAD_ELEM(2,*), FR_ELEM(*), ITAB(*), 
     .        NODNX_SMS(*),NSNFIOLD(*),IFORM,INTTH,IELEC(*),
     .        NISUB,ADDSUBS(*),LISUBS(*),IPARTFRICS(*),INFLG_SUBS(*)

      my_real
     .        X(3,*), V(3,*), MS(*), BMINMAL(6), 
     .        STIFS(NRTS), GAP_S(NRTS),
     .        GAP_S_L(*), TZINF, PENIS(2,*),AREAS(*),TEMP(*)
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
#ifdef MPI
      INTEGER MSGTYP,INFO,I, LOC_PROC,P,IDEB,
     .        MSGOFF, MSGOFF2, MSGOFF3, MSGOFF4,
     .        SIZ,J, L, LEN, NB, IERROR1, IAD,
     .        STATUS(MPI_STATUS_SIZE),IERROR,REQ_SB(NSPMD),
     .        REQ_RB(NSPMD),KK,NBIRECV,IRINDEXI(NSPMD),
     .        REQ_RD(NSPMD),REQ_SD(NSPMD),REQ_SD2(NSPMD),
     .        REQ_RC(NSPMD),REQ_SC(NSPMD),
     .        INDEXI,ISINDEXI(NSPMD),INDEX(NRTS),NBOX(NSPMD),
     .        NBX,NBY,NBZ,IX,IY,IZ, N1, N2,
     .        IX1,IY1,IZ1,IX2,IY2,IZ2, NOD,
     .        RSIZ, ISIZ, L2, REQ_SD3(NSPMD),
     .        REQ_RD2(NSPMD), RSHIFT, ISHIFT, LEN2, K,LL
      my_real
     .        BMINMA(6,NSPMD),
     .        XMAXB,YMAXB,ZMAXB,XMINB,YMINB,ZMINB,
     .        XMINS, YMINS, ZMINS, XMAXS, YMAXS, ZMAXS,
     .        DX, DY, DZ
         
      TYPE(real_pointer), DIMENSION(NSPMD) :: RBUF
      TYPE(int_pointer) , DIMENSION(NSPMD) :: IBUF  
           
      DATA MSGOFF/6009/
      DATA MSGOFF2/6010/
      DATA MSGOFF3/6011/
      DATA MSGOFF4/6012/
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
C
C=======================================================================
C     tag des boites contenant des facettes
C     et creation des candidats
C=======================================================================
      LOC_PROC = ISPMD + 1

      NBX = LRVOXEL
      NBY = LRVOXEL
      NBZ = LRVOXEL

C Sauvegarde valeur ancienne des nsn frontieres
C
      IF(IFORM == 2) THEN
         DO P = 1, NSPMD
           NSNFIOLD(P) = NSNFI(NIN)%P(P)
         END DO
      END IF
C
C
C
Cel boite minmax pour le tri provenant de i11uce BMINMA
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
     .          CRVOXEL(0,0,LOC_PROC),
     .          (LRVOXEL+1)*(LRVOXEL+1),
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
     .          CRVOXEL(0,0,P),
     .         (LRVOXEL+1)*(LRVOXEL+1),
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
c general case
      RSIZ = 15
      ISIZ = 3

c specific cases  
c IGAP=1 or IGAP=2 
      IF(IGAP.EQ.1.OR.IGAP.EQ.2) THEN 
        RSIZ = RSIZ + 1 
c IGAP=3		
      ELSEIF(IGAP.EQ.3) THEN
        RSIZ = RSIZ + 2
      ENDIF

c INACTI = 5 or 6     
      IF(INACTI.EQ.5.OR.INACTI.EQ.6) RSIZ = RSIZ + 2      
      
C -- IDTMINS==2      
      IF(IDTMINS == 2)THEN
        ISIZ = ISIZ + 4
C -- IDTMINS_INT /= 0		
      ELSEIF(IDTMINS_INT/=0)THEN
        ISIZ = ISIZ + 2
      END IF
      IF(INTTH > 0)THEN
        RSIZ = RSIZ + 3
        ISIZ = ISIZ + 1 
      ENDIF
C Friction      
      IF(INTFRIC > 0 ) THEN    
	ISIZ = ISIZ + 1
      ENDIF

C -- SUBINTERFACES
      IF (NISUB > 0) THEN
        ISIZ = ISIZ + 1 + NISUB
        ISIZ = ISIZ + NISUB
      ENDIF 
C      
      IDEB = 1      
    
      IF(ISENDTO(NIN,LOC_PROC).NE.0) THEN
        DO KK = 1, NBIRECV
          CALL MPI_WAITANY(NBIRECV,REQ_RB,INDEXI,STATUS,IERROR)
          P=IRINDEXI(INDEXI) 


          CALL MPI_WAIT(REQ_RC(INDEXI),STATUS,IERROR)
C Traitement special sur d.d. ne consever que les noeuds internes
!          DO J = IAD_ELEM(1,P), IAD_ELEM(1,P+1)-1
!            NOD = FR_ELEM(J)
!C weight < 0 temporairement pour ne conserver que les noeuds non frontiere
!            WEIGHT(NOD) = WEIGHT(NOD)*(-1)
!          ENDDO
C
          L = IDEB
          NBOX(P) = 0
          NB = 0
          XMAXB = BMINMA(1,P)
          YMAXB = BMINMA(2,P)
          ZMAXB = BMINMA(3,P)
          XMINB = BMINMA(4,P)
          YMINB = BMINMA(5,P)
          ZMINB = BMINMA(6,P)
          DX=XMAXB-XMINB
          DY=YMAXB-YMINB
          DZ=ZMAXB-ZMINB
          DO I=1,NRTS
           N1=IRECTS(1,I)
           N2=IRECTS(2,I)
           IF(STIFS(I).GT.ZERO) THEN
             XMINS = MIN(X(1,N1),X(1,N2))!-TZINF
             YMINS = MIN(X(2,N1),X(2,N2))!-TZINF
             ZMINS = MIN(X(3,N1),X(3,N2))!-TZINF
             XMAXS = MAX(X(1,N1),X(1,N2))!+TZINF
             YMAXS = MAX(X(2,N1),X(2,N2))!+TZINF
             ZMAXS = MAX(X(3,N1),X(3,N2))!+TZINF
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
                     IF(BTEST(CRVOXEL(IY,IZ,P),IX)) THEN
                      NB = NB + 1
                      INDEX(NB) = I
                      GOTO 111 !next I
                     END IF
                    END DO
                   END DO
                  END DO
                 ENDIF
                ENDIF
               ENDIF

  111      CONTINUE       

           ENDIF !(STIFS(I).GT.ZERO)

          ENDDO !I=1,NRTS
          NBOX(P) = NB
C
C
C Envoi taille msg
C
          MSGTYP = MSGOFF3 
          CALL MPI_ISEND(NBOX(P),1,MPI_INTEGER,IT_SPMD(P),MSGTYP,
     .                 MPI_COMM_WORLD,REQ_SD(P),IERROR)
C
C Alloc buffer
C
          IF (NB.GT.0) THEN
            ALLOCATE(RBUF(P)%P(RSIZ*NB),STAT=IERROR)
	    ALLOCATE(IBUF(P)%P(ISIZ*NB),STAT=IERROR)    
            IF(IERROR.NE.0) THEN
              CALL ANCMSG(MSGID=20,ANMODE=ANINFO)
              CALL ARRET(2)
            ENDIF
            L = 0
	    L2= 0
C
c general case
#include      "vectorize.inc"
            DO J = 1, NB
              I = INDEX(J)
              N1=IRECTS(1,I)
              N2=IRECTS(2,I)
              RBUF(P)%p(L+1) = X(1,N1)
              RBUF(P)%p(L+2) = X(2,N1)
              RBUF(P)%p(L+3) = X(3,N1)
              RBUF(P)%p(L+4) = V(1,N1)
              RBUF(P)%p(L+5) = V(2,N1)
              RBUF(P)%p(L+6) = V(3,N1)
              RBUF(P)%p(L+7) = MS(N1)
              RBUF(P)%p(L+8)= X(1,N2)
              RBUF(P)%p(L+9)= X(2,N2)
              RBUF(P)%p(L+10)= X(3,N2)
              RBUF(P)%p(L+11)= V(1,N2)
              RBUF(P)%p(L+12)= V(2,N2)
              RBUF(P)%p(L+13)= V(3,N2)
              RBUF(P)%p(L+14)= MS(N2)
              RBUF(P)%p(L+15)= STIFS(I)
	      IBUF(P)%p(L2+1)= I
	      IBUF(P)%p(L2+2)= ITAB(N1)
	      IBUF(P)%p(L2+3)= ITAB(N2)			
              L = L + RSIZ
              L2 = L2 + ISIZ
            END DO
	       
c shift for real variables	    
	    RSHIFT = 16
c shift for integer variables    
	    ISHIFT = 4	
	      
c specific cases
c IGAP=1 or IGAP=2		 
            IF(IGAP.EQ.1 .OR. IGAP.EQ.2)THEN
	       L = 0	    
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
                 N1=IRECTS(1,I)
                 N2=IRECTS(2,I)
                 RBUF(P)%p(L+RSHIFT)   = TEMP(N1)
                 RBUF(P)%p(L+RSHIFT+1) = TEMP(N2)
                 RBUF(P)%p(L+RSHIFT+2) = AREAS(I)
		 IBUF(P)%p(L2+ISHIFT) = IELEC(I)
                 L = L + RSIZ
                 L2 = L2 + ISIZ
               END DO
	       RSHIFT = RSHIFT + 3
	       ISHIFT = ISHIFT + 1	       
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
	    
c INACTI = 5 or 6 
            IF(INACTI.EQ.5.OR.INACTI.EQ.6)THEN
	       L = 0	    
#include      "vectorize.inc"	       
               DO J = 1, NB
                 I = INDEX(J)   
                 RBUF(P)%p(L+RSHIFT)  = PENIS(1,I)
                 RBUF(P)%p(L+RSHIFT+1)= PENIS(2,I)
                 L = L + RSIZ  	 
	       ENDDO
	       RSHIFT = RSHIFT + 2
	    ENDIF
	       
C -- IDTMINS==2
            IF(IDTMINS==2)THEN
               L2 = 0
#include      "vectorize.inc"	       	       
               DO J = 1, NB
                 I = INDEX(J)
                 N1=IRECTS(1,I)
                 N2=IRECTS(2,I)
                 IBUF(P)%p(L2+ISHIFT)  = NODNX_SMS(N1)
                 IBUF(P)%p(L2+ISHIFT+1)= N1
                 IBUF(P)%p(L2+ISHIFT+2)= NODNX_SMS(N2)
                 IBUF(P)%p(L2+ISHIFT+3)= N2		 
                 L2 = L2 + ISIZ
               END DO	
	       ISHIFT = ISHIFT + 4       
C -- IDTMINS_INT /= 0	       
            ELSEIF(IDTMINS_INT.NE.0)THEN
              L2 = 0	    
#include      "vectorize.inc"	      
              DO J = 1, NB
                I = INDEX(J)
                N1=IRECTS(1,I)
                N2=IRECTS(2,I)
		IBUF(P)%p(L2+ISHIFT)  = N1
		IBUF(P)%p(L2+ISHIFT+1)= N2	
                L2 = L2 + ISIZ
              END DO
	      ISHIFT = ISHIFT + 2      
	    ENDIF

C-- SUBINTERFACES
            IF(NISUB > 0)THEN
              L2 = 0	    
#include      "vectorize.inc"     
              DO J = 1, NB
                I = INDEX(J)
	            IBUF(P)%p(L2+ISHIFT)  = ADDSUBS(I+1)-ADDSUBS(I)
                LL = 0
                DO K = 1,ADDSUBS(I+1)-ADDSUBS(I)  
                   LL = LL + 1
		           IBUF(P)%p(L2+ISHIFT+LL)=LISUBS(ADDSUBS(I)+K-1)
                   LL = LL + 1
		           IBUF(P)%p(L2+ISHIFT+LL)=INFLG_SUBS(ADDSUBS(I)+K-1)
                END DO 	
                L2 = L2 + ISIZ
              END DO
	          ISHIFT = ISHIFT + 2*NISUB + 1  
            ENDIF
C	    	       	    	         
            MSGTYP = MSGOFF4 
            CALL MPI_ISEND(
     1        RBUF(P)%P(1),NB*RSIZ,REAL,IT_SPMD(P),MSGTYP,
     2        MPI_COMM_WORLD,REQ_SD2(P),ierror)
             
	     MSGTYP = MSGOFF4
            CALL MPI_ISEND(
     1        IBUF(P)%P(1),NB*ISIZ,MPI_INTEGER,IT_SPMD(P),MSGTYP,
     2        MPI_COMM_WORLD,REQ_SD3(P),ierror)     
          ENDIF !ENDIF NB > 0
        ENDDO !ENDDO KK = 1, NBIRECV
      ENDIF !ENDIF SENDTO(NIN,LOC_PROC).NE.0
C
C
Cel reception  des donnees XREM
C
      IF(IRCVFROM(NIN,LOC_PROC).NE.0) THEN
        NRTSR = 0
        L=0
        DO P = 1, NSPMD
          NSNFI(NIN)%P(P) = 0
          IF(ISENDTO(NIN,P).NE.0) THEN
            IF(LOC_PROC.NE.P) THEN
              MSGTYP = MSGOFF3 
              CALL MPI_RECV(NSNFI(NIN)%P(P),1,MPI_INTEGER,IT_SPMD(P),
     .                      MSGTYP,MPI_COMM_WORLD,STATUS,IERROR)
              IF(NSNFI(NIN)%P(P).GT.0) THEN
                L=L+1
                ISINDEXI(L)=P
                NRTSR = NRTSR + NSNFI(NIN)%P(P)
              ENDIF
            ENDIF
          ENDIF
        ENDDO
        NBIRECV=L
C
C Allocate total size
C
        IF(NRTSR.GT.0) THEN
          ALLOCATE(XREM(RSIZ,NRTSR),STAT=IERROR)
          ALLOCATE(IREM(ISIZ,NRTSR),STAT=IERROR1)
	  
          IERROR=IERROR+IERROR1
          IF(IERROR.NE.0) THEN
            CALL ANCMSG(MSGID=20,ANMODE=ANINFO)
            CALL ARRET(2)
          ENDIF
          IDEB = 1
          DO L = 1, NBIRECV
            P = ISINDEXI(L)
            LEN = NSNFI(NIN)%P(P)*RSIZ
            MSGTYP = MSGOFF4 
            CALL MPI_IRECV(
     1        XREM(1,IDEB),LEN,REAL,IT_SPMD(P),
     2        MSGTYP,MPI_COMM_WORLD,REQ_RD(L),IERROR)
     
            LEN2 = NSNFI(NIN)%P(P)*ISIZ
            MSGTYP = MSGOFF4
            CALL MPI_IRECV(
     1        IREM(1,IDEB),LEN2,MPI_INTEGER,IT_SPMD(P),
     2        MSGTYP,MPI_COMM_WORLD,REQ_RD2(L),IERROR) 
        
            IDEB = IDEB + NSNFI(NIN)%P(P)
          ENDDO
          DO L = 1, NBIRECV
            CALL MPI_WAITANY(NBIRECV,REQ_RD,INDEXI,STATUS,IERROR)
            CALL MPI_WAITANY(NBIRECV,REQ_RD2,INDEXI,STATUS,IERROR)	    
          ENDDO
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
              IF(NBOX(P).NE.0) THEN
                CALL MPI_WAIT(REQ_SD2(P),STATUS,IERROR)
                DEALLOCATE(RBUF(P)%p)
                CALL MPI_WAIT(REQ_SD3(P),STATUS,IERROR)
                DEALLOCATE(IBUF(P)%p)		
              END IF
            ENDIF
          ENDIF
        ENDDO
      ENDIF
C
#endif
      RETURN
      END
C
C END SPMD_TRI11VOX
Chd|====================================================================
Chd|  SPMD_TRI11GAT                 source/mpi/interfaces/spmd_int.F
Chd|-- called by -----------
Chd|        I11MAIN_TRI                   source/interfaces/intsort/i11main_tri.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        ARRET                         source/system/arret.F         
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        TRI7BOX                       share/modules/tri7box.F       
Chd|====================================================================
      SUBROUTINE SPMD_TRI11GAT(RESULT,NRTS ,CAND_S ,I_STOK,NIN,
     2                         INACTI,NRTSR,MULTIMP,IGAP,INTTH,
     2                         NISUB,INTFRIC)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE TRI7BOX
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
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "task_c.inc"
#include      "units_c.inc"
#include      "scr18_c.inc"
#include      "parit_c.inc"
#include      "spmd_c.inc"
#include      "sms_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER RESULT, NIN, NRTS, I_STOK, INACTI, NRTSR, MULTIMP, IGAP,
     .        CAND_S(*),INTTH,NISUB,INTFRIC
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
#ifdef MPI
      INTEGER OLDNRTSR,SEGFI,NODFI,NNP,LSKYFI,
     .        NOD, LOC_PROC, I, N, NN, P, IDEB, N1, N2,
     .        IERROR1,IERROR2,IERROR3,IERROR4,IERROR5,IERROR6,IERROR7,
     .        IERROR8,IERROR9,IERROR10,IERROR11,IERROR12,IERROR13,IERROR14,
     .        IERROR15,IERROR16,IERROR17,INDEX(NRTSR), NN2, RSHIFT, ISHIFT, J, K, L,IDEB_SUBINT,
     .        LL
    
       INTEGER, DIMENSION(:), ALLOCATABLE :: IAUX
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
      LOC_PROC = ISPMD + 1
C
C
      SEGFI = 0
      LSKYFI= 0
      IF(RESULT.EQ.0) THEN
C
C Reperage des candidats
C
        SEGFI = 0
        DO I = 1, I_STOK
          N = CAND_S(I)
          NN = N-NRTS
          IF(NN.GT.0)THEN
            IF(IREM(1,NN).GT.0)THEN
              SEGFI = SEGFI + 1
              IREM(1,NN) = -IREM(1,NN)
            ENDIF
          ENDIF
        ENDDO
C non optimal car noeuds dupliques potentiellement
        NODFI = 2*SEGFI
C
C Allocation des tableaux de frontieres interfaces
C
        IERROR1 = 0
        IERROR2 = 0
        IERROR3 = 0
        IERROR4 = 0
        IERROR5 = 0
        IERROR6 = 0
        IERROR7 = 0
        IERROR8 = 0
        IERROR9 = 0
        IERROR10 = 0
        IERROR11 = 0
        IERROR12 = 0
        IERROR13 = 0    
        IERROR14 = 0
        IERROR15 = 0
        IERROR16 = 0 
        IERROR17 = 0       
C
        IF(ASSOCIATED(NSVFI(NIN)%P)) DEALLOCATE(NSVFI(NIN)%P)
        ALLOCATE(NSVFI(NIN)%P(SEGFI),STAT=IERROR1)
        IF(ASSOCIATED(XFI(NIN)%P)) DEALLOCATE(XFI(NIN)%P)
        ALLOCATE(XFI(NIN)%P(3,NODFI),STAT=IERROR2)
        IF(ASSOCIATED(VFI(NIN)%P)) DEALLOCATE(VFI(NIN)%P)
        ALLOCATE(VFI(NIN)%P(3,NODFI),STAT=IERROR3)
        IF(ASSOCIATED(MSFI(NIN)%P)) DEALLOCATE(MSFI(NIN)%P)
        ALLOCATE(MSFI(NIN)%P(NODFI),STAT=IERROR4)
        IF(ASSOCIATED(STIFI(NIN)%P)) DEALLOCATE(STIFI(NIN)%P)
        ALLOCATE(STIFI(NIN)%P(SEGFI),STAT=IERROR5)
        IF(ASSOCIATED(ITAFI(NIN)%P)) DEALLOCATE(ITAFI(NIN)%P)
        ALLOCATE(ITAFI(NIN)%P(NODFI),STAT=IERROR6)
        IF(IDTMINS == 2) THEN
         IF(ASSOCIATED(NODNXFI(NIN)%P)) DEALLOCATE(NODNXFI(NIN)%P)
         ALLOCATE(NODNXFI(NIN)%P(NODFI),STAT=IERROR7)
         IF(ASSOCIATED(NODAMSFI(NIN)%P)) DEALLOCATE(NODAMSFI(NIN)%P)
         ALLOCATE(NODAMSFI(NIN)%P(NODFI),STAT=IERROR8)
         IF(ASSOCIATED(PROCAMSFI(NIN)%P)) DEALLOCATE(PROCAMSFI(NIN)%P)
         ALLOCATE(PROCAMSFI(NIN)%P(NODFI),STAT=IERROR9)
        ELSEIF(IDTMINS_INT /= 0) THEN
         IF(ASSOCIATED(NODAMSFI(NIN)%P)) DEALLOCATE(NODAMSFI(NIN)%P)
         ALLOCATE(NODAMSFI(NIN)%P(NODFI),STAT=IERROR8)
         IF(ASSOCIATED(PROCAMSFI(NIN)%P)) DEALLOCATE(PROCAMSFI(NIN)%P)
         ALLOCATE(PROCAMSFI(NIN)%P(NODFI),STAT=IERROR9)
        ENDIF 
        IF(IGAP.NE.0) THEN
          IF(ASSOCIATED(GAPFI(NIN)%P)) DEALLOCATE(GAPFI(NIN)%P)
          ALLOCATE(GAPFI(NIN)%P(SEGFI),STAT=IERROR10)
        END IF
        IF(IGAP.EQ.3) THEN
          IF(ASSOCIATED(GAP_LFI(NIN)%P)) DEALLOCATE(GAP_LFI(NIN)%P)
          ALLOCATE(GAP_LFI(NIN)%P(NODFI),STAT=IERROR7)
        ENDIF
        IF(INACTI.EQ.5.OR.INACTI.EQ.6) THEN
          IF(ASSOCIATED(PENFI(NIN)%P)) DEALLOCATE(PENFI(NIN)%P)
          ALLOCATE(PENFI(NIN)%P(2,SEGFI),STAT=IERROR11)
        END IF

       IF(INTTH > 0 ) THEN
           IF(ASSOCIATED(TEMPFI(NIN)%P)) DEALLOCATE(TEMPFI(NIN)%P)
           ALLOCATE(TEMPFI(NIN)%P(2*NODFI),STAT=IERROR12)
           IF(ASSOCIATED(MATSFI(NIN)%P)) DEALLOCATE(MATSFI(NIN)%P)
           ALLOCATE(MATSFI(NIN)%P(SEGFI),STAT=IERROR13)
           IF(ASSOCIATED(AREASFI(NIN)%P)) DEALLOCATE(AREASFI(NIN)%P)
           ALLOCATE(AREASFI(NIN)%P(SEGFI),STAT=IERROR14)
        ENDIF

        IF(INTFRIC > 0 ) THEN
           IF(ASSOCIATED(IPARTFRICSFI(NIN)%P)) DEALLOCATE(IPARTFRICSFI(NIN)%P)
           ALLOCATE(IPARTFRICSFI(NIN)%P(SEGFI),STAT=IERROR17)
        ENDIF

        IF(NISUB > 0 ) THEN
           IF(ASSOCIATED(ADDSUBSFI(NIN)%P)) DEALLOCATE(ADDSUBSFI(NIN)%P)
           ALLOCATE(ADDSUBSFI(NIN)%P(SEGFI),STAT=IERROR15)
           IF(ASSOCIATED(LISUBSFI(NIN)%P))  DEALLOCATE(LISUBSFI(NIN)%P)
           ALLOCATE(LISUBSFI(NIN)%P(NISUB*SEGFI),STAT=IERROR16)
           IF(ASSOCIATED(INFLG_SUBSFI(NIN)%P))  DEALLOCATE(INFLG_SUBSFI(NIN)%P)
           ALLOCATE(INFLG_SUBSFI(NIN)%P(NISUB*SEGFI),STAT=IERROR16)
        ENDIF
C
        IF(IERROR1+IERROR2+IERROR3+IERROR4+IERROR5+
     +     IERROR6+IERROR7+IERROR8+IERROR9+IERROR10+
     +     IERROR11+IERROR12+IERROR13+IERROR14+IERROR15+
     +     IERROR16+IERROR17.NE.0) THEN
          CALL ANCMSG(MSGID=20,ANMODE=ANINFO)
          CALL ARRET(2)
        ENDIF
C
C Compactage des candidats
C
C
        IDEB = 0
        NN2 = 0
        IDEB_SUBINT = 0
	
        DO P = 1, NSPMD
	  NN = 0
          OLDNRTSR = NSNFI(NIN)%P(P)
	  
          IF(OLDNRTSR.NE.0) THEN
	  
	   ALLOCATE(IAUX(OLDNRTSR),STAT=IERROR12)
           IF(IERROR12.NE.0) THEN
             CALL ANCMSG(MSGID=20,ANMODE=ANINFO)
             CALL ARRET(2)
           ENDIF
           NNP = NN2
	   
           DO I = 1, OLDNRTSR
             IF(IREM(1,I+IDEB).LT.0) THEN	    
	       NN = NN + 1
	       IAUX(NN) = I
             ENDIF
           ENDDO	   
	   
c general case 
#include      "vectorize.inc"	   
           DO J = 1, NN
	     I = IAUX(J)
             INDEX(I+IDEB) = NN2+J
             N1 = 2*((NN2+J)-1)+1
             N2 = 2*(NN2+J)	     
             XFI(NIN)%P(1,N1) = XREM(1,I+IDEB)
             XFI(NIN)%P(2,N1) = XREM(2,I+IDEB)
             XFI(NIN)%P(3,N1) = XREM(3,I+IDEB)
             VFI(NIN)%P(1,N1) = XREM(4,I+IDEB)
             VFI(NIN)%P(2,N1) = XREM(5,I+IDEB)
             VFI(NIN)%P(3,N1) = XREM(6,I+IDEB)
             MSFI(NIN)%P(N1)  = XREM(7,I+IDEB)
             XFI(NIN)%P(1,N2) = XREM(8,I+IDEB)
             XFI(NIN)%P(2,N2) = XREM(9,I+IDEB)
             XFI(NIN)%P(3,N2) = XREM(10,I+IDEB)
             VFI(NIN)%P(1,N2) = XREM(11,I+IDEB)
             VFI(NIN)%P(2,N2) = XREM(12,I+IDEB)
             VFI(NIN)%P(3,N2) = XREM(13,I+IDEB)
             MSFI(NIN)%P(N2)  = XREM(14,I+IDEB)
             STIFI(NIN)%P(NN2+J) = XREM(15,I+IDEB)
             NSVFI(NIN)%P(NN2+J) = -IREM(1,I+IDEB)		
             ITAFI(NIN)%P(N1) = IREM(2,I+IDEB)	 
             ITAFI(NIN)%P(N2) = IREM(3,I+IDEB)	         
           END DO
	   
	   RSHIFT = 16
	   ISHIFT = 4	

c IGAP=1 or IGAP=2	
           IF(IGAP.EQ.1 .OR. IGAP.EQ.2)THEN 	     
#include      "vectorize.inc"	      
             DO J = 1, NN
	       I = IAUX(J)
               GAPFI(NIN)%P(NN2+J) = XREM(RSHIFT,I+IDEB)
             ENDDO   
             RSHIFT = RSHIFT + 1  	    
c IGAP=3	   
           ELSEIF(IGAP.EQ.3)THEN
#include      "vectorize.inc"	      	 	         
             DO J = 1, NN	       
	       I = IAUX(J)
	       GAPFI(NIN)%P(NN2+J)   = XREM(RSHIFT,I+IDEB)	  
               GAP_LFI(NIN)%P(NN2+J) = XREM(RSHIFT+1,I+IDEB)
             ENDDO
             RSHIFT = RSHIFT + 2
	   ENDIF

C thermic
            IF(INTTH.GT.0)THEN  
#include      "vectorize.inc"	      
              DO J = 1, NN
	        I = IAUX(J)
                N1 = 2*((NN2+J)-1)+1
                N2 = 2*(NN2+J)
                TEMPFI(NIN)%P(N1)  = XREM(RSHIFT,I+IDEB)
                TEMPFI(NIN)%P(N2)  = XREM(RSHIFT+1,I+IDEB)
                AREASFI(NIN)%P(NN2+J) = XREM(RSHIFT+2,I+IDEB)
                MATSFI(NIN)%P(NN2+J)  = IREM(ISHIFT,I+IDEB)		  
              ENDDO
	      RSHIFT = RSHIFT + 3
	      ISHIFT = ISHIFT + 1	      
	    ENDIF
C Friction model
            IF(INTFRIC.GT.0)THEN    
#include      "vectorize.inc"	      
              DO J = 1, NN
	        I = IAUX(J)
                IPARTFRICSFI(NIN)%P(NN2+J)  = IREM(ISHIFT,I+IDEB)		  
              ENDDO
	      ISHIFT = ISHIFT + 1	      
         ENDIF
	    
c INACTI = 5 or 6 	    	
           IF(INACTI.EQ.5.OR.INACTI.EQ.6)THEN	    	
#include      "vectorize.inc"	       
             DO J = 1, NN
               I = IAUX(J)
               PENFI(NIN)%P(1,NN2+J) = XREM(RSHIFT,I+IDEB)
               PENFI(NIN)%P(2,NN2+J) = XREM(RSHIFT+1,I+IDEB)
	     ENDDO
             RSHIFT = RSHIFT + 2
	   ENDIF


	    
C -- IDTMINS==2	   
           IF(IDTMINS.EQ.2)THEN		
#include      "vectorize.inc"	       
             DO J = 1, NN
               I = IAUX(J)	     
               N1 = 2*((NN2+J)-1)+1
               N2 = 2*(NN2+J)	     	 	
               NODNXFI(NIN)%P(N1)   = IREM(ISHIFT,I+IDEB)
               NODAMSFI(NIN)%P(N1)  = IREM(ISHIFT+1,I+IDEB)
               PROCAMSFI(NIN)%P(N1) = P
               NODNXFI(NIN)%P(N2)   = IREM(ISHIFT+2,I+IDEB)
               NODAMSFI(NIN)%P(N2)  = IREM(ISHIFT+3,I+IDEB)
               PROCAMSFI(NIN)%P(N2) = P			
             ENDDO
             ISHIFT = ISHIFT + 4
	       
C -- IDTMINS_INT /= 0	   
           ELSEIF(IDTMINS_INT/=0)THEN	

#include      "vectorize.inc"	       
             DO J = 1, NN
               I = IAUX(J)	     
               N1 = 2*((NN2+J)-1)+1
               N2 = 2*(NN2+J)		     
               NODAMSFI(NIN)%P(N1)  = IREM(ISHIFT,I+IDEB)
               PROCAMSFI(NIN)%P(N1) = P
               NODAMSFI(NIN)%P(N2)  = IREM(ISHIFT+1,I+IDEB)
               PROCAMSFI(NIN)%P(N2) = P
	     ENDDO	
             ISHIFT = ISHIFT + 2	     			                  
	   ENDIF

C -- SUBINTERFACES
           IF ((NISUB>0).AND.(NN>0)) THEN
C-- First line	
             I = IAUX(1) 
             ADDSUBSFI(NIN)%P(NN2+1) = IDEB_SUBINT + 1
             LL = 0
             DO K = 1,IREM(ISHIFT,I+IDEB)
               LL = LL + 1
               LISUBSFI(NIN)%P(IDEB_SUBINT+K) = IREM(ISHIFT+LL,I+IDEB) 
               LL = LL + 1
               INFLG_SUBSFI(NIN)%P(IDEB_SUBINT+K) = IREM(ISHIFT+LL,I+IDEB) 
             END DO
C   
#include      "vectorize.inc"       
             DO J = 2, NN
               I = IAUX(J)	     		     
               ADDSUBSFI(NIN)%P(NN2+J) = ADDSUBSFI(NIN)%P(NN2+J-1) + IREM(ISHIFT,I+IDEB)
               LL = 0
               DO K = 1,IREM(ISHIFT,I+IDEB)
                 LL = LL + 1
                 LISUBSFI(NIN)%P(ADDSUBSFI(NIN)%P(NN2+J)+K-1) = IREM(ISHIFT+LL,I+IDEB)
                 LL = LL + 1
                 INFLG_SUBSFI(NIN)%P(ADDSUBSFI(NIN)%P(NN2+J)+K-1) = IREM(ISHIFT+LL,I+IDEB) 
               END DO             
	     ENDDO
C
             IDEB_SUBINT = ADDSUBSFI(NIN)%P(NN2+NN)	
             ISHIFT = ISHIFT + 1 + 2*NISUB	     			                  
	   ENDIF
C	     
	   NN2 = NN2 + NN	            
           IDEB = IDEB + OLDNRTSR    
           NSNFI(NIN)%P(P) = NN2-NNP
C	    
	   DEALLOCATE(IAUX)
	    
	  ENDIF !IF(OLDNRTSR.NE.0)  	     

        ENDDO ! end do NSPMD 
	
Cel facteur 2 car 2 noeuds par segement => pris en compte au niveau
Cel de la 1ere dimension allouee
        LSKYFI = NN2*MULTIMAX
        NRTSR = NN2
      ENDIF
C
C Deallocation de XREM IREM
C
      IF(ALLOCATED(XREM)) DEALLOCATE(XREM)
      IF(ALLOCATED(IREM)) DEALLOCATE(IREM)
C
      IERROR1=0 
      IERROR2=0 
      IERROR3=0 
      IERROR4=0 
C
      IF(INTTH == 0 ) THEN
C
C Allocation Parith/OFF
C
       IF(IPARIT.EQ.0) THEN
        IF(ASSOCIATED(AFI(NIN)%P)) DEALLOCATE(AFI(NIN)%P)
        IF(ASSOCIATED(STNFI(NIN)%P)) DEALLOCATE(STNFI(NIN)%P)
        IF(NODFI.GT.0)ALLOCATE(AFI(NIN)%P(3,NODFI*NTHREAD),STAT=IERROR1)
        IF(NODFI.GT.0)ALLOCATE(STNFI(NIN)%P(NODFI*NTHREAD),STAT=IERROR2)
C Init a 0
        DO I = 1, NODFI*NTHREAD
          AFI(NIN)%P(1,I) = ZERO
          AFI(NIN)%P(2,I) = ZERO
          AFI(NIN)%P(3,I) = ZERO
          STNFI(NIN)%P(I) = ZERO
        ENDDO
C
        IF(KDTINT.NE.0)THEN
          IF(ASSOCIATED(VSCFI(NIN)%P)) DEALLOCATE(VSCFI(NIN)%P)
          IF(NODFI.GT.0)ALLOCATE(VSCFI(NIN)%P(NODFI*NTHREAD),STAT=IERROR3)
C Init a 0
          DO I = 1, NODFI*NTHREAD
            VSCFI(NIN)%P(I) = ZERO
          ENDDO
        ENDIF
C
        NLSKYFI(NIN) = NODFI
C
        ELSE
C
C Allocation Parith/ON Dans UPGRADE_REM_2RY

       ENDIF
      ELSE ! INTTH .NE. 0
C
C Allocation Parith/OFF
C
       IF(IPARIT.EQ.0) THEN
        IF(ASSOCIATED(AFI(NIN)%P)) DEALLOCATE(AFI(NIN)%P)
        IF(ASSOCIATED(STNFI(NIN)%P)) DEALLOCATE(STNFI(NIN)%P)
        IF(NODFI.GT.0)ALLOCATE(AFI(NIN)%P(3,NODFI*NTHREAD),STAT=IERROR1)
        IF(NODFI.GT.0)ALLOCATE(STNFI(NIN)%P(NODFI*NTHREAD),STAT=IERROR2)
        IF(NODFI.GT.0)ALLOCATE(FTHEFI(NIN)%P(NODFI*NTHREAD),STAT=IERROR3)
C
        IF(NODADT_THERM ==1) THEN
          IF(ASSOCIATED(CONDNFI(NIN)%P)) DEALLOCATE(CONDNFI(NIN)%P)
          IF(NODFI.GT.0) ALLOCATE(CONDNFI(NIN)%P(NODFI*NTHREAD),STAT=IERROR4)
        ENDIF
C
C Init a 0
        DO I = 1, NODFI*NTHREAD
          AFI(NIN)%P(1,I) = ZERO
          AFI(NIN)%P(2,I) = ZERO
          AFI(NIN)%P(3,I) = ZERO
          STNFI(NIN)%P(I) = ZERO
          FTHEFI(NIN)%P(I) = ZERO
        ENDDO
          IF(NODADT_THERM ==1) THEN
            DO I = 1, NODFI*NTHREAD
               CONDNFI(NIN)%P(I) = ZERO
            ENDDO
          ENDIF
C
        IF(KDTINT.NE.0)THEN
          IF(ASSOCIATED(VSCFI(NIN)%P)) DEALLOCATE(VSCFI(NIN)%P)
          IF(NODFI.GT.0)ALLOCATE(VSCFI(NIN)%P(NODFI),STAT=IERROR3)
C Init a 0
          DO I = 1, NODFI
            VSCFI(NIN)%P(I) = ZERO
          ENDDO
        ENDIF
C
      ELSE ! IF PARITH/ON
C
C Allocation Parith/ON done in UPGRADE_REM_2RY
C
         ENDIF !PARITH/ON
       ENDIF !INTTH
          IF(IERROR1+IERROR2+IERROR3+IERROR4.NE.0) THEN
            CALL ANCMSG(MSGID=20,ANMODE=ANINFO)
            CALL ARRET(2)
          ENDIF
C
C Renumerotation des candidats
C
      DO I = 1, I_STOK
        N = CAND_S(I)
        NN = N-NRTS
        IF(NN.GT.0)THEN
          CAND_S(I) = INDEX(NN)+NRTS
        ENDIF
      ENDDO
C
#endif
      RETURN
      END
C
C END SPMD_TRI11GAT
Chd|====================================================================
Chd|  SPMD_TRI23VOX0                source/mpi/interfaces/spmd_int.F
Chd|-- called by -----------
Chd|        I23MAIN_TRI                   source/interfaces/intsort/i23main_tri.F
Chd|-- calls ---------------
Chd|        TRI7BOX                       share/modules/tri7box.F       
Chd|====================================================================
      SUBROUTINE SPMD_TRI23VOX0(
     1   X      ,BMINMAL ,IGAP   ,NRTM  ,STF   ,
     2   TZINF  ,CURV_MAX,GAPMIN ,GAPMAX,GAP_M ,
     3   IRECT  ,GAP     ,BGAPSMX,MSR   )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE TRI7BOX
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "comlock.inc"
#include      "r4r8_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "task_c.inc"
#include      "timeri_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IGAP, NRTM, IRECT(4,*), MSR(*)
      my_real
     .        X(3,*), BMINMAL(*),
     .        STF(*), GAP_M(*), BGAPSMX,
     .        TZINF,GAPMIN,GAPMAX,GAP,CURV_MAX(NRTM)
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
      INTEGER LOC_PROC,
     .        NBX,NBY,NBZ,NE,M1,M2,M3,M4,
     .        IX1,IY1,IZ1,IX2,IY2,IZ2,IX,IY,IZ
      my_real
     .        RATIO, AAA, MARGE,
     .        XMAXB,YMAXB,ZMAXB,XMINB,YMINB,ZMINB,
     .        XMINE,YMINE,ZMINE,XMAXE,YMAXE,ZMAXE,
     .        XX1,XX2,XX3,XX4,YY1,YY2,YY3,YY4,ZZ1,ZZ2,ZZ3,ZZ4
c      DATA IPWR2/1,2,4,8,16,32,64,128,256,512,1024,2048,4096,8192,
c     .           16384,32768,65536,131072,262144,524288,1048576,
c     .           2097152,4194304,8388608,16777216,33554432,67108864,
c     .           134217728,268435456,536870912,1073741824,2147483648/
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
C
C=======================================================================
C     tag des boites contenant des facettes
C     et creation des candidats
C=======================================================================

      LOC_PROC = ISPMD + 1
      MARGE = TZINF-GAP

      NBX = LRVOXEL
      NBY = LRVOXEL
      NBZ = LRVOXEL

      XMAXB = BMINMAL(1)
      YMAXB = BMINMAL(2)
      ZMAXB = BMINMAL(3)
      XMINB = BMINMAL(4)
      YMINB = BMINMAL(5)
      ZMINB = BMINMAL(6)

      DO NE=1,NRTM
C on ne retient pas les facettes detruites
        IF(STF(NE) == ZERO)CYCLE

         IF(IGAP == 0)THEN
           AAA = TZINF+SQRT(THREE)*CURV_MAX(NE)
         ELSE
           AAA = MARGE+SQRT(THREE)*(CURV_MAX(NE)+
     .           MIN(GAPMAX,MAX(GAPMIN,BGAPSMX+GAP_M(NE))))
         ENDIF

c     il est possible d'ameliorer l'algo en decoupant la facette
c     en 2(4,3,6,9...) si la facette est grande devant AAA et inclinee

         M1 = IRECT(1,NE)
         M2 = IRECT(2,NE)
         M3 = IRECT(3,NE)
         M4 = IRECT(4,NE)

         XX1=X(1,M1)
         XX2=X(1,M2)
         XX3=X(1,M3)
         XX4=X(1,M4)
         XMAXE=MAX(XX1,XX2,XX3,XX4)
         XMINE=MIN(XX1,XX2,XX3,XX4)

         YY1=X(2,M1)
         YY2=X(2,M2)
         YY3=X(2,M3)
         YY4=X(2,M4)
         YMAXE=MAX(YY1,YY2,YY3,YY4)
         YMINE=MIN(YY1,YY2,YY3,YY4)

         ZZ1=X(3,M1)
         ZZ2=X(3,M2)
         ZZ3=X(3,M3)
         ZZ4=X(3,M4)
         ZMAXE=MAX(ZZ1,ZZ2,ZZ3,ZZ4)
         ZMINE=MIN(ZZ1,ZZ2,ZZ3,ZZ4)

c        indice des voxels occupes par la facette

         IX1=INT(NBX*(XMINE-AAA-XMINB)/(XMAXB-XMINB))
         IY1=INT(NBY*(YMINE-AAA-YMINB)/(YMAXB-YMINB))
         IZ1=INT(NBZ*(ZMINE-AAA-ZMINB)/(ZMAXB-ZMINB))

         IX1=MAX(0,MIN(NBX,IX1))
         IY1=MAX(0,MIN(NBY,IY1))
         IZ1=MAX(0,MIN(NBZ,IZ1))

         IX2=INT(NBX*(XMAXE+AAA-XMINB)/(XMAXB-XMINB))
         IY2=INT(NBY*(YMAXE+AAA-YMINB)/(YMAXB-YMINB))
         IZ2=INT(NBZ*(ZMAXE+AAA-ZMINB)/(ZMAXB-ZMINB))

         IX2=MAX(0,MIN(NBX,IX2))
         IY2=MAX(0,MIN(NBY,IY2))
         IZ2=MAX(0,MIN(NBZ,IZ2))

#include "lockon.inc"
         DO IZ = IZ1, IZ2
           DO IY = IY1, IY2
             DO IX = IX1, IX2
               CRVOXEL(IY,IZ,LOC_PROC)=IBSET(CRVOXEL(IY,IZ,LOC_PROC),IX)
             END DO
           END DO
         END DO
#include "lockoff.inc"

      ENDDO

C
      RETURN
      END
C
C END SPMD_TRI23VOX0
Chd|====================================================================
Chd|  SPMD_TRI24VOX0                source/mpi/interfaces/spmd_int.F
Chd|-- called by -----------
Chd|        I24MAIN_TRI                   source/interfaces/intsort/i24main_tri.F
Chd|-- calls ---------------
Chd|        TRI7BOX                       share/modules/tri7box.F       
Chd|====================================================================
      SUBROUTINE SPMD_TRI24VOX0(
     1   X      ,BMINMAL ,NRTM  ,STF   ,MARGE  ,
     2   CURV_MAX,GAP_M  ,IRECT  ,GAP  ,BGAPSMX,
     3   PMAX_GAP,VMAXDT ,DGAPLOAD )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE TRI7BOX
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "comlock.inc"
#include      "r4r8_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "task_c.inc"
#include      "timeri_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NRTM, IRECT(4,*)
      my_real
     .        X(3,*), BMINMAL(*),
     .        STF(*), GAP_M(*), BGAPSMX,PMAX_GAP,VMAXDT,
     .        MARGE,GAP,CURV_MAX(NRTM)
      my_real , INTENT(IN) :: DGAPLOAD
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
      INTEGER LOC_PROC,
     .        NBX,NBY,NBZ,NE,M1,M2,M3,M4,
     .        IX1,IY1,IZ1,IX2,IY2,IZ2,IX,IY,IZ
      my_real
     .        RATIO, AAA,
     .        XMAXB,YMAXB,ZMAXB,XMINB,YMINB,ZMINB,
     .        XMINE,YMINE,ZMINE,XMAXE,YMAXE,ZMAXE,
     .        XX1,XX2,XX3,XX4,YY1,YY2,YY3,YY4,ZZ1,ZZ2,ZZ3,ZZ4
c      DATA IPWR2/1,2,4,8,16,32,64,128,256,512,1024,2048,4096,8192,
c     .           16384,32768,65536,131072,262144,524288,1048576,
c     .           2097152,4194304,8388608,16777216,33554432,67108864,
c     .           134217728,268435456,536870912,1073741824,2147483648/
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
C
C=======================================================================
C     tag des boites contenant des facettes
C     et creation des candidats
C=======================================================================

      LOC_PROC = ISPMD + 1

      NBX = LRVOXEL
      NBY = LRVOXEL
      NBZ = LRVOXEL

      XMAXB = BMINMAL(1)
      YMAXB = BMINMAL(2)
      ZMAXB = BMINMAL(3)
      XMINB = BMINMAL(4)
      YMINB = BMINMAL(5)
      ZMINB = BMINMAL(6)

      DO NE=1,NRTM
C on ne retient pas les facettes detruites
        IF(STF(NE) == ZERO)CYCLE
         AAA = MARGE+CURV_MAX(NE)+VMAXDT
     +       + MAX(PMAX_GAP,BGAPSMX+GAP_M(NE))+DGAPLOAD

c     il est possible d'ameliorer l'algo en decoupant la facette
c     en 2(4,3,6,9...) si la facette est grande devant AAA et inclinee

         M1 = IRECT(1,NE)
         M2 = IRECT(2,NE)
         M3 = IRECT(3,NE)
         M4 = IRECT(4,NE)

         XX1=X(1,M1)
         XX2=X(1,M2)
         XX3=X(1,M3)
         XX4=X(1,M4)
         XMAXE=MAX(XX1,XX2,XX3,XX4)
         XMINE=MIN(XX1,XX2,XX3,XX4)

         YY1=X(2,M1)
         YY2=X(2,M2)
         YY3=X(2,M3)
         YY4=X(2,M4)
         YMAXE=MAX(YY1,YY2,YY3,YY4)
         YMINE=MIN(YY1,YY2,YY3,YY4)

         ZZ1=X(3,M1)
         ZZ2=X(3,M2)
         ZZ3=X(3,M3)
         ZZ4=X(3,M4)
         ZMAXE=MAX(ZZ1,ZZ2,ZZ3,ZZ4)
         ZMINE=MIN(ZZ1,ZZ2,ZZ3,ZZ4)

c        indice des voxels occupes par la facette

         IX1=INT(NBX*(XMINE-AAA-XMINB)/(XMAXB-XMINB))
         IY1=INT(NBY*(YMINE-AAA-YMINB)/(YMAXB-YMINB))
         IZ1=INT(NBZ*(ZMINE-AAA-ZMINB)/(ZMAXB-ZMINB))

         IX1=MAX(0,MIN(NBX,IX1))
         IY1=MAX(0,MIN(NBY,IY1))
         IZ1=MAX(0,MIN(NBZ,IZ1))

         IX2=INT(NBX*(XMAXE+AAA-XMINB)/(XMAXB-XMINB))
         IY2=INT(NBY*(YMAXE+AAA-YMINB)/(YMAXB-YMINB))
         IZ2=INT(NBZ*(ZMAXE+AAA-ZMINB)/(ZMAXB-ZMINB))

         IX2=MAX(0,MIN(NBX,IX2))
         IY2=MAX(0,MIN(NBY,IY2))
         IZ2=MAX(0,MIN(NBZ,IZ2))

#include "lockon.inc"
         DO IZ = IZ1, IZ2
           DO IY = IY1, IY2
             DO IX = IX1, IX2
               CRVOXEL(IY,IZ,LOC_PROC)=IBSET(CRVOXEL(IY,IZ,LOC_PROC),IX)
             END DO
           END DO
         END DO
#include "lockoff.inc"

      ENDDO

C
      RETURN
      END
C
C END SPMD_TRI24VOX0
C
Chd|====================================================================
Chd|  SPMD_EXCH_SLID                source/mpi/interfaces/spmd_int.F
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE SPMD_EXCH_SLID(
     1   IAD_FRNOR,FR_NOR,IPARI ,LENS, BUFS  ,
     2   IADBUFS  ,LENR  ,BUFR  ,IADBUFR )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "r4r8_p.inc"
C-----------------------------------------------------------------
C   M e s s a g e   P a s s i n g
C-----------------------------------------------
#ifdef MPI
#include "mpif.h"
#endif
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "com_xfem1.inc"
#include      "task_c.inc"
#include      "parit_c.inc"
#include      "spmd_c.inc"
#include      "units_c.inc"
#include      "timeri_c.inc"
#include      "warn_c.inc"
#include      "param_c.inc"
#include      "tabsiz_c.inc"
#include      "sphcom.inc"
#include      "scr18_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IAD_FRNOR(NINTER25,*), FR_NOR(*),
     .        IPARI(NPARI,*), LENS, LENR, 
     .        IADBUFS(NINTER25,NSPMD+1), IADBUFR(NSPMD+1)
      INTEGER BUFS(LENS), BUFR(LENR)
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
#ifdef MPI
      INTEGER NI, NI25, NTY,
     .        MSGTYP, I, LOC_PROC, IERROR, L0,
     .        SIZ, L, J,NBIRECV, NBISEND, II, MSGOFF,
     .        REQ_R(NSPMD),REQ_S(NSPMD),IRINDEX(NSPMD),
     .        ISINDEX(NSPMD), STATUS(MPI_STATUS_SIZE), ICOMM(NSPMD)
      INTEGER SBUF(LENS)
      DATA MSGOFF/6015/
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
C
      LOC_PROC = ISPMD + 1
C
      L = 0
      NBISEND = 0
      DO I = 1, NSPMD
        ICOMM(I)=0
        L0 = L+1
        IF(I/=LOC_PROC)THEN
          NI25 = 0
          DO NI = 1, NINTER
            NTY = IPARI(7,NI)
            IF(NTY/=25) CYCLE
            NI25 = NI25 + 1
            ICOMM(I) = ICOMM(I) + IAD_FRNOR(NI25,I+1)-IAD_FRNOR(NI25,I) 
            IF(IADBUFS(NI25,I+1)-IADBUFS(NI25,I).GT.0) THEN
              DO J=IADBUFS(NI25,I),IADBUFS(NI25,I+1)-1
                L = L + 1
                SBUF(L)=BUFS(J)
              END DO
            ENDIF
          END DO
          SIZ = L - L0 + 1
          IF(ICOMM(I) > 0)THEN
            MSGTYP = MSGOFF
            NBISEND = NBISEND + 1
            ISINDEX(NBISEND)=I
            CALL MPI_ISEND(
     S      SBUF(L0),SIZ,MPI_INTEGER,IT_SPMD(I),MSGTYP,
     G      MPI_COMM_WORLD,REQ_S(I),IERROR)
          ENDIF
        ENDIF
      ENDDO

C
      NBIRECV = 0
      L = 1
      DO I = 1, NSPMD
        IADBUFR(I)=L
        IF(ICOMM(I) > 0)THEN
          SIZ = NSNT25 + ICOMM(I)
          MSGTYP = MSGOFF
          NBIRECV = NBIRECV + 1
          IRINDEX(NBIRECV) = I
          CALL MPI_RECV(
     S        BUFR(L),SIZ,MPI_INTEGER,IT_SPMD(I),MSGTYP,
     G        MPI_COMM_WORLD,STATUS,IERROR)
          CALL MPI_GET_COUNT(STATUS,MPI_INTEGER,SIZ,ierror)
          L = L + SIZ
        ENDIF
      ENDDO
      IADBUFR(NSPMD+1)=L
C
      DO II=1,NBISEND
        I = ISINDEX(II)
        CALL MPI_WAIT(REQ_S(I),STATUS,IERROR)
      ENDDO
C
#endif
      RETURN
      END

