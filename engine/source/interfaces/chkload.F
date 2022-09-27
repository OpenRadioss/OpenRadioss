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
C
Chd|====================================================================
Chd|  CHKLOAD                       source/interfaces/chkload.F   
Chd|-- called by -----------
Chd|        RESOL                         source/engine/resol.F         
Chd|-- calls ---------------
Chd|        MY_BARRIER                    source/system/machine.F       
Chd|        SPMD_EXCHSEG_IDEL             source/mpi/kinematic_conditions/spmd_exchseg_idel.F
Chd|        SPMD_INIT_IDEL                source/mpi/interfaces/spmd_init_idel.F
Chd|====================================================================
      SUBROUTINE CHKLOAD(
     1    IB     ,IXS     ,IXQ     ,IXC    ,IXT     ,IXP     ,
     2    IXR    ,IXTG    ,ITAG   ,ITASK   ,ITAGL   ,ITAB    ,
     3    ITABM1 ,ADDCNEL ,CNEL   ,TAGEL   ,IPARG   ,GEO     ,
     4    IBUFS  ,NINDEX  ,NINDG  ,NPRESLOAD,LOADP_TAGDEL    ,
     5    ILOADP ,LLOADP  ,IAD_ELEM)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "comlock.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "task_c.inc"
#include      "scr01_c.inc"
#include      "scr05_c.inc"
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "param_c.inc"
#include      "parit_c.inc"
#include      "tabsiz_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER ITASK, NINDG 
      INTEGER, INTENT(IN) :: NPRESLOAD
      INTEGER IB(NIBCLD,*), ITAG(*),
     .        IXS(NIXS,*),IXQ(NIXQ,*),IXC(NIXC,*), IXT(NIXT,*), IXP(NIXP,*),
     .        IXR(NIXR,*), IXTG(NIXTG,*),IPARG(NPARG,*), ITAGL(*), ITAB(*),
     .        ITABM1(*), CNEL(0:*), ADDCNEL(0:*), TAGEL(*), IBUFS(*) , NINDEX(*)
      INTEGER, INTENT(INOUT) :: LOADP_TAGDEL(NPRESLOAD)
      INTEGER, INTENT(IN) :: LLOADP(SLLOADP), ILOADP(SIZLOADP,NLOADP)
      INTEGER, DIMENSION(2,NSPMD+1), INTENT(in) :: IAD_ELEM
      my_real
     .        GEO(NPROPG,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J, NMNF, NMNL, NRTF, NRTL, N1, N2, N3, N4, 
     .        NN, II, IX, K, NIND,  N, IRSIZE, IRECV(NSPMD),
     .        OFC, OFQ, OFT, OFP ,OFR ,OFTG, OFUR,NCONLDF ,
     .        NCONLDL,NL ,ICOMP ,IDBS ,IDB , IERR, NIND2  ,
     .        NINDPLOAD ,NUMPRESLOAD, JJ, IAD, NPRES, NP

      INTEGER, DIMENSION(:,:), ALLOCATABLE :: NINDL      
C-----------------------------------------------
      OFQ=NUMELS
      OFC=OFQ+NUMELQ
      OFT=OFC+NUMELC
      OFP=OFT+NUMELT
      OFR=OFP+NUMELP
      OFTG=OFR+NUMELR
      OFUR=OFTG+NUMELTG
C
      ALLOCATE(NINDL(2,NCONLD+NPRESLOAD))      
C
      CALL MY_BARRIER()
C
      NCONLDF = 1 + ITASK*NCONLD / NTHREAD
      NCONLDL = (ITASK+1)*NCONLD / NTHREAD
C
      NIND = 0
C
C--------------------------------------------------------
C     SEARCH FOR NODES WHERE SURROUNDING ELEMENTS 
C--------------------------------------------------------
      DO NL=NCONLDF,NCONLDL
         IF( IB(7,NL) == 1 ) THEN ! IF deleted Segment is ON
            N1  = IB(1,NL)
            N2  = IB(2,NL)
            N3  = IB(3,NL)
            N4  = IB(4,NL)
            IF(N3 == 0) N3 = N2
            IF(N4 == 0) N4 = N3
            IF(ITAG(N1) == 0.OR.ITAG(N2) == 0.OR.
     +         ITAG(N3) == 0.OR.ITAG(N4) == 0) THEN ! IF ALL SURROUNDED ELEMENTS ARE OFF
               IB(8,NL) = 1
            ELSEIF(ITAG(NUMNOD+N1)>=1.AND.ITAG(NUMNOD+N2)>=1.AND.
     +             ITAG(NUMNOD+N3)>=1.AND.ITAG(NUMNOD+N4)>=1) THEN ! IF ALL NODES BELONGING TO 1 ACTIF ELEMENT
               NIND = NIND + 1
               NINDL(1,NIND) = NL
            ENDIF
         ENDIF
       ENDDO

       NINDPLOAD = NIND

       NUMPRESLOAD = 0

       DO  NP=1,NLOADP_HYD

           NPRES = ILOADP(1,NP)  
           IAD = ILOADP(4,NP)

           DO N=1, NPRES/4
C
              N1 = LLOADP(IAD+4*(N-1))
              N2 = LLOADP(IAD+4*(N-1)+1)
              N3 = LLOADP(IAD+4*(N-1)+2)
              N4 = LLOADP(IAD+4*(N-1)+3)
              NUMPRESLOAD = NUMPRESLOAD + 1

              IF(N3 == 0) N3 = N2
              IF(N4 == 0) N4 = N3
              IF(ITAG(N1) == 0.OR.ITAG(N2) == 0.OR.
     +           ITAG(N3) == 0.OR.ITAG(N4) == 0) THEN ! IF ALL SURROUNDED ELEMENTS ARE OFF
                 LOADP_TAGDEL(NUMPRESLOAD) = 1
              ELSEIF(ITAG(NUMNOD+N1)>=1.AND.ITAG(NUMNOD+N2)>=1.AND.
     +             ITAG(NUMNOD+N3)>=1.AND.ITAG(NUMNOD+N4)>=1) THEN ! IF ALL NODES BELONGING TO 1 ACTIF ELEMENT
                 NIND = NIND + 1
                 NINDL(1,NIND) = IAD+4*(N-1)
                 NINDL(2,NIND) = NUMPRESLOAD
              ENDIF
           ENDDO
       ENDDO
C--------------------------------------------------------
C     SEARCH IF SEGMENT ELEMENT IS DELETED 
C--------------------------------------------------------
C
      DO N = 1, NIND
        I = NINDL(1,N)
        IF(N <= NINDPLOAD) THEN
           N1 = IB(1,I)
           N2 = IB(2,I)
           N3 = IB(3,I)
           N4 = IB(4,I)
        ELSE
           N1 = LLOADP(I)
           N2 = LLOADP(I+1)
           N3 = LLOADP(I+2)
           N4 = LLOADP(I+3)
        ENDIF
        IF(N3 == 0) N3 = N2
        IF(N4 == 0) N4 = N3

        DO J = ADDCNEL(N1),ADDCNEL(N1+1)-1
          II = CNEL(J)
          IF(TAGEL(II) > 0) THEN    !    elt actif found
           ITAGL(N1) = 0
           ITAGL(N2) = 0
           ITAGL(N3) = 0
           ITAGL(N4) = 0
           IF(II<=OFQ) THEN ! Solid Actif
             DO K = 2, 9
               IX = IXS(K,II)
               ITAGL(IX) = 1
             END DO
           ELSEIF(II > OFQ.AND.II<=OFC) THEN ! Quad actif
             II = II - OFQ
             DO K=2,5
               IX = IXQ(K,II)
               ITAGL(IX)=1
             END DO
           ELSEIF(II > OFC.AND.II<=OFT) THEN ! shell actif
             II = II - OFC
             DO K=2,5
               IX = IXC(K,II)
               ITAGL(IX)=1
             END DO
           ELSEIF(II > OFTG.AND.II<=OFUR)THEN ! triangle actif
             II = II - OFTG
             DO K=2,4
               IX = IXTG(K,II)
               ITAGL(IX) = 1
             END DO
           ELSEIF(II > OFT.AND.II<=OFP)THEN ! truss actif
             II = II - OFT
             DO K=2,3
               IX = IXT(K,II)
               ITAGL(IX) = 1
             ENDDO
           ELSEIF(II > OFP.AND.II<=OFR)THEN ! Beam actif
             II = II - OFP
             DO K=2,3
               IX = IXP(K,II)
               ITAGL(IX) = 1
             ENDDO
           ELSEIF(II > OFR.AND.II<=OFTG)THEN ! Spring actif
             II = II - OFR
             DO K=2,3
               IX = IXR(K,II)
               ITAGL(IX) = 1
             ENDDO
             IF(NINT(GEO(12,IXR(1,II))) == 12) THEN ! Spring actif
               IX = IXR(4,II)
               ITAGL(IX) = 1
             ENDIF
           END IF

           IF(ITAGL(N1)+ITAGL(N2)+ITAGL(N3)+ITAGL(N4) == 4)THEN ! SEGMENT IS BELONGING TO ACTIF ELEMENT
             GOTO 400
           END IF
          END IF
        END DO
C No element actif 
        IF(NSPMD == 1) THEN
          IF(N <= NINDPLOAD) THEN
            IB(8,I) = 1
          ELSE
            JJ = NINDL(2,N)
            LOADP_TAGDEL(JJ) = 1
          ENDIF

        ELSE
C Comm en spmd needed : look if nodes belonging to another element actif of another proc
#include "lockon.inc"
          NINDG = NINDG + 1
          NIND2 = NINDG
C
          IF(N <= NINDPLOAD) THEN
              NINDEX(NIND2) = I
          ELSE
              JJ = NINDL(2,N)
              NINDEX(NIND2) = -JJ
          ENDIF
          IBUFS(4*(NIND2-1)+1 ) = ITAB(N1)
          IBUFS(4*(NIND2-1)+2 ) = ITAB(N2)
          IBUFS(4*(NIND2-1)+3 ) = ITAB(N3)
          IBUFS(4*(NIND2-1)+4 ) = ITAB(N4)

#include "lockoff.inc"

        END IF

 400    CONTINUE

      END DO

C
      CALL MY_BARRIER()
C
C
C Partie non parallele

      IF(NSPMD > 1) THEN

!$OMP SINGLE

C
C SPMD communication : if a node is not in the same proc as element
C

       CALL SPMD_INIT_IDEL(4*NINDG  , IRSIZE, IRECV,IAD_ELEM)
       CALL SPMD_EXCHSEG_IDEL(
     1         IBUFS  ,4*NINDG   ,IXS    ,IXC    ,IXTG   , 
     2         IXQ    ,IPARG     ,ITAGL  ,ITABM1 ,TAGEL  ,
     3         IRSIZE ,IRECV     ,CNEL   ,ADDCNEL,OFC    ,
     4         OFT    ,OFTG      ,OFUR   ,OFR    ,OFP    ,
     5         OFQ    ,NINDG     ,IXP    ,IXR    ,IXT    ,
     6         GEO    ,IAD_ELEM)

C
C If no element actif after spmd comm
C
       DO J = 1, NINDG
          NN = IBUFS(J)
          IF(NN == 0) THEN
            I  = NINDEX(J)
C Segment is deleted
            IF(I > 0) THEN
               IB(8,I) = 1
            ELSE
               LOADP_TAGDEL(-I) = 1
            ENDIF
          END IF
       END DO

C Fin Partie non parallele
!$OMP END SINGLE
      ENDIF 

C
      DEALLOCATE(NINDL)      
C
      RETURN
      END

