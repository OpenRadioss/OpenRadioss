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
Chd|  I25NEIGH                      source/interfaces/inter3d1/i25neigh.F
Chd|-- called by -----------
Chd|        ININT3                        source/interfaces/inter3d1/inint3.F
Chd|-- calls ---------------
Chd|        I25NEIGH_MSG_ERR              source/interfaces/inter3d1/i25neigh.F
Chd|        I25NEIGH_SEG_EN               source/interfaces/inter3d1/i25neigh.F
Chd|        MY_ORDERS                     ../common_source/tools/sort/my_orders.c
Chd|        BITGET                        source/interfaces/inter3d1/bitget.F
Chd|        BITSET                        source/interfaces/inter3d1/bitget.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE I25NEIGH(NRTM ,NSN   ,NSV     ,IRECT  ,IRTLM,
     2                         MVOISIN,EVOISIN ,MSEGLO ,MSEGTYP,ITAB   ,
     3                         X      ,ID      ,TITR   ,IGEO   ,NADMSR ,
     4                         ADMSR  ,ADSKYN  ,IADNOR ,NRTM_SH,IEDGE  ,
     5                         NEDGE  ,LEDGE   ,LBOUND ,EDG_COS,NISUB  ,
     6                         LISUB  ,ADDSUBM,LISUBM  ,INFLG_SUBM ,NISUBE,
     7                         ADDSUBE,LISUBE ,INFLG_SUBE,NOINT,NMN,MSR,
     8                         NOM_OPT,ILEV   ,MBINFLG ,EBINFLG,IELEM_M,
     9                         IDEL_SOLID)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MY_ALLOC_MOD
      USE MESSAGE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "units_c.inc"
#include      "param_c.inc"
#include      "scr03_c.inc"
#include      "scr17_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NRTM,NSN,NMN,NSV(*),IRECT(4,NRTM),MVOISIN(4,NRTM),EVOISIN(4,NRTM),
     .        MSEGLO(NRTM),IRTLM(4,NSN),MSEGTYP(NRTM),ITAB(*),
     .        IGEO(NPROPGI,*), NADMSR, ADMSR(4,NRTM), ADSKYN(4*NRTM+1),
     .        IADNOR(4,NRTM), NRTM_SH, IEDGE, NEDGE, LEDGE(NLEDGE,*), 
     .        LBOUND(*), 
     .        NISUB, LISUB(*), ADDSUBM(*), LISUBM(*), INFLG_SUBM(*), 
     .        NISUBE, ADDSUBE(*), LISUBE(*), INFLG_SUBE(*), MSR(*),
     .        ILEV, MBINFLG(*), EBINFLG(*)
      INTEGER NOINT, NOM_OPT(LNOPT1,*)
      INTEGER ID
      my_real
     .   X(3,*), EDG_COS
      CHARACTER*nchartitle,
     .   TITR
      INTEGER  , INTENT(IN) :: IDEL_SOLID         ! solid erosion
      INTEGER  , INTENT(IN) :: IELEM_M(2,NRTM)    ! elements connected to the segment ( espacially solid)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,K,L,IW,I1,I2,I3,I4,M,N,NMAX,E_MAX,E_ID,N_EI,
     1        NE0,NRTM0,ISH,NVERTX, IVERTX, JVERTX,
     2        J1,J2,J3,J4,K1,K2,L1,L2,KPERM1(4),KPERM2(4),IRR,
     3        NFT,JLT,MI,II(4),JJ(4),IEDG,JEDG,IOK,       
     4        IBASE, KBASE, KOLD, FIN, NE, EJ, NEDGE_TMP, 
     5        SOL_EDGE, SH_EDGE, ISTORE,
     6        LL, KK, CUR, NEXT, JSUB, KSUB, IMS1, IMS2, IMS3, IMS4, 
     7        NISUBN, INFLG, MAXADD, STAT
      INTEGER IX1, IX2, IX3, IX4,
     ,        JX1, JX2, JX3, JX4,
     .        NA, NB, EA, EB,JM,MJ
      INTEGER, DIMENSION(:),ALLOCATABLE :: MVOI
      INTEGER, DIMENSION(:,:),ALLOCATABLE :: EIDNOD
      my_real
     .     XA1,XA2,XA3,XA4,
     .     YA1,YA2,YA3,YA4,
     .     ZA1,ZA2,ZA3,ZA4,
     .     XB1,XB2,XB3,XB4,
     .     YB1,YB2,YB3,YB4,
     .     ZB1,ZB2,ZB3,ZB4,
     .     X01,Y01,Z01,X02,Y02,Z02,
     .     XNA, YNA, ZNA, XNB, YNB, ZNB, AAA, BBB, ANG
      INTEGER WORK(70000)
      INTEGER,  DIMENSION(:,:),ALLOCATABLE :: CLEF,LEDGE_TMP1,LEDGE_TMP2
      INTEGER,  DIMENSION(:),ALLOCATABLE :: INDEX, ITAG, NTAG, ROOT
      INTEGER, DIMENSION(:), ALLOCATABLE :: MLOC,KAD,ADDSUBN,ADDSUBN_TMP,
     .                                      LISUBN,INFLG_SUBN,IXSUB
      INTEGER, DIMENSION(:,:,:),ALLOCATABLE :: MNEIGH_SOLID
C-----------------------------------------------
C   E x t e r n a l   F u n c t i o n s
C-----------------------------------------------
      INTEGER BITSET, BITGET
      EXTERNAL BITSET, BITGET
C-----------------------------------------------
      DATA KPERM1/1,3,5,7/
      DATA KPERM2/2,4,6,8/
C-----------------------------------------------
      CALL MY_ALLOC(ITAG,NUMNOD)
      CALL MY_ALLOC(NTAG,4*NRTM)
      CALL MY_ALLOC(ROOT,4*NRTM)
C
      DO I=1,NRTM
        MSEGLO(I)=I      
      ENDDO
C-------------------------------------------------------
C     Neighbhoors researching
C       shell segs have been duplicated w/ inverse order
C       for the moment all antisymmetry surface will be stored at the end
C       for Solids : searching of neighbhoors is only done for external segments
C-----------------------------------------------
C
      DO I=1,NUMNOD
        ITAG(I)=0
      ENDDO
      DO I=1,NRTM
        IF(IELEM_M(2,I) == 0) THEN

          DO J=1,3
            M=IRECT(J,I)
            ITAG(M)=ITAG(M)+1
          END DO
          IF (IRECT(4,I)/=IRECT(3,I))THEN
            M= IRECT(4,I)
            ITAG(M)=ITAG(M)+1
          END IF
        ENDIF
      END DO
C-----MSEGTYP (> NRTM for i=NRTM0+1,NRTM0+NRTM_SH) -> IM2SH = MSEGTYP-NRTM---------
C-----------max number of connected segment per node
      NMAX=0
      DO I=1,NUMNOD
       NMAX=MAX(NMAX,ITAG(I))
       ITAG(I)=0
      ENDDO
      ALLOCATE(MVOI(NMAX+10),EIDNOD(NMAX,NUMNOD))
      MVOI(1:NMAX+10)=0
      EIDNOD(1:NMAX,1:NUMNOD)=0
C------------ini- E_ids of each node
      DO I=1,NRTM
        IF(IELEM_M(2,I) == 0) THEN

          DO J=1,3
            M=IRECT(J,I)
            ITAG(M)=ITAG(M)+1
            EIDNOD(ITAG(M),M)=I
          END DO
          IF (IRECT(4,I)/=IRECT(3,I)) THEN
            M= IRECT(4,I)
            ITAG(M)=ITAG(M)+1
            EIDNOD(ITAG(M),M)=I
          END IF
        ENDIF
      END DO
C------------MVOISIN-(seg number) ---
      E_MAX=4
      DO I=1,NRTM
       DO J=1,E_MAX
        MVOISIN(J,I)=0
       END DO
      END DO
C
      DO I=1,NRTM
        IF(IELEM_M(2,I) == 0) THEN
          N_EI=0
C----seg 1-2------
          I1 =IRECT(1,I)
          I2 =IRECT(2,I)
          CALL I25NEIGH_SEG_EN(ITAG(I1),EIDNOD(1,I1),ITAG(I2),EIDNOD(1,I2),MVOISIN,
     1                    N_EI,MVOI ,I   ,I1 ,I2 ,IRECT,
     2                    X  ,MVOISIN(1,I),NRTM,MSEGTYP,IRR   )
          IF (IRR >0) CALL I25NEIGH_MSG_ERR(I1,I2,ITAB,IRR)
C----seg 2-3------
          I1 =IRECT(2,I)
          I2 =IRECT(3,I)
          CALL I25NEIGH_SEG_EN(ITAG(I1),EIDNOD(1,I1),ITAG(I2),EIDNOD(1,I2),MVOISIN,
     1                    N_EI,MVOI ,I   ,I1 ,I2 ,IRECT,
     2                    X  ,MVOISIN(1,I),NRTM,MSEGTYP,IRR   )
          IF (IRR >0) CALL I25NEIGH_MSG_ERR(I1,I2,ITAB,IRR)
C----seg 3-4------
          I1 =IRECT(3,I)
          I2 =IRECT(4,I)
          CALL I25NEIGH_SEG_EN(ITAG(I1),EIDNOD(1,I1),ITAG(I2),EIDNOD(1,I2),MVOISIN,
     1                  N_EI,MVOI ,I   ,I1 ,I2 ,IRECT,
     2                  X  ,MVOISIN(1,I),NRTM,MSEGTYP,IRR  )
          IF (IRR >0) CALL I25NEIGH_MSG_ERR(I1,I2,ITAB,IRR)
C----seg 1-4------
          I1 =IRECT(4,I)
          I2 =IRECT(1,I)
          CALL I25NEIGH_SEG_EN(ITAG(I1),EIDNOD(1,I1),ITAG(I2),EIDNOD(1,I2),MVOISIN,
     1                    N_EI,MVOI ,I   ,I1 ,I2 ,IRECT,
     2                    X  ,MVOISIN(1,I),NRTM,MSEGTYP,IRR   )
          IF (IRR >0) CALL I25NEIGH_MSG_ERR(I1,I2,ITAB,IRR)
C
           MVOI(1:N_EI)=0 ! Reset MVOI
C
        ENDIF
      END DO !I=1,NRTM     
C
C--------------------------------------------
      DO I=1,NRTM
        DO J=1,4
          L = MVOISIN(J,I)
          IF(L/=0) THEN
            DO K=1,4
              IF(MVOISIN(K,L)==I) GOTO 120
            END DO
            WRITE(ISTDO,'(A,/,10I10)') 
     .                      'i25inisu_nei - internal error : a segment is not neighboring its neighbor segments...',
     .                      i,(itab(irect(m,i)),m=1,4),
     .                      l,(itab(irect(m,l)),m=1,4)
 120        CONTINUE
          END IF
        END DO
      END DO !I=1,NRTM
C--------------------------------------------
      DEALLOCATE(MVOI,EIDNOD)
C--------------------------------------------
C------------------------------------------------------
C        Solid erosion : 
C        if internal segments of solids are defined : need to to buid ADMSR for all NRTM 
C        Store all segments connected to the same edge for solid segments
C-----------------------------------------------------
      IF(IDEL_SOLID > 0) THEN  
        DO I=1,NUMNOD
          ITAG(I)=0
        ENDDO
        DO I=1,NRTM  
          IF(IELEM_M(1,I) <= NUMELS) THEN

            DO J=1,3
              M=IRECT(J,I)
              ITAG(M)=ITAG(M)+1
            END DO
            IF (IRECT(4,I)/=IRECT(3,I))THEN
               M= IRECT(4,I)
               ITAG(M)=ITAG(M)+1
            END IF
          ENDIF
        END DO
C        
        NMAX=0
        DO I=1,NUMNOD
         NMAX=MAX(NMAX,ITAG(I))
         ITAG(I)=0
        ENDDO
        ALLOCATE(EIDNOD(NMAX,NUMNOD))
        EIDNOD(1:NMAX,1:NUMNOD)=0
        ALLOCATE(MVOI(NMAX+10))
        MVOI(1:NMAX+10)=0
C
        ALLOCATE(MNEIGH_SOLID(NMAX,4,NRTM))
        MNEIGH_SOLID(1:NMAX,1:4,1:NRTM) = 0
        DO I=1,NRTM
          IF(IELEM_M(1,I) <= NUMELS) THEN

            DO J=1,3
              M=IRECT(J,I)
              ITAG(M)=ITAG(M)+1
              EIDNOD(ITAG(M),M)=I
            END DO
            IF (IRECT(4,I)/=IRECT(3,I)) THEN
              M= IRECT(4,I)
              ITAG(M)=ITAG(M)+1
              EIDNOD(ITAG(M),M)=I
            END IF
          ENDIF
        END DO
C
C-------------neighbour segments-----------       
C
        DO I=1,NRTM 
          IF(IELEM_M(1,I) <= NUMELS) THEN
            N_EI=0
C----seg 1-2------
            I1 =IRECT(1,I)
            I2 =IRECT(2,I)
            CALL I25NEIGH_SEG_E(ITAG(I1),EIDNOD(1,I1),ITAG(I2),EIDNOD(1,I2),N_EI,MVOI,I,
     .                          I1,I2,IRECT,NRTM,MSEGTYP ,MVOISIN)
            MNEIGH_SOLID(1:N_EI,1,I) = MVOI(1:N_EI)
            NE0 =N_EI
C----seg 2-3------
            I1 =IRECT(2,I)
            I2 =IRECT(3,I)
            CALL I25NEIGH_SEG_E(ITAG(I1),EIDNOD(1,I1),ITAG(I2),EIDNOD(1,I2),N_EI,MVOI,I,
     .                          I1,I2,IRECT,NRTM,MSEGTYP ,MVOISIN)
            MNEIGH_SOLID(1:N_EI,2,I) = MVOI(NE0+1:N_EI)
            NE0 =N_EI
C----seg 3-4------
            I1 =IRECT(3,I)
            I2 =IRECT(4,I)
            CALL I25NEIGH_SEG_E(ITAG(I1),EIDNOD(1,I1),ITAG(I2),EIDNOD(1,I2),N_EI,MVOI,I,
     .                          I1,I2,IRECT,NRTM,MSEGTYP ,MVOISIN)
            MNEIGH_SOLID(1:N_EI,3,I) = MVOI(NE0+1:N_EI)
            NE0 =N_EI
C----seg 1-4------
            I1 =IRECT(4,I)
            I2 =IRECT(1,I)
            CALL I25NEIGH_SEG_E(ITAG(I1),EIDNOD(1,I1),ITAG(I2),EIDNOD(1,I2),N_EI,MVOI,I,
     .                          I1,I2,IRECT,NRTM,MSEGTYP ,MVOISIN)
            MNEIGH_SOLID(1:N_EI,4,I) = MVOI(NE0+1:N_EI)

C
            MVOI(1:N_EI)=0 ! Reset MVOI    
          ENDIF      
        ENDDO

        DEALLOCATE(MVOI,EIDNOD)

      ENDIF
      DEALLOCATE(ITAG)

C--------------------------------------------


      NVERTX=0
      DO I=1,NRTM
        DO J=1,3
          NVERTX      =NVERTX+1
          ADMSR(J,I)  =NVERTX
          ROOT(NVERTX)=NVERTX
        END DO
        IF(IRECT(4,I)/=IRECT(3,I))THEN
          NVERTX      =NVERTX+1
          ADMSR(4,I)  =NVERTX
          ROOT(NVERTX)=NVERTX
        ELSE
          ADMSR(4,I)=ADMSR(3,I)
        END IF
      END DO
C
      DO I=1,NRTM
C
        II(1:4)=IRECT(1:4,I)
C
        DO IEDG=1,4
C
          IF(II(4)==II(3).AND.IEDG==3) CYCLE
          I1=IEDG
          I2=MOD(IEDG,4)+1
C
          MI=MVOISIN(IEDG,I)
          IF(MI/=0)THEN
C
            JJ(1:4)=IRECT(1:4,MI)
C
            IOK=0
            DO JEDG=1,4
C
              IF(JJ(4)==JJ(3).AND.JEDG==3) CYCLE
C
              J1=JEDG
              J2=MOD(JEDG,4)+1
              IF(II(I2)==JJ(J1).AND.II(I1)==JJ(J2))THEN

                IVERTX=MIN(ROOT(ADMSR(I1,I)),ROOT(ADMSR(J2,MI)))
                JVERTX=MAX(ROOT(ADMSR(I1,I)),ROOT(ADMSR(J2,MI)))
                IF(JVERTX/=IVERTX) ROOT(JVERTX)=IVERTX

                IVERTX=MIN(ROOT(ADMSR(I2,I)),ROOT(ADMSR(J1,MI)))
                JVERTX=MAX(ROOT(ADMSR(I2,I)),ROOT(ADMSR(J1,MI)))
                IF(JVERTX/=IVERTX) ROOT(JVERTX)=IVERTX

                IOK=1

                EVOISIN(IEDG,I)=JEDG

              ELSEIF(II(I1)==JJ(J1).AND.II(I2)==JJ(J2))THEN
                print *,'i25inisu_nei - internal error : non-consistent neighboring segment'

              END IF
            END DO

            IF(IOK==0)
     .        WRITE(ISTDO,*) 'i25inisu_nei - internal error : no common edge w/neighboring segment',
     .                                        itab(ii(1)),itab(ii(2)),itab(ii(3)),itab(ii(4)),
     .                                        itab(jj(1)),itab(jj(2)),itab(jj(3)),itab(jj(4))
          END IF ! IF(MI/=0)THEN
C
          IF(IELEM_M(1,I) <= NUMELS.AND.IDEl_SOLID > 0) THEN
            DO K=1,NMAX
              MJ = MNEIGH_SOLID(K,IEDG,I)
              IF(MJ/=0.AND.MJ/=MI) THEN
C
                JJ(1:4)=IRECT(1:4,MJ)
C
                IOK=0
                DO JEDG=1,4
C
                  IF(JJ(4)==JJ(3).AND.JEDG==3) CYCLE
C
                  J1=JEDG
                  J2=MOD(JEDG,4)+1
                  IF(II(I2)==JJ(J1).AND.II(I1)==JJ(J2))THEN
                      
                    IVERTX=MIN(ROOT(ADMSR(I1,I)),ROOT(ADMSR(J2,MJ)))
                    JVERTX=MAX(ROOT(ADMSR(I1,I)),ROOT(ADMSR(J2,MJ)))
                    IF(JVERTX/=IVERTX) ROOT(JVERTX)=IVERTX
      
                    IVERTX=MIN(ROOT(ADMSR(I2,I)),ROOT(ADMSR(J1,MJ)))
                    JVERTX=MAX(ROOT(ADMSR(I2,I)),ROOT(ADMSR(J1,MJ)))
                    IF(JVERTX/=IVERTX) ROOT(JVERTX)=IVERTX
      
                    IOK=1
            
c                 ELSEIF(II(I1)==JJ(J1).AND.II(I2)==JJ(J2))THEN
c                      print *,'i25inisu_nei - internal error : non-consistent neighboring segment'
      
                  END IF
                END DO
      
c               IF(IOK==0)
c           .        WRITE(ISTDO,*) 'i25inisu_nei - internal error : no common edge w/neighboring segment',
c           .                                        itab(ii(1)),itab(ii(2)),itab(ii(3)),itab(ii(4)),
c           .                                        itab(jj(1)),itab(jj(2)),itab(jj(3)),itab(jj(4))
c                END IF ! IF(MJ/=0.AND.MJ/=MI)THEN
              ENDIF
            ENDDO
          ENDIF
        END DO
C
      END DO
C
      IF(IDEl_SOLID > 0) DEALLOCATE(MNEIGH_SOLID)
C
      DO I=1,NVERTX
        J=I
        DO WHILE(ROOT(J) < J)
          J=ROOT(J)
        END DO
        ROOT(I)=J
      END DO
C
      NADMSR=0
      NTAG(1:NVERTX)=0
      DO I=1,NVERTX
        J=ROOT(I)
        IF(NTAG(J)==0)THEN
          NADMSR =NADMSR+1
          NTAG(J)=NADMSR
        END IF
      END DO
C
      DO I=1,NRTM
        ADMSR(1,I)=ROOT(ADMSR(1,I))
        ADMSR(1,I)=NTAG(ADMSR(1,I))
        ADMSR(2,I)=ROOT(ADMSR(2,I))
        ADMSR(2,I)=NTAG(ADMSR(2,I))
        ADMSR(3,I)=ROOT(ADMSR(3,I))
        ADMSR(3,I)=NTAG(ADMSR(3,I))
        ADMSR(4,I)=ROOT(ADMSR(4,I))
        ADMSR(4,I)=NTAG(ADMSR(4,I))
      END DO
C-------------------------------------------------
C     Compute addresses for Parith/ON assembling of the normals
C-------------------------------------------------
      NTAG(1:4*NRTM)=0
      DO I=1,NRTM
        I1 = ABS(ADMSR(1,I))
        I2 = ABS(ADMSR(2,I))
        I3 = ABS(ADMSR(3,I))
        I4 = ABS(ADMSR(4,I))
        NTAG(I1) = NTAG(I1) + 1
        NTAG(I2) = NTAG(I2) + 1
        NTAG(I3) = NTAG(I3) + 1
        IF(I4/=I3) NTAG(I4) = NTAG(I4) + 1
      END DO

      ADSKYN(1) = 1
      DO N=1,NADMSR
        ADSKYN(N+1) = ADSKYN(N)+NTAG(N)
      END DO

      DO N=1,NRTM
        I1 = ABS(ADMSR(1,N))
        I2 = ABS(ADMSR(2,N))
        I3 = ABS(ADMSR(3,N))
        I4 = ABS(ADMSR(4,N))
        IF(IRECT(3,N)/=IRECT(4,N))THEN
          IADNOR(1,N)=ADSKYN(I1)
          ADSKYN(I1) = ADSKYN(I1)+1
          IADNOR(2,N)=ADSKYN(I2)
          ADSKYN(I2) = ADSKYN(I2)+1
          IADNOR(3,N)=ADSKYN(I3)
          ADSKYN(I3) = ADSKYN(I3)+1
          IADNOR(4,N)=ADSKYN(I4)
          ADSKYN(I4) = ADSKYN(I4)+1
        ELSE
          IADNOR(1,N)=ADSKYN(I1)
          ADSKYN(I1) = ADSKYN(I1)+1
          IADNOR(2,N)=ADSKYN(I2)
          ADSKYN(I2) = ADSKYN(I2)+1
          IADNOR(3,N)=ADSKYN(I3)
          ADSKYN(I3) = ADSKYN(I3)+1
        END IF
      END DO

C
C     Reset ADSKYN
      ADSKYN(1) = 1
      DO N=1,NADMSR
        ADSKYN(N+1) = ADSKYN(N)+NTAG(N)
      END DO
C      
      DEALLOCATE(NTAG,ROOT)
C
      NRTM0=NRTM-NRTM_SH
C
      NEDGE =0 
      LBOUND(1:NADMSR)=0
C-----------------------------------------------------------------------
C     LEDGE(1:NLEDGE)
C       1: "left" segment number <=> index dans IRECT(1:NRTM)
C       2: local number of the edge within "left" segment
C       3: "right" segment number
C       4: local number of the edge within "right" segment     ! Squeezing T, X.. shapes wrt rupture
C       5: I1 = numero local du noeud 1 dans NSV(1:NSN)
C       6: I2 = numero local du noeud 1 dans NSV(1:NSN)
C       7: Type of the edge :
C             < 0 <=> l'arete est uniquement main, pas second
C             +/-1 arete de solide
C             +/-2 arete de coque
C for engine:
C       8: Global ID (id in starter)
C       9: Weight = 1 => secondary secnd handled by current domain
C      10: orientation flag left seg
C      11: first node of left seg
C      12: second node left seg
C      13: orientation flag right seg
C      14: first node of right seg
C      15: second node right seg

C-----------------------------------------------------------------------
      ALLOCATE(LEDGE_TMP1(NLEDGE,4*NRTM))
C
      DO I=1,NRTM
        IF(IELEM_M(2,I) == 0) THEN
          IF(I <= NRTM0)THEN
            IBASE=I
          ELSE
           IBASE=-MSEGTYP(I)
          ! IF(IBASE > 0) THEN
            IF(IBASE > NRTM)IBASE=IBASE-NRTM
          ! END IF
          END IF

          DO J=1,4
            I1=IRECT(J,I)
            I2=IRECT(MOD(J,4)+1,I)
C
            K=MVOISIN(J,I)
C
            IF(K <= NRTM0)THEN
              KBASE=K ! y-compris K=0
            ELSE
              KBASE=-MSEGTYP(K)
            ! IF(KBASE > 0) THEN
              IF(KBASE > NRTM)KBASE=KBASE-NRTM
            ! END IF
            END IF
C    
            IF(KBASE < IBASE)THEN

              IF(.NOT.(I1==I2.AND.J==3))THEN
C
                NEDGE=NEDGE+1
                LEDGE_TMP1(1,NEDGE)=I
                LEDGE_TMP1(2,NEDGE)=J
                LEDGE_TMP1(3,NEDGE)=K
                LEDGE_TMP1(4,NEDGE)=0
                IF(ITAB(I1) < ITAB(I2))THEN
                  LEDGE_TMP1(5,NEDGE)=I1
                  LEDGE_TMP1(6,NEDGE)=I2
                ELSE
                  LEDGE_TMP1(5,NEDGE)=I2
                  LEDGE_TMP1(6,NEDGE)=I1
                END IF
C
                IF(K/=0)THEN 
                  IF(MSEGTYP(I)==0.AND.MSEGTYP(K)==0)THEN ! Solid only
                    LEDGE_TMP1(7,NEDGE)=1 ! arete solide-solide
                  ELSE
                    LEDGE_TMP1(7,NEDGE)=2 ! arete coque-coque ou coque-solide
                  END IF
                ELSE ! Bord
                  LEDGE_TMP1(7,NEDGE)=2
                END IF
C
                IF(K/=0)THEN
                  DO L=1,4
                    K1=IRECT(L,K)
                    K2=IRECT(MOD(L,4)+1,K)
                    IF(.NOT.(K1==K2.AND.L==3).AND.((K1==I1.AND.K2==I2).OR.(K2==I1.AND.K1==I2))) THEN
                      LEDGE_TMP1(4,NEDGE)=L
                    END IF
                  END DO
                  IF(LEDGE_TMP1(4,NEDGE)==0)THEN
                    WRITE(ISTDO,'(A)') 
     .                      'i25inisu_nei - internal error : could not find the edge on neighboring element'
                  END IF
                END IF
C
              END IF


            END IF
C
            IF(K==0)THEN
              IF(.NOT.(I1==I2.AND.J==3))THEN
C
                LBOUND(ADMSR(J,I))=1
                LBOUND(ADMSR(MOD(J,4)+1,I))=1
C
              END IF
            END IF
          END DO
        ENDIF
      END DO
C-----------------------------------------------------------------------
C
C     Only 1 edge upon upper and lower skin is retained
C           Except for T, X... connections <=> keeping all edges between vertices (cf ADMSR)
C
C           x-------x-------x
C                   |  Only 1 Edge
C           x-------x-------x
C
C
C                   x
C                   | 
C              edge | edge
C           x-------x-------x
C                   |              T shape <=> 3 edges, X shape <=> 4 edges, ...
C           x-------x-------x
C                 edge
C
C-----------------------------------------------------------------------
C
C           For T, X ... shapes
C           If 1 element fails, free edges may appear while it is not really the case
C                   x                                 x
C                   |                                 | 
C              edge | edge                  free edge | edge
C           x-------x-------x                         x-------x
C                   |            ====>                |        
C           x-------x-------x                         x-------x
C                 edge                            free edge
C           It is considered as it is not a real problem
C
C-----------------------------------------------------------------------
      ALLOCATE(CLEF(4,NEDGE),LEDGE_TMP2(NLEDGE,NEDGE),INDEX(2*NEDGE))
      DO I=1,NEDGE

       I1 = LEDGE_TMP1(5,I)
       I2 = LEDGE_TMP1(6,I)
C
       NE  =LEDGE_TMP1(1,I)
       EJ  =LEDGE_TMP1(2,I)

       IF(NE <= NRTM0)THEN
         IBASE=NE
       ELSE
         IBASE=-MSEGTYP(NE)
         IF(IBASE > NRTM)IBASE=IBASE-NRTM
       END IF
C
       NE  =LEDGE_TMP1(3,I)
       EJ  =LEDGE_TMP1(4,I)

       IF(NE <= NRTM0)THEN
         KBASE=NE ! y-compris NE=0
       ELSE
         KBASE=-MSEGTYP(NE)
         IF(KBASE > NRTM)KBASE=KBASE-NRTM
       END IF

       INDEX(I)  = I

       CLEF(1,I) = I1
       CLEF(2,I) = I2
       CLEF(3,I) = IBASE
       CLEF(4,I) = KBASE

      END DO
C
      CALL MY_ORDERS(0,WORK,CLEF,INDEX,NEDGE,4)
C
      NEDGE_TMP = NEDGE
      LEDGE_TMP2(1:NLEDGE,1:NEDGE)=LEDGE_TMP1(1:NLEDGE,1:NEDGE)
C
      NEDGE = 0
C
      I=1
      DO WHILE(I <= NEDGE_TMP)
        NEDGE = NEDGE+1
        KOLD  = INDEX(I)
        LEDGE_TMP1(1:NLEDGE,NEDGE)=LEDGE_TMP2(1:NLEDGE,KOLD)

        FIN = 0
        DO WHILE (FIN == 0 .AND. I < NEDGE_TMP) 
          I = I+1
          K = INDEX(I)
          IF(CLEF(1,K)/=CLEF(1,KOLD).OR.
     .       CLEF(2,K)/=CLEF(2,KOLD).OR.
     .       CLEF(3,K)/=CLEF(3,KOLD).OR.
     .       CLEF(4,K)/=CLEF(4,KOLD))THEN
            FIN=1
          END IF
          KOLD=K
        END DO

        IF(I==NEDGE_TMP .AND. FIN==0) I=I+1 ! Exit    

      END DO

C
      NEDGE_TMP = NEDGE
      NEDGE     = 0

      SOL_EDGE =IEDGE/10 ! solids
      SH_EDGE  =IEDGE-10*SOL_EDGE ! shells
C
      DO I=1,NEDGE_TMP
C
C       IEDG= 1 : Edge to edge contact is activated using the external border edges from surf_ID1 and surf_ID2
C                       &  Edge to edge contact is not activated for edges supported by solids only 
C       IEDG= 2 : Edge to edge contact is activated using all edges supported by shell elements from surf_ID1 and surf_ID2
C                     & Edge to edge contact is not activated for edges supported by solids only 
C       IEDG= 10 : Edge to edge contact is activated using sharp edges between contact solid segments
C                     & Edge to edge contact is not activated for edges supported by shells only 
C       IEDG= 20: Edge to edge contact is activated using all edges between contact solid segments
C                      & Edge to edge contact is not activated for edges supported by shells only
C       And then the combinations 11, 12, 21 and 22 (let, 1st digit for solids, 2nd for shells)
C
        ISTORE=0
        IF(LEDGE_TMP1(7,I)==1)THEN ! edge between contact solid segments
          IF(SOL_EDGE==1.OR.SOL_EDGE==3)THEN
C
            NA  =LEDGE_TMP1(1,I)
            EA  =LEDGE_TMP1(2,I)
            NB  =LEDGE_TMP1(3,I)
            EB  =LEDGE_TMP1(4,I)

            IF(NA==0 .OR. NB==0) THEN
              print *,' internal error - i25neigh'
            END IF

            IX1 =IRECT(1,NA)
            IX2 =IRECT(2,NA)
            IX3 =IRECT(3,NA)
            IX4 =IRECT(4,NA)
C
            XA1=X(1,IX1)
            YA1=X(2,IX1)
            ZA1=X(3,IX1)
            XA2=X(1,IX2)
            YA2=X(2,IX2)
            ZA2=X(3,IX2)
            XA3=X(1,IX3)
            YA3=X(2,IX3)
            ZA3=X(3,IX3)
            XA4=X(1,IX4)
            YA4=X(2,IX4)
            ZA4=X(3,IX4)
C
            X01 = XA3 - XA1
            Y01 = YA3 - YA1
            Z01 = ZA3 - ZA1
C
            X02 = XA4 - XA2
            Y02 = YA4 - YA2
            Z02 = ZA4 - ZA2
C
            XNA = Y01*Z02 - Z01*Y02
            YNA = Z01*X02 - X01*Z02
            ZNA = X01*Y02 - Y01*X02
C
            AAA=ONE/MAX(EM30,SQRT(XNA*XNA+YNA*YNA+ZNA*ZNA))
            XNA = -XNA*AAA
            YNA = -YNA*AAA
            ZNA = -ZNA*AAA
C
            JX1 =IRECT(1,NB)
            JX2 =IRECT(2,NB)
            JX3 =IRECT(3,NB)
            JX4 =IRECT(4,NB)
C
            XB1=X(1,JX1)
            YB1=X(2,JX1)
            ZB1=X(3,JX1)
            XB2=X(1,JX2)
            YB2=X(2,JX2)
            ZB2=X(3,JX2)
            XB3=X(1,JX3)
            YB3=X(2,JX3)
            ZB3=X(3,JX3)
            XB4=X(1,JX4)
            YB4=X(2,JX4)
            ZB4=X(3,JX4)
C
            X01 = XB3 - XB1
            Y01 = YB3 - YB1
            Z01 = ZB3 - ZB1
C
            X02 = XB4 - XB2
            Y02 = YB4 - YB2
            Z02 = ZB4 - ZB2
C
            XNB = Y01*Z02 - Z01*Y02
            YNB = Z01*X02 - X01*Z02
            ZNB = X01*Y02 - Y01*X02
C
            BBB=ONE/MAX(EM30,SQRT(XNB*XNB+YNB*YNB+ZNB*ZNB))
            XNB = -XNB*BBB
            YNB = -YNB*BBB
            ZNB = -ZNB*BBB
C
            ANG = XNA*XNB+YNA*YNB+ZNA*ZNB
            IF (ANG < EDG_COS) THEN
              ISTORE=1 ! arete vive
            ELSEIF(SOL_EDGE==1)THEN
              LEDGE_TMP1(7,I)=-1 
              ISTORE=1 ! all edges on main side
            END IF
          ELSEIF(SOL_EDGE==2)THEN
            ISTORE=1
          END IF
        ELSEIF(SH_EDGE/=0)THEN
         ISTORE=1 ! all shell edges are stored even if IEDG=1 (because of sorting & rupture)
        END IF


        IF(ISTORE==1)THEN
          NEDGE = NEDGE+1
          LEDGE(1:NLEDGE,NEDGE)=LEDGE_TMP1(1:NLEDGE,I)
        END IF

      END DO
C
      DEALLOCATE(CLEF,INDEX,LEDGE_TMP1,LEDGE_TMP2)
C
c      DO I=1,NRTM
c       print *,'I,MSEGTYP(I)=',I,MSEGTYP(I),itab(irect(1:4,i))
c       print *,'MVOISIN(1,I)=',(MVOISIN(J,I),J=1,4)
c      END DO
c      DO I=1,NEDGE
c       print *,I,LEDGE(1,I),LEDGE(2,I),LEDGE(3,I),LEDGE(4,I),ITAB(LEDGE(5,I)),ITAB(LEDGE(6,I)),LEDGE(7,I)
c      END DO
C
C-----------------------------------------
C     Flag d'appartenance des aretes a S1/S2
C-----------------------------------------
      IF(NEDGE/=0 .AND. ILEV==2)THEN
        EBINFLG(1:NEDGE)=0
        DO I=1,NEDGE
          NE   =LEDGE(1,I)
          INFLG=MBINFLG(NE)
          IMS1=BITGET(INFLG,0)
          IMS2=BITGET(INFLG,1)
          IF(IMS1/=0) EBINFLG(I)=BITSET(EBINFLG(I),0)
          IF(IMS2/=0) EBINFLG(I)=BITSET(EBINFLG(I),1)
          NE=LEDGE(3,I)
          IF(NE/=0)THEN
            INFLG=MBINFLG(NE)
            IMS1=BITGET(INFLG,0)
            IMS2=BITGET(INFLG,1)
            IF(IMS1/=0) EBINFLG(I)=BITSET(EBINFLG(I),0)
            IF(IMS2/=0) EBINFLG(I)=BITSET(EBINFLG(I),1)
          END IF
        END DO
      END IF
C
      IF(NEDGE/=0.AND.NISUB/=0)THEN

        ALLOCATE (MLOC(NUMNOD),KAD(MAX(NMN,NEDGE)),STAT=stat)
        MLOC(1:NUMNOD)=0
        DO I=1,NMN
          N       = MSR(I)
          MLOC(N) = I
        END DO

        ALLOCATE(ADDSUBN_TMP(NMN+1), ADDSUBN(NMN+1))
C-----------------------------------------
C       Calcul de ADDSUBN, LISUBN, INFLG_SUBN pour les noeuds mains
C-----------------------------------------
        ADDSUBN_TMP(1:NMN+1)=0
        DO I=1,NRTM
          DO J=1,4
            IF(.NOT.(J==3.AND.IRECT(3,I)==IRECT(4,I)))THEN
              N = MLOC(IRECT(J,I))
              ADDSUBN_TMP(N)=ADDSUBN_TMP(N)+ADDSUBM(I+1)-ADDSUBM(I)
            END IF
          END DO
        END DO
C
        CUR=1
        DO N=1,NMN
          NEXT    = CUR+ADDSUBN_TMP(N)
          ADDSUBN_TMP(N) = CUR
          CUR     = NEXT
        END DO
        ADDSUBN_TMP(1+NMN)=CUR
C
        NISUBN=ADDSUBN_TMP(1+NMN)-1
        ALLOCATE (LISUBN(NISUBN),INFLG_SUBN(NISUBN),STAT=stat)
        INFLG_SUBN(1:NISUBN)=0
C
C       utilise KAD(1:NMN) pour remplir LISUBN, INFLG_SUBN
        DO N=1,NMN
          KAD(N)=ADDSUBN_TMP(N)
        END DO
        DO I=1,NRTM
          DO J=1,4
            IF(.NOT.(J==3.AND.IRECT(3,I)==IRECT(4,I)))THEN
              N = MLOC(IRECT(J,I))
              DO  KK=ADDSUBM(I),ADDSUBM(I+1)-1
                LISUBN(KAD(N))    =LISUBM(KK)
                INFLG_SUBN(KAD(N))=INFLG_SUBM(KK)
                KAD(N)=KAD(N)+1
              END DO
            END IF
          END DO
        END DO
C-----------------------------------------
C       Compactage de LISUBN ET Combinaison des flags INFLG_SUBN
C-----------------------------------------
        MAXADD = 0
        DO N=1,NMN
          MAXADD=MAX(MAXADD,ADDSUBN_TMP(N+1)-ADDSUBN_TMP(N))
        END DO
        ALLOCATE(IXSUB(2*MAXADD),STAT=stat)
C
        DO N=1,NMN
          KAD(N)=ADDSUBN_TMP(N)
        END DO
C
        DO N=1,NMN
          DO LL = 1,ADDSUBN_TMP(N+1)-ADDSUBN_TMP(N)
             IXSUB(LL) = LL
          ENDDO
C
C 8.1356:  Subscript #1 of the array LISUBN has value 12641 which is greater than the upper bound of 12640
          IF(ADDSUBN_TMP(N+1)-ADDSUBN_TMP(N) > 1 .AND. ADDSUBN_TMP(N) <=NISUBN ) THEN
            CALL MY_ORDERS(0,WORK,LISUBN(ADDSUBN_TMP(N)),IXSUB,ADDSUBN_TMP(N+1)-ADDSUBN_TMP(N),1)
          ENDIF
C
          CUR =0
          DO LL=ADDSUBN_TMP(N),ADDSUBN_TMP(N+1)-1
            KK = ADDSUBN_TMP(N)-1+IXSUB(LL-ADDSUBN_TMP(N)+1)
C
            IF(LISUBN(KK)/=CUR)THEN  
C
C             Combines INFLG
              INFLG=INFLG_SUBN(KK)
              IMS1=BITGET(INFLG,0)
              IF(IMS1/=0) INFLG_SUBN(KAD(N))=
     .                  BITSET(INFLG_SUBN(KAD(N)),0)
              IMS2=BITGET(INFLG,1)
              IF(IMS2/=0) INFLG_SUBN(KAD(N))=
     .                  BITSET(INFLG_SUBN(KAD(N)),1)
C
              CUR           = LISUBN(KK)
              LISUBN(KAD(N))=CUR
              KAD(N)=KAD(N)+1
            ELSE
C
C             Combines INFLG
              INFLG=INFLG_SUBN(KK)
              IMS1=BITGET(INFLG,0)
              IF(IMS1/=0) INFLG_SUBN(KAD(N)-1)=
     .                  BITSET(INFLG_SUBN(KAD(N)-1),0)
              IMS2=BITGET(INFLG,1)
              IF(IMS2/=0) INFLG_SUBN(KAD(N)-1)=
     .                  BITSET(INFLG_SUBN(KAD(N)-1),1)
            END IF
          END DO
C
          ADDSUBN(N)=KAD(N)-ADDSUBN_TMP(N) 
        END DO
C
        CUR=1
        DO N=1,NMN
          NEXT       = CUR+ADDSUBN(N)
          ADDSUBN(N) = CUR
          CUR        = NEXT
        END DO
        ADDSUBN(1+NMN)=CUR
C
        DO N=1,NMN
          DO KK=ADDSUBN(N),ADDSUBN(N+1)-1
            LISUBN(KK)    =LISUBN(ADDSUBN_TMP(N)+KK-ADDSUBN(N))
            INFLG_SUBN(KK)=INFLG_SUBN(ADDSUBN_TMP(N)+KK-ADDSUBN(N))
          END DO
        END DO
C-----------------------------------------
C       Calcul de ADDSUBE, LISUBE, INFLG_SUBE a partir de ADDSUBN, LISUBN, INFLG_SUBN
C-----------------------------------------
        ADDSUBE(1:NEDGE+1)  = 0 
        INFLG_SUBE(1:NISUBE)=0
C
        DO NE=1,NEDGE
          I1  =MLOC(LEDGE(5,NE))
          I2  =MLOC(LEDGE(6,NE))

          LL  =ADDSUBN(I1)
          KK  =ADDSUBN(I2)
          DO WHILE(LL<ADDSUBN(I1+1))
            JSUB=LISUBN(LL)
            DO WHILE(KK<ADDSUBN(I2+1))
              KSUB=LISUBN(KK)   
              IF(KSUB==JSUB)THEN
                ADDSUBE(NE)=ADDSUBE(NE)+1
                KK=KK+1
              ELSE IF(KSUB<JSUB)THEN
                KK=KK+1
              ELSE
                GO TO 100
              END IF
            END DO
 100      CONTINUE
          LL=LL+1
          END DO
        END DO
C
        CUR=1
        DO NE=1,NEDGE
          NEXT       = CUR+ADDSUBE(NE)
          ADDSUBE(NE)= CUR
          CUR        = NEXT
        END DO
        ADDSUBE(1+NEDGE)=CUR
C
C       utilise KAD(1:NEDGE) pour remplir LISUBE, INFLG_SUBE
        DO NE=1,NEDGE
          KAD(NE)=ADDSUBE(NE)
        END DO

        DO NE=1,NEDGE
          I1  =MLOC(LEDGE(5,NE))
          I2  =MLOC(LEDGE(6,NE))

          LL  =ADDSUBN(I1)
          KK  =ADDSUBN(I2)
          DO WHILE(LL<ADDSUBN(I1+1))
            JSUB=LISUBN(LL)
            IMS1 = BITGET(INFLG_SUBN(LL),0)
            IMS2 = BITGET(INFLG_SUBN(LL),1)
            DO WHILE(KK<ADDSUBN(I2+1))
              KSUB=LISUBN(KK)   
              IMS3 = BITGET(INFLG_SUBN(KK),0)
              IMS4 = BITGET(INFLG_SUBN(KK),1)
              IF(KSUB==JSUB)THEN
                LISUBE(KAD(NE))=JSUB
                IF(IMS1==1.AND.IMS3==1) ! edge belongs to S1 is I1 and I2 belong to S1
     .           INFLG_SUBE(KAD(NE))=
     .                BITSET(INFLG_SUBE(KAD(NE)),0)
                IF(IMS2==1.AND.IMS4==1) ! edge belongs to S2 is I1 and I2 belong to S2
     .           INFLG_SUBE(KAD(NE))=
     .                BITSET(INFLG_SUBE(KAD(NE)),1)
                KAD(NE)   =KAD(NE)+1
                KK=KK+1
              ELSE IF(KSUB<JSUB)THEN
                KK=KK+1
              ELSE
                GO TO 200
              END IF
            END DO
 200      CONTINUE
          LL=LL+1
          END DO
        END DO

        DEALLOCATE (MLOC,KAD,ADDSUBN_TMP,ADDSUBN,LISUBN,INFLG_SUBN,IXSUB)
C-------------------------------------
        IF(IPRI>=6) THEN
          WRITE(IOUT,1000)
          WRITE(IOUT,1010)NOINT
          WRITE(IOUT,'(10I10)')
     .         (NOM_OPT(1,NINTER+LISUB(JSUB)),JSUB=1,NISUB)
          WRITE(IOUT,1030)
          DO NE=1,NEDGE
            JSUB=ADDSUBE(NE)
            N   =ADDSUBE(NE+1)-ADDSUBE(NE)
            IF(N>0)THEN
              WRITE(IOUT,'(3I10)')NE,ITAB(LEDGE(5,NE)),ITAB(LEDGE(6,NE))
              WRITE(IOUT,'(30X,2I10)')
     .          (LISUBE(JSUB-1+K),INFLG_SUBE(JSUB-1+K),K=1,N)
            END IF
          END DO
        END IF
      END IF ! IF(NEDGE/=0.AND.NISUB/=0)THEN
C-------------------------------------
 1000 FORMAT(    /1X,'   STRUCTURE OF SUB-INTERFACES OUTPUT TO TH'/
     .            1X,'   ----------------------------------------'// )
 1010 FORMAT(    /1X,'   INTERFACE ID . . . . . . . . . . . . . .',I10/,
     .               '    -> LIST OF SUB-INTERFACES IDS :        ')
 1030 FORMAT(/,'    EDGE    NODE 1    NODE 2'/
     .         '    NUMBER      ID        ID'/
     .       ' -> LIST OF SUB-INTERFACES (LOCAL NUMBERS IN INTERFACE)'/)
C-------------------------------------
      RETURN
      END
Chd|====================================================================
Chd|  I25INI_GAP_N                  source/interfaces/inter3d1/i25neigh.F
Chd|-- called by -----------
Chd|        ININT3                        source/interfaces/inter3d1/inint3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE I25INI_GAP_N(
     1 NRTM  ,IRECT ,IXS   ,GEO   ,IXC   ,IXTG  ,
     2 IXT   ,IXP   ,IPART  ,IPARTC   ,IPARTTG  ,
     3 THK   ,THK_PART,GAP_N ,GAP_M   ,NMN      ,
     4 MSR   ,GAPN_M  ,GAPMAX_M ,GAPSCALE,IGEO  ,
     5 MSEGTYP,GAPMSAV,ITHK25)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "param_c.inc"
#include      "scr17_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NRTM,IRECT(4,*), IXS(NIXS,*), IXC(NIXC,*),
     .   IXTG(NIXTG,*), IXT(NIXT,*), IXP(NIXP,*),
     .   IPART(LIPART1,*), IPARTC(*), IPARTTG(*),
     .   MSR(*),NMN,IGEO(NPROPGI,*),MSEGTYP(*), ITHK25
C     REAL
      my_real
     .   GEO(NPROPG,*), THK(*),THK_PART(*),GAP_N(4,*),GAP_M(*),
     .   GAPN_M(*),GAPMAX_M, GAPSCALE,GAPMSAV(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,K,IW,I1,I2,I3,MG,M,IP,IGTYP
      my_real
     .   DX
      my_real, DIMENSION(:), ALLOCATABLE :: WA
C init 
C
       ALLOCATE(WA(NUMNOD))
       DO I=1,NUMNOD
        WA(I)=ZERO
       END DO
C        
C------------------------------------
C     GAP NOEUDS IN WA
C------------------------------------
       DO I=1,NUMELC
         MG=IXC(6,I)
         IGTYP = IGEO(11,MG)
         IP = IPARTC(I)
         IF ( THK_PART(IP) /= ZERO .AND. IINTTHICK == 0) THEN
           DX=HALF*THK_PART(IP)
         ELSEIF ( THK(I) /= ZERO .AND. IINTTHICK == 0) THEN
           DX=HALF*THK(I)
         ELSEIF(IGTYP == 17) THEN
           DX=HALF*THK(I)
         ELSE
           DX=HALF*GEO(1,MG)
         ENDIF
         WA(IXC(2,I))=MAX(WA(IXC(2,I)),DX)
         WA(IXC(3,I))=MAX(WA(IXC(3,I)),DX)
         WA(IXC(4,I))=MAX(WA(IXC(4,I)),DX)
         WA(IXC(5,I))=MAX(WA(IXC(5,I)),DX)
       ENDDO
C
       DO I=1,NUMELTG
         MG=IXTG(5,I)
         IGTYP = IGEO(11,MG)
         IP = IPARTTG(I)
         IF ( THK_PART(IP) /= ZERO .AND. IINTTHICK == 0) THEN
           DX=HALF*THK_PART(IP)
         ELSEIF ( THK(NUMELC+I) /= ZERO .AND. IINTTHICK == 0) THEN
           DX=HALF*THK(NUMELC+I)
         ELSEIF(IGTYP == 17) THEN
           DX=HALF*THK(NUMELC+I)
         ELSE
           DX=HALF*GEO(1,MG)
         ENDIF
         WA(IXTG(2,I))=MAX(WA(IXTG(2,I)),DX)
         WA(IXTG(3,I))=MAX(WA(IXTG(3,I)),DX)
         WA(IXTG(4,I))=MAX(WA(IXTG(4,I)),DX)
       ENDDO
C
       DO I=1,NUMNOD
        WA(I)=GAPSCALE*WA(I)
       END DO
C--------exclude lines in main surfaces
C       DO I=1,NUMELT
C        MG=IXT(4,I)
C        DX=HALF*SQRT(GEO(1,MG))
C        WA(IXT(2,I))=MAX(WA(IXT(2,I)),DX)
C        WA(IXT(3,I))=MAX(WA(IXT(3,I)),DX)
C       ENDDO
C
C       DO I=1,NUMELP
C        MG=IXP(5,I)
C        DX=HALF*SQRT(GEO(1,MG))
C        WA(IXP(2,I))=MAX(WA(IXP(2,I)),DX)
C        WA(IXP(3,I))=MAX(WA(IXP(3,I)),DX)
C       ENDDO
C------------------------------------
C     INI GAP_N (4), GAP_M is modified taking into account nodal gap for sorting
C------------------------------------
C -----due to the fact that if surf_M does not contain the defining w/ shell
C-------> should not take into account GAP_shell    
      DO I=1,NRTM
       IF (MSEGTYP(I)==0) THEN
         DO J=1,4
           M=IRECT(J,I)
           WA(M) = ZERO
         END DO
       END IF !(MSEGTYP(I)==0) THEN
      END DO  ! nrtm
C
      DO I=1,NMN
       M = MSR(I)
       WA(M) = MIN(WA(M),GAPMAX_M)
       GAPN_M(I) = WA(M)
      END DO
C                
      DO I=1,NRTM
        GAP_M(I) = ZERO
        DO J=1,4
          M=IRECT(J,I)
          GAP_N(J,I)=WA(M)
          GAP_M(I) = MAX(GAP_M(I),WA(M))
        END DO
      END DO  ! nrtm
C
      IF(ITHK25==1) THEN
        DO I=1,NRTM
          GAPMSAV(I) = GAP_M(I)
        ENDDO
      ENDIF
      DEALLOCATE(WA)
C        
      RETURN
      END
Chd|====================================================================
Chd|  I25NEIGH_SEG_EN               source/interfaces/inter3d1/i25neigh.F
Chd|-- called by -----------
Chd|        I25NEIGH                      source/interfaces/inter3d1/i25neigh.F
Chd|-- calls ---------------
Chd|        I25NEIGH_REMOVEALLBUT1        source/interfaces/inter3d1/i25neigh.F
Chd|        I25NEIGH_SEG_E                source/interfaces/inter3d1/i25neigh.F
Chd|        I25NEIGH_SEG_OPP              source/interfaces/inter3d1/i25neigh.F
Chd|====================================================================
      SUBROUTINE I25NEIGH_SEG_EN(N1,IED1,N2   ,IED2 ,MVOISIN,
     .                           NE ,ICE,ISELF,I1,I2  ,IRECT,
     .                           X   ,IE ,NRTM,MSEGTYP,IRR  )
C----6---------------------------------------------------------------7---------8
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C-----------------------------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER N1,IED1(*),N2,IED2(*),NE,ICE(*),ISELF,IRR, 
     .        I1,I2,IRECT(4,*),IE(*), NRTM, MSEGTYP(*), MVOISIN(4,*)
      my_real
     .   X(3,*)
C-----------------------------------------------
c FUNCTION: find neighbour segment and nodes which share the same nodes I1,I2
c
c Note:
c ARGUMENTS:  (I: input, O: output, IO: input * output, W: workspace)
c
c TYPE NAME                FUNCTION
c  I   N1,IED1(N1)       - number and neighbour segment id list of node I1
c  I   N2,IED2(N2)       - number and neighbour segment id list of node I2
c  O   NE,ICE(NE)        - Number and neighbour segment id list of segment id ISELF
c  I   ISELF,I1,I2       - input segment id ISELF w/ nodes I1,I2 (I25NEIGH_un nodes)
c  I   IRECT(4,*)        - connectivity of segment id *
c  I   X(3,*)            - node co-ordinates
c  O   IE(NE)            - final (reduced) neighbour segment,array
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,J1,J2,JJ,NEW,K,M,NE0,DNE,IOP
C---------------------
       IRR = 0
       NE0=NE
C
C      MVOISIN already filled due to symetrization ::
       IF(IE(NE0+1)/=0)  THEN
         NE=NE0+1
         RETURN
       END IF
C-------------neighbour segments-----------       
       CALL I25NEIGH_SEG_E(N1,IED1,N2,IED2,NE,ICE,ISELF,
     .                I1,I2,IRECT,NRTM,MSEGTYP ,MVOISIN)
C-------------treatment of multi-neighbours (T shape,shell) segments => reduce to 1----------
       DNE = NE-NE0
       IF (DNE > 1) THEN
         CALL I25NEIGH_REMOVEALLBUT1(DNE,ICE(NE0+1),ISELF,IRECT,X ,I1,I2,IRR)
         NE=NE0+1
       END IF
       IF (ICE(NE)>0 )THEN
C
C       shell symmetric segment will be removed here !
        CALL I25NEIGH_SEG_OPP(ISELF,ICE(NE),IRECT,X ,IOP)
        IF (IOP > 0 ) ICE(NE) = 0
       END IF !(ICE(NE)>0 )THEN
C-------------after convention--------------
       IF (NE-NE0 > 1) THEN
C       print *,'!!!error (report to developer)!!!',(NE-NE0)
        IRR=12
        NE=NE0+1
       END IF 

       DO I=1+NE0,NE
        IE(I)=ICE(I)
       END DO

       IF(NE-NE0 == 1 .AND. IE(NE)/=0)THEN
C
C        Enforce symmetry ::
         DO JJ=1,4
          IF(I2==IRECT(JJ,IE(NE)))THEN
            J2 = JJ
            EXIT
          END IF
         END DO
         DO JJ=1,4
           IF(I1==IRECT(JJ,IE(NE)))THEN
             J1 = JJ
             EXIT
           END IF
         END DO
C        Triangles ::
         IF(IRECT(3,IE(NE))==IRECT(4,IE(NE)).AND.J2==3.AND.J1==1) J2=4
C
         MVOISIN(J2,IE(NE))=ISELF
C        
      END IF
C----6---------------------------------------------------------------7---------8
      RETURN
      END
Chd|====================================================================
Chd|  I25NEIGH_SEG_E                source/interfaces/inter3d1/i25neigh.F
Chd|-- called by -----------
Chd|        I25NEIGH_SEG_EN               source/interfaces/inter3d1/i25neigh.F
Chd|-- calls ---------------
Chd|        ADD_ID                        source/interfaces/inter3d1/i24tools.F
Chd|        INTAB                         source/interfaces/inter3d1/i24tools.F
Chd|        SAME_SEG                      source/interfaces/inter3d1/i24tools.F
Chd|====================================================================
      SUBROUTINE I25NEIGH_SEG_E(N1,IED1,N2,IED2,N,IC,ISELF,
     .                      I1,I2,IRECT,NRTM,MSEGTYP,MVOISIN)
C----6---------------------------------------------------------------7---------8
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C-----------------------------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER N1,IED1(*),N2,IED2(*),N,IC(*),ISELF,
     .        I1,I2,IRECT(4,*),NRTM,MSEGTYP(*),MVOISIN(4,*)
C-----------------------------------------------
c FUNCTION: find neighbour segment and which share the same nodes I1,I2
c        ---neighbour node array will be built after taking into account of treatment w/ IC 
c           (remains only one segment at the end)
c
c Note:
c ARGUMENTS:  (I: input, O: output, IO: input * output, W: workspace)
c
c TYPE NAME                FUNCTION
c  I   N1,IED1(N1)       - number and neighbour segment id list of node I1
c  I   N2,IED2(N2)       - number and neighbour segment id list of node I2
c  O   N,IC(N)           - Number and neighbour segment id list of segment id ISELF
c  I   ISELF,I1,I2       - input segment id ISELF w/ nodes I1,I2 (I25NEIGH_un nodes)
c  I   IRECT(4,*)        - connectivity of segment id *
C-----------------------------------------------
C   External function
C-----------------------------------------------
      LOGICAL INTAB,SAME_SEG
      EXTERNAL INTAB,SAME_SEG
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,NEW,K,M,LING,NE,NN,JJ,J1,J2
      DATA LING/0/
C----add I25NEIGH_un ID--at end--------------------------
      IF (I1==I2) THEN
C----add 0 in IC as convention ----------------------------
        CALL ADD_ID(N,IC,LING)
      ELSE
       NE=N
       DO J=1,N2
        NEW=IED2(J)
        IF (NEW==ISELF) CYCLE
        DO JJ=1,4
          IF(I2==IRECT(JJ,NEW))THEN
            J2 = JJ
            EXIT
          END IF
        END DO
        IF (INTAB(N1,IED1,NEW)) THEN
          J1 = 0
          DO JJ=1,4
            IF(I1==IRECT(JJ,NEW))THEN
              J1 = JJ
              EXIT
            END IF
          END DO
          if(j1==0) then
            print *,'I25NEIGH_SEG_E - internal error',i1,irect(1:4,new)
          end if
          IF( (J1-J2==1.OR.
     .        (IRECT(3,NEW)/=IRECT(4,NEW).AND.J1-J2==-3).OR.
     .        (IRECT(3,NEW)==IRECT(4,NEW).AND.J1-J2==-2))
     .       .AND. .NOT.SAME_SEG(IRECT(1,ISELF),IRECT(1,NEW))
C
C                   A coating shell can not be neighboring a non coating shell (cf bumper)
C
     .       .AND. .NOT. ((IABS(MSEGTYP(ISELF)) > NRTM  .AND. IABS(MSEGTYP(NEW)) <= NRTM).OR.
     .                    (IABS(MSEGTYP(ISELF)) <= NRTM .AND. IABS(MSEGTYP(NEW)) > NRTM))) THEN
C            Triangles ::
             IF(IRECT(3,NEW)==IRECT(4,NEW).AND.J2==3.AND.J1==1) J2=4
C
C            Does not consider NEW if NEW is already neighbor of another segment.            
             IF(MVOISIN(J2,NEW)==0) CALL ADD_ID(N,IC,NEW)
C
          end if
        END IF
       END DO 
C----add 0 for IC if find nothing -> consisting w/ ICN------------------------
       IF (NE==N) CALL ADD_ID(N,IC,LING)
      END IF !(I1==I2) THEN
C----6---------------------------------------------------------------7---------8
      RETURN
      END
Chd|====================================================================
Chd|  I25NEIGH_REMOVEALLBUT1        source/interfaces/inter3d1/i25neigh.F
Chd|-- called by -----------
Chd|        I25NEIGH_SEG_EN               source/interfaces/inter3d1/i25neigh.F
Chd|-- calls ---------------
Chd|        NORMA4N                       source/interfaces/inter3d1/norma1.F
Chd|        NORMV3                        source/interfaces/inter3d1/i24tools.F
Chd|====================================================================
      SUBROUTINE I25NEIGH_REMOVEALLBUT1(N,IC,ISELF,IRECT,X ,I1,I2,IRR)
C----6---------------------------------------------------------------7---------8
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C-----------------------------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER N,IC(*),ISELF,IRECT(4,*),I1,I2,IE,IRR
C     REAL
      my_real
     .   X(3,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,J1,J2,INV,ipr
      my_real
     .   S,YJ(3,N),YI(3),Y0(3),YJNI(N),ANGLE(N),SMIN,NORM,
     .   NXI,NYI,NZI,X12,Y12,Z12,NXJ,NYJ,NZJ,SMAX
C-----------------------------------------------
C --------YI = N_iself ^ 12       
        CALL NORMA4N(NXI,NYI,NZI,NORM,IRECT(1,ISELF) ,X )
        X12= X(1,I2)-X(1,I1)
        Y12= X(2,I2)-X(2,I1)
        Z12= X(3,I2)-X(3,I1)
        YI(1)=NYI*Z12-NZI*Y12
        YI(2)=NZI*X12-NXI*Z12
        YI(3)=NXI*Y12-NYI*X12
        CALL NORMV3(YI,NORM)
        J=0
        DO I=1,N
         IE=IC(I)
         CALL NORMA4N(NXJ,NYJ,NZJ,NORM,IRECT(1,IE) ,X )
C----YJ =  N_ie ^ 21        
         YJ(1,I)=-NYJ*Z12+NZJ*Y12
         YJ(2,I)=-NZJ*X12+NXJ*Z12
         YJ(3,I)=-NXJ*Y12+NYJ*X12
         CALL NORMV3(YJ(1,I),NORM)
         YJNI(I)=NXI*YJ(1,I)+NYI*YJ(2,I)+NZI*YJ(3,I)
         IF (YJNI(I)>=ZERO) J=J+1
         ANGLE(I)=YI(1)*YJ(1,I)+YI(2)*YJ(2,I)+YI(3)*YJ(3,I)
        END DO
C        
        SMAX=-ONEP01
        J1=0
C--------groupe YJ*Ni>=0 :concave keep angle (max_cos) only
        DO I=1,N
         IF (YJNI(I)<ZERO) CYCLE
          IF (ANGLE(I)>=-ONE) THEN
           IF(SMAX < ANGLE(I)) THEN
            SMAX=ANGLE(I)
            J1=I
           END IF
          END IF !(ANGLE(I)>=-ONE) THEN
        END DO
C------angle >180------        
        IF(J1==0.AND.J >0) THEN
         SMIN=EP10
         DO I=1,N
          IF (YJNI(I)<ZERO) CYCLE
          IF (SMIN > ANGLE(I)) THEN
            SMIN=ANGLE(I)
            J1=I
          END IF
         END DO
        END IF
C--------same side
        IF (J==N) THEN
C--------only groupe YJ*Ni<0(convex)  and no valid one before    
        ELSEIF(J==0.OR.J1==0) THEN
C------angle >180- first-----        
          SMAX=-ONEP01
          DO I=1,N
           IF (YJNI(I)>=ZERO) CYCLE
           IF(ANGLE(I)< -ONE .AND.SMAX < ANGLE(I)) THEN
            SMAX=ANGLE(I)
            J1=I
           END IF
          END DO
C ------------------         
         IF (J1==0) then
          SMIN=EP10
          DO I=1,N
           IF (YJNI(I)>=ZERO) CYCLE
C--------groupe YJ*Ni<0 :convex keep angle (min_cos) only
           IF(ANGLE(I)>= -ONE .AND. SMIN > ANGLE(I)) THEN
            SMIN=ANGLE(I)
            J1=I
           END IF
          END DO
         END IF !(J1==0) then
        END IF !(J==N) then
C ------still no valid one-----------       
        IF (J1==0) then
c        print *,'***warning** No valid Neighbour Segs of',N,' candidats'
         IRR = 11
         IC(1)=0         
        ELSE
         IC(1)=IC(J1)
        END IF
        DO I=2,N
         IC(I)=0
        END DO 
C----6---------------------------------------------------------------7---------8
      RETURN
      END
Chd|====================================================================
Chd|  I25NEIGH_SEG_OPP              source/interfaces/inter3d1/i25neigh.F
Chd|-- called by -----------
Chd|        I25NEIGH_SEG_EN               source/interfaces/inter3d1/i25neigh.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE I25NEIGH_SEG_OPP(EI,EJ,IRECT,X ,IOP)
C----6---------------------------------------------------------------7---------8
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C-----------------------------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER EI,EJ,IRECT(4,*),IOP
C     REAL
      my_real
     .   X(3,*)
C----if the normals of two segments are opposite or near opposite
C----if two common segs, will also be eliminated 
C-----------------------------------------------
C   External function
C-----------------------------------------------
      LOGICAL INTAB
      EXTERNAL INTAB
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,NN,IMIN,JMIN,IRMIN,JRMIN,IRI(4),IRJ(4)
C-----------------------------------------------
      IRMIN=MAX(IRECT(1,EI),IRECT(2,EI),IRECT(3,EI),IRECT(4,EI))
      IF(IRECT(3,EI)/=IRECT(4,EI))THEN
        DO I=1,4
          IF(IRECT(I,EI) <= IRMIN)THEN
           IRMIN=IRECT(I,EI)
           IMIN =I
          END IF
        END DO
        IRI(1)=IRECT(IMIN,EI)
        IRI(2)=IRECT(MOD(IMIN  ,4)+1,EI)
        IRI(3)=IRECT(MOD(IMIN+1,4)+1,EI)
        IRI(4)=IRECT(MOD(IMIN+2,4)+1,EI)
      ELSE
        DO I=1,3
          IF(IRECT(I,EI) <= IRMIN)THEN
           IRMIN=IRECT(I,EI)
           IMIN =I
          END IF
        END DO
        IRI(1)=IRECT(IMIN,EI)
        IRI(2)=IRECT(MOD(IMIN  ,3)+1,EI)
        IRI(3)=IRECT(MOD(IMIN+1,3)+1,EI)
        IRI(4)=IRI(3)
      END IF
C-----------------------------------------------
      JRMIN=MAX(IRECT(1,EJ),IRECT(2,EJ),IRECT(3,EJ),IRECT(4,EJ))
      IF(IRECT(3,EJ)/=IRECT(4,EJ))THEN
        DO J=1,4
          IF(IRECT(J,EJ) <= JRMIN)THEN
           JRMIN=IRECT(J,EJ)
           JMIN =J
          END IF
        END DO
        IRJ(1)=IRECT(JMIN,EJ)
        IRJ(2)=IRECT(MOD(JMIN  ,4)+1,EJ)
        IRJ(3)=IRECT(MOD(JMIN+1,4)+1,EJ)
        IRJ(4)=IRECT(MOD(JMIN+2,4)+1,EJ)
      ELSE
        DO J=1,3
          IF(IRECT(J,EJ) <= JRMIN)THEN
           JRMIN=IRECT(J,EJ)
           JMIN =J
          END IF
        END DO
        IRJ(1)=IRECT(JMIN,EJ)
        IRJ(2)=IRECT(MOD(JMIN  ,3)+1,EJ)
        IRJ(3)=IRECT(MOD(JMIN+1,3)+1,EJ)
        IRJ(4)=IRJ(3)
      END IF
C-----------------------------------------------
      IOP=0
      IF(IRJ(3)/=IRJ(4) .AND. IRI(3)/=IRI(4))THEN
        IF(IRI(1)==IRJ(1))THEN
          IF(IRI(2)==IRJ(4))THEN
            IF(IRI(3)==IRJ(3))THEN
              IF(IRI(4)==IRJ(2))THEN
                IOP=1
              END IF
            END IF
          END IF
        END IF
      ELSEIF(IRJ(3)==IRJ(4) .AND. IRI(3)==IRI(4))THEN
        IF(IRI(1)==IRJ(1))THEN
          IF(IRI(2)==IRJ(3))THEN
            IF(IRI(3)==IRJ(2))THEN
              IOP=1
            END IF
          END IF
        END IF
      END IF
C----6---------------------------------------------------------------7---------8
      RETURN
      END
Chd|====================================================================
Chd|  I25NEIGH_MSG_ERR              source/interfaces/inter3d1/i25neigh.F
Chd|-- called by -----------
Chd|        I25NEIGH                      source/interfaces/inter3d1/i25neigh.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE I25NEIGH_MSG_ERR(I1,I2,ITAB,IRR)
      USE MESSAGE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "scr03_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER I1,I2,ITAB(*),IRR
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
C----Warning,ERROR out----------------
       IF(IPRI==0) RETURN
#ifndef HYPERMESH_LIB
       IF (IRR ==11) THEN
C-----multi-neibour but no valid one
            CALL ANCMSG(MSGID=1245,
     .               MSGTYPE=MSGWARNING,
     .               ANMODE=ANINFO_BLIND_2,
     .               I2=Itab(I1),I3=Itab(I2))
c        write(iout,*) '***Warning: No valid commun Seg with line:',
c     +                 Itab(I1),Itab(I2)
       ELSEIF (IRR ==12) THEN
C-----multi-neibour but no valid one
            CALL ANCMSG(MSGID=1246,
     .               MSGTYPE=MSGERROR,
     .               ANMODE=ANINFO_BLIND_2,
     .               I2=Itab(I1),I3=Itab(I2))
c        write(iout,*) '***ERROR: Too many commun Seg with line:',
c     +                 Itab(I1),Itab(I2)
c        CALL ARRET(2)
       END IF
#endif
C----6---------------------------------------------------------------7---------8
      RETURN
      END
