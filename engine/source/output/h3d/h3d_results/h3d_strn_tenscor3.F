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
!||    strn_tenscor3        ../engine/source/output/h3d/h3d_results/h3d_strn_tenscor3.F
!||--- called by ------------------------------------------------------
!||    h3d_solid_tensor_1   ../engine/source/output/h3d/h3d_results/h3d_solid_tensor_1.F
!||--- calls      -----------------------------------------------------
!||    srota6               ../engine/source/output/anim/generate/srota6.F
!||    szstraingps          ../engine/source/elements/solid/solidez/szstraingps.F
!||--- uses       -----------------------------------------------------
!||    elbufdef_mod         ../common_source/modules/mat_elem/elbufdef_mod.F90
!||    initbuf_mod          ../engine/share/resol/initbuf.F
!||====================================================================
      SUBROUTINE STRN_TENSCOR3(ELBUF_TAB,IPARG   ,IXS      ,IXS10   ,X        ,
     .                         PM       ,KCVT    ,NEL      ,EVAR    )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE INITBUF_MOD
      USE ELBUFDEF_MOD            
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "vect01_c.inc"
#include      "mvsiz_p.inc"
#include      "com04_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
C     REAL
      my_real
     .   EVAR(6,20,MVSIZ),X(3,*),PM(NPROPM,*)
      INTEGER IPARG(NPARG),IXS(NIXS,*),IXS10(6,*),KCVT ,NEL 
      TYPE (ELBUF_STRUCT_), TARGET :: ELBUF_TAB
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
C     REAL
      my_real
     .   GAMA(6),OFF, P, VONM2, VONM, S1, S2, S12, S3, VALUE,
     .   A1,B1,B2,B3,YEQ,F1,M1,M2,M3,FOR,AREA(MVSIZ),
     .   A_GAUSS_R,A_GAUSS_S,A_GAUSS_T,N1,
     .   A_GAUSS_R1,A_GAUSS_S1,A_GAUSS_T1,
     .   A_GAUSS_P_R,A_GAUSS_P_S,A_GAUSS_P_T
      INTEGER I,II, ISS, ISC,NBGAMA,
     .        IADD, N, J, MLW,  
     .        ISTRAIN,NN, JTURB,MT, IMID, IALEL,IPID,
     .        NN1,NF,OFFSET,K,INC,KK, IUS, NUVAR,
     .        INOD, ISOLNOD, IPRT, LIAD, NPTR, NPTS, NPTT, IPT,
     .        IS, IR, IT, NPTG,NC(10,MVSIZ),NNOD,IEXPAN,IHBE,MPT,ILAY,
     .        ICSIG,DIR,IVISC,JJ(6),IP
      INTEGER MLW2,NLAY
      TYPE(G_BUFEL_)  ,POINTER :: GBUF     
      TYPE(L_BUFEL_)  ,POINTER :: LBUF   
      my_real
     .  A_GAUSS(9,9),EVAR_TMP(6),ALPHA,BETA,ALPHA_1,BETA_1,
     .   STR_IS24(MVSIZ,6,8),EVAR_T10(6,10)
      INTEGER 
     .  SOL_NODE(3,8), IPERM1(10),IPERM2(10),NN2
      DATA IPERM1/0,0,0,0,1,2,3,1,2,3/
      DATA IPERM2/0,0,0,0,2,3,1,4,4,4/
C======================================================================= 
      DATA A_GAUSS / 
     1 0.               ,0.               ,0.               ,
     1 0.               ,0.               ,0.               ,
     1 0.               ,0.               ,0.               ,
     2 -.577350269189626,0.577350269189626,0.               ,
     2 0.               ,0.               ,0.               ,
     2 0.               ,0.               ,0.               ,
     3 -.774596669241483,0.               ,0.774596669241483,
     3 0.               ,0.               ,0.               ,
     3 0.               ,0.               ,0.               ,
     4 -.861136311594053,-.339981043584856,0.339981043584856,
     4 0.861136311594053,0.               ,0.               ,
     4 0.               ,0.               ,0.               ,
     5 -.906179845938664,-.538469310105683,0.               ,
     5 0.538469310105683,0.906179845938664,0.               ,
     5 0.               ,0.               ,0.               ,
     6 -.932469514203152,-.661209386466265,-.238619186083197,
     6 0.238619186083197,0.661209386466265,0.932469514203152,
     6 0.               ,0.               ,0.               ,
     7 -.949107912342759,-.741531185599394,-.405845151377397,
     7 0.               ,0.405845151377397,0.741531185599394,
     7 0.949107912342759,0.               ,0.               ,
     8 -.960289856497536,-.796666477413627,-.525532409916329,
     8 -.183434642495650,0.183434642495650,0.525532409916329,
     8 0.796666477413627,0.960289856497536,0.               ,
     9 -.968160239507626,-.836031107326636,-.613371432700590,
     9 -.324253423403809,0.               ,0.324253423403809,
     9 0.613371432700590,0.836031107326636,0.968160239507626/  
      DATA SOL_NODE / 
     1 -1               ,-1               ,-1               ,
     2 -1               ,-1               , 1               ,
     3  1               ,-1               , 1               ,
     4  1               ,-1               ,-1               ,
     5 -1               , 1               ,-1               ,
     6 -1               , 1               , 1               ,
     7  1               , 1               , 1               ,
     8  1               , 1               ,-1               /
C======================================================================= 
      ALPHA = ZEP1381966
      BETA  = ZEP5854102
      EVAR = ZERO
      IVISC = IPARG(61)
      GBUF => ELBUF_TAB%GBUF
       ISOLNOD = IPARG(28)
        LFT=1
        LLT=NEL
        NNOD = 0
!
        DO I=1,6
          JJ(I) = NEL*(I-1)
        ENDDO
!
C-----------------------------------------------
C       SOLID 8N
C-----------------------------------------------
         IF (KCVT==1.AND.ISORTH/=0) KCVT=2
         NNOD = ISOLNOD
         DO I=LFT,LLT
           N = I + NFT
           IF(ISOLNOD == 8)THEN
             DO J = 1,ISOLNOD
               NC(J,I) = IXS(J+1,N)
             ENDDO
           ELSEIF(ISOLNOD == 4)THEN
             NC(1,I)=IXS(2,N)
             NC(2,I)=IXS(4,N)
             NC(3,I)=IXS(7,N)
             NC(4,I)=IXS(6,N)
           ELSEIF(ISOLNOD == 6)THEN
             NC(1,I)=IXS(2,N)
             NC(2,I)=IXS(3,N)
             NC(3,I)=IXS(4,N)
             NC(4,I)=IXS(6,N)
             NC(5,I)=IXS(7,N)
             NC(6,I)=IXS(8,N)
           ELSEIF(ISOLNOD == 10)THEN
             NC(1,I)=IXS(2,N)
             NC(2,I)=IXS(4,N)
             NC(3,I)=IXS(7,N)
             NC(4,I)=IXS(6,N)
             NN1 = N - NUMELS8
             DO J=1,6
               NC(J+4,I) = IXS10(J,NN1)
             ENDDO
           ENDIF
         ENDDO 
C
         NPTR   = ELBUF_TAB%NPTR
         NPTS   = ELBUF_TAB%NPTS
         NPTT   = ELBUF_TAB%NPTT
         NLAY   = ELBUF_TAB%NLAY
         NPT = NPTR*NPTS*NPTT
         IF (JHBE == 24) THEN
            LBUF => ELBUF_TAB%BUFLY(1)%LBUF(1,1,1)
           NPTR = 2
           NPTS = 2
           NPTT = 2
           CALL SZSTRAINGPS(
     1   LBUF%STRA, STR_IS24,  GBUF%STRHG,NEL)
         ENDIF
C----------
         IF(ISOLNOD == 6 .OR. ISOLNOD == 8 .OR. 
     .              ISOLNOD == 16 .OR. ISOLNOD == 20)THEN
c
c T_SHELL ( JHBE = 15/16 )
          IF(NLAY > 1 .AND. JHBE /= 14) THEN
           DO I=LFT,LLT
             II = 6*(I-1) 
             N = I + NFT
             IF (KCVT /= 0) THEN
               IF(KCVT==2)THEN
                 GAMA(1:6) = GBUF%GAMA(JJ(1:6) + I)
               ELSE
                 GAMA(1)=ONE
                 GAMA(2)=ZERO
                 GAMA(3)=ZERO
                 GAMA(4)=ZERO
                 GAMA(5)=ONE
                 GAMA(6)=ZERO
               END IF
             END IF
             NPTS = NLAY
C 
             DO J=1,8
               DO K=1,8
                IF(SOL_NODE(2,K) == SOL_NODE(2,J)) THEN
c
                 IF (SOL_NODE(1,K) == -1 .AND. SOL_NODE(1,J) == -1)
     .           IR = 1
                 IF (SOL_NODE(1,K) == -1 .AND. SOL_NODE(1,J) == 1)
     .           IR = MAX(1,NPTR-1)
                 IF (SOL_NODE(1,K) == 1 .AND. SOL_NODE(1,J) == 1)
     .           IR = NPTR
                 IF (SOL_NODE(1,K) == 1 .AND. SOL_NODE(1,J) == -1)
     .           IR = MIN(NPTR,2)
                 IF (SOL_NODE(2,K) == -1 .AND. SOL_NODE(2,J) == -1)
     .           IS = 1
                 IF (SOL_NODE(2,K) == -1 .AND. SOL_NODE(2,J) == 1)
     .           IS = MAX(1,NPTS-1)
                 IF (SOL_NODE(2,K) == 1 .AND. SOL_NODE(2,J) == 1)
     .           IS = NPTS
                 IF (SOL_NODE(2,K) == 1 .AND. SOL_NODE(2,J) == -1)
     .           IS = MIN(NPTS,2)
                 IF (SOL_NODE(3,K) == -1 .AND. SOL_NODE(3,J) == -1)
     .           IT = 1
                 IF (SOL_NODE(3,K) == -1 .AND. SOL_NODE(3,J) == 1)
     .           IT = MAX(1,NPTT-1)
                 IF (SOL_NODE(3,K) == 1 .AND. SOL_NODE(3,J) == 1)
     .           IT = NPTT
                 IF (SOL_NODE(3,K) == 1 .AND. SOL_NODE(3,J) == -1)
     .           IT = MIN(NPTT,2)
c
                 A_GAUSS_P_R = ZERO
                 A_GAUSS_P_S = ZERO
                 A_GAUSS_P_T = ZERO
c
                 IF (NPTR == 1)THEN
                   A_GAUSS_P_R = ZERO
                 ELSEIF (SOL_NODE(1,J) == -1 )THEN
                   A_GAUSS_R = A_GAUSS(1,NPTR)
                   A_GAUSS_R1 = A_GAUSS(2,NPTR)
                   A_GAUSS_P_R =
     .             (-ONE-HALF*(A_GAUSS_R1+A_GAUSS_R))/ 
     .             (HALF*(A_GAUSS_R1-A_GAUSS_R))
                 ELSEIF(SOL_NODE(1,J) == 1 )THEN
                   A_GAUSS_R = A_GAUSS(NPTR-1,NPTR)
                   A_GAUSS_R1 = A_GAUSS(NPTR,NPTR)
                   A_GAUSS_P_R =
     .             (ONE+HALF*(A_GAUSS_R1+A_GAUSS_R))/ 
     .             (HALF*(A_GAUSS_R1-A_GAUSS_R))
                 ENDIF
c
                 IF (NPTS == 1)THEN
                   A_GAUSS_P_S = ZERO
                 ELSEIF (SOL_NODE(2,J) == -1 )THEN
                   A_GAUSS_S = A_GAUSS(1,NPTS)
                   A_GAUSS_S1 = A_GAUSS(2,NPTS)
                   A_GAUSS_P_S =
     .             (-ONE-HALF*(A_GAUSS_S1+A_GAUSS_S))/ 
     .             (HALF*(A_GAUSS_S1-A_GAUSS_S))
                 ELSEIF(SOL_NODE(2,J) == 1 )THEN
                   A_GAUSS_S = A_GAUSS(NPTS-1,NPTS)
                   A_GAUSS_S1 = A_GAUSS(NPTS,NPTS)
                   A_GAUSS_P_S =
     .             (ONE+HALF*(A_GAUSS_S1+A_GAUSS_S))/ 
     .             (HALF*(A_GAUSS_S1-A_GAUSS_S))
                 ENDIF
c
                 IF (NPTT == 1)THEN
                   A_GAUSS_P_T = ZERO
                 ELSEIF (SOL_NODE(3,J) == -1 )THEN
                   A_GAUSS_T = A_GAUSS(1,NPTT)
                   A_GAUSS_T1 = A_GAUSS(2,NPTT)
                   A_GAUSS_P_T =
     .             (-ONE-HALF*(A_GAUSS_T1+A_GAUSS_T))/ 
     .             (HALF*(A_GAUSS_T1-A_GAUSS_T))
                 ELSEIF(SOL_NODE(3,J) == 1 )THEN
                   A_GAUSS_T = A_GAUSS(NPTT-1,NPTT)
                   A_GAUSS_T1 = A_GAUSS(NPTT,NPTT)
                   A_GAUSS_P_T =
     .             (ONE+HALF*(A_GAUSS_T1+A_GAUSS_T))/ 
     .             (HALF*(A_GAUSS_T1-A_GAUSS_T))
                 ENDIF
c
                 IF (JHBE == 15 .OR. JHBE == 16) THEN
                  ILAY = IS
                  IS = 1 
                  N1 = FOURTH*(
     .               (ONE+SOL_NODE(1,K) * A_GAUSS_P_R)  *
     .               (ONE+SOL_NODE(3,K) * A_GAUSS_P_T)  )
                 ENDIF
c       STRHG(NEL,6,8)
                   LBUF => ELBUF_TAB%BUFLY(ILAY)%LBUF(IR,IS,IT)
                   IP = IR + ( (IS-1) + (IT-1)*2 )*2
                   EVAR_TMP(1) = LBUF%STRA(JJ(1) + I)
                   EVAR_TMP(2) = LBUF%STRA(JJ(2) + I)
                   EVAR_TMP(3) = LBUF%STRA(JJ(3) + I)
                   EVAR_TMP(4) = LBUF%STRA(JJ(4) + I)*HALF
                   EVAR_TMP(5) = LBUF%STRA(JJ(5) + I)*HALF
                   EVAR_TMP(6) = LBUF%STRA(JJ(6) + I)*HALF
                   IF (KCVT /= 0)
     .             CALL SROTA6(
     1   X,       IXS(1,N),KCVT,    EVAR_TMP,
     2   GAMA,    JHBE,    IGTYP,   ISORTH)
                   EVAR(1:6,J,I) = EVAR(1:6,J,I) + N1 * EVAR_TMP(1:6)
                 ENDIF
               ENDDO
             ENDDO
           ENDDO
         ELSE 
           DO I=LFT,LLT
             II = 6*(I-1) 
             N = I + NFT
             IF (KCVT /= 0) THEN
               IF(KCVT==2)THEN
                 GAMA(1:6) = GBUF%GAMA(JJ(1:6) + I)
               ELSE
                 GAMA(1)=ONE
                 GAMA(2)=ZERO
                 GAMA(3)=ZERO
                 GAMA(4)=ZERO
                 GAMA(5)=ONE
                 GAMA(6)=ZERO
               END IF
             END IF
             IF(IGTYP == 20 .OR. IGTYP ==21 .OR. IGTYP == 22) THEN
               NPTT = NLAY
             ENDIF
             DO J=1,8
               DO K=1,8
                 IF (SOL_NODE(1,K) == -1 .AND. SOL_NODE(1,J) == -1)
     .           IS = 1
                 IF (SOL_NODE(1,K) == -1 .AND. SOL_NODE(1,J) == 1)
     .           IS = MAX(1,NPTS-1)
                 IF (SOL_NODE(1,K) == 1 .AND. SOL_NODE(1,J) == 1)
     .           IS = NPTS
                 IF (SOL_NODE(1,K) == 1 .AND. SOL_NODE(1,J) == -1)
     .           IS = MIN(NPTS,2)
                 IF (SOL_NODE(2,K) == -1 .AND. SOL_NODE(2,J) == -1)
     .           IT = 1
                 IF (SOL_NODE(2,K) == -1 .AND. SOL_NODE(2,J) == 1)
     .           IT = MAX(1,NPTT-1)
                 IF (SOL_NODE(2,K) == 1 .AND. SOL_NODE(2,J) == 1)
     .           IT = NPTT
                 IF (SOL_NODE(2,K) == 1 .AND. SOL_NODE(2,J) == -1)
     .           IT = MIN(NPTT,2)
                 IF (SOL_NODE(3,K) == -1 .AND. SOL_NODE(3,J) == -1)
     .           IR = 1
                 IF (SOL_NODE(3,K) == -1 .AND. SOL_NODE(3,J) == 1)
     .           IR = MAX(1,NPTR-1)
                 IF (SOL_NODE(3,K) == 1 .AND. SOL_NODE(3,J) == 1)
     .           IR = NPTR
                 IF (SOL_NODE(3,K) == 1 .AND. SOL_NODE(3,J) == -1)
     .           IR = MIN(NPTR,2)
c
                 A_GAUSS_P_R = ZERO
                 A_GAUSS_P_S = ZERO
                 A_GAUSS_P_T = ZERO
c
                 IF (NPTR == 1)THEN
                   A_GAUSS_P_R = ZERO
                 ELSEIF (SOL_NODE(1,J) == -1 )THEN
                   A_GAUSS_R = A_GAUSS(1,NPTR)
                   A_GAUSS_R1 = A_GAUSS(2,NPTR)
                   A_GAUSS_P_R =
     .             (-ONE-HALF*(A_GAUSS_R1+A_GAUSS_R))/ 
     .             (HALF*(A_GAUSS_R1-A_GAUSS_R))
                 ELSEIF(SOL_NODE(1,J) == 1 )THEN
                   A_GAUSS_R = A_GAUSS(NPTR-1,NPTR)
                   A_GAUSS_R1 = A_GAUSS(NPTR,NPTR)
                   A_GAUSS_P_R =
     .             (ONE+HALF*(A_GAUSS_R1+A_GAUSS_R))/ 
     .             (HALF*(A_GAUSS_R1-A_GAUSS_R))
                 ENDIF
c
                 IF (NPTS == 1)THEN
                   A_GAUSS_P_S = ZERO
                 ELSEIF (SOL_NODE(2,J) == -1 )THEN
                   A_GAUSS_S = A_GAUSS(1,NPTS)
                   A_GAUSS_S1 = A_GAUSS(2,NPTS)
                   A_GAUSS_P_S =
     .             (-ONE-HALF*(A_GAUSS_S1+A_GAUSS_S))/ 
     .             (HALF*(A_GAUSS_S1-A_GAUSS_S))
                 ELSEIF(SOL_NODE(2,J) == 1 )THEN
                   A_GAUSS_S = A_GAUSS(NPTS-1,NPTS)
                   A_GAUSS_S1 = A_GAUSS(NPTS,NPTS)
                   A_GAUSS_P_S =
     .             (ONE+HALF*(A_GAUSS_S1+A_GAUSS_S))/ 
     .             (HALF*(A_GAUSS_S1-A_GAUSS_S))
                 ENDIF
c
                 IF (NPTT == 1)THEN
                   A_GAUSS_P_T = ZERO
                 ELSEIF (SOL_NODE(3,J) == -1 )THEN
                   A_GAUSS_T = A_GAUSS(1,NPTT)
                   A_GAUSS_T1 = A_GAUSS(2,NPTT)
                   A_GAUSS_P_T =
     .             (-ONE-HALF*(A_GAUSS_T1+A_GAUSS_T))/ 
     .             (HALF*(A_GAUSS_T1-A_GAUSS_T))
                 ELSEIF(SOL_NODE(3,J) == 1 )THEN
                   A_GAUSS_T = A_GAUSS(NPTT-1,NPTT)
                   A_GAUSS_T1 = A_GAUSS(NPTT,NPTT)
                   A_GAUSS_P_T =
     .             (ONE+HALF*(A_GAUSS_T1+A_GAUSS_T))/ 
     .             (HALF*(A_GAUSS_T1-A_GAUSS_T))
                 ENDIF
c
                 N1 = ONE_OVER_8*(
     .               (ONE+SOL_NODE(1,K) * A_GAUSS_P_R)  *
     .               (ONE+SOL_NODE(2,K) * A_GAUSS_P_S)  *
     .               (ONE+SOL_NODE(3,K) * A_GAUSS_P_T)  )
c
                 IF (IGTYP == 20 .OR. IGTYP ==21 .OR. IGTYP == 22) THEN
                   ILAY = IT
                   IT = 1
                 ELSE
                   ILAY = 1
                 ENDIF

                 IF (JHBE == 24 .AND. GBUF%G_STRHG > 0) THEN
                   IP = IR + ( (IS-1) + (IT-1)*2 )*2
                   EVAR_TMP(1) = STR_IS24(I,1,IP)
                   EVAR_TMP(2) = STR_IS24(I,2,IP)
                   EVAR_TMP(3) = STR_IS24(I,3,IP)
                   EVAR_TMP(4) = STR_IS24(I,4,IP)*HALF
                   EVAR_TMP(5) = STR_IS24(I,5,IP)*HALF
                   EVAR_TMP(6) = STR_IS24(I,6,IP)*HALF
                 ELSE
                   LBUF => ELBUF_TAB%BUFLY(ILAY)%LBUF(IR,IS,IT)
                   EVAR_TMP(1) =  LBUF%STRA(JJ(1) + I)
                   EVAR_TMP(2) =  LBUF%STRA(JJ(2) + I)
                   EVAR_TMP(3) =  LBUF%STRA(JJ(3) + I)
                   EVAR_TMP(4) =  LBUF%STRA(JJ(4) + I)*HALF
                   EVAR_TMP(5) =  LBUF%STRA(JJ(5) + I)*HALF
                   EVAR_TMP(6) =  LBUF%STRA(JJ(6) + I)*HALF
                 ENDIF
                 IF (KCVT /= 0)
     .           CALL SROTA6(
     1   X,       IXS(1,N),KCVT,    EVAR_TMP,
     2   GAMA,    JHBE,    IGTYP,   ISORTH)
                 EVAR(1:6,J,I) = EVAR(1:6,J,I) + N1 * EVAR_TMP(1:6)
               ENDDO
             ENDDO
           ENDDO
          ENDIF
c    
         ELSEIF(ISOLNOD == 4 )THEN
c    
           DO I=LFT,LLT
             N = I + NFT
             IF (KCVT /= 0) THEN
               IF(KCVT==2)THEN
                 GAMA(1) = GBUF%GAMA(JJ(1) + I)
                 GAMA(2) = GBUF%GAMA(JJ(2) + I)
                 GAMA(3) = GBUF%GAMA(JJ(3) + I)
                 GAMA(4) = GBUF%GAMA(JJ(4) + I)
                 GAMA(5) = GBUF%GAMA(JJ(5) + I)
                 GAMA(6) = GBUF%GAMA(JJ(6) + I)
               ELSE
                 GAMA(1)=ONE
                 GAMA(2)=ZERO
                 GAMA(3)=ZERO
                 GAMA(4)=ZERO
                 GAMA(5)=ONE
                 GAMA(6)=ZERO
               END IF
             END IF
                   N1 = FOURTH
                   ILAY = 1
                   LBUF => ELBUF_TAB%BUFLY(ILAY)%LBUF(1,1,1)
                 EVAR_TMP(1) = LBUF%STRA(JJ(1) + I)
                 EVAR_TMP(2) = LBUF%STRA(JJ(2) + I)
                 EVAR_TMP(3) = LBUF%STRA(JJ(3) + I)
                 EVAR_TMP(4) = LBUF%STRA(JJ(4) + I)*HALF
                 EVAR_TMP(5) = LBUF%STRA(JJ(5) + I)*HALF
                 EVAR_TMP(6) = LBUF%STRA(JJ(6) + I)*HALF
                 IF (KCVT /= 0)
     .           CALL SROTA6(
     1   X,       IXS(1,N),KCVT,    EVAR_TMP,
     2   GAMA,    JHBE,    IGTYP,   ISORTH)
             DO J=1,4
               EVAR(1:6,J,I) = EVAR(1:6,J,I) + N1 * EVAR_TMP(1:6)
             ENDDO
           ENDDO
         ELSEIF(ISOLNOD == 10)THEN
c    
           ALPHA_1 = -ALPHA/(BETA-ALPHA)
           BETA_1  = (ONE-ALPHA)/(BETA-ALPHA)
           DO I=LFT,LLT
             N = I + NFT
             IF (KCVT /= 0) THEN
               IF(KCVT==2)THEN
                 GAMA(1) = GBUF%GAMA(JJ(1) + I)
                 GAMA(2) = GBUF%GAMA(JJ(2) + I)
                 GAMA(3) = GBUF%GAMA(JJ(3) + I)
                 GAMA(4) = GBUF%GAMA(JJ(4) + I)
                 GAMA(5) = GBUF%GAMA(JJ(5) + I)
                 GAMA(6) = GBUF%GAMA(JJ(6) + I)
               ELSE
                 GAMA(1)=ONE
                 GAMA(2)=ZERO
                 GAMA(3)=ZERO
                 GAMA(4)=ZERO
                 GAMA(5)=ONE
                 GAMA(6)=ZERO
               END IF
             END IF
             DO J=1,4
               EVAR_T10(1:6,J)=ZERO
               DO K=1,4
                   IR = K
                   IS = 1
                   IT = 1
C
                   IF (J==K) THEN
                     N1 = BETA_1
                   ELSE
                     N1 = ALPHA_1
                   ENDIF
                   ILAY = 1
                   LBUF => ELBUF_TAB%BUFLY(ILAY)%LBUF(IR,IS,IT)
                 EVAR_T10(1,J) = EVAR_T10(1,J)+ N1 *LBUF%STRA(JJ(1) + I)
                 EVAR_T10(2,J) = EVAR_T10(2,J)+ N1 *LBUF%STRA(JJ(2) + I)
                 EVAR_T10(3,J) = EVAR_T10(3,J)+ N1 *LBUF%STRA(JJ(3) + I)
                 EVAR_T10(4,J) = EVAR_T10(4,J)+ N1 *LBUF%STRA(JJ(4) + I)*HALF
                 EVAR_T10(5,J) = EVAR_T10(5,J)+ N1 *LBUF%STRA(JJ(5) + I)*HALF
                 EVAR_T10(6,J) = EVAR_T10(6,J)+ N1 *LBUF%STRA(JJ(6) + I)*HALF
               ENDDO
                 IF (KCVT /= 0)
     .           CALL SROTA6(
     1   X,            IXS(1,N),     KCVT,         EVAR_T10(1,J),
     2   GAMA,         JHBE,         IGTYP,        ISORTH)
             ENDDO
             DO J=5,10
                NN1=IPERM1(J)
                NN2=IPERM2(J)
                EVAR_T10(1:6,J) = HALF*(EVAR_T10(1:6,NN1)+EVAR_T10(1:6,NN2))
             END DO
             DO J=1,10
               EVAR(1:6,J,I) = EVAR_T10(1:6,J)
             ENDDO
           ENDDO
         ENDIF
C-----------------------------------------------
      RETURN
      END SUBROUTINE STRN_TENSCOR3
