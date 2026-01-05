Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2026 Altair Engineering Inc.
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
!||    i9wal2            ../engine/source/interfaces/int09/i9wal2.F
!||--- called by ------------------------------------------------------
!||    i9wale            ../engine/source/interfaces/int09/i9wale.F
!||--- calls      -----------------------------------------------------
!||    bcs2              ../engine/source/constraints/general/bcs/bcs2.F
!||    i9grd2            ../engine/source/interfaces/int09/i9grd2.F
!||    spmd_glob_dsum9   ../engine/source/mpi/interfaces/spmd_th.F
!||    spmd_glob_isum9   ../engine/source/mpi/interfaces/spmd_th.F
!||    spmd_ibcast       ../engine/source/mpi/generic/spmd_ibcast.F
!||    spmd_rbcast       ../engine/source/mpi/generic/spmd_rbcast.F
!||--- uses       -----------------------------------------------------
!||    elbufdef_mod      ../common_source/modules/mat_elem/elbufdef_mod.F90
!||    element_mod       ../common_source/modules/elements/element_mod.F90
!||====================================================================
      SUBROUTINE I9WAL2(X,V   ,W         ,A         ,CRST      ,
     2   NSV       ,ILOC      ,IRTL      ,ICODE     ,ISKEW     ,
     3   SKEW      ,MSR       ,LMSR      ,NSEG      ,IRECTS    ,
     4   IRECT     ,UPW       ,IXQ       ,ELBUF_TAB ,
     5   IPARG     ,PM        ,NALE      ,EE        ,IELES     ,
     6   IELEM     ,TSTIF     ,INTTH     ,IEULT     ,STENS     ,
     7   ISIZES    ,ISIZEM, NRTS, NRTM,NSN,NMN) 
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE ELBUFDEF_MOD      
      use element_mod , only : nixq      
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com08_c.inc"
#include      "scr08_a_c.inc"
#include      "param_c.inc"
#include      "task_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER, INTENT(IN) :: NRTS, NRTM,NSN,NMN
      INTEGER IRECT(4,*), NSV(NSN), ILOC(*), IRTL(NSN), ICODE(*), ISKEW(*),
     .   MSR(*), IRECTS(4,*), LMSR(*), NSEG(*),IXQ(NIXQ,*),
     .   IPARG(NPARG,*), IELES(*), IELEM(*),NALE(*) ,
     .   INTTH, IEULT, ISIZES, ISIZEM
C     REAL
      my_real
     .   UPW, TSTIF,TTT, STENS,
     .   X(3,*), V(3,*), W(3,*), A(3,*), CRST(2,*), SKEW(LSKEW,*),
     .   PM(NPROPM,*),EE(*)
      TYPE (ELBUF_STRUCT_), TARGET, DIMENSION(NGROUP) :: ELBUF_TAB
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER II, N, L, JJ, NN, LL, LL1, LL2, LG, KK, KKK, K1, 
     . I1, IERR, IGROU, IELN,
     .   IXX(4), IPERM(2), JPERM(2),
     .   ITEMP(2), IS, IM, ILEN,
     .   TAGS(ISIZES),TAGM(ISIZEM), LISTS(ISIZES),LISTM(ISIZEM),
     .   ICOMERR(ISIZEM+ISIZES),ICOMNGR(ISIZEM+ISIZES),
     .   ICOMNEL(ISIZEM+ISIZES)
C     REAL
      my_real H(2),
     . VMY, VMZ, VY, VZ, VV, NY, NZ, VT,
     . NNY, NNZ, FAC, P, TY, TZ,
     .   EFRIC, VOLS, VOLM, TS, TM ,TSTIFM, TSTIFS, DVN,
     .   TSTIFT, PHI, AREAS, AREAM, VN, WN, STENSY, STENSZ,
     .   COMAREA(ISIZEM+ISIZES),COMSTF(ISIZEM+ISIZES),
     .   COMT(ISIZEM+ISIZES),COMVOL(ISIZEM+ISIZES),COMBUF(ISIZEM+ISIZES)
 
c
      DATA IPERM/ 2, 1/,JPERM/ 1, -1/
C-----------------------------------------------
C
C preparation phase for spmd
C
      IF(INTTH/=ZERO) THEN
       IF(ISPMD==0) THEN
        DO II = 1, NRTS
          TAGS(II) = 0
        ENDDO
        DO II = 1, NRTM
          TAGM(II) = 0
        ENDDO
        IS = 0
        IM = 0
        DO II = 1, NSN
          L = IRTL(II)
          IF(ILOC(II)>0.AND.NMN>0)THEN
           IF(TAGM(L)==0)THEN
             IM = IM + 1
             LISTM(IM) = L
             TAGM(L) = IM
           END IF
           LL1=NSEG(II)
           LL2=NSEG(II+1)-1
           DO LL=LL1,LL2
             LG  = LMSR(LL)
             IF(TAGS(LG)==0) THEN
               IS = IS + 1
               LISTS(IS) = LG
               TAGS(LG) = IS
             ENDIF
           ENDDO
          ENDIF
        ENDDO
C
C Elem lists compacting
C
        ITEMP(1) = IS
        ITEMP(2) = IM
       ENDIF
C
C Sending second/mains facet list in contact
C
       IF(NSPMD > 1) THEN
         CALL SPMD_IBCAST(ITEMP,ITEMP,1,2,0,2)
         IS = ITEMP(1)
         IM = ITEMP(2)
         ILEN = IM+IS
         CALL SPMD_IBCAST(LISTM,LISTM,1,IM,0,2)
         CALL SPMD_IBCAST(LISTS,LISTS,1,IS,0,2)
       END IF
       DO II = 1, IM
         L = LISTM(II)
         IX(1) = MSR(IRECT(1,L))
         IX(2) = MSR(IRECT(2,L))
         IF(IELEM(L)>0) THEN
          CALL I9GRD2(
     1       IERR ,COMAREA(II),COMSTF(II),COMT(II),COMVOL(II),
     2       IELEM(L)   ,X  ,IXQ(1,IELEM(L)),IX        ,
     3       IPARG,PM         ,ELBUF_TAB , IGROU   ,IELN    )
           ICOMERR(II) = IERR
           ICOMNGR(II) = IGROU
           ICOMNEL(II) = IELN
         ELSE
           COMAREA(II) = ZERO
           COMSTF(II)  = ZERO
           COMT(II)    = ZERO
           COMVOL(II)  = ZERO
           ICOMERR(II) = 0
           ICOMNGR(II) = 0
           ICOMNEL(II) = 0
         ENDIF
         COMBUF(II) = ZERO
       ENDDO
C
       DO II = 1, IS
         L = LISTS(II)
         IXX(1)=NSV(IRECTS(1,L))
         IXX(2)=NSV(IRECTS(2,L))
         IF(IELES(L)>0) THEN
          CALL I9GRD2(
     1     IERR ,COMAREA(IM+II),COMSTF(IM+II),COMT(IM+II),COMVOL(IM+II),
     2     IELES(L)      ,X    ,IXQ(1,IELES(L))    ,IXX                ,
     3     IPARG,PM            ,ELBUF_TAB ,IGROU   ,IELN               )
           ICOMERR(IM+II) = IERR
           ICOMNGR(IM+II) = IGROU
           ICOMNEL(IM+II) = IELN
         ELSE
           COMAREA(IM+II) = ZERO
           COMSTF(IM+II)  = ZERO
           COMT(IM+II)    = ZERO
           COMVOL(IM+II)  = ZERO
           ICOMERR(IM+II) = 0
           ICOMNGR(IM+II) = 0
           ICOMNEL(IM+II) = 0
         ENDIF
         COMBUF(IM+II) = ZERO
       ENDDO
C
       IF (NSPMD > 1) THEN
C
C gather values
C
                 CALL SPMD_GLOB_DSUM9(COMAREA,ILEN)
                 CALL SPMD_GLOB_DSUM9(COMSTF,ILEN)
                 CALL SPMD_GLOB_DSUM9(COMT,ILEN)
                 CALL SPMD_GLOB_DSUM9(COMVOL,ILEN)
                 CALL SPMD_GLOB_ISUM9(ICOMERR,ILEN)
                 CALL SPMD_GLOB_ISUM9(ICOMNGR,ILEN)
                 CALL SPMD_GLOB_ISUM9(ICOMNEL,ILEN)
C   node part on P0 only
              IF(ISPMD/=0) GOTO 900
       END IF
C   interface processed by p0
      ELSE
       IF(ISPMD/=0) RETURN
      ENDIF
C
      DO 800 II=1,NSN
      LL1=NSEG(II)
      LL2=NSEG(II+1)-1
      N=NSV(II)
      IF(ILOC(II)>0.AND.NMN>0)THEN
C---------------------------------
C       CONTACT
C---------------------------------
        L=IRTL(II)
        DO 10 JJ=1,2
        NN=IRECT(JJ,L)
 10     IX(JJ)=MSR(NN)
C
        H(1) = HALF*(ONE - CRST(1,II))
        H(2) = HALF*(ONE + CRST(1,II))
C---------------------------------
C       mesh velocity
C---------------------------------
        VMY=ZERO
        VMZ=ZERO
C
        DO JJ=1,2
         VMY=VMY+W(2,IX(JJ))*H(JJ)
         VMZ=VMZ+W(3,IX(JJ))*H(JJ)
        ENDDO
C
        W(2,N)=VMY
        W(3,N)=VMZ
C---------------------------------
C       PONT THERMIQUE
C---------------------------------
        IF(INTTH/=ZERO)THEN
           KK = TAGM(L)
           EFRIC = HALF * EE(II) / (LL2-LL1+1)
             IERR = ICOMERR(KK)
             AREAM = COMAREA(KK)
             TSTIFM = COMSTF(KK)
             TM = COMT(KK)
             VOLM = COMVOL(KK)
           IF(IERR==0) THEN
             DO LL = LL1,LL2
               LG = LMSR(LL)
               JJ = TAGS(LG) + IM
               IERR = ICOMERR(JJ)
               AREAS = COMAREA(JJ)
               TSTIFS = COMSTF(JJ)
               TS = COMT(JJ)
               VOLS = COMVOL(JJ)
               IF(IERR==0) THEN
                 TSTIFT = TSTIFM + TSTIFS + TSTIF
                 PHI = AREAS * DT1 * (TM-TS) / TSTIFT
                 COMBUF(JJ) = COMBUF(JJ)
     +                      + (EFRIC+PHI)/VOLS
                 COMBUF(KK) = COMBUF(KK)
     +                      + (EFRIC-PHI)/VOLM
               ENDIF
             ENDDO
           ENDIF
c         ENDIF
        ENDIF
C
      ELSEIF(ILOC(II)<0.OR.NMN==0)THEN
C---------------------------------------
C       PAS DE CONTACT => SURFACE LIBRE
C---------------------------------------
        ILOC(II) = -ILOC(II)
C
        VY = V(2,N) - W(2,N)
        VZ = V(3,N) - W(3,N)
        VV = MAX(EM30,SQRT(VY**2+VZ**2))
        NNY = ZERO
        NNZ = ZERO
C------------------------------------------------------
C       loop over facets connected to node ii
C------------------------------------------------------
        DO 300 LL=LL1,LL2
         LG=LMSR(LL)
         DO 200 KKK=1,2
          KK=KKK
 200     IF(IRECTS(KK,LG)==II) GO TO 250
 250     CONTINUE
C------------------------------------------------------
C         calculation of the normal with upwind on the upstream side
C------------------------------------------------------
          K1 = IPERM(KK)
          I1 = NSV(IRECTS(K1,LG))
          TY = X(2,I1) - X(2,N)
          TZ = X(3,I1) - X(3,N)
          TTT = MAX(EM30,SQRT(TY**2+TZ**2))
C          VT = VY*TY + VZ*TZ
          VT = V(2,N)*TY + V(3,N)*TZ
          P = ONEP0001 - UPW*(HALF + SIGN(HALF,VT))
          NY = TZ
          NZ =-TY
C          FAC = P / MAX(EM30,SQRT(NY**2+NZ**2))
          FAC = P * JPERM(KK)
          NNY = NNY + NY*FAC
          NNZ = NNZ + NZ*FAC
C-------------------------------------
C       surface tension
C-------------------------------------
          STENSY = STENS * TY / TTT
          STENSZ = STENS * TZ / TTT
          A(2,N)  = A(2,N)  + STENSY
          A(3,N)  = A(3,N)  + STENSZ
 300    CONTINUE
        FAC = MAX(EM30,SQRT(NNY**2+NNZ**2))
        NNY = NNY/FAC
        NNZ = NNZ/FAC
C---------------------------------
C       grid boundary conditions
C---------------------------------
        IF(ICODE(N)/=0)THEN
C---------------------------------
C       W LAGRANGIEN ACCORDING TO N
C---------------------------------
          DVN = VY * NNY + VZ * NNZ
          W(2,N) = W(2,N) + DVN * NNY
          W(3,N) = W(3,N) + DVN * NNZ
          CALL BCS2(W(1,N),SKEW(1,ISKEW(N)),ISKEW(N),ICODE(N))
          VN = V(2,N)*NNY + V(3,N)*NNZ
          WN = W(2,N)*NNY + W(3,N)*NNZ
C-------------------------------------
C         W LAGRANGIEN ACCORDING TO N + BCS
C-------------------------------------
          IF(ABS(WN)>EM30)THEN
            FAC = VN / WN
            W(2,N) = W(2,N) * FAC
            W(3,N) = W(3,N) * FAC
          ENDIF
        ELSEIF(IEULT/=0)THEN
C-------------------------------------
C         W LAGRANGIEN ACCORDING TO N
C         W EULERIEN ACCORDING TO T
C---------------------------------
          VN = V(2,N) * NNY + V(3,N) * NNZ
          W(2,N) = VN * NNY
          W(3,N) = VN * NNZ
        ELSE
C-------------------------------------
C         W LAGRANGIEN ACCORDING TO N
C         LIBRE ACCORDING TO T
C---------------------------------
          DVN = VY * NNY + VZ * NNZ
          W(2,N) = W(2,N) + DVN * NNY
          W(3,N) = W(3,N) + DVN * NNZ
        ENDIF
      ENDIF
C
  800 CONTINUE
C
C finalization phase for spmd
C
  900 CONTINUE
      IF(INTTH/=ZERO) THEN
       IF(NSPMD > 1) THEN
C
C Envoi buffer elements updates
C
         CALL SPMD_RBCAST(COMBUF,COMBUF,1,ILEN,0,2)
       END IF
C
C Local Elbuf update
C
       DO II = 1, IM
         L = LISTM(II)
         IF(IELEM(L)>0) THEN
           IGROU = ICOMNGR(II)
           IELN  = ICOMNEL(II)
           ELBUF_TAB(IGROU)%GBUF%EINT(IELN) = 
     .     ELBUF_TAB(IGROU)%GBUF%EINT(IELN) + COMBUF(II)
         ENDIF
       ENDDO
C
       DO II = 1, IS
         L =  LISTS(II)
         IF(IELES(L)>0) THEN
           IGROU = ICOMNGR(IM+II)
           IELN  = ICOMNEL(IM+II)
           ELBUF_TAB(IGROU)%GBUF%EINT(IELN) = 
     .     ELBUF_TAB(IGROU)%GBUF%EINT(IELN) + COMBUF(IM+II)
         ENDIF
       ENDDO
      ENDIF
C
      RETURN
      END
