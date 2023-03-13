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
Chd|  I9WAL3                        source/interfaces/int09/i9wal3.F
Chd|-- called by -----------
Chd|        I9WALE                        source/interfaces/int09/i9wale.F
Chd|-- calls ---------------
Chd|        BCS2                          source/constraints/general/bcs/bcs2.F
Chd|        I9GRD3                        source/interfaces/int09/i9grd3.F
Chd|        SHAPEH                        source/ale/inter/shapeh.F     
Chd|        SPMD_GLOB_DSUM9               source/mpi/interfaces/spmd_th.F
Chd|        SPMD_GLOB_ISUM9               source/mpi/interfaces/spmd_th.F
Chd|        SPMD_IBCAST                   source/mpi/generic/spmd_ibcast.F
Chd|        SPMD_RBCAST                   source/mpi/generic/spmd_rbcast.F
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|====================================================================
      SUBROUTINE I9WAL3(X    ,V         ,W         ,A     ,CRST     ,
     2          NSV      ,ILOC      ,IRTL      ,ICODE     ,ISKEW    ,
     3          SKEW     ,MSR       ,LMSR      ,NSEG      ,IRECTS   ,
     4          IRECT    ,UPW       ,IXS       ,ELBUF_TAB,
     5          IPARG    ,PM        ,NALE      ,EE        ,IELES    ,
     6          IELEM    ,TSTIF     ,INTTH     ,IEULT     ,STENS    ,
     7          NOR      ,ISIZES    ,ISIZEM    )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE ELBUFDEF_MOD            
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com08_c.inc"
#include      "scr04_c.inc"
#include      "scr08_a_c.inc"
#include      "param_c.inc"
#include      "task_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IRECT(4,*), NSV(*), ILOC(*), IRTL(*), ICODE(*), ISKEW(*),
     .   MSR(*), IRECTS(4,*), LMSR(*), NSEG(*),IXS(NIXS,*),
     .   IPARG(NPARG,*), IELES(*), IELEM(*),NALE(*) ,
     .   INTTH, IEULT, ISIZES, ISIZEM
C     REAL
      my_real
     .   UPW, TSTIF,TTT, STENS,
     .   X(3,*), V(3,*), W(3,*), A(3,*), CRST(2,*), SKEW(LSKEW,*),
     .   PM(NPROPM,*),EE(*),NOR(3,*)
      TYPE (ELBUF_STRUCT_), TARGET, DIMENSION(NGROUP) :: ELBUF_TAB
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER II, N, L, JJ, NN, LL, LL1, LL2, LG, KK, KKK, K1, K2,
     .   I1, I2, IERR, IGROU, IELN,
     .   IXX(4), IPERM(0:5),
     .   ITEMP(2), IS, IM, ILEN,
     .   TAGS(ISIZES),TAGM(ISIZEM), LISTS(ISIZES),LISTM(ISIZEM),
     .   ICOMERR(ISIZEM+ISIZES),ICOMNGR(ISIZEM+ISIZES),
     .   ICOMNEL(ISIZEM+ISIZES)
C     REAL
      my_real
     .   H(4), VMX, VMY, VMZ, VX, VY, VZ, VV, NX, NY, NZ, VT,
     .   NNX, NNY, NNZ, FAC, P, X1, Y1, Z1,X2, Y2, Z2, TX, TY, TZ,
     .   EFRIC, VOLS, VOLM, TS, TM ,TSTIFM, TSTIFS, DVN, TT2, TT3,
     .   TSTIFT, PHI, AREAS, AREAM, VN, WN, T2X, T2Y, T2Z, T2T,
     .   T3X, T3Y, T3Z, STENSX, STENSY, STENSZ,
     .   COMAREA(ISIZEM+ISIZES),COMSTF(ISIZEM+ISIZES),
     .   COMT(ISIZEM+ISIZES),COMVOL(ISIZEM+ISIZES),COMBUF(ISIZEM+ISIZES)
      TYPE(G_BUFEL_)  ,POINTER :: GBUF     
c
      DATA IPERM/ 4, 1, 2, 3, 4, 1/
C-----------------------------------------------
C
C Phase de Preparation pour SPMD
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
          IF(ILOC(II)>0.AND.NMN>0.AND.TAGM(L)==0)THEN
            IM = IM + 1
            LISTM(IM) = L
            TAGM(L) = IM
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
C Compactage listes elem
C
        ITEMP(1) = IS
        ITEMP(2) = IM
       ENDIF
C
C Envoi liste facettes seconds/mains en contact
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
         IX(3) = MSR(IRECT(3,L))
         IX(4) = MSR(IRECT(4,L))
         IF(IELEM(L)>0) THEN
           CALL I9GRD3(
     1       IERR ,COMAREA(II),COMSTF(II),COMT(II),COMVOL(II),
     2       IELEM(L)   ,X         ,IXS(1,IELEM(L)), IX,
     3       IPARG,PM         ,ELBUF_TAB ,IGROU   ,IELN    )
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
         IXX(3)=NSV(IRECTS(3,L))
         IXX(4)=NSV(IRECTS(4,L))
         IF(IELES(L)>0) THEN
          CALL I9GRD3(
     1     IERR ,COMAREA(IM+II),COMSTF(IM+II),COMT(IM+II),COMVOL(IM+II),
     2     IELES(L)      ,X,IXS(1,IELES(L))        ,IXX          ,
     3     IPARG,PM            ,ELBUF_TAB    ,IGROU   ,IELN    )
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
C gather des valeurs
C
         CALL SPMD_GLOB_DSUM9(COMAREA,ILEN)
         CALL SPMD_GLOB_DSUM9(COMSTF,ILEN)
         CALL SPMD_GLOB_DSUM9(COMT,ILEN)
         CALL SPMD_GLOB_DSUM9(COMVOL,ILEN)
         CALL SPMD_GLOB_ISUM9(ICOMERR,ILEN)
 	       CALL SPMD_GLOB_ISUM9(ICOMNGR,ILEN)
 	       CALL SPMD_GLOB_ISUM9(ICOMNEL,ILEN)
Cel partie noeud sur P0 uniquement
         IF(ISPMD/=0) GOTO 900
       END IF
Cel interface traitee par p0
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
        DO 10 JJ=1,4
        NN=IRECT(JJ,L)
 10     IX(JJ)=MSR(NN)
C
        CALL SHAPEH(H,CRST(1,II),CRST(2,II))
C---------------------------------
C       VITESSE DE MAILLAGE
C---------------------------------
        VMX=ZERO
        VMY=ZERO
        VMZ=ZERO
C
        DO 30 JJ=1,4
        VMX=VMX+W(1,IX(JJ))*H(JJ)
        VMY=VMY+W(2,IX(JJ))*H(JJ)
 30     VMZ=VMZ+W(3,IX(JJ))*H(JJ)
C
        DVN = (VMX-W(1,N)) * NOR(1,II)
     .        + (VMY-W(2,N)) * NOR(2,II)
     .        + (VMZ-W(3,N)) * NOR(3,II)
        W(1,N) = W(1,N) + DVN * NOR(1,II)
        W(2,N) = W(2,N) + DVN * NOR(2,II)
        W(3,N) = W(3,N) + DVN * NOR(3,II)
C
C        W(1,N)=VMX
C        W(2,N)=VMY
C        W(3,N)=VMZ
C---------------------------------
C       PONT THERMIQUE
C---------------------------------
        IF(INTTH/=ZERO)THEN
c         IF(IMACH/=3) THEN
c          CALL I9GRD3(
c     1       IERR  ,AREAM   ,TSTIFM,TM             ,VOLM      ,
c     2       NB3M  ,IELEM(L),X     ,IXS(1,IELEM(L)),IX        ,
c     3       IPARG ,PM      ,ELBUF )
c          IF(IERR==0) THEN
c            EFRIC = HALF * EE(II) / (LL2-LL1+1)
c            DO 80 LL=LL1,LL2
c             LG  = LMSR(LL)
c             IXX(1)=NSV(IRECTS(1,LG))
c             IXX(2)=NSV(IRECTS(2,LG))
c             IXX(3)=NSV(IRECTS(3,LG))
c             IXX(4)=NSV(IRECTS(4,LG))
c             CALL I9GRD3(
c     1        IERR  ,AREAS    ,TSTIFS,TS              ,VOLS    ,
c     2        NB3S  ,IELES(LG),X     ,IXS(1,IELES(LG)),IXX     ,
c     3        IPARG ,PM       ,ELBUF )
c             IF(IERR==0) THEN
c               TSTIFT = TSTIFM + TSTIFS + TSTIF
c               PHI = AREAS * DT1 * (TM-TS) / TSTIFT
c               ELBUF(NB3S) = ELBUF(NB3S) + (EFRIC+PHI)/VOLS
c               ELBUF(NB3M) = ELBUF(NB3M) + (EFRIC-PHI)/VOLM
c             ENDIF
c 80         CONTINUE
c          ENDIF
c         ELSEIF(ISPMD==0)THEN
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
C------------------------------------------------------
C       BOUCLE SUR LES FACETTES CONNECTEES AU NOEUD II
C------------------------------------------------------
        ILOC(II) = -ILOC(II)
C
        VX = V(1,N) - W(1,N)
        VY = V(2,N) - W(2,N)
        VZ = V(3,N) - W(3,N)
        VV = MAX(EM30,SQRT(VX**2+VY**2+VZ**2))
        NNX = ZERO
        NNY = ZERO
        NNZ = ZERO
C------------------------------------------------------
C       BOUCLE SUR LES FACETTES CONNECTEES AU NOEUD II
C------------------------------------------------------
        DO 300 LL=LL1,LL2
         LG=LMSR(LL)
         DO 200 KKK=1,4
          KK=KKK
 200     IF(IRECTS(KK,LG)==II) GO TO 250
 250     CONTINUE
C------------------------------------------------------
C         CALCUL DE LA NORMALE AVEC UPWIND SUR L'AMONT
C------------------------------------------------------
          K1 = IPERM(KK-1)
          K2 = IPERM(KK+1)
          I1 = NSV(IRECTS(K1,LG))
          I2 = NSV(IRECTS(K2,LG))
          X1 = X(1,I1) - X(1,N)
          Y1 = X(2,I1) - X(2,N)
          Z1 = X(3,I1) - X(3,N)
          X2 = X(1,I2) - X(1,N)
          Y2 = X(2,I2) - X(2,N)
          Z2 = X(3,I2) - X(3,N)
          TX = X1 + X2
          TY = Y1 + Y2
          TZ = Z1 + Z2
          TTT = MAX(EM30,SQRT(TX**2+TY**2+TZ**2))
          VT = V(1,N)*TX + V(2,N)*TY + V(3,N)*TZ
          P = ONEP0001 - UPW*(HALF + SIGN(HALF,VT))
          NX = Y1 * Z2 - Z1 * Y2
          NY = Z1 * X2 - X1 * Z2
          NZ = X1 * Y2 - Y1 * X2
C          FAC = P / MAX(EM30,SQRT(NX**2+NY**2+NZ**2))
          FAC = P
          NNX = NNX + NX*FAC
          NNY = NNY + NY*FAC
          NNZ = NNZ + NZ*FAC
C-------------------------------------
C       TENSION DE SURFACE
C-------------------------------------
          IF(STENS>ZERO)THEN
            T2X = -X1 + X2
            T2Y = -Y1 + Y2
            T2Z = -Z1 + Z2
            TT2 = MAX(EM30,T2X**2+T2Y**2+T2Z**2)
            T2T = (T2X*TX + T2Y*TY +T2Z*TZ) / TT2
            T3X = TX - T2X * T2T
            T3Y = TY - T2Y * T2T
            T3Z = TZ - T2Z * T2T
            TT3 = STENS * SQRT(TT2/MAX(EM30,T3X**2+T3Y**2+T3Z**2))
            STENSX = T3X * TT3
            STENSY = T3Y * TT3
            STENSZ = T3Z * TT3
            A(1,N)  = A(1,N)  + STENSX
            A(2,N)  = A(2,N)  + STENSY
            A(3,N)  = A(3,N)  + STENSZ
          ENDIF
 300    CONTINUE
        FAC = MAX(EM30,SQRT(NNX**2+NNY**2+NNZ**2))
        NNX = NNX/FAC
        NNY = NNY/FAC
        NNZ = NNZ/FAC
C
C---------------------------------
C       W LAGRANGIEN SUIVANT N
C---------------------------------
C---------------------------------
C       BCS DE GRILLE
C---------------------------------
        IF(ICODE(N)/=0)THEN
          DVN = VX * NNX + VY * NNY + VZ * NNZ
          W(1,N) = W(1,N) + DVN * NNX
          W(2,N) = W(2,N) + DVN * NNY
          W(3,N) = W(3,N) + DVN * NNZ
          CALL BCS2(W(1,N),SKEW(1,ISKEW(N)),ISKEW(N),ICODE(N))
          VN = V(1,N)*NNX + V(2,N)*NNY + V(3,N)*NNZ
          WN = W(1,N)*NNX + W(2,N)*NNY + W(3,N)*NNZ
C-------------------------------------
C         W LAGRANGIEN SUIVANT N + BCS
C-------------------------------------
          IF(ABS(WN)>EM30)THEN
            FAC = VN / WN
            W(1,N) = W(1,N) * FAC
            W(2,N) = W(2,N) * FAC
            W(3,N) = W(3,N) * FAC
          ENDIF
        ELSEIF(IEULT/=0)THEN
C-------------------------------------
C         W LAGRANGIEN SUIVANT N
C         W EULERIEN SUIVANT T
C---------------------------------
          VN = V(1,N)*NNX + V(2,N)*NNY + V(3,N)*NNZ
          W(1,N) = VN * NNX
          W(2,N) = VN * NNY
          W(3,N) = VN * NNZ
        ELSE
C-------------------------------------
C         W LAGRANGIEN SUIVANT N
C         LIBRE SUIVANT T
C---------------------------------
          DVN = VX * NNX + VY * NNY + VZ * NNZ
          W(1,N) = W(1,N) + DVN * NNX
          W(2,N) = W(2,N) + DVN * NNY
          W(3,N) = W(3,N) + DVN * NNZ
        ENDIF
      ENDIF
C
  800 CONTINUE
C
C Phase de Finalisation pour SPMD
C
  900 CONTINUE
      IF(INTTH/=ZERO) THEN
       IF(NSPMD > 1) THEN
C
C Envoi buffer elems updates
C
         CALL SPMD_RBCAST(COMBUF,COMBUF,1,ILEN,0,2)
       END IF
C
C Mise a jour ELBUF local
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
