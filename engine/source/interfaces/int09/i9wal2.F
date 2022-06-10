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
Chd|  I9WAL2                        source/interfaces/int09/i9wal2.F
Chd|-- called by -----------
Chd|        I9WALE                        source/interfaces/int09/i9wale.F
Chd|-- calls ---------------
Chd|        BCS2                          source/constraints/general/bcs/bcs2.F
Chd|        I9GRD2                        source/interfaces/int09/i9grd2.F
Chd|        SPMD_GLOB_DSUM9               source/mpi/interfaces/spmd_th.F
Chd|        SPMD_GLOB_ISUM9               source/mpi/interfaces/spmd_th.F
Chd|        SPMD_IBCAST                   source/mpi/generic/spmd_ibcast.F
Chd|        SPMD_RBCAST                   source/mpi/generic/spmd_rbcast.F
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|====================================================================
      SUBROUTINE I9WAL2(X,V   ,W         ,A         ,CRST      ,
     2   NSV       ,ILOC      ,IRTL      ,ICODE     ,ISKEW     ,
     3   SKEW      ,MSR       ,LMSR      ,NSEG      ,IRECTS    ,
     4   IRECT     ,UPW       ,IXQ       ,ELBUF_TAB ,
     5   IPARG     ,PM        ,NALE      ,EE        ,IELES     ,
     6   IELEM     ,TSTIF     ,INTTH     ,IEULT     ,STENS     ,
     7   ISIZES    ,ISIZEM    )
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
#include      "scr08_s_c.inc"
#include      "scr08_a_c.inc"
#include      "param_c.inc"
#include      "scr05_c.inc"
#include      "task_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IRECT(4,*), NSV(*), ILOC(*), IRTL(*), ICODE(*), ISKEW(*),
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
      INTEGER II, N, L, JJ, NN, LL, LL1, LL2, LG, KK, KKK, K1, K2,
     .   I1, I2, IERR, IGROU, IELN,
     .   IXX(4), IPERM(2), JPERM(2),
     .   ITEMP(2), IS, IM, ILEN,
     .   TAGS(ISIZES),TAGM(ISIZEM), LISTS(ISIZES),LISTM(ISIZEM),
     .   ICOMERR(ISIZEM+ISIZES),ICOMNGR(ISIZEM+ISIZES),
     .   ICOMNEL(ISIZEM+ISIZES)
C     REAL
      my_real H(2),
     .   VMX, VMY, VMZ, VX, VY, VZ, VV, NX, NY, NZ, VT,
     .   NNX, NNY, NNZ, FAC, P, X1, Y1, Z1,X2, Y2, Z2, TX, TY, TZ,
     .   EFRIC, VOLS, VOLM, TS, TM ,TSTIFM, TSTIFS, DVN,
     .   TSTIFT, PHI, AREAS, AREAM, VN, WN, STENSY, STENSZ,
     .   COMAREA(ISIZEM+ISIZES),COMSTF(ISIZEM+ISIZES),
     .   COMT(ISIZEM+ISIZES),COMVOL(ISIZEM+ISIZES),COMBUF(ISIZEM+ISIZES)
      TYPE(G_BUFEL_)  ,POINTER :: GBUF     
c
      DATA IPERM/ 2, 1/,JPERM/ 1, -1/
C-----------------------------------------------
C
C Phase de Preparation pour SPMD
C
      IF(INTTH.NE.ZERO) THEN
       IF(ISPMD.EQ.0) THEN
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
          IF(ILOC(II).GT.0.AND.NMN.GT.0)THEN
           IF(TAGM(L).EQ.0)THEN
             IM = IM + 1
             LISTM(IM) = L
             TAGM(L) = IM
           END IF
           LL1=NSEG(II)
           LL2=NSEG(II+1)-1
           DO LL=LL1,LL2
             LG  = LMSR(LL)
             IF(TAGS(LG).EQ.0) THEN
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
         IF(IELEM(L).GT.0) THEN
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
         IF(IELES(L).GT.0) THEN
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
 	    IF(ISPMD.NE.0) GOTO 900
       END IF
Cel interface traitee par p0
      ELSE
       IF(ISPMD.NE.0) RETURN
      ENDIF
C
      DO 800 II=1,NSN
      LL1=NSEG(II)
      LL2=NSEG(II+1)-1
      N=NSV(II)
      IF(ILOC(II).GT.0.AND.NMN.GT.0)THEN
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
C       VITESSE DE MAILLAGE
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
        IF(INTTH.NE.ZERO)THEN
c         IF(IMACH.NE.3) THEN
c          CALL I9GRD2(
c     1       IERR  ,AREAM   ,TSTIFM,TM             ,VOLM      ,
c     2       NB3M  ,IELEM(L),X     ,IXQ(1,IELEM(L)),IX        ,
c     3       IPARG ,PM      ,ELBUF )
c          IF(IERR.EQ.0) THEN
c            EFRIC = HALF * EE(II) / (LL2-LL1 + 1)
c            DO 80 LL=LL1,LL2
c             LG  = LMSR(LL)
c             IXX(1)=NSV(IRECTS(1,LG))
c             IXX(2)=NSV(IRECTS(2,LG))
c             CALL I9GRD2(
c     1        IERR  ,AREAS    ,TSTIFS,TS              ,VOLS    ,
c     2        NB3S  ,IELES(LG),X     ,IXQ(1,IELES(LG)),IXX     ,
c     3        IPARG ,PM       ,ELBUF )
c             IF(IERR.EQ.0) THEN
c               TSTIFT = TSTIFM + TSTIFS + TSTIF
c               PHI = AREAS * DT1 * (TM-TS) / TSTIFT
c               ELBUF(NB3S) = ELBUF(NB3S) + (EFRIC+PHI)/VOLS
c               ELBUF(NB3M) = ELBUF(NB3M) + (EFRIC-PHI)/VOLM
c             ENDIF
c 80         CONTINUE
c          ENDIF
c         ELSEIF(ISPMD.EQ.0)THEN
           KK = TAGM(L)
           EFRIC = HALF * EE(II) / (LL2-LL1+1)
	   IERR = ICOMERR(KK)
	   AREAM = COMAREA(KK)
	   TSTIFM = COMSTF(KK)
	   TM = COMT(KK)
	   VOLM = COMVOL(KK)
           IF(IERR.EQ.0) THEN
             DO LL = LL1,LL2
               LG = LMSR(LL)
               JJ = TAGS(LG) + IM
               IERR = ICOMERR(JJ)
               AREAS = COMAREA(JJ)
               TSTIFS = COMSTF(JJ)
               TS = COMT(JJ)
               VOLS = COMVOL(JJ)
               IF(IERR.EQ.0) THEN
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
      ELSEIF(ILOC(II).LT.0.OR.NMN.EQ.0)THEN
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
C       BOUCLE SUR LES FACETTES CONNECTEES AU NOEUD II
C------------------------------------------------------
        DO 300 LL=LL1,LL2
         LG=LMSR(LL)
         DO 200 KKK=1,2
          KK=KKK
 200     IF(IRECTS(KK,LG).EQ.II) GO TO 250
 250     CONTINUE
C------------------------------------------------------
C         CALCUL DE LA NORMALE AVEC UPWIND SUR L'AMONT
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
C       TENSION DE SURFACE
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
C       BCS DE GRILLE
C---------------------------------
        IF(ICODE(N).NE.0)THEN
C---------------------------------
C       W LAGRANGIEN SUIVANT N
C---------------------------------
          DVN = VY * NNY + VZ * NNZ
          W(2,N) = W(2,N) + DVN * NNY
          W(3,N) = W(3,N) + DVN * NNZ
          CALL BCS2(W(1,N),SKEW(1,ISKEW(N)),ISKEW(N),ICODE(N))
          VN = V(2,N)*NNY + V(3,N)*NNZ
          WN = W(2,N)*NNY + W(3,N)*NNZ
C-------------------------------------
C         W LAGRANGIEN SUIVANT N + BCS
C-------------------------------------
          IF(ABS(WN).GT.EM30)THEN
            FAC = VN / WN
            W(2,N) = W(2,N) * FAC
            W(3,N) = W(3,N) * FAC
          ENDIF
        ELSEIF(IEULT.NE.0)THEN
C-------------------------------------
C         W LAGRANGIEN SUIVANT N
C         W EULERIEN SUIVANT T
C---------------------------------
          VN = V(2,N) * NNY + V(3,N) * NNZ
          W(2,N) = VN * NNY
          W(3,N) = VN * NNZ
        ELSE
C-------------------------------------
C         W LAGRANGIEN SUIVANT N
C         LIBRE SUIVANT T
C---------------------------------
          DVN = VY * NNY + VZ * NNZ
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
      IF(INTTH.NE.ZERO) THEN
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
         IF(IELEM(L).GT.0) THEN
           IGROU = ICOMNGR(II)
           IELN  = ICOMNEL(II)
           ELBUF_TAB(IGROU)%GBUF%EINT(IELN) = 
     .     ELBUF_TAB(IGROU)%GBUF%EINT(IELN) + COMBUF(II)
         ENDIF
       ENDDO
C
       DO II = 1, IS
         L =  LISTS(II)
         IF(IELES(L).GT.0) THEN
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
