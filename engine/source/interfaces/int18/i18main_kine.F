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
Chd|  I18MAIN_KINE_1                source/interfaces/int18/i18main_kine.F
Chd|-- called by -----------
Chd|        RESOL                         source/engine/resol.F         
Chd|-- calls ---------------
Chd|        I18MAIN_KINE_F                source/interfaces/int18/i18main_kine.F
Chd|        I18MAIN_KINE_I                source/interfaces/int18/i18main_kine.F
Chd|        I18_KINE_M                    source/interfaces/int18/i18main_kine.F
Chd|        MY_BARRIER                    source/system/machine.F       
Chd|        SPMD_I18KINE_COM_ACC          source/mpi/interfaces/spmd_i18kine_com_acc.F
Chd|        SPMD_I18KINE_COM_MS           source/mpi/interfaces/spmd_i18kine_com_ms.F
Chd|        SPMD_I18KINE_MACC_COM_POFF    source/mpi/interfaces/spmd_i18kine_macc_com_poff.F
Chd|        SPMD_I18KINE_MSF_COM_POFF     source/mpi/interfaces/spmd_i18kine_msf_com_poff.F
Chd|        SPMD_I18KINE_PENE_COM_POFF    source/mpi/interfaces/spmd_i18kine_pene_com_poff.F
Chd|        H3D_MOD                       share/modules/h3d_mod.F       
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|        TRI7BOX                       share/modules/tri7box.F       
Chd|====================================================================
      SUBROUTINE I18MAIN_KINE_1(IPARI,INTBUF_TAB,X      ,V       ,
     2                   A    ,ISKEW   ,SKEW    ,LCOD    ,WA      ,
     3                   MS   ,ITAB    ,JTASK   ,KINET   ,STIFN   ,
     4                   MTF  ,CAND_SAV,INT18ADD,IAD_ELEM,FR_ELEM ,
     5                   TAGPENE,H3D_DATA)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE TRI7BOX
      USE INTBUFDEF_MOD
      USE H3D_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "com08_c.inc"
#include      "param_c.inc"
#include      "secur_c.inc"
#include      "task_c.inc"
#include      "units_c.inc"
#include      "warn_c.inc"
#include      "scr05_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IPARI(NPARI,*), ISKEW(*), LCOD(*), ITAB(*),
     .   KINET(*),INT18ADD(*),JTASK,IAD_ELEM(2,*),FR_ELEM(*),TAGPENE(*)
C     REAL ou REAL*8
      my_real
     .   X(*), V(*), A(3,*), SKEW(*), WA(*), MS(*),
     .   MTF(14,*),CAND_SAV(*),STIFN(*)
      TYPE(INTBUF_STRUCT_) INTBUF_TAB(*)
      TYPE(H3D_DATABASE) :: H3D_DATA
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,NODF,NODL,NRTMDIM
      INTEGER N, NTY, NMN, NINT,INACTI,LINDMAX
      INTEGER NB_JLT(PARASIZ),NB_JLT_NEW(PARASIZ),NB_STOK_N(PARASIZ),
     *        NN,P,NODFI,IERROR1,IERROR2,IERROR3,IERROR4
      my_real
     .    STARTT,STOPT,BID
      SAVE NB_JLT,NB_JLT_NEW,NB_STOK_N
C=================================================================
C     initialisation MTF(1:14,1:NUMNOD)
C=================================================================
C
c
c     a reecrire sur mains uniquement
      NODF = 1 + (JTASK-1)*NUMNOD / NTHREAD
      NODL = JTASK*NUMNOD / NTHREAD
      DO I = NODF,NODL
        MTF(1,I) = ZERO   ! Mxx mains
        MTF(2,I) = ZERO   ! Mxy mains
        MTF(3,I) = ZERO   ! Mxz mains
        MTF(4,I) = ZERO   ! Myy mains
        MTF(5,I) = ZERO   ! Myz mains
        MTF(6,I) = ZERO   ! Mzz mains
        MTF(7,I) = ZERO   ! Fx mains
        MTF(8,I) = ZERO   ! Fy mains
        MTF(9,I) = ZERO   ! Fz mains
        MTF(10,I)= ZERO   ! PENE cumulee sur second
c        MTF(11,I)= EP30   ! Distance relative min
        MTF(11,I)= ZERO   ! pene max
        MTF(12,I) = ZERO  ! Nx second
        MTF(13,I) = ZERO  ! Ny second
        MTF(14,I) = ZERO  ! Nz second
      ENDDO

      IF (NSPMD > 1)THEN
c
        DO N=1,NINTER
          NTY=IPARI(7,N)
          INACTI =IPARI(22,N)
C
          IF (NTY==7.AND.INACTI==7.AND.IPARI(34,N)==-2)THEN
          NODFI=0
          DO P = 1, NSPMD
            NODFI = NODFI + NSNFI(N)%P(P)
          END DO

          IF(NODFI > 0)THEN
            IF(ASSOCIATED(MTFI_PENE(N)%P)) DEALLOCATE(MTFI_PENE(N)%P)
            ALLOCATE(MTFI_PENE(N)%P(NODFI),STAT=IERROR1)
            MTFI_PENE(N)%P(1:NODFI)=ZERO
c
            IF(ASSOCIATED(MTFI_PENEMIN(N)%P))
     *                                     DEALLOCATE(MTFI_PENEMIN(N)%P)
            ALLOCATE(MTFI_PENEMIN(N)%P(NODFI),STAT=IERROR2)
            MTFI_PENEMIN(N)%P(1:NODFI)=ZERO

            IF(ASSOCIATED(MTFI_V(N)%P)) DEALLOCATE(MTFI_V(N)%P)
            ALLOCATE(MTFI_V(N)%P(6,NODFI),STAT=IERROR3)
            MTFI_V(N)%P(1,1:NODFI)=ZERO
            MTFI_V(N)%P(2,1:NODFI)=ZERO
            MTFI_V(N)%P(3,1:NODFI)=ZERO
            MTFI_V(N)%P(4,1:NODFI)=ZERO
            MTFI_V(N)%P(5,1:NODFI)=ZERO
            MTFI_V(N)%P(6,1:NODFI)=ZERO

            IF(ASSOCIATED(MTFI_A(N)%P)) DEALLOCATE(MTFI_A(N)%P)
            ALLOCATE(MTFI_A(N)%P(7,NODFI),STAT=IERROR4)
            MTFI_A(N)%P(1,1:NODFI)=ZERO
            MTFI_A(N)%P(2,1:NODFI)=ZERO
            MTFI_A(N)%P(3,1:NODFI)=ZERO
            MTFI_A(N)%P(4,1:NODFI)=ZERO
            MTFI_A(N)%P(5,1:NODFI)=ZERO
            MTFI_A(N)%P(6,1:NODFI)=ZERO
            MTFI_A(N)%P(7,1:NODFI)=ZERO

            IF(ASSOCIATED(MTFI_N(N)%P)) DEALLOCATE(MTFI_N(N)%P)
            ALLOCATE(MTFI_N(N)%P(3,NODFI),STAT=IERROR4)
            MTFI_N(N)%P(1,1:NODFI)=ZERO
            MTFI_N(N)%P(2,1:NODFI)=ZERO
            MTFI_N(N)%P(3,1:NODFI)=ZERO

            IF(ASSOCIATED(I18KAFI(N)%P)) DEALLOCATE(I18KAFI(N)%P)
            ALLOCATE(I18KAFI(N)%P(3,NODFI),STAT=IERROR4)
            I18KAFI(N)%P(1,1:NODFI)=ZERO
            I18KAFI(N)%P(2,1:NODFI)=ZERO
            I18KAFI(N)%P(3,1:NODFI)=ZERO
          ENDIF
         ENDIF
        ENDDO
        TAGPENE(1:NUMNOD)=0
      ENDIF
C -------------------
      CALL MY_BARRIER
C -------------------
      IF (NSPMD > 1)THEN
C     MSFI (MASSE des noeuds seconds  n est pas mis a jour
C     la masse est necessaire pour les calculs ensuite
          CALL SPMD_I18KINE_COM_MS(IPARI,INTBUF_TAB,
     *                MTF,MS,ITAB)
      ENDIF
C-----------------------------------------------
C statistique interface
      IF (DEBUG(3).GE.1.AND.NCYCLE.EQ.0) THEN
        NB_JLT(JTASK) = 0
        NB_JLT_NEW(JTASK) = 0
        NB_STOK_N(JTASK) = 0
      ENDIF
C=================================================================
C     calcul des penetrations cumulees ...
C     MTF(10,i) pene cumulee
C     MTF(11,i) dist min relative
C     MTF(12:14,i) normales cumulee
C=================================================================
      DO N=1,NINTER
       NTY   =IPARI(7,N)
       INACTI =IPARI(22,N)
C LINDMAX = NCONT*MULTIMP
       LINDMAX  = IPARI(18,N)*IPARI(23,N)
       IF(NTY==7.and.IPARI(34,N)==-2.and.INACTI==7)THEN
         NRTMDIM=IPARI(4,N)
         NMN    =IPARI(6,N)
         CALL I18MAIN_KINE_I(
     1 N,IPARI(1,N),INTBUF_TAB(N)     ,X        ,
     2 STIFN     ,V         ,A        ,MS               ,NMN      ,
     3 ITAB      ,LINDMAX  ,CAND_SAV(INT18ADD(N)),MTF  ,
     4 NRTMDIM,JTASK,NB_JLT(JTASK),NB_JLT_NEW(JTASK),NB_STOK_N(JTASK),
     5 KINET    )
       ENDIF
      ENDDO
C=================================================================
!$OMP SINGLE

C     COMM SPMD : MTF(10,*)=somme des PENE
C     COMM SPMD : MTF(11,*)=min des distances relatives
C     COMM SPMD : MTF(12:14,*)=sommes des normales
c   1: envoie sur le proc qui possede le noeud
c   2: cumul(ou min) sur les noeuds frontieres secnd 18
c   2: cumul sur les noeuds frontieres secnd 18
      IF (NSPMD > 1)THEN
         CALL SPMD_I18KINE_PENE_COM_POFF(IPARI,INTBUF_TAB,BID,
     *                      MTF,A,IAD_ELEM,FR_ELEM,1,BID,TAGPENE,ITAB,
     .                      H3D_DATA  )

c     Besoin des accelerations pour le noeud second
c     quand on a surface main, noeud second. distant
c     Dans le meme cas, retour du MTF_PENE+MTF_PENEMIN pour les
c     calculs suivants.

          CALL SPMD_I18KINE_COM_ACC(IPARI,INTBUF_TAB,
     *                MTF,A,ITAB,TAGPENE)
      ENDIF
!$OMP END SINGLE
C=================================================================
C     calcul des forces et des masses a transmettre aux mains
c     ponderation en pene/somme(pene)
C=================================================================
C -------------------
      CALL MY_BARRIER
C -------------------
      DO N=1,NINTER
       NTY   =IPARI(7,N)
       INACTI =IPARI(22,N)
       IF(NTY==7.and.IPARI(34,N)==-2.and.INACTI==7)THEN
         CALL I18MAIN_KINE_F( N,
     1 IPARI(1,N)   ,INTBUF_TAB(N)  ,X      ,STIFN    ,
     2 V            ,A          ,MS         ,ITAB       ,LINDMAX  ,
     3 CAND_SAV(INT18ADD(N)),MTF,JTASK,NB_JLT(JTASK),NB_JLT_NEW(JTASK),
     4 NB_STOK_N(JTASK))
       ENDIF
      ENDDO
C=================================================================
C     COMM SPMD : MTF(1:9,*)= masses et forces des mains
C     comm sur les noeuds frontieres main int 18
!$OMP SINGLE
      IF (NSPMD > 1)THEN
        CALL SPMD_I18KINE_MSF_COM_POFF(MTF,IAD_ELEM,FR_ELEM,ITAB)
      ENDIF
!$OMP END SINGLE
C=================================================================
C     calcul des nouvelles accelerations des mains
C=================================================================
C -------------------
      CALL MY_BARRIER
C -------------------
      DO N=1,NINTER
       NTY   =IPARI(7,N)
       INACTI =IPARI(22,N)
       IF(NTY==7.and.IPARI(34,N)==-2.and.INACTI==7)THEN
         NMN    =IPARI(6,N)
         STARTT=INTBUF_TAB(N)%VARIABLES(3)
         STOPT =INTBUF_TAB(N)%VARIABLES(11)
         IF(TT >= STARTT .and. TT <= STOPT)THEN
           CALL I18_KINE_M(
     1      JTASK-1  ,NMN    ,INTBUF_TAB(N)%MSR,V,A   ,MS      ,
     2      MTF      ,ISKEW  ,SKEW         ,LCOD      ,ITAB   )
         ENDIF
       ENDIF
      ENDDO
C=================================================================
c     si MTF(1,*) /= 0 le noeuds est main int 18 sur ce proc
c     et peut etre utilise comme flag

c     => envoyer MTF(1,*) et A(1:3,*) pour TOUS les noeuds frontieres
c     en reception si MTF(recu) est non nul
c     => ecraser A(local) par A(recu)
!$OMP SINGLE
      IF(NSPMD > 1) CALL SPMD_I18KINE_MACC_COM_POFF(MTF,A,
     *                                       IAD_ELEM,FR_ELEM,ITAB)
!$OMP END SINGLE
C=================================================================

      RETURN
C
      END
Chd|====================================================================
Chd|  I18MAIN_KINE_2                source/interfaces/int18/i18main_kine.F
Chd|-- called by -----------
Chd|        RESOL                         source/engine/resol.F         
Chd|-- calls ---------------
Chd|        I18MAIN_KINE_S                source/interfaces/int18/i18main_kine.F
Chd|        I18MAIN_KINE_V                source/interfaces/int18/i18main_kine.F
Chd|        MY_BARRIER                    source/system/machine.F       
Chd|        SPMD_I18KINE_COM_A            source/mpi/interfaces/spmd_i18kine_com_a.F
Chd|        SPMD_I18KINE_COM_V            source/mpi/interfaces/spmd_i18kine_com_v.F
Chd|        SPMD_I18KINE_PENE_COM_POFF    source/mpi/interfaces/spmd_i18kine_pene_com_poff.F
Chd|        H3D_MOD                       share/modules/h3d_mod.F       
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|        TRI7BOX                       share/modules/tri7box.F       
Chd|====================================================================
      SUBROUTINE I18MAIN_KINE_2(IPARI,INTBUF_TAB ,X   ,V       ,
     2                   A    ,ISKEW ,SKEW    ,LCOD   ,WA      ,
     3                   MS   ,ITAB  ,FSAV    ,JTASK  ,KINET   ,
     4                   STIFN,MTF   ,CAND_SAV,FCONT  ,INT18ADD,
     5                   IAD_ELEM,FR_ELEM,H3D_DATA)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE TRI7BOX
      USE INTBUFDEF_MOD
      USE H3D_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "com08_c.inc"
#include      "param_c.inc"
#include      "secur_c.inc"
#include      "task_c.inc"
#include      "units_c.inc"
#include      "warn_c.inc"
#include      "scr05_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IPARI(NPARI,*), ISKEW(*), LCOD(*), ITAB(*),
     .   KINET(*),INT18ADD(*),JTASK,IAD_ELEM(2,*),FR_ELEM(*)
C     REAL ou REAL*8
      my_real
     .   X(*), V(*), A(3,*), SKEW(*), WA(*), MS(*),
     .   FSAV(NTHVKI,*),MTF(14,*),CAND_SAV(*),STIFN(*),
     .   FCONT(3,*)
      TYPE(INTBUF_STRUCT_) INTBUF_TAB(*)
      TYPE(H3D_DATABASE) :: H3D_DATA
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,NODF,NODL,NRTMDIM
      INTEGER N, NTY, NMN, NINT,INACTI,LINDMAX
      INTEGER NB_JLT(PARASIZ),NB_JLT_NEW(PARASIZ),NB_STOK_N(PARASIZ),
     *        NN,P,NODFI,IERROR1,IERROR2,IERROR3,IERROR4,IBID
      my_real
     .    STARTT,STOPT
      SAVE NB_JLT,NB_JLT_NEW,NB_STOK_N
      INTEGER, DIMENSION(:), ALLOCATABLE ::  SLVNDTAG
C=================================================================
C     initialisation MTF(1:14,1:NUMNOD)
C=================================================================
C Pbme en hybride SLVNDTAG commun    un proc SPMD
C
      IF (NSPMD > 1)THEN
         ALLOCATE(SLVNDTAG(NUMNOD))
         SLVNDTAG = 0
      ELSE
         ALLOCATE(SLVNDTAG(1))
         SLVNDTAG = 0
      ENDIF
C=================================================================
C     calcul des vitesses a imposer aux second
c     ponderation en pene/somme(pene)
C=================================================================
C -------------------
      CALL MY_BARRIER
C -------------------
c
      NODF = 1 + (JTASK-1)*NUMNOD / NTHREAD
      NODL = JTASK*NUMNOD / NTHREAD
      DO I = NODF,NODL
        MTF(1,I) = ZERO   ! vx second. impose par main
        MTF(2,I) = ZERO   ! vy second. impose par main
        MTF(3,I) = ZERO   ! vz second. impose par main
        MTF(4,I) = ZERO   ! vxp second. old (v+a*dt)
        MTF(5,I) = ZERO   ! vyp second. old (v+a*dt)
        MTF(6,I) = ZERO   ! vzp second. old (v+a*dt)
      ENDDO

!$OMP SINGLE
      IF (NSPMD > 1)THEN
        DO N=1,NINTER
          NTY=IPARI(7,N)
          INACTI =IPARI(22,N)
C
          IF (NTY==7.AND.INACTI==7.AND.IPARI(34,N)==-2)THEN
          NODFI=0
          DO P = 1, NSPMD
            NODFI = NODFI + NSNFI(N)%P(P)
          END DO

          IF(NODFI > 0)THEN
            MTFI_V(N)%P(1,1:NODFI)=ZERO
            MTFI_V(N)%P(2,1:NODFI)=ZERO
            MTFI_V(N)%P(3,1:NODFI)=ZERO
            MTFI_V(N)%P(4,1:NODFI)=ZERO
            MTFI_V(N)%P(5,1:NODFI)=ZERO
            MTFI_V(N)%P(6,1:NODFI)=ZERO
          ENDIF

         ENDIF
        ENDDO

      ENDIF

      IF (NSPMD > 1)
     *    CALL  SPMD_I18KINE_COM_A(IPARI,INTBUF_TAB,
     *                A,ITAB)
!$OMP END SINGLE
C -------------------
      CALL MY_BARRIER
C -------------------
      DO N=1,NINTER
       NTY   =IPARI(7,N)
       INACTI =IPARI(22,N)
       IF(NTY==7.and.IPARI(34,N)==-2.and.INACTI==7)THEN
         CALL I18MAIN_KINE_V(N,
     1    IPARI(1,N)   ,INTBUF_TAB(N)  ,X       ,STIFN   ,
     2    V            ,A          ,MS          ,JTASK      ,ITAB    ,
     3    CAND_SAV(INT18ADD(N)),MTF,ISKEW       ,SKEW       ,LCOD    ,
     4    NB_JLT(JTASK),NB_JLT_NEW(JTASK),NB_STOK_N(JTASK))
       ENDIF
      ENDDO
C=================================================================

C     a faire:
C     COMM SPMD : MTF(1:6,*)= vitesses des seconds

c   1: envoie sur le proc qui possede le noeud
c   2: CUMUL sur les noeuds frontieres secnd 18
!$OMP SINGLE
      IF (NSPMD > 1)THEN
        CALL SPMD_I18KINE_PENE_COM_POFF(IPARI,INTBUF_TAB,FCONT,
     *                      MTF,A,IAD_ELEM,FR_ELEM,2,SLVNDTAG,IBID,ITAB,
     .                      H3D_DATA )
C Besoin en retour de reinitialiser les MTF_V sur les proc
C qui n ont pas le noeud secnd et qui l'ont en candidat
        CALL SPMD_I18KINE_COM_V(IPARI,INTBUF_TAB,MTF,A,ITAB)
      ENDIF
!$OMP END SINGLE

C=================================================================
C=================================================================
C     on impose les vitesses mains aux seconds
C     (en fait on impose l'acceleration correspondante)
C=================================================================
C -------------------
      CALL MY_BARRIER
C -------------------
      DO N=1,NINTER
       NTY   =IPARI(7,N)
       INACTI =IPARI(22,N)
       IF(NTY==7.and.IPARI(34,N)==-2.and.INACTI==7)THEN
         CALL I18MAIN_KINE_S(N,
     1    IPARI(1,N) ,INTBUF_TAB(N)  ,X        ,STIFN     ,
     2    V          ,A          ,MS           ,FSAV(1,N)  ,FCONT     ,
     3    JTASK      ,ITAB       ,CAND_SAV(INT18ADD(N)),MTF,
     4    NB_JLT(JTASK),NB_JLT_NEW(JTASK),NB_STOK_N(JTASK) ,ISKEW,SKEW,
     5    LCOD       ,SLVNDTAG   ,H3D_DATA )
       ENDIF
      ENDDO
C=================================================================
!$OMP SINGLE
C     COMM SPMD : A(1:3,*)= acceleration des seconds

c   1: envoie sur le proc qui possede le noeud qui ecrase l'acceleration
      IF (NSPMD > 1)THEN
        CALL SPMD_I18KINE_PENE_COM_POFF(IPARI,INTBUF_TAB,FCONT,
     *                      MTF,A,IAD_ELEM,FR_ELEM,3,SLVNDTAG,IBID,ITAB,
     .                      H3D_DATA)
      ENDIF

C=================================================================

      DEALLOCATE(SLVNDTAG)
!$OMP END SINGLE

      RETURN
C
      END
Chd|====================================================================
Chd|  I18MAIN_KINE_I                source/interfaces/int18/i18main_kine.F
Chd|-- called by -----------
Chd|        I18MAIN_KINE_1                source/interfaces/int18/i18main_kine.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        ARRET                         source/system/arret.F         
Chd|        I18DST3                       source/interfaces/int18/i18dst3.F
Chd|        I18KINE_I                     source/interfaces/int18/i18main_kine.F
Chd|        I7CDCOR3                      source/interfaces/int07/i7cdcor3.F
Chd|        I7COR3                        source/interfaces/int07/i7cor3.F
Chd|        MY_BARRIER                    source/system/machine.F       
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE I18MAIN_KINE_I(NIN,IPARI,INTBUF_TAB      ,X        ,
     2      STIFN    ,V         ,A          ,MS           ,NMN      ,
     3      ITAB     ,LINDMAX   ,CAND_SAV   ,MTF          ,
     4      NRTMDIM  ,JTASK     ,NB_JLT     ,NB_JLT_NEW   ,NB_STOK_N,
     5      KINET    )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
      USE INTBUFDEF_MOD
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C
C-------------------------------------------------------------------------------
C     NOM    DIMENSION               DESCRIPTION                       E/S
C-------------------------------------------------------------------------------
C
C    NIN    1                        NUMERO INTERFACE                   E
C
C   IPARI   NPARI,NINTER             PARAMETRES D'INTERFACE             E
C
C     X     3,NUMNOD                 COORDONNEES                        E
C
C     V     3,NUMNOD                 VITESSES                           E
C
C    EMINX  6*NME<6*NUMELS           MIN MAX DE CHAQUE ELEMENT        TMP_GLOBAL
C
C
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
#include      "comlock.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "com08_c.inc"
#include      "param_c.inc"
#include      "units_c.inc"
#include      "task_c.inc"
#include      "warn_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NIN,JTASK ,LINDMAX,NMN     ,
     .        NB_JLT,NB_JLT_NEW,NB_STOK_N,NRTMDIM
      INTEGER IPARI(NPARI), KINET(*),STIFN(*),
     .        ITAB(*)
C     REAL ou REAL*8
      my_real
     .   X(3,*), V(3,*), A(3,*), MS(*),
     .   MTF(14,*),CAND_SAV(8,*)
      TYPE(INTBUF_STRUCT_) INTBUF_TAB
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,I_STOK_GLOB,NSN,NME,NAD,EAD,
     .        NME_T,ESH_T,IGN,IGE,MULTIMP,NOINT,MX_CAND,NTY,IVIS2,
     .        IGAP,INACTI,IBAG,I_STOK, I_STOK_LOC, JLT_NEW,NMNF,NMNL,
     .        JLT, NFT,DEBUT,NBID,NB_LOC, I3N,IGSTI,ICURV,IADM
      INTEGER IX1(MVSIZ), IX2(MVSIZ), IX3(MVSIZ), IX4(MVSIZ),
     .        NSVG(MVSIZ), CN_LOC(MVSIZ),CE_LOC(MVSIZ),INDEX2(LINDMAX),
     .        CAND_N_N(MVSIZ),CAND_E_N(MVSIZ),KINI(MVSIZ),IBID
C     REAL ou REAL*8
      my_real
     .   STARTT, STOPT,GAP,GAPMIN,MAXBOX,MINBOX,BID,
     .   KMIN, KMAX, GAPMAX,SURF(3,NRTMDIM)
      my_real
     .     NX1(MVSIZ), NX2(MVSIZ), NX3(MVSIZ), NX4(MVSIZ),
     .     NY1(MVSIZ), NY2(MVSIZ), NY3(MVSIZ), NY4(MVSIZ),
     .     NZ1(MVSIZ), NZ2(MVSIZ), NZ3(MVSIZ), NZ4(MVSIZ),
     .     LB1(MVSIZ), LB2(MVSIZ), LB3(MVSIZ), LB4(MVSIZ),
     .     LC1(MVSIZ), LC2(MVSIZ), LC3(MVSIZ), LC4(MVSIZ),
     .     P1(MVSIZ), P2(MVSIZ), P3(MVSIZ), P4(MVSIZ),
     .   X1(MVSIZ), X2(MVSIZ), X3(MVSIZ), X4(MVSIZ),
     .   Y1(MVSIZ), Y2(MVSIZ), Y3(MVSIZ), Y4(MVSIZ),
     .   Z1(MVSIZ), Z2(MVSIZ), Z3(MVSIZ), Z4(MVSIZ),
     .   XI(MVSIZ), YI(MVSIZ), ZI(MVSIZ), STIF(MVSIZ),
     .   N1(MVSIZ), N2(MVSIZ), N3(MVSIZ), PENE(MVSIZ),
     .   H1(MVSIZ), H2(MVSIZ), H3(MVSIZ), H4(MVSIZ),
     .   GAPV(MVSIZ),VXI(MVSIZ),VYI(MVSIZ),VZI(MVSIZ),MSI(MVSIZ)
      my_real
     .    NNX1(MVSIZ), NNX2(MVSIZ), NNX3(MVSIZ), NNX4(MVSIZ),
     .    NNY1(MVSIZ), NNY2(MVSIZ), NNY3(MVSIZ), NNY4(MVSIZ),
     .    NNZ1(MVSIZ), NNZ2(MVSIZ), NNZ3(MVSIZ), NNZ4(MVSIZ)
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------
      INACTI=IPARI(22)
      IF(INACTI/=7.or.IPARI(34)==0)RETURN
C
C -------------------
      CALL MY_BARRIER
C -------------------
c      NMNF = 1 + (JTASK-1)*NMN / NTHREAD
c      NMNL = JTASK*NMN / NTHREAD
c#include "vectorize.inc"
c      DO J=NMNF,NMNL
c        I=MSR(J)
c        IF(I.GT.0) THEN
c#include "lockon.inc"
c          MTF(1,I) = MS(I)
c          MTF(4,I) = MS(I)
c          MTF(6,I) = MS(I)
c          MTF(7,I) = A(1,I)*MS(I)
c          MTF(8,I) = A(2,I)*MS(I)
c          MTF(9,I) = A(3,I)*MS(I)
c#include "lockoff.inc"
c        ENDIF
c      ENDDO
cC -------------------
c      CALL MY_BARRIER
cC -------------------

C=================================================================
C     calcul des pene,normale,Hi...
C     et calcul de la pene cumulee pour chaque second. MTF(10,i)
C=================================================================
      NBID=0
      BID=ZERO
      IBID = 0
C
      NSN   =IPARI(5)
      NTY   =IPARI(7)
      IVIS2 =IPARI(14)
      NOINT =IPARI(15)
      IGAP  =IPARI(21)
      INACTI=IPARI(22)
      IBAG =IPARI(32)
      IGSTI=IPARI(34)
      ICURV =0
      IADM =IPARI(44)
      STARTT=INTBUF_TAB%VARIABLES(3)
      STOPT =INTBUF_TAB%VARIABLES(11)
      IF(STARTT.GT.TT) RETURN
      IF(TT.GT.STOPT)  RETURN
      GAP   =INTBUF_TAB%VARIABLES(2)
      GAPMIN=INTBUF_TAB%VARIABLES(13)
C
      I_STOK = INTBUF_TAB%I_STOK(1)
      MAXBOX = INTBUF_TAB%VARIABLES(9)
      MINBOX = INTBUF_TAB%VARIABLES(12)
      GAPMAX=INTBUF_TAB%VARIABLES(16)
      KMIN  =INTBUF_TAB%VARIABLES(17)
      KMAX  =INTBUF_TAB%VARIABLES(18)

C     cette partie est effectuee en // apres le calcul des forces des elem.
C     decoupage statique
      NB_LOC = I_STOK / NTHREAD
      IF (JTASK.EQ.NTHREAD) THEN
          I_STOK_LOC = I_STOK-NB_LOC*(NTHREAD-1)
      ELSE
          I_STOK_LOC = NB_LOC
      ENDIF
      DEBUT = (JTASK-1)*NB_LOC
      I_STOK = 0
C recalcul du istok
      DO I = DEBUT+1, DEBUT+I_STOK_LOC
            IF(INTBUF_TAB%CAND_N(I).LT.0) THEN
              IF(I_STOK + 1.GT.4*NUMNOD) THEN
                CALL ANCMSG(MSGID=94,ANMODE=ANINFO)
                CALL ARRET(2)
              ENDIF
              I_STOK = I_STOK + 1
              INDEX2(I_STOK) = I
C inbuf == cand_n
              INTBUF_TAB%CAND_N(I) = -INTBUF_TAB%CAND_N(I)
            ENDIF
c remise a zero de la pene
            CAND_SAV(8,I) = ZERO
      ENDDO
C
      IF (DEBUG(3).GE.1) THEN
          NB_JLT = NB_JLT + I_STOK_LOC
          NB_STOK_N = NB_STOK_N + I_STOK
      ENDIF
C
      DO NFT = 0 , I_STOK - 1 , NVSIZ
          JLT = MIN( NVSIZ, I_STOK - NFT )
C preparation candidats retenus
          CALL I7CDCOR3(
     1         JLT,INDEX2(NFT+1),INTBUF_TAB%CAND_E,INTBUF_TAB%CAND_N,
     2         CAND_E_N,CAND_N_N)
C cand_n et cand_e remplace par cand_n_n et cand_e_n
          CALL I7COR3(
     1         JLT       ,X   ,INTBUF_TAB%IRECTM,INTBUF_TAB%NSV,CAND_E_N,
     2         CAND_N_N  ,INTBUF_TAB%STFM   ,INTBUF_TAB%STFNS  ,X1    ,X2   ,
     3         X3        ,X4                ,Y1                ,Y2    ,Y3   ,
     4         Y4        ,Z1                ,Z2                ,Z3    ,Z4   ,
     5         XI        ,YI                ,ZI                ,STIF  ,IX1  ,
     6         IX2       ,IX3               ,IX4               ,NSVG  ,IGAP ,
     7         GAP       ,INTBUF_TAB%GAP_S  ,INTBUF_TAB%GAP_M  ,GAPV  ,
     9         MS        ,VXI       ,VYI    ,
     A         VZI       ,MSI       ,NSN    ,V                 ,KINET ,
     B         KINI      ,NTY       ,NIN    ,IGSTI             ,KMIN  ,
     C         KMAX      ,GAPMAX    ,GAPMIN ,IADM              ,BID   ,
     D         BID       ,BID       ,BID    ,IBID              ,BID   ,
     E         BID       ,BID       ,BID    ,IBID              ,BID   ,
     F         IBID      ,IBID      ,IBID   ,BID               ,BID   ,
     G         IBID      ,IBID      ,IBID   ,IBID              ,IBID  ,
     H         IBID      ,IBID      ,BID    ,IBID              ,BID   )
   
          JLT_NEW = 0
          CALL I18DST3(
     1        JLT          ,CAND_N_N ,CAND_E_N ,CN_LOC           ,CE_LOC ,
     2        X1           ,X2       ,X3       ,X4               ,Y1     ,
     3        Y2           ,Y3       ,Y4       ,Z1               ,Z2     ,
     4        Z3           ,Z4       ,XI       ,YI               ,ZI     ,
     5        NX1          ,NX2      ,NX3      ,NX4              ,NY1    ,
     6        NY2          ,NY3      ,NY4      ,NZ1              ,NZ2    ,
     7        NZ3          ,NZ4      ,LB1      ,LB2              ,LB3    ,
     8        LB4          ,LC1      ,LC2      ,LC3              ,LC4    ,
     9        P1           ,P2       ,P3       ,P4               ,IX1    ,
     A        IX2          ,IX3      ,IX4      ,NSVG             ,STIF   ,
     B        JLT_NEW      ,GAPV     ,INACTI   ,INTBUF_TAB%CAND_P,
     C        INDEX2(NFT+1),VXI      ,VYI      ,ITAB,
     D        VZI   ,MSI   ,KINI     ,SURF     ,IBAG)
          JLT = JLT_NEW
          IF(JLT_NEW.NE.0) THEN
            IPARI(29) = 1
            IF (DEBUG(3).GE.1)
     .        NB_JLT_NEW = NB_JLT_NEW + JLT_NEW
            CALL I18KINE_I(
     1  JLT          ,A         ,V            ,
     2  GAP          ,MS        ,NOINT        ,INTBUF_TAB%STFNS,ITAB ,
     3  STIFN        ,STIF      ,X            ,INTBUF_TAB%IRECTM     ,
     4  NX1          ,NX2       ,NX3          ,NX4         ,NY1      ,
     5  NY2          ,NY3       ,NY4          ,NZ1         ,NZ2      ,
     6  NZ3          ,NZ4       ,LB1          ,LB2         ,LB3      ,
     7  LB4          ,LC1       ,LC2          ,LC3         ,LC4      ,
     8  P1           ,P2        ,P3           ,P4          ,NIN      ,
     9  IX1          ,IX2       ,IX3          ,IX4         ,NSVG     ,
     A  GAPV         ,INACTI    ,VXI          ,VYI         ,VZI      ,
     B  MSI          ,MTF       ,INDEX2(NFT+1),CAND_SAV)
          ENDIF
      ENDDO

      RETURN
      END
Chd|====================================================================
Chd|  I18MAIN_KINE_F                source/interfaces/int18/i18main_kine.F
Chd|-- called by -----------
Chd|        I18MAIN_KINE_1                source/interfaces/int18/i18main_kine.F
Chd|-- calls ---------------
Chd|        I18KINE_F                     source/interfaces/int18/i18main_kine.F
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|====================================================================
      SUBROUTINE I18MAIN_KINE_F(NIN,
     1      IPARI     ,INTBUF_TAB          ,X         ,STIFN     ,
     2      V         ,A        ,MS        ,ITAB      ,LINDMAX   ,
     3      CAND_SAV  ,MTF      ,JTASK     ,NB_JLT    ,NB_JLT_NEW,
     4      NB_STOK_N )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE INTBUFDEF_MOD
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C
C-------------------------------------------------------------------------------
C     NOM    DIMENSION               DESCRIPTION                       E/S
C-------------------------------------------------------------------------------
C
C    NIN    1                        NUMERO INTERFACE                   E
C
C   IPARI   NPARI,NINTER             PARAMETRES D'INTERFACE             E
C
C     X     3,NUMNOD                 COORDONNEES                        E
C
C     V     3,NUMNOD                 VITESSES                           E
C
C    EMINX  6*NME<6*NUMELS           MIN MAX DE CHAQUE ELEMENT        TMP_GLOBAL
C
C
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "com08_c.inc"
#include      "param_c.inc"
#include      "units_c.inc"
#include      "task_c.inc"
#include      "warn_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NIN,JTASK ,LINDMAX,
     .        NB_JLT,NB_JLT_NEW,NB_STOK_N
      INTEGER IPARI(NPARI), STIFN(*),
     .        ITAB(*)
C     REAL ou REAL*8
      my_real
     .   X(3,*), V(3,*), A(3,*), MS(*),
     .   MTF(14,*),CAND_SAV(8,*)
      TYPE(INTBUF_STRUCT_) INTBUF_TAB
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I_STOK_GLOB,NSN,NME,NAD,EAD,
     .        NME_T,ESH_T,IGN,IGE,MULTIMP,NOINT,I,MX_CAND,NTY,IVIS2,
     .        IGAP,INACTI,IBAG,I_STOK, I_STOK_LOC, JLT_NEW,
     .        JLT, NFT,DEBUT,NB_LOC, I3N,IGSTI,ICURV,IADM
C     REAL ou REAL*8
      my_real
     .   STARTT, STOPT,GAP,GAPMIN,MAXBOX,MINBOX,
     .   KMIN, KMAX, GAPMAX
      my_real
     .     NX1(MVSIZ), NX2(MVSIZ), NX3(MVSIZ), NX4(MVSIZ),
     .     NY1(MVSIZ), NY2(MVSIZ), NY3(MVSIZ), NY4(MVSIZ),
     .     NZ1(MVSIZ), NZ2(MVSIZ), NZ3(MVSIZ), NZ4(MVSIZ),
     .     LB1(MVSIZ), LB2(MVSIZ), LB3(MVSIZ), LB4(MVSIZ),
     .     LC1(MVSIZ), LC2(MVSIZ), LC3(MVSIZ), LC4(MVSIZ),
     .     P1(MVSIZ), P2(MVSIZ), P3(MVSIZ), P4(MVSIZ),
     .   X1(MVSIZ), X2(MVSIZ), X3(MVSIZ), X4(MVSIZ),
     .   Y1(MVSIZ), Y2(MVSIZ), Y3(MVSIZ), Y4(MVSIZ),
     .   Z1(MVSIZ), Z2(MVSIZ), Z3(MVSIZ), Z4(MVSIZ),
     .   XI(MVSIZ), YI(MVSIZ), ZI(MVSIZ), STIF(MVSIZ),
     .   N1(MVSIZ), N2(MVSIZ), N3(MVSIZ), PENE(MVSIZ),
     .   H1(MVSIZ), H2(MVSIZ), H3(MVSIZ), H4(MVSIZ),
     .   GAPV(MVSIZ),VXI(MVSIZ),VYI(MVSIZ),VZI(MVSIZ),MSI(MVSIZ)
      my_real
     .    NNX1(MVSIZ), NNX2(MVSIZ), NNX3(MVSIZ), NNX4(MVSIZ),
     .    NNY1(MVSIZ), NNY2(MVSIZ), NNY3(MVSIZ), NNY4(MVSIZ),
     .    NNZ1(MVSIZ), NNZ2(MVSIZ), NNZ3(MVSIZ), NNZ4(MVSIZ)
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------
      INACTI=IPARI(22)
      IF(INACTI/=7.or.IPARI(34)==0)RETURN
C
C=================================================================
C     calcul des pene,normale,Hi...
C     et calcul de la pene cumulee pour chaque second. MTF(10,i)
C=================================================================
      NSN   =IPARI(5)
      NTY   =IPARI(7)
      IVIS2 =IPARI(14)
      NOINT =IPARI(15)
      IGAP  =IPARI(21)
      INACTI=IPARI(22)
      IBAG =IPARI(32)
      IGSTI=IPARI(34)
      ICURV =0
      IADM =IPARI(44)
      STARTT=INTBUF_TAB%VARIABLES(3)
      STOPT =INTBUF_TAB%VARIABLES(11)
      IF(STARTT.GT.TT) RETURN
      IF(TT.GT.STOPT)  RETURN
      GAP   =INTBUF_TAB%VARIABLES(2)
      GAPMIN=INTBUF_TAB%VARIABLES(13)
C
      I_STOK = INTBUF_TAB%I_STOK(1)
      MAXBOX = INTBUF_TAB%VARIABLES(9)
      MINBOX = INTBUF_TAB%VARIABLES(12)
      GAPMAX=INTBUF_TAB%VARIABLES(16)
      KMIN  =INTBUF_TAB%VARIABLES(17)
      KMAX  =INTBUF_TAB%VARIABLES(18)
C     cette partie est effectuee en // apres le calcul des forces des elem.
C     decoupage statique
      NB_LOC = I_STOK / NTHREAD
      IF (JTASK.EQ.NTHREAD) THEN
          I_STOK_LOC = I_STOK-NB_LOC*(NTHREAD-1)
      ELSE
          I_STOK_LOC = NB_LOC
      ENDIF
      DEBUT = NB_LOC*(JTASK-1)
C=================================================================
C     calcul des forces et des masses a transmettre aux mains
c     ponderation en pene/somme(pene)
C=================================================================
c      DO NFT = DEBUT , DEBUT + I_STOK_LOC - 1 , NVSIZ
c          JLT = MIN( NVSIZ, I_STOK_LOC - NFT )
      if(JTASK/=1)return
      DO NFT = 0 , I_STOK - 1 , NVSIZ
          JLT = MIN( NVSIZ, I_STOK - NFT )
          CALL I18KINE_F(
     1  JLT          ,A         ,V       ,INTBUF_TAB%CAND_E(1+NFT) ,INTBUF_TAB%CAND_N(1+NFT) ,
     2  GAP          ,MS        ,NOINT   ,INTBUF_TAB%STFNS,ITAB         ,
     3  STIFN        ,STIF      ,X       ,INTBUF_TAB%IRECTM,INTBUF_TAB%NSV,
     4  NX1          ,NX2       ,NX3     ,NX4          ,NY1          ,
     5  NY2          ,NY3       ,NY4     ,NZ1          ,NZ2          ,
     6  NZ3          ,NZ4       ,LB1     ,LB2          ,LB3          ,
     7  LB4          ,LC1       ,LC2     ,LC3          ,LC4          ,
     8  P1           ,P2        ,P3      ,P4           ,NIN          ,
     9  GAPV         ,INACTI    ,VXI     ,VYI          ,VZI          ,
     A  MSI          ,MTF       ,CAND_SAV(1,1+NFT) ,NSN)
      ENDDO
C
      RETURN
      END
Chd|====================================================================
Chd|  I18_KINE_M                    source/interfaces/int18/i18main_kine.F
Chd|-- called by -----------
Chd|        I18MAIN_KINE_1                source/interfaces/int18/i18main_kine.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE I18_KINE_M(
     1      ITASK     ,NMN     ,MSR     ,V        ,A       ,MS      ,
     2      MTF       ,ISKEW  ,SKEW     ,LCOD     ,ITAB    )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "units_c.inc"
#include      "warn_c.inc"
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "com08_c.inc"
#include      "scr05_c.inc"
#include      "task_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NMN,ITASK,MSR(*), ISKEW(*), LCOD(*) , ITAB(*)
      my_real
     .   A(3,*), V(3,*), MS(*),MTF(14,*), SKEW(LSKEW,*)
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER NMNF,NMNL,I, J,ISK
      my_real
     .    A11,A12,A13,A22,A23,A33,B11,B12,B13,B22,B23,B33,
     .    USDET,FX   ,FY   ,FZ
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
      NMNF = 1 + ITASK*NMN / NTHREAD
      NMNL = (ITASK+1)*NMN / NTHREAD

#include "vectorize.inc"
      DO I=NMNF,NMNL
        J=MSR(I)
        IF(J.GT.0) THEN

c     invertion matrice 3x3 sym et multiplication par un vecteur
c
c     B = A^-1
c
c     a = B f
c
c     optimisation : 27* 1/

          A11 = MTF(1,J) + MS(J)
          A12 = MTF(2,J)
          A13 = MTF(3,J)
          A22 = MTF(4,J) + MS(J)
          A23 = MTF(5,J)
          A33 = MTF(6,J) + MS(J)
          FX  = MTF(7,J) + A(1,J)
          FY  = MTF(8,J) + A(2,J)
          FZ  = MTF(9,J) + A(3,J)

          B11 = (A22*A33 - A23*A23)
          B22 = (A33*A11 - A13*A13)
          B33 = (A11*A22 - A12*A12)

          B12 = (A23*A13 - A33*A12)
          B23 = (A13*A12 - A11*A23)
          B13 = (A12*A23 - A22*A13)

          USDET = MS(J) / ( A11*B11 + A12*B12 + A13*B13)

c         a = [B] f
          A(1,J) = (B11*FX + B12*FY + B13*FZ)*USDET
          A(2,J) = (B12*FX + B22*FY + B23*FZ)*USDET
          A(3,J) = (B13*FX + B23*FY + B33*FZ)*USDET

        ENDIF
      ENDDO


      RETURN
      END
Chd|====================================================================
Chd|  I18MAIN_KINE_V                source/interfaces/int18/i18main_kine.F
Chd|-- called by -----------
Chd|        I18MAIN_KINE_2                source/interfaces/int18/i18main_kine.F
Chd|-- calls ---------------
Chd|        I18KINE_V                     source/interfaces/int18/i18main_kine.F
Chd|        MY_BARRIER                    source/system/machine.F       
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|====================================================================
      SUBROUTINE I18MAIN_KINE_V(NIN,
     1      IPARI     ,INTBUF_TAB         ,X       ,STIFN     ,
     2      V         ,A         ,MS      ,JTASK   ,ITAB      ,
     3      CAND_SAV  ,MTF       ,ISKEW   ,SKEW    ,LCOD      ,
     4      NB_JLT    ,NB_JLT_NEW,NB_STOK_N)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE INTBUFDEF_MOD
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C
C-------------------------------------------------------------------------------
C     NOM    DIMENSION               DESCRIPTION                       E/S
C-------------------------------------------------------------------------------
C
C    NIN    1                        NUMERO INTERFACE                   E
C
C   IPARI   NPARI,NINTER             PARAMETRES D'INTERFACE             E
C
C     X     3,NUMNOD                 COORDONNEES                        E
C
C     V     3,NUMNOD                 VITESSES                           E
C
C    EMINX  6*NME<6*NUMELS           MIN MAX DE CHAQUE ELEMENT        TMP_GLOBAL
C
C
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "com08_c.inc"
#include      "param_c.inc"
#include      "units_c.inc"
#include      "task_c.inc"
#include      "warn_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NIN,JTASK ,
     .        NB_JLT,NB_JLT_NEW,NB_STOK_N
      INTEGER IPARI(NPARI), STIFN(*),
     .        ITAB(*), ISKEW(*), LCOD(*)
C     REAL ou REAL*8
      my_real
     .   X(3,*), V(3,*), A(3,*), MS(*),
     .   MTF(14,*),CAND_SAV(8,*), SKEW(*)
      TYPE(INTBUF_STRUCT_) INTBUF_TAB
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I_STOK_GLOB,NSN,NME,NAD,EAD,
     .        NME_T,ESH_T,IGN,IGE,MULTIMP,NOINT,I,MX_CAND,NTY,IVIS2,
     .        IGAP,INACTI,IBAG,I_STOK, I_STOK_LOC, JLT_NEW,DEBUT,
     .        JLT, NFT,NBID,NB_LOC, I3N,IADM
C     REAL ou REAL*8
      my_real
     .   STARTT, STOPT,GAP,GAPMIN,MAXBOX,MINBOX,
     .   KMIN, KMAX, GAPMAX
      my_real
     .     NX1(MVSIZ), NX2(MVSIZ), NX3(MVSIZ), NX4(MVSIZ),
     .     NY1(MVSIZ), NY2(MVSIZ), NY3(MVSIZ), NY4(MVSIZ),
     .     NZ1(MVSIZ), NZ2(MVSIZ), NZ3(MVSIZ), NZ4(MVSIZ),
     .     LB1(MVSIZ), LB2(MVSIZ), LB3(MVSIZ), LB4(MVSIZ),
     .     LC1(MVSIZ), LC2(MVSIZ), LC3(MVSIZ), LC4(MVSIZ),
     .     P1(MVSIZ), P2(MVSIZ), P3(MVSIZ), P4(MVSIZ),
     .   X1(MVSIZ), X2(MVSIZ), X3(MVSIZ), X4(MVSIZ),
     .   Y1(MVSIZ), Y2(MVSIZ), Y3(MVSIZ), Y4(MVSIZ),
     .   Z1(MVSIZ), Z2(MVSIZ), Z3(MVSIZ), Z4(MVSIZ),
     .   XI(MVSIZ), YI(MVSIZ), ZI(MVSIZ), STIF(MVSIZ),
     .   N1(MVSIZ), N2(MVSIZ), N3(MVSIZ), PENE(MVSIZ),
     .   H1(MVSIZ), H2(MVSIZ), H3(MVSIZ), H4(MVSIZ),
     .   GAPV(MVSIZ),VXI(MVSIZ),VYI(MVSIZ),VZI(MVSIZ),MSI(MVSIZ)
      my_real
     .    NNX1(MVSIZ), NNX2(MVSIZ), NNX3(MVSIZ), NNX4(MVSIZ),
     .    NNY1(MVSIZ), NNY2(MVSIZ), NNY3(MVSIZ), NNY4(MVSIZ),
     .    NNZ1(MVSIZ), NNZ2(MVSIZ), NNZ3(MVSIZ), NNZ4(MVSIZ)
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------
C -------------------
      CALL MY_BARRIER
C -------------------
      NSN   =IPARI(5)
      NTY   =IPARI(7)
      IVIS2 =IPARI(14)
      NOINT =IPARI(15)
      IGAP  =IPARI(21)
      INACTI=IPARI(22)
      IBAG  =IPARI(32)
      IADM  =IPARI(44)
      STARTT=INTBUF_TAB%VARIABLES(3)
      STOPT =INTBUF_TAB%VARIABLES(11)
      IF(STARTT.GT.TT) RETURN
      IF(TT.GT.STOPT)  RETURN
      GAP   =INTBUF_TAB%VARIABLES(2)
      GAPMIN=INTBUF_TAB%VARIABLES(13)
C
      I_STOK = INTBUF_TAB%I_STOK(1)
      MAXBOX = INTBUF_TAB%VARIABLES(9)
      MINBOX = INTBUF_TAB%VARIABLES(12)
      GAPMAX=INTBUF_TAB%VARIABLES(16)
      KMIN  =INTBUF_TAB%VARIABLES(17)
      KMAX  =INTBUF_TAB%VARIABLES(18)
C     cette partie est effectuee en // apres le calcul des forces des elem.
C     decoupage statique
      NB_LOC = I_STOK / NTHREAD
      IF (JTASK.EQ.NTHREAD) THEN
          I_STOK_LOC = I_STOK-NB_LOC*(NTHREAD-1)
      ELSE
          I_STOK_LOC = NB_LOC
      ENDIF
      DEBUT = NB_LOC*(JTASK-1)
C=================================================================
C     calcul des vitesses a imposer aux second
c     ponderation en pene/somme(pene)
C=================================================================
c      DO NFT = DEBUT , DEBUT + I_STOK_LOC - 1 , NVSIZ
c          JLT = MIN( NVSIZ, I_STOK_LOC - NFT )
      if(JTASK/=1)return
      DO NFT = 0 , I_STOK - 1 , NVSIZ
          JLT = MIN( NVSIZ, I_STOK - NFT )
          CALL I18KINE_V(
     1  JLT          ,A         ,V       ,INTBUF_TAB%CAND_E(1+NFT) ,INTBUF_TAB%CAND_N(1+NFT) ,
     2  GAP          ,MS        ,NOINT   ,INTBUF_TAB%STFNS,ITAB       ,
     3  STIFN        ,STIF      ,X       ,INTBUF_TAB%IRECTM,INTBUF_TAB%NSV,
     4  NX1          ,NX2       ,NX3     ,NX4          ,NY1          ,
     5  NY2          ,NY3       ,NY4     ,NZ1          ,NZ2          ,
     6  NZ3          ,NZ4       ,LB1     ,LB2          ,LB3          ,
     7  LB4          ,LC1       ,LC2     ,LC3          ,LC4          ,
     8  P1           ,P2        ,P3      ,P4           ,NIN          ,
     9  GAPV         ,INACTI    ,VXI     ,VYI          ,VZI          ,
     A  MSI          ,MTF       ,CAND_SAV(1,1+NFT)     ,NSN)
      ENDDO
C
      RETURN
      END
Chd|====================================================================
Chd|  I18MAIN_KINE_S                source/interfaces/int18/i18main_kine.F
Chd|-- called by -----------
Chd|        I18MAIN_KINE_2                source/interfaces/int18/i18main_kine.F
Chd|-- calls ---------------
Chd|        I18KINE_S                     source/interfaces/int18/i18main_kine.F
Chd|        MY_BARRIER                    source/system/machine.F       
Chd|        H3D_MOD                       share/modules/h3d_mod.F       
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|====================================================================
      SUBROUTINE I18MAIN_KINE_S(NIN,
     1      IPARI     ,INTBUF_TAB         ,X       ,STIFN    ,
     2      V         ,A         ,MS      ,FSAV    ,FCONT    ,
     3      JTASK     ,ITAB      ,CAND_SAV,MTF      ,
     4      NB_JLT    ,NB_JLT_NEW,NB_STOK_N,ISKEW  ,SKEW     ,
     5      LCOD      ,SLVNDTAG, H3D_DATA )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE INTBUFDEF_MOD
      USE H3D_MOD
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C
C-------------------------------------------------------------------------------
C     NOM    DIMENSION               DESCRIPTION                       E/S
C-------------------------------------------------------------------------------
C
C    NIN    1                        NUMERO INTERFACE                   E
C
C   IPARI   NPARI,NINTER             PARAMETRES D'INTERFACE             E
C
C     X     3,NUMNOD                 COORDONNEES                        E
C
C     V     3,NUMNOD                 VITESSES                           E
C
C    EMINX  6*NME<6*NUMELS           MIN MAX DE CHAQUE ELEMENT        TMP_GLOBAL
C
C
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "com08_c.inc"
#include      "param_c.inc"
#include      "units_c.inc"
#include      "task_c.inc"
#include      "warn_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NIN,JTASK ,
     .        NB_JLT,NB_JLT_NEW,NB_STOK_N
      INTEGER IPARI(NPARI), STIFN(*),
     .        ITAB(*), ISKEW(*), LCOD(*) ,SLVNDTAG(*)
C     REAL ou REAL*8
      my_real
     .   X(3,*), V(3,*), A(3,*), MS(*),
     .   MTF(14,*),CAND_SAV(8,*), FSAV(*),FCONT(3,*), SKEW(*)
      TYPE(INTBUF_STRUCT_) INTBUF_TAB
      TYPE(H3D_DATABASE) :: H3D_DATA
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I_STOK_GLOB,NSN,NME,NAD,EAD,
     .        NME_T,ESH_T,IGN,IGE,MULTIMP,NOINT,I,MX_CAND,NTY,IVIS2,
     .        IGAP,INACTI,IBAG,I_STOK, I_STOK_LOC, JLT_NEW,DEBUT,
     .        JLT, NFT,NBID,NB_LOC, I3N,IADM
C     REAL ou REAL*8
      my_real
     .   STARTT, STOPT,GAP,GAPMIN,MAXBOX,MINBOX,BID,
     .   KMIN, KMAX, GAPMAX
      my_real
     .     NX1(MVSIZ), NX2(MVSIZ), NX3(MVSIZ), NX4(MVSIZ),
     .     NY1(MVSIZ), NY2(MVSIZ), NY3(MVSIZ), NY4(MVSIZ),
     .     NZ1(MVSIZ), NZ2(MVSIZ), NZ3(MVSIZ), NZ4(MVSIZ),
     .     LB1(MVSIZ), LB2(MVSIZ), LB3(MVSIZ), LB4(MVSIZ),
     .     LC1(MVSIZ), LC2(MVSIZ), LC3(MVSIZ), LC4(MVSIZ),
     .     P1(MVSIZ), P2(MVSIZ), P3(MVSIZ), P4(MVSIZ),
     .   X1(MVSIZ), X2(MVSIZ), X3(MVSIZ), X4(MVSIZ),
     .   Y1(MVSIZ), Y2(MVSIZ), Y3(MVSIZ), Y4(MVSIZ),
     .   Z1(MVSIZ), Z2(MVSIZ), Z3(MVSIZ), Z4(MVSIZ),
     .   XI(MVSIZ), YI(MVSIZ), ZI(MVSIZ), STIF(MVSIZ),
     .   N1(MVSIZ), N2(MVSIZ), N3(MVSIZ), PENE(MVSIZ),
     .   H1(MVSIZ), H2(MVSIZ), H3(MVSIZ), H4(MVSIZ),
     .   GAPV(MVSIZ),VXI(MVSIZ),VYI(MVSIZ),VZI(MVSIZ),MSI(MVSIZ)
      my_real
     .    NNX1(MVSIZ), NNX2(MVSIZ), NNX3(MVSIZ), NNX4(MVSIZ),
     .    NNY1(MVSIZ), NNY2(MVSIZ), NNY3(MVSIZ), NNY4(MVSIZ),
     .    NNZ1(MVSIZ), NNZ2(MVSIZ), NNZ3(MVSIZ), NNZ4(MVSIZ)
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------
C -------------------
      CALL MY_BARRIER
C -------------------
      NBID=0
      BID=ZERO
C
      NSN   =IPARI(5)
      NTY   =IPARI(7)
      IVIS2 =IPARI(14)
      NOINT =IPARI(15)
      IGAP  =IPARI(21)
      INACTI=IPARI(22)
      IBAG  =IPARI(32)
      IADM  =IPARI(44)
      STARTT=INTBUF_TAB%VARIABLES(3)
      STOPT =INTBUF_TAB%VARIABLES(11)
      IF(STARTT.GT.TT) RETURN
      IF(TT.GT.STOPT)  RETURN
      GAP   =INTBUF_TAB%VARIABLES(2)
      GAPMIN=INTBUF_TAB%VARIABLES(13)
C
      I_STOK = INTBUF_TAB%I_STOK(1)
      MAXBOX = INTBUF_TAB%VARIABLES(9)
      MINBOX = INTBUF_TAB%VARIABLES(12)
      GAPMAX=INTBUF_TAB%VARIABLES(16)
      KMIN  =INTBUF_TAB%VARIABLES(17)
      KMAX  =INTBUF_TAB%VARIABLES(18)
C     cette partie est effectuee en // apres le calcul des forces des elem.
C     decoupage statique
      NB_LOC = I_STOK / NTHREAD
      IF (JTASK.EQ.NTHREAD) THEN
          I_STOK_LOC = I_STOK-NB_LOC*(NTHREAD-1)
      ELSE
          I_STOK_LOC = NB_LOC
      ENDIF
      DEBUT = NB_LOC*(JTASK-1)
C=================================================================
C     on impose les vitesses mains aux seconds
C     (en fait on impose l'acceleration correspondante)
C     et calcul des forces mains et seconds
C=================================================================
c      DO NFT = DEBUT , DEBUT + I_STOK_LOC - 1 , NVSIZ
c          JLT = MIN( NVSIZ, I_STOK_LOC - NFT )
      if(JTASK/=1)return
      DO NFT = 0 , I_STOK - 1 , NVSIZ
          JLT = MIN( NVSIZ, I_STOK - NFT )
          CALL I18KINE_S(
     1  JLT          ,A         ,V       ,INTBUF_TAB%CAND_E(1+NFT) ,INTBUF_TAB%CAND_N(1+NFT) ,
     2  GAP          ,MS        ,NOINT   ,INTBUF_TAB%STFNS,ITAB         ,
     3  STIFN        ,STIF      ,X       ,INTBUF_TAB%IRECTM,INTBUF_TAB%NSV,
     4  NX1          ,NX2       ,NX3          ,NX4         ,NY1      ,
     5  NY2          ,NY3       ,NY4          ,NZ1         ,NZ2      ,
     6  NZ3          ,NZ4       ,LB1          ,LB2         ,LB3      ,
     7  LB4          ,LC1       ,LC2          ,LC3         ,LC4      ,
     8  P1           ,P2        ,P3           ,P4          ,NIN      ,     
     9  GAPV         ,INACTI    ,VXI          ,VYI         ,VZI      ,
     A  MSI          ,MTF   ,CAND_SAV(1,1+NFT),FCONT       ,FSAV     ,
     B  NSN          ,SLVNDTAG  ,H3D_DATA     )
      ENDDO
C
      RETURN
      END
Chd|====================================================================
Chd|  I18KINE_I                     source/interfaces/int18/i18main_kine.F
Chd|-- called by -----------
Chd|        I18MAIN_KINE_I                source/interfaces/int18/i18main_kine.F
Chd|-- calls ---------------
Chd|        TRI7BOX                       share/modules/tri7box.F       
Chd|====================================================================
      SUBROUTINE I18KINE_I(JLT ,A      ,V      ,
     2                  GAP    ,MS     ,NOINT  ,STFN  ,ITAB   ,
     3                  STIFN  ,STIF   ,X      ,IRECT  ,
     4                  NX1    ,NX2    ,NX3    ,NX4   ,NY1    ,
     5                  NY2    ,NY3    ,NY4    ,NZ1   ,NZ2    ,
     6                  NZ3    ,NZ4    ,LB1    ,LB2   ,LB3    ,
     7                  LB4    ,LC1    ,LC2    ,LC3   ,LC4    ,
     8                  P1     ,P2     ,P3     ,P4    ,NIN    ,
     9                  IX1    ,IX2    ,IX3    ,IX4   ,NSVG   ,
     A                  GAPV   ,INACTI ,VXI    ,VYI    ,VZI   ,
     B                  MSI    ,MTF    ,INDEX  ,CAND_SAV)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE TRI7BOX
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
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "com06_c.inc"
#include      "com08_c.inc"
#include      "scr02_c.inc"
#include      "scr03_c.inc"
#include      "scr05_c.inc"
#include      "scr07_c.inc"
#include      "scr11_c.inc"
#include      "scr14_c.inc"
#include      "scr16_c.inc"
#include      "scr18_c.inc"
#include      "units_c.inc"
#include      "parit_c.inc"
#include      "warn_c.inc"
#include      "couple_c.inc"
#include      "param_c.inc"
ctmp+1
#include      "task_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER JLT,IBCM,IBCS,INACTI,NIN,
     .        ITAB(*),INDEX(*),
     .        NOINT,IRECT(4,*),
     .        INTTH,IFORM
      INTEGER IX1(MVSIZ), IX2(MVSIZ), IX3(MVSIZ), IX4(MVSIZ),
     .        NSVG(MVSIZ)
      my_real
     .   X(3,*),A(3,*), MS(*), V(3,*), MTF(14,*),
     .   GAP, STFN(*),STIFN(*),CAND_SAV(8,*)
      my_real
     .     NX1(MVSIZ), NX2(MVSIZ), NX3(MVSIZ), NX4(MVSIZ),
     .     NY1(MVSIZ), NY2(MVSIZ), NY3(MVSIZ), NY4(MVSIZ),
     .     NZ1(MVSIZ), NZ2(MVSIZ), NZ3(MVSIZ), NZ4(MVSIZ),
     .     LB1(MVSIZ), LB2(MVSIZ), LB3(MVSIZ), LB4(MVSIZ),
     .     LC1(MVSIZ), LC2(MVSIZ), LC3(MVSIZ), LC4(MVSIZ),
     .     P1(MVSIZ), P2(MVSIZ), P3(MVSIZ), P4(MVSIZ), STIF(MVSIZ),
     .     GAPV(MVSIZ),
     .     VXI(MVSIZ),VYI(MVSIZ),VZI(MVSIZ),MSI(MVSIZ),
     .     RSTIF
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J1, IG, J, JG , K0,NBINTER,K1S,K,IL,IE, NN, NI,
     .        NA1,NA2
      my_real
     .   FXI(MVSIZ), FYI(MVSIZ), FZI(MVSIZ), FNI(MVSIZ),
     .   FXT(MVSIZ),FYT(MVSIZ),FZT(MVSIZ),
     .   FX1(MVSIZ), FX2(MVSIZ), FX3(MVSIZ), FX4(MVSIZ),
     .   FY1(MVSIZ), FY2(MVSIZ), FY3(MVSIZ), FY4(MVSIZ),
     .   FZ1(MVSIZ), FZ2(MVSIZ), FZ3(MVSIZ), FZ4(MVSIZ),
     .   N1(MVSIZ), N2(MVSIZ), N3(MVSIZ), PENE(MVSIZ),
     .   DTMI(MVSIZ), XMU(MVSIZ),STIF0(MVSIZ),
     .   H1(MVSIZ), H2(MVSIZ), H3(MVSIZ), H4(MVSIZ),
     .   VX(MVSIZ), VY(MVSIZ), VZ(MVSIZ), VN(MVSIZ),DIST(MVSIZ),
     .   VNX, VNY, VNZ, AA, CRIT,S2,RDIST,
     .   V2, FM2, DT1INV, FAC,FF,ALPHI,ALPHA,BETA,
     .   FX, FY, FZ, F2, MAS2, M2SK, DTMI0,DTI,FT,FN,FMAX,FTN,
     .   FACM1, ECONTT, ECONVT, H0, LA1, LA2, LA3, LA4,
     .   D1,D2,D3,D4,A1,A2,A3,A4,
     .   E10, H0D, S2D, SUM,
     .   LA1D,LA2D,LA3D,LA4D,T1,T1D,T2,T2D,FFD,FACD,D1D,
     .   P1S(MVSIZ),P2S(MVSIZ),P3S(MVSIZ),P4S(MVSIZ),
     .   D2D,D3D,D4D,VNXD,VNYD,VNZD,V2D,FM2D,F2D,AAD,FXD,FYD,FZD,
     .   A1D,A2D,A3D,A4D,VV,AX1,AX2,AY1,AY2,AZ1,AZ2,AX,AY,AZ,
     .   AREA,P,VV1,VV2,V21,DMU, DTI2,H00 ,A0X,A0Y,A0Z,RX,RY,RZ,
     .   ANX,ANY,ANZ,AAN,AAX,AAY,AAZ ,RR,RS,AAA ,TM,TS
      my_real
     .   PREC
      my_real
     .   ST1(MVSIZ),ST2(MVSIZ),ST3(MVSIZ),ST4(MVSIZ),STV(MVSIZ),
     .   KT(MVSIZ),C(MVSIZ),CF(MVSIZ),
     .   KS(MVSIZ),K1(MVSIZ),K2(MVSIZ),K3(MVSIZ),K4(MVSIZ),
     .   CS(MVSIZ),C1(MVSIZ),C2(MVSIZ),C3(MVSIZ),C4(MVSIZ),
     .   CX,CY,CFI,AUX,PHI1(MVSIZ), PHI2(MVSIZ), PHI3(MVSIZ),
     .   PHI4(MVSIZ)
      INTEGER JJ,KK,IN,IBID
      my_real
     .   IMPX,IMPY,IMPZ,PP1,PP2,PP3,PP4,BID
C
C-----------------------------------------------
#include "sync_dir.inc"
      IF (IRESP.EQ.1) THEN
           PREC = CINQEM4
      ELSE
           PREC = EM10
      ENDIF
      IF(DT1.GT.ZERO)THEN
        DT1INV = UN/DT1
      ELSE
        DT1INV =ZERO
      ENDIF
      ECONTT = ZERO
      ECONVT = ZERO
C--------------------------------------------------------
C  CAS DES PAQUETS MIXTES
C--------------------------------------------------------
      BID = ZERO
      IBID = 0
C
      DO I=1,JLT
        IF(IX3(I).NE.IX4(I))THEN
C
         D1 = SQRT(P1(I))
         PP1 = MAX(ZERO, GAPV(I) - D1)
C
         D2 = SQRT(P2(I))
         PP2 = MAX(ZERO, GAPV(I) - D2)
C
         D3 = SQRT(P3(I))
         PP3 = MAX(ZERO, GAPV(I) - D3)
C
         D4 = SQRT(P4(I))
         PP4 = MAX(ZERO, GAPV(I) - D4)
C
         PENE(I) = MAX(PP1,PP2,PP3,PP4)
         LA1 = UN - LB1(I) - LC1(I)
         LA2 = UN - LB2(I) - LC2(I)
         LA3 = UN - LB3(I) - LC3(I)
         LA4 = UN - LB4(I) - LC4(I)
         IF(PENE(I) == PP1)THEN
           N1(I) = NX1(I)
           N2(I) = NY1(I)
           N3(I) = NZ1(I)
           H0    = QUART * LA1
           H1(I) = H0 + LB1(I)
           H2(I) = H0 + LC1(I)
           H3(I) = H0
           H4(I) = H0
         ELSEIF(PENE(I) == PP2)THEN
           N1(I) = NX2(I)
           N2(I) = NY2(I)
           N3(I) = NZ2(I)
           H0    = QUART * LA2
           H1(I) = H0
           H2(I) = H0 + LB2(I)
           H3(I) = H0 + LC2(I)
           H4(I) = H0
         ELSEIF(PENE(I) == PP3)THEN
           N1(I) = NX3(I)
           N2(I) = NY3(I)
           N3(I) = NZ3(I)
           H0    = QUART * LA3
           H1(I) = H0
           H2(I) = H0
           H3(I) = H0 + LB3(I)
           H4(I) = H0 + LC3(I)
         ELSEIF(PENE(I) == PP4)THEN
           N1(I) = NX4(I)
           N2(I) = NY4(I)
           N3(I) = NZ4(I)
           H0    = QUART * LA4
           H1(I) = H0 + LC4(I)
           H2(I) = H0
           H3(I) = H0
           H4(I) = H0 + LB4(I)
         ENDIF
c         A1 = PP1/MAX(EM20,D1)
c         A2 = PP2/MAX(EM20,D2)
c         A3 = PP3/MAX(EM20,D3)
c         A4 = PP4/MAX(EM20,D4)
c         N1(I) = A1*NX1(I) + A2*NX2(I) + A3*NX3(I) + A4*NX4(I)
c         N2(I) = A1*NY1(I) + A2*NY2(I) + A3*NY3(I) + A4*NY4(I)
c         N3(I) = A1*NZ1(I) + A2*NZ2(I) + A3*NZ3(I) + A4*NZ4(I)
C
c         LA1 = UN - LB1(I) - LC1(I)
c         LA2 = UN - LB2(I) - LC2(I)
c         LA3 = UN - LB3(I) - LC3(I)
c         LA4 = UN - LB4(I) - LC4(I)
C
c         H0    = QUART *
c     .         (PP1*LA1 + PP2*LA2 + PP3*LA3 + PP4*LA4)
c         H1(I) = H0 + PP1 * LB1(I) + PP4 * LC4(I)
c         H2(I) = H0 + PP2 * LB2(I) + PP1 * LC1(I)
c         H3(I) = H0 + PP3 * LB3(I) + PP2 * LC2(I)
c         H4(I) = H0 + PP4 * LB4(I) + PP3 * LC3(I)
c         H0    = QUART *
c     .         (A1*LA1 + A2*LA2 + A3*LA3 + A4*LA4)
c         H1(I) = H0 + A1 * LB1(I) + A4 * LC4(I)
c         H2(I) = H0 + A2 * LB2(I) + A1 * LC1(I)
c         H3(I) = H0 + A3 * LB3(I) + A2 * LC2(I)
c         H4(I) = H0 + A4 * LB4(I) + A3 * LC3(I)
         H00    = UN/MAX(EM20,H1(I) + H2(I) + H3(I) + H4(I))
         H1(I) = H1(I) * H00
         H2(I) = H2(I) * H00
         H3(I) = H3(I) * H00
         H4(I) = H4(I) * H00
C
        ELSE
C
         D1 = SQRT(P1(I))
         PP1 = MAX(ZERO, GAPV(I) - D1)
         PENE(I) = PP1
         N1(I) = NX1(I)
         N2(I) = NY1(I)
         N3(I) = NZ1(I)
         H1(I) = LB1(I)
         H2(I) = LC1(I)
         H3(I) = UN - LB1(I) - LC1(I)
         H4(I) = ZERO
        ENDIF
      ENDDO
C---------------------
C     NORMALE
C---------------------
      DO I=1,JLT
         S2 = UN/MAX(EM30,SQRT(N1(I)**2 + N2(I)**2 + N3(I)**2))
         N1(I) = N1(I)*S2
         N2(I) = N2(I)*S2
         N3(I) = N3(I)*S2
      ENDDO
C---------------------
C      PENE
C---------------------
c      DO I=1,JLT
cC       correction hourglass
c        H0 = -.25*(H1(I) - H2(I) + H3(I) - H4(I))
c        H0 = MIN(H0,H2(I),H4(I))
c        H0 = MAX(H0,-H1(I),-H3(I))
c        IF(IX3(I).EQ.IX4(I))H0 = ZERO
c        H1(I) = H1(I) + H0
c        H2(I) = H2(I) - H0
c        H3(I) = H3(I) + H0
c        H4(I) = H4(I) - H0
c      ENDDO
C---------------------
      DO I=1,JLT
        IG=NSVG(I)
        IF(IG.GT.0)THEN
#include "lockon.inc"
         IF(PENE(I) > MTF(11,IG))THEN
           MTF(11,IG) = PENE(I)
         ELSEIF(PENE(I) == MTF(11,IG))THEN
           PENE(I) = PENE(I)*(UN-EM6)
         ENDIF
         MTF(10,IG) = MTF(10,IG) + PENE(I)
         MTF(12,IG) = MTF(12,IG) + PENE(I)*N1(I)
         MTF(13,IG) = MTF(13,IG) + PENE(I)*N2(I)
         MTF(14,IG) = MTF(14,IG) + PENE(I)*N3(I)
#include "lockoff.inc"
        ELSE
         NN=-IG
#include "lockon.inc"
         IF(PENE(I) > MTFI_PENEMIN(NIN)%P(NN))THEN
           MTFI_PENEMIN(NIN)%P(NN) = PENE(I)
         ELSEIF(PENE(I) == MTFI_PENEMIN(NIN)%P(NN))THEN
           PENE(I) = PENE(I)*(UN-EM6)
         ENDIF
         MTFI_PENE(NIN)%P(NN) = MTFI_PENE(NIN)%P(NN) +  PENE(I)
         MTFI_N(NIN)%P(1,NN) = MTFI_N(NIN)%P(1,NN) + PENE(I)*N1(I)
         MTFI_N(NIN)%P(2,NN) = MTFI_N(NIN)%P(2,NN) + PENE(I)*N2(I)
         MTFI_N(NIN)%P(3,NN) = MTFI_N(NIN)%P(3,NN) + PENE(I)*N3(I)
#include "lockoff.inc"
        ENDIF
        CAND_SAV(1,INDEX(I)) = H1(I)
        CAND_SAV(2,INDEX(I)) = H2(I)
        CAND_SAV(3,INDEX(I)) = H3(I)
        CAND_SAV(4,INDEX(I)) = H4(I)
        CAND_SAV(5,INDEX(I)) = N1(I)
        CAND_SAV(6,INDEX(I)) = N2(I)
        CAND_SAV(7,INDEX(I)) = N3(I)
        CAND_SAV(8,INDEX(I)) = PENE(I)
      ENDDO
C-----------------------------------------------------
      RETURN
      END
Chd|====================================================================
Chd|  I18KINE_F                     source/interfaces/int18/i18main_kine.F
Chd|-- called by -----------
Chd|        I18MAIN_KINE_F                source/interfaces/int18/i18main_kine.F
Chd|-- calls ---------------
Chd|        TRI7BOX                       share/modules/tri7box.F       
Chd|====================================================================
      SUBROUTINE I18KINE_F(JLT ,A      ,V       ,CAND_E,CAND_N ,
     2                  GAP    ,MS     ,NOINT   ,STFN  ,ITAB   ,
     3                  STIFN  ,STIF   ,X       ,IRECT ,NSV    ,
     4                  NX1    ,NX2    ,NX3     ,NX4   ,NY1    ,
     5                  NY2    ,NY3    ,NY4     ,NZ1   ,NZ2    ,
     6                  NZ3    ,NZ4    ,LB1     ,LB2   ,LB3    ,
     7                  LB4    ,LC1    ,LC2     ,LC3   ,LC4    ,
     8                  P1     ,P2     ,P3      ,P4    ,NIN    ,
     A                  GAPV   ,INACTI ,VXI     ,VYI   ,VZI    ,
     B                  MSI    ,MTF    ,CAND_SAV,NSN)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE TRI7BOX
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
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "com06_c.inc"
#include      "com08_c.inc"
#include      "scr02_c.inc"
#include      "scr03_c.inc"
#include      "scr05_c.inc"
#include      "scr07_c.inc"
#include      "scr11_c.inc"
#include      "scr14_c.inc"
#include      "scr16_c.inc"
#include      "scr18_c.inc"
#include      "units_c.inc"
#include      "parit_c.inc"
#include      "warn_c.inc"
#include      "couple_c.inc"
#include      "param_c.inc"
ctmp+1
#include      "task_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER JLT,IBCM,IBCS,INACTI,NIN,
     .        ITAB(*),CAND_N(*),CAND_E(*), NSV(*),
     .        NOINT,IRECT(4,*),
     .        INTTH,IFORM,NSN
      my_real
     .   X(3,*),A(3,*), MS(*), V(3,*), MTF(14,*),
     .   GAP, STFN(*),STIFN(*),CAND_SAV(8,*)
      my_real
     .     NX1(MVSIZ), NX2(MVSIZ), NX3(MVSIZ), NX4(MVSIZ),
     .     NY1(MVSIZ), NY2(MVSIZ), NY3(MVSIZ), NY4(MVSIZ),
     .     NZ1(MVSIZ), NZ2(MVSIZ), NZ3(MVSIZ), NZ4(MVSIZ),
     .     LB1(MVSIZ), LB2(MVSIZ), LB3(MVSIZ), LB4(MVSIZ),
     .     LC1(MVSIZ), LC2(MVSIZ), LC3(MVSIZ), LC4(MVSIZ),
     .     P1(MVSIZ), P2(MVSIZ), P3(MVSIZ), P4(MVSIZ), STIF(MVSIZ),
     .     GAPV(MVSIZ),
     .     VXI(MVSIZ),VYI(MVSIZ),VZI(MVSIZ),MSI(MVSIZ),
     .     RSTIF
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J1, IG, J, JG , K0,NBINTER,K1S,K,IL,IE, NN, NI,
     .        NA1,NA2,L
      INTEGER IX1(MVSIZ),IX2(MVSIZ),IX3(MVSIZ),IX4(MVSIZ),NSVG(MVSIZ)
      my_real
     .   FXI(MVSIZ), FYI(MVSIZ), FZI(MVSIZ), FNI(MVSIZ),
     .   FXT(MVSIZ),FYT(MVSIZ),FZT(MVSIZ),
     .   FX1(MVSIZ), FX2(MVSIZ), FX3(MVSIZ), FX4(MVSIZ),
     .   FY1(MVSIZ), FY2(MVSIZ), FY3(MVSIZ), FY4(MVSIZ),
     .   FZ1(MVSIZ), FZ2(MVSIZ), FZ3(MVSIZ), FZ4(MVSIZ),
     .   N1(MVSIZ), N2(MVSIZ), N3(MVSIZ), PENE(MVSIZ),
     .   DTMI(MVSIZ), XMU(MVSIZ),STIF0(MVSIZ),
     .   H1(MVSIZ), H2(MVSIZ), H3(MVSIZ), H4(MVSIZ),
     .   VX(MVSIZ), VY(MVSIZ), VZ(MVSIZ), VN(MVSIZ),DIST(MVSIZ),
     .   MT1XX(MVSIZ), MT1XY(MVSIZ), MT1XZ(MVSIZ),
     .   MT1YY(MVSIZ), MT1YZ(MVSIZ), MT1ZZ(MVSIZ),
     .   MT2XX(MVSIZ), MT2XY(MVSIZ), MT2XZ(MVSIZ),
     .   MT2YY(MVSIZ), MT2YZ(MVSIZ), MT2ZZ(MVSIZ),
     .   MT3XX(MVSIZ), MT3XY(MVSIZ), MT3XZ(MVSIZ),
     .   MT3YY(MVSIZ), MT3YZ(MVSIZ), MT3ZZ(MVSIZ),
     .   MT4XX(MVSIZ), MT4XY(MVSIZ), MT4XZ(MVSIZ),
     .   MT4YY(MVSIZ), MT4YZ(MVSIZ), MT4ZZ(MVSIZ),
     .   VNX, VNY, VNZ, AA, CRIT,S2,RDIST,
     .   V2, FM2, DT1INV, FAC,FF,ALPHI,ALPHA,BETA,
     .   FX, FY, FZ, F2, MAS2, M2SK, DTMI0,DTI,FT,FN,FMAX,FTN,
     .   FACM1, ECONTT, ECONVT, H0, LA1, LA2, LA3, LA4,
     .   D1,D2,D3,D4,A1,A2,A3,A4,
     .   E10, H0D, S2D, SUM,
     .   LA1D,LA2D,LA3D,LA4D,T1,T1D,T2,T2D,FFD,FACD,D1D,
     .   P1S(MVSIZ),P2S(MVSIZ),P3S(MVSIZ),P4S(MVSIZ),
     .   D2D,D3D,D4D,VNXD,VNYD,VNZD,V2D,FM2D,F2D,AAD,FXD,FYD,FZD,
     .   A1D,A2D,A3D,A4D,VV,AX1,AX2,AY1,AY2,AZ1,AZ2,AX,AY,AZ,
     .   AREA,P,VV1,VV2,V21,DMU, DTI2,H00 ,A0X,A0Y,A0Z,RX,RY,RZ,
     .   ANX,ANY,ANZ,AAN,AAX,AAY,AAZ ,RR,RS,AAA ,TM,TS
      my_real
     .   PREC
      my_real
     .   ST1(MVSIZ),ST2(MVSIZ),ST3(MVSIZ),ST4(MVSIZ),STV(MVSIZ),
     .   KT(MVSIZ),C(MVSIZ),CF(MVSIZ),
     .   KS(MVSIZ),K1(MVSIZ),K2(MVSIZ),K3(MVSIZ),K4(MVSIZ),
     .   CS(MVSIZ),C1(MVSIZ),C2(MVSIZ),C3(MVSIZ),C4(MVSIZ),
     .   CX,CY,CFI,AUX,PHI1(MVSIZ), PHI2(MVSIZ), PHI3(MVSIZ),
     .   PHI4(MVSIZ)
      INTEGER JJ,KK,IN,IBID
      my_real
     .   IMPX,IMPY,IMPZ,PP1,PP2,PP3,PP4,BID,
     .   MTXX,MTXY,MTXZ,MTYY,MTYZ,MTZZ
C
C-----------------------------------------------
#include "sync_dir.inc"
C---------------------
C
      DO I=1,JLT
        H1(I)   = CAND_SAV(1,I)
        H2(I)   = CAND_SAV(2,I)
        H3(I)   = CAND_SAV(3,I)
        H4(I)   = CAND_SAV(4,I)
        N1(I)   = CAND_SAV(5,I)
        N2(I)   = CAND_SAV(6,I)
        N3(I)   = CAND_SAV(7,I)
        PENE(I) = CAND_SAV(8,I)
        L       = CAND_E(I)
        IX1(I)  = IRECT(1,L)
        IX2(I)  = IRECT(2,L)
        IX3(I)  = IRECT(3,L)
        IX4(I)  = IRECT(4,L)
c       NSVG(I) = NSV(CAND_N(I))
        NI = CAND_N(I)
        IF(NI.LE.NSN)THEN
          IG = NSV(NI)
          NSVG(I) = IG
        ELSE
          NN = NI - NSN
          NSVG(I) = -NN
        ENDIF
      ENDDO
C
      DO I=1,JLT
        MSI(I)  = ZERO
        FXI(I)  = ZERO
        FYI(I)  = ZERO
        FZI(I)  = ZERO
        IF(PENE(I) > ZERO)THEN
          IG=NSVG(I)
          IF(IG.GT.0)THEN
           IF(PENE(I) == MTF(11,IG))THEN
c            N1(I)   = MTF(12,IG)
c            N2(I)   = MTF(13,IG)
c            N3(I)   = MTF(14,IG)

c           ponderation impact multiple
cc            FAC  = PENE(I) / MTF(10,IG)
c           ponderation distance
c quad           FAC = FAC * (UN-MTF(11,IG)*MTF(11,IG))
c line           FAC = FAC * (UN-MTF(11,IG))
c            FAC = FAC * (MTF(11,IG)**4-DEUX*MTF(11,IG)**2+UN)
cc            MSI(I)  = MS(IG) * FAC
c           la normale n'est pas normee!
c            FNI(I)  = N1(I) * A(1,IG) * FAC
c     .              + N2(I) * A(2,IG) * FAC
c     .              + N3(I) * A(3,IG) * FAC
            MSI(I)  = MS(IG)
            FNI(I)  = N1(I) * A(1,IG)
     .              + N2(I) * A(2,IG)
     .              + N3(I) * A(3,IG)

            AAA = UN/MAX(EM30,N1(I)**2 + N2(I)**2 + N3(I)**2)
            MSI(I)  = MSI(I) * AAA
            AAA = FNI(I) * AAA
            FXI(I)  = N1(I) * AAA
            FYI(I)  = N2(I) * AAA
            FZI(I)  = N3(I) * AAA
           ELSE
            PENE(I) = ZERO
           ENDIF
          ELSE
           IF(PENE(I) == MTFI_PENEMIN(NIN)%P(-IG))THEN
            NN=-IG
c            N1(I)   = MTFI_N(NIN)%P(1,NN)
c            N2(I)   = MTFI_N(NIN)%P(2,NN)
c            N3(I)   = MTFI_N(NIN)%P(3,NN)
cc            FAC  = PENE(I) / MTFI_PENE(NIN)%P(NN)
c           ponderation distance
c            FAC = FAC * (MTFI_PENEMIN(NIN)%P(NN)**4-
c     *              DEUX*MTFI_PENEMIN(NIN)%P(NN)**2+UN)
cc            MSI(I)= MSFI(NIN)%P(NN) * FAC
cc            FNI(I)  = N1(I) * I18KAFI(NIN)%P(1,NN) * FAC
cc     .              + N2(I) * I18KAFI(NIN)%P(2,NN) * FAC
cc     .              + N3(I) * I18KAFI(NIN)%P(3,NN) * FAC

            MSI(I)= MSFI(NIN)%P(NN)
            FNI(I)  = N1(I) * I18KAFI(NIN)%P(1,NN)
     .              + N2(I) * I18KAFI(NIN)%P(2,NN)
     .              + N3(I) * I18KAFI(NIN)%P(3,NN)

            AAA = UN/MAX(EM30,N1(I)**2 + N2(I)**2 + N3(I)**2)
            MSI(I)  = MSI(I) * AAA
            AAA = FNI(I) * AAA
            FXI(I)  = N1(I) * AAA
            FYI(I)  = N2(I) * AAA
            FZI(I)  = N3(I) * AAA
           ELSE
            PENE(I) = ZERO
           ENDIF
          ENDIF
          CAND_SAV(8,I) = PENE(I)
        ENDIF
C
      ENDDO
C---------------------------------
C     transfers secnd forces to main nodes
C---------------------------------
      DO I=1,JLT
        IF(PENE(I).GT.ZERO)THEN
          FX1(I)=FXI(I)*H1(I)
          FY1(I)=FYI(I)*H1(I)
          FZ1(I)=FZI(I)*H1(I)
C
          FX2(I)=FXI(I)*H2(I)
          FY2(I)=FYI(I)*H2(I)
          FZ2(I)=FZI(I)*H2(I)
C
          FX3(I)=FXI(I)*H3(I)
          FY3(I)=FYI(I)*H3(I)
          FZ3(I)=FZI(I)*H3(I)
C
          FX4(I)=FXI(I)*H4(I)
          FY4(I)=FYI(I)*H4(I)
          FZ4(I)=FZI(I)*H4(I)
C---------------------------------
c       transfers secnd tensorial mass to main nodes
C---------------------------------
c
c            | nx*nx nx*ny nx*nz |
c       Mt = |       ny*ny ny*nz | ms
c            |             nz*nz |
c
C---------------------------------

          MTXX = MSI(I)*N1(I)*N1(I)
          MTXY = MSI(I)*N1(I)*N2(I)
          MTXZ = MSI(I)*N1(I)*N3(I)
          MTYY = MSI(I)*N2(I)*N2(I)
          MTYZ = MSI(I)*N2(I)*N3(I)
          MTZZ = MSI(I)*N3(I)*N3(I)

          MT1XX(I) = H1(I)*MTXX
          MT1XY(I) = H1(I)*MTXY
          MT1XZ(I) = H1(I)*MTXZ
          MT1YY(I) = H1(I)*MTYY
          MT1YZ(I) = H1(I)*MTYZ
          MT1ZZ(I) = H1(I)*MTZZ

          MT2XX(I) = H2(I)*MTXX
          MT2XY(I) = H2(I)*MTXY
          MT2XZ(I) = H2(I)*MTXZ
          MT2YY(I) = H2(I)*MTYY
          MT2YZ(I) = H2(I)*MTYZ
          MT2ZZ(I) = H2(I)*MTZZ

          MT3XX(I) = H3(I)*MTXX
          MT3XY(I) = H3(I)*MTXY
          MT3XZ(I) = H3(I)*MTXZ
          MT3YY(I) = H3(I)*MTYY
          MT3YZ(I) = H3(I)*MTYZ
          MT3ZZ(I) = H3(I)*MTZZ

          MT4XX(I) = H4(I)*MTXX
          MT4XY(I) = H4(I)*MTXY
          MT4XZ(I) = H4(I)*MTXZ
          MT4YY(I) = H4(I)*MTYY
          MT4YZ(I) = H4(I)*MTYZ
          MT4ZZ(I) = H4(I)*MTZZ
        ENDIF
      ENDDO
c
c  provisoire NON PARITH/ON  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
C
      DO I=1,JLT
        IG=NSVG(I)
        IF(PENE(I).GT.ZERO)THEN
#include "lockon.inc"
          MTF(1,IX1(I)) = MTF(1,IX1(I)) + MT1XX(I)
          MTF(2,IX1(I)) = MTF(2,IX1(I)) + MT1XY(I)
          MTF(3,IX1(I)) = MTF(3,IX1(I)) + MT1XZ(I)
          MTF(4,IX1(I)) = MTF(4,IX1(I)) + MT1YY(I)
          MTF(5,IX1(I)) = MTF(5,IX1(I)) + MT1YZ(I)
          MTF(6,IX1(I)) = MTF(6,IX1(I)) + MT1ZZ(I)
          MTF(7,IX1(I)) = MTF(7,IX1(I)) + FX1(I)
          MTF(8,IX1(I)) = MTF(8,IX1(I)) + FY1(I)
          MTF(9,IX1(I)) = MTF(9,IX1(I)) + FZ1(I)

          MTF(1,IX2(I)) = MTF(1,IX2(I)) + MT2XX(I)
          MTF(2,IX2(I)) = MTF(2,IX2(I)) + MT2XY(I)
          MTF(3,IX2(I)) = MTF(3,IX2(I)) + MT2XZ(I)
          MTF(4,IX2(I)) = MTF(4,IX2(I)) + MT2YY(I)
          MTF(5,IX2(I)) = MTF(5,IX2(I)) + MT2YZ(I)
          MTF(6,IX2(I)) = MTF(6,IX2(I)) + MT2ZZ(I)
          MTF(7,IX2(I)) = MTF(7,IX2(I)) + FX2(I)
          MTF(8,IX2(I)) = MTF(8,IX2(I)) + FY2(I)
          MTF(9,IX2(I)) = MTF(9,IX2(I)) + FZ2(I)

          MTF(1,IX3(I)) = MTF(1,IX3(I)) + MT3XX(I)
          MTF(2,IX3(I)) = MTF(2,IX3(I)) + MT3XY(I)
          MTF(3,IX3(I)) = MTF(3,IX3(I)) + MT3XZ(I)
          MTF(4,IX3(I)) = MTF(4,IX3(I)) + MT3YY(I)
          MTF(5,IX3(I)) = MTF(5,IX3(I)) + MT3YZ(I)
          MTF(6,IX3(I)) = MTF(6,IX3(I)) + MT3ZZ(I)
          MTF(7,IX3(I)) = MTF(7,IX3(I)) + FX3(I)
          MTF(8,IX3(I)) = MTF(8,IX3(I)) + FY3(I)
          MTF(9,IX3(I)) = MTF(9,IX3(I)) + FZ3(I)

          MTF(1,IX4(I)) = MTF(1,IX4(I)) + MT4XX(I)
          MTF(2,IX4(I)) = MTF(2,IX4(I)) + MT4XY(I)
          MTF(3,IX4(I)) = MTF(3,IX4(I)) + MT4XZ(I)
          MTF(4,IX4(I)) = MTF(4,IX4(I)) + MT4YY(I)
          MTF(5,IX4(I)) = MTF(5,IX4(I)) + MT4YZ(I)
          MTF(6,IX4(I)) = MTF(6,IX4(I)) + MT4ZZ(I)
          MTF(7,IX4(I)) = MTF(7,IX4(I)) + FX4(I)
          MTF(8,IX4(I)) = MTF(8,IX4(I)) + FY4(I)
          MTF(9,IX4(I)) = MTF(9,IX4(I)) + FZ4(I)
#include "lockoff.inc"
        ENDIF
      ENDDO
C-----------------------------------------------------
      RETURN
      END
Chd|====================================================================
Chd|  I18KINE_V                     source/interfaces/int18/i18main_kine.F
Chd|-- called by -----------
Chd|        I18MAIN_KINE_V                source/interfaces/int18/i18main_kine.F
Chd|-- calls ---------------
Chd|        TRI7BOX                       share/modules/tri7box.F       
Chd|====================================================================
      SUBROUTINE I18KINE_V(JLT ,A      ,V      ,CAND_E ,CAND_N ,
     2                  GAP    ,MS     ,NOINT  ,STFN   ,ITAB  ,
     3                  STIFN  ,STIF   ,X      ,IRECT  ,NSV    ,
     4                  NX1    ,NX2    ,NX3    ,NX4    ,NY1    ,
     5                  NY2    ,NY3    ,NY4    ,NZ1    ,NZ2    ,
     6                  NZ3    ,NZ4    ,LB1    ,LB2    ,LB3    ,
     7                  LB4    ,LC1    ,LC2    ,LC3    ,LC4    ,
     8                  P1     ,P2     ,P3     ,P4     ,NIN    ,
     9                  GAPV   ,INACTI ,VXI    ,VYI    ,VZI    ,
     A                  MSI    ,MTF    ,CAND_SAV,NSN)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE TRI7BOX
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
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "com06_c.inc"
#include      "com08_c.inc"
#include      "scr02_c.inc"
#include      "scr03_c.inc"
#include      "scr05_c.inc"
#include      "scr07_c.inc"
#include      "scr11_c.inc"
#include      "scr14_c.inc"
#include      "scr16_c.inc"
#include      "scr18_c.inc"
#include      "units_c.inc"
#include      "parit_c.inc"
#include      "warn_c.inc"
#include      "couple_c.inc"
#include      "param_c.inc"
ctmp+1
#include      "task_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER JLT,IBCM,IBCS,INACTI,NIN,
     .        ITAB(*),NOINT,IRECT(4,*),CAND_N(*),CAND_E(*),
     .        INTTH,IFORM,NSN
      INTEGER NSV(*)
      my_real
     .   X(3,*),
     .   A(3,*), MS(*), V(3,*), GAP, STFN(*),STIFN(*)
      my_real
     .     NX1(MVSIZ), NX2(MVSIZ), NX3(MVSIZ), NX4(MVSIZ),
     .     NY1(MVSIZ), NY2(MVSIZ), NY3(MVSIZ), NY4(MVSIZ),
     .     NZ1(MVSIZ), NZ2(MVSIZ), NZ3(MVSIZ), NZ4(MVSIZ),
     .     LB1(MVSIZ), LB2(MVSIZ), LB3(MVSIZ), LB4(MVSIZ),
     .     LC1(MVSIZ), LC2(MVSIZ), LC3(MVSIZ), LC4(MVSIZ),
     .     P1(MVSIZ), P2(MVSIZ), P3(MVSIZ), P4(MVSIZ), STIF(MVSIZ),
     .     GAPV(MVSIZ),STIFSAV(MVSIZ), MTF(14,*),CAND_SAV(8,*),
     .     VXI(MVSIZ),VYI(MVSIZ),VZI(MVSIZ),MSI(MVSIZ),
     .     RSTIF
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J1, IG, J, JG , K0,NBINTER,K1S,K,IL,IE, NN, NI,
     .        NA1,NA2,L
      INTEGER IX1(MVSIZ),IX2(MVSIZ),IX3(MVSIZ),IX4(MVSIZ),NSVG(MVSIZ)
      my_real
     .   FXI(MVSIZ), FYI(MVSIZ), FZI(MVSIZ), FNI(MVSIZ),
     .   FXT(MVSIZ),FYT(MVSIZ),FZT(MVSIZ),
     .   FX1(MVSIZ), FX2(MVSIZ), FX3(MVSIZ), FX4(MVSIZ),
     .   FY1(MVSIZ), FY2(MVSIZ), FY3(MVSIZ), FY4(MVSIZ),
     .   FZ1(MVSIZ), FZ2(MVSIZ), FZ3(MVSIZ), FZ4(MVSIZ),
     .   N1(MVSIZ), N2(MVSIZ), N3(MVSIZ), PENE(MVSIZ),
     .   VIS2(MVSIZ), DTMI(MVSIZ), XMU(MVSIZ),STIF0(MVSIZ),
     .   H1(MVSIZ), H2(MVSIZ), H3(MVSIZ), H4(MVSIZ),
     .   VX(MVSIZ), VY(MVSIZ), VZ(MVSIZ), VN(MVSIZ),DIST(MVSIZ),
     .   VNX, VNY, VNZ, AA, CRIT,S2,RDIST,
     .   FM2, VISCA, FAC,FF,ALPHI,ALPHA,BETA,
     .   FX, FY, FZ, F2, MAS2, M2SK, DTMI0,DTI,FT,FN,FMAX,FTN,
     .   FACM1, ECONTT, ECONVT, H0, LA1, LA2, LA3, LA4,
     .   D1,D2,D3,D4,A1,A2,A3,A4,
     .   FSAV1, FSAV2, FSAV3, FSAV4, FSAV5, FSAV6, FSAV7, FSAV8,
     .   FSAV9, FSAV10, FSAV11, FSAV12, FSAV13, FSAV14, FSAV15, FFO,
     .   E10, H0D, S2D, SUM,
     .   LA1D,LA2D,LA3D,LA4D,T1,T1D,T2,T2D,FFD,VISD,FACD,D1D,
     .   P1S(MVSIZ),P2S(MVSIZ),P3S(MVSIZ),P4S(MVSIZ),
     .   D2D,D3D,D4D,VNXD,VNYD,VNZD,V2D,FM2D,F2D,AAD,FXD,FYD,FZD,
     .   A1D,A2D,A3D,A4D,VV,AX1,AX2,AY1,AY2,AZ1,AZ2,AX,AY,AZ,
     .   AREA,P,VV1,VV2,V21,DMU, DTI2,H00 ,A0X,A0Y,A0Z,RX,RY,RZ,
     .   ANX,ANY,ANZ,AAN,AAX,AAY,AAZ ,RR,RS,AAA ,TM,TS
      my_real
     .   PREC
      my_real
     .   ST1(MVSIZ),ST2(MVSIZ),ST3(MVSIZ),ST4(MVSIZ),STV(MVSIZ),
     .   KT(MVSIZ),C(MVSIZ),CF(MVSIZ),
     .   KS(MVSIZ),K1(MVSIZ),K2(MVSIZ),K3(MVSIZ),K4(MVSIZ),
     .   CS(MVSIZ),C1(MVSIZ),C2(MVSIZ),C3(MVSIZ),C4(MVSIZ),
     .   CX,CY,CFI,AUX,PHI1(MVSIZ), PHI2(MVSIZ), PHI3(MVSIZ),
     .   PHI4(MVSIZ)
      INTEGER JSUB,KSUB,JJ,KK,IN,NSUB,IBID
      my_real
     .   IMPX,IMPY,IMPZ,PP1,PP2,PP3,PP4,BID,
     .   V1,V2,V3,V4,VSX,VSY,VSZ
C
C-----------------------------------------------
#include "sync_dir.inc"
      ECONTT = ZERO
      ECONVT = ZERO
C--------------------------------------------------------
      DO I=1,JLT
        H1(I)   = CAND_SAV(1,I)
        H2(I)   = CAND_SAV(2,I)
        H3(I)   = CAND_SAV(3,I)
        H4(I)   = CAND_SAV(4,I)
        N1(I)   = CAND_SAV(5,I)
        N2(I)   = CAND_SAV(6,I)
        N3(I)   = CAND_SAV(7,I)
        PENE(I) = CAND_SAV(8,I)
        L       = CAND_E(I)
        IX1(I)  = IRECT(1,L)
        IX2(I)  = IRECT(2,L)
        IX3(I)  = IRECT(3,L)
        IX4(I)  = IRECT(4,L)
c        NSVG(I) = NSV(CAND_N(I))
        NI = CAND_N(I)
        IF(NI.LE.NSN)THEN
          IG = NSV(NI)
          NSVG(I) = IG
        ELSE
          NN = NI - NSN
          NSVG(I) = -NN
        ENDIF

      ENDDO
C---------------------------------
C     le main impose sa vitesse normale au secnd
C      + calcul des forces pour post
C---------------------------------
      DO I=1,JLT
        IF(PENE(I).GT.ZERO)THEN
          IG=NSVG(I)
          FAC = UN
c          IF(IG.GT.0)THEN
c            N1(I)   = MTF(12,IG)
c            N2(I)   = MTF(13,IG)
c            N3(I)   = MTF(14,IG)
c            FAC  = PENE(I) / MTF(10,IG)
c          ELSE
c            NN=-IG
c            N1(I) = MTFI_N(NIN)%P(1,NN)
c            N2(I) = MTFI_N(NIN)%P(2,NN)
c            N3(I) = MTFI_N(NIN)%P(3,NN)
c            FAC  = PENE(I) / MTFI_PENE(NIN)%P(NN)
c          ENDIF
c         la normale n'est pas normee
          V1 = N1(I) * (V(1,IX1(I))+DT12*A(1,IX1(I)))
     .       + N2(I) * (V(2,IX1(I))+DT12*A(2,IX1(I)))
     .       + N3(I) * (V(3,IX1(I))+DT12*A(3,IX1(I)))
          V2 = N1(I) * (V(1,IX2(I))+DT12*A(1,IX2(I)))
     .       + N2(I) * (V(2,IX2(I))+DT12*A(2,IX2(I)))
     .       + N3(I) * (V(3,IX2(I))+DT12*A(3,IX2(I)))
          V3 = N1(I) * (V(1,IX3(I))+DT12*A(1,IX3(I)))
     .       + N2(I) * (V(2,IX3(I))+DT12*A(2,IX3(I)))
     .       + N3(I) * (V(3,IX3(I))+DT12*A(3,IX3(I)))
          V4 = N1(I) * (V(1,IX4(I))+DT12*A(1,IX4(I)))
     .       + N2(I) * (V(2,IX4(I))+DT12*A(2,IX4(I)))
     .       + N3(I) * (V(3,IX4(I))+DT12*A(3,IX4(I)))
          AAA = MAX(EM30,N1(I)**2 + N2(I)**2 + N3(I)**2)
          AAA = FAC*(H1(I)*V1 + H2(I)*V2 + H3(I)*V3 + H4(I)*V4)/AAA
c         on divise par le carre de la normale Vs = (n.Vm).n / n.n
          VSX = N1(I) * AAA
          VSY = N2(I) * AAA
          VSZ = N3(I) * AAA
          IF(IG.GT.0)THEN
#include "lockon.inc"
            MTF(1,IG) = MTF(1,IG)+VSX
            MTF(2,IG) = MTF(2,IG)+VSY
            MTF(3,IG) = MTF(3,IG)+VSZ
            MTF(4,IG) = V(1,IG) + DT12*A(1,IG)
            MTF(5,IG) = V(2,IG) + DT12*A(2,IG)
            MTF(6,IG) = V(3,IG) + DT12*A(3,IG)
#include "lockoff.inc"
          ELSE
            NN=-IG
            MTFI_V(NIN)%P(1,NN) = MTFI_V(NIN)%P(1,NN)+VSX
            MTFI_V(NIN)%P(2,NN) = MTFI_V(NIN)%P(2,NN)+VSY
            MTFI_V(NIN)%P(3,NN) = MTFI_V(NIN)%P(3,NN)+VSZ
            MTFI_V(NIN)%P(4,NN) = VFI(NIN)%P(1,NN)+
     *                                      DT12*I18KAFI(NIN)%P(1,NN)
            MTFI_V(NIN)%P(5,NN) = VFI(NIN)%P(2,NN)+
     *                                      DT12*I18KAFI(NIN)%P(2,NN)
            MTFI_V(NIN)%P(6,NN) = VFI(NIN)%P(3,NN)+
     *                                      DT12*I18KAFI(NIN)%P(3,NN)
          ENDIF

        ENDIF
C
C
      ENDDO

C-----------------------------------------------------
C Pour le moment pas utile
C-----------------------------------------------------
CC spmd : identification des noeuds interf. utiles a envoyer
C      IF (NSPMD > 1) THEN
C        DO I = 1,JLT
C          IF(PENE(I).GT.ZERO)THEN
C           NN = NSVG(I)
C          IF(NN.LT.0)THEN
C tag temporaire de NSVFI a -
C            NSVFI(NIN)%P(-NN) = -ABS(NSVFI(NIN)%P(-NN))
C          ENDIF
C         ENDIF
C        ENDDO
C      ENDIF
C-----------------------------------------------------
C
      RETURN
      END
Chd|====================================================================
Chd|  I18KINE_S                     source/interfaces/int18/i18main_kine.F
Chd|-- called by -----------
Chd|        I18MAIN_KINE_S                source/interfaces/int18/i18main_kine.F
Chd|-- calls ---------------
Chd|        H3D_MOD                       share/modules/h3d_mod.F       
Chd|        TRI7BOX                       share/modules/tri7box.F       
Chd|====================================================================
      SUBROUTINE I18KINE_S(JLT ,A      ,V      ,CAND_E,CAND_N ,
     2                  GAP    ,MS    ,NOINT  ,STFN   ,ITAB   ,
     3                  STIFN  ,STIF   ,X      ,IRECT ,NSV    ,
     4                  NX1    ,NX2    ,NX3    ,NX4   ,NY1    ,
     5                  NY2    ,NY3    ,NY4    ,NZ1   ,NZ2    ,
     6                  NZ3    ,NZ4    ,LB1    ,LB2   ,LB3    ,
     7                  LB4    ,LC1    ,LC2    ,LC3   ,LC4    ,
     8                  P1     ,P2     ,P3     ,P4    ,NIN    ,
     9                  GAPV   ,INACTI ,VXI    ,VYI   ,VZI    ,
     A                  MSI    ,MTF    ,CAND_SAV,FCONT,FSAV   ,
     B                  NSN    ,SLVNDTAG,H3D_DATA )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE TRI7BOX
      USE H3D_MOD
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
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "com06_c.inc"
#include      "com08_c.inc"
#include      "scr02_c.inc"
#include      "scr03_c.inc"
#include      "scr05_c.inc"
#include      "scr07_c.inc"
#include      "scr11_c.inc"
#include      "scr14_c.inc"
#include      "scr16_c.inc"
#include      "scr18_c.inc"
#include      "units_c.inc"
#include      "parit_c.inc"
#include      "warn_c.inc"
#include      "couple_c.inc"
#include      "param_c.inc"
ctmp+1
#include      "task_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER JLT,IBCM,IBCS,INACTI,NIN,
     .        ITAB(*), NOINT, IRECT(4,*),CAND_N(*),CAND_E(*),
     .        INTTH,IFORM,NSN
      INTEGER NSV(*) ,SLVNDTAG(*)
      my_real
     .   X(3,*),
     .   A(3,*), MS(*), V(3,*), FSAV(*),FCONT(3,*),
     .   GAP, STFN(*),STIFN(*), MTF(14,*),CAND_SAV(8,*)
      my_real
     .     NX1(MVSIZ), NX2(MVSIZ), NX3(MVSIZ), NX4(MVSIZ),
     .     NY1(MVSIZ), NY2(MVSIZ), NY3(MVSIZ), NY4(MVSIZ),
     .     NZ1(MVSIZ), NZ2(MVSIZ), NZ3(MVSIZ), NZ4(MVSIZ),
     .     LB1(MVSIZ), LB2(MVSIZ), LB3(MVSIZ), LB4(MVSIZ),
     .     LC1(MVSIZ), LC2(MVSIZ), LC3(MVSIZ), LC4(MVSIZ),
     .     P1(MVSIZ), P2(MVSIZ), P3(MVSIZ), P4(MVSIZ), STIF(MVSIZ),
     .     GAPV(MVSIZ),
     .     STIFSAV(MVSIZ),
     .     VXI(MVSIZ),VYI(MVSIZ),VZI(MVSIZ),MSI(MVSIZ),
     .     RSTIF
      TYPE(H3D_DATABASE) :: H3D_DATA
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J1, IG, J, JG , K0,NBINTER,K1S,K,IL,IE, NN, NI,
     .        NA1,NA2,L
      INTEGER IX1(MVSIZ),IX2(MVSIZ),IX3(MVSIZ),IX4(MVSIZ),NSVG(MVSIZ)
      my_real
     .   FXI(MVSIZ), FYI(MVSIZ), FZI(MVSIZ), FNI(MVSIZ),
     .   FXT(MVSIZ),FYT(MVSIZ),FZT(MVSIZ),
     .   FX1(MVSIZ), FX2(MVSIZ), FX3(MVSIZ), FX4(MVSIZ),
     .   FY1(MVSIZ), FY2(MVSIZ), FY3(MVSIZ), FY4(MVSIZ),
     .   FZ1(MVSIZ), FZ2(MVSIZ), FZ3(MVSIZ), FZ4(MVSIZ),
     .   N1(MVSIZ), N2(MVSIZ), N3(MVSIZ), PENE(MVSIZ),
     .   VIS2(MVSIZ), DTMI(MVSIZ), XMU(MVSIZ),STIF0(MVSIZ),
     .   H1(MVSIZ), H2(MVSIZ), H3(MVSIZ), H4(MVSIZ),
     .   VX(MVSIZ), VY(MVSIZ), VZ(MVSIZ), VN(MVSIZ),DIST(MVSIZ),
     .   VNX, VNY, VNZ, AA, CRIT,S2,RDIST,
     .   FM2, DT12INV, VISCA, FAC, FAC2,FF,ALPHI,ALPHA,BETA,
     .   FX, FY, FZ, F2, MAS2, M2SK, DTMI0,DTI,FT,FN,FMAX,FTN,
     .   FACM1, ECONTT, ECONVT, H0, LA1, LA2, LA3, LA4,
     .   D1,D2,D3,D4,A1,A2,A3,A4,
     .   FSAV1, FSAV2, FSAV3, FSAV4, FSAV5, FSAV6, FSAV7, FSAV8,
     .   FSAV9, FSAV10, FSAV11, FSAV12, FSAV13, FSAV14, FSAV15, FFO,
     .   E10, H0D, S2D, SUM,
     .   LA1D,LA2D,LA3D,LA4D,T1,T1D,T2,T2D,FFD,VISD,FACD,D1D,
     .   P1S(MVSIZ),P2S(MVSIZ),P3S(MVSIZ),P4S(MVSIZ),
     .   D2D,D3D,D4D,VNXD,VNYD,VNZD,V2D,FM2D,F2D,AAD,FXD,FYD,FZD,
     .   A1D,A2D,A3D,A4D,VV,AX1,AX2,AY1,AY2,AZ1,AZ2,AX,AY,AZ,
     .   AREA,P,VV1,VV2,V21,DMU, DTI2,H00 ,A0X,A0Y,A0Z,RX,RY,RZ,
     .   ANX,ANY,ANZ,AAN,AAX,AAY,AAZ ,RR,RS,AAA ,TM,TS
      my_real
     .   PREC
      my_real
     .   ST1(MVSIZ),ST2(MVSIZ),ST3(MVSIZ),ST4(MVSIZ),STV(MVSIZ),
     .   KT(MVSIZ),C(MVSIZ),CF(MVSIZ),
     .   KS(MVSIZ),K1(MVSIZ),K2(MVSIZ),K3(MVSIZ),K4(MVSIZ),
     .   CS(MVSIZ),C1(MVSIZ),C2(MVSIZ),C3(MVSIZ),C4(MVSIZ),
     .   CX,CY,CFI,AUX,PHI1(MVSIZ), PHI2(MVSIZ), PHI3(MVSIZ),
     .   PHI4(MVSIZ)
      INTEGER JSUB,KSUB,JJ,KK,IN,NSUB,IBID
      my_real
     .   IMPX,IMPY,IMPZ,PP1,PP2,PP3,PP4,BID,BBB,
     .   V1,V2,V3,V4,VSX,VSY,VSZ,DVSX,DVSY,DVSZ,VSXP,VSYP,VSZP
C
C-----------------------------------------------
#include "sync_dir.inc"
      IF(DT12.GT.ZERO)THEN
        DT12INV = UN/DT12
      ELSE
        DT12INV =ZERO
      ENDIF
      ECONTT = ZERO
      ECONVT = ZERO
C--------------------------------------------------------
      DO I=1,JLT
        H1(I)   = CAND_SAV(1,I)
        H2(I)   = CAND_SAV(2,I)
        H3(I)   = CAND_SAV(3,I)
        H4(I)   = CAND_SAV(4,I)
c        N1(I)   = CAND_SAV(5,I)
c        N2(I)   = CAND_SAV(6,I)
c        N3(I)   = CAND_SAV(7,I)
        PENE(I) = CAND_SAV(8,I)
        L       = CAND_E(I)
        IX1(I)  = IRECT(1,L)
        IX2(I)  = IRECT(2,L)
        IX3(I)  = IRECT(3,L)
        IX4(I)  = IRECT(4,L)
c        NSVG(I) = NSV(CAND_N(I))
        NI = CAND_N(I)
        IF(NI.LE.NSN)THEN
          IG = NSV(NI)
          NSVG(I) = IG
        ELSE
          NN = NI - NSN
          NSVG(I) = -NN
        ENDIF

      ENDDO
C---------------------------------
C     le main impose sa vitesse normale au secnd
C      + calcul des forces pour post
C---------------------------------
      DO I=1,JLT
        IF(PENE(I).GT.ZERO)THEN
c si un second. est traite plusieurs fois,
c la meme vitesse sera imposee a chaque fois.
c la force est ponderee avec FAC
          IG=NSVG(I)
          IF(IG.GT.0)THEN
            VSX = MTF(1,IG)
            VSY = MTF(2,IG)
            VSZ = MTF(3,IG)
            VSXP = MTF(4,IG)
            VSYP = MTF(5,IG)
            VSZP = MTF(6,IG)
c            FAC  = PENE(I) / MTF(10,IG)
            FAC = UN
            FAC2 = UN
c quad           FAC2 = (UN-MTF(11,IG)*MTF(11,IG))
c line            FAC2 = (UN-MTF(11,IG))
c            FAC2 = (MTF(11,IG)**4-DEUX*MTF(11,IG)**2+UN)
            MSI(I)  = MS(IG)
            VXI(I)  = V(1,IG)
            VYI(I)  = V(2,IG)
            VZI(I)  = V(3,IG)
          ELSE
            NN=-IG
            VSX  = MTFI_V(NIN)%P(1,NN)
            VSY  = MTFI_V(NIN)%P(2,NN)
            VSZ  = MTFI_V(NIN)%P(3,NN)
            VSXP = MTFI_V(NIN)%P(4,NN)
            VSYP = MTFI_V(NIN)%P(5,NN)
            VSZP = MTFI_V(NIN)%P(6,NN)
c            FAC  = PENE(I) / MTFI_PENE(NIN)%P(NN)
            FAC = UN
            FAC2 = UN
c            FAC2 = (     MTFI_PENEMIN(NIN)%P(NN)**4-
c     *              DEUX*MTFI_PENEMIN(NIN)%P(NN)**2+UN)
            MSI(I)  = MSFI(NIN)%P(NN)
            VXI(I)  = VFI(NIN)%P(1,NN)
            VYI(I)  = VFI(NIN)%P(2,NN)
            VZI(I)  = VFI(NIN)%P(3,NN)
          ENDIF
          AAA = VSX*VSX + VSY*VSY + VSZ*VSZ
          BBB = MAX(AAA,EM30)
          AAA = (VXI(I)*VSX + VYI(I)*VSY + VZI(I)*VSZ)/BBB
          AAA = (UN-AAA)*FAC2
          DVSX = AAA * VSX
          DVSY = AAA * VSY
          DVSZ = AAA * VSZ
c          VSX = DVSX + VXI(I)
c          VSY = DVSY + VYI(I)
c          VSZ = DVSZ + VZI(I)
c=============================================
c           ce calcul est fait plusieurs fois
c           en cas d'impact multiple
c           mais le resultat est identique
c=============================================
          IF(IG.GT.0)THEN
            A(1,IG)  = DVSX*DT12INV
            A(2,IG)  = DVSY*DT12INV
            A(3,IG)  = DVSZ*DT12INV
            IF(NSPMD > 1)SLVNDTAG(IG)=1
         ELSE
            NN=-IG
            MTFI_A(NIN)%P(1,NN) = DVSX*DT12INV
            MTFI_A(NIN)%P(2,NN) = DVSY*DT12INV
            MTFI_A(NIN)%P(3,NN) = DVSZ*DT12INV
C on emmene la pene comme marqueur si l'on doit mettre a jour
C les accelerations sur le proc qui possede le noeud.
            MTFI_A(NIN)%P(7,NN) = PENE(I)
          ENDIF

c         forces d'interface

          AAA = (VSXP*VSX + VSYP*VSY + VSZP*VSZ)/BBB
          AAA = (UN-AAA)*FAC2
          AAA = AAA*FAC*MSI(I)*DT12INV

          FXI(I) = -AAA * VSX
          FYI(I) = -AAA * VSY
          FZI(I) = -AAA * VSZ

c          FNI(I) = N1(I) * FXI(I) + N2(I) * FYI(I) + N3(I) * FZI(I)
          FNI(I) = SQRT(
     .        FXI(I) * FXI(I) + FYI(I) * FYI(I) + FZI(I) * FZI(I))

          FX1(I)=FXI(I)*H1(I)
          FY1(I)=FYI(I)*H1(I)
          FZ1(I)=FZI(I)*H1(I)

          FX2(I)=FXI(I)*H2(I)
          FY2(I)=FYI(I)*H2(I)
          FZ2(I)=FZI(I)*H2(I)

          FX3(I)=FXI(I)*H3(I)
          FY3(I)=FYI(I)*H3(I)
          FZ3(I)=FZI(I)*H3(I)

          FX4(I)=FXI(I)*H4(I)
          FY4(I)=FYI(I)*H4(I)
          FZ4(I)=FZI(I)*H4(I)
        ENDIF
C
      ENDDO

CC spmd : identification des noeuds interf. utiles a envoyer
C      IF (NSPMD > 1) THEN
C        DO I = 1,JLT
C         IF(PENE(I).GT.ZERO)THEN
C          NN = NSVG(I)
C          IF(NN.LT.0)THEN
C tag temporaire de NSVFI a -
C            NSVFI(NIN)%P(-NN) = -ABS(NSVFI(NIN)%P(-NN))
C          ENDIF
C         ENDIF
C        ENDDO
C      ENDIF
C---------------------------------
C     SAUVEGARDE DE L'IMPULSION NORMALE
C---------------------------------
      FSAV1 = ZERO
      FSAV2 = ZERO
      FSAV3 = ZERO
      FSAV8 = ZERO
      FSAV9 = ZERO
      FSAV10= ZERO
      FSAV11= ZERO
      DO I=1,JLT
c       ECONVT = ECONVT + DT1*VN(I)*FNI(I)
C fait uniquement si ibag <> 0
C       PRES(CE_LOC(I)) = PRES(CE_LOC(I)) + FNI(I)
c       FXI(I)=N1(I)*FNI(I)
c       FYI(I)=N2(I)*FNI(I)
c       FZI(I)=N3(I)*FNI(I)
        IF(PENE(I).GT.ZERO)THEN
          IMPX=FXI(I)*DT12
          IMPY=FYI(I)*DT12
          IMPZ=FZI(I)*DT12
          FSAV1 =FSAV1 +IMPX
          FSAV2 =FSAV2 +IMPY
          FSAV3 =FSAV3 +IMPZ
          FSAV8 =FSAV8 +ABS(IMPX)
          FSAV9 =FSAV9 +ABS(IMPY)
          FSAV10=FSAV10+ABS(IMPZ)
          FSAV11=FSAV11+FNI(I)*DT12
        ENDIF
      ENDDO
#include "lockon.inc"
        FSAV(1)=FSAV(1)+FSAV1
        FSAV(2)=FSAV(2)+FSAV2
        FSAV(3)=FSAV(3)+FSAV3
        FSAV(8)=FSAV(8)+FSAV8
        FSAV(9)=FSAV(9)+FSAV9
        FSAV(10)=FSAV(10)+FSAV10
        FSAV(11)=FSAV(11)+FSAV11
#include "lockoff.inc"
C
      IF(ANIM_V(4)+OUTP_V(4)+H3D_DATA%N_VECT_CONT.GT.0.AND.
     .          (TT.GE.TANIM.OR.TT.GE.TOUTP.OR.TT.GE.H3D_DATA%TH3D.OR.
     .              (MANIM.GE.4.AND.MANIM.LE.15).OR. H3D_DATA%MH3D .NE. 0))THEN
#include "lockon.inc"
         DO I=1,JLT
          IF(PENE(I).GT.ZERO)THEN
	    FCONT(1,IX1(I)) =FCONT(1,IX1(I)) + FX1(I)
	    FCONT(2,IX1(I)) =FCONT(2,IX1(I)) + FY1(I)
	    FCONT(3,IX1(I)) =FCONT(3,IX1(I)) + FZ1(I)
	    FCONT(1,IX2(I)) =FCONT(1,IX2(I)) + FX2(I)
	    FCONT(2,IX2(I)) =FCONT(2,IX2(I)) + FY2(I)
	    FCONT(3,IX2(I)) =FCONT(3,IX2(I)) + FZ2(I)
	    FCONT(1,IX3(I)) =FCONT(1,IX3(I)) + FX3(I)
	    FCONT(2,IX3(I)) =FCONT(2,IX3(I)) + FY3(I)
	    FCONT(3,IX3(I)) =FCONT(3,IX3(I)) + FZ3(I)
	    FCONT(1,IX4(I)) =FCONT(1,IX4(I)) + FX4(I)
	    FCONT(2,IX4(I)) =FCONT(2,IX4(I)) + FY4(I)
	    FCONT(3,IX4(I)) =FCONT(3,IX4(I)) + FZ4(I)
            JG = NSVG(I)
            IF(JG.GT.0) THEN
              FCONT(1,JG)=FCONT(1,JG)- FXI(I)
              FCONT(2,JG)=FCONT(2,JG)- FYI(I)
              FCONT(3,JG)=FCONT(3,JG)- FZI(I)
            ELSE
              NN=-JG
              MTFI_A(NIN)%P(4,NN) = MTFI_A(NIN)%P(4,NN) - FXI(I)
              MTFI_A(NIN)%P(5,NN) = MTFI_A(NIN)%P(5,NN) - FYI(I)
              MTFI_A(NIN)%P(6,NN) = MTFI_A(NIN)%P(6,NN) - FZI(I)
            ENDIF
           ENDIF
         ENDDO
#include "lockoff.inc"
      ENDIF
C-----------------------------------------------------
C
      RETURN
      END
