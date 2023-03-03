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
C
Chd|====================================================================
Chd|  C_FRONT                       source/restart/ddsplit/c_front.F
Chd|-- called by -----------
Chd|        DDSPLIT                       source/restart/ddsplit/ddsplit.F
Chd|-- calls ---------------
Chd|        C_IFRONT                      source/spmd/node/ddtools.F    
Chd|        NLOCAL                        source/spmd/node/ddtools.F    
Chd|        FRONT_MOD                     share/modules1/front_mod.F    
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|====================================================================
      SUBROUTINE C_FRONT(PROC     ,NBDDACC,NBDDKIN,NBDDNRB,
     2                   NPBY     ,NRBYKIN_L,LJOINT ,NBDDNCJ,IBVEL  ,
     3                   NBDDNRBM ,IADLL    ,LLL    ,NLAGF_L,FRONT_RM,
     4                   NRBYMK_L ,NBDDNRBYM, USERNODS,SFR_USERWI_L,
     5                   SDD_R2R_ELEM,ADDCSRECT,CSRECT,NBDDNORT,NBDDNOR_MAX,
     6                   NBCCNOR,NBCCFR25,NBDDEDGT  ,NBDDEDG_MAX,NRTMX25   ,
     7                   IPARI  ,INTBUF_TAB,INTERCEP,NODGLOB    ,NODLOCAL  ,
     8                   NUMNOD_L)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE FRONT_MOD
      USE INTBUFDEF_MOD  
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
#include      "lagmult.inc"
#include      "r2r_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
       INTEGER  PROC, NBDDACC, NBDDKIN, NBDDNRB,NRBYKIN_L, NBDDNCJ,
     .          NBDDNRBM, NLAGF_L,NRBYMK_L ,NBDDNRBYM, NBDDNORT, 
     .          NBDDNOR_MAX, NBCCNOR, NBCCFR25, NBDDEDGT,NBDDEDG_MAX,NRTMX25,
     .          NPBY(NNPBY,*), LJOINT(*),
     .          IBVEL(NBVELP,*) , IADLL(*), LLL(*),FRONT_RM(NRBYM,*),
     .          USERNODS(*),SFR_USERWI_L,SDD_R2R_ELEM,
     .          ADDCSRECT(*), CSRECT(*), IPARI(NPARI,*)
       INTEGER, INTENT(IN) :: NUMNOD_L
       INTEGER, DIMENSION(NUMNOD_L), INTENT(IN) :: NODGLOB
       INTEGER, DIMENSION(NUMNOD), INTENT(IN) :: NODLOCAL
       TYPE(INTBUF_STRUCT_) INTBUF_TAB(*)
       TYPE(INTERSURFP) :: INTERCEP(3,NINTER)
!       -*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*
!       NODGLOB   : integer, dimension=NUMNOD_L
!                   gives the global ID of a local element
!                   NODGLOB( local_id) = global_id
!       NODLOCAL  : integer, dimension=NUMNOD
!                   gives the local ID of a global element
!                   NODLOCAL( global_id) = local_id
!                   --> used here to avoid NLOCAL call (the NLOCAL perf is bad)
!                       NODLOCAL /= 0 if the element is on the current domain/processor
!                       and =0 if the element is not on the current domain
!       -*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*
C-----------------------------------------------
C   F u n c t i o n
C-----------------------------------------------     
      INTEGER  NLOCAL
      EXTERNAL NLOCAL   
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, P, N, M, NSN, K, J,
     .        IC, IK0, IKN, IK,
     .        IFRLAG(NSPMD),CPT,
     .        NADMSR, NADMSR_L, NI, NTY, NI25, NBDDNOR, NRTM, ISHIFT,
     .        N1, N2, N3, N4, ISBOUND,
     .        NRTM_L, NBDDEDG, II
      INTEGER, DIMENSION(:), ALLOCATABLE :: TAG_SM, TAG_MS, ITAG
      INTEGER TAGP(NSPMD)
! ------------------------------------------------------------
!     allocate 1d array
      ALLOCATE( ITAG(NUMNOD) )
! ------------------------------
C
C Frontiere domdec pure
C
      NBDDACC = 0
      NBDDKIN = 0
      CPT = 0

      DO II = 1,NUMNOD_L
        I = NODGLOB(II)        
          CALL C_IFRONT(I,CPT)
          !returns in CPT the number of procs on which node I is sticked  
          IF(FLAGKIN(I)==0)THEN
           !FLAGKIN array identities boundary nodes with kinematic constraints
           !(FLAGKIN(N)=1 <=> old FRONT TAG=10)  
           !FLAGKIN(N) can be set to one only for first SPMD domain       
           !Add CPT-1 in order to don't take into account current proc himself
            NBDDACC = NBDDACC + (CPT - 1)
          ELSE
            IF(PROC/=1)THEN
              !add only one time when PROC ne 1 and FLAGKIN(I)=1
              NBDDKIN = NBDDKIN + 1
              !do not count proc itself and proc 1
              NBDDACC = NBDDACC + (CPT - 2)
            ELSE
              !Add CPT-1 in order to don't take into account current proc himself 
              NBDDKIN = NBDDKIN + (CPT - 1)
            ENDIF
          ENDIF
      ENDDO
C
C Frontiere Multidomaines
C      
      SDD_R2R_ELEM = 0       
      IF ((NSUBDOM>0).AND.(IDDOM==0)) THEN
        SDD_R2R_ELEM = 2*(NBDDKIN + NBDDACC)       
      ENDIF     
C
C Frontiere RBY (main nodes)
C
      NBDDNRB = 0
      NRBYKIN_L = 0
      DO N = 1, NRBYKIN
        M=NPBY(1,N)
        IF(NODLOCAL(M)/=0.AND.NODLOCAL(M)<=NUMNOD_L)THEN
          NRBYKIN_L = NRBYKIN_L + 1
          DO P = 1, NSPMD
            IF(P/=PROC) THEN
              IF(NLOCAL(M,P)==1) THEN
                NBDDNRB = NBDDNRB + 1
              ENDIF
            ENDIF
          ENDDO
        ENDIF
      ENDDO
C
C Frontiere Cyl. JOINT (proc0)
C
      NBDDNCJ = 0
      K = 1
      DO N = 1, NJOINT
        NSN=LJOINT(K)
        DO J = 1, NSN
          M = LJOINT(K+J)
          IF(PROC/=1) THEN
C proc <> 0, frontiere si noeud sur le proc
             IF(NODLOCAL(M)/=0.AND.NODLOCAL(M)<=NUMNOD_L)THEN
              NBDDNCJ = NBDDNCJ + 1
            END IF
          ELSE
C proc = 0, recherche des autres procs ayant le noeud
            DO P = 2, NSPMD
              IF(NLOCAL(M,P)==1) THEN
                NBDDNCJ = NBDDNCJ + 1
              ENDIF
            END DO
          END IF
        END DO
        K = K + NSN + 1
      END DO
C
C Frontiere RBY MOU (main nodes)
C
      NBDDNRBM = 0
      DO N = 1, NIBVEL
        M=IBVEL(4,N)
        IF(NODLOCAL(M)/=0.AND.NODLOCAL(M)<=NUMNOD_L)THEN
          DO P = 1, NSPMD
            IF(P/=PROC) THEN
              IF(NLOCAL(M,P)==1) THEN         
                NBDDNRBM = NBDDNRBM + 1
              ENDIF
            ENDIF
          ENDDO
        ENDIF
      ENDDO
C
C Frontiere Rigid material (effectif main nodes)
C
      NBDDNRBYM = 0
      NRBYMK_L = 0
      DO N = 1, NRBYM
        IF(MOD(FRONT_RM(N,PROC),10)==1)THEN
          NRBYMK_L = NRBYMK_L + 1
          DO P = 1, NSPMD
            IF(P/=PROC) THEN
              IF(MOD(FRONT_RM(N,P),10)==1) THEN
                NBDDNRBYM = NBDDNRBYM + 1
              ENDIF
            ENDIF
          ENDDO
        ENDIF
      ENDDO
C
C Frontiere LAG MULT
C
      IF(LAG_NCF>0) THEN
        DO N = 1, NUMNOD
          ITAG(N) = 0
        END DO
        DO P = 1, NSPMD
          IFRLAG(P) = 0
        END DO
        DO IC = 1, LAG_NCF
         IK0 = IADLL(IC)
         IKN = IADLL(IC+1)-1
         DO IK = IK0,IKN
          N = LLL(IK)
          IF(ITAG(N)==0) THEN
            ITAG(N) = 1
            DO P = 1, NSPMD
              IF(NLOCAL(N,P)==1)THEN          
                IFRLAG(P) = IFRLAG(P) + 1
                GOTO 100
              END IF
            END DO
  100       CONTINUE
          END IF
         END DO
        END DO
        NLAGF_L = IFRLAG(PROC)
      END IF
! ------------------------------
!     deallocate 1d array
      DEALLOCATE( ITAG )
! ------------------------------
C ---------------------
C     Interfaces TYPE25, Max nb of frontiers wrt vertices overall interfaces
C ---------------------
      NBCCFR25  = 0
      NBCCNOR   = 0

      NBDDNOR_MAX = 0
      NBDDNORT    = 0
      IF(NINTER25/=0)THEN
   
        NI25=0
        ISHIFT = 0

        DO NI=1,NINTER 
          NTY=IPARI(7,NI)
          IF(NTY/=25) CYCLE

          NBDDNOR = 0

          NI25=NI25+1

          NRTM  =IPARI(4,NI)
          NADMSR=IPARI(67,NI)

          ALLOCATE(TAG_SM(NADMSR),TAG_MS(NADMSR))
          TAG_SM(1:NADMSR)=0

          NADMSR_L=0
          DO K=1,NRTM
            N1 = INTBUF_TAB(NI)%ADMSR(4*(K-1)+1)
            N2 = INTBUF_TAB(NI)%ADMSR(4*(K-1)+2)
            N3 = INTBUF_TAB(NI)%ADMSR(4*(K-1)+3)
            N4 = INTBUF_TAB(NI)%ADMSR(4*(K-1)+4) 
            IF(INTERCEP(1,NI)%P(K)==PROC)THEN
              IF(TAG_SM(N1)==0)THEN
                NADMSR_L=NADMSR_L+1
                TAG_SM(N1)=NADMSR_L
              END IF
              IF(TAG_SM(N2)==0)THEN
                NADMSR_L=NADMSR_L+1
                TAG_SM(N2)=NADMSR_L
              END IF
              IF(TAG_SM(N3)==0)THEN
                NADMSR_L=NADMSR_L+1
                TAG_SM(N3)=NADMSR_L
              END IF
              IF(TAG_SM(N4)==0)THEN
                NADMSR_L=NADMSR_L+1
                TAG_SM(N4)=NADMSR_L
              END IF
            ENDIF
          ENDDO 
          
          DO I = 1, NADMSR  
            K = TAG_SM(I)        
            IF(K/=0)THEN
               TAG_MS(K)=I
            END IF
          END DO

          DO I = 1, NADMSR_L         
            N = TAG_MS(I) + ISHIFT
            ISBOUND=0
            TAGP(1:NSPMD)=0
            DO J = ADDCSRECT(N), ADDCSRECT(N+1)-1
               K = CSRECT(J)
               P = INTERCEP(1,NI)%P(K)
               IF(P /= PROC.AND.TAGP(P)==0) THEN
                 NBDDNOR = NBDDNOR + 1
                 ISBOUND = 1
                 TAGP(P) = 1
               ENDIF
            ENDDO
            NBCCFR25 = NBCCFR25 + (ADDCSRECT(N+1)- ADDCSRECT(N))*ISBOUND
            NBCCNOR  = NBCCNOR + (ADDCSRECT(N+1)- ADDCSRECT(N))
          ENDDO
          ISHIFT=ISHIFT+NADMSR

          NBDDNOR_MAX = MAX(NBDDNOR_MAX,NBDDNOR)
          NBDDNORT    = NBDDNORT+NBDDNOR


          DEALLOCATE(TAG_SM, TAG_MS)

        END DO

      END IF ! NINTER25/=0

C ---------------------
C     Interfaces TYPE25, Max nb of frontiers wrt edges overall interfaces
C ---------------------
      NBDDEDG_MAX = 0
      NBDDEDGT    = 0

      NRTMX25=0
      IF(NINTER25/=0)THEN
   
        NI25=0

        DO NI=1,NINTER 
          NTY=IPARI(7,NI)
          IF(NTY/=25) CYCLE

          NBDDEDG = 0

          NI25=NI25+1

          NRTM  =IPARI(4,NI)

          ALLOCATE(TAG_SM(NRTM),TAG_MS(NRTM))
          TAG_SM(1:NRTM)=0

          NRTM_L=0
          DO K=1,NRTM
            IF(INTERCEP(1,NI)%P(K)==PROC)THEN
              NRTM_L=NRTM_L+1
              TAG_SM(K)=NRTM_L
            ENDIF
          ENDDO 

          NRTMX25 = MAX(NRTMX25,NRTM_L) 
                   
          DO I = 1, NRTM 
            K = TAG_SM(I)        
            IF(K/=0)THEN
               TAG_MS(K)=I
            END IF
          END DO

          DO I = 1, NRTM_L         
            N = TAG_MS(I)

            DO J = 1,4
               K = INTBUF_TAB(NI)%MVOISIN(4*(N-1)+J)
               IF(K/=0)THEN
                 P = INTERCEP(1,NI)%P(K)
                 IF(P /= PROC) THEN
                   NBDDEDG = NBDDEDG + 1
                 ENDIF
               ENDIF
            ENDDO
          ENDDO

          NBDDEDG_MAX = MAX(NBDDEDG_MAX,NBDDEDG)
          NBDDEDGT    = NBDDEDGT+NBDDEDG

          DEALLOCATE(TAG_SM,TAG_MS)

        END DO

      END IF ! NINTER25/=0
C
C ---------------------
C Fontiere UserWindow comptage de la taille du tableau FR_USERW
C ---------------------
      SFR_USERWI_L = 0
      IF (IUSERWI /= 0)THEN
        IF(PROC/=1)THEN
C pour les procs autre que le premier on note les noeuds
C dont on va recevoir les accels.
          DO II=1,NUMNOD_L
            I=NODGLOB(II)
            IF(USERNODS(I)==1)THEN
               SFR_USERWI_L=SFR_USERWI_L+1
            ENDIF
          ENDDO
        ELSE
          DO I=1,NUMNOD
             IF(USERNODS(I)==1)THEN
               DO P=2,NSPMD
                 IF(NLOCAL(I,P)==1)THEN          
                    SFR_USERWI_L=SFR_USERWI_L+1
                 ENDIF
               ENDDO
             ENDIF
          ENDDO
        ENDIF
      ENDIF
      RETURN
      END
