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
Chd|  C_FVBAG                       source/airbag/c_fvbag.F       
Chd|-- called by -----------
Chd|        DDSPLIT                       source/restart/ddsplit/ddsplit.F
Chd|-- calls ---------------
Chd|        NLOCAL                        source/spmd/node/ddtools.F    
Chd|        FVBAG_MOD                     share/modules1/fvbag_mod.F    
Chd|====================================================================
      SUBROUTINE C_FVBAG(
     .    MONVOL,   NODLOCAL,  IXS     , PROC, NB_NODE, FVMASTER)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE FVBAG_MOD
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
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER 
     .   MONVOL(*), NODLOCAL(*), IXS(NIXS,*), PROC, NB_NODE,
     .   FVMASTER(*)
C-----------------------------------------------
C   E x t e r n a l   F u n c t i o n s
C-----------------------------------------------   
      INTEGER  NLOCAL
      EXTERNAL NLOCAL     
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER K1, K2, IFV, I, ITYP, NN, KI1, J, NMAX, PMASTER, P, NN_L,
     .        PP, JJ, NTG, NBA, NNA, KIA1, NNA_L, 
     .        K, KK, NNSA, NNSA_L, KI2,
     .        NTGM, NNI, NTGI, NNI_L
      INTEGER, DIMENSION(:), ALLOCATABLE :: IBUF, IBUFA,ITAG,REDIR,IBUFSA
C-----------------------------------------------
!     1d array
      ALLOCATE( ITAG(NB_NODE),REDIR(NB_NODE) )
      ALLOCATE( IBUFSA(NB_NODE) )

!     and deallocated in fvwrestp
      ALLOCATE( FVSPMD(NFVBAG) )
! ---------------------------------
C
      IF (PROC.EQ.1)THEN
        NTGM = 0
        K1 = 1
        DO I=1,NVOLU
          ITYP=MONVOL(K1-1+2)
          IF (ITYP.EQ.6.OR.ITYP.EQ.8) THEN
            NTG=MONVOL(K1-1+33)
          ELSE
            NTG = 0
          ENDIF
          NTGM = MAX(NTGM,NTG)
          K1=K1+NIMV
        ENDDO
      ENDIF
C
      K1=1
      K2=1+NIMV*NVOLU+LICBAG+LIBAGJET+LIBAGHOL
      IFV=0
      DO I=1,NVOLU
         ITYP=MONVOL(K1-1+2)
         IF (ITYP.EQ.6.OR.ITYP.EQ.8) THEN
            IFV=IFV+1
            NN= MONVOL(K1-1+32)
            NTG=MONVOL(K1-1+33)
            NBA=MONVOL(K1-1+62)
            NNA=MONVOL(K1-1+64)
            NNI=MONVOL(K1-1+68)
            NTGI=MONVOL(K1-1+69)
            KI1 =K2+MONVOL(K1-1+31)
            KIA1=K2+MONVOL(K1-1+20)-1
C Noeuds de l'enveloppe
            ALLOCATE(IBUF(NN+NNI), IBUFA(NNA))
            DO J=1,NN+NNI
               IBUF(J)=MONVOL(KI1-1+J)
            ENDDO
            DO J=1,NNA
               IBUFA(J)=MONVOL(KIA1-1+J)
            ENDDO
C Identification du PMASTER
            NMAX=0
            PMASTER=1
            DO P=1,NSPMD
               NN_L=0
               DO J=1,NN
                  JJ=IBUF(J)
                   IF(NLOCAL(JJ,P)==1)THEN
                     DO PP = 1, P-1
                        IF(NLOCAL(JJ,PP)==1)THEN
                          GOTO 100
                        ENDIF
                     ENDDO
                     NN_L=NN_L+1
 100                 CONTINUE
                  ENDIF
               ENDDO
               NNI_L=0
               DO J=NN+1,NN+NNI
                  JJ=IBUF(J)
                   IF(NLOCAL(JJ,P)==1)THEN
                     DO PP = 1, P-1
                        IF(NLOCAL(JJ,PP)==1)THEN
                          GOTO 200
                        ENDIF
                     ENDDO
                     NNI_L=NNI_L+1
 200                 CONTINUE
                  ENDIF
               ENDDO
               IF(NN_L+NNI_L.GT.NMAX)THEN
                  PMASTER=P
                  NMAX=NN_L+NNI_L
               ENDIF
            ENDDO


            IF ((ITYP.EQ.6.OR.ITYP.EQ.8) .AND. FVMASTER(IFV) >= 0) THEN
           !FVMASTER(IFV) > 0 : The master processor was computed during domain decomposition
              PMASTER = FVMASTER(IFV)
            ENDIF 
            FVSPMD(IFV)%PMASTER=PMASTER
C Noeuds locaux
            NN_L=0
            DO J=1,NN
               JJ=IBUF(J)
               IF (NODLOCAL(JJ).NE.0) NN_L=NN_L+1
            ENDDO
            NNI_L=0
            DO J=NN+1,NN+NNI
               JJ=IBUF(J)
               IF (NODLOCAL(JJ).NE.0) NNI_L=NNI_L+1
            ENDDO
            NNA_L=0
            DO J=1,NNA
               JJ=IBUFA(J)
               IF (NODLOCAL(JJ).NE.0) NNA_L=NNA_L+1
            ENDDO
            FVSPMD(IFV)%NN_L=NN_L
            FVSPMD(IFV)%NNI_L=NNI_L
            FVSPMD(IFV)%NNA_L=NNA_L

            ALLOCATE(FVSPMD(IFV)%IBUF_L(2,NN_L+NNI_L),
     .               FVSPMD(IFV)%IBUFA_L(2,NNA_L))
            NN_L=0
            DO J=1,NN+NNI
               JJ=IBUF(J)
               IF (NODLOCAL(JJ).NE.0) THEN
                  NN_L=NN_L+1
                  FVSPMD(IFV)%IBUF_L(1,NN_L)=J
                  FVSPMD(IFV)%IBUF_L(2,NN_L)=NODLOCAL(JJ)
               ENDIF
            ENDDO
            NNA_L=0
            DO J=1,NNA
               JJ=IBUFA(J)
               IF (NODLOCAL(JJ).NE.0) THEN
                  NNA_L=NNA_L+1
                  FVSPMD(IFV)%IBUFA_L(1,NNA_L)=J
                  FVSPMD(IFV)%IBUFA_L(2,NNA_L)=NODLOCAL(JJ)
               ENDIF
            ENDDO
            DEALLOCATE(IBUF, IBUFA)
C Solides additionnels
            FVSPMD(IFV)%NSA=NBA
            FVSPMD(IFV)%NNSA=0
            FVSPMD(IFV)%NNSA_L=0
            FVSPMD(IFV)%NELSA=0
            IF (NBA.GT.0) THEN 
               IF (PROC.EQ.PMASTER) ALLOCATE(FVSPMD(IFV)%IXSA(8,NBA))
               KIA1=K2+MONVOL(K1-1+19)-1
               DO J=1,NB_NODE
                  ITAG(J)=0
                  REDIR(J)=0
               ENDDO
               DO J=1,NBA
                  JJ=MONVOL(KIA1-1+2*(J-1)+1)
                  DO K=1,8
                     KK=IXS(1+K,JJ)
                     ITAG(KK)=1
                  ENDDO
               ENDDO
               NNSA=0
               DO J=1,NB_NODE
                  IF (ITAG(J).EQ.1) THEN
                     NNSA=NNSA+1
                     REDIR(J)=NNSA
                     IBUFSA(NNSA)=J
                  ENDIF
               ENDDO
               FVSPMD(IFV)%NNSA=NNSA
               IF (PROC.EQ.PMASTER) THEN
                  DO J=1,NBA                            
                     JJ=MONVOL(KIA1-1+J)                
                     DO K=1,8                           
                        KK=IXS(1+K,JJ)                  
                        FVSPMD(IFV)%IXSA(K,J)=REDIR(KK) 
                     ENDDO                              
                  ENDDO
C
                  FVSPMD(IFV)%NELSA=NTG
                  ALLOCATE(FVSPMD(IFV)%ELEMSA(3,NTG))
                  KI2=KI1+NN+NNI
                  DO J=1,NTG
                     DO K=1,3
                        KK=MONVOL(KI2-1+3*(J-1)+K)
                        FVSPMD(IFV)%ELEMSA(K,J)=REDIR(KK)
                     ENDDO
                  ENDDO
                  DO J=1,FVDATA(IFV)%NNS
                     IF (FVDATA(IFV)%IFVNOD(1,J).EQ.2) THEN
                        JJ=FVDATA(IFV)%IFVNOD(2,J)
                        FVDATA(IFV)%IFVNOD(2,J)=REDIR(JJ)
                     ENDIF
                  ENDDO
               ENDIF                                
               NNSA_L=0
               DO J=1,NNSA
                  JJ=IBUFSA(J)
                  IF (NODLOCAL(JJ).NE.0) NNSA_L=NNSA_L+1
               ENDDO
               FVSPMD(IFV)%NNSA_L=NNSA_L
               ALLOCATE(FVSPMD(IFV)%IBUFSA_L(2,NNSA_L))
               NNSA_L=0
               DO J=1,NNSA
                  JJ=IBUFSA(J)
                  IF (NODLOCAL(JJ).NE.0) THEN
                     NNSA_L=NNSA_L+1
                     FVSPMD(IFV)%IBUFSA_L(1,NNSA_L)=J
                     FVSPMD(IFV)%IBUFSA_L(2,NNSA_L)=NODLOCAL(JJ)
                  ENDIF
               ENDDO
            ENDIF  
         ENDIF
         K1=K1+NIMV
      ENDDO
C
!     1d array
      DEALLOCATE( ITAG,REDIR )
      DEALLOCATE( IBUFSA )
! ---------------------------------
      RETURN
      END
