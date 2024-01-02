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
Chd|  ADMMAP3                       source/model/remesh/admmap3.F 
Chd|-- called by -----------
Chd|        ADMDIV                        source/model/remesh/admdiv.F  
Chd|        ADMINI                        source/model/remesh/admini.F  
Chd|        ADMREGUL                      source/model/remesh/admregul.F
Chd|-- calls ---------------
Chd|        ELBUFDEF_MOD                  ../common_source/modules/mat_elem/elbufdef_mod.F
Chd|        REMESH_MOD                    share/modules/remesh_mod.F    
Chd|====================================================================
      SUBROUTINE ADMMAP3(N      ,IXTG   ,X      ,IPARG  ,ELBUF_TAB,
     .                   IGEO   ,IPM    ,SH3TREE)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE REMESH_MOD
      USE ELBUFDEF_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "vect01_c.inc"
#include      "com01_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER N, IXTG(NIXTG,*), IPARG(NPARG,*),
     .        IGEO(NPROPGI,*), IPM(NPROPMI,*), SH3TREE(KSH3TREE,*)
      my_real
     .        X(3,*)
      TYPE(ELBUF_STRUCT_), DIMENSION(NGROUP), TARGET :: ELBUF_TAB
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER IB,M,N1,N2,N3,N4,IR,IS,IT,IL,NPTR,NPTS,NPTT,NLAY,
     .   I,J,K,II,JJ,I1,NG,NG1,NEL1,NFT1,MLW,NEL,
     .   MATLY,NUVAR,IVAR,ISTRA,IEXPAN,NPTM,KK(8),KK1(8)
      my_real
     .   NX,NY,NZ,
     .   STOT,X12,Y12,Z12,X13,Y13,Z13,S2WAKE(4)
      TYPE(G_BUFEL_) ,POINTER :: GBUFS,GBUFT     
      TYPE(L_BUFEL_) ,POINTER :: LBUFS,LBUFT    
      TYPE(BUF_LAY_) ,POINTER :: BUFLY     
C-----------------------------------------------
      STOT=ZERO
c
      DO IB=1,4
        M = SH3TREE(2,N)+IB-1
        N1 = IXTG(2,M)
        N2 = IXTG(3,M)
        N3 = IXTG(4,M)

        X12 = X(1,N2) - X(1,N1)
        Y12 = X(2,N2) - X(2,N1)
        Z12 = X(3,N2) - X(3,N1)

        X13 = X(1,N3) - X(1,N1)
        Y13 = X(2,N3) - X(2,N1)
        Z13 = X(3,N3) - X(3,N1)

        NX = Y12*Z13 - Z12*Y13
        NY = Z12*X13 - X12*Z13
        NZ = X12*Y13 - Y12*X13

        S2WAKE(IB)=SQRT(NX*NX+NY*NY+NZ*NZ)
        STOT = STOT+S2WAKE(IB)
      END DO
C-----------------------------------------------
      NG  = SH3TREE(4,N)
      MLW = IPARG(1,NG)
c
C     IF (MLW==0) GOTO 250
C---
      NEL  = IPARG(2,NG)
      NFT  = IPARG(3,NG)
      NPT  = IPARG(6,NG)
      ISTRA= IPARG(44,NG)
      IGTYP= IPARG(38,NG)
      IEXPAN=IPARG(49,NG)
      NPTM = MAX(1,NPT)
      I = N-NFT
c      
      GBUFS => ELBUF_TAB(NG)%GBUF
      NLAY  = ELBUF_TAB(NG)%NLAY
      NPTR  = ELBUF_TAB(NG)%NPTR
      NPTS  = ELBUF_TAB(NG)%NPTS
      NPTT  = ELBUF_TAB(NG)%NPTT
C
C---- T3
C
      DO IB=1,3

        M   = SH3TREE(2,N)+IB-1
        NG1 = SH3TREE(4,M)
        NEL1  = IPARG(2,NG1)
        NFT1  = IPARG(3,NG1)
        I1    = M-NFT1
        GBUFT => ELBUF_TAB(NG1)%GBUF
!
        DO K=1,8  ! length max of GBUF%G_STRA = 8
          KK(K)  = NEL *(K-1)
          KK1(K) = NEL1*(K-1)
        ENDDO
!
        GBUFT%FOR(KK1(1)+I1) = GBUFS%FOR(KK(1)+I)
        GBUFT%FOR(KK1(2)+I1) = GBUFS%FOR(KK(2)+I)
        GBUFT%FOR(KK1(3)+I1) = GBUFS%FOR(KK(3)+I)
        GBUFT%FOR(KK1(4)+I1) = GBUFS%FOR(KK(4)+I)
        GBUFT%FOR(KK1(5)+I1) = GBUFS%FOR(KK(5)+I)
c
        GBUFT%MOM(KK1(1)+I1) = GBUFS%MOM(KK(1)+I)
        GBUFT%MOM(KK1(2)+I1) = GBUFS%MOM(KK(2)+I)
        GBUFT%MOM(KK1(3)+I1) = GBUFS%MOM(KK(3)+I)
c
        GBUFT%EINT(I1)     = GBUFS%EINT(I)*S2WAKE(IB)/STOT
        GBUFT%EINT(I1+NEL1) = GBUFS%EINT(I+NEL)*S2WAKE(IB)/STOT
c
        GBUFT%THK(I1) = GBUFS%THK(I) !thk
        GBUFT%OFF(I1) = GBUFS%OFF(I)
c
        IF (GBUFT%G_EPSD > 0) THEN
          GBUFT%EPSD(I1) = GBUFS%EPSD(I) ! eps_dot
        ENDIF
c
        IF (ISTRA > 0) THEN
          DO K=1,8 ! deformations
            GBUFT%STRA(KK1(K)+I1) = GBUFS%STRA(KK(K)+I)
          END DO
        END IF
c
        IF (IEXPAN /= 0) THEN
          GBUFT%TEMP(I1) = GBUFS%TEMP(I)
        END IF
c
c       Local Stress
c
        DO IR=1,NPTR                                                    
          DO IS=1,NPTS                                                  
            DO IL=1,NLAY                                                
              DO IT=1,NPTT                                              
                LBUFT => ELBUF_TAB(NG1)%BUFLY(IL)%LBUF(IR,IS,IT)
                LBUFS => ELBUF_TAB(NG )%BUFLY(IL)%LBUF(IR,IS,IT)
                LBUFT%SIG(KK1(1)+I1) = LBUFS%SIG(KK(1)+I)
                LBUFT%SIG(KK1(2)+I1) = LBUFS%SIG(KK(2)+I)
                LBUFT%SIG(KK1(3)+I1) = LBUFS%SIG(KK(3)+I)
                LBUFT%SIG(KK1(4)+I1) = LBUFS%SIG(KK(4)+I)
                LBUFT%SIG(KK1(5)+I1) = LBUFS%SIG(KK(5)+I)
              END DO                                                     
            END DO                                                       
          END DO                                                        
        END DO                                                          
c
c       pla
c
        IF (GBUFT%G_PLA > 0) THEN 
          DO IL=1,NLAY
          DO IR=1,NPTR
          DO IS=1,NPTS
          DO IT=1,NPTT
            ELBUF_TAB(NG1)%BUFLY(IL)%LBUF(IR,IS,IT)%PLA(I1) = 
     .      ELBUF_TAB(NG)%BUFLY(IL)%LBUF(IR,IS,IT)%PLA(I)
          END DO       
          END DO       
          END DO       
          END DO       
        ENDIF
c
c       Uvar
c
        IF (MLW>=28 .AND. MLW/=32) THEN
          DO IL=1,NLAY
          DO IR=1,NPTR
          DO IS=1,NPTS
          DO IT=1,NPTT
            DO K=1,ELBUF_TAB(NG)%BUFLY(IL)%NVAR_MAT
              ELBUF_TAB(NG1)%BUFLY(IL)%MAT(IR,IS,IT)%VAR(NEL1*(K-1)+I1)=
     .        ELBUF_TAB(NG )%BUFLY(IL)%MAT(IR,IS,IT)%VAR(NEL*(K-1)+I)
            END DO
          END DO       
          END DO       
          END DO       
          END DO       
        END IF
c
c       sig moyen                                                           
!        IF (NLAY > 1) THEN
!          DO K=1,NLAY
!            DO J=1,5                                                      
!              ELBUF_TAB(NG1)%BUFLY(K)%SIGPT(I1) =            
!     .        ELBUF_TAB(NG)%BUFLY(K)%SIGPT(I)          
!            END DO                                                        
!          END DO 
!        ELSE
!          II = 5*(I1-1)                                                   
!          JJ = 5*(I-1)                                                    
!          DO K=1,NPT
!            DO J=1,5                                                      
!              ELBUF_TAB(NG1)%BUFLY(1)%SIGPT(II+I1) =          
!     .        ELBUF_TAB(NG)%BUFLY(1)%SIGPT(JJ+I)          
!            END DO                                                        
!          END DO 
!        ENDIF                                                          

c-----
      END DO  ! IB=1,3
c---------------------------------------------------
c     IB=4
c---------------------------------------------------
      M   = SH3TREE(2,N)+3
      NG1 = SH3TREE(4,M)

      NEL1 = IPARG(2,NG1)
      NFT1 = IPARG(3,NG1)
      GBUFT => ELBUF_TAB(NG1)%GBUF
      I1 = M-NFT1
!
      DO K=1,8  ! length max of GBUF%G_STRA = 8
        KK(K)  = NEL *(K-1)
        KK1(K) = NEL1*(K-1)
      ENDDO
!
      GBUFT%FOR(KK1(1)+I1) = GBUFS%FOR(KK(1)+I)  
      GBUFT%FOR(KK1(2)+I1) = GBUFS%FOR(KK(2)+I)  
      GBUFT%FOR(KK1(3)+I1) = GBUFS%FOR(KK(3)+I)  
      GBUFT%FOR(KK1(4)+I1) = GBUFS%FOR(KK(4)+I)  
      GBUFT%FOR(KK1(5)+I1) = GBUFS%FOR(KK(5)+I)  
c
      GBUFT%MOM(KK1(1)+I1) = GBUFS%MOM(KK(1)+I)  
      GBUFT%MOM(KK1(2)+I1) = GBUFS%MOM(KK(2)+I)  
      GBUFT%MOM(KK1(3)+I1) = GBUFS%MOM(KK(3)+I)  
c
      GBUFT%THK(I1) = GBUFS%THK(I) !thk  
      GBUFT%OFF(I1) = GBUFS%OFF(I)               
c

c     ener totale approximation
      GBUFT%EINT(I1)      = GBUFS%EINT(I)*S2WAKE(IB)/STOT  
      GBUFT%EINT(I1+NEL1) = GBUFS%EINT(I+NEL)*S2WAKE(IB)/STOT
c
c
      IF (GBUFT%G_EPSD > 0) THEN    
        GBUFT%EPSD(I1) = GBUFS%EPSD(I) ! eps_dot           
      ENDIF                                                
c
      IF (ISTRA > 0) THEN                      
        DO K=1,8 ! deformations                
          GBUFT%STRA(KK1(K)+I1) = GBUFS%STRA(KK(K)+I)  
        END DO                                 
      END IF                                   
c
      IF (IEXPAN/=0) THEN                                  
        GBUFT%TEMP(I1)=GBUFS%TEMP(I)                       
      END IF                                               
c
c     Local Stress
c
      IF (IGTYP == 1) THEN
        DO IR=1,NPTR                                                    
         DO IS=1,NPTS                                                  
          DO IL=1,NLAY                                                
            DO IT=1,NPTT                                              
              LBUFT => ELBUF_TAB(NG1)%BUFLY(IL)%LBUF(IR,IS,IT)
              LBUFS => ELBUF_TAB(NG )%BUFLY(IL)%LBUF(IR,IS,IT)
              LBUFT%SIG(KK1(1)+I1) = LBUFS%SIG(KK(1)+I)
              LBUFT%SIG(KK1(2)+I1) = LBUFS%SIG(KK(2)+I)
              LBUFT%SIG(KK1(3)+I1) = LBUFS%SIG(KK(3)+I)
              LBUFT%SIG(KK1(4)+I1) =-LBUFS%SIG(KK(4)+I)
              LBUFT%SIG(KK1(5)+I1) =-LBUFS%SIG(KK(5)+I)
            END DO                                                     
          END DO                                                       
         END DO                                                        
        END DO                                                          
      ELSE
        DO IR=1,NPTR                                                    
         DO IS=1,NPTS                                                  
          DO IL=1,NLAY                                                
            DO IT=1,NPTT                                              
              LBUFT => ELBUF_TAB(NG1)%BUFLY(IL)%LBUF(IR,IS,IT)
              LBUFS => ELBUF_TAB(NG )%BUFLY(IL)%LBUF(IR,IS,IT)
              LBUFT%SIG(KK1(1)+I1) = LBUFS%SIG(KK(1)+I)
              LBUFT%SIG(KK1(2)+I1) = LBUFS%SIG(KK(2)+I)
              LBUFT%SIG(KK1(3)+I1) = LBUFS%SIG(KK(3)+I)
              LBUFT%SIG(KK1(4)+I1) = LBUFS%SIG(KK(4)+I)
              LBUFT%SIG(KK1(5)+I1) = LBUFS%SIG(KK(5)+I)
            END DO                                                     
          END DO                                                       
         END DO                                                        
        END DO                                                          
      END IF
c
c     pla                                                    
c
      IF (GBUFT%G_PLA > 0) THEN                              
        DO IL=1,NLAY                                         
        DO IR=1,NPTR                                         
        DO IS=1,NPTS                                         
        DO IT=1,NPTT                                         
          ELBUF_TAB(NG1)%BUFLY(IL)%LBUF(IR,IS,IT)%PLA(I1) =   
     .    ELBUF_TAB(NG )%BUFLY(IL)%LBUF(IR,IS,IT)%PLA(I) 
        END DO                                                
        END DO                                                
        END DO                                                
        END DO                                                
      ENDIF                                                  
c
c     Uvar                                               
c
      IF (MLW>=28 .AND. MLW/=32) THEN                               
        DO IL=1,NLAY                                                    
        DO IR=1,NPTR                                                    
        DO IS=1,NPTS                                                    
        DO IT=1,NPTT                                                    
          DO K=1,ELBUF_TAB(NG)%BUFLY(IL)%NVAR_MAT                       
            ELBUF_TAB(NG1)%BUFLY(IL)%MAT(IR,IS,IT)%VAR(NEL1*(K-1)+I1)=  
     .      ELBUF_TAB(NG )%BUFLY(IL)%MAT(IR,IS,IT)%VAR(NEL*(K-1)+I)     
          END DO                                                        
        END DO                                                           
        END DO                                                           
        END DO                                                           
        END DO                                                           
      END IF                                                            

c
c     sig moyen                                                             
!      IF (NLAY > 1) THEN                                     
!        DO IL=1,NLAY                                          
!          BUFLY => ELBUF_TAB(NG)%BUFLY(IL)
!          DO K=1,5                                                        
!            BUFLY%SIGPT(I1+K) = BUFLY%SIGPT(I+K)                 
!          END DO                                                          
!        END DO                                               
!      ELSE                                                   
!        BUFLY => ELBUF_TAB(NG)%BUFLY(1)
!        II = 5*(I1-1)                                                     
!        JJ = 5*(I-1)                                                      
!        DO IT=1,NPT                                           
!          II = (IT-1)*NEL*5                                  
!          JJ = (IT-1)*NEL1*5                                  
!          DO K=1,5                                                        
!            BUFLY%SIGPT(II+K) = BUFLY%SIGPT(JJ+K)              
!          END DO                                                          
!        END DO                                               
!      ENDIF                                                            
c---------------------------------------------
c     reset source element variables
c---------------------------------------------
      GBUFS%OFF(I)    =-ABS(GBUFS%OFF(I))
!
      GBUFS%FOR(KK(1)+I) = ZERO
      GBUFS%FOR(KK(2)+I) = ZERO
      GBUFS%FOR(KK(3)+I) = ZERO
      GBUFS%FOR(KK(4)+I) = ZERO
      GBUFS%FOR(KK(5)+I) = ZERO
!
      GBUFS%MOM(KK(1)+I) = ZERO
      GBUFS%MOM(KK(2)+I) = ZERO
      GBUFS%MOM(KK(3)+I) = ZERO
      GBUFS%EINT(I) = ZERO
      GBUFS%EINT(I+NEL) = ZERO
      IF (GBUFS%G_EPSD > 0) GBUFS%EPSD(I) = ZERO
      IF (ISTRA > 0) THEN ! deformations
        DO K=1,8
          GBUFS%STRA(KK(K)+I) = ZERO
        END DO
      END IF
c
      DO IR=1,NPTR                                                    
        DO IS=1,NPTS                                                  
          DO IL=1,NLAY                                                
            DO IT=1,NPTT                                              
              DO K=1,5                                                
               ELBUF_TAB(NG)%BUFLY(IL)%LBUF(IR,IS,IT)%SIG(KK(K)+I)=ZERO  
              ENDDO                                                   
               ELBUF_TAB(NG)%BUFLY(IL)%LBUF(IR,IS,IT)%PLA(I)=ZERO  
            END DO                                                     
          END DO                                                       
        END DO                                                        
      END DO                                                          
c
c     sig moyen
c      IF (NLAY > 1) THEN
c        DO K=1,NLAY
c          BUFLY => ELBUF_TAB(NG)%BUFLY(K)
c          DO J=1,5
c            BUFLY%SIGPT(J)=ZERO
c          END DO
c        END DO
c      ELSE
c        BUFLY => ELBUF_TAB(NG)%BUFLY(1)
c        DO K=1,NPT
c          II = (K-1)*NEL*5                                  
c          DO J=1,5
c            BUFLY%SIGPT(II+J)=ZERO
c          END DO
c        END DO
c      ENDIF
C-----------
      RETURN
      END     
      
