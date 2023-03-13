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
Chd|  TENSOR0                       source/output/anim/generate/tensor0.F
Chd|-- called by -----------
Chd|        GENANI                        source/output/anim/generate/genani.F
Chd|-- calls ---------------
Chd|        INITBUF                       share/resol/initbuf.F         
Chd|        SPMD_R4GET_PARTN              source/mpi/anim/spmd_r4get_partn.F
Chd|        WRITE_R_C                     source/output/tools/sortie_c.c
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|        INITBUF_MOD                   share/resol/initbuf.F         
Chd|====================================================================
      SUBROUTINE TENSOR0(ELBUF_TAB,IPARG   ,ITENS   ,PM      ,EL2FA   ,
     2                   NBF      ,TENS    ,EPSDOT           ,IADP    ,
     3                   NBPART   ,X       ,IADG    ,IPART   ,IPARTSP ,
     4                   IPM      )
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
#include      "com01_c.inc"
#include      "sphcom.inc"
#include      "param_c.inc"
#include      "scr17_c.inc"
#include      "task_c.inc"
#include      "spmd_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
C     REAL
      my_real
     .   TENS(6,*),EPSDOT(6,*),PM(NPROPM,*),X(3,*)
      INTEGER IPARG(NPARG,*),ITENS, EL2FA(*),IADG(NSPMD,*),
     .   NBF,IADP(*),NBPART,IPART(LIPART1,*),IPARTSP(*),IPM(NPROPMI,*)
      TYPE (ELBUF_STRUCT_), DIMENSION(NGROUP), TARGET :: ELBUF_TAB
C-----------------------------------------------
C     REAL
      my_real
     .   G11,G22,G33,G12,G21,G23,G32,G13,G31,
     .   L11,L22,L33,L12,L21,L23,L32,L13,L31,
     .   S11,S22,S33,S12,S21,S23,S32,S13,S31
      REAL R4(18),
     .   WA(6*NBF)

      INTEGER I,II, NG, NEL, IPT, MT1,IADD, N, J, MLW,
     .        NN1,NN2,IPRT,BUF, ISTRAIN, NUVAR, NUVARR,JJ(6)
      TYPE(G_BUFEL_) ,POINTER :: GBUF     
      TYPE(L_BUFEL_) ,POINTER :: LBUF     
C=======================================================================
       DO 5 J=1,18
           R4(J) = ZERO
 5     CONTINUE
C
      NN1 = 1
      NN2 = NN1 + (NUMSPH+MAXPJET)
C
       DO 490 NG=1,NGROUP
          CALL INITBUF (IPARG    ,NG      ,                    
     2          MLW     ,NEL     ,NFT     ,IAD     ,ITY     ,  
     3          NPT     ,JALE    ,ISMSTR  ,JEUL    ,JTUR    ,  
     4          JTHE    ,JLAG    ,JMULT   ,JHBE    ,JIVF    ,  
     5          NVAUX   ,JPOR    ,JCVT    ,JCLOSE  ,JPLASOL ,  
     6          IREP    ,IINT    ,IGTYP   ,ISRAT   ,ISROT   ,  
     7          ICSEN   ,ISORTH  ,ISORTHG ,IFAILURE,JSMS    )
        LFT=1
        LLT=NEL
!
        DO I=1,6
          JJ(I) = NEL*(I-1)
        ENDDO
!
        IF (ITY == 51) THEN
C-----------------------------------------------
C         PARTICULES SPH.
C-----------------------------------------------
          GBUF => ELBUF_TAB(NG)%GBUF
          LBUF => ELBUF_TAB(NG)%BUFLY(1)%LBUF(1,1,1)
          IPRT=IPARTSP(1 + NFT)
          MT1 =IPART(1,IPRT)
          IF(ITENS == 1)THEN
C-----------------------------------------------
C          STRESS
C-----------------------------------------------
           DO I=LFT,LLT
             N  = I + NFT
             IF(EL2FA(NN1+N)/=0)THEN
               TENS(1,EL2FA(NN1+N)) = GBUF%SIG(JJ(1) + I)
               TENS(2,EL2FA(NN1+N)) = GBUF%SIG(JJ(2) + I)
               TENS(3,EL2FA(NN1+N)) = GBUF%SIG(JJ(3) + I)
               TENS(4,EL2FA(NN1+N)) = GBUF%SIG(JJ(4) + I)
               TENS(5,EL2FA(NN1+N)) = GBUF%SIG(JJ(5) + I)
               TENS(6,EL2FA(NN1+N)) = GBUF%SIG(JJ(6) + I)
             ENDIF
           ENDDO
          ELSEIF(ITENS == 4.AND.MLW == 24.
     .                      AND.NINT(PM(56,MT1)) == 1)THEN
C-----------------------------------------------
C         CRACKS
C-----------------------------------------------
            IF(ISORTH==0)THEN
               DO I=LFT,LLT
                 N  = I + NFT
                 TENS(1,EL2FA(NN1+N)) =  LBUF%DGLO(JJ(1) + I)
                 TENS(2,EL2FA(NN1+N)) =  LBUF%DGLO(JJ(2) + I)
                 TENS(3,EL2FA(NN1+N)) =  LBUF%DGLO(JJ(3) + I)			   
                 TENS(4,EL2FA(NN1+N)) =  LBUF%DGLO(JJ(4) + I)
                 TENS(5,EL2FA(NN1+N)) =  LBUF%DGLO(JJ(5) + I)
                 TENS(6,EL2FA(NN1+N)) =  LBUF%DGLO(JJ(6) + I)
               ENDDO
            ELSE
              DO I=LFT,LLT                                              
                N  = I + NFT
                L11 = LBUF%DGLO(JJ(1) + I)	       
                L21 = LBUF%DGLO(JJ(2) + I)	       
                L31 = LBUF%DGLO(JJ(3) + I)	       
                L12 = LBUF%DGLO(JJ(4) + I)	
                L22 = LBUF%DGLO(JJ(5) + I)	
                L32 = LBUF%DGLO(JJ(6) + I)
                L13 = L21*L32-L31*L22
                L23 = L31*L12-L11*L32
                L33 = L11*L22-L21*L12
                G11 = GBUF%GAMA(JJ(1) + I)
                G21 = GBUF%GAMA(JJ(2) + I)
                G31 = GBUF%GAMA(JJ(3) + I)
                G12 = GBUF%GAMA(JJ(4) + I)
                G22 = GBUF%GAMA(JJ(5) + I)
                G32 = GBUF%GAMA(JJ(6) + I)
                G13 = G21*G32-G31*G22
                G23 = G31*G12-G11*G32
                G33 = G11*G22-G21*G12
                S11 =L11*G11+L12*G12+L13*G13 
                S12 =L11*G21+L12*G22+L13*G23 
                S13 =L11*G31+L12*G32+L13*G33
                S21 =L12*G11+L22*G12+L23*G13
                S22 =L12*G21+L22*G22+L23*G23
                S23 =L12*G31+L22*G32+L23*G33 
                S31 =L13*G11+L23*G12+L33*G13
                S32 =L13*G21+L23*G22+L33*G23
                S33 =L13*G31+L23*G32+L33*G33
                TENS(1,EL2FA(NN1+N)) = G11*S11+G12*S21+G13*S31  
                TENS(2,EL2FA(NN1+N)) = G21*S12+G22*S22+G23*S32  
                TENS(3,EL2FA(NN1+N)) = G31*S13+G32*S23+G33*S33  
                TENS(4,EL2FA(NN1+N)) = G11*S12+G12*S22+G13*S32  
                TENS(5,EL2FA(NN1+N)) = G21*S13+G22*S23+G23*S33  
                TENS(6,EL2FA(NN1+N)) = G11*S13+G12*S23+G13*S33  
              ENDDO                                                    
            END IF            
          ELSEIF(ITENS == 2)THEN
C-----------------------------------------------
C          STRAIN
C-----------------------------------------------
            IPRT=IPARTSP(1 + NFT)
            MT1 =IPART(1,IPRT)
            ISTRAIN= IPARG(44,NG)    
            NUVAR  = IPM(8,MT1)
            NUVARR = IPM(221,MT1) 
            IF (MLW>=28.AND.MLW/=49)THEN
              DO I=LFT,LLT
               N = I + NFT
               TENS(1,EL2FA(NN1+N)) =  LBUF%STRA(JJ(1) + I)
               TENS(2,EL2FA(NN1+N)) =  LBUF%STRA(JJ(2) + I)
               TENS(3,EL2FA(NN1+N)) =  LBUF%STRA(JJ(3) + I)
               TENS(4,EL2FA(NN1+N)) =  LBUF%STRA(JJ(4) + I)*HALF
               TENS(5,EL2FA(NN1+N)) =  LBUF%STRA(JJ(5) + I)*HALF
               TENS(6,EL2FA(NN1+N)) =  LBUF%STRA(JJ(6) + I)*HALF
              ENDDO   
            ELSEIF(MLW == 14)THEN
              DO I=LFT,LLT
               N = I + NFT                                    
               TENS(1,EL2FA(NN1+N)) = LBUF%EPE(JJ(1) + I)
               TENS(2,EL2FA(NN1+N)) = LBUF%EPE(JJ(2) + I)
               TENS(3,EL2FA(NN1+N)) = LBUF%EPE(JJ(3) + I)	  
               TENS(4,EL2FA(NN1+N)) =  ZERO
               TENS(5,EL2FA(NN1+N)) =  ZERO
               TENS(6,EL2FA(NN1+N)) =  ZERO
              ENDDO
            ELSEIF(MLW == 24)THEN
              DO I=LFT,LLT
               N  = I + NFT                                    
               TENS(1,EL2FA(NN1+N)) = LBUF%STRA(JJ(1) + I)
               TENS(2,EL2FA(NN1+N)) = LBUF%STRA(JJ(2) + I)
               TENS(3,EL2FA(NN1+N)) = LBUF%STRA(JJ(3) + I)			
               TENS(4,EL2FA(NN1+N)) = LBUF%STRA(JJ(4) + I)*HALF
               TENS(5,EL2FA(NN1+N)) = LBUF%STRA(JJ(5) + I)*HALF
               TENS(6,EL2FA(NN1+N)) = LBUF%STRA(JJ(6) + I)*HALF   
              ENDDO
            ELSEIF(ISTRAIN == 1)THEN
              IF(MLW/=14.AND.MLW/=24.AND.MLW<28.OR.
     .            MLW == 49)THEN               
                DO I=LFT,LLT
                  N  = I + NFT  
                  TENS(1,EL2FA(NN1+N)) = LBUF%STRA(JJ(1) + I)
                  TENS(2,EL2FA(NN1+N)) = LBUF%STRA(JJ(2) + I)	       
                  TENS(3,EL2FA(NN1+N)) = LBUF%STRA(JJ(3) + I)			  
                  TENS(4,EL2FA(NN1+N)) = LBUF%STRA(JJ(4) + I)*HALF   
                  TENS(5,EL2FA(NN1+N)) = LBUF%STRA(JJ(5) + I)*HALF   
                  TENS(6,EL2FA(NN1+N)) = LBUF%STRA(JJ(6) + I)*HALF   
                ENDDO
             ELSE
               DO I=LFT,LLT
                 TENS(1,EL2FA(NN1+N)) =  ZERO
                 TENS(2,EL2FA(NN1+N)) =  ZERO
                 TENS(3,EL2FA(NN1+N)) =  ZERO                      
                 TENS(4,EL2FA(NN1+N)) =  ZERO
                 TENS(5,EL2FA(NN1+N)) =  ZERO
                 TENS(6,EL2FA(NN1+N)) =  ZERO
               ENDDO
             ENDIF
            ENDIF
          ELSEIF (ITENS == 5) THEN
C-----------------------------------------------
C          PLASTIC STRAIN TENSOR
C-----------------------------------------------
            IF (MLW == 24) THEN
              DO I=LFT,LLT
               N  = I + NFT                                    
               TENS(1,EL2FA(NN1+N)) = LBUF%PLA(JJ(1) + I + NEL)
               TENS(2,EL2FA(NN1+N)) = LBUF%PLA(JJ(2) + I + NEL)
               TENS(3,EL2FA(NN1+N)) = LBUF%PLA(JJ(3) + I + NEL)			
               TENS(4,EL2FA(NN1+N)) = LBUF%PLA(JJ(4) + I + NEL)*HALF
               TENS(5,EL2FA(NN1+N)) = LBUF%PLA(JJ(5) + I + NEL)*HALF
               TENS(6,EL2FA(NN1+N)) = LBUF%PLA(JJ(6) + I + NEL)*HALF   
              ENDDO
            ENDIF ! IF (MLW == 24)
c-----------
          ELSE
C-----------------------------------------------
C          
C-----------------------------------------------
           DO I=LFT,LLT
             N = I + NFT
             IF(EL2FA(NN1+N)/=0)THEN
               TENS(1,EL2FA(NN1+N)) = ZERO
               TENS(2,EL2FA(NN1+N)) = ZERO
               TENS(3,EL2FA(NN1+N)) = ZERO
               TENS(4,EL2FA(NN1+N)) = ZERO
               TENS(5,EL2FA(NN1+N)) = ZERO
               TENS(6,EL2FA(NN1+N)) = ZERO               
             ENDIF
           ENDDO
          ENDIF
C-----------------------------------------------
        ELSE
        ENDIF
 490   CONTINUE
 500  CONTINUE
C-----------------------------------------------
      IF (NSPMD == 1)THEN
        DO N=1,NBF
          R4(1) = TENS(1,N)
          R4(2) = TENS(2,N)
          R4(3) = TENS(3,N)
          R4(4) = TENS(4,N)
          R4(5) = TENS(5,N)
          R4(6) = TENS(6,N)
          CALL WRITE_R_C(R4,6)
        ENDDO
      ELSE
        DO N = 1, NBF
          WA(6*N-5) = TENS(1,N)
          WA(6*N-4) = TENS(2,N)
          WA(6*N-3) = TENS(3,N)
          WA(6*N-2) = TENS(4,N)
          WA(6*N-1) = TENS(5,N)
          WA(6*N  ) = TENS(6,N)
        ENDDO

        IF(ISPMD == 0) THEN
           BUF = NUMSPHG*6
        ELSE
           BUF = 1
        ENDIF
        CALL SPMD_R4GET_PARTN(6,6*NBF,NBPART,IADG,WA,BUF)
      ENDIF
C
C-------------
 600  CONTINUE
C-------------
      RETURN
      END
