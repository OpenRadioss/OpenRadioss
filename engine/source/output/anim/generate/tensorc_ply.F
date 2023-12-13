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
C available just with ply/xfem formulation 
Chd|====================================================================
Chd|  TENSORC_PLY                   source/output/anim/generate/tensorc_ply.F
Chd|-- called by -----------
Chd|        GENANI                        source/output/anim/generate/genani.F
Chd|-- calls ---------------
Chd|        SIGROTA                       source/output/anim/generate/sigrota.F
Chd|        SPMD_R4GET_PARTN              source/mpi/anim/spmd_r4get_partn.F
Chd|        WRITE_R_C                     ../common_source/tools/input_output/write_routtines.c
Chd|        ELBUFDEF_MOD                  ../common_source/modules/mat_elem/elbufdef_mod.F
Chd|        PLYXFEM_MOD                   share/modules/plyxfem_mod.F   
Chd|        STACK_MOD                     share/modules/stack_mod.F     
Chd|====================================================================
      SUBROUTINE TENSORC_PLY(IPLY,  NEL_PLY, ELBUF_TAB, IPARG,
     1                       ITENS, INVERT,  EL2FA, NBF,
     2                       TENS,  EPSDOT,  IADP,  NBF_L,
     3                       NBPART,IADG,    X,     IXC,
     4                       IGEO,  IXTG, NBF_PXFEMG, IPM  ,STACK)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE ELBUFDEF_MOD
      USE PLYXFEM_MOD
      USE STACK_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "param_c.inc"
#include      "task_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IPARG(NPARG,*),ITENS, INVERT(*),
     .   EL2FA(*),IXC(NIXC,*), IGEO(NPROPGI,*), 
     .   NBF,IADP(*),NBF_L,NBPART,IADG(NSPMD,*),
     .   IXTG(NIXTG,*),NEL_PLY,IPLY, NBF_PXFEMG,
     .   IPM(NPROPMI,*)
C     REAL
      my_real
     .   TENS(3,*),EPSDOT(6,*), X(3,*)
      TYPE (ELBUF_STRUCT_), DIMENSION(NGROUP), TARGET :: ELBUF_TAB
      TYPE (STACK_PLY) :: STACK
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
C     REAL
      my_real
     .   OFF, FAC, A1, A2, A3, THK, SIGE(MVSIZ,5)
      REAL R4(18)
      INTEGER I, NG, NEL, NFT, IAD, ITY, LFT, NPT, IR,IS,IPT,
     .        IADD, N, J, LLT, MLW, ISTRAIN,NPTR,NPTS,NPTT,NLAY,
     .        IPID, I1, I2, IAD2, NS1, NS2  , IALEL, ISTRE,
     .        NN1,NN2,NN3,NN4,NN5,NN6,NN7,NN8,NN9,NN10,NNI,N0,
     .        II, II_L, KK ,INC,IHBE,LEN ,IREP,BUF,IEXPAN,ILAYER,
     .        JJ(8)
      INTEGER IE,ISHPLYXFEM,IP,JPID,IPPID,IPLY0,IFLAG,ION,NPG,K,
     .        ELC,PLYELEMS(NUMELC),PLYS,IFAILURE,IVISC,NUVARV,
     .        MAT(MVSIZ),PID(MVSIZ),IGTYP,IADR,MATLY,IPMAT,
     .        IIGEO,IADI,ISUBSTACK,IPMAT_IPLY
      REAL WA(3,NBF_L)
      TYPE(BUF_LAY_) ,POINTER :: BUFLY
      TYPE(G_BUFEL_) ,POINTER :: GBUF
      TYPE(L_BUFEL_) ,POINTER :: LBUF
C-----------------------------------------------
!
      NEL_PLY = 0

      DO PLYS = 1,NPLYPART
         IPLY = INDX_PLY(PLYS)

         PLYELEMS=0
         DO I=1,PLYSHELL(IPLY)%PLYNUMSHELL
           IPT = PLYSHELL(IPLY)%SHELLIPT(I)
           ELC = PLYSHELL(IPLY)%SHID(I)
           PLYELEMS(ELC)=IPT
        ENDDO

        DO  J=1,18
           R4(J) = ZERO
        ENDDO
C
        IE = 0
        ILAYER = 0
        IFLAG = 0
        ION  = 0
        NPG = 0
C      
        NN1 = 1
        NN2 = NN1
        NN3 = NN2
        NN4 = NN3 + NUMELQ
        NN5 = NN4 + NUMELC
        NN6 = NN5 + NUMELTG
C
       
        DO 490 NG=1,NGROUP
         II = 0
C        IF(ANIM_K==0.AND.IPARG(8,NG)==1)GOTO 490
         MLW   =IPARG(1,NG)
         NEL   =IPARG(2,NG)
         NFT   =IPARG(3,NG)
         IAD   =IPARG(4,NG)
         ITY   =IPARG(5,NG)
         IHBE  = IPARG(23,NG)
         IGTYP =IPARG(38,NG)
         IFAILURE = IPARG(43,NG)
         ISTRAIN = IPARG(44,NG)
         ISHPLYXFEM = IPARG(50,NG)
         ISUBSTACK = IPARG(71,NG)
         LFT=1
         LLT=NEL
!
         DO I=1,8  ! length max of GBUF%G_STRA = 8
           JJ(I) = NEL*(I-1)
         ENDDO
!
         DO I=LFT,LLT          
           DO J=1,5            
             SIGE(I,J) = ZERO  
           ENDDO               
         ENDDO
!
C-----------------------------------------------
C       COQUES
C-----------------------------------------------
         IF(ITY==3.OR.ITY==7.AND.ISHPLYXFEM > 0)THEN
            GBUF => ELBUF_TAB(NG)%GBUF
            NPTR = ELBUF_TAB(NG)%NPTR
            NPTS = ELBUF_TAB(NG)%NPTS
            NPTT = ELBUF_TAB(NG)%NPTT
            NLAY = ELBUF_TAB(NG)%NLAY
            NPG  = NPTR*NPTS
            NPT  = NLAY*NPTT
C
          IF(ITY==3)THEN
             N0 = 0
             NNI = NN4
          ELSE
             N0 = NUMELC
             NNI = NN5
          ENDIF
C
C
          FAC   = ZERO
          A1    = ZERO
          A2    = ZERO
          A3    = ZERO
          ISTRE = 1
C          
C test sur un seul element du groupe          
C
           N = 1 + NFT
C
           DO  I=LFT,LLT
            N = I + NFT 
            ILAYER = PLYELEMS(N)
            IF(ILAYER > 0) IFLAG = ILAYER
           ENDDO
           IF(IFLAG == 0) GO TO 490
           ILAYER = IFLAG
           IFLAG = 1
C------------------------
C        STRESS
C------------------------
           IF(ITENS==1)THEN
             NS1 = 5
             NS2 = 3
             A1   = ONE
             A2   = ZERO
           ELSEIF(ITENS==2)THEN
             NS1 = 5
             NS2 = 3
             A1   = ZERO
             A2   = ONE
           ELSEIF(ITENS==3)THEN
             NS1 = 5
             NS2 = 3
             IF(MLW==1)THEN
               A1   = ONE
               A2   = SIX
             ELSEIF(MLW==2.OR.MLW==19.OR.
     .            MLW==15.OR.
     .            MLW==22.OR.MLW==25.OR.
     .            MLW==27.OR.MLW==32.OR.
     .            MLW>=28)THEN 
               A1   = ZERO
               A2   = ZERO
             ELSEIF(MLW==3.OR.MLW==23)THEN
               A1   = ZERO
               A2   = ZERO
             ENDIF
           ELSEIF(ITENS==4)THEN
             NS1 = 5
             NS2 = 3
             IF(MLW==1)THEN
               A1   = ZERO
               A2   = ZERO
             ELSEIF(MLW==2.OR.MLW==19.OR.
     .            MLW==15.OR.
     .            MLW==22.OR.MLW==25.OR.
     .            MLW==27.OR.MLW==32.OR.
     .            MLW>=28)THEN 
               A1   = ONE
               A2   = ZERO
             ELSEIF(MLW==3.OR.MLW==23)THEN
               A1   = ONE
               A2   = ZERO
             ENDIF
           ELSEIF(ITENS>=101.AND.ITENS<=200)THEN
             NS1 = 5
             NS2 = 3
             IF(MLW==1.OR.MLW==3.OR.MLW==23)THEN
               A1   = ONE
               A2   = ZERO
             ELSEIF(MLW==2.OR.MLW==19.OR.
     .            MLW==15.OR.
     .            MLW==22.OR.MLW==25.OR.
     .            MLW==27.OR.MLW==32.OR.
     .            MLW>=28)THEN 
               IPT = MIN(NPT,ITENS-100)
               A1   = ONE
               A2   = ZERO
               IF(IPT == IPLY ) ION = 1
             ENDIF
C------------------------
C        STRAIN
C------------------------
           ELSEIF(ITENS==5)THEN
             ISTRE  = 0
             NS1 = 8
             NS2 = 8
             IF(ISTRAIN==1)THEN
               A1   = ZERO
               A2   = ZERO
             ELSE
               A1   = ZERO
               A2   = ZERO
             ENDIF
           ELSEIF(ITENS==6)THEN
             ISTRE  = 0
             NS1 = 8
             NS2 = 8
             IF(ISTRAIN==1)THEN
               A1   = ZERO
               A2   = ZERO
             ELSE
               A1   = ZERO
               A2   = ZERO
             ENDIF
           ELSEIF(ITENS==7)THEN
             ISTRE  = 0
             NS1 = 8
             NS2 = 8
             IF(ISTRAIN==1)THEN
               A1   = ZERO
               A2   = ZERO
             ELSE
               A1   = ZERO
               A2   = ZERO
             ENDIF
           ELSEIF(ITENS==8)THEN
             ISTRE  = 0
             NS1 = 8
             NS2 = 8
             IF(ISTRAIN==1)THEN
               A1   = ZERO
               A2   = ZERO
             ELSE
               A1   = ZERO
               A2   = ZERO
             ENDIF
           ELSEIF(ITENS>=201.AND.ITENS<=300)THEN
             ISTRE  = 0
             NS1 = 8
             NS2 = 8
             IPT = MIN(NPT,ITENS - 200)
             IF(IPT == IPLY ) ION = 1
             IF(ISTRAIN==1.AND.NPT/=0)THEN
cc             IPT = ILAYER          
              A1   = ONE
              A2   = HALF*(((2*ILAYER-ONE)/NPT)-ONE)
             ELSE
               A1   = ZERO
               A2   = ZERO
             ENDIF
C------------------------
C        STRAIN RATE
C------------------------
           ELSEIF(ITENS==91)THEN
             ISTRE  = 2
             A1   = ZERO
             A2   = ZERO
           ELSEIF(ITENS==92)THEN
             ISTRE  = 2
             A1   = ZERO
             A2   = ZERO
           ELSEIF(ITENS==93)THEN
             ISTRE  = 2
             A1   = ZERO
             A2   = ZERO
           ELSEIF(ITENS==94)THEN
             ISTRE  = 2
             A1   = ZERO
             A2   = ZERO
           ELSEIF(ITENS>=301.AND.ITENS<=400)THEN
            IPT = MIN(NPT,ITENS - 300)
            IF(IPT == IPLY ) ION = 1
            IF(NPT/=0)THEN
             ISTRE  = 2
cc             IPT = ILAYER             
             A1   = ONE
             A2   = HALF*(((2*ILAYER-ONE)/NPT)-ONE)
                  ELSE
                   ISTRE  = 2
                   A1   = ZERO
             A2   = ZERO
                  ENDIF
           ENDIF
C
           IF(ISTRE==1)THEN
C------------------------
C          STRESS
C------------------------
             IF(ITY==3)THEN
               IPID = IXC(6,NFT+1)
             ELSE
               IPID = IXTG(5,NFT+1)
             ENDIF
             IREP = IGEO(6,IPID)
c------------
             IF (ITENS>=101.AND.ITENS<=200
     .         .AND.(MLW==25.OR.MLW==15).AND.IREP==1) THEN
             IVISC = 0
             NUVARV = 0
             IF(ITY==3)THEN
              DO I=1,NEL
          MAT(I)=IXC(1,NFT+I)
          PID(I)=IXC(6,NFT+I)
              END DO            
            ELSE
              DO I=1,NEL
          MAT(I)=IXTG(1,NFT+I)
          PID(I)=IXTG(5,NFT+I)
              END DO            
            END IF   
             IF(MLW == 25) THEN
               IF(IGTYP == 17)THEN
!!                  IIGEO   =  40 + 5*(ISUBSTACK - 1)   
!!                  IADI    = IGEO(IIGEO + 3,PID(1))   
                  IPMAT   = 2 + NPT  
                  IPMAT_IPLY   = IPMAT + NPT
! old stack organisation           IPMAT = 300
                  NUVARV = 0                                
                  DO N=1,NPT
                     IADR = (N-1)*NEL                       
                      DO I=1,NEL                          
                        MATLY   = STACK%IGEO(IPMAT+N,ISUBSTACK)
                        NUVARV =  MAX(NUVARV, IPM(225,MATLY)) 
                        IF(IPM(222,MATLY) > 0 ) IVISC = 1
                      END DO                                
                  END DO                                       
              END IF  
             ENDIF                     
               IF(ION == 1)THEN
                 CALL SIGROTA(LFT  ,LLT     ,NFT   ,ILAYER  ,NEL  ,
     2                        NS1     ,X     ,IXC     ,ELBUF_TAB(NG),
     3                        SIGE ,ITY     ,IXTG  ,IHBE    ,ISTRAIN ,
     4                        IVISC   )
                 DO I=LFT,LLT
                   N = I + NFT
                   ILAYER = PLYELEMS(N)
                   IF(ILAYER > 0) THEN 
                     IE = IE + 1
                      DO J = 1 , 3
                         R4(J) = SIGE(I,J)
                      ENDDO
cc               R4(3) = R4(3) * INVERT(EL2FA(NEL_PLY + IE))
                     TENS(1,EL2FA(NEL_PLY + IE)) = R4(1)
                     TENS(2,EL2FA(NEL_PLY + IE)) = R4(2)
                     TENS(3,EL2FA(NEL_PLY + IE)) = R4(3)
                   ENDIF
                 ENDDO
               ELSE ! ION = 0
                 DO I=LFT,LLT
                   N = I + NFT
                   ILAYER = PLYELEMS(N)
                   IF(ILAYER > 0) THEN 
                      IE = IE + 1
                      TENS(1,EL2FA(NEL_PLY + IE)) = ZERO
                      TENS(2,EL2FA(NEL_PLY + IE)) = ZERO
                      TENS(3,EL2FA(NEL_PLY + IE)) = ZERO
                   ENDIF
                 ENDDO
               ENDIF ! ION
c------------
             ELSEIF (ITENS>=101.AND.ITENS<=200
     .             .AND.MLW==25.AND.IREP==0) THEN
C    
              IF(ION == 1) THEN
                     DO I=LFT,LLT
                       DO J = 1 , 5
                         SIGE(I,J) = ZERO
                       ENDDO
                     ENDDO
                     DO I=LFT,LLT
                       N = I + NFT
                       ILAYER = PLYELEMS(N)
                       IF (ILAYER > 0) THEN
                         DO IR=1,NPTR                                             
                           DO IS=1,NPTS                                           
                             LBUF => ELBUF_TAB(NG)%BUFLY(ILAYER)%LBUF(IR,IS,1)    
                             DO J = 1 , 5
                               SIGE(I,J) = SIGE(I,J) + LBUF%SIG(JJ(J)+I)/NPG         
                             ENDDO
                           ENDDO
                         ENDDO
                       ENDIF  
                     ENDDO
c
                 DO I=LFT,LLT
                   N = I + NFT
                   ILAYER = PLYELEMS(N)
                   IF(ILAYER > 0) THEN
                     IE = IE  + 1
                     DO J = 1 , 3
                      R4(J) = SIGE(I,J)
                     ENDDO
cc                     R4(3) = R4(3) * INVERT(EL2FA(NNI+N))         
                     TENS(1,EL2FA(NEL_PLY + IE)) = R4(1)
                     TENS(2,EL2FA(NEL_PLY + IE)) = R4(2)
                     TENS(3,EL2FA(NEL_PLY + IE)) = R4(3)
                   ENDIF  
                ENDDO                
               ELSE
                 DO I=LFT,LLT
                   N = I + NFT
                   ILAYER = PLYELEMS(N)
                   IF(ILAYER > 0) THEN
                     IE = IE  + 1
cc                     R4(3) = R4(3) * INVERT(EL2FA(NEL_PLY + IE))
                     TENS(1,EL2FA(NEL_PLY + IE)) = ZERO
                     TENS(2,EL2FA(NEL_PLY + IE)) = ZERO
                     TENS(3,EL2FA(NEL_PLY + IE)) = ZERO
                   ENDIF  
                 ENDDO  
               ENDIF       
             ELSE
               DO I=LFT,LLT
                 N = I + NFT
                 ILAYER = PLYELEMS(N)
                 IF(ILAYER > 0) THEN
                  IE = IE + 1
                  DO J = 1,3
                    R4(J) = A1 * GBUF%FOR(JJ(J)+I) + A2 * GBUF%MOM(JJ(J)+I)
                  ENDDO
cc                R4(3) = R4(3) * INVERT(EL2FA(NEL_PLY + IE))
                  TENS(1,EL2FA(NEL_PLY + IE)) = R4(1)
                  TENS(2,EL2FA(NEL_PLY + IE)) = R4(2)
                  TENS(3,EL2FA(NEL_PLY + IE)) = R4(3)
                 ENDIF 
               ENDDO
             ENDIF
           ELSEIF (ISTRE == 0 .AND. GBUF%G_STRA > 0) THEN
C------------------------
C          STRAIN
C------------------------
             DO I=LFT,LLT
               N = I + NFT
               ILAYER = PLYELEMS(N)
               IF(ILAYER > 0) THEN
                   THK = GBUF%THK(I)
                   IF(ITENS/=6)THEN
                    DO J = 1 , 3
                       R4(J) = A1*GBUF%STRA(JJ(J)+I)+A2*GBUF%STRA(JJ(J)+I)*THK
                    ENDDO
                   ELSE
                    DO J = 1 , 3
                       R4(J) = GBUF%STRA(JJ(J)+I)
                    ENDDO
                   ENDIF
                   IE = IE + 1
cc                   R4(3) = R4(3) * INVERT(EL2FA(NEL_PLY + IE)) * HALF
                   TENS(1,EL2FA(NEL_PLY + IE)) = R4(1)
                   TENS(2,EL2FA(NEL_PLY + IE)) = R4(2)
                   TENS(3,EL2FA(NEL_PLY + IE)) = R4(3)
              ENDIF 
             ENDDO
           ELSEIF(ISTRE==2)THEN
C------------------------
C          STRAIN RATE
C------------------------
            DO I=LFT,LLT
               N = I + NFT
               ILAYER = PLYELEMS(N)
               IF(ILAYER > 0) THEN
                   THK = GBUF%THK(I)               
                   IF(ITENS/=92)THEN
                    DO J = 1 , 3
                     R4(J) = A1*EPSDOT(J,N+N0) + A2*EPSDOT(J+3,N+N0)*THK
                    ENDDO
                   ELSE
                    DO J = 1 , 3
                     R4(J) = EPSDOT(J+3,N+N0)
                    ENDDO
                   ENDIF
cc                   R4(3) = R4(3) * INVERT(EL2FA(NEL_PLY + IE)) * HALF
                   IE = IE + 1
                   TENS(1,EL2FA(NEL_PLY + IE)) = R4(1)
                   TENS(2,EL2FA(NEL_PLY + IE)) = R4(2)
                   TENS(3,EL2FA(NEL_PLY + IE)) = R4(3)
               ENDIF    
            ENDDO
           ENDIF
C-----------------------------------------------
         ELSE
         ENDIF
 490    CONTINUE
C----------------------------------------------
        IF(IFLAG  > 0 ) THEN
         IF (NSPMD == 1)THEN
          DO I=1,IE
            N = EL2FA(NEL_PLY + I)
            R4(1) = TENS(1,N)
            R4(2) = TENS(2,N)
            R4(3) = TENS(3,N)
            CALL WRITE_R_C(R4,3)
          ENDDO
         ELSE
          DO I=1,IE
            N = EL2FA(NEL_PLY + I)
            WA(1,I+NEL_PLY) = TENS(1,N)
            WA(2,I+NEL_PLY) = TENS(2,N)
            WA(3,I+NEL_PLY) = TENS(3,N)
          ENDDO
         ENDIF
        ENDIF
C
        NEL_PLY = NEL_PLY + PLYSHELL(IPLY)%PLYNUMSHELL
      ENDDO

      IF (NSPMD > 1)THEN
         IF(ISPMD==0) THEN
           BUF = NBF_PXFEMG*3
         ELSE
           BUF = 1
         ENDIF
         CALL SPMD_R4GET_PARTN(3,3*NBF_L,NPLYPART,IADG,WA,BUF)
      ENDIF

      RETURN
      END
