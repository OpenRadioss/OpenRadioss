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
Chd|  DIM_TSHEDG                    source/elements/thickshell/solidec/scdtdc3.F
Chd|-- called by -----------
Chd|        RESOL_INIT                    source/engine/resol_init.F    
Chd|-- calls ---------------
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|====================================================================
      SUBROUTINE DIM_TSHEDG(ELBUF_STR,NEDG, IXS ,IPARG )
C=======================================================================
      USE ELBUFDEF_MOD     
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   A n a l y s e   M o d u l e
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NEDG
      INTEGER IXS(NIXS,*),IPARG(NPARG,NGROUP)
      TYPE(ELBUF_STRUCT_),DIMENSION(NGROUP), TARGET :: ELBUF_STR
C     REAL
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
#include      "com01_c.inc"
#include      "com04_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,NG, NEL, NFT,N, ITY,JHBE,IGTYP,ICSTR,ISOLNOD
      INTEGER ITAG(NUMNOD),N1,N2,NC(8,MVSIZ)
      TYPE(G_BUFEL_) ,POINTER :: GBUF     
C     REAL
C-------
      ITAG(1:NUMNOD)=0
      NEDG = 0
      DO NG=1,NGROUP
       NEL=IPARG(2,NG)
       NFT=IPARG(3,NG)
       ITY=IPARG(5,NG)
       ICSTR= IPARG(17,NG)
       JHBE = IPARG(23,NG)
       IGTYP  = IPARG(38,NG)
       ISOLNOD= IPARG(28,NG)
       IF (IPARG(8,NG)==1) CYCLE                                 
       IF (ITY /= 1) CYCLE
       IF (IGTYP == 20.OR.IGTYP == 21.OR.IGTYP == 22)THEN
         GBUF => ELBUF_STR(NG)%GBUF
C--------only linear for the moment       
        IF(ISOLNOD==6)THEN
          DO I=1,NEL
 	        N = NFT+I
            NC(1:3,I)=IXS(2:4,N)
            NC(4:6,I)=IXS(6:8,N)
          ENDDO
            DO I=1,NEL
              IF (GBUF%IDT_TSH(I)<=0) CYCLE
 	          N1 = NC(1,I)
 	          N2 = NC(4,I)
	          IF (ITAG(N1)==0.AND.ITAG(N2)==0) THEN
	            NEDG = NEDG + 1
	            ITAG(N1)=NEDG
	            ITAG(N2)=NEDG
              END IF
 	          N1 = NC(2,I)
 	          N2 = NC(5,I)
	          IF (ITAG(N1)==0.AND.ITAG(N2)==0) THEN
	            NEDG = NEDG + 1
	            ITAG(N1)=NEDG
	            ITAG(N2)=NEDG
              END IF
 	          N1 = NC(3,I)
 	          N2 = NC(6,I)
	          IF (ITAG(N1)==0.AND.ITAG(N2)==0) THEN
	            NEDG = NEDG + 1
	            ITAG(N1)=NEDG
	            ITAG(N2)=NEDG
              END IF
            ENDDO
        ELSEIF(ISOLNOD==8)THEN
          DO I=1,NEL
 	        N = NFT+I
            NC(1:8,I)=IXS(2:9,N)
          ENDDO
          IF (JHBE==14) THEN
             SELECT CASE (ICSTR)
              CASE(100)                                             
                DO I=1,NEL
                  IF (GBUF%IDT_TSH(I)<=0) CYCLE
 	              N1 = NC(1,I)
 	              N2 = NC(4,I)
	              IF (ITAG(N1)==0.AND.ITAG(N2)==0) THEN
	                NEDG = NEDG + 1
	                ITAG(N1)=NEDG
	                ITAG(N2)=NEDG
                  END IF
 	              N1 = NC(2,I)
 	              N2 = NC(3,I)
	              IF (ITAG(N1)==0.AND.ITAG(N2)==0) THEN
	                NEDG = NEDG + 1
	                ITAG(N1)=NEDG
	                ITAG(N2)=NEDG
                  END IF
 	              N1 = NC(5,I)
 	              N2 = NC(8,I)
	              IF (ITAG(N1)==0.AND.ITAG(N2)==0) THEN
	                NEDG = NEDG + 1
	                ITAG(N1)=NEDG
	                ITAG(N2)=NEDG
                  END IF
 	              N1 = NC(6,I)
 	              N2 = NC(7,I)
	              IF (ITAG(N1)==0.AND.ITAG(N2)==0) THEN
	                NEDG = NEDG + 1
	                ITAG(N1)=NEDG
	                ITAG(N2)=NEDG
                  END IF
                ENDDO
              CASE(10)                                          
                DO I=1,NEL
                  IF (GBUF%IDT_TSH(I)<=0) CYCLE
 	              N1 = NC(1,I)
 	              N2 = NC(5,I)
	              IF (ITAG(N1)==0.AND.ITAG(N2)==0) THEN
	                NEDG = NEDG + 1
	                ITAG(N1)=NEDG
	                ITAG(N2)=NEDG
                  END IF
 	              N1 = NC(2,I)
 	              N2 = NC(6,I)
	              IF (ITAG(N1)==0.AND.ITAG(N2)==0) THEN
	                NEDG = NEDG + 1
	                ITAG(N1)=NEDG
	                ITAG(N2)=NEDG
                  END IF
 	              N1 = NC(3,I)
 	              N2 = NC(7,I)
	              IF (ITAG(N1)==0.AND.ITAG(N2)==0) THEN
	                NEDG = NEDG + 1
	                ITAG(N1)=NEDG
	                ITAG(N2)=NEDG
                  END IF
 	              N1 = NC(4,I)
 	              N2 = NC(8,I)
	              IF (ITAG(N1)==0.AND.ITAG(N2)==0) THEN
	                NEDG = NEDG + 1
	                ITAG(N1)=NEDG
	                ITAG(N2)=NEDG
                  END IF
                ENDDO
              CASE(1)                                          
                DO I=1,NEL
                  IF (GBUF%IDT_TSH(I)<=0) CYCLE
 	              N1 = NC(1,I)
 	              N2 = NC(2,I)
	              IF (ITAG(N1)==0.AND.ITAG(N2)==0) THEN
	                NEDG = NEDG + 1
	                ITAG(N1)=NEDG
	                ITAG(N2)=NEDG
                  END IF
 	              N1 = NC(4,I)
 	              N2 = NC(3,I)
	              IF (ITAG(N1)==0.AND.ITAG(N2)==0) THEN
	                NEDG = NEDG + 1
	                ITAG(N1)=NEDG
	                ITAG(N2)=NEDG
                  END IF
 	              N1 = NC(5,I)
 	              N2 = NC(6,I)
	              IF (ITAG(N1)==0.AND.ITAG(N2)==0) THEN
	                NEDG = NEDG + 1
	                ITAG(N1)=NEDG
	                ITAG(N2)=NEDG
                  END IF
 	              N1 = NC(8,I)
 	              N2 = NC(7,I)
	              IF (ITAG(N1)==0.AND.ITAG(N2)==0) THEN
	                NEDG = NEDG + 1
	                ITAG(N1)=NEDG
	                ITAG(N2)=NEDG
                  END IF
                ENDDO
             END SELECT 
          ELSEIF (JHBE==15) THEN
            DO I=1,NEL
              IF (GBUF%IDT_TSH(I)<=0) CYCLE
 	          N1 = NC(1,I)
 	          N2 = NC(5,I)
	          IF (ITAG(N1)==0.AND.ITAG(N2)==0) THEN
	            NEDG = NEDG + 1
	            ITAG(N1)=NEDG
	            ITAG(N2)=NEDG
              END IF
 	          N1 = NC(2,I)
 	          N2 = NC(6,I)
	          IF (ITAG(N1)==0.AND.ITAG(N2)==0) THEN
	            NEDG = NEDG + 1
	            ITAG(N1)=NEDG
	            ITAG(N2)=NEDG
              END IF
 	          N1 = NC(3,I)
 	          N2 = NC(7,I)
	          IF (ITAG(N1)==0.AND.ITAG(N2)==0) THEN
	            NEDG = NEDG + 1
	            ITAG(N1)=NEDG
	            ITAG(N2)=NEDG
              END IF
 	          N1 = NC(4,I)
 	          N2 = NC(8,I)
	          IF (ITAG(N1)==0.AND.ITAG(N2)==0) THEN
	            NEDG = NEDG + 1
	            ITAG(N1)=NEDG
	            ITAG(N2)=NEDG
              END IF
            ENDDO
          END IF
        END IF
       END IF
      ENDDO 
C
      RETURN
      END SUBROUTINE DIM_TSHEDG
Chd|====================================================================
Chd|  IND_TSHEDG                    source/elements/thickshell/solidec/scdtdc3.F
Chd|-- called by -----------
Chd|        RESOL_INIT                    source/engine/resol_init.F    
Chd|-- calls ---------------
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|====================================================================
      SUBROUTINE IND_TSHEDG(ELBUF_STR,IENUNL,IXS ,IPARG )
C=======================================================================
      USE ELBUFDEF_MOD     
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   A n a l y s e   M o d u l e
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IENUNL(2,*),IXS(NIXS,*),IPARG(NPARG,NGROUP)
      TYPE(ELBUF_STRUCT_),DIMENSION(NGROUP), TARGET :: ELBUF_STR
C     REAL
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
#include      "com01_c.inc"
#include      "com04_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,NG, NEL, NFT,N, ITY,JHBE,IGTYP,ICSTR,ISOLNOD
      INTEGER ITAG(NUMNOD),N1,N2,NC(8,MVSIZ),NEDG
      TYPE(G_BUFEL_) ,POINTER :: GBUF     
C     REAL
C------ -
      ITAG(1:NUMNOD)=0
      NEDG = 0
      DO NG=1,NGROUP
       NEL=IPARG(2,NG)
       NFT=IPARG(3,NG)
       ITY=IPARG(5,NG)
       ICSTR= IPARG(17,NG)
       JHBE = IPARG(23,NG)
       IGTYP  = IPARG(38,NG)
       ISOLNOD= IPARG(28,NG)
       IF (IPARG(8,NG)==1) CYCLE                                 
       IF (ITY /= 1) CYCLE
       IF (IGTYP == 20.OR.IGTYP == 21.OR.IGTYP == 22)THEN
         GBUF => ELBUF_STR(NG)%GBUF
C--------only linear for the moment       
        IF(ISOLNOD==6)THEN
          DO I=1,NEL
 	        N = NFT+I
            NC(1:3,I)=IXS(2:4,N)
            NC(4:6,I)=IXS(6:8,N)
          ENDDO
           DO I=1,NEL
             IF (GBUF%IDT_TSH(I)<=0) CYCLE
 	         N1 = NC(1,I)
 	         N2 = NC(4,I)
	         IF (ITAG(N1)==0.AND.ITAG(N2)==0) THEN
	           NEDG = NEDG + 1
               IENUNL(1,NEDG) = N1
               IENUNL(2,NEDG) = N2
	           ITAG(N1)=NEDG
	           ITAG(N2)=NEDG
             END IF
 	         N1 = NC(2,I)
 	         N2 = NC(5,I)
	         IF (ITAG(N1)==0.AND.ITAG(N2)==0) THEN
	           NEDG = NEDG + 1
               IENUNL(1,NEDG) = N1
               IENUNL(2,NEDG) = N2
	           ITAG(N1)=NEDG
	           ITAG(N2)=NEDG
             END IF
 	         N1 = NC(3,I)
 	         N2 = NC(6,I)
	         IF (ITAG(N1)==0.AND.ITAG(N2)==0) THEN
	           NEDG = NEDG + 1
               IENUNL(1,NEDG) = N1
               IENUNL(2,NEDG) = N2
	           ITAG(N1)=NEDG
	           ITAG(N2)=NEDG
             END IF
           ENDDO
        ELSEIF(ISOLNOD==8)THEN
          DO I=1,NEL
 	        N = NFT+I
            NC(1:8,I)=IXS(2:9,N)
          ENDDO
          IF (JHBE==14) THEN
             SELECT CASE (ICSTR)
              CASE(100)                                             
                DO I=1,NEL
                  IF (GBUF%IDT_TSH(I)<=0) CYCLE
 	              N1 = NC(1,I)
 	              N2 = NC(4,I)
	              IF (ITAG(N1)==0.AND.ITAG(N2)==0) THEN
	                NEDG = NEDG + 1
                    IENUNL(1,NEDG) = N1
                    IENUNL(2,NEDG) = N2
	                ITAG(N1)=NEDG
	                ITAG(N2)=NEDG
                  END IF
 	              N1 = NC(2,I)
 	              N2 = NC(3,I)
	              IF (ITAG(N1)==0.AND.ITAG(N2)==0) THEN
	                NEDG = NEDG + 1
                    IENUNL(1,NEDG) = N1
                    IENUNL(2,NEDG) = N2
	                ITAG(N1)=NEDG
	                ITAG(N2)=NEDG
                  END IF
 	              N1 = NC(5,I)
 	              N2 = NC(8,I)
	              IF (ITAG(N1)==0.AND.ITAG(N2)==0) THEN
	                NEDG = NEDG + 1
                    IENUNL(1,NEDG) = N1
                    IENUNL(2,NEDG) = N2
	                ITAG(N1)=NEDG
	                ITAG(N2)=NEDG
                  END IF
 	              N1 = NC(6,I)
 	              N2 = NC(7,I)
	              IF (ITAG(N1)==0.AND.ITAG(N2)==0) THEN
	                NEDG = NEDG + 1
                    IENUNL(1,NEDG) = N1
                    IENUNL(2,NEDG) = N2
	                ITAG(N1)=NEDG
	                ITAG(N2)=NEDG
                  END IF
                ENDDO
              CASE(10)                                          
                DO I=1,NEL
                  IF (GBUF%IDT_TSH(I)<=0) CYCLE
 	              N1 = NC(1,I)
 	              N2 = NC(5,I)
	              IF (ITAG(N1)==0.AND.ITAG(N2)==0) THEN
	                NEDG = NEDG + 1
                    IENUNL(1,NEDG) = N1
                    IENUNL(2,NEDG) = N2
	                ITAG(N1)=NEDG
	                ITAG(N2)=NEDG
                  END IF
 	              N1 = NC(2,I)
 	              N2 = NC(6,I)
	              IF (ITAG(N1)==0.AND.ITAG(N2)==0) THEN
	                NEDG = NEDG + 1
                    IENUNL(1,NEDG) = N1
                    IENUNL(2,NEDG) = N2
	                ITAG(N1)=NEDG
	                ITAG(N2)=NEDG
                  END IF
 	              N1 = NC(3,I)
 	              N2 = NC(7,I)
	              IF (ITAG(N1)==0.AND.ITAG(N2)==0) THEN
	                NEDG = NEDG + 1
                    IENUNL(1,NEDG) = N1
                    IENUNL(2,NEDG) = N2
	                ITAG(N1)=NEDG
	                ITAG(N2)=NEDG
                  END IF
 	              N1 = NC(4,I)
 	              N2 = NC(8,I)
	              IF (ITAG(N1)==0.AND.ITAG(N2)==0) THEN
	                NEDG = NEDG + 1
                    IENUNL(1,NEDG) = N1
                    IENUNL(2,NEDG) = N2
	                ITAG(N1)=NEDG
	                ITAG(N2)=NEDG
                  END IF
                ENDDO
              CASE(1)                                          
                DO I=1,NEL
                  IF (GBUF%IDT_TSH(I)<=0) CYCLE
 	              N1 = NC(1,I)
 	              N2 = NC(2,I)
	              IF (ITAG(N1)==0.AND.ITAG(N2)==0) THEN
	                NEDG = NEDG + 1
                    IENUNL(1,NEDG) = N1
                    IENUNL(2,NEDG) = N2
	                ITAG(N1)=NEDG
	                ITAG(N2)=NEDG
                  END IF
 	              N1 = NC(4,I)
 	              N2 = NC(3,I)
	              IF (ITAG(N1)==0.AND.ITAG(N2)==0) THEN
	                NEDG = NEDG + 1
                    IENUNL(1,NEDG) = N1
                    IENUNL(2,NEDG) = N2
	                ITAG(N1)=NEDG
	                ITAG(N2)=NEDG
                  END IF
 	              N1 = NC(5,I)
 	              N2 = NC(6,I)
	              IF (ITAG(N1)==0.AND.ITAG(N2)==0) THEN
	                NEDG = NEDG + 1
                    IENUNL(1,NEDG) = N1
                    IENUNL(2,NEDG) = N2
	                ITAG(N1)=NEDG
	                ITAG(N2)=NEDG
                  END IF
 	              N1 = NC(8,I)
 	              N2 = NC(7,I)
	              IF (ITAG(N1)==0.AND.ITAG(N2)==0) THEN
	                NEDG = NEDG + 1
                    IENUNL(1,NEDG) = N1
                    IENUNL(2,NEDG) = N2
	                ITAG(N1)=NEDG
	                ITAG(N2)=NEDG
                  END IF
                ENDDO
             END SELECT 
          ELSEIF (JHBE==15) THEN
            DO I=1,NEL
              IF (GBUF%IDT_TSH(I)<=0) CYCLE
 	          N1 = NC(1,I)
 	          N2 = NC(5,I)
	          IF (ITAG(N1)==0.AND.ITAG(N2)==0) THEN
	            NEDG = NEDG + 1
                IENUNL(1,NEDG) = N1
                IENUNL(2,NEDG) = N2
	            ITAG(N1)=NEDG
	            ITAG(N2)=NEDG
              END IF
 	          N1 = NC(2,I)
 	          N2 = NC(6,I)
	          IF (ITAG(N1)==0.AND.ITAG(N2)==0) THEN
	            NEDG = NEDG + 1
                IENUNL(1,NEDG) = N1
                IENUNL(2,NEDG) = N2
	            ITAG(N1)=NEDG
	            ITAG(N2)=NEDG
              END IF
 	          N1 = NC(3,I)
 	          N2 = NC(7,I)
	          IF (ITAG(N1)==0.AND.ITAG(N2)==0) THEN
	            NEDG = NEDG + 1
                IENUNL(1,NEDG) = N1
                IENUNL(2,NEDG) = N2
	            ITAG(N1)=NEDG
	            ITAG(N2)=NEDG
              END IF
 	          N1 = NC(4,I)
 	          N2 = NC(8,I)
	          IF (ITAG(N1)==0.AND.ITAG(N2)==0) THEN
	            NEDG = NEDG + 1
                IENUNL(1,NEDG) = N1
                IENUNL(2,NEDG) = N2
	            ITAG(N1)=NEDG
	            ITAG(N2)=NEDG
              END IF
            ENDDO
          END IF
        END IF
       END IF
      ENDDO 
C
      RETURN
      END SUBROUTINE IND_TSHEDG
Chd|====================================================================
Chd|  SDLENSH                       source/elements/thickshell/solidec/scdtdc3.F
Chd|-- called by -----------
Chd|        S16FORC3                      source/elements/thickshell/solide16/s16forc3.F
Chd|        S8CFORC3                      source/elements/thickshell/solide8c/s8cforc3.F
Chd|        SCFORC3                       source/elements/thickshell/solidec/scforc3.F
Chd|-- calls ---------------
Chd|        CLSYS3                        source/output/h3d/h3d_results/h3d_shell_tensor.F
Chd|====================================================================
      SUBROUTINE SDLENSH(VOLN,LLSH,AREA,
     .   X1, X2, X3, X4, X5, X6, X7, X8,
     .   Y1, Y2, Y3, Y4, Y5, Y6, Y7, Y8,
     .   Z1, Z2, Z3, Z4, Z5, Z6, Z7, Z8)
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
#include      "scr17_c.inc"
#include      "vect01_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      my_real, DIMENSION(MVSIZ) , INTENT(OUT)  :: AREA 
      my_real
     .   VOLN(*),LLSH(*),
     .   X1(*), X2(*), X3(*), X4(*), X5(*), X6(*), X7(*), X8(*),
     .   Y1(*), Y2(*), Y3(*), Y4(*), Y5(*), Y6(*), Y7(*), Y8(*),  
     .   Z1(*), Z2(*), Z3(*), Z4(*), Z5(*), Z6(*), Z7(*), Z8(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J, N
      my_real
     .   RX(MVSIZ),RY(MVSIZ),RZ(MVSIZ),SX(MVSIZ),SY(MVSIZ),SZ(MVSIZ),
     .   VQ(3,3,MVSIZ), LXYZ0(3),DETA1(MVSIZ),XX,YY,ZZ,
     .   XL2(MVSIZ),XL3(MVSIZ),XL4(MVSIZ),YL2(MVSIZ),
     .   YL3(MVSIZ),YL4(MVSIZ),ZL1(MVSIZ),
     .   XN(4,MVSIZ) , YN(4,MVSIZ) , ZN(4,MVSIZ) 
      my_real
     .   AL1,AL2,LL(MVSIZ),COREL(2,4)
      my_real
     .   X13,X24,Y13,Y24,L13,L24,C1,C2,THKLY,POSLY,
     .   FAC,VISCE,RX1,RY1,SX1,SY1,S1,FAC1,FAC2,FACI,FAC11,FACDT
C=======================================================================
        DO I=LFT,LLT
          XN(1,I) = HALF*(X1(I)+X5(I))
          YN(1,I) = HALF*(Y1(I)+Y5(I))
          ZN(1,I) = HALF*(Z1(I)+Z5(I))
          XN(2,I) = HALF*(X2(I)+X6(I))
          YN(2,I) = HALF*(Y2(I)+Y6(I))
          ZN(2,I) = HALF*(Z2(I)+Z6(I))
          XN(3,I) = HALF*(X3(I)+X7(I))
          YN(3,I) = HALF*(Y3(I)+Y7(I))
          ZN(3,I) = HALF*(Z3(I)+Z7(I))
          XN(4,I) = HALF*(X4(I)+X8(I))
          YN(4,I) = HALF*(Y4(I)+Y8(I))
          ZN(4,I) = HALF*(Z4(I)+Z8(I))
        ENDDO 
C------g1,g2 :
        DO I=LFT,LLT
          RX(I)=XN(2,I)+XN(3,I)-XN(1,I)-XN(4,I)
          RY(I)=YN(2,I)+YN(3,I)-YN(1,I)-YN(4,I)
          RZ(I)=ZN(2,I)+ZN(3,I)-ZN(1,I)-ZN(4,I)
          SX(I)=XN(3,I)+XN(4,I)-XN(1,I)-XN(2,I)
          SY(I)=YN(3,I)+YN(4,I)-YN(1,I)-YN(2,I)
          SZ(I)=ZN(3,I)+ZN(4,I)-ZN(1,I)-ZN(2,I)
        ENDDO 
C------Local elem. base:
        CALL CLSYS3(RX, RY, RZ, SX, SY, SZ, 
     .              VQ, DETA1,LLT)
C------ Global -> Local Coordinate  FOURTH=0.25 ;
        DO I=LFT,LLT
          LXYZ0(1)=FOURTH*(XN(1,I)+XN(2,I)+XN(3,I)+XN(4,I))
          LXYZ0(2)=FOURTH*(YN(1,I)+YN(2,I)+YN(3,I)+YN(4,I))
          LXYZ0(3)=FOURTH*(ZN(1,I)+ZN(2,I)+ZN(3,I)+ZN(4,I))
          XX=XN(2,I)-XN(1,I)
          YY=YN(2,I)-YN(1,I)
          ZZ=ZN(2,I)-ZN(1,I)
          XL2(I)=VQ(1,1,I)*XX+VQ(2,1,I)*YY+VQ(3,1,I)*ZZ
          YL2(I)=VQ(1,2,I)*XX+VQ(2,2,I)*YY+VQ(3,2,I)*ZZ
          XX=XN(2,I)-LXYZ0(1)
          YY=YN(2,I)-LXYZ0(2)
          ZZ=ZN(2,I)-LXYZ0(3)
          ZL1(I)=VQ(1,3,I)*XX+VQ(2,3,I)*YY+VQ(3,3,I)*ZZ
C          
          XX=XN(3,I)-XN(1,I)
          YY=YN(3,I)-YN(1,I)
          ZZ=ZN(3,I)-ZN(1,I)
          XL3(I)=VQ(1,1,I)*XX+VQ(2,1,I)*YY+VQ(3,1,I)*ZZ
          YL3(I)=VQ(1,2,I)*XX+VQ(2,2,I)*YY+VQ(3,2,I)*ZZ
C
          XX=XN(4,I)-XN(1,I)
          YY=YN(4,I)-YN(1,I)
          ZZ=ZN(4,I)-ZN(1,I)
          XL4(I)=VQ(1,1,I)*XX+VQ(2,1,I)*YY+VQ(3,1,I)*ZZ
          YL4(I)=VQ(1,2,I)*XX+VQ(2,2,I)*YY+VQ(3,2,I)*ZZ
          AREA(I)=FOURTH*DETA1(I)
        ENDDO 
      FAC = TWO
      FACDT = FIVE_OVER_4
C-------same than QBAT       
      IF (IDT1SOL>0) FACDT =FOUR_OVER_3
C---- compute COREL(2,4) mean surface and area     
      DO I=LFT,LLT
        LXYZ0(1)=FOURTH*(XL2(I)+XL3(I)+XL4(I))
        LXYZ0(2)=FOURTH*(YL2(I)+YL3(I)+YL4(I))
        COREL(1,1)=-LXYZ0(1)
        COREL(1,2)=XL2(I)-LXYZ0(1)
        COREL(1,3)=XL3(I)-LXYZ0(1)
        COREL(1,4)=XL4(I)-LXYZ0(1)
        COREL(2,1)=-LXYZ0(2)
        COREL(2,2)=YL2(I)-LXYZ0(2)
        COREL(2,3)=YL3(I)-LXYZ0(2)
        COREL(2,4)=YL4(I)-LXYZ0(2)
        X13=(COREL(1,1)-COREL(1,3))*HALF
        X24=(COREL(1,2)-COREL(1,4))*HALF
        Y13=(COREL(2,1)-COREL(2,3))*HALF
        Y24=(COREL(2,2)-COREL(2,4))*HALF
C
        L13=X13*X13+Y13*Y13
        L24=X24*X24+Y24*Y24
        AL1=MAX(L13,L24)
        C1 =COREL(1,2)*COREL(2,4)-COREL(2,2)*COREL(1,4)
        C2 =COREL(1,1)*COREL(2,3)-COREL(2,1)*COREL(1,3)
        AL2 =MAX(ABS(C1),ABS(C2))/AREA(I)
        RX1=X24-X13
        RY1=Y24-Y13
        SX1=-X24-X13
        SY1=-Y24-Y13
        C1=SQRT(RX1*RX1+RY1*RY1)
        C2=SQRT(SX1*SX1+SY1*SY1)
        S1=FOURTH*(MAX(C1,C2)/MIN(C1,C2)-ONE)
        FAC1=MIN(HALF,S1)+ONE
        FAC2=AREA(I)/(C1*C2)
        FAC2=3.413*MAX(ZERO,FAC2-0.7071)
        FAC2=0.78+0.22*FAC2*FAC2*FAC2
        FACI=TWO*FAC1*FAC2
        S1 = SQRT(FACI*(FACDT+AL2)*AL1)
        S1 = MAX(S1,EM20)
        LLSH(I) = AREA(I)/S1
      ENDDO
C
      RETURN
      END SUBROUTINE SDLENSH
Chd|====================================================================
Chd|  SDLENSH3N                     source/elements/thickshell/solidec/scdtdc3.F
Chd|-- called by -----------
Chd|        S6CFORC3                      source/elements/thickshell/solide6c/s6cforc3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE SDLENSH3N(VOLN,LLSH3N,AREA,
     .                     X1, X2, X3, X4, X5, X6,
     .                     Y1, Y2, Y3, Y4, Y5, Y6,
     .                     Z1, Z2, Z3, Z4, Z5, Z6)
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
#include      "vect01_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      my_real, DIMENSION(MVSIZ) , INTENT(OUT)  :: AREA 
      my_real
     .   VOLN(*),LLSH3N(*),
     .   X1(*), X2(*), X3(*), X4(*), X5(*), X6(*),
     .   Y1(*), Y2(*), Y3(*), Y4(*), Y5(*), Y6(*),  
     .   Z1(*), Z2(*), Z3(*), Z4(*), Z5(*), Z6(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J, N
      my_real
     .        E1X(MVSIZ), E1Y(MVSIZ), E1Z(MVSIZ),
     .        E2X(MVSIZ), E2Y(MVSIZ), E2Z(MVSIZ),
     .        E3X(MVSIZ), E3Y(MVSIZ), E3Z(MVSIZ),
     .        X31(MVSIZ), Y31(MVSIZ), Z31(MVSIZ),
     .        X32(MVSIZ), Y32(MVSIZ), Z32(MVSIZ),
     .        X21(MVSIZ), Y21(MVSIZ), Z21(MVSIZ), 
     .        X2L(MVSIZ), X3L(MVSIZ), Y3L(MVSIZ),
     .        XN(3,MVSIZ) , YN(3,MVSIZ) , ZN(3,MVSIZ) 
      my_real
     .   AL1,AL2,AL3,ALMAX,SUM
C=======================================================================
       DO I=LFT,LLT
          XN(1,I) = HALF*(X1(I)+X4(I))
          YN(1,I) = HALF*(Y1(I)+Y4(I))
          ZN(1,I) = HALF*(Z1(I)+Z4(I))
          XN(2,I) = HALF*(X2(I)+X5(I))
          YN(2,I) = HALF*(Y2(I)+Y5(I))
          ZN(2,I) = HALF*(Z2(I)+Z5(I))
          XN(3,I) = HALF*(X3(I)+X6(I))
          YN(3,I) = HALF*(Y3(I)+Y6(I))
          ZN(3,I) = HALF*(Z3(I)+Z6(I))
       ENDDO 
      DO I=LFT,LLT
        X21(I)=XN(2,I)-XN(1,I)
        Y21(I)=YN(2,I)-YN(1,I)
        Z21(I)=ZN(2,I)-ZN(1,I)
        X31(I)=XN(3,I)-XN(1,I)
        Y31(I)=YN(3,I)-YN(1,I)
        Z31(I)=ZN(3,I)-ZN(1,I)
        X32(I)=XN(3,I)-XN(2,I)
        Y32(I)=YN(3,I)-YN(2,I)
        Z32(I)=ZN(3,I)-ZN(2,I)
      ENDDO
C
      DO I=LFT,LLT
        E1X(I)= X21(I)
        E1Y(I)= Y21(I)
        E1Z(I)= Z21(I)
        X2L(I) = SQRT(E1X(I)*E1X(I)+E1Y(I)*E1Y(I)+E1Z(I)*E1Z(I))
        E1X(I)=E1X(I)/X2L(I)
        E1Y(I)=E1Y(I)/X2L(I)
        E1Z(I)=E1Z(I)/X2L(I)
      ENDDO
C
      DO I=LFT,LLT
        E3X(I)=Y31(I)*Z32(I)-Z31(I)*Y32(I)
        E3Y(I)=Z31(I)*X32(I)-X31(I)*Z32(I)
        E3Z(I)=X31(I)*Y32(I)-Y31(I)*X32(I)
        SUM = SQRT(E3X(I)*E3X(I)+E3Y(I)*E3Y(I)+E3Z(I)*E3Z(I))
        E3X(I)=E3X(I)/SUM
        E3Y(I)=E3Y(I)/SUM
        E3Z(I)=E3Z(I)/SUM
        AREA(I) = HALF * SUM
      ENDDO
C
      DO I=LFT,LLT
        E2X(I)=E3Y(I)*E1Z(I)-E3Z(I)*E1Y(I)
        E2Y(I)=E3Z(I)*E1X(I)-E3X(I)*E1Z(I)
        E2Z(I)=E3X(I)*E1Y(I)-E3Y(I)*E1X(I)
        SUM = SQRT(E2X(I)*E2X(I)+E2Y(I)*E2Y(I)+E2Z(I)*E2Z(I))
        E2X(I)=E2X(I)/SUM
        E2Y(I)=E2Y(I)/SUM
        E2Z(I)=E2Z(I)/SUM
        Y3L(I)=E2X(I)*X31(I)+E2Y(I)*Y31(I)+E2Z(I)*Z31(I)
        X3L(I)=E1X(I)*X31(I)+E1Y(I)*Y31(I)+E1Z(I)*Z31(I)
      ENDDO
      DO I=LFT,LLT
        AL1 = X2L(I) * X2L(I)      
        AL2 = (X3L(I)-X2L(I)) * (X3L(I)-X2L(I)) + Y3L(I) * Y3L(I)
        AL3 = X3L(I) * X3L(I) + Y3L(I) * Y3L(I)
        ALMAX = MAX(AL1,AL2,AL3)
        LLSH3N(I)= TWO*AREA(I) / SQRT(ALMAX)
      ENDDO
C
      RETURN
      END SUBROUTINE SDLENSH3N
Chd|====================================================================
Chd|  SCUMUALPHA                    source/elements/thickshell/solidec/scdtdc3.F
Chd|-- called by -----------
Chd|        S8CFORC3                      source/elements/thickshell/solide8c/s8cforc3.F
Chd|        SCFORC3                       source/elements/thickshell/solidec/scforc3.F
Chd|-- calls ---------------
Chd|        DTDC_MOD                      share/modules/dtdc_mod.F      
Chd|====================================================================
      SUBROUTINE SCUMUALPHA(OFFG,ALPHA_E,NC1, NC2, NC3, NC4,
     +                      NC5, NC6, NC7, NC8)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE DTDC_MOD  
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "comlock.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "vect01_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NC1(*), NC2(*), NC3(*), NC4(*), NC5(*), NC6(*), NC7(*),
     .        NC8(*)
C     REAL
      my_real
     .   OFFG(*),ALPHA_E(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER  I, J,N
C-----------------------------------------------
#include "lockon.inc"
      DO I=LFT,LLT
        IF (OFFG(I)==ZERO) CYCLE
        N = NC1(I)
        ALPHA_DC(N) = MAX(ALPHA_DC(N),ALPHA_E(I))
        N = NC2(I)
        ALPHA_DC(N) = MAX(ALPHA_DC(N),ALPHA_E(I))
        N = NC3(I)
        ALPHA_DC(N) = MAX(ALPHA_DC(N),ALPHA_E(I))
        N = NC4(I)
        ALPHA_DC(N) = MAX(ALPHA_DC(N),ALPHA_E(I))
        N = NC5(I)
        ALPHA_DC(N) = MAX(ALPHA_DC(N),ALPHA_E(I))
        N = NC6(I)
        ALPHA_DC(N) = MAX(ALPHA_DC(N),ALPHA_E(I))
        N = NC7(I)
        ALPHA_DC(N) = MAX(ALPHA_DC(N),ALPHA_E(I))
        N = NC8(I)
        ALPHA_DC(N) = MAX(ALPHA_DC(N),ALPHA_E(I))
      END DO
#include "lockoff.inc"
C      
      RETURN
      END SUBROUTINE SCUMUALPHA
Chd|====================================================================
Chd|  SCUMUALPHA6                   source/elements/thickshell/solidec/scdtdc3.F
Chd|-- called by -----------
Chd|        S6CFORC3                      source/elements/thickshell/solide6c/s6cforc3.F
Chd|-- calls ---------------
Chd|        DTDC_MOD                      share/modules/dtdc_mod.F      
Chd|====================================================================
      SUBROUTINE SCUMUALPHA6(OFFG,ALPHA_E,NC1, NC2, NC3, NC4,
     +                       NC5, NC6)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE DTDC_MOD  
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "comlock.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "vect01_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NC1(*), NC2(*), NC3(*), NC4(*), NC5(*), NC6(*)
C     REAL
      my_real
     .   OFFG(*),ALPHA_E(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER  I, J,N
C-----------------------------------------------
#include "lockon.inc"
      DO I=LFT,LLT
        IF (OFFG(I)==ZERO) CYCLE
        N = NC1(I)
        ALPHA_DC(N) = MAX(ALPHA_DC(N),ALPHA_E(I))
        N = NC2(I)
        ALPHA_DC(N) = MAX(ALPHA_DC(N),ALPHA_E(I))
        N = NC3(I)
        ALPHA_DC(N) = MAX(ALPHA_DC(N),ALPHA_E(I))
        N = NC4(I)
        ALPHA_DC(N) = MAX(ALPHA_DC(N),ALPHA_E(I))
        N = NC5(I)
        ALPHA_DC(N) = MAX(ALPHA_DC(N),ALPHA_E(I))
        N = NC6(I)
        ALPHA_DC(N) = MAX(ALPHA_DC(N),ALPHA_E(I))
      END DO
#include "lockoff.inc"
C      
      RETURN
      END SUBROUTINE SCUMUALPHA6
Chd|====================================================================
Chd|  ACCDTDC                       source/elements/thickshell/solidec/scdtdc3.F
Chd|-- called by -----------
Chd|        RESOL                         source/engine/resol.F         
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE ACCDTDC(EFTSK ,ELTSK ,IENUNL ,ALPHA_DC, A       ,MS   ,ITAB )
C-----------------------------------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER EFTSK ,ELTSK ,IENUNL(2,*), ITAB(*)
      my_real
     .   A(3,*) ,ALPHA_DC(*),MS(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER  I,N1,N2,inn
      my_real
     .     AM(3),AD(3),ALPHA_1,F1(3),F2(3),MSS_1
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------     
      
      !------------------------------------------------!
      !       MODIF  ACCELERATIONS (TRANSLATIONS) !
      !------------------------------------------------!
#include "vectorize.inc"
      DO I=EFTSK ,ELTSK
        N1= IENUNL(1,I)
        N2= IENUNL(2,I)
        IF (MAX(ALPHA_DC(N1),ALPHA_DC(N2))<=ONE) CYCLE
        IF (MAX(MS(N1),MS(N2))==ZERO) CYCLE
        IF (MS(N1)==ZERO) THEN
          F1(1:3) = A(1:3,N1)
        ELSE
          F1(1:3) = MS(N1)*A(1:3,N1)
        END IF
        IF (MS(N2)==ZERO) THEN
          F2(1:3) = A(1:3,N2)
        ELSE
          F2(1:3) = MS(N2)*A(1:3,N2)
        END IF
        MSS_1 = ONE/(MS(N1)+MS(N2))
        AM(1:3) = (F1(1:3)+F2(1:3))*MSS_1
        AD(1:3) = (F2(1:3)-F1(1:3))*MSS_1
        ALPHA_1 =ONE/MAX(ALPHA_DC(N1),ALPHA_DC(N2))
        A(1:3,N1) = AM(1:3) - AD(1:3)*ALPHA_1
        A(1:3,N2) = AM(1:3) + AD(1:3)*ALPHA_1
      END DO 
C      
      RETURN
      END SUBROUTINE ACCDTDC
Chd|====================================================================
Chd|  TSHCDCOM_DIM                  source/elements/thickshell/solidec/scdtdc3.F
Chd|-- called by -----------
Chd|        RESOL_INIT                    source/engine/resol_init.F    
Chd|-- calls ---------------
Chd|        SPMD_EXCH_TAG_TSH             source/mpi/elements/spmd_exch_dttsh.F
Chd|====================================================================
      SUBROUTINE TSHCDCOM_DIM(IENUNL,FR_ELEM,IAD_ELEM,NBDDS,NBDDR ,
     .                        ISEND ,IRECV )
C-----------------------------------------------
C   M o d u l e s
C----------------------------------------------- 
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IENUNL(2,*),FR_ELEM(*),IAD_ELEM(2,*),NBDDS,NBDDR,
     .        IRECV(*),ISEND(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,N,IP,ITAGS(NUMNOD),LEN
C=======================================================================
       ITAGS(1:NUMNOD)=0
       DO I = 1, NTSHEG
         N = IENUNL(1,I)
         ITAGS(N) = I
         N = IENUNL(2,I)
         ITAGS(N) = I
       END DO
C--------
       NBDDS = 0
       DO IP = 1,NSPMD
         DO J= IAD_ELEM(1,IP),IAD_ELEM(1,IP+1)-1
          N = FR_ELEM(J)
          IF (ITAGS(N)>0) THEN
            NBDDS = NBDDS + 1
            ISEND(J) = 1
          END IF
         END DO
       END DO
       LEN = IAD_ELEM(1,NSPMD+1)-IAD_ELEM(1,1)
       CALL SPMD_EXCH_TAG_TSH(IAD_ELEM ,FR_ELEM ,ISEND,IRECV ,LEN)
       NBDDR = 0
       DO IP = 1,NSPMD
         DO J= IAD_ELEM(1,IP),IAD_ELEM(1,IP+1)-1
          IF (IRECV(J)>0) NBDDR = NBDDR + 1
         END DO
       END DO
C
      RETURN
      END
Chd|====================================================================
Chd|  TSHCDCOM_INI                  source/elements/thickshell/solidec/scdtdc3.F
Chd|-- called by -----------
Chd|        RESOL_INIT                    source/engine/resol_init.F    
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE TSHCDCOM_INI(ISEND,IAD_ELEM,FR_ELEM,IAD_STSH,FR_STSH)
C-----------------------------------------------
C   M o d u l e s
C----------------------------------------------- 
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER ISEND(*),FR_ELEM(*),IAD_ELEM(2,*),FR_STSH(*),IAD_STSH(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,N,IP,NB
C=======================================================================
       NB = 1
       IAD_STSH(NB) =1
       DO IP = 1,NSPMD
         DO J= IAD_ELEM(1,IP),IAD_ELEM(1,IP+1)-1
          N = FR_ELEM(J)
          IF (ISEND(J)>0) THEN
            FR_STSH(NB) = N
            NB = NB + 1
          END IF
         END DO
         IAD_STSH(IP+1) =NB
       END DO
C
      RETURN
      END SUBROUTINE TSHCDCOM_INI
