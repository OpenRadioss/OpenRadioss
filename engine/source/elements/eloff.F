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
Chd|  ELOFF                         source/elements/eloff.F       
Chd|-- called by -----------
Chd|        DESACTI                       source/elements/desacti.F     
Chd|-- calls ---------------
Chd|        S10NXT4                       source/elements/solid/solide10/s10nxt4.F
Chd|        S4VOLUME                      source/elements/solid/solide4/s4volume.F
Chd|        S8ETEMPER                     source/elements/solid/solide8e/s8etemper.F
Chd|        S8EVOLUME                     source/elements/solid/solide8e/s8evolume.F
Chd|        ELBUFDEF_MOD                  ../common_source/modules/mat_elem/elbufdef_mod.F
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|====================================================================
       SUBROUTINE ELOFF(IXS     ,IXQ     ,IXC      ,IXP     ,IXT      ,
     .                  IXR     ,IXTG    ,IPARG    , 
     .                  IACTIV  ,TIME    ,IFLAG    ,NN      ,ELBUF_TAB,
     .                  X       ,TEMP    ,MCP      ,PM      ,IGROUPS  ,
     .                  MCP_OFF ,IGRBRIC ,IGRQUAD  ,IGRSH4N ,IGRSH3N  ,
     .                  IGRTRUSS,IGRBEAM ,IGRSPRING)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE ELBUFDEF_MOD
      USE GROUPDEF_MOD
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
#include      "param_c.inc"
#include      "units_c.inc"
#include      "com01_c.inc"
#include      "com04_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IACTIV(LACTIV,*),IPARG(NPARG,*),
     .   IXS(NIXS,*), IXQ(NIXQ,*),IXC(NIXC,*), 
     .   IXT(NIXT,*),IXP(NIXP,*), IXR(NIXR,*), IXTG(NIXTG,*),
     .   IGROUPS(*)
      INTEGER IFLAG, NN
      my_real TIME, X(3,*), TEMP(*), MCP(*), PM(NPROPM,*),MCP_OFF(*)
      TYPE(ELBUF_STRUCT_), DIMENSION(NGROUP), TARGET :: ELBUF_TAB
C IFLAG==0 : activation
C IFLAG==1 : deactivation
C IFLAG==-1 : set MCP_OFF(i) = 0 to all nodes that belong only to
C             deactiavted solids
C-----------------------------------------------
      TYPE (GROUP_)  , DIMENSION(NGRBRIC) :: IGRBRIC
      TYPE (GROUP_)  , DIMENSION(NGRQUAD) :: IGRQUAD
      TYPE (GROUP_)  , DIMENSION(NGRSHEL) :: IGRSH4N
      TYPE (GROUP_)  , DIMENSION(NGRSH3N) :: IGRSH3N
      TYPE (GROUP_)  , DIMENSION(NGRTRUS) :: IGRTRUSS
      TYPE (GROUP_)  , DIMENSION(NGRBEAM) :: IGRBEAM
      TYPE (GROUP_)  , DIMENSION(NGRSPRI) :: IGRSPRING
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,II,J,NG,NEL,MLW,NFT,ITY,IGOF,
     .        ISENS,IGSH,IGSH3,IGBR,IGQU,IGBM,IGTR,IGSP,
     .        JTHE, IFORM, ISOLNOD, ITETRA4
      INTEGER NELA,NPTR,NPTS,NPTT,IR,IS,IT,IP,K,KK
      INTEGER INDEX(MVSIZ)
      INTEGER NC1(MVSIZ), NC2(MVSIZ), NC3(MVSIZ), NC4(MVSIZ),
     .        NC5(MVSIZ), NC6(MVSIZ), NC7(MVSIZ), NC8(MVSIZ)
      my_real VOLGN(MVSIZ), VOLPN(MVSIZ,8), TEMPN(MVSIZ,8), MCPS, RHOCP
      my_real NXT4(MVSIZ,4,4), FACVOL
      my_real, DIMENSION(:), POINTER :: OFFG
      my_real, DIMENSION(:), POINTER :: VOLG
      my_real, DIMENSION(:), POINTER :: VOLP
      my_real, DIMENSION(:), POINTER :: TEIP
      INTEGER, DIMENSION(:,:), ALLOCATABLE :: INDEX2
C======================================================================|
      IF( IFLAG == 0 .OR. IFLAG == 1) THEN
      IGBR  = IACTIV(3,NN)
      IGQU  = IACTIV(4,NN) 
      IGSH  = IACTIV(5,NN) 
      IGTR  = IACTIV(6,NN) 
      IGBM  = IACTIV(7,NN) 
      IGSP  = IACTIV(8,NN) 
      IGSH3 = IACTIV(9,NN) 
      IFORM = IACTIV(10,NN) 
      
      ALLOCATE(INDEX2(1+MVSIZ,NGROUP))
      INDEX2=0
      ENDIF
C      
      IF (IFLAG==0) THEN
C------------------------------
C       ACTIVATION DES ELEMENTS
C------------------------------

       IF (IGBR /= 0) THEN
         DO J=1,IGRBRIC(IGBR)%NENTITY
           II = IGRBRIC(IGBR)%ENTITY(J)
           NG = IGROUPS(II)                
           NFT= IPARG(3,NG)
           MLW=IPARG(1,NG)
           IF (MLW == 0 .OR. MLW == 13) CYCLE  ! loi0, pas de off
           I  = II - NFT
           INDEX2(1,NG) = INDEX2(1,NG) + 1
           NELA = INDEX2(1,NG) 
           INDEX2(NELA+1,NG) = I
           WRITE(IOUT,'(A,I10,A,G13.5)')' BRICK ACTIVATION:',IXS(11,II),' AT TIME:',TIME
           OFFG => ELBUF_TAB(NG)%GBUF%OFF
           OFFG(I) = ONE
          ENDDO
       ENDIF

        DO NG=1,NGROUP
         MLW=IPARG(1,NG)
         NEL=IPARG(2,NG)
         NFT=IPARG(3,NG)
         ITY=IPARG(5,NG)
         JTHE=IPARG(13,NG)    
         IF (MLW == 0 .OR. MLW == 13) CYCLE  ! loi0, pas de off
C         
         IF(ITY==1)THEN       
C---       ELEMENTS SOLIDES
           ISOLNOD=IPARG(28,NG)    
           ITETRA4=IPARG(41,NG)
           OFFG => ELBUF_TAB(NG)%GBUF%OFF
           NELA = INDEX2(1,NG)
           INDEX(1:NELA) = INDEX2(2:NELA+1,NG)
      
           IF(NELA == 0) CYCLE
C
           IF(ITHERM_FE > 0) THEN
             VOLG => ELBUF_TAB(NG)%GBUF%VOL
             NPTR   = ELBUF_TAB(NG)%NPTR
             NPTS   = ELBUF_TAB(NG)%NPTS
             NPTT   = ELBUF_TAB(NG)%NPTT
             FACVOL=ONE
             IF(ISOLNOD == 4 .AND. ITETRA4 == 1) FACVOL=FOUR			   

             DO I=1,NELA
                J=INDEX(I)+NFT
                NC1(I)=IXS(2,J)
                NC2(I)=IXS(3,J)
                NC3(I)=IXS(4,J)
                NC4(I)=IXS(5,J)
                NC5(I)=IXS(6,J)
                NC6(I)=IXS(7,J)
                NC7(I)=IXS(8,J)
                NC8(I)=IXS(9,J)

                MCP_OFF(NC1(I)) = ONE
                MCP_OFF(NC2(I)) = ONE
                MCP_OFF(NC3(I)) = ONE
                MCP_OFF(NC4(I)) = ONE
                MCP_OFF(NC5(I)) = ONE
                MCP_OFF(NC6(I)) = ONE
                MCP_OFF(NC7(I)) = ONE
                MCP_OFF(NC8(I)) = ONE
             ENDDO
			 
             IF(IFORM == 2) THEN
               RHOCP=PM(69,IXS(1,1+NFT))
               DO I=1,NELA
                  J=INDEX(I)
                  MCPS=ONE_OVER_8*RHOCP*VOLG(J)*FACVOL
                  MCP(NC1(I)) = MCP(NC1(I)) + MCPS
                  MCP(NC2(I)) = MCP(NC2(I)) + MCPS
                  MCP(NC3(I)) = MCP(NC3(I)) + MCPS
                  MCP(NC4(I)) = MCP(NC4(I)) + MCPS
                  MCP(NC5(I)) = MCP(NC5(I)) + MCPS
                  MCP(NC6(I)) = MCP(NC6(I)) + MCPS
                  MCP(NC7(I)) = MCP(NC7(I)) + MCPS
                  MCP(NC8(I)) = MCP(NC8(I)) + MCPS
               ENDDO
             ENDIF

C---      VOLUME AT ACTIVATION TIME IS UPDATED TO CURRENT VOLUME
C---      TEMPERATURE AT ACTIVATION TIME
             IF(ISOLNOD == 4) THEN
               DO I=1,NELA
                 J=INDEX(I)+NFT
                 NC1(I)=IXS(2,J)
                 NC2(I)=IXS(4,J)
                 NC3(I)=IXS(7,J)
                 NC4(I)=IXS(6,J)
               ENDDO
               CALL S4VOLUME(X, VOLGN, NELA, NC1, NC2, NC3, NC4)
			   
               IF(ITETRA4 == 1) THEN
                 IF(JTHE < 0) CALL S10NXT4(NXT4,NELA)
                 DO IP=1,NPTR
                   DO I=1,NELA
                    VOLPN(I,IP) = FOURTH*VOLGN(I)
				    IF(JTHE >= 0 ) CYCLE
                    TEMPN(I,IP) = NXT4(I,1,IP)*TEMP(NC1(I))+NXT4(I,2,IP)*TEMP(NC2(I))+
     .                            NXT4(I,3,IP)*TEMP(NC3(I))+NXT4(I,4,IP)*TEMP(NC4(I))
                   ENDDO
                 ENDDO
               ELSE
                 DO I=1,NELA
                  VOLPN(I,1) = VOLGN(I)
				  IF(JTHE >= 0 ) CYCLE
                  TEMPN(I,1) = FOURTH*(TEMP(NC1(I))+TEMP(NC2(I))+TEMP(NC3(I))+TEMP(NC4(I)))
                 ENDDO
               ENDIF
             ELSE
               CALL S8EVOLUME(X, VOLGN, VOLPN, NELA, NPTR, NPTS, NPTT,
     .                        NC1, NC2, NC3, NC4, NC5, NC6, NC7, NC8 )
               IF(JTHE < 0 ) THEN 
                  CALL S8ETEMPER(TEMP, TEMPN, NELA, NPTR, NPTS, NPTT, 
     .                        NC1, NC2, NC3, NC4, NC5, NC6, NC7, NC8 )
               ENDIF
             ENDIF
C
             DO I=1,NELA
                J=INDEX(I)
                VOLG(J) = VOLGN(I)/FACVOL
             ENDDO
C
             DO IR=1,NPTR
              DO IS=1,NPTS
               DO IT=1,NPTT
                  IP = IR + ( (IS-1) + (IT-1)*NPTS )*NPTR
                  VOLP => ELBUF_TAB(NG)%BUFLY(1)%LBUF(IR,IS,IT)%VOL
                  IF(JTHE < 0 ) TEIP => ELBUF_TAB(NG)%BUFLY(1)%LBUF(IR,IS,IT)%TEMP
                  DO I=1,NELA
                    J=INDEX(I)
                    VOLP(J) = VOLPN(I,IP)
                    IF(JTHE < 0 ) TEIP(J) = TEMPN(I,IP)
                  ENDDO
               ENDDO 
              ENDDO 
             ENDDO
           ENDIF

C---       TEST POUR L'ACTIVATION D'ONE GROUPE
           IGOF = 1
           DO I = 1,NEL
             IF (OFFG(I) /= ZERO) IGOF=0
           ENDDO
           IPARG(8,NG) = IGOF


         ELSEIF(ITY==2) THEN
C---       ELEMENTS QUADS
           OFFG => ELBUF_TAB(NG)%GBUF%OFF
           DO I=1,NEL
             II=I+NFT
             IF (IGQU /= 0) THEN
             DO J=1,IGRQUAD(IGQU)%NENTITY
               IF (II == IGRQUAD(IGQU)%ENTITY(J)) THEN
                 OFFG(I) = ONE
                 WRITE(IOUT,'(A,I10,A,G13.5)')' QUAD ACTIVATION:',IXQ(7,II),' AT TIME:',TIME
               ENDIF
             ENDDO
             ENDIF
           ENDDO
C---       TEST POUR L'ACTIVATION D'ONE GROUPE
           IGOF = 1
           DO I = 1,NEL
             IF (OFFG(I) /= ZERO) IGOF=0
           ENDDO
           IPARG(8,NG) = IGOF

         ELSEIF(ITY==3)THEN 
C---     ELEMENTS COQUES
          OFFG => ELBUF_TAB(NG)%GBUF%OFF
           DO I=1,NEL
             II=I+NFT
             IF (IGSH /= 0) THEN
             DO J=1,IGRSH4N(IGSH)%NENTITY
               IF (II == IGRSH4N(IGSH)%ENTITY(J)) THEN
                 OFFG(I) = ABS(OFFG(I))
                 WRITE(IOUT,'(A,I10,A,G13.5)')' SHELL ACTIVATION:',IXC(7,II),' AT TIME:',TIME
               ENDIF
             ENDDO
             ENDIF
           ENDDO
C---       TEST POUR L'ACTIVATION D'ONE GROUPE
           IGOF = 1
           DO I = 1,NEL
             IF (OFFG(I) > ZERO) IGOF=0
           ENDDO
           IPARG(8,NG) = IGOF

         ELSEIF(ITY==4) THEN
C---     ELEMENTS TRUSS
           OFFG => ELBUF_TAB(NG)%GBUF%OFF
           DO I=1,NEL
             II=I+NFT
             IF (IGTR /= 0) THEN
             DO J=1,IGRTRUSS(IGTR)%NENTITY
               IF (II == IGRTRUSS(IGTR)%ENTITY(J)) THEN
                 OFFG(I)= ONE
                 WRITE(IOUT,'(A,I10,A,G13.5)')' TRUSS ACTIVATION:',IXT(5,II),' AT TIME:',TIME
               ENDIF
             ENDDO
             ENDIF
           ENDDO
C---       TEST POUR L'ACTIVATION D'ONE GROUPE
           IGOF = 1
           DO I = 1,NEL
             IF (OFFG(I) /= ZERO) IGOF=0
           ENDDO
           IPARG(8,NG) = IGOF

         ELSEIF(ITY==5) THEN
C---     ELEMENTS POUTRES
           OFFG => ELBUF_TAB(NG)%GBUF%OFF
           DO I=1,NEL
             II=I+NFT
             IF (IGBM /= 0) THEN
             DO J=1,IGRBEAM(IGBM)%NENTITY
               IF (II == IGRBEAM(IGBM)%ENTITY(J)) THEN
                 OFFG(I)= ONE
                 WRITE(IOUT,'(A,I10,A,G13.5)')' BEAM ACTIVATION:',IXP(6,II),' AT TIME:',TIME
               ENDIF
             ENDDO
             ENDIF
           ENDDO
C---       TEST POUR L'ACTIVATION D'ONE GROUPE
           IGOF = 1
           DO I = 1,NEL
             IF(OFFG(I) > ZERO) IGOF=0
           ENDDO
           IPARG(8,NG) = IGOF

         ELSEIF(ITY==6) THEN
C---     ELEMENTS RESSORTS
           OFFG => ELBUF_TAB(NG)%GBUF%OFF
!        *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*
!        We need to compute the local referentiel for spring 
!        --> even if off(1:mvsiz)=1, IPARG(8) = 0
!        *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*
           DO I=1,NEL
             II=I+NFT
             IF (IGSP /= 0) THEN
             DO J=1,IGRSPRING(IGSP)%NENTITY
               IF (II == IGRSPRING(IGSP)%ENTITY(J)) THEN
                 OFFG(I)= ONE
                 WRITE(IOUT,'(A,I10,A,G13.5)')' SPRING ACTIVATION:',IXR(NIXR,II),' AT TIME:',TIME
               ENDIF
             ENDDO
             ENDIF
           ENDDO
             IGOF = 0
             IPARG(8,NG) = IGOF

         ELSEIF(ITY==7)THEN 
C---     ELEMENTS COQUES 3N
           OFFG => ELBUF_TAB(NG)%GBUF%OFF
           DO I=1,NEL
             II=I+NFT
             IF (IGSH3 /= 0) THEN
             DO J=1,IGRSH3N(IGSH3)%NENTITY
               IF (II == IGRSH3N(IGSH3)%ENTITY(J)) THEN
                   OFFG(I) = ONE
                 WRITE(IOUT,'(A,I10,A,G13.5)')' SH_3N ACTIVATION:',IXTG(6,II),' AT TIME:',TIME
               ENDIF
             ENDDO
             ENDIF
           ENDDO
C---       TEST POUR L'ACTIVATION D'ONE GROUPE
           IGOF = 1
           DO I = 1,NEL
             IF (OFFG(I) /= ZERO) IGOF=0
           ENDDO
           IPARG(8,NG) = IGOF
C
         ENDIF
        ENDDO
      ELSE IF (IFLAG == 1) THEN
C-----------------------
C     DEACTIVATION DES ELEMENTS
C-----------------------
!       Solids
        IF (IGBR /= 0) THEN
         DO J=1,IGRBRIC(IGBR)%NENTITY
          II = IGRBRIC(IGBR)%ENTITY(J)
          NG = IGROUPS(II)
          OFFG => ELBUF_TAB(NG)%GBUF%OFF
          VOLG => ELBUF_TAB(NG)%GBUF%VOL
          MLW=IPARG(1,NG)
          NEL=IPARG(2,NG)
          NFT=IPARG(3,NG)
          ITY=IPARG(5,NG)
          ISOLNOD=IPARG(28,NG)    
          ITETRA4=IPARG(41,NG)
          IF (MLW == 0 .OR. MLW == 13) CYCLE  ! loi0, pas de off
          I = II - NFT
          OFFG(I) = ZERO
          WRITE(IOUT,'(A,I10,A,G13.5)')' BRICK DEACTIVATION:',IXS(11,II),' AT TIME:',TIME
          IF(ITHERM_FE > 0 .AND. IFORM == 2) THEN
            FACVOL=ONE
            IF(ISOLNOD == 4 .AND. ITETRA4 == 1) FACVOL=FOUR			   
            RHOCP=PM(69,IXS(1,II))
            MCPS=ONE_OVER_8*RHOCP*VOLG(I)*FACVOL
            DO K=2,9
              KK = IXS(K,II)
              MCP(KK) = MCP(KK) - MCPS
            ENDDO
          ENDIF
         ENDDO
        ENDIF

        DO NG=1,NGROUP
         MLW=IPARG(1,NG)
         NEL=IPARG(2,NG)
         NFT=IPARG(3,NG)
         ITY=IPARG(5,NG)
         IF (MLW == 0 .OR. MLW == 13) CYCLE  ! loi0, pas de off
         IF(ITY==1) THEN       
C---     ELEMENTS SOLIDES
           OFFG => ELBUF_TAB(NG)%GBUF%OFF
           VOLG => ELBUF_TAB(NG)%GBUF%VOL
C---       TEST POUR L'ELIMINATION D'ONE GROUPE
           IGOF = 1
           DO I = 1,NEL
             IF (OFFG(I) > ZERO) IGOF=0
           ENDDO
           IPARG(8,NG) = IGOF

         ELSEIF(ITY==2) THEN
C---     ELEMENTS QUADS
           OFFG => ELBUF_TAB(NG)%GBUF%OFF
           DO I=1,NEL
             II=I+NFT
             IF (IGQU /= 0) THEN
             DO J=1,IGRQUAD(IGQU)%NENTITY
               IF (II == IGRQUAD(IGQU)%ENTITY(J)) THEN
                 OFFG(I) = ZERO
                 WRITE(IOUT,'(A,I10,A,G13.5)')' QUAD DEACTIVATION:',IXQ(7,II),' AT TIME:',TIME
               ENDIF
             ENDDO
             ENDIF
           ENDDO
C---       TEST POUR L'ELIMINATION D'ONE GROUPE
           IGOF = 1
           DO I = 1,NEL
             IF (OFFG(I)  /= ZERO) IGOF=0
           ENDDO
           IPARG(8,NG) = IGOF

         ELSEIF(ITY==3) THEN 
C---     ELEMENTS COQUES
           OFFG => ELBUF_TAB(NG)%GBUF%OFF
           DO I=1,NEL
             II=I+NFT
             IF (IGSH /= 0) THEN
             DO J=1,IGRSH4N(IGSH)%NENTITY
               IF (II == IGRSH4N(IGSH)%ENTITY(J)) THEN
                 OFFG(I) = -ABS(OFFG(I))
C
                 WRITE(IOUT,'(A,I10,A,G13.5)')' SHELL DEACTIVATION:',IXC(7,II),' AT TIME:',TIME
               ENDIF
             ENDDO
             ENDIF
           ENDDO
C---       TEST POUR L'ELIMINATION D'ONE GROUPE
           IGOF = 1
           DO I = 1,NEL
             IF (OFFG(I) > ZERO) IGOF=0
           ENDDO
           IPARG(8,NG) = IGOF

         ELSEIF(ITY==4) THEN
C---     ELEMENTS TRUSS
           OFFG => ELBUF_TAB(NG)%GBUF%OFF
           DO I=1,NEL
             II=I+NFT
             IF (IGTR /= 0) THEN
             DO J=1,IGRTRUSS(IGTR)%NENTITY
               IF (II == IGRTRUSS(IGTR)%ENTITY(J)) THEN
                 OFFG(I)= ZERO
                 WRITE(IOUT,'(A,I10,A,G13.5)')' TRUSS DEACTIVATION:',IXT(5,II),' AT TIME:',TIME
               ENDIF
             ENDDO
             ENDIF
           ENDDO
C---       TEST POUR L'ELIMINATION D'ONE GROUPE
           IGOF = 1
           DO I = 1,NEL
             IF(OFFG(I) /= ZERO) IGOF=0
           ENDDO
           IPARG(8,NG) = IGOF

         ELSEIF(ITY==5) THEN
C---     ELEMENTS POUTRES
           OFFG => ELBUF_TAB(NG)%GBUF%OFF
           DO I=1,NEL
             II=I+NFT
             IF (IGBM /= 0) THEN
             DO J=1,IGRBEAM(IGBM)%NENTITY
               IF (II == IGRBEAM(IGBM)%ENTITY(J)) THEN
                 OFFG(I)= ZERO
                 WRITE(IOUT,'(A,I10,A,G13.5)')' BEAM DEACTIVATION:',IXP(6,II),' AT TIME:',TIME
               ENDIF
             ENDDO
             ENDIF
           ENDDO
C---       TEST POUR L'ELIMINATION D'ONE GROUPE
           IGOF = 1
           DO I = 1,NEL
             IF(OFFG(I) > ZERO) IGOF=0
           ENDDO
           IPARG(8,NG) = IGOF

         ELSEIF(ITY==6) THEN
C---     ELEMENTS RESSORTS
           OFFG => ELBUF_TAB(NG)%GBUF%OFF
!        *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*
!        We need to compute the local referentiel for spring 
!        --> even if off(1:mvsiz)=1, IPARG(8) = 0
!        *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*
           DO I=1,NEL
             II=I+NFT
             IF (IGSP /= 0) THEN
             DO J=1,IGRSPRING(IGSP)%NENTITY
               IF (II == IGRSPRING(IGSP)%ENTITY(J)) THEN
                 OFFG(I)= ZERO
                 WRITE(IOUT,'(A,I10,A,G13.5)')' SPRING DEACTIVATION:',IXR(NIXR,II),' AT TIME:',TIME
               ENDIF
             ENDDO
             ENDIF
           ENDDO
C---
           IGOF = 0
           IPARG(8,NG) = IGOF

         ELSEIF(ITY==7) THEN 
C---     ELEMENTS COQUES 3N
          OFFG => ELBUF_TAB(NG)%GBUF%OFF
           DO I=1,NEL
           II=I+NFT
             IF (IGSH3 /= 0) THEN
             DO J=1,IGRSH3N(IGSH3)%NENTITY
               IF (II == IGRSH3N(IGSH3)%ENTITY(J)) THEN
                 OFFG(I) = ZERO
                 WRITE(IOUT,'(A,I10,A,G13.5)')' SH_3N DEACTIVATION:',IXTG(6,II),' AT TIME:',TIME
               ENDIF
             ENDDO
             ENDIF
           ENDDO
C---       TEST POUR L'ELIMINATION D'ONE GROUPE
           IGOF = 1
           DO I = 1,NEL
             IF (OFFG(I) /= ZERO) IGOF=0
           ENDDO
           IPARG(8,NG) = IGOF
C----------------------------------------
         ENDIF
       ENDDO
      ELSE  ! IFLAG == -1 
      IF(ITHERM_FE > 0 ) THEN
C Set MCP_OFF(i) == 0 if all solids connected to i are deactivated
          MCP_OFF(1:NUMNOD) = ONE
          DO II = 1,NUMELS
            NG  = IGROUPS(II)
            OFFG => ELBUF_TAB(NG)%GBUF%OFF
            MLW=IPARG(1,NG)
            NEL=IPARG(2,NG)
            NFT=IPARG(3,NG)
            ITY=IPARG(5,NG)
            I = II - NFT
            IF(OFFG(I) ==  0) THEN
             ! put 0 to nodes that belong to at least one
             ! deactivated element
             DO K=2,9
               KK = IXS(K,II)
               MCP_OFF(KK) = 0.0
             ENDDO
            ENDIF
          ENDDO
          DO II = 1,NUMELS
            NG  = IGROUPS(II)
            OFFG => ELBUF_TAB(NG)%GBUF%OFF
            MLW=IPARG(1,NG)
            NEL=IPARG(2,NG)
            NFT=IPARG(3,NG)
            ITY=IPARG(5,NG)
            I = II - NFT
            IF(OFFG(I) /= 0) THEN
             ! put one to all nodes that belong to at least one
             ! activated element
             DO K=2,9
               KK = IXS(K,II)
               MCP_OFF(KK) = ONE
             ENDDO
            ENDIF
          ENDDO
       ENDIF ! ITHERM_FE

      ENDIF


      IF(IFLAG == 1 .OR. IFLAG == 0) DEALLOCATE(INDEX2)
C----------
      RETURN
      END
