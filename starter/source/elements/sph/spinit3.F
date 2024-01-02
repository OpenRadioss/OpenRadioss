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
Chd|  SPINIT3                       source/elements/sph/spinit3.F 
Chd|-- called by -----------
Chd|        INITIA                        source/elements/initia/initia.F
Chd|-- calls ---------------
Chd|        DTMAIN                        source/materials/time_step/dtmain.F
Chd|        MATINI                        source/materials/mat_share/matini.F
Chd|        SPORTH3                       source/elements/sph/sporth3.F 
Chd|        SPPART3                       source/elements/sph/sppart3.F 
Chd|        GET_U_GEO                     source/user_interface/uaccess.F
Chd|        ALE_CONNECTIVITY_MOD          ../common_source/modules/ale/ale_connectivity_mod.F
Chd|        DETONATORS_MOD                share/modules1/detonators_mod.F
Chd|        ELBUFDEF_MOD                  ../common_source/modules/mat_elem/elbufdef_mod.F
Chd|====================================================================
      SUBROUTINE SPINIT3(IGRTYP  ,SPBUF    ,KXSP   ,X     ,GEO    ,
     .                   XMAS    ,NPC      ,PLD    ,XIN   ,SKEW   ,
     .                   DTELEM  ,NEL      ,STIFN  ,STIFR ,IGEO   ,
     .                   PARTSAV ,V        ,IPARTSP,BUFMAT,
     .                   PM      ,ITAB     ,MSR    ,INR   ,IXSP   ,
     .                   NOD2SP  ,IPARG    ,ALE_CONNECTIVITY  ,DETONATORS  ,
     .                   SIGSPH  ,ISPTAG   ,IPART,
     .                   IPM     ,NSIGSPH  ,PTSPH  ,NPF   ,
     .                   TF      ,ELBUF_STR,MCP    ,TEMP  ,ILOADP ,
     .                   FACLOAD)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE ELBUFDEF_MOD    
      USE DETONATORS_MOD              
      USE ALE_CONNECTIVITY_MOD
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
#include      "param_c.inc"
#include      "scr17_c.inc"
#include      "scry_c.inc"
#include      "sphcom.inc"
#include      "vect01_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER KXSP(NISP,*), NPC(*),IPARTSP(*),ITAB(*),IGEO(*),
     .        IXSP(KVOISPH,*),NOD2SP(*),IPARG(*),ISPTAG(*),
     .        IPART(LIPART1,*),IPM(NPROPMI,*), PTSPH(*), NPF(*)
      INTEGER IGRTYP, NEL,NSIGSPH
      my_real
     .   X(3,*), GEO(NPROPG,*), XMAS(*), PLD(*), XIN(*),
     .   SKEW(LSKEW,*), DTELEM(*),STIFN(*),STIFR(*),PARTSAV(20,*), V(*),
     .   BUFMAT(*),PM(NPROPM,*), MSR(3,*), INR(3,*),
     .   SPBUF(NSPBUF,*),SIGSPH(NSIGSPH,*), TF(*), MCP(*), TEMP(*)
      TYPE (ELBUF_STRUCT_), TARGET :: ELBUF_STR
      INTEGER,INTENT(IN) :: ILOADP(SIZLOADP,*)
      my_real,INTENT(IN) :: FACLOAD(LFACLOAD,*)
      TYPE(DETONATOR_STRUCT_)::DETONATORS
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER IPRT,IMAT,IG,N,I,M,J,INOD,IGTYP,IBID,NF1,NDEPAR,JJ,IP,II(6)
      INTEGER MXT(MVSIZ),NGEO(MVSIZ),NC1(MVSIZ),NGL(MVSIZ)
      my_real
     .        VOL(MVSIZ),MASS(MVSIZ),RHO(MVSIZ),DELTAX(MVSIZ),DTX(MVSIZ),
     .        X1(MVSIZ),Y1(MVSIZ),Z1(MVSIZ),RBID(1), AIRE(MVSIZ)
      my_real
     .        DIST,STI,FV,MP,BID,RHOCP
      TYPE(G_BUFEL_) ,POINTER :: GBUF     
      TYPE(L_BUFEL_) ,POINTER :: LBUF     
      TYPE(BUF_MAT_) ,POINTER :: MBUF
      TYPE(t_ale_connectivity), INTENT(INOUT) :: ALE_CONNECTIVITY
C-----------------------------------------------
      INTEGER  GET_U_PNU,GET_U_PID,GET_U_MID,GET_U_MNU
      my_real
     .         GET_U_MAT,GET_U_GEO,GET_U_FUNC
      EXTERNAL GET_U_PNU,GET_U_MNU,GET_U_MAT,GET_U_GEO,GET_U_PID,
     .         GET_U_MID,GET_U_FUNC
C=======================================================================
C     GENERAL SPH CELLS.
C--------------------------------------------------
      GBUF => ELBUF_STR%GBUF
      LBUF => ELBUF_STR%BUFLY(1)%LBUF(1,1,1)
      MBUF => ELBUF_STR%BUFLY(1)%MAT(1,1,1)
      RBID = ZERO
      IBID = 0
!
      DO I=1,6
        II(I) = NEL*(I-1)
      ENDDO
!
c
      IF(ISPH2SOL==0)THEN
        DO I=LFT,LLT
          N =I+NFT
          IPRT=IPARTSP(N)
          IMAT=IPART(1,IPRT)
          IG  =IPART(2,IPRT)
          MP    =GET_U_GEO(1,IG)
          RHO(I)=PM(1,IMAT)
          VOL(I)=MP/RHO(I)
          IF(NSPCOND/=0) VOL(I)=VOL(I)/ISPTAG(N)
          MASS(I)   =RHO(I)*VOL(I)
          SPBUF(2,N) =RHO(I)
          SPBUF(12,N)=MASS(I)
        END DO
      ELSE
        DO I=LFT,LLT
          N =I+NFT
          IPRT=IPARTSP(N)
          IMAT=IPART(1,IPRT)
          IG  =IPART(2,IPRT)
          RHO(I)=PM(1,IMAT)
C
C Rho, Vol prepared in SINIT3
          VOL(I)     =SPBUF(12,N)
          MASS(I)    =RHO(I)*VOL(I)
          IF(MASS(I)/=SPBUF(2,N))THEN
C error !
          END IF
          SPBUF(2,N) =RHO(I)
          SPBUF(12,N)=MASS(I)
        END DO
      END IF
C-----------------------------------------------
       NF1 =NFT+1
C--------------------------------------------------
C      NUMERO DE MATERIAU ET PID.
C--------------------------------------------------
       DO I=LFT,LLT
        N=NFT+I
        IPRT   =IPARTSP(N)
        MXT(I) =IPART(1,IPRT)
        NGEO(I)=IPART(2,IPRT)
        NGL(I) =KXSP(NISP,N)
        NC1(I) =KXSP(3,N)
       ENDDO
C--------------------------------------------------
C      LONGUEUR CARACTERISTIQUE.
C--------------------------------------------------
       DO I=LFT,LLT
        N=NFT+I
        DELTAX(I)=SPBUF(1,N)
       ENDDO
C--------------------------------------------------
C      VOLUME INITIAL.
C--------------------------------------------------
       DO I=LFT,LLT
         GBUF%RHO(I)=RHO(I)
         GBUF%VOL(I)=VOL(I)
       ENDDO
C--------------------------------------------------
C      POSITION (for LAW NUMBER 5).
C--------------------------------------------------
       DO I=LFT,LLT
        N=NFT+I
        INOD =KXSP(3,N)
        X1(I)=X(1,INOD)
        Y1(I)=X(2,INOD)
        Z1(I)=X(3,INOD)
       ENDDO
C--------------------------------------------------
       IF(ISORTH/=0)THEN
         CALL SPORTH3(IPART ,IPARTSP(NFT+1) ,IGEO ,GBUF%GAMA,SKEW,
     .                NEL   )
       END IF
C--------------------------------------------------
C     GENERAL CELLS, END.
C--------------------------------------------------
      IP=1
      CALL MATINI(PM      ,KXSP   ,NISP       ,X         ,
     .            GEO     ,ALE_CONNECTIVITY  ,DETONATORS ,IPARG     ,
     .            SIGSPH  ,NEL    ,SKEW       ,IGEO      ,
     .            IPART   ,IPARTSP,
     .            MXT     ,IPM    ,NSIGSPH    ,NUMSPHY   ,PTSPH  ,
     .            IP      ,NGL    ,NPF        ,TF        ,BUFMAT ,
     .            GBUF    ,LBUF   ,MBUF       ,ELBUF_STR,ILOADP ,
     .            FACLOAD, DELTAX)
C--------------------------------------------------
C      Diametre initial (Fichiers Y)
C--------------------------------------------------
       IF(ISIGI==3.OR.ISIGI==4.OR.ISIGI==5)THEN
         DO I=LFT,LLT
           N = I+NFT
           JJ=PTSPH(N)
           IF(JJ/=0.AND.SIGSPH(11,JJ)/=0.)THEN
             SPBUF(1,N)=SIGSPH(11,JJ)
           END IF
           SPBUF(2,N) = GBUF%RHO(I)
         ENDDO
       ENDIF
C----------------------------------------
C     INITIALISATION DE LA THERMIQUE
C----------------------------------------
      IF(JTHE > 0)THEN
        DO I=LFT,LLT
          GBUF%TEMP(I)=PM(79,MXT(I))
        ENDDO
      ELSEIF (JTHE < 0) THEN
        INTHEAT = 1
        DO I=LFT,LLT
          J = NC1(I)
          RHOCP = PM(69,MXT(I))*VOL(I)
          MCP(J) = RHOCP+MCP(J)
          TEMP(J) = PM(79,MXT(I))
        ENDDO
      END IF
C--------------------------------------------------
C     INITIALISATION DES MASSES.
C--------------------------------------------------
      CALL SPPART3(XMAS,PARTSAV,NC1,MASS,X,V,IPARTSP(NF1))
C--------------------------------------------------
C     CALCUL DES DT ELEMENTAIRES
C--------------------------------------------------
       NDEPAR=NUMELC+NUMELS+NUMELT+NUMELQ+NUMELP+NUMELR+NUMELTG
     .       +NUMELX+NFT
     
      AIRE(:) = ZERO
      IGTYP = IPARG(38)
      CALL DTMAIN(GEO       ,PM        ,IPM         ,NGEO    ,MXT     ,FV    ,
     .     GBUF%EINT ,GBUF%TEMP ,GBUF%DELTAX ,GBUF%RK ,GBUF%RE ,BUFMAT, DELTAX, AIRE, 
     .     GBUF%VOL, DTX, IGEO,IGTYP)

       DO I=LFT,LLT
         DTELEM(NDEPAR+I)=DTX(I)
         STI = TWO * MASS(I) / MAX(EM20,DTX(I)*DTX(I))
         STIFN(KXSP(3,I+NFT))=STIFN(KXSP(3,I+NFT))+STI
       ENDDO
C--------------------------------------------------
      DO I=LFT,LLT
        N=NFT+I
        IF(KXSP(2,N) < 0.AND.
     .     (N < FIRST_SPHSOL.OR.N >= FIRST_SPHSOL+NSPHSOL))THEN
          GBUF%OFF(I)    = ZERO
          GBUF%RHO(I)    = ZERO
          GBUF%EINT(I)   = ZERO
          GBUF%SIG(II(1)+I) = ZERO
          GBUF%SIG(II(2)+I) = ZERO
          GBUF%SIG(II(3)+I) = ZERO
          GBUF%SIG(II(4)+I) = ZERO
          GBUF%SIG(II(5)+I) = ZERO
          GBUF%SIG(II(6)+I) = ZERO
        ELSEIF(KXSP(2,N) < 0 .AND.
     .     FIRST_SPHSOL <= N .AND. N < FIRST_SPHSOL+NSPHSOL)THEN
          GBUF%OFF(I)    = -ONE
        ENDIF
      ENDDO
C--------------------------------------------------
      RETURN
C--------------------------------------------------
 999  CONTINUE
      RETURN
      END
