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
Chd|  C3INMAS                       source/elements/sh3n/coque3n/c3inmas.F
Chd|-- called by -----------
Chd|        C3INIT3                       source/elements/sh3n/coque3n/c3init3.F
Chd|        CDKINIT3                      source/elements/sh3n/coquedk/cdkinit3.F
Chd|        INIRIG_MAT                    source/elements/initia/inirig_mat.F
Chd|        INIVOID                       source/elements/initia/inivoid.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        FRETITL2                      source/starter/freform.F
Chd|        DRAPE_MOD                     share/modules1/drape_mod.F
Chd|        ELBUFDEF_MOD                  ../common_source/modules/mat_elem/elbufdef_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        STACK_MOD                     share/modules1/stack_mod.F
Chd|====================================================================
      SUBROUTINE C3INMAS(X,XREFTG ,IXTG ,GEO,PM ,MS ,TINER,THKE  ,
     .                   PARTSAV,V     ,IPART  ,MSTG    ,INTG    ,
     .                   PTG   ,IGEO    ,IMAT  ,IPROP   ,AREA    ,
     .                   ETNOD ,NSHNOD ,STTG   ,SH3TREE ,MCP     ,
     .                   MCPTG  , TEMP ,SH3TRIM,ISUBSTACK,NLAY   ,
     .                   ELBUF_STR, STACK,THKI ,RNOISE   ,DRAPE ,
     .                   PERTURB,IX1   ,IX2    ,IX3    ,
     .                   X2     ,X3     ,Y3    ,IDRAPE   ,INDX)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
        USE ELBUFDEF_MOD
        USE MESSAGE_MOD
        USE STACK_MOD
        USE DRAPE_MOD
C----------------------------------------------
C     INITIALISATION DES MASSES ET INERTIES NODALES
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
#include      "com04_c.inc"
#include      "param_c.inc"
#include      "remesh_c.inc"
#include      "scr03_c.inc"
#include      "scr05_c.inc"
#include      "scr12_c.inc"
#include      "scr17_c.inc"
#include      "vect01_c.inc"
#include      "drape_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
        INTEGER IXTG(NIXTG,*),IPART(*),IMAT,IPROP,
     .          IGEO(NPROPGI,*),IGTYP, NSHNOD(*), SH3TREE(KSH3TREE,*),
     .          SH3TRIM(*),ISUBSTACK,NLAY,IDRAPE,
     .          PERTURB(NPERTURB)
        INTEGER IX1(MVSIZ), IX2(MVSIZ), IX3(MVSIZ)
        INTEGER, DIMENSION (STDRAPE)  :: INDX
        my_real
     .     X(3,*), GEO(NPROPG,*), PM(NPROPM,*), MS(*), TINER(*),THKE(*),
     .     V(3,*),PARTSAV(20,*),MSTG(*),INTG(*),PTG(3,*),
     .     ETNOD(*), STTG(*),MCP(*),MCPTG(*),TEMP(*),XREFTG(3,3,*),THKI(*),
     .     RNOISE(NPERTURB,*), AREA(MVSIZ)
        TYPE(ELBUF_STRUCT_), TARGET :: ELBUF_STR
        TYPE (STACK_PLY) :: STACK
        TYPE (DRAPE_), DIMENSION(NUMELC_DRAPE + NUMELTG_DRAPE), TARGET :: DRAPE
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
        INTEGER I,J,K,N,IP,I1,I2,I3,I4,IPTHK,IPMAT,IPPOS,MATLY,IDPROP,
     .          IPID,IPPID,IIGEO,IADI,IADR,IPANG,NTHK,IGMAT,ILAY,NPTT,
     .          IT,MAX_NPTT,IINT,IPID_LY,IPGMAT,IPOS,IPT,IPT_ALL,NSLICE,IE,
     .          NLAY_MAX
        INTEGER ID
        CHARACTER*nchartitle,
     .     TITR
        my_real XX,YY,ZZ,XY,YZ,ZX,ZSHIFT
        my_real X2(MVSIZ), X3(MVSIZ), Y3(MVSIZ),
     .     RHO(MVSIZ), THK(MVSIZ), EM(MVSIZ), XI(MVSIZ),
     .     P1, P2, P3, AA, BB, CC, A2, B2, C2, XIN, EMS, MSL, XIL,
     .     MST,DMS,INS,POSLY,THKLY,THICKT, ET,EMSCP(MVSIZ),RHOCP(MVSIZ),
     .     EMS_NPTT,MS_NPTT,POS_0,THINNING
        my_real, DIMENSION(:)  , ALLOCATABLE ::  THK_IT,   POS_IT
        my_real, DIMENSION(:,:), ALLOCATABLE ::  LAYPOS,   RATIO_THKLY
C-----------------------------------------------
        TYPE (DRAPE_PLY_) , POINTER :: DRAPE_PLY
C-----------------------------------------------
        my_real
     .    A_GAUSS(9,9),W_GAUSS(9,9)
C-----------------------------------------------
        DATA A_GAUSS /
     1   0.               ,0.               ,0.               ,
     1   0.               ,0.               ,0.               ,
     1   0.               ,0.               ,0.               ,
     2   -.577350269189626,0.577350269189626,0.               ,
     2   0.               ,0.               ,0.               ,
     2   0.               ,0.               ,0.               ,
     3   -.774596669241483,0.               ,0.774596669241483,
     3   0.               ,0.               ,0.               ,
     3   0.               ,0.               ,0.               ,
     4   -.861136311594053,-.339981043584856,0.339981043584856,
     4   0.861136311594053,0.               ,0.               ,
     4   0.               ,0.               ,0.               ,
     5   -.906179845938664,-.538469310105683,0.               ,
     5   0.538469310105683,0.906179845938664,0.               ,
     5   0.               ,0.               ,0.               ,
     6   -.932469514203152,-.661209386466265,-.238619186083197,
     6   0.238619186083197,0.661209386466265,0.932469514203152,
     6   0.               ,0.               ,0.               ,
     7   -.949107912342759,-.741531185599394,-.405845151377397,
     7   0.               ,0.405845151377397,0.741531185599394,
     7   0.949107912342759,0.               ,0.               ,
     8   -.960289856497536,-.796666477413627,-.525532409916329,
     8   -.183434642495650,0.183434642495650,0.525532409916329,
     8   0.796666477413627,0.960289856497536,0.               ,
     9   -.968160239507626,-.836031107326636,-.613371432700590,
     9   -.324253423403809,0.               ,0.324253423403809,
     9   0.613371432700590,0.836031107326636,0.968160239507626/
        DATA W_GAUSS /
     1   2.               ,0.               ,0.               ,
     1   0.               ,0.               ,0.               ,
     1   0.               ,0.               ,0.               ,
     2   1.               ,1.               ,0.               ,
     2   0.               ,0.               ,0.               ,
     2   0.               ,0.               ,0.               ,
     3   0.555555555555556,0.888888888888889,0.555555555555556,
     3   0.               ,0.               ,0.               ,
     3   0.               ,0.               ,0.               ,
     4   0.347854845137454,0.652145154862546,0.652145154862546,
     4   0.347854845137454,0.               ,0.               ,
     4   0.               ,0.               ,0.               ,
     5   0.236926885056189,0.478628670499366,0.568888888888889,
     5   0.478628670499366,0.236926885056189,0.               ,
     5   0.               ,0.               ,0.               ,
     6   0.171324492379170,0.360761573048139,0.467913934572691,
     6   0.467913934572691,0.360761573048139,0.171324492379170,
     6   0.               ,0.               ,0.               ,
     7   0.129484966168870,0.279705391489277,0.381830050505119,
     7   0.417959183673469,0.381830050505119,0.279705391489277,
     7   0.129484966168870,0.               ,0.               ,
     8   0.101228536290376,0.222381034453374,0.313706645877887,
     8   0.362683783378362,0.362683783378362,0.313706645877887,
     8   0.222381034453374,0.101228536290376,0.               ,
     9   0.081274388361574,0.180648160694857,0.260610696402935,
     9   0.312347077040003,0.330239355001260,0.312347077040003,
     9   0.260610696402935,0.180648160694857,0.081274388361574/
C=======================================================================
        IGTYP = NINT(GEO(12,IPROP))
        IGMAT = IGEO(98,IPROP)
        MAX_NPTT   = 1
        IF(IGTYP == 51 .OR. IGTYP == 52) THEN
          DO ILAY=1,NLAY   !  NPT
            MAX_NPTT = MAX(MAX_NPTT ,ELBUF_STR%BUFLY(ILAY)%NPTT)
          ENDDO
          ALLOCATE(THK_IT(MAX_NPTT),POS_IT(MAX_NPTT))
        ELSE
          ALLOCATE(THK_IT(0),POS_IT(0))
        ENDIF
        NLAY_MAX = MAX(NLAY, NPT)
        ALLOCATE( LAYPOS(NLAY_MAX*MAX_NPTT,MVSIZ),RATIO_THKLY(NLAY_MAX*MAX_NPTT,MVSIZ) )
        IF ((IGTYP == 17 .OR. IGTYP == 51) .AND. IGMAT < 0) THEN
          DO I=LFT,LLT
            IF (NDRAPE == 0) THEN
              IF (THKE(I)== ZERO) THEN
                THK(I) = STACK%GEO(1,ISUBSTACK)
                THKE(I)= THK(I)
              ELSE
                THK(I)=THKE(I)
              ENDIF
              THKI(I) = STACK%GEO(1,ISUBSTACK)
            ELSEIF (NDRAPE > 0) THEN
              THK(I) = THKE(I)
              THKI(I)= THK(I)
            ENDIF ! IF (NDRAPE == 0)
c
            IF (NPERTURB /= 0) THEN
              DO J=1,NPERTURB
                IF (PERTURB(J) == 1 .AND.
     .               RNOISE(J,NUMELC+I+NFT) /= ZERO)  THEN
                  THK(I)  = THK(I)  * RNOISE(J,NUMELC+I+NFT)
                  THKE(I) = THKE(I) * RNOISE(J,NUMELC+I+NFT)
                  THKI(I) = THKI(I) * RNOISE(J,NUMELC+I+NFT)
                ENDIF
              ENDDO
            ENDIF
c
            RHO(I)= PM(89,IMAT)
            RHOCP(I) =  PM(69,IMAT)
            IF(THK(I)==ZERO.AND.IGTYP/=0)THEN
              CALL ANCMSG(MSGID=495,
     .                    MSGTYPE=MSGERROR,
     .                    ANMODE=ANINFO_BLIND_1,
     .                    I1=IXTG(6,I))
            ENDIF
          ENDDO
        ELSEIF (IGTYP == 52 .OR.
     .        ((IGTYP == 17 .OR. IGTYP == 51) .AND. IGMAT > 0)) THEN
          DO I=LFT,LLT
            IF (NDRAPE == 0) THEN
              IF (THKE(I)== ZERO) THEN
                THK(I) = STACK%GEO(1,ISUBSTACK)
                THKE(I)= THK(I)
              ELSE
                THK(I)=THKE(I)
              ENDIF
              THKI(I) = STACK%GEO(1,ISUBSTACK)
            ELSEIF (NDRAPE > 0) THEN
              THK(I) = THKE(I)
              THKI(I)= THK(I)
            ENDIF ! IF (NDRAPE == 0)
c
            IF (NPERTURB /= 0) THEN
              DO J=1,NPERTURB
                IF (PERTURB(J) == 1 .AND.
     .               RNOISE(J,NUMELC+I+NFT) /= ZERO) THEN
                  THK(I)  = THK(I)  * RNOISE(J,NUMELC+I+NFT)
                  THKE(I) = THKE(I) * RNOISE(J,NUMELC+I+NFT)
                  THKI(I) = THKI(I) * RNOISE(J,NUMELC+I+NFT)
                ENDIF
              ENDDO
            ENDIF
c
            RHO(I)   = STACK%PM(1,ISUBSTACK)
            RHOCP(I) = STACK%PM(15,ISUBSTACK)* RHO(I)/STACK%PM(14,ISUBSTACK)
            IF (THK(I) == ZERO .AND. IGTYP /= 0) THEN
              CALL ANCMSG(MSGID=495,
     .                    MSGTYPE=MSGERROR,
     .                    ANMODE=ANINFO_BLIND_1,
     .                    I1=IXTG(6,I))
            ENDIF
          ENDDO
        ELSE
          DO I=LFT,LLT
            IF(THKE(I)==ZERO)THEN
              THK(I) =GEO(1,IPROP)
              THKE(I)=THK(I)
            ELSE
              THK(I)=THKE(I)
            ENDIF
            THKI(I) =GEO(1,IPROP)
c
            IF (NPERTURB /= 0) THEN
              DO J=1,NPERTURB
                IF (PERTURB(J) == 1 .AND.
     .               RNOISE(J,NUMELC+I+NFT) /= ZERO) THEN
                  THK(I)  = THK(I)  * RNOISE(J,NUMELC+I+NFT)
                  THKE(I) = THKE(I) * RNOISE(J,NUMELC+I+NFT)
                  THKI(I) = THKI(I) * RNOISE(J,NUMELC+I+NFT)
                ENDIF
              ENDDO
            ENDIF
c
            RHO(I)= PM(89,IMAT)
            RHOCP(I) =  PM(69,IMAT)
            IF(THK(I)==ZERO.AND.IGTYP/=0)THEN
              CALL ANCMSG(MSGID=495,
     .                    MSGTYPE=MSGERROR,
     .                    ANMODE=ANINFO_BLIND_1,
     .                    I1=IXTG(6,I))
            ENDIF
          ENDDO
        ENDIF
C----------------------------------------------
C     MASSES ELEMENTAIRES
C----------------------------------------------
        IF (IGTYP==11 .OR. IGTYP==16) THEN
          IPTHK = 300
          IPPOS = 400
          IPMAT = 100
          DO I=LFT,LLT
            EM(I)=ZERO
            DO N=1,NPT
              I1=IPTHK+N
              I2=IPMAT+N
              THICKT = GEO(200,IPROP)
              THKLY  = GEO(I1,IPROP)*THICKT
              MATLY  = IGEO(I2,IPROP)
              EM(I)  = EM(I) + PM(89,MATLY)*THKLY*AREA(I)
            ENDDO
C---      Test stability condition for mass
            MST = RHO(I)*THK(I)*AREA(I)
            DMS = ABS(EM(I)-MST)/MST
C! for igty=11 ---> IGMAT=-1 (old G-mat), 1 for new g-mat
C! for igtyp=16 --->IGMAT= 0
C! for igtyp=17 --->IGMAT= 0
            IF(IGMAT <= 0) THEN
              IF (DMS > EM02) THEN
                IDPROP = NINT(GEO(40,IPROP))
                CALL FRETITL2(TITR,
     .                     IGEO(NPROPGI-LTITR+1,IPROP),LTITR)
                CALL ANCMSG(MSGID=519,
     .                   MSGTYPE=MSGWARNING,
     .                   ANMODE=ANINFO_BLIND_2,
     .                   I1=IGEO(1,IPROP),
     .                   C1=TITR,I2=IXTG(NIXTG,NFT+I),
     .                   R1=EM(I),R2=MST)
              ENDIF
            ENDIF
          ENDDO
        ELSEIF (IGTYP == 17) THEN
          IPPID  = 2
          IPMAT   = IPPID + NPT
          IPANG  =  1
          IPTHK  =  IPANG + NPT
          IPPOS  =  IPTHK + NPT
          NTHK   =  IPPOS + NPT
          IF(IDRAPE == 0) THEN
            DO I=LFT,LLT
              EM(I)=ZERO
              DO N=1,NPT
                I1=IPTHK + N
                I2=IPPID + N
                I3=IPMAT + N
                THICKT = STACK%GEO(1,ISUBSTACK)
                THKLY  = STACK%GEO(I1,ISUBSTACK)*THICKT
                MATLY  = STACK%IGEO(I3,ISUBSTACK)
                EM(I)  = EM(I) + PM(89,MATLY)*THKLY*AREA(I)
              ENDDO
C---         Test stability condition for mass
              MST = RHO(I)*THK(I)*AREA(I)
              DMS = ABS(EM(I)-MST)/MST
              IF(IGMAT < 0) THEN
                IF (DMS > EM02) THEN
                  IDPROP = NINT(GEO(40,IPROP))
                  CALL FRETITL2(TITR,
     .                          IGEO(NPROPGI-LTITR+1,IPROP),LTITR)
                  CALL ANCMSG(MSGID=519,
     .                        MSGTYPE=MSGWARNING,
     .                        ANMODE=ANINFO_BLIND_2,
     .                        I1=IGEO(1,IPROP),
     .                        C1=TITR,I2=IXTG(NIXTG,NFT+I),
     .                        R1=EM(I),R2=MST)
                ENDIF
              ENDIF
            ENDDO
          ELSE ! IDRAPE > 0
            DO I=LFT,LLT
              EM(I)=ZERO
              IE = INDX(NFT + I)
              IF(IE == 0) THEN
                DO N=1,NPT
                  I1=IPTHK + N
                  I2=IPPID + N
                  I3=IPMAT + N
                  THICKT = STACK%GEO(1,ISUBSTACK)
                  THKLY  = STACK%GEO(I1,ISUBSTACK)*THICKT
                  MATLY  = STACK%IGEO(I3,ISUBSTACK)
                  EM(I)  = EM(I) + PM(89,MATLY)*THKLY*AREA(I)
                ENDDO
              ELSE
                DO N=1,NPT
                  I1=IPTHK + N
                  I2=IPPID + N
                  I3=IPMAT + N
                  IP = DRAPE(IE)%INDX_PLY(N)
                  IF(IP > 0) THEN
                    DRAPE_PLY => DRAPE(IE)%DRAPE_PLY(IP)
                    NSLICE = DRAPE_PLY%NSLICE
                    THINNING = DRAPE_PLY%RDRAPE(1,1) ! one slice by layer
                    THICKT = STACK%GEO(1,ISUBSTACK)
                    THKLY  = STACK%GEO(I1,ISUBSTACK)*THICKT
                    MATLY  = STACK%IGEO(I3,ISUBSTACK)
                    THKLY  = THKLY*THINNING
                    MATLY  = STACK%IGEO(I3,ISUBSTACK)
                    EM(I)  = EM(I) + PM(89,MATLY)*THKLY*AREA(I)
                  ELSE
                    THICKT = STACK%GEO(1,ISUBSTACK)
                    THKLY  = STACK%GEO(I1,ISUBSTACK)*THICKT
                    MATLY  = STACK%IGEO(I3,ISUBSTACK)
                    MATLY  = STACK%IGEO(I3,ISUBSTACK)
                    EM(I)  = EM(I) + PM(89,MATLY)*THKLY*AREA(I)
                  ENDIF
                ENDDO
              ENDIF ! IE
C---         Test stability condition for mass
              MST = RHO(I)*THK(I)*AREA(I)
              DMS = ABS(EM(I)-MST)/MST
              IF(IGMAT < 0) THEN
                IF (DMS > EM02) THEN
                  IDPROP = NINT(GEO(40,IPROP))
                  CALL FRETITL2(TITR,
     .                          IGEO(NPROPGI-LTITR+1,IPROP),LTITR)
                  CALL ANCMSG(MSGID=519,
     .                        MSGTYPE=MSGWARNING,
     .                        ANMODE=ANINFO_BLIND_2,
     .                        I1=IGEO(1,IPROP),
     .                        C1=TITR,I2=IXTG(NIXTG,NFT+I),
     .                        R1=EM(I),R2=MST)
                ENDIF
              ENDIF
            ENDDO
          ENDIF ! DRAPE
        ELSEIF (IGTYP == 51 .OR. IGTYP == 52) THEN
          IPANG  =  1
          IPPID  =  2
          IPMAT  =  IPPID + NLAY  !  NPT
          IPTHK  =  IPANG + NLAY  !  NPT
          IPPOS  =  IPTHK + NLAY  !  NPT
          NTHK   =  IPPOS + NLAY  !  NPT
          !
          IPOS = IGEO(99,IPROP)
          THICKT  = STACK%GEO(1,ISUBSTACK)
          ZSHIFT = GEO(199,IPROP)
          IF(IPOS == 0 )THEN
            ZSHIFT = - HALF
          ELSEIF(IPOS == 2 )THEN
            ZSHIFT =  -  ZSHIFT /MAX(THICKT,EM20)
          ELSEIF(IPOS == 3) THEN
            ZSHIFT = -ONE
          ELSEIF(IPOS == 4) THEN
            ZSHIFT = ZERO
          ENDIF
          IF(IDRAPE == 0) THEN
            DO I=LFT,LLT
              EM(I)=ZERO
              IPT_ALL = 0
              DO ILAY=1,NLAY  !  NPT
                NPTT = ELBUF_STR%BUFLY(ILAY)%NPTT
                I1=IPTHK + ILAY
                I2=IPPID + ILAY
                I3=IPMAT + ILAY
                THICKT = STACK%GEO(1,ISUBSTACK)
                THKLY  = STACK%GEO(I1,ISUBSTACK)*THICKT
                IPID_LY= STACK%IGEO(I2,ISUBSTACK)
                MATLY  = STACK%IGEO(I3,ISUBSTACK)
                IPID = STACK%IGEO(IPPID,ISUBSTACK)  ! or IPID = IX(NIXC-1,NFT+I)
                IINT = IGEO(47,IPROP)
                IF (IINT == 1) THEN !  uniform distribution - by default
                  DO IT=1,NPTT
                    THK_IT(IT) = THKLY/NPTT  ! equally distribution of NPTT through layer
                  ENDDO
                ELSEIF (IINT == 2) THEN  !  Gauss distribution
                  DO IT=1,NPTT
                    THK_IT(IT) = HALF*THKLY*W_GAUSS(IT,NPTT)
                  ENDDO
                ENDIF
                DO IT=1,NPTT
                  EMS_NPTT = PM(89,MATLY)*THK_IT(IT)*AREA(I)
                  EM(I) = EM(I) + EMS_NPTT
                ENDDO
C----
              ENDDO
C---         Test stability condition for mass
              MST = RHO(I)*THK(I)*AREA(I)
              DMS = ABS(EM(I)-MST)/MST
              IF(IGTYP == 51 .AND.IGMAT < 0) THEN
                IF (DMS > EM02) THEN
                  IDPROP = NINT(GEO(40,IPROP))
                  CALL FRETITL2(TITR,
     .                          IGEO(NPROPGI-LTITR+1,IPROP),LTITR)
                  CALL ANCMSG(MSGID=519,
     .                        MSGTYPE=MSGWARNING,
     .                        ANMODE=ANINFO_BLIND_2,
     .                        I1=IGEO(1,IPROP),
     .                        C1=TITR,I2=IXTG(NIXTG,NFT+I),
     .                        R1=EM(I),R2=MST)
                ENDIF
              ENDIF
            ENDDO
          ELSE ! IDRAPE > 0
            DO I=LFT,LLT
              EM(I)=ZERO
              IE = INDX(NFT + I)
              IPT_ALL = 0
              IF(IE == 0) THEN
                DO ILAY=1,NLAY  !  NPT
                  NPTT = ELBUF_STR%BUFLY(ILAY)%NPTT
                  I1=IPTHK + ILAY
                  I2=IPPID + ILAY
                  I3=IPMAT + ILAY
                  IPID = STACK%IGEO(I2,ISUBSTACK)  ! or IPID = IX(NIXC-1,NFT+I)
                  IINT = IGEO(47,IPROP)
                  THICKT  = STACK%GEO(1,ISUBSTACK)
                  IF(IINT == 1) THEN
                    THICKT = STACK%GEO(1,ISUBSTACK)
                    THKLY  = STACK%GEO(I1,ISUBSTACK)*THICKT
                    IPID_LY= STACK%IGEO(I2,ISUBSTACK)
                    MATLY  = STACK%IGEO(I3,ISUBSTACK)
                    DO IT=1,NPTT
                      THK_IT(IT) = THKLY/NPTT  ! uniform distribution of NPTT through layer
                    ENDDO
                  ELSEIF(IINT == 2)THEN
                    THICKT = STACK%GEO(1,ISUBSTACK)
                    THKLY  = STACK%GEO(I1,ISUBSTACK)*THICKT
                    IPID_LY= STACK%IGEO(I2,ISUBSTACK)
                    MATLY  = STACK%IGEO(I3,ISUBSTACK)
                    DO IT=1,NPTT
                      THK_IT(IT) = HALF*THKLY*W_GAUSS(IT,NPTT)  ! G Gauss distribution of NPTT through layer
                    ENDDO
                  ENDIF ! iint
C
                  DO IT=1,NPTT
                    EMS_NPTT = PM(89,MATLY)*THK_IT(IT)*AREA(I)
                    EM(I) = EM(I) + EMS_NPTT
                  ENDDO
                ENDDO ! NLAY
              ELSE ! IE > 0
                IPT_ALL = 0
                DO ILAY=1,NLAY  !  NPT
                  NPTT = ELBUF_STR%BUFLY(ILAY)%NPTT
                  I1=IPTHK + ILAY
                  I2=IPPID + ILAY
                  I3=IPMAT + ILAY
                  IPID = STACK%IGEO(I2,ISUBSTACK)  ! or IPID = IX(NIXC-1,NFT+I)
                  IINT = IGEO(47,IPROP)
                  THICKT  = STACK%GEO(1,ISUBSTACK)
                  IP = DRAPE(IE)%INDX_PLY(ILAY)
                  IF(IP == 0) THEN
                    THICKT = STACK%GEO(1,ISUBSTACK)
                    THKLY  = STACK%GEO(I1,ISUBSTACK)*THICKT
                    IPID_LY= STACK%IGEO(I2,ISUBSTACK)
                    MATLY  = STACK%IGEO(I3,ISUBSTACK)
                    IF(IINT == 1) THEN
                      DO IT=1,NPTT
                        THK_IT(IT)   = THKLY/NPTT
                      ENDDO  ! nptt
                    ELSEIF(IINT == 2)THEN
                      DO IT=1,NPTT
                        THK_IT(IT)   = HALF*THKLY*W_GAUSS(IT,NPTT) ! Gauss distribution of NPTT through laye    r
                      ENDDO  ! nptt
                    ENDIF ! iint
                  ELSE
                    DRAPE_PLY => DRAPE(IE)%DRAPE_PLY(IP)
                    NSLICE = DRAPE_PLY%NSLICE !NPTT
                    THICKT = STACK%GEO(1,ISUBSTACK)
                    THKLY  = STACK%GEO(I1,ISUBSTACK)*THICKT
                    IPID_LY= STACK%IGEO(I2,ISUBSTACK)
                    MATLY  = STACK%IGEO(I3,ISUBSTACK)
                    IF(IINT == 1) THEN
                      DO IT=1,NPTT
                        THINNING     = DRAPE_PLY%RDRAPE(IT,1)
                        THK_IT(IT)   = THKLY*THINNING/NPTT
                      ENDDO  ! nptt
                    ELSEIF(IINT == 2)THEN
                      DO IT=1,NPTT
                        THINNING = DRAPE_PLY%RDRAPE(IT,1)
                        THK_IT(IT)   = HALF*THKLY*W_GAUSS(IT,NPTT)*THINNING ! Gauss distribution of NPTT through layer
                      ENDDO  ! nptt
                    ENDIF ! iint
                  ENDIF
                  DO IT=1,NPTT
                    EMS_NPTT = PM(89,MATLY)*THK_IT(IT)*AREA(I)
                    EM(I) = EM(I) + EMS_NPTT
                  ENDDO
                ENDDO ! NLAY
              ENDIF    !IE
C---         Test stability condition for mass
              MST = RHO(I)*THK(I)*AREA(I)
              DMS = ABS(EM(I)-MST)/MST
              IF(IGTYP == 51 .AND.IGMAT < 0) THEN
                IF (DMS > EM02) THEN
                  IDPROP = NINT(GEO(40,IPROP))
                  CALL FRETITL2(TITR,
     .                          IGEO(NPROPGI-LTITR+1,IPROP),LTITR)
                  CALL ANCMSG(MSGID=519,
     .                        MSGTYPE=MSGWARNING,
     .                        ANMODE=ANINFO_BLIND_2,
     .                        I1=IGEO(1,IPROP),
     .                        C1=TITR,I2=IXTG(NIXTG,NFT+I),
     .                        R1=EM(I),R2=MST)
                ENDIF
              ENDIF
            ENDDO ! LFT,LLT
          ENDIF  ! IDRAPE
        ELSE
          DO I=LFT,LLT
            EM(I) = RHO(I)*THK(I)*AREA(I)
          ENDDO
          DO I=LFT,LLT
            EMSCP(I)=RHOCP(I)*THK(I)*AREA(I)
          ENDDO
        ENDIF
C----------------------------------------------
C     INERTIES ELEMENTAIRES
C----------------------------------------------
        IF (IGTYP==11 .OR. IGTYP==16) THEN
          DO I=LFT,LLT
            XI(I)=ZERO
            DO N=1,NPT
              I1=IPTHK+N
              I2=IPMAT+N
              I3=IPPOS+N
              THICKT= GEO(200,IPROP)
              THKLY = GEO(I1,IPROP)*THICKT
              POSLY = GEO(I3,IPROP)*THICKT
              MATLY = IGEO(I2,IPROP)
              MSL   = PM(89,MATLY)*THKLY*AREA(I)
              XIL = MSL*(AREA(I)/NINE_OVER_2+THKLY*THKLY*ONE_OVER_12+POSLY*POSLY)
              XI(I) = XI(I) + XIL
            ENDDO
C---      Test stability condition for inertia
            IF(IGMAT <= 0) THEN !
              INS = EM(I)*(AREA(I)/NINE_OVER_2+THK(I)*THK(I)*ONE_OVER_12)
              DMS = ABS(XI(I)-INS)/INS
              IF(DMS > EM02) THEN
                IDPROP = NINT(GEO(40,IPROP))
                CALL FRETITL2(TITR,
     .                    IGEO(NPROPGI-LTITR+1,IPROP),LTITR)
                CALL ANCMSG(MSGID=520,
     .                  MSGTYPE=MSGWARNING,
     .                  ANMODE=ANINFO_BLIND_2,
     .                  I1=IGEO(1,IPROP),
     .                  C1=TITR,I2=IXTG(NIXTG,NFT+I))
              ENDIF
            ENDIF
          ENDDO
        ELSEIF (IGTYP == 17) THEN
          IF(IDRAPE == 0 ) THEN
            DO I=LFT,LLT
              XI(I)=ZERO
              DO N=1,NPT
                I1 =IPPID + N
                I2 =IPMAT + N
                I3 =IPTHK + N
                I4 =IPPOS + N
                THICKT= STACK%GEO(1,ISUBSTACK)
                THKLY = STACK%GEO(I3,ISUBSTACK)*THICKT
                POSLY = STACK%GEO(I4,ISUBSTACK)*THICKT
                MATLY = STACK%IGEO(I2,ISUBSTACK)
                MSL   = PM(89,MATLY)*THKLY*AREA(I)
                XIL = MSL*(AREA(I)/NINE_OVER_2+THKLY*THKLY*ONE_OVER_12+POSLY*POSLY)
                XI(I) = XI(I) + XIL
              ENDDO
C---         Test stability condition for inertia
              INS = EM(I)*(AREA(I)/NINE_OVER_2+THK(I)*THK(I)*ONE_OVER_12)
              DMS = ABS(XI(I)-INS)/INS

              IF(IGMAT <= 0) THEN !
                IF(DMS > EM02) THEN
                  IDPROP = NINT(GEO(40,IPROP))
                  CALL FRETITL2(TITR,
     .                          IGEO(NPROPGI-LTITR+1,IPROP),LTITR)
                  CALL ANCMSG(MSGID=520,
     .                        MSGTYPE=MSGWARNING,
     .                        ANMODE=ANINFO_BLIND_2,
     .                        I1=IGEO(1,IPROP),
     .                        C1=TITR,I2=IXTG(NIXTG,NFT+I))
                ENDIF
              ENDIF
            ENDDO
          ELSE !IDRAPE > 0
            DO I=LFT,LLT
              XI(I)=ZERO
              IE = INDX(NFT + I)
              IF(IE == 0) THEN
                DO N=1,NPT
                  I1 =IPPID + N
                  I2 =IPMAT + N
                  I3 =IPTHK + N
                  I4 =IPPOS + N
                  THICKT= STACK%GEO(1,ISUBSTACK)
                  THKLY = STACK%GEO(I3,ISUBSTACK)*THICKT
                  POSLY = STACK%GEO(I4,ISUBSTACK)*THICKT
                  MATLY = STACK%IGEO(I2,ISUBSTACK)
                  MSL   = PM(89,MATLY)*THKLY*AREA(I)
                  XIL = MSL*(AREA(I)/NINE_OVER_2+THKLY*THKLY*ONE_OVER_12+POSLY*POSLY)
                  XI(I) = XI(I) + XIL
                ENDDO
              ELSE ! IE > 0
                DO N=1,NPT
                  IP = DRAPE(IE)%INDX_PLY(N)
                  I1 =IPPID + N
                  I2 =IPMAT + N
                  I3 =IPTHK + N
                  I4 =IPPOS + N
                  IF(IP == 0 ) THEN
                    THKLY = STACK%GEO(I3,ISUBSTACK)*THICKT
                    MATLY = STACK%IGEO(I2,ISUBSTACK)
                    RATIO_THKLY(N,I) = THKLY/THK(I)
                  ELSE
                    DRAPE_PLY => DRAPE(IE)%DRAPE_PLY(IP)
                    THINNING = DRAPE_PLY%RDRAPE(1,1)
                    THKLY = STACK%GEO(I3,ISUBSTACK)*THICKT
                    THKLY = THKLY*THINNING
                    MATLY = STACK%IGEO(I2,ISUBSTACK)
                    RATIO_THKLY(N,I) = THKLY/THK(I)
                  ENDIF
                  IF (N == 1) THEN
                    LAYPOS(N,I) = -HALF + HALF*RATIO_THKLY(N,I)
                  ELSE
                    LAYPOS(N,I) = LAYPOS(N-1,I) + HALF*(RATIO_THKLY(N,I)+RATIO_THKLY(N-1,I))
                  ENDIF ! IF (N == 1)
                  POSLY = LAYPOS(N,I)*THK(I)
                  MSL   = PM(89,MATLY)*THKLY*AREA(I)
                  XIL = MSL*(AREA(I)/NINE_OVER_2+THKLY*THKLY*ONE_OVER_12+POSLY*POSLY)
                  XI(I) = XI(I) + XIL
                ENDDO
              ENDIF ! IE
C---         Test stability condition for inertia
              INS = EM(I)*(AREA(I)/NINE_OVER_2+THK(I)*THK(I)*ONE_OVER_12)
              DMS = ABS(XI(I)-INS)/INS

              IF(IGMAT <= 0) THEN !
                IF(DMS > EM02) THEN
                  IDPROP = NINT(GEO(40,IPROP))
                  CALL FRETITL2(TITR,
     .                          IGEO(NPROPGI-LTITR+1,IPROP),LTITR)
                  CALL ANCMSG(MSGID=520,
     .                        MSGTYPE=MSGWARNING,
     .                        ANMODE=ANINFO_BLIND_2,
     .                        I1=IGEO(1,IPROP),
     .                        C1=TITR,I2=IXTG(NIXTG,NFT+I))
                ENDIF
              ENDIF
            ENDDO
          ENDIF ! IDRAPE
        ELSEIF (IGTYP == 51 .OR. IGTYP == 52) THEN
          IPANG  =  1
          IPPID  =  2
          IPMAT  =  IPPID + NLAY  !  NPT
          IPTHK  =  IPANG + NLAY  !  NPT
          IPPOS  =  IPTHK + NLAY  !  NPT
          NTHK   =  IPPOS + NLAY  !  NPT
          !!

          IPOS = IGEO(99,IPROP)
          THICKT  = STACK%GEO(1,ISUBSTACK)
          ZSHIFT  = GEO(199,IPROP)
          IF(IPOS == 0 )THEN
            ZSHIFT = - HALF
          ELSEIF(IPOS == 2 )THEN
            ZSHIFT =  -  ZSHIFT /MAX(THICKT,EM20)
          ELSEIF(IPOS == 3) THEN
            ZSHIFT = -ONE
          ELSEIF(IPOS == 4) THEN
            ZSHIFT = ZERO
          ENDIF
          IF(IDRAPE == 0) THEN
            DO I=LFT,LLT
              XI(I)=ZERO
              IPT_ALL = 0
              DO ILAY=1,NLAY  !  NPT
                NPTT = ELBUF_STR%BUFLY(ILAY)%NPTT
                I1 =IPPID + ILAY
                I2 =IPMAT + ILAY
                I3 =IPTHK + ILAY
                I4 =IPPOS + ILAY
                IPID_LY = STACK%IGEO(I1,ISUBSTACK)
                MATLY = STACK%IGEO(I2,ISUBSTACK)
                IINT = IGEO(47,IPROP)
                IF(IINT == 1) THEN
                  THICKT  = STACK%GEO(1,ISUBSTACK)
                  THKLY   = STACK%GEO(I3,ISUBSTACK)*THICKT
                  DO IT=1,NPTT
                    IPT = IPT_ALL + IT
                    THK_IT(IT) =  THKLY/NPTT
                    RATIO_THKLY(IPT,I) = THK_IT(IT)/THK(I)
                    IF (IPT == 1) THEN
                      LAYPOS(IPT,I) = ZSHIFT + HALF*RATIO_THKLY(IPT,I)
                    ELSE
                      LAYPOS(IPT,I) = LAYPOS(IPT-1,I)  +
     .                       HALF*(RATIO_THKLY(IPT,I)+RATIO_THKLY(IPT-1,I))
                    ENDIF ! IF (N == 1)
                    POS_IT(IT) = LAYPOS(IPT,I)*THK(I)
                  ENDDO
                ELSEIF(IINT == 2) THEN
                  THICKT  = STACK%GEO(1,ISUBSTACK)
                  THKLY   = STACK%GEO(I3,ISUBSTACK)*THICKT
                  DO IT=1,NPTT
                    IPT = IPT_ALL + IT
                    THK_IT(IT) =  HALF*THKLY*W_GAUSS(IT,NPTT)
                    RATIO_THKLY(IPT,I) = THK_IT(IT)/THK(I)
                    IF (IPT == 1) THEN
                      LAYPOS(IPT,I) = ZSHIFT + HALF*RATIO_THKLY(IPT,I)
                    ELSE
                      LAYPOS(IPT,I) = LAYPOS(IPT-1,I) +
     .                        HALF*(RATIO_THKLY(IPT,I)+RATIO_THKLY(IPT-1,I))
                    ENDIF ! IF (N == 1)
                    POS_IT(IT) = LAYPOS(IPT,I)*THK(I)
                  ENDDO
                ENDIF  ! iit
                DO IT=1,NPTT
                  MS_NPTT = PM(89,MATLY)*THK_IT(IT)*AREA(I)
                  XIL = MS_NPTT*(AREA(I)/NINE_OVER_2+THK_IT(IT)*THK_IT(IT)*ONE_OVER_12 +
     .                               POS_IT(IT)*POS_IT(IT))
                  XI(I) = XI(I) + XIL
                ENDDO
                IPT_ALL = IPT_ALL + NPTT
              ENDDO ! NLAY
C             Test stability condition for inertia
              INS = EM(I)*(AREA(I)/NINE_OVER_2+THK(I)*THK(I)*ONE_OVER_12)
              DMS = ABS(XI(I)-INS)/INS
              IF(IGTYP == 51 .AND. IGMAT < 0) THEN
                IF (DMS > EM02) THEN
                  IDPROP = NINT(GEO(40,IPROP))
                  CALL FRETITL2(TITR,
     .                            IGEO(NPROPGI-LTITR+1,IPROP),LTITR)
                  CALL ANCMSG(MSGID=520,
     .                         MSGTYPE=MSGWARNING,
     .                         ANMODE=ANINFO_BLIND_2,
     .                         I1=IGEO(1,IPROP),
     .                         C1=TITR,I2=IXTG(NIXTG,NFT+I))
                ENDIF
              ENDIF
            ENDDO ! LFT,LLT
          ELSE ! IDRAPE > 0
            DO I=LFT,LLT
              XI(I)=ZERO
              IPT_ALL = 0
              IE = INDX(NFT + I)
              IF(IE == 0) THEN
                DO ILAY=1,NLAY  !  NPT
                  NPTT = ELBUF_STR%BUFLY(ILAY)%NPTT
                  I1 =IPPID + ILAY
                  I2 =IPMAT + ILAY
                  I3 =IPTHK + ILAY
                  I4 =IPPOS + ILAY
                  IPID_LY = STACK%IGEO(I1,ISUBSTACK)
                  MATLY = STACK%IGEO(I2,ISUBSTACK)
                  IINT = IGEO(47,IPROP)
                  IF(IINT == 1) THEN
                    THICKT  = STACK%GEO(1,ISUBSTACK)
                    THKLY   = STACK%GEO(I3,ISUBSTACK)*THICKT
                    DO IT=1,NPTT
                      IPT = IPT_ALL + IT
                      THK_IT(IT) =  THKLY/NPTT
                      RATIO_THKLY(IPT,I) = THK_IT(IT)/THK(I)
                      IF (IPT == 1) THEN
                        LAYPOS(IPT,I) = ZSHIFT + HALF*RATIO_THKLY(IPT,I)
                      ELSE
                        LAYPOS(IPT,I) = LAYPOS(IPT-1,I)  +
     .                         HALF*(RATIO_THKLY(IPT,I)+RATIO_THKLY(IPT-1,I))
                      ENDIF ! IF (N == 1)
                      POS_IT(IT) = LAYPOS(IPT,I)*THK(I)
                    ENDDO
                  ELSEIF(IINT == 2) THEN
                    THICKT  = STACK%GEO(1,ISUBSTACK)
                    THKLY   = STACK%GEO(I3,ISUBSTACK)*THICKT
                    DO IT=1,NPTT
                      IPT = IPT_ALL + IT
                      THK_IT(IT) =  HALF*THKLY*W_GAUSS(IT,NPTT)
                      RATIO_THKLY(IPT,I) = THK_IT(IT)/THK(I)
                      IF (IPT == 1) THEN
                        LAYPOS(IPT,I) = ZSHIFT + HALF*RATIO_THKLY(IPT,I)
                      ELSE
                        LAYPOS(IPT,I) = LAYPOS(IPT-1,I) +
     .                          HALF*(RATIO_THKLY(IPT,I)+RATIO_THKLY(IPT-1,I))
                      ENDIF ! IF (N == 1)
                      POS_IT(IT) = LAYPOS(IPT,I)*THK(I)
                    ENDDO
                  ENDIF  ! iit
                  DO IT=1,NPTT
                    MS_NPTT = PM(89,MATLY)*THK_IT(IT)*AREA(I)
                    XIL = MS_NPTT*(AREA(I)/NINE_OVER_2+THK_IT(IT)*THK_IT(IT)*ONE_OVER_12 +
     .                                 POS_IT(IT)*POS_IT(IT))
                    XI(I) = XI(I) + XIL
                  ENDDO
                  IPT_ALL = IPT_ALL + NPTT
                ENDDO ! NLAY
              ELSE ! IE > 0
                IPT_ALL = 0
                DO ILAY=1,NLAY  !  NPT
                  IP = DRAPE(IE)%INDX_PLY(ILAY)
                  NPTT = ELBUF_STR%BUFLY(ILAY)%NPTT
                  I1 =IPPID + ILAY
                  I2 =IPMAT + ILAY
                  I3 =IPTHK + ILAY
                  I4 =IPPOS + ILAY
                  IPID_LY = STACK%IGEO(I1,ISUBSTACK)
                  MATLY = STACK%IGEO(I2,ISUBSTACK)
                  IINT = IGEO(47,IPROP)
                  IF(IP > 0) THEN
                    DRAPE_PLY => DRAPE(IE)%DRAPE_PLY(IP)
                    NSLICE = DRAPE_PLY%NSLICE ! NPTT
                    IF(IINT == 1) THEN
                      THICKT  = STACK%GEO(1,ISUBSTACK)
                      THKLY   = STACK%GEO(I3,ISUBSTACK)*THICKT
                      DO IT=1,NPTT
                        IPT = IPT_ALL + IT
                        THINNING = DRAPE_PLY%RDRAPE(IT,1)
                        THK_IT(IT) =  THKLY*THINNING/NPTT
                        RATIO_THKLY(IPT,I) = THK_IT(IT)/THK(I)
                        IF (IPT == 1) THEN
                          LAYPOS(IPT,I) = ZSHIFT + HALF*RATIO_THKLY(IPT,I)
                        ELSE
                          LAYPOS(IPT,I) = LAYPOS(IPT-1,I)  +
     .                           HALF*(RATIO_THKLY(IPT,I)+RATIO_THKLY(IPT-1,I))
                        ENDIF ! IF (N == 1)
                        POS_IT(IT) = LAYPOS(IPT,I)*THK(I)
                      ENDDO
                    ELSEIF(IINT == 2) THEN
                      THICKT  = STACK%GEO(1,ISUBSTACK)
                      THKLY   = STACK%GEO(I3,ISUBSTACK)*THICKT
                      DO IT=1,NPTT
                        IPT = IPT_ALL + IT
                        THINNING = DRAPE_PLY%RDRAPE(IT,1)
                        THK_IT(IT) =  HALF*THKLY*W_GAUSS(IT,NPTT)*THINNING
                        RATIO_THKLY(IPT,I) = THK_IT(IT)/THK(I)
                        IF (IPT == 1) THEN
                          LAYPOS(IPT,I) = ZSHIFT + HALF*RATIO_THKLY(IPT,I)
                        ELSE
                          LAYPOS(IPT,I) = LAYPOS(IPT-1,I) +
     .                            HALF*(RATIO_THKLY(IPT,I)+RATIO_THKLY(IPT-1,I))
                        ENDIF ! IF (N == 1)
                        POS_IT(IT) = LAYPOS(IPT,I)*THK(I)
                      ENDDO
                    ENDIF  ! iit
                  ELSE ! IP == 0
                    IF(IINT == 1) THEN
                      THICKT  = STACK%GEO(1,ISUBSTACK)
                      THKLY   = STACK%GEO(I3,ISUBSTACK)*THICKT
                      DO IT=1,NPTT
                        IPT = IPT_ALL + IT
                        THK_IT(IT) =  THKLY/NPTT
                        RATIO_THKLY(IPT,I) = THK_IT(IT)/THK(I)
                        IF (IPT == 1) THEN
                          LAYPOS(IPT,I) = ZSHIFT + HALF*RATIO_THKLY(IPT,I)
                        ELSE
                          LAYPOS(IPT,I) = LAYPOS(IPT-1,I)  +
     .                           HALF*(RATIO_THKLY(IPT,I)+RATIO_THKLY(IPT-1,I))
                        ENDIF ! IF (N == 1)
                        POS_IT(IT) = LAYPOS(IPT,I)*THK(I)
                      ENDDO
                    ELSEIF(IINT == 2) THEN
                      THICKT  = STACK%GEO(1,ISUBSTACK)
                      THKLY   = STACK%GEO(I3,ISUBSTACK)*THICKT
                      DO IT=1,NPTT
                        IPT = IPT_ALL + IT
                        THK_IT(IT) =  HALF*THKLY*W_GAUSS(IT,NPTT)
                        RATIO_THKLY(IPT,I) = THK_IT(IT)/THK(I)
                        IF (IPT == 1) THEN
                          LAYPOS(IPT,I) = ZSHIFT + HALF*RATIO_THKLY(IPT,I)
                        ELSE
                          LAYPOS(IPT,I) = LAYPOS(IPT-1,I) +
     .                            HALF*(RATIO_THKLY(IPT,I)+RATIO_THKLY(IPT-1,I))
                        ENDIF ! IF (N == 1)
                        POS_IT(IT) = LAYPOS(IPT,I)*THK(I)
                      ENDDO
                    ENDIF  ! iit
                  ENDIF
                  DO IT=1,NPTT
                    MS_NPTT = PM(89,MATLY)*THK_IT(IT)*AREA(I)
                    XIL = MS_NPTT*(AREA(I)/NINE_OVER_2+THK_IT(IT)*THK_IT(IT)*ONE_OVER_12 +
     .                                 POS_IT(IT)*POS_IT(IT))
                    XI(I) = XI(I) + XIL
                  ENDDO
                  IPT_ALL = IPT_ALL + NPTT
                ENDDO ! NLAY
              ENDIF  ! ie
C             Test stability condition for inertia
              INS = EM(I)*(AREA(I)/NINE_OVER_2+THK(I)*THK(I)*ONE_OVER_12)
              DMS = ABS(XI(I)-INS)/INS
              IF(IGTYP == 51 .AND. IGMAT < 0) THEN
                IF (DMS > EM02) THEN
                  IDPROP = NINT(GEO(40,IPROP))
                  CALL FRETITL2(TITR,
     .                            IGEO(NPROPGI-LTITR+1,IPROP),LTITR)
                  CALL ANCMSG(MSGID=520,
     .                         MSGTYPE=MSGWARNING,
     .                         ANMODE=ANINFO_BLIND_2,
     .                         I1=IGEO(1,IPROP),
     .                         C1=TITR,I2=IXTG(NIXTG,NFT+I))
                ENDIF
              ENDIF
            ENDDO ! LFT,LLT
          ENDIF ! IDRAPE
        ELSE
          DO I=LFT,LLT
C---        Exact:  XIN = EMS*(AREA(I)/(6.*SQRT(3.))+THK(I)*THK(I)/12.)
C---        Compatibilite avec coques 4 noeuds:
            XI(I)=EM(I)*(AREA(I)/NINE_OVER_2+THK(I)*THK(I)*ONE_OVER_12)
          ENDDO
        ENDIF
C---------------------------------------------------
C     INITIALISATION DES MASSES ET INERTIES NODALES
C---------------------------------------------------
C   mass and inertia spmd parith/on + adaptive meshing
        IF(JTHE == 0 ) THEN
          DO I=LFT,LLT
            MSTG(I)=EM(I)
            INTG(I)=XI(I)
          END DO
        ELSE
          DO I=LFT,LLT
            MSTG(I)=EM(I)
            INTG(I)=XI(I)
            MCPTG(I) = EMSCP(I)
          END DO
        ENDIF
C
        DO I=LFT,LLT
          AA = X2(I)
          A2 = AA**2
          B2 = (X2(I)-X3(I))**2 + Y3(I)**2
          BB = SQRT(B2)
          C2 = X3(I)**2 + Y3(I)**2
          CC = SQRT(C2)
          P1 = ACOS((A2 + C2 - B2)/(TWO * AA * CC)) / PI
          P2 = ACOS((A2 + B2 - C2)/(TWO * AA * BB)) / PI
          P3 = ACOS((B2 + C2 - A2)/(TWO * BB * CC)) / PI
          PTG(1,I)=P1
          PTG(2,I)=P2
          PTG(3,I)=P3
          IF ( ( ((A2 + C2 - B2)/(TWO * AA * CC) <= -ONE) .OR.
     .         ( (A2 + C2 - B2)/(TWO * AA * CC) >= ONE) .OR.
     .         ( (A2 + B2 - C2)/(TWO * AA * BB) <= -ONE) .OR.
     .         ( (A2 + B2 - C2)/(TWO * AA * BB) >= ONE) .OR.
     .         ( (B2 + C2 - A2)/(TWO * BB * CC) <= -ONE) .OR.
     .         ( (B2 + C2 - A2)/(TWO * BB * CC) >= ONE)) .AND.
     .                LSH3TRIM /= 0    ) THEN
            IF (SH3TRIM(NFT+I) /= -1) CALL ANCMSG(MSGID=750,
     .                   MSGTYPE=MSGERROR,
     .                   ANMODE=ANINFO,
     .                   I1=IXTG(6,NFT+I))
          ELSEIF ( ( ((A2 + C2 - B2)/(TWO * AA * CC) <= -ONE) .OR.
     .         ( (A2 + C2 - B2)/(TWO * AA * CC) >= ONE) .OR.
     .         ( (A2 + B2 - C2)/(TWO * AA * BB) <= -ONE) .OR.
     .         ( (A2 + B2 - C2)/(TWO * AA * BB) >= ONE) .OR.
     .         ( (B2 + C2 - A2)/(TWO * BB * CC) <= -ONE) .OR.
     .         ( (B2 + C2 - A2)/(TWO * BB * CC) >= ONE)) .AND.
     .                LSH3TRIM == 0      ) THEN
            CALL ANCMSG(MSGID=750,
     .                  MSGTYPE=MSGERROR,
     .                  ANMODE=ANINFO,
     .                  I1=IXTG(6,NFT+I))
          ENDIF
        END DO
C
C-----
        IF(NADMESH==0)THEN
          IF (NXREF == 0) THEN
            DO I=LFT,LLT
              P1 = PTG(1,I)
              P2 = PTG(2,I)
              P3 = PTG(3,I)
              EMS = EM(I)
              XIN = XI(I)
C
              I1 = IX1(I)
              I2 = IX2(I)
              I3 = IX3(I)
C
              IP=IPART(I)
              PARTSAV(1,IP)=PARTSAV(1,IP) + EMS
              PARTSAV(2,IP)=PARTSAV(2,IP) + EMS*
     .             (X(1,I1)*P1+X(1,I2)*P2+X(1,I3)*P3)
              PARTSAV(3,IP)=PARTSAV(3,IP) + EMS*
     .             (X(2,I1)*P1+X(2,I2)*P2+X(2,I3)*P3)
              PARTSAV(4,IP)=PARTSAV(4,IP) + EMS*
     .             (X(3,I1)*P1+X(3,I2)*P2+X(3,I3)*P3)
              XX = (X(1,I1)*X(1,I1)*P1+X(1,I2)*X(1,I2)*P2
     .             +X(1,I3)*X(1,I3)*P3)
              XY = (X(1,I1)*X(2,I1)*P1+X(1,I2)*X(2,I2)*P2
     .             +X(1,I3)*X(2,I3)*P3)
              YY = (X(2,I1)*X(2,I1)*P1+X(2,I2)*X(2,I2)*P2
     .             +X(2,I3)*X(2,I3)*P3)
              YZ = (X(2,I1)*X(3,I1)*P1+X(2,I2)*X(3,I2)*P2
     .             +X(2,I3)*X(3,I3)*P3)
              ZZ = (X(3,I1)*X(3,I1)*P1+X(3,I2)*X(3,I2)*P2
     .             +X(3,I3)*X(3,I3)*P3)
              ZX = (X(3,I1)*X(1,I1)*P1+X(3,I2)*X(1,I2)*P2
     .             +X(3,I3)*X(1,I3)*P3)
              PARTSAV(5,IP) =PARTSAV(5,IP) + XIN + EMS * (YY+ZZ)
              PARTSAV(6,IP) =PARTSAV(6,IP) + XIN + EMS * (ZZ+XX)
              PARTSAV(7,IP) =PARTSAV(7,IP) + XIN + EMS * (XX+YY)
              PARTSAV(8,IP) =PARTSAV(8,IP)  - EMS * XY
              PARTSAV(9,IP) =PARTSAV(9,IP)  - EMS * YZ
              PARTSAV(10,IP)=PARTSAV(10,IP) - EMS * ZX
C
              PARTSAV(11,IP)=PARTSAV(11,IP) + EMS*
     .             (V(1,I1)*P1+V(1,I2)*P2+V(1,I3)*P3)
              PARTSAV(12,IP)=PARTSAV(12,IP) + EMS*
     .             (V(2,I1)*P1+V(2,I2)*P2+V(2,I3)*P3)
              PARTSAV(13,IP)=PARTSAV(13,IP) + EMS*
     .             (V(3,I1)*P1+V(3,I2)*P2+V(3,I3)*P3)
              PARTSAV(14,IP)=PARTSAV(14,IP) + HALF * EMS *
     .           ((V(1,I1)*V(1,I1)+V(2,I1)*V(2,I1)+V(3,I1)*V(3,I1))*P1
     .           +(V(1,I2)*V(1,I2)+V(2,I2)*V(2,I2)+V(3,I2)*V(3,I2))*P2
     .           +(V(1,I3)*V(1,I3)+V(2,I3)*V(2,I3)+V(3,I3)*V(3,I3))*P3)
            ENDDO
          ELSE
            DO I=LFT,LLT
              P1 = PTG(1,I)
              P2 = PTG(2,I)
              P3 = PTG(3,I)
              EMS = EM(I)
              XIN = XI(I)
C
              I1 = IX1(I)
              I2 = IX2(I)
              I3 = IX3(I)
              IP=IPART(I)
C
              PARTSAV(1,IP)=PARTSAV(1,IP) + EMS
              PARTSAV(2,IP)=PARTSAV(2,IP) + EMS*
     .             (P1*XREFTG(1,1,I)+P2*XREFTG(2,1,I)+P3*XREFTG(3,1,I))
              PARTSAV(3,IP)=PARTSAV(3,IP) + EMS*
     .             (P1*XREFTG(1,2,I)+P2*XREFTG(2,2,I)+P3*XREFTG(3,2,I))
              PARTSAV(4,IP)=PARTSAV(4,IP) + EMS*
     .             (P1*XREFTG(1,3,I)+P2*XREFTG(2,3,I)+P3*XREFTG(3,3,I))
              XX = P1*XREFTG(1,1,I)**2
     .           + P2*XREFTG(2,1,I)**2
     .           + P3*XREFTG(3,1,I)**2
              XY = P1*XREFTG(1,1,I)*XREFTG(1,2,I)
     .           + P2*XREFTG(2,1,I)*XREFTG(2,2,I)
     .           + P3*XREFTG(3,1,I)*XREFTG(3,2,I)
              YY = P1*XREFTG(1,2,I)**2
     .           + P2*XREFTG(2,2,I)**2
     .           + P3*XREFTG(3,2,I)**2
              YZ = P1*XREFTG(1,2,I)*XREFTG(1,3,I)
     .           + P2*XREFTG(2,2,I)*XREFTG(2,3,I)
     .           + P3*XREFTG(3,2,I)*XREFTG(3,3,I)
              ZZ = P1*XREFTG(1,3,I)**2
     .           + P2*XREFTG(2,3,I)**2
     .           + P3*XREFTG(3,3,I)**2
              ZX = P1*XREFTG(1,3,I)*XREFTG(1,1,I)
     .           + P2*XREFTG(2,3,I)*XREFTG(2,1,I)
     .           + P3*XREFTG(3,3,I)*XREFTG(3,1,I)
C
              PARTSAV(5,IP) =PARTSAV(5,IP) + XIN + EMS * (YY+ZZ)
              PARTSAV(6,IP) =PARTSAV(6,IP) + XIN + EMS * (ZZ+XX)
              PARTSAV(7,IP) =PARTSAV(7,IP) + XIN + EMS * (XX+YY)
              PARTSAV(8,IP) =PARTSAV(8,IP)  - EMS * XY
              PARTSAV(9,IP) =PARTSAV(9,IP)  - EMS * YZ
              PARTSAV(10,IP)=PARTSAV(10,IP) - EMS * ZX
C
              PARTSAV(11,IP)=PARTSAV(11,IP) + EMS*
     .             (V(1,I1)*P1+V(1,I2)*P2+V(1,I3)*P3)
              PARTSAV(12,IP)=PARTSAV(12,IP) + EMS*
     .             (V(2,I1)*P1+V(2,I2)*P2+V(2,I3)*P3)
              PARTSAV(13,IP)=PARTSAV(13,IP) + EMS*
     .             (V(3,I1)*P1+V(3,I2)*P2+V(3,I3)*P3)
              PARTSAV(14,IP)=PARTSAV(14,IP) + HALF * EMS *
     .           ((V(1,I1)*V(1,I1)+V(2,I1)*V(2,I1)+V(3,I1)*V(3,I1))*P1
     .           +(V(1,I2)*V(1,I2)+V(2,I2)*V(2,I2)+V(3,I2)*V(3,I2))*P2
     .           +(V(1,I3)*V(1,I3)+V(2,I3)*V(2,I3)+V(3,I3)*V(3,I3))*P3)
            ENDDO
          ENDIF
        ELSEIF (ISTATCND==0)THEN
          IF (NXREF == 0) THEN
            DO I=LFT,LLT
              N=NFT+I
              IF(SH3TREE(3,N) >= 0)THEN
                P1 = PTG(1,I)
                P2 = PTG(2,I)
                P3 = PTG(3,I)
                EMS = EM(I)
                XIN = XI(I)
C
                I1 = IX1(I)
                I2 = IX2(I)
                I3 = IX3(I)
                IP = IPART(I)
C
                PARTSAV(1,IP)=PARTSAV(1,IP) + EMS
                PARTSAV(2,IP)=PARTSAV(2,IP) + EMS*
     .               (X(1,I1)*P1+X(1,I2)*P2+X(1,I3)*P3)
                PARTSAV(3,IP)=PARTSAV(3,IP) + EMS*
     .               (X(2,I1)*P1+X(2,I2)*P2+X(2,I3)*P3)
                PARTSAV(4,IP)=PARTSAV(4,IP) + EMS*
     .               (X(3,I1)*P1+X(3,I2)*P2+X(3,I3)*P3)
                XX = (X(1,I1)*X(1,I1)*P1+X(1,I2)*X(1,I2)*P2
     .               +X(1,I3)*X(1,I3)*P3)
                XY = (X(1,I1)*X(2,I1)*P1+X(1,I2)*X(2,I2)*P2
     .               +X(1,I3)*X(2,I3)*P3)
                YY = (X(2,I1)*X(2,I1)*P1+X(2,I2)*X(2,I2)*P2
     .               +X(2,I3)*X(2,I3)*P3)
                YZ = (X(2,I1)*X(3,I1)*P1+X(2,I2)*X(3,I2)*P2
     .               +X(2,I3)*X(3,I3)*P3)
                ZZ = (X(3,I1)*X(3,I1)*P1+X(3,I2)*X(3,I2)*P2
     .               +X(3,I3)*X(3,I3)*P3)
                ZX = (X(3,I1)*X(1,I1)*P1+X(3,I2)*X(1,I2)*P2
     .               +X(3,I3)*X(1,I3)*P3)
C
                PARTSAV(5,IP) =PARTSAV(5,IP) + XIN + EMS * (YY+ZZ)
                PARTSAV(6,IP) =PARTSAV(6,IP) + XIN + EMS * (ZZ+XX)
                PARTSAV(7,IP) =PARTSAV(7,IP) + XIN + EMS * (XX+YY)
                PARTSAV(8,IP) =PARTSAV(8,IP)  - EMS * XY
                PARTSAV(9,IP) =PARTSAV(9,IP)  - EMS * YZ
                PARTSAV(10,IP)=PARTSAV(10,IP) - EMS * ZX
C
                PARTSAV(11,IP)=PARTSAV(11,IP) + EMS*
     .               (V(1,I1)*P1+V(1,I2)*P2+V(1,I3)*P3)
                PARTSAV(12,IP)=PARTSAV(12,IP) + EMS*
     .               (V(2,I1)*P1+V(2,I2)*P2+V(2,I3)*P3)
                PARTSAV(13,IP)=PARTSAV(13,IP) + EMS*
     .               (V(3,I1)*P1+V(3,I2)*P2+V(3,I3)*P3)
                PARTSAV(14,IP)=PARTSAV(14,IP) + HALF * EMS *
     .             ((V(1,I1)*V(1,I1)+V(2,I1)*V(2,I1)+V(3,I1)*V(3,I1))*P1
     .             +(V(1,I2)*V(1,I2)+V(2,I2)*V(2,I2)+V(3,I2)*V(3,I2))*P2
     .             +(V(1,I3)*V(1,I3)+V(2,I3)*V(2,I3)+V(3,I3)*V(3,I3))*P3)
              END IF
            END DO
          ELSE
            DO I=LFT,LLT
              N=NFT+I
              IF (SH3TREE(3,N) >= 0)THEN
                P1 = PTG(1,I)
                P2 = PTG(2,I)
                P3 = PTG(3,I)
                EMS = EM(I)
                XIN = XI(I)
C
                I1 = IX1(I)
                I2 = IX2(I)
                I3 = IX3(I)
                IP = IPART(I)
C
                PARTSAV(1,IP)=PARTSAV(1,IP) + EMS
                PARTSAV(2,IP)=PARTSAV(2,IP) + EMS*
     .               (P1*XREFTG(1,1,I)+P2*XREFTG(2,1,I)+P3*XREFTG(3,1,I))
                PARTSAV(3,IP)=PARTSAV(3,IP) + EMS*
     .               (P1*XREFTG(1,2,I)+P2*XREFTG(2,2,I)+P3*XREFTG(3,2,I))
                PARTSAV(4,IP)=PARTSAV(4,IP) + EMS*
     .               (P1*XREFTG(1,3,I)+P2*XREFTG(2,3,I)+P3*XREFTG(3,3,I))
                XX = P1*XREFTG(1,1,I)**2
     .             + P2*XREFTG(2,1,I)**2
     .             + P3*XREFTG(3,1,I)**2
                XY = P1*XREFTG(1,1,I)*XREFTG(1,2,I)
     .             + P2*XREFTG(2,1,I)*XREFTG(2,2,I)
     .             + P3*XREFTG(3,1,I)*XREFTG(3,2,I)
                YY = P1*XREFTG(1,2,I)**2
     .             + P2*XREFTG(2,2,I)**2
     .             + P3*XREFTG(3,2,I)**2
                YZ = P1*XREFTG(1,2,I)*XREFTG(1,3,I)
     .             + P2*XREFTG(2,2,I)*XREFTG(2,3,I)
     .             + P3*XREFTG(3,2,I)*XREFTG(3,3,I)
                ZZ = P1*XREFTG(1,3,I)**2
     .             + P2*XREFTG(2,3,I)**2
     .             + P3*XREFTG(3,3,I)**2
                ZX = P1*XREFTG(1,3,I)*XREFTG(1,1,I)
     .             + P2*XREFTG(2,3,I)*XREFTG(2,1,I)
     .             + P3*XREFTG(3,3,I)*XREFTG(3,1,I)
C
                PARTSAV(5,IP) =PARTSAV(5,IP) + XIN + EMS * (YY+ZZ)
                PARTSAV(6,IP) =PARTSAV(6,IP) + XIN + EMS * (ZZ+XX)
                PARTSAV(7,IP) =PARTSAV(7,IP) + XIN + EMS * (XX+YY)
                PARTSAV(8,IP) =PARTSAV(8,IP)  - EMS * XY
                PARTSAV(9,IP) =PARTSAV(9,IP)  - EMS * YZ
                PARTSAV(10,IP)=PARTSAV(10,IP) - EMS * ZX
C
                PARTSAV(11,IP)=PARTSAV(11,IP) + EMS*
     .               (V(1,I1)*P1+V(1,I2)*P2+V(1,I3)*P3)
                PARTSAV(12,IP)=PARTSAV(12,IP) + EMS*
     .               (V(2,I1)*P1+V(2,I2)*P2+V(2,I3)*P3)
                PARTSAV(13,IP)=PARTSAV(13,IP) + EMS*
     .               (V(3,I1)*P1+V(3,I2)*P2+V(3,I3)*P3)
                PARTSAV(14,IP)=PARTSAV(14,IP) + HALF * EMS *
     .             ((V(1,I1)*V(1,I1)+V(2,I1)*V(2,I1)+V(3,I1)*V(3,I1))*P1
     .             +(V(1,I2)*V(1,I2)+V(2,I2)*V(2,I2)+V(3,I2)*V(3,I2))*P2
     .             +(V(1,I3)*V(1,I3)+V(2,I3)*V(2,I3)+V(3,I3)*V(3,I3))*P3)
              END IF
            END DO
          ENDIF
        ELSE
          IF (NXREF == 0) THEN
            DO I=LFT,LLT
              N=NFT+I
              IF(SH3TREE(3,N) == 0 .OR. SH3TREE(3,N) == -1 )THEN
                P1 = PTG(1,I)
                P2 = PTG(2,I)
                P3 = PTG(3,I)
                EMS = EM(I)
                XIN = XI(I)
C
                I1 = IX1(I)
                I2 = IX2(I)
                I3 = IX3(I)
                IP = IPART(I)
C
                PARTSAV(1,IP)=PARTSAV(1,IP) + EMS
                PARTSAV(2,IP)=PARTSAV(2,IP) + EMS*
     .               (X(1,I1)*P1+X(1,I2)*P2+X(1,I3)*P3)
                PARTSAV(3,IP)=PARTSAV(3,IP) + EMS*
     .               (X(2,I1)*P1+X(2,I2)*P2+X(2,I3)*P3)
                PARTSAV(4,IP)=PARTSAV(4,IP) + EMS*
     .               (X(3,I1)*P1+X(3,I2)*P2+X(3,I3)*P3)
                XX = (X(1,I1)*X(1,I1)*P1+X(1,I2)*X(1,I2)*P2
     .               +X(1,I3)*X(1,I3)*P3)
                XY = (X(1,I1)*X(2,I1)*P1+X(1,I2)*X(2,I2)*P2
     .               +X(1,I3)*X(2,I3)*P3)
                YY = (X(2,I1)*X(2,I1)*P1+X(2,I2)*X(2,I2)*P2
     .               +X(2,I3)*X(2,I3)*P3)
                YZ = (X(2,I1)*X(3,I1)*P1+X(2,I2)*X(3,I2)*P2
     .               +X(2,I3)*X(3,I3)*P3)
                ZZ = (X(3,I1)*X(3,I1)*P1+X(3,I2)*X(3,I2)*P2
     .               +X(3,I3)*X(3,I3)*P3)
                ZX = (X(3,I1)*X(1,I1)*P1+X(3,I2)*X(1,I2)*P2
     .               +X(3,I3)*X(1,I3)*P3)
C
                PARTSAV(5,IP) =PARTSAV(5,IP) + XIN + EMS * (YY+ZZ)
                PARTSAV(6,IP) =PARTSAV(6,IP) + XIN + EMS * (ZZ+XX)
                PARTSAV(7,IP) =PARTSAV(7,IP) + XIN + EMS * (XX+YY)
                PARTSAV(8,IP) =PARTSAV(8,IP)  - EMS * XY
                PARTSAV(9,IP) =PARTSAV(9,IP)  - EMS * YZ
                PARTSAV(10,IP)=PARTSAV(10,IP) - EMS * ZX
C
                PARTSAV(11,IP)=PARTSAV(11,IP) + EMS*
     .               (V(1,I1)*P1+V(1,I2)*P2+V(1,I3)*P3)
                PARTSAV(12,IP)=PARTSAV(12,IP) + EMS*
     .               (V(2,I1)*P1+V(2,I2)*P2+V(2,I3)*P3)
                PARTSAV(13,IP)=PARTSAV(13,IP) + EMS*
     .               (V(3,I1)*P1+V(3,I2)*P2+V(3,I3)*P3)
                PARTSAV(14,IP)=PARTSAV(14,IP) + HALF * EMS *
     .             ((V(1,I1)*V(1,I1)+V(2,I1)*V(2,I1)+V(3,I1)*V(3,I1))*P1
     .             +(V(1,I2)*V(1,I2)+V(2,I2)*V(2,I2)+V(3,I2)*V(3,I2))*P2
     .             +(V(1,I3)*V(1,I3)+V(2,I3)*V(2,I3)+V(3,I3)*V(3,I3))*P3)
              END IF
            END DO
          ELSE
            DO I=LFT,LLT
              N=NFT+I
              IF(SH3TREE(3,N) == 0 .OR. SH3TREE(3,N) == -1 )THEN
                P1 = PTG(1,I)
                P2 = PTG(2,I)
                P3 = PTG(3,I)
                EMS = EM(I)
                XIN = XI(I)
C
                I1 = IX1(I)
                I2 = IX2(I)
                I3 = IX3(I)
                IP = IPART(I)
C
                PARTSAV(1,IP)=PARTSAV(1,IP) + EMS
                PARTSAV(2,IP)=PARTSAV(2,IP) + EMS*
     .               (P1*XREFTG(1,1,I)+P2*XREFTG(2,1,I)+P3*XREFTG(3,1,I))
                PARTSAV(3,IP)=PARTSAV(3,IP) + EMS*
     .               (P1*XREFTG(1,2,I)+P2*XREFTG(2,2,I)+P3*XREFTG(3,2,I))
                PARTSAV(4,IP)=PARTSAV(4,IP) + EMS*
     .               (P1*XREFTG(1,3,I)+P2*XREFTG(2,3,I)+P3*XREFTG(3,3,I))
                XX = P1*XREFTG(1,1,I)**2
     .             + P2*XREFTG(2,1,I)**2
     .             + P3*XREFTG(3,1,I)**2
                XY = P1*XREFTG(1,1,I)*XREFTG(1,2,I)
     .             + P2*XREFTG(2,1,I)*XREFTG(2,2,I)
     .             + P3*XREFTG(3,1,I)*XREFTG(3,2,I)
                YY = P1*XREFTG(1,2,I)**2
     .             + P2*XREFTG(2,2,I)**2
     .             + P3*XREFTG(3,2,I)**2
                YZ = P1*XREFTG(1,2,I)*XREFTG(1,3,I)
     .             + P2*XREFTG(2,2,I)*XREFTG(2,3,I)
     .             + P3*XREFTG(3,2,I)*XREFTG(3,3,I)
                ZZ = P1*XREFTG(1,3,I)**2
     .             + P2*XREFTG(2,3,I)**2
     .             + P3*XREFTG(3,3,I)**2
                ZX = P1*XREFTG(1,3,I)*XREFTG(1,1,I)
     .             + P2*XREFTG(2,3,I)*XREFTG(2,1,I)
     .             + P3*XREFTG(3,3,I)*XREFTG(3,1,I)
C
                PARTSAV(5,IP) =PARTSAV(5,IP) + XIN + EMS * (YY+ZZ)
                PARTSAV(6,IP) =PARTSAV(6,IP) + XIN + EMS * (ZZ+XX)
                PARTSAV(7,IP) =PARTSAV(7,IP) + XIN + EMS * (XX+YY)
                PARTSAV(8,IP) =PARTSAV(8,IP)  - EMS * XY
                PARTSAV(9,IP) =PARTSAV(9,IP)  - EMS * YZ
                PARTSAV(10,IP)=PARTSAV(10,IP) - EMS * ZX
C
                PARTSAV(11,IP)=PARTSAV(11,IP) + EMS*
     .               (V(1,I1)*P1+V(1,I2)*P2+V(1,I3)*P3)
                PARTSAV(12,IP)=PARTSAV(12,IP) + EMS*
     .               (V(2,I1)*P1+V(2,I2)*P2+V(2,I3)*P3)
                PARTSAV(13,IP)=PARTSAV(13,IP) + EMS*
     .               (V(3,I1)*P1+V(3,I2)*P2+V(3,I3)*P3)
                PARTSAV(14,IP)=PARTSAV(14,IP) + HALF * EMS *
     .             ((V(1,I1)*V(1,I1)+V(2,I1)*V(2,I1)+V(3,I1)*V(3,I1))*P1
     .             +(V(1,I2)*V(1,I2)+V(2,I2)*V(2,I2)+V(3,I2)*V(3,I2))*P2
     .             +(V(1,I3)*V(1,I3)+V(2,I3)*V(2,I3)+V(3,I3)*V(3,I3))*P3)
              END IF
            END DO
          ENDIF
        END IF
C-----
        IF(JTHE > 0 ) THEN
          IF(NINTEMP > 0 ) THEN
            DO I= LFT,LLT
              IF(TEMP(IX1(I))== ZERO) TEMP(IX1(I)) = PM(79,IMAT)
              IF(TEMP(IX2(I))== ZERO) TEMP(IX2(I)) = PM(79,IMAT)
              IF(TEMP(IX3(I))== ZERO) TEMP(IX3(I)) = PM(79,IMAT)
            ENDDO
          ELSE
            DO I=LFT,LLT
              TEMP(IX1(I))=  PM(79,IMAT)
              TEMP(IX2(I))=  PM(79,IMAT)
              TEMP(IX3(I))=  PM(79,IMAT)
            ENDDO
          ENDIF
        ENDIF
C----------------------------------------------
C     INITIALISATION DES RIGIDITES NODALES POUR INTERFACES
C----------------------------------------------
        IF (IGTYP == 52 .OR.
     .        ((IGTYP == 17 .OR. IGTYP == 51) .AND. IGMAT > 0))THEN
          IF(I7STIFS/=0)THEN
            IF (IMACH/=3) THEN
              DO I=LFT,LLT
                ET=STACK%PM(2,ISUBSTACK)*THK(I)
                ETNOD(IX1(I))=ETNOD(IX1(I))+ET
                ETNOD(IX2(I))=ETNOD(IX2(I))+ET
                ETNOD(IX3(I))=ETNOD(IX3(I))+ET
                NSHNOD(IX1(I))=NSHNOD(IX1(I))+1
                NSHNOD(IX2(I))=NSHNOD(IX2(I))+1
                NSHNOD(IX3(I))=NSHNOD(IX3(I))+1
              ENDDO
            ELSE
              DO I=LFT,LLT
                ET=STACK%PM(2,ISUBSTACK)*THK(I)
                STTG(I)=ET
                NSHNOD(IX1(I))=NSHNOD(IX1(I))+1
                NSHNOD(IX2(I))=NSHNOD(IX2(I))+1
                NSHNOD(IX3(I))=NSHNOD(IX3(I))+1
              ENDDO
            ENDIF
          ENDIF
        ELSEIF(IGTYP == 11 .AND. IGMAT >  0) THEN
          IPGMAT = 700
          IF(I7STIFS/=0)THEN
            IF (IMACH/=3) THEN
              DO I=LFT,LLT
                ET=GEO(IPGMAT +2 ,IPROP)*THK(I)
                ETNOD(IX1(I))=ETNOD(IX1(I))+ET
                ETNOD(IX2(I))=ETNOD(IX2(I))+ET
                ETNOD(IX3(I))=ETNOD(IX3(I))+ET
                NSHNOD(IX1(I))=NSHNOD(IX1(I))+1
                NSHNOD(IX2(I))=NSHNOD(IX2(I))+1
                NSHNOD(IX3(I))=NSHNOD(IX3(I))+1
              ENDDO
            ELSE
              DO I=LFT,LLT
                ET=GEO(IPGMAT +2 ,IPROP)*THK(I)
                STTG(I)=ET
                NSHNOD(IX1(I))=NSHNOD(IX1(I))+1
                NSHNOD(IX2(I))=NSHNOD(IX2(I))+1
                NSHNOD(IX3(I))=NSHNOD(IX3(I))+1
              ENDDO
            ENDIF
          ENDIF
        ElSE
          IF(I7STIFS/=0)THEN
            IF (IMACH/=3) THEN
              DO I=LFT,LLT
                ET=PM(20,IMAT)*THK(I)
                ETNOD(IX1(I))=ETNOD(IX1(I))+ET
                ETNOD(IX2(I))=ETNOD(IX2(I))+ET
                ETNOD(IX3(I))=ETNOD(IX3(I))+ET
                NSHNOD(IX1(I))=NSHNOD(IX1(I))+1
                NSHNOD(IX2(I))=NSHNOD(IX2(I))+1
                NSHNOD(IX3(I))=NSHNOD(IX3(I))+1
              ENDDO
            ELSE
              DO I=LFT,LLT
                ET=PM(20,IMAT)*THK(I)
                STTG(I)=ET
                NSHNOD(IX1(I))=NSHNOD(IX1(I))+1
                NSHNOD(IX2(I))=NSHNOD(IX2(I))+1
                NSHNOD(IX3(I))=NSHNOD(IX3(I))+1
              ENDDO
            ENDIF
          ENDIF
        ENDIF ! IGMAT
        DEALLOCATE(THK_IT,POS_IT,   LAYPOS,RATIO_THKLY )
C-----------
        RETURN
      END
