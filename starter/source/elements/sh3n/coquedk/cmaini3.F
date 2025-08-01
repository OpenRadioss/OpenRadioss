Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2025 Altair Engineering Inc.
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
!||====================================================================
!||    cmaini3                ../starter/source/elements/sh3n/coquedk/cmaini3.F
!||--- called by ------------------------------------------------------
!||    cbainit3               ../starter/source/elements/shell/coqueba/cbainit3.F
!||    cdkinit3               ../starter/source/elements/sh3n/coquedk/cdkinit3.F
!||--- calls      -----------------------------------------------------
!||    ancmsg                 ../starter/source/output/message/message.F
!||    cm27in3                ../starter/source/materials/mat/mat027/cm27in3.F
!||    cm58in3                ../starter/source/materials/mat/mat058/cm58in3.F
!||    corthdir               ../starter/source/elements/shell/coque/corthdir.F
!||    corthini               ../starter/source/elements/shell/coque/corthini.F
!||    law158_init            ../starter/source/materials/mat/mat158/law158_init.F
!||--- uses       -----------------------------------------------------
!||    drape_mod              ../starter/share/modules1/drape_mod.F
!||    message_mod            ../starter/share/message_module/message_mod.F
!||    stack_mod              ../starter/share/modules1/stack_mod.F
!||====================================================================
      SUBROUTINE CMAINI3(ELBUF_STR,PM      ,GEO      ,NEL      ,NLAY      ,
     .                   SKEW     ,IGEO    ,IX       ,NIX      ,NUMEL     ,
     .                   NSIGSH   ,SIGSH   ,PTSH     ,IGTYP    ,IORTHLOC  ,
     .                   IPM      ,PROPID  ,ALDT     ,MAT_PARAM,
     .                   IR       ,IS      ,ISUBSTACK,STACK    ,IREP      ,
     .                   DRAPE    ,SHANG   ,GEO_STACK,IGEO_STACK,
     .                   IGMAT    ,IMAT    ,IPROP    ,NUMMAT ,
     .                   X1  ,X2  ,X3  ,X4  ,Y1  ,Y2  ,
     .                   Y3  ,Y4  ,Z1  ,Z2  ,Z3  ,Z4  ,
     .                   E1X, E2X, E3X, E1Y, E2Y, E3Y ,E1Z, E2Z, E3Z ,X  ,
     .                   NPT_ALL  ,IDRAPE   ,NUMEL_DRAPE , INDX)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MAT_ELEM_MOD            
      USE MESSAGE_MOD           
      USE STACK_MOD          
      USE DRAPE_MOD
      USE NAMES_AND_TITLES_MOD , ONLY : NCHARTITLE
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
#include      "vect01_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER ,INTENT(IN) :: NUMMAT
      INTEGER NEL,NLAY,NIX,NSIGSH,NUMEL,IGTYP,PROPID,IR,IS,IREP,IGMAT,IMAT,IPROP,
     .        IDRAPE ,NPT_ALL,NUMEL_DRAPE
      INTEGER IGEO(NPROPGI,*),IX(NIX,*),PTSH(*),IORTHLOC(MVSIZ),
     .        IPM(NPROPMI,*),ISUBSTACK,IGEO_STACK(*)
      INTEGER , DIMENSION(NUMEL_DRAPE) :: INDX
      my_real
     .   PM(*),GEO(NPROPG,*),SKEW(LSKEW,*),ALDT(*),
     .   SIGSH(NSIGSH,*),SHANG(*),GEO_STACK(*),X(3,*)
      TYPE(ELBUF_STRUCT_), TARGET :: ELBUF_STR
      TYPE (STACK_PLY) :: STACK
      my_real, DIMENSION(MVSIZ), INTENT(IN) :: E1X,E2X,E3X,E1Y,E2Y,E3Y,E1Z,E2Z,E3Z,
     .                                         X1,X2,X3,X4,Y1,Y2,Y3,Y4,Z1,Z2,Z3,Z4
      TYPE (MATPARAM_STRUCT_) ,DIMENSION(NUMMAT) ,INTENT(IN) :: MAT_PARAM
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER IT,IL,NUVAR,NPTT,ILAW,NUPARAM
      my_real
     .   VX(MVSIZ),VY(MVSIZ),VZ(MVSIZ),PHI1(NPT_ALL,MVSIZ),PHI2(NPT_ALL,MVSIZ)
      my_real
     .   COOR1(NPT_ALL,MVSIZ),COOR2(NPT_ALL,MVSIZ),COOR3(NPT_ALL,MVSIZ),
     .   COOR4(NPT_ALL,MVSIZ)
      CHARACTER(LEN=NCHARTITLE) :: TITR,TITR1
      my_real,
     .   DIMENSION(:),POINTER :: UVAR,DIR1,DIR2
      TYPE(G_BUFEL_) ,POINTER :: GBUF     
      TYPE(L_BUFEL_) ,POINTER :: LBUF     
      TYPE(BUF_LAY_) ,POINTER :: BUFLY   
      TYPE(DRAPE_) :: DRAPE(NUMELC_DRAPE + NUMELTG_DRAPE)
C=======================================================================
      COOR1 = ZERO
      COOR2 = ZERO
      COOR3 = ZERO
      COOR4 = ZERO
C
      GBUF  => ELBUF_STR%GBUF
C----
      CALL CORTHINI(
     .   LFT        ,LLT       ,NFT       ,NLAY       ,NUMEL      ,
     .   NSIGSH     ,NIX       ,IX        ,IGEO       ,GEO        ,
     .   SKEW       ,SIGSH     ,PTSH      ,PHI1       ,PHI2       ,
     .   VX         ,VY        ,VZ        ,COOR1      ,COOR2      ,
     .   COOR3      ,COOR4     ,IORTHLOC  ,ISUBSTACK  ,STACK      ,
     .   IREP       ,ELBUF_STR ,DRAPE     ,SHANG      ,X          ,
     .   GEO_STACK  ,E3X       ,E3Y        ,E3Z        ,
     .   GBUF%BETAORTH,X1      ,X2        ,Y1         ,Y2         ,
     .   Z1           ,Z2      ,NEL       ,GBUF%G_ADD_NODE,GBUF%ADD_NODE,
     .   NPT_ALL      ,IDRAPE  ,INDX)
c---     
C----      
      IF(IGTYP == 51 .OR. IGTYP == 52 .OR. IGMAT > 0) THEN
        CALL CORTHDIR(ELBUF_STR,
     .                IGEO     ,GEO       ,VX        ,VY      ,VZ       ,
     .                PHI1     ,PHI2      ,COOR1     ,COOR2   ,COOR3    ,
     .                COOR4    ,IORTHLOC  ,NLAY      ,IREP    ,ISUBSTACK,
     .                STACK    ,GEO_STACK ,IGEO_STACK,IR      ,IS       ,
     .                NEL      ,IMAT      ,IPROP     ,
     .                X1  ,X2  ,X3  ,X4  ,Y1  ,Y2  ,
     .                Y3  ,Y4  ,Z1  ,Z2  ,Z3  ,Z4  ,
     .                E1X, E2X, E3X, E1Y, E2Y, E3Y ,E1Z, E2Z, E3Z ,
     .                NPT_ALL  ,IDRAPE)
     
      ELSEIF (MTN == 27) THEN
        CALL CM27IN3(ELBUF_STR,
     .               GEO ,IGEO ,PM ,IPM ,IX ,NIX,
     .               NLAY,IR   ,IS ,IMAT )
      ELSEIF (MTN==15 .or. MTN==19 .or. MTN==25 .or. MTN >= 28) THEN
        IF (MTN == 19 .and. IGTYP /= 9) THEN
          CALL ANCMSG(MSGID=5,
     .                ANMODE=ANINFO,
     .                MSGTYPE=MSGERROR,
     .                I1=IX(1,1))
        ENDIF
c
        CALL CORTHDIR(ELBUF_STR,
     .                IGEO     ,GEO       ,VX        ,VY      ,VZ       ,
     .                PHI1     ,PHI2      ,COOR1     ,COOR2   ,COOR3    ,
     .                COOR4    ,IORTHLOC  ,NLAY      ,IREP    ,ISUBSTACK,
     .                STACK    ,GEO_STACK ,IGEO_STACK,IR      ,IS       ,
     .                NEL      ,IMAT      ,IPROP     ,
     .                X1  ,X2  ,X3  ,X4  ,Y1  ,Y2  ,
     .                Y3  ,Y4  ,Z1  ,Z2  ,Z3  ,Z4  ,
     .                E1X, E2X, E3X, E1Y, E2Y, E3Y ,E1Z, E2Z, E3Z ,
     .                NPT_ALL  ,IDRAPE )
      ENDIF
C-----
      IF ((MTN == 58 .or. MTN == 158) .AND. 
     .    IGTYP /= 16 .AND. IGTYP /= 51 .AND. IGTYP /= 52) THEN  
        CALL ANCMSG(MSGID=658,                                               
     .              MSGTYPE=MSGERROR,                                        
     .              ANMODE=ANINFO_BLIND_1,                                   
     .              I1=PROPID,                                               
     .              C1=TITR,                                                 
     .              I2=MTN,                                                  
     .              I3=IGTYP)                                                
      ELSEIF (MTN == 58 .or. MTN == 158 .OR. IGTYP == 51 .OR. IGTYP == 52) THEN
C
       IF (IDRAPE == 0) THEN
          DO IL = 1,NLAY
            NPTT  = ELBUF_STR%BUFLY(IL)%NPTT
            IMAT  = ELBUF_STR%BUFLY(IL)%IMAT
            ILAW  = ELBUF_STR%BUFLY(IL)%ILAW
            NUVAR = ELBUF_STR%BUFLY(IL)%NVAR_MAT
            DIR1 => ELBUF_STR%BUFLY(IL)%DIRA
            DIR2 => ELBUF_STR%BUFLY(IL)%DIRB
            NUPARAM = MAT_PARAM(IMAT)%NUPARAM
!
            IF (ILAW == 58) THEN
              DO IT=1,NPTT
                LBUF => ELBUF_STR%BUFLY(IL)%LBUF(IR,IS,IT)
                UVAR => ELBUF_STR%BUFLY(IL)%MAT(IR,IS,IT)%VAR 
                CALL CM58IN3(IREP     ,DIR1    ,DIR2    ,MAT_PARAM(IMAT)%UPARAM,
     .                       UVAR     ,ALDT    ,NEL     ,NUVAR   ,LBUF%ANG ,
     .                       X1  ,X2  ,X3  ,X4  ,Y1  ,Y2  ,
     .                       Y3  ,Y4  ,Z1  ,Z2  ,Z3  ,Z4  ,
     .                       E1X, E2X, E3X, E1Y, E2Y, E3Y ,E1Z, E2Z, E3Z )
              ENDDO
            ELSE IF (ILAW == 158) THEN
              DO IT=1,NPTT
                LBUF => ELBUF_STR%BUFLY(IL)%LBUF(IR,IS,IT)
                UVAR => ELBUF_STR%BUFLY(IL)%MAT(IR,IS,IT)%VAR 
                CALL LAW158_INIT(DIR1    ,DIR2    ,
     .               UVAR     ,ALDT    ,NEL     ,NUVAR   ,LBUF%ANG ,
     .               X1  ,X2  ,X3  ,X4  ,Y1  ,Y2  ,
     .               Y3  ,Y4  ,Z1  ,Z2  ,Z3  ,Z4  ,
     .               E1X, E2X, E3X, E1Y, E2Y, E3Y ,E1Z, E2Z, E3Z )
              ENDDO
            ENDIF ! ILAW
          ENDDO ! DO IL = 1,NLAY
        ELSE
          DO IL = 1,NLAY
            NPTT =  ELBUF_STR%BUFLY(IL)%NPTT
            IMAT =  ELBUF_STR%BUFLY(IL)%IMAT
            ILAW =  ELBUF_STR%BUFLY(IL)%ILAW
            NUVAR = ELBUF_STR%BUFLY(IL)%NVAR_MAT
            NUPARAM = MAT_PARAM(IMAT)%NUPARAM
!
            IF (ILAW == 58) THEN
              DO IT=1,NPTT
                LBUF => ELBUF_STR%BUFLY(IL)%LBUF(IR,IS,IT)
                UVAR => ELBUF_STR%BUFLY(IL)%MAT(IR,IS,IT)%VAR 
                DIR1 => ELBUF_STR%BUFLY(IL)%LBUF_DIR(IT)%DIRA
                DIR2 => ELBUF_STR%BUFLY(IL)%LBUF_DIR(IT)%DIRB
                CALL CM58IN3(IREP     ,DIR1    ,DIR2    ,MAT_PARAM(IMAT)%UPARAM,
     .                       UVAR     ,ALDT    ,NEL     ,NUVAR   ,LBUF%ANG ,
     .                       X1  ,X2  ,X3  ,X4  ,Y1  ,Y2  ,
     .                       Y3  ,Y4  ,Z1  ,Z2  ,Z3  ,Z4  ,
     .                       E1X, E2X, E3X, E1Y, E2Y, E3Y ,E1Z, E2Z, E3Z )
              ENDDO
            ELSE IF (ILAW == 158) THEN
              DO IT=1,NPTT
                LBUF => ELBUF_STR%BUFLY(IL)%LBUF(IR,IS,IT)
                UVAR => ELBUF_STR%BUFLY(IL)%MAT(IR,IS,IT)%VAR 
                DIR1 => ELBUF_STR%BUFLY(IL)%LBUF_DIR(IT)%DIRA
                DIR2 => ELBUF_STR%BUFLY(IL)%LBUF_DIR(IT)%DIRB
                CALL LAW158_INIT(DIR1    ,DIR2    ,
     .               UVAR     ,ALDT    ,NEL     ,NUVAR   ,LBUF%ANG ,
     .               X1  ,X2  ,X3  ,X4  ,Y1  ,Y2  ,
     .               Y3  ,Y4  ,Z1  ,Z2  ,Z3  ,Z4  ,
     .               E1X, E2X, E3X, E1Y, E2Y, E3Y ,E1Z, E2Z, E3Z )
              ENDDO
            ENDIF ! ILAW
          ENDDO ! DO IL = 1,NLAY
        ENDIF  
      ENDIF
C-----------
      RETURN
      END
