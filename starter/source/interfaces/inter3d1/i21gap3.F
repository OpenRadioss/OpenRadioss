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
Chd|  I21GAP3                       source/interfaces/inter3d1/i21gap3.F
Chd|-- called by -----------
Chd|        ININT3_THKVAR                 source/interfaces/inter3d1/inint3_thkvar.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        I4GMX3                        source/interfaces/inter3d1/i4gmx3.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE I21GAP3(
     1 X     ,IRECTS ,IRECTM ,NRTS  ,NRTM  ,
     2 GEO   ,PM    ,IXS     ,IXC   ,IXTG  ,
     3 NINT  ,NTY   ,NOINT   ,NSN   ,NSV   ,
     4 GAP   ,IGAP  ,GAP_S   ,GAPMIN,CRITER,
     5 GAPMAX,IELES ,STF     ,NMN   ,MSR   ,
     6 KNOD2ELS ,KNOD2ELC    ,KNOD2ELTG ,NOD2ELS ,NOD2ELC,
     7 NOD2ELTG ,THKNOD      ,
     8 IKINE    ,ITAB        ,INACTI   ,GAPSCALE,STFN   ,
     9 DEPTH    ,GAP_S0      ,AREA_S0  ,XM0     ,LXM    ,
     A LYM      ,LZM         ,INTTH    ,DRAD    ,IPARTS ,
     B IPARTC   ,IPARTG      ,THK_PART ,THKNOD0 ,ID     ,
     C TITR     ,DGAPLOAD    )
      USE MESSAGE_MOD
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
#include      "scr17_c.inc"
#include      "units_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NRTS, NRTM, NINT, NTY, NOINT,NSN, NMN, IGAP,
     .        INACTI, INTTH
      INTEGER IRECTS(4,*), IRECTM(4,*), IXS(NIXS,*), IXC(NIXC,*),
     .   NSV(*), IXTG(NIXTG,*), 
     .   KNOD2ELS(*), KNOD2ELC(*), KNOD2ELTG(*), NOD2ELS(*), NOD2ELC(*), 
     .   NOD2ELTG(*),IELES(*), 
     .   MSR(*), ITAB(*), IKINE(*), IPARTS(*), IPARTC(*), IPARTG(*)
C     REAL
      my_real  , INTENT(IN) :: DGAPLOAD
      my_real
     .   GAP,GAPMIN,CRITER, GAPMAX, GAPSCALE, DEPTH, DRAD, LXM, LYM, LZM
      my_real
     .   X(3,*), PM(NPROPM,*), GEO(NPROPG,*),
     .   GAP_S(*), THKNOD(*), STF(*), STFN(*), 
     .   GAP_S0(*), AREA_S0(*), XM0(3,*),THK_PART(*),THKNOD0(*)
      INTEGER ID
      CHARACTER*nchartitle,
     .   TITR
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER NDX, I, J, II, INRT, NELS, NELC, NELTG, NEL,
     .        N1,N2,N3,N4, IX, N, L, LLT, NN, IP, STAT
      INTEGER ITMP(NUMNOD)
C     REAL
      my_real
     .   DXM, GAPMX, GAPMN, AREA, DX,GAPS1,GAPS2, GAPM, DDX, 
     .   GAPTMP, XXX, YYY, ZZZ, X0, X1, Y0, Y1, Z0, Z1
      my_real
     .        X12(MVSIZ),Y12(MVSIZ),Z12(MVSIZ),
     .        X13(MVSIZ),Y13(MVSIZ),Z13(MVSIZ),
     .        X24(MVSIZ),Y24(MVSIZ),Z24(MVSIZ),
     .        NX(MVSIZ),NY(MVSIZ),NZ(MVSIZ),AA(MVSIZ)
      my_real, DIMENSION(:), ALLOCATABLE :: THK_PART_NODS
C--------------------------------------------------------------
      DXM=ZERO
      NDX=ZERO
      GAPMX=EP30
      GAPMN=EP30
      GAPS1=ZERO
      GAPS2=ZERO
C------------------------------------
C     GAP FACES SECONDS
C------------------------------------
      DO 250 I=1,NRTS
        GAPM  =ZERO
        INRT=I
        CALL I4GMX3(X,IRECTS,INRT,GAPMX)
  250 CONTINUE
C-------------------------------------------------------
C     THICKNESS PART ON SECND NODES FOR GAP CALCULATION
C-------------------------------------------------------
      IF(IGAP>=1)THEN
         ALLOCATE (THK_PART_NODS(NUMNOD)        ,STAT=stat)
         IF (STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                                     MSGTYPE=MSGERROR,
     .                                   C1='THK_PART_NODS')
         THK_PART_NODS(1:NUMNOD) = ZERO        
         DO I=1,NRTS
            NEL = IELES(I)
            IF(NEL<=NUMELS) THEN ! SOLID ELEMENT
               IP = IPARTS(NEL)
               DO N =1,4
                  NN = IRECTS(N,I)
                  THK_PART_NODS(NN) = MAX(THK_PART_NODS(N),THK_PART(IP))
               ENDDO
            ELSEIF(NEL<=(NUMELS+NUMELC)) THEN ! SHELL ELEMENT
               IP = IPARTC(NEL-NUMELS)
               DO N =1,4
                  NN = IRECTS(N,I)
                  THK_PART_NODS(NN) = MAX(THK_PART_NODS(N),THK_PART(IP))
               ENDDO
            ELSE         ! SHELL 3 ELEMENT
               IP = IPARTG(NEL-NUMELS-NUMELC)
               DO N =1,4
                  NN = IRECTS(N,I)
                  THK_PART_NODS(NN) = MAX(THK_PART_NODS(N),THK_PART(IP))
               ENDDO      
           ENDIF    
         ENDDO    
      ENDIF

C------------------------------------
C     GAP VARIABLE NOEUDS SECONDS
C------------------------------------
      IF(IGAP>=1)THEN
       DO I=1,NSN
         IF(THK_PART_NODS(NSV(I))/=ZERO) THEN  ! IF a contact thickness is defined
            DX      = THK_PART_NODS(NSV(I))*GAPSCALE
         ELSE
        DX      = THKNOD(NSV(I))*GAPSCALE
         ENDIF
        GAPM    = HALF*DX

        GAPS2 = MAX(GAPS2,GAPM)
        GAP_S(I)= GAPM
C                                =====
C       Gapmin >= t average of nodal thicknesses
C                                =====
        DXM=DXM+DX
        NDX=NDX+1

        THKNOD0(I)    = THKNOD(NSV(I)) ! Initial THICK NODE STORED
       ENDDO
       IF (ALLOCATED(THK_PART_NODS)) DEALLOCATE(THK_PART_NODS)  
      ENDIF

C------------------------------------
C     GAP 
C------------------------------------
       GAPMX=SQRT(GAPMX)
       IF(IGAP==0)THEN
C GAP FIXE
         IF(GAP<=ZERO)THEN
           DO I=1,NSN
            DX      = THKNOD(NSV(I))
C                                =====
C           Gap = t average of nodal thicknesses
C                                 =====
            DXM=DXM+DX
            NDX=NDX+1
           ENDDO
           GAP = HALF*DXM/NDX
           WRITE(IOUT,1000)GAP
         ENDIF
         GAPMIN = GAP
         GAPMAX = GAP
       ELSE
C SUP DES GAPS VARIABLES
         IF(GAP>ZERO)GAPMIN=GAP
         WRITE(IOUT,1000)GAPMIN
C
C        GAP n'est pas utilise pour Igap > 0 ; Gapmin peut etre egal a 0.
         IF(GAPMAX==ZERO)GAPMAX=EP30
         WRITE(IOUT,1500)GAPMAX
         GAP = MIN(GAP,GAPMAX)
       ENDIF
C---------------------------------------------
C---------------------------------------------
C SUP DES GAPS VARIABLES
       GAP = MIN(GAPMAX,MAX(GAPS2,GAPMIN))    
C---------------------------------------------
C
C Calcul du gap reel a utiliser lors du critere de retri
C
      IF (IGAP==0) THEN
        CRITER=GAP 
      ELSE
        CRITER=EP30
        DO I = 1, NSN
          CRITER = MIN(CRITER,GAP_S(I))
        ENDDO
        CRITER=MAX(CRITER,GAPMIN)
      ENDIF
C
       IF(DGAPLOAD > ZERO) CRITER=MAX(CRITER,EM01*(GAP + DGAPLOAD))
C
      IF(DEPTH==ZERO)THEN
C Valeur par defaut de Depth = max( sup des gaps , largeur des elts )
        DEPTH=MAX(GAP,GAPMX)
C cest encore necessaire au tri dans le starter
      ELSEIF(DEPTH<GAP)THEN
C Depth est tjrs superieur au gap (sup des gaps si variable)
        DEPTH=GAP
      END IF
      WRITE(IOUT,2000)DEPTH
C
      CRITER=MAX(CRITER,EM01*DEPTH)
C
      IF(DEPTH>GAPMX)THEN
       CALL ANCMSG(MSGID=687,
     .             MSGTYPE=MSGWARNING,
     .             ANMODE=ANINFO_BLIND_2,
     .                   I1=ID,
     .                   C1=TITR,
     .                R1=DEPTH,
     .                R2=GAPMX,
     .                I2=ID)
      ENDIF
C
      IF(INTTH/=0)THEN
        IF(DRAD==ZERO)THEN
          DRAD=MAX(GAP,GAPMX)
        ELSEIF(DRAD<GAP)THEN
          DRAD=GAP
        END IF
        WRITE(IOUT,2001)DRAD
C
        CRITER=MAX(CRITER,EM01*DRAD)
C
        IF(DRAD>GAPMX)THEN
         CALL ANCMSG(MSGID=918,
     .               MSGTYPE=MSGWARNING,
     .               ANMODE=ANINFO_BLIND_2,
     .               I1=ID,
     .               C1=TITR,
     .               R1=DRAD ,
     .               R2=GAPMX,
     .               I2=ID)
        END IF
      END IF
C------------------------------------
C     STiff cote main (1: active ; 0: inactive)
C------------------------------------
      DO I=1,NRTM
        STF(I)=ONE
      END DO
C---------------------------------------------
C     MISE A ONE DU MULTIPLICATEUR NODALE DES RIGIDITES 
C---------------------------------------------
      DO I=1,NSN
        STFN(I) = ONE
      END DO
C------------------------------------
      IF(IGAP==2)THEN
        DO I=1,NSN
          GAP_S0(I) = MIN(GAP_S(I),GAPMAX)
          GAP_S0(I) = MAX(GAPMIN  ,GAP_S0(I))
        END DO
        
        IF(INTTH == 0) THEN
          ITMP=0
          DO I=1,NSN
             II=NSV(I)
             ITMP(II)=I
          END DO
          DO N=1,NRTS,MVSIZ
C
           LLT=MIN(NRTS-N+1,MVSIZ)
C
           DO L=1,LLT
             I=N+L-1
C
             N1=IRECTS(1,I)
             N2=IRECTS(2,I)
             N3=IRECTS(3,I)
             N4=IRECTS(4,I)
             IF(N4/=N3)THEN
               X13(L)=X(1,N3)-X(1,N1)
               Y13(L)=X(2,N3)-X(2,N1)
               Z13(L)=X(3,N3)-X(3,N1)
               X24(L)=X(1,N4)-X(1,N2)
               Y24(L)=X(2,N4)-X(2,N2)
               Z24(L)=X(3,N4)-X(3,N2)
               NX(L)=Y13(L)*Z24(L)-Z13(L)*Y24(L)
               NY(L)=Z13(L)*X24(L)-X13(L)*Z24(L)
               NZ(L)=X13(L)*Y24(L)-Y13(L)*X24(L)
               AA(L)=ONE_OVER_8*SQRT(NX(L)*NX(L)+NY(L)*NY(L)+NZ(L)*NZ(L))
               AREA_S0(ITMP(N1))=AREA_S0(ITMP(N1))+AA(L)
               AREA_S0(ITMP(N2))=AREA_S0(ITMP(N2))+AA(L)
               AREA_S0(ITMP(N3))=AREA_S0(ITMP(N3))+AA(L)
               AREA_S0(ITMP(N4))=AREA_S0(ITMP(N4))+AA(L)
             ELSE
               X12(L)=X(1,N2)-X(1,N1)
               Y12(L)=X(2,N2)-X(2,N1)
               Z12(L)=X(3,N2)-X(3,N1)
               X13(L)=X(1,N3)-X(1,N1)
               Y13(L)=X(2,N3)-X(2,N1)
               Z13(L)=X(3,N3)-X(3,N1)
               NX(L)=Y12(L)*Z13(L)-Z12(L)*Y13(L)
               NY(L)=Z12(L)*X13(L)-X12(L)*Z13(L)
               NZ(L)=X12(L)*Y13(L)-Y12(L)*X13(L)
               AA(L)=ONE_OVER_6*SQRT(NX(L)*NX(L)+NY(L)*NY(L)+NZ(L)*NZ(L))
               AREA_S0(ITMP(N1))=AREA_S0(ITMP(N1))+AA(L)
               AREA_S0(ITMP(N2))=AREA_S0(ITMP(N2))+AA(L)
               AREA_S0(ITMP(N3))=AREA_S0(ITMP(N3))+AA(L)
             END IF
           END DO
          END DO
          IGAP = 1
        ENDIF
      ELSE
        IF(INTTH==0) THEN
          ITMP=0
          DO I=1,NSN
            II=NSV(I)
            ITMP(II)=I
          END DO
          DO N=1,NRTS,MVSIZ
C
            LLT=MIN(NRTS-N+1,MVSIZ)
C
            DO L=1,LLT
             I=N+L-1
C
             N1=IRECTS(1,I)
             N2=IRECTS(2,I)
             N3=IRECTS(3,I)
             N4=IRECTS(4,I)
             IF(N4/=N3)THEN
               X13(L)=X(1,N3)-X(1,N1)
               Y13(L)=X(2,N3)-X(2,N1)
               Z13(L)=X(3,N3)-X(3,N1)
               X24(L)=X(1,N4)-X(1,N2)
               Y24(L)=X(2,N4)-X(2,N2)
               Z24(L)=X(3,N4)-X(3,N2)
               NX(L)=Y13(L)*Z24(L)-Z13(L)*Y24(L)
               NY(L)=Z13(L)*X24(L)-X13(L)*Z24(L)
               NZ(L)=X13(L)*Y24(L)-Y13(L)*X24(L)
               AA(L)=ONE_OVER_8*SQRT(NX(L)*NX(L)+NY(L)*NY(L)+NZ(L)*NZ(L))
               AREA_S0(ITMP(N1))=AREA_S0(ITMP(N1))+AA(L)
               AREA_S0(ITMP(N2))=AREA_S0(ITMP(N2))+AA(L)
               AREA_S0(ITMP(N3))=AREA_S0(ITMP(N3))+AA(L)
               AREA_S0(ITMP(N4))=AREA_S0(ITMP(N4))+AA(L)
             ELSE
               X12(L)=X(1,N2)-X(1,N1)
               Y12(L)=X(2,N2)-X(2,N1)
               Z12(L)=X(3,N2)-X(3,N1)
               X13(L)=X(1,N3)-X(1,N1)
               Y13(L)=X(2,N3)-X(2,N1)
               Z13(L)=X(3,N3)-X(3,N1)
               NX(L)=Y12(L)*Z13(L)-Z12(L)*Y13(L)
               NY(L)=Z12(L)*X13(L)-X12(L)*Z13(L)
               NZ(L)=X12(L)*Y13(L)-Y12(L)*X13(L)
               AA(L)=ONE_OVER_6*SQRT(NX(L)*NX(L)+NY(L)*NY(L)+NZ(L)*NZ(L))
               AREA_S0(ITMP(N1))=AREA_S0(ITMP(N1))+AA(L)
               AREA_S0(ITMP(N2))=AREA_S0(ITMP(N2))+AA(L)
               AREA_S0(ITMP(N3))=AREA_S0(ITMP(N3))+AA(L)
             END IF
           END DO
          END DO
        ENDIF
      ENDIF  
C------------------------------------
      LXM=ZERO
      LYM=ZERO
      LZM=ZERO
      DO I=1,NRTM
        X0=EP30
        X1=-EP30
        Y0=EP30
        Y1=-EP30
        Z0=EP30
        Z1=-EP30
        DO J=1,4
          IX=MSR(IRECTM(J,I))
          XXX=X(1,IX)
          YYY=X(2,IX)
          ZZZ=X(3,IX)
          X0=MIN(X0,XXX)
          Y0=MIN(Y0,YYY)
          Z0=MIN(Z0,ZZZ)
          X1=MAX(X1,XXX)
          Y1=MAX(Y1,YYY)
          Z1=MAX(Z1,ZZZ)
        END DO
        LXM=MAX(LXM,X1-X0)
        LYM=MAX(LYM,Y1-Y0)
        LZM=MAX(LZM,Z1-Z0)
      ENDDO
C------------------------------------
      RETURN
 1000 FORMAT(2X,'GAP MIN = ',1PG20.13)
 1500 FORMAT(2X,'GAP MAX = ',1PG20.13)
 2000 FORMAT(2X,'DEPTH BEFORE RELEASE = ',1PG20.13)
 2001 FORMAT(2X,'Maximum distance for radiation computation = ',
     .                                                    1PG20.13)
      END
