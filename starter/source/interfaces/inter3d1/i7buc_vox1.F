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
Chd|  I7BUC_VOX1                    source/interfaces/inter3d1/i7buc_vox1.F
Chd|-- called by -----------
Chd|        ININT3                        source/interfaces/inter3d1/inint3.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        I7CMP3                        source/interfaces/inter3d1/i7cmp3.F
Chd|        I7COR3                        source/interfaces/inter3d1/i7cor3.F
Chd|        I7DST3                        source/interfaces/inter3d1/i7dst3.F
Chd|        I7PEN3                        source/interfaces/inter3d1/i7pen3.F
Chd|        I7TRIVOX1                     source/interfaces/inter3d1/i7trivox1.F
Chd|        FRONT_MOD                     share/modules1/front_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        TRI7BOX                       share/modules1/tri7box.F
Chd|====================================================================
      SUBROUTINE I7BUC_VOX1(
     1   X     ,IRECT,NSV    ,BUMULT,NSEG    ,
     2   NMN   ,NRTM ,MWA    ,NSN   ,CAND_E  ,
     3   CAND_N,GAP  ,XYZM   ,NOINT ,I_STOK  ,
     4   DIST  ,TZINF,MSR     ,
     5   STF   ,STFN ,MULTIMP,ISTF  ,IDDLEVEL,
     6   ITAB  ,GAP_S,GAP_M  ,IGAP  ,GAPMIN  ,
     7   GAPMAX,INACTI,GAP_S_L,GAP_M_L,I_MEM ,
     8   NCONT ,ICURV ,BGAPSMX,ID, TITR,
     9   DRAD  ,INTERCEP,NUMINT,PROV_N,PROV_E,
     1   IX11  ,IX12    ,IX13  ,IX14  ,NSVG  ,
     2   X1    ,X2      ,X3    ,X4    ,Y1    ,
     3   Y2    ,Y3      ,Y4    ,Z1    ,Z2    ,
     4   Z3    ,Z4      ,XI    ,YI    ,ZI    ,
     5   X0    ,Y0      ,Z0    ,NX1   ,NY1   ,
     6   NZ1   ,NX2     ,NY2   ,NZ2   ,NX3   ,
     7   NY3   ,NZ3     ,NX4   ,NY4   ,NZ4   ,
     8   P1    ,P2      ,P3    ,P4    ,LB1   ,
     9   LB2   ,LB3     ,LB4   ,LC1   ,LC2   ,
     1   LC3   ,LC4     ,N11   ,N21   ,N31   ,
     2   STIF  ,PENE    ,IREMNODE,FLAGREMNODE,
     3   KREMNODE,REMNODE,DGAPLOAD)
C============================================================================
C   M o d u l e s
C-----------------------------------------------
        USE MESSAGE_MOD
        USE TRI7BOX
        USE FRONT_MOD
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
#include      "units_c.inc"
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "vect07_c.inc"
#include      "scr06_c.inc"
#include      "scr17_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
        INTEGER NMN, NRTM, NSN, NOINT,I_STOK,MULTIMP,ISTF,IGAP,
     .          INACTI,I_MEM,NUMINT,IREMNODE,FLAGREMNODE
        INTEGER IRECT(4,*),NSV(*),NSEG(*),MWA(*)
        INTEGER CAND_E(*),CAND_N(*),MSR(*),IDDLEVEL
        INTEGER ITAB(*),NCONT,ICURV,KREMNODE(*),REMNODE(*)
        my_real
     .     STF(*),STFN(*),X(3,*),XYZM(6,2),GAP_S(*),GAP_M(*),
     .     DIST,BUMULT,GAP,TZINF,GAPMIN,GAPMAX,
     .     GAP_S_L(*),GAP_M_L(*),BGAPSMX, DRAD
        my_real , INTENT(IN) :: DGAPLOAD
        INTEGER ID
        CHARACTER*nchartitle,
     .     TITR
        TYPE(INTERSURFP) :: INTERCEP(3,NINTER)
        INTEGER, DIMENSION(MVSIZ), INTENT(IN) :: PROV_N, PROV_E,NSVG
        INTEGER, DIMENSION(MVSIZ), INTENT(IN) :: IX11,IX12,IX13,IX14
        my_real, DIMENSION(MVSIZ), INTENT(IN) :: X1,X2,X3,X4
        my_real, DIMENSION(MVSIZ), INTENT(IN) :: Y1,Y2,Y3,Y4
        my_real, DIMENSION(MVSIZ), INTENT(IN) :: Z1,Z2,Z3,Z4
        my_real, DIMENSION(MVSIZ), INTENT(IN) :: XI,YI,ZI
        my_real, DIMENSION(MVSIZ), INTENT(IN) :: X0,Y0,Z0
        my_real, DIMENSION(MVSIZ), INTENT(IN) :: NX1,NY1,NZ1
        my_real, DIMENSION(MVSIZ), INTENT(IN) :: NX2,NY2,NZ2
        my_real, DIMENSION(MVSIZ), INTENT(IN) :: NX3,NY3,NZ3
        my_real, DIMENSION(MVSIZ), INTENT(IN) :: NX4,NY4,NZ4
        my_real, DIMENSION(MVSIZ), INTENT(IN) :: P1,P2,P3,P4
        my_real, DIMENSION(MVSIZ), INTENT(IN) :: LB1,LB2,LB3,LB4
        my_real, DIMENSION(MVSIZ), INTENT(IN) :: LC1,LC2,LC3,LC4
        my_real, DIMENSION(MVSIZ), INTENT(IN) :: N11,N21,N31
        my_real, DIMENSION(MVSIZ), INTENT(IN) :: STIF,PENE
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
        INTEGER NRTM_L
        INTEGER, DIMENSION(:), ALLOCATABLE :: INDEX
        INTEGER NBX,NBY,NBZ
        INTEGER (KIND=8) :: NBX8,NBY8,NBZ8,RES8,LVOXEL8
        INTEGER I, J, K, I_ADD, L, LOC_PROC, N, ISZNSNR,
     .          N1, N2, N3, N4, NCONTACT,I_BID,J_STOK,I_STOK_OLD,
     .          IX1,IY1,IZ1,IX2,IY2,IZ2,IX,IY,IZ
        my_real
     .           MARGE, AAA,TZINF_ST,MARGE_ST
        my_real
     .     DX1,DY1,DZ1,
     .     DX3,DY3,DZ3,
     .     DX4,DY4,DZ4,
     .     DX6,DY6,DZ6,
     .     DD1,DD2,DD3,DD4,DD,DD0,XMIN,YMIN,ZMIN,
     .     XMAX_M,YMAX_M,ZMAX_M,XMIN_M,YMIN_M,ZMIN_M,
     .     XMAX_S,YMAX_S,ZMAX_S,XMIN_S,YMIN_S,ZMIN_S,
     .     XMAX,YMAX,ZMAX,XXX,YYY,ZZZ,
     .     XMINB, YMINB, ZMINB, XMAXB, YMAXB, ZMAXB,
     .     MEAN_X, MEAN_Y, MEAN_Z, DEV_X, DEV_Y, DEV_Z,
     .     GAPV(MVSIZ),C_MAX,
     .     TSTART,TSTOP,
     .     XMINE,YMINE,ZMINE,XMAXE,YMAXE,ZMAXE,
     .     XX1,XX2,XX3,XX4,YY1,YY2,YY3,YY4,ZZ1,ZZ2,ZZ3,ZZ4
        my_real, DIMENSION(:), ALLOCATABLE :: CURV_MAX

        LOGICAL :: TYPE18
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------
        ALLOCATE( INDEX(NRTM) )
        ALLOCATE( CURV_MAX(NRTM) )
        TYPE18=.FALSE.
        IF(INACTI==7)TYPE18=.TRUE.

        MWA(1:NUMNOD+NUMFAKENODIGEO) = 0
        NCONTACT = MULTIMP * NCONT
        C_MAX = ZERO
        IF(ICURV/=0)THEN
          DO I=1,NRTM
            XXX=MAX(X(1,IRECT(1,I)),X(1,IRECT(2,I)),
     .              X(1,IRECT(3,I)),X(1,IRECT(4,I)))
     .         -MIN(X(1,IRECT(1,I)),X(1,IRECT(2,I)),
     .              X(1,IRECT(3,I)),X(1,IRECT(4,I)))
            YYY=MAX(X(2,IRECT(1,I)),X(2,IRECT(2,I)),
     .              X(2,IRECT(3,I)),X(2,IRECT(4,I)))
     .         -MIN(X(2,IRECT(1,I)),X(2,IRECT(2,I)),
     .            X(2,IRECT(3,I)),X(2,IRECT(4,I)))
            ZZZ=MAX(X(3,IRECT(1,I)),X(3,IRECT(2,I)),
     .              X(3,IRECT(3,I)),X(3,IRECT(4,I)))
     .         -MIN(X(3,IRECT(1,I)),X(3,IRECT(2,I)),
     .              X(3,IRECT(3,I)),X(3,IRECT(4,I)))
            CURV_MAX(I) = HALF * MAX(XXX,YYY,ZZZ)
            C_MAX = MAX(C_MAX,CURV_MAX(I))
          ENDDO
        ELSE
          DO I=1,NRTM
            CURV_MAX(I)=ZERO
          ENDDO
        ENDIF
C-----
        DD=ZERO
        DO 10 L=1,NRTM
C      CONNECIVITES ELEMENT
          N1=IRECT(1,L)
          N2=IRECT(2,L)
          N3=IRECT(3,L)
          N4=IRECT(4,L)
C      LONGUEUR COTE 1
          DX1=(X(1,N1)-X(1,N2))
          DY1=(X(2,N1)-X(2,N2))
          DZ1=(X(3,N1)-X(3,N2))
          DD1=SQRT(DX1**2+DY1**2+DZ1**2)
C      LONGUEUR COTE 2
          DX3=(X(1,N1)-X(1,N4))
          DY3=(X(2,N1)-X(2,N4))
          DZ3=(X(3,N1)-X(3,N4))
          DD2=SQRT(DX3**2+DY3**2+DZ3**2)
C      LONGUEUR COTE 3
          DX4=(X(1,N3)-X(1,N2))
          DY4=(X(2,N3)-X(2,N2))
          DZ4=(X(3,N3)-X(3,N2))
          DD3=SQRT(DX4**2+DY4**2+DZ4**2)
C      LONGUEUR COTE 4
          DX6=(X(1,N4)-X(1,N3))
          DY6=(X(2,N4)-X(2,N3))
          DZ6=(X(3,N4)-X(3,N3))
          DD4=SQRT(DX6**2+DY6**2+DZ6**2)
          DD=DD+ (DD1+DD2+DD3+DD4)
  10    CONTINUE
        DD0=DD/NRTM/FOUR
        DD =DD0
C      DD = MAX(DD0,ONEP251*GAP)
C      DD = MAX(DD ,ONEP251*DRAD)
C remove this additional test which breaks igap performance
C in case this factor is needed, then needs to compute mean gap instead
C
C MARGE : marge engine dependante du BUMULT en input (ajustable pour performance)
        MARGE = BUMULT*DD
        TZINF = MARGE + MAX(GAP+DGAPLOAD,DRAD)
C MARGE_ST : marge independante du BUMULT en input necessaire pour trouver les pene initiales de maniere parith/on
        MARGE_ST = BMUL0*DD
C 1er passage avec marge x2 pour trouver plus de candidats (revient a prendre bmul0=0.4 au lieu de 0.2)
C      IF(IDDLEVEL==0) MARGE_ST = 2*MARGE_ST
        IF(IDDLEVEL==0) MARGE_ST = MARGE
        TZINF_ST = MARGE_ST + MAX(GAP+DGAPLOAD,DRAD)
C     MIS A ZERO POUR FAIRE SEARCH COMPLET CYCLE 0 ENGINE
        DIST = ZERO
C     TEST TO BY-PASS SECOND SEARCH
C     IF(IDDLEVEL==1 .AND. MARGE >= MARGE_ST) GOTO 999 !
C----- BORNES DU DOMAINE
        XMAX_M=-EP30
        YMAX_M=-EP30
        ZMAX_M=-EP30
        XMIN_M=EP30
        YMIN_M=EP30
        ZMIN_M=EP30
 100    CONTINUE
        I_STOK = 0
        J_STOK = 0
        I_MEM = 0
C Loop over the domain to improve sorting performance
        DO LOC_PROC=1,NSPMD
          NRTM_L=0
          DO I=1,NRTM
            IF(INTERCEP(1,NUMINT)%P(I)==LOC_PROC)THEN
              NRTM_L=NRTM_L+1
              INDEX(NRTM_L)=I
            END IF
          END DO
C
c        print *,NOINT,'***************start loop:',
c     .  LOC_PROC,NSPMD,NRTM,NRTM_L
          IF(NRTM_L == 0)CYCLE
          MEAN_X=ZERO
          MEAN_Y=ZERO
          MEAN_Z=ZERO
          DO K=1,NRTM_L
            I = INDEX(K)
            J=IRECT(1,I)
            XMAX_M= MAX(XMAX_M,X(1,J))
            YMAX_M= MAX(YMAX_M,X(2,J))
            ZMAX_M= MAX(ZMAX_M,X(3,J))
            XMIN_M= MIN(XMIN_M,X(1,J))
            YMIN_M= MIN(YMIN_M,X(2,J))
            ZMIN_M= MIN(ZMIN_M,X(3,J))
            MEAN_X=MEAN_X+X(1,J)
            MEAN_Y=MEAN_Y+X(2,J)
            MEAN_Z=MEAN_Z+X(3,J)
            J=IRECT(2,I)
            XMAX_M= MAX(XMAX_M,X(1,J))
            YMAX_M= MAX(YMAX_M,X(2,J))
            ZMAX_M= MAX(ZMAX_M,X(3,J))
            XMIN_M= MIN(XMIN_M,X(1,J))
            YMIN_M= MIN(YMIN_M,X(2,J))
            ZMIN_M= MIN(ZMIN_M,X(3,J))
            MEAN_X=MEAN_X+X(1,J)
            MEAN_Y=MEAN_Y+X(2,J)
            MEAN_Z=MEAN_Z+X(3,J)
            J=IRECT(3,I)
            XMAX_M= MAX(XMAX_M,X(1,J))
            YMAX_M= MAX(YMAX_M,X(2,J))
            ZMAX_M= MAX(ZMAX_M,X(3,J))
            XMIN_M= MIN(XMIN_M,X(1,J))
            YMIN_M= MIN(YMIN_M,X(2,J))
            ZMIN_M= MIN(ZMIN_M,X(3,J))
            MEAN_X=MEAN_X+X(1,J)
            MEAN_Y=MEAN_Y+X(2,J)
            MEAN_Z=MEAN_Z+X(3,J)
            J=IRECT(4,I)
            XMAX_M= MAX(XMAX_M,X(1,J))
            YMAX_M= MAX(YMAX_M,X(2,J))
            ZMAX_M= MAX(ZMAX_M,X(3,J))
            XMIN_M= MIN(XMIN_M,X(1,J))
            YMIN_M= MIN(YMIN_M,X(2,J))
            ZMIN_M= MIN(ZMIN_M,X(3,J))
            MEAN_X=MEAN_X+X(1,J)
            MEAN_Y=MEAN_Y+X(2,J)
            MEAN_Z=MEAN_Z+X(3,J)
          END DO

C MIN/MAX DOMAINE
          XMIN=XMIN_M-TZINF_ST
          YMIN=YMIN_M-TZINF_ST
          ZMIN=ZMIN_M-TZINF_ST
          XMAX=XMAX_M+TZINF_ST
          YMAX=YMAX_M+TZINF_ST
          ZMAX=ZMAX_M+TZINF_ST
c        print*,'bornes min:',XMIN,YMIN,ZMIN
c        print*,'bornes max:',XMAX,YMAX,ZMAX

          MEAN_X=MEAN_X/MAX((4*NRTM_L),1)
          MEAN_Y=MEAN_Y/MAX((4*NRTM_L),1)
          MEAN_Z=MEAN_Z/MAX((4*NRTM_L),1)
C     STD DEVIATION & CRVOXEL
          DEV_X=ZERO
          DEV_Y=ZERO
          DEV_Z=ZERO
          CRVOXEL(0:LRVOXEL,0:LRVOXEL)=0
          NBX = LRVOXEL
          NBY = LRVOXEL
          NBZ = LRVOXEL
          DO K=1,NRTM_L
            I = INDEX(K)
            N1 = IRECT(1,I)
            N2 = IRECT(2,I)
            N3 = IRECT(3,I)
            N4 = IRECT(4,I)
            XX1=X(1,N1)
            XX2=X(1,N2)
            XX3=X(1,N3)
            XX4=X(1,N4)
            XMAXE=MAX(XX1,XX2,XX3,XX4)
            XMINE=MIN(XX1,XX2,XX3,XX4)
            DEV_X=DEV_X+(XX1-MEAN_X)**2+(XX2-MEAN_X)**2
     .                +(XX3-MEAN_X)**2+(XX4-MEAN_X)**2
            YY1=X(2,N1)
            YY2=X(2,N2)
            YY3=X(2,N3)
            YY4=X(2,N4)
            YMAXE=MAX(YY1,YY2,YY3,YY4)
            YMINE=MIN(YY1,YY2,YY3,YY4)
            DEV_Y=DEV_Y+(YY1-MEAN_Y)**2+(YY2-MEAN_Y)**2
     .                 +(YY3-MEAN_Y)**2+(YY4-MEAN_Y)**2
            ZZ1=X(3,N1)
            ZZ2=X(3,N2)
            ZZ3=X(3,N3)
            ZZ4=X(3,N4)
            ZMAXE=MAX(ZZ1,ZZ2,ZZ3,ZZ4)
            ZMINE=MIN(ZZ1,ZZ2,ZZ3,ZZ4)
            DEV_Z=DEV_Z+(ZZ1-MEAN_Z)**2+(ZZ2-MEAN_Z)**2
     .                 +(ZZ3-MEAN_Z)**2+(ZZ4-MEAN_Z)**2

C        indice des voxels occupes par la facette

            IX1=INT(NBX*(XMINE-TZINF_ST-XMIN)/(XMAX-XMIN))
            IY1=INT(NBY*(YMINE-TZINF_ST-YMIN)/(YMAX-YMIN))
            IZ1=INT(NBZ*(ZMINE-TZINF_ST-ZMIN)/(ZMAX-ZMIN))
            IX1=MAX(0,MIN(NBX,IX1))
            IY1=MAX(0,MIN(NBY,IY1))
            IZ1=MAX(0,MIN(NBZ,IZ1))
            IX2=INT(NBX*(XMAXE+TZINF_ST-XMIN)/(XMAX-XMIN))
            IY2=INT(NBY*(YMAXE+TZINF_ST-YMIN)/(YMAX-YMIN))
            IZ2=INT(NBZ*(ZMAXE+TZINF_ST-ZMIN)/(ZMAX-ZMIN))
            IX2=MAX(0,MIN(NBX,IX2))
            IY2=MAX(0,MIN(NBY,IY2))
            IZ2=MAX(0,MIN(NBZ,IZ2))

            DO IZ = IZ1, IZ2
              DO IY = IY1, IY2
                DO IX = IX1, IX2
                  CRVOXEL(IY,IZ)=IBSET(CRVOXEL(IY,IZ),IX)
                END DO
              END DO
            END DO

          END DO
          DEV_X=SQRT(DEV_X/MAX(4*NRTM_L,1))
          DEV_Y=SQRT(DEV_Y/MAX(4*NRTM_L,1))
          DEV_Z=SQRT(DEV_Z/MAX(4*NRTM_L,1))
C we keep all in +/- 2 sigma => 95% of the population for normal law distribution
c      print*,'STAT X, MEAN / DEV:',MEAN_X,DEV_X
c      print*,'STAT Y, MEAN / DEV:',MEAN_Y,DEV_Y
c      print*,'STAT Z, MEAN / DEV:',MEAN_Z,DEV_Z
          XMINB=MAX(MEAN_X-2*DEV_X,XMIN)
          YMINB=MAX(MEAN_Y-2*DEV_Y,YMIN)
          ZMINB=MAX(MEAN_Z-2*DEV_Z,ZMIN)
          XMAXB=MIN(MEAN_X+2*DEV_X,XMAX)
          YMAXB=MIN(MEAN_Y+2*DEV_Y,YMAX)
          ZMAXB=MIN(MEAN_Z+2*DEV_Z,ZMAX)
C Test cas particulier 2D
          IF(ABS(XMINB-XMAXB) < EM10)THEN
            XMINB=XMIN
            XMAXB=XMAX
          END IF
          IF(ABS(YMINB-YMAXB) < EM10)THEN
            YMINB=YMIN
            YMAXB=YMAX
          END IF
          IF(ABS(ZMINB-ZMAXB) < EM10)THEN
            ZMINB=ZMIN
            ZMAXB=ZMAX
          END IF
c        print*,'R bornes min:',XMINB,YMINB,ZMINB
c        print*,'R bornes max:',XMAXB,YMAXB,ZMAXB

          XYZM(1,1) = XMIN
          XYZM(2,1) = YMIN
          XYZM(3,1) = ZMIN
          XYZM(4,1) = XMAX
          XYZM(5,1) = YMAX
          XYZM(6,1) = ZMAX
          XYZM(1,2) = XMINB
          XYZM(2,2) = YMINB
          XYZM(3,2) = ZMINB
          XYZM(4,2) = XMAXB
          XYZM(5,2) = YMAXB
          XYZM(6,2) = ZMAXB

c      I_STOK = 0
c      J_STOK = 0
c      I_MEM = 0
C-----DEBUT DE LA PHASE DE TRI
c        AAA = SQRT(NMN /
c     .           ((XMAX-XMIN)*(YMAX-YMIN)
c     .           +(YMAX-YMIN)*(ZMAX-ZMIN)
c     .           +(ZMAX-ZMIN)*(XMAX-XMIN)))
c        AAA = 0.75*AAA
c
c        NBX = NINT(AAA*(XMAX-XMIN))
c        NBY = NINT(AAA*(YMAX-YMIN))
c        NBZ = NINT(AAA*(ZMAX-ZMIN))
c        NBX = MAX(NBX,1)
c        NBY = MAX(NBY,1)
c        NBZ = MAX(NBZ,1)
c        print *,'intermediate NBx:',NBX,NBY,NBZ,AAA
c        NBX8=NBX
c        NBY8=NBY
c        NBZ8=NBZ
c        RES8=(NBX8+2)*(NBY8+2)*(NBZ8+2)
c        LVOXEL8 = LVOXEL
c
c        IF(RES8 > LVOXEL8) THEN
c          AAA = LVOXEL
c          AAA = AAA/((NBX8+2)*(NBY8+2)*(NBZ8+2))
c          AAA = AAA**(THIRD)
c          NBX = INT((NBX+2)*AAA)-2
c          NBY = INT((NBY+2)*AAA)-2
c          NBZ = INT((NBZ+2)*AAA)-2
c          NBX = MAX(NBX,1)
c          NBY = MAX(NBY,1)
c          NBZ = MAX(NBZ,1)
c        ENDIF
c
c        NBX8=NBX
c        NBY8=NBY
c        NBZ8=NBZ
c        RES8=(NBX8+2)*(NBY8+2)*(NBZ8+2)
c        print *,'original final NBx:',NBX,NBY,NBZ,RES8,AAA

C
          AAA = SQRT(NMN /
     .             ((XMAXB-XMINB)*(YMAXB-YMINB)
     .             +(YMAXB-YMINB)*(ZMAXB-ZMINB)
     .             +(ZMAXB-ZMINB)*(XMAXB-XMINB)))
          AAA = 0.75*AAA

          NBX = NINT(AAA*(XMAXB-XMINB))
          NBY = NINT(AAA*(YMAXB-YMINB))
          NBZ = NINT(AAA*(ZMAXB-ZMINB))
          NBX = MAX(NBX,1)
          NBY = MAX(NBY,1)
          NBZ = MAX(NBZ,1)
c        print *,'intermediate NBx:',NBX,NBY,NBZ,AAA

          NBX8=NBX
          NBY8=NBY
          NBZ8=NBZ
          RES8=(NBX8+2)*(NBY8+2)*(NBZ8+2)
          LVOXEL8 = LVOXEL

          IF(RES8 > LVOXEL8) THEN
            AAA = LVOXEL
            AAA = AAA/((NBX8+2)*(NBY8+2)*(NBZ8+2))
            AAA = AAA**(THIRD)
            NBX = INT((NBX+2)*AAA)-2
            NBY = INT((NBY+2)*AAA)-2
            NBZ = INT((NBZ+2)*AAA)-2
            NBX = MAX(NBX,1)
            NBY = MAX(NBY,1)
            NBZ = MAX(NBZ,1)
          ENDIF

          NBX8=NBX
          NBY8=NBY
          NBZ8=NBZ
          RES8=(NBX8+2)*(NBY8+2)*(NBZ8+2)
c        print *,'final NBx:',NBX,NBY,NBZ,RES8,AAA


c 100  CONTINUE

          IF(RES8 > LVOXEL8) THEN
            NBX = MIN(100,MAX(NBX8,1))
            NBY = MIN(100,MAX(NBY8,1))
            NBZ = MIN(100,MAX(NBZ8,1))
          ENDIF

c        print *,'init voxel:',NBX,NBY,NBZ,NBX*NBY*NBZ
C     initialisation complete de VOXEL
          DO I=INIVOXEL,(NBX+2)*(NBY+2)*(NBZ+2)
            VOXEL1(I)=0
          ENDDO
          INIVOXEL = MAX(INIVOXEL,(NBX+2)*(NBY+2)*(NBZ+2)+1)
C------------------
  200     CONTINUE
C------------------
C call with NRTM_L
          CALL I7TRIVOX1(
     1     NSN      ,I_MEM   ,IRECT    ,X       ,STF     ,
     2     STFN     ,XYZM    ,NSV      ,CAND_N  ,CAND_E  ,
     3     NCONTACT ,NOINT   ,TZINF_ST ,GAP_S_L ,GAP_M_L ,
     4     VOXEL1   ,NBX     ,NBY      ,NBZ     ,NRTM_L  ,
     5     IGAP     ,GAP     ,GAP_S    ,GAP_M   ,GAPMIN  ,
     6     GAPMAX   ,MARGE_ST,CURV_MAX ,BGAPSMX ,ISTF    ,
     7     I_STOK   ,MULTIMP ,J_STOK   ,PROV_N  ,PROV_E  ,
     8     ID       ,TITR    ,DRAD     ,INDEX   ,
     1     IX11  ,IX12    ,IX13  ,IX14  ,NSVG   ,
     2     X1    ,X2      ,X3    ,X4    ,Y1    ,
     3     Y2    ,Y3      ,Y4    ,Z1    ,Z2    ,
     4     Z3    ,Z4      ,XI    ,YI    ,ZI    ,
     5     X0    ,Y0      ,Z0    ,NX1   ,NY1   ,
     6     NZ1   ,NX2     ,NY2   ,NZ2   ,NX3   ,
     7     NY3   ,NZ3     ,NX4   ,NY4   ,NZ4   ,
     8     P1    ,P2      ,P3    ,P4    ,LB1   ,
     9     LB2   ,LB3     ,LB4   ,LC1   ,LC2   ,
     1     LC3   ,LC4     ,N11   ,N21   ,N31   ,
     2     STIF  ,PENE,IREMNODE,FLAGREMNODE,
     N     KREMNODE,REMNODE,DGAPLOAD)
C------------------

          IF (I_MEM == 2)THEN
            RETURN
          ENDIF
C     I_MEM = 1 ==> PAS ASSEZ DE MEMOIRE PILE
C     I_MEM = 2 ==> PAS ASSEZ DE MEMOIRE CANDIDATS
          IF(I_MEM==1)THEN
            I_MEM = 0
            GO TO 100
          ELSE IF(I_MEM==2) THEN
            MARGE_ST = THREE_OVER_4*MARGE_ST
            TZINF_ST = MARGE_ST + MAX(GAP,DRAD)
            I_MEM = 0
            IF(MARGE_ST<EM03) THEN
              CALL ANCMSG(MSGID=83,
     .                  MSGTYPE=MSGERROR,
     .                  ANMODE=ANINFO,
     .                  I1=ID,
     .                  C1=TITR)
            ENDIF
            GO TO 100
          ENDIF
C---------------------------------
          IF(J_STOK/=0)THEN
            LFT = 1
            LLT = J_STOK
            CALL I7COR3(X    ,IRECT,NSV  ,PROV_E ,PROV_N,
     .                  STF  ,STFN ,GAPV ,IGAP   ,GAP   ,
     .                  GAP_S,GAP_M,ISTF ,GAPMIN ,GAPMAX,
     .                  GAP_S_L,GAP_M_L  ,DRAD   ,IX11,IX12 ,
     5                  IX13   ,IX14   ,NSVG,X1      ,X2    ,
     6                  X3     ,X4     ,Y1  ,Y2      ,Y3    ,
     7                  Y4     ,Z1     ,Z2  ,Z3      ,Z4    ,
     8                  XI     ,YI     ,ZI  ,STIF    ,DGAPLOAD)
            CALL I7DST3( IX13,IX14,X1 ,X2 ,X3 ,
     1                     X4 ,Y1 ,Y2 ,Y3 ,Y4 ,
     2                     Z1 ,Z2 ,Z3 ,Z4 ,XI ,
     3                     YI ,ZI ,X0 ,Y0 ,Z0 ,
     4                     NX1,NY1,NZ1,NX2,NY2,
     5                     NZ2,NX3,NY3,NZ3,NX4,
     6                     NY4,NZ4,P1 ,P2 ,P3 ,
     7                     P4 ,LB1,LB2,LB3,LB4,
     8                     LC1,LC2,LC3,LC4)
            CALL I7PEN3(MARGE_ST,GAPV,N11,N21,N31,
     1                     PENE ,NX1 ,NY1,NZ1,NX2,
     2                     NY2  ,NZ2 ,NX3,NY3,NZ3,
     3                     NX4  ,NY4 ,NZ4,P1 ,P2 ,
     4                     P3   ,P4)
            IF(I_STOK+J_STOK<MULTIMP*NSN) THEN
              CALL I7CMP3(I_STOK,CAND_E  ,CAND_N,1,PENE,
     1                    PROV_N,PROV_E)
            ELSE
              I_BID = 0
              CALL I7CMP3(I_BID,CAND_E,CAND_N,0,PENE,
     1                    PROV_N,PROV_E)
              IF(I_STOK+I_BID<MULTIMP*NSN) THEN
                CALL I7CMP3(I_STOK,CAND_E,CAND_N,1,PENE,
     1                      PROV_N,PROV_E)
              ELSE
                I_MEM = 2
                I_STOK = 0
                J_STOK = 0
                RETURN
                MARGE_ST = THREE_OVER_4*MARGE_ST
                TZINF_ST = MARGE_ST + MAX(GAP+DGAPLOAD,DRAD)
                I_MEM = 0
C ne pas dimunuer la taille des boite
                IF(MARGE_ST<EM03) THEN
                  CALL ANCMSG(MSGID=83,
     .                    MSGTYPE=MSGERROR,
     .                    ANMODE=ANINFO,
     .                    I1=ID,
     .                    C1=TITR)
                ENDIF
                GO TO 100
              ENDIF
            ENDIF
            J_STOK=0
          ENDIF
C
        END DO ! end loop over NSPMD domains

        IF(.NOT.TYPE18)THEN
          IF(NSN/=0)THEN
            WRITE(IOUT,*)' POSSIBLE IMPACT NUMBER:',I_STOK,' (<=',
     .      1+(I_STOK-1)/NSN,'*NSN)'

C
          ELSE
            CALL ANCMSG(MSGID=552,
     .                  MSGTYPE=MSGWARNING,
     .                  ANMODE=ANINFO_BLIND_2,
     .                  I1=ID,
     .                  C1=TITR)
          ENDIF
        ENDIF!(.NOT.TYPE18)
C
        DEALLOCATE( INDEX )
        DEALLOCATE( CURV_MAX )
        RETURN
      END
