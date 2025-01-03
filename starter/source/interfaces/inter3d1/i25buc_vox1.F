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
      !||    i25buc_vox1            ../starter/source/interfaces/inter3d1/i25buc_vox1.F
      !||--- called by ------------------------------------------------------
      !||    inint3                 ../starter/source/interfaces/inter3d1/inint3.F
      !||--- calls      -----------------------------------------------------
      !||    ancmsg                 ../starter/source/output/message/message.F
      !||    i25trivox1             ../starter/source/interfaces/inter3d1/i25trivox1.F
      !||    insol25                ../starter/source/interfaces/inter3d1/i25buc_vox1.F
      !||--- uses       -----------------------------------------------------
      !||    message_mod            ../starter/share/message_module/message_mod.F
      !||    tri7box                ../starter/share/modules1/tri7box.F
      !||====================================================================
      SUBROUTINE I25BUC_VOX1(
     1   X     ,IRECT,NSV    ,BUMULT,
     2   NMN   ,NRTM ,NSN   ,INTBUF_TAB  ,
     3   GAP  ,I_STOK  ,
     4   DIST  ,TZINF,MAXBOX ,MINBOX,MSR     ,   
     5   STF   ,STFN ,IDDLEVEL,
     6   GAP_S,GAP_M  ,IGAP  ,GAPMIN  ,
     7   GAPMAX,INACTI,GAP_S_L,GAP_M_L,
     8   MARGE ,ID   ,TITR   ,NBINFLG,MBINFLG,
     9   ILEV  ,MSEGTYP,GAP_N,BGAPSMX,
     A   IPARTS,KNOD2ELS,NOD2ELS,
     B   KREMNODE,REMNODE,
     C   IXS, IXS10, IXS16, IXS20,ICODE,ISKEW ,
     D   DRAD ,DGAPLOAD,NRTMT,FLAG_REMOVED_NODE,
     E   IELEM_M,nin,npari,ipari )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
      USE TRI7BOX
      USE NAMES_AND_TITLES_MOD , ONLY : NCHARTITLE
      USE INTBUFDEF_MOD 
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "units_c.inc"
#include      "com04_c.inc"
#include      "scr06_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NMN, NRTM, NSN, I_STOK,IGAP,
     .        INACTI,
     .        IPARTS(*), KNOD2ELS(*), NOD2ELS(*)
      INTEGER IRECT(4,*),NSV(*),ICODE(*),ISKEW(*)
      INTEGER MSR(*),IDDLEVEL
      INTEGER NBINFLG(*),MBINFLG(*),ILEV,MSEGTYP(*)
      INTEGER KREMNODE(*),REMNODE(*)
      INTEGER IXS(*), IXS10(*), IXS16(*), IXS20(*)
      LOGICAL, INTENT(in) :: FLAG_REMOVED_NODE !< flag to remove some S node from the list of candidates
      INTEGER  , INTENT(IN) :: IELEM_M(2,NRTM)
      integer, intent(in) :: nin !< interface index
      integer, intent(in) :: npari !< first dim of ipari
      integer, dimension(npari), intent(inout) :: ipari !< interface data
C     REAL
      my_real
     .   STF(*),STFN(*),X(3,*),GAP_S(*),GAP_M(*),
     .   DIST,BUMULT,GAP,TZINF,MAXBOX,MINBOX,GAPMIN,GAPMAX,
     .   GAP_S_L(*),GAP_M_L(*),MARGE,GAP_N(4,*)
      my_real
     .   BGAPSMX
      my_real , INTENT(IN) :: DRAD, DGAPLOAD
      INTEGER ID
      CHARACTER(LEN=NCHARTITLE) :: TITR

      INTEGER , INTENT(IN) :: NRTMT
      type(intbuf_struct_), intent(inout) :: intbuf_tab !< interface data 
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J, L, N1, N2, N3, N4,N_SOL, ESHIFT
      INTEGER IBID, NELS
      INTEGER,  DIMENSION(:),ALLOCATABLE :: IIX,IIY,IIZ
C     REAL
      my_real
     .   DX1,DY1,DZ1,
     .   DX3,DY3,DZ3,
     .   DX4,DY4,DZ4,
     .   DX6,DY6,DZ6,
     .   DD1,DD2,DD3,DD4,DD,DD0,XMIN,YMIN,ZMIN,
     .   XMAX,YMAX,ZMAX,TZINF0,MINBOX0,MAXBOX0,
     .   BID,TZINF_ST,MARGE_ST,DD_ST,D_MAX,PENSOL,D_MOY,
     .   XYZM(6),BMINMA(6),AAA,LEDGMAX
      my_real,
     .   DIMENSION(:), ALLOCATABLE :: EDGE_L2,EDGE_L2_TMP
      INTEGER TAGP(NPART),IAD,N,IE,IL,IP
      INTEGER, DIMENSION(:), ALLOCATABLE :: NPARTNS,IELEM,LPARTNS
      INTEGER NBX,NBY,NBZ
      INTEGER, DIMENSION(:), ALLOCATABLE :: IS_LARGE_NODE,LARGE_NODE, TAGNOD,LOCAL_NEXT_NOD
      INTEGER :: NB_LARGE_NODES
      INTEGER (KIND=8) :: NBX8,NBY8,NBZ8,RES8,LVOXEL8
      INTEGER (KIND=8) :: IONE,IHUNDRED !< Integer constants in INTEGER 8 for comparisions
C-----------------------------------------------
      IONE=1
      IHUNDRED=100
      GAPMAX=EP30
      GAPMIN=ZERO

C-------use temporarily GAP_N(1,*)=V/A
C       Not used :: MVOISN(1,*)-> MTYPE(solid),MVOISN(2,*)-> E_id
C
C
C     1-CALCUL TAILLE DES ZONES INFLUENCES
C     DD EST LA LONGEUR MOYENNE ELEMENT
C     DD_ST EST LA LONGEUR MAX ELEMENT
      DD=ZERO
      DD_ST=ZERO
      PENSOL=EP30
      D_MOY = ZERO
      N_SOL = 0
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
C-------only for solid---  and coating shell
       IF (MSEGTYP(L)==0.OR.MSEGTYP(L)>NRTMT) THEN
        D_MAX=MAX(DD1,DD2,DD3,DD4)
        D_MAX=MIN(D_MAX,GAP_N(1,L))
C--------correction of too huge GAP_N(1,L) w/ irregular mesh         
        GAP_N(1,L)=D_MAX
        DD_ST=MAX(DD_ST,D_MAX)
        N_SOL = N_SOL + 1
        D_MOY = D_MOY + D_MAX
       END IF
C       IF (MSEGTYP(L)==0) DD_ST=MAX(DD_ST,DD1,DD2,DD3,DD4)
  10  CONTINUE
C     TAILLE ZINF = .1*TAILLE MOYENNE ELEMENT DE CHAQUE COTE
C     TAILLE BUCKET MIN = TZINF  * BUMULT
      DD0=DD/NRTM/FOUR
      DD=DD0
C     TZINF = BUMULT*DD
      MARGE = BUMULT*DD
C calcul de TZINF en fct de la marge et non le contraire
      TZINF = MARGE + MAX(GAP+DGAPLOAD,DRAD)
C MARGE_ST : marge independante du BUMULT en input pour trouver les memes pene initiales (cas delete ou chgt coord.)
      MARGE_ST = BMUL0*DD

C Si IDDLEVEL = 0, alors 1er passage ici, avec la MARGE de l'ENGINE pour trouver les memes candidats
      IF(IDDLEVEL==0) MARGE_ST = MARGE
      TZINF_ST = MARGE_ST + MAX(GAP+DGAPLOAD,DRAD)
      BID = FOUR_OVER_5*DD
      IF (INACTI/=7.AND.TZINF>BID) THEN
        IBID = NINT(TZINF/DD0)
        IBID =(2*IBID+4)*IBID*2
      ENDIF
C      MAXBOX= ZEP9*(DD - TZINF)
C DD + 2*TZINF : taille element augmentee de zone influence
      MAXBOX= HALF*(DD + 2*TZINF)
      MINBOX= HALF*MAXBOX
      TZINF0 = TZINF
      MINBOX0 = MINBOX
      MAXBOX0 = MAXBOX
C     MIS A ZERO POUR FAIRE SEARCH COMPLET CYCLE 0 ENGINE
      DIST = ZERO     

C--------------------------------
C     CALCUL DES BORNES DU DOMAINE
C--------------------------------
      BMINMA(1)=-EP30
      BMINMA(2)=-EP30
      BMINMA(3)=-EP30
      BMINMA(4)= EP30
      BMINMA(5)= EP30
      BMINMA(6)= EP30
C
      XMIN=EP30
      XMAX=-EP30
      YMIN=EP30
      YMAX=-EP30
      ZMIN=EP30
      ZMAX=-EP30 
C
      DO 20 I=1,NMN
        J=MSR(I) 
        XMIN= MIN(XMIN,X(1,J))
        YMIN= MIN(YMIN,X(2,J))
        ZMIN= MIN(ZMIN,X(3,J))
        XMAX= MAX(XMAX,X(1,J))
        YMAX= MAX(YMAX,X(2,J))
        ZMAX= MAX(ZMAX,X(3,J))
 20   CONTINUE
C
      XMIN=XMIN-TZINF_ST
      YMIN=YMIN-TZINF_ST
      ZMIN=ZMIN-TZINF_ST
      XMAX=XMAX+TZINF_ST
      YMAX=YMAX+TZINF_ST
      ZMAX=ZMAX+TZINF_ST
C
      DO 25 I=1,NSN
        J=NSV(I)
        XMIN= MIN(XMIN,X(1,J))
        YMIN= MIN(YMIN,X(2,J))
        ZMIN= MIN(ZMIN,X(3,J))
        XMAX= MAX(XMAX,X(1,J))
        YMAX= MAX(YMAX,X(2,J))
        ZMAX= MAX(ZMAX,X(3,J))
 25   CONTINUE
C
      BMINMA(1) = MAX(BMINMA(1),XMAX)
      BMINMA(2) = MAX(BMINMA(2),YMAX)
      BMINMA(3) = MAX(BMINMA(3),ZMAX)
      BMINMA(4) = MIN(BMINMA(4),XMIN)
      BMINMA(5) = MIN(BMINMA(5),YMIN)
      BMINMA(6) = MIN(BMINMA(6),ZMIN)

      XYZM(1) = BMINMA(4)
      XYZM(2) = BMINMA(5)
      XYZM(3) = BMINMA(6)
      XYZM(4) = BMINMA(1)
      XYZM(5) = BMINMA(2)
      XYZM(6) = BMINMA(3)

      AAA = SQRT(NMN /
     .           ((BMINMA(1)-BMINMA(4))*(BMINMA(2)-BMINMA(5))
     .           +(BMINMA(2)-BMINMA(5))*(BMINMA(3)-BMINMA(6))
     .           +(BMINMA(3)-BMINMA(6))*(BMINMA(1)-BMINMA(4))))

      AAA = 0.75*AAA

      NBX = NINT(AAA*(BMINMA(1)-BMINMA(4)))
      NBY = NINT(AAA*(BMINMA(2)-BMINMA(5)))
      NBZ = NINT(AAA*(BMINMA(3)-BMINMA(6)))
      NBX = MAX(NBX,1)
      NBY = MAX(NBY,1)
      NBZ = MAX(NBZ,1)

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
      
      IF(RES8 > LVOXEL8) THEN
        NBX = MIN(IHUNDRED,MAX(NBX8,IONE))
        NBY = MIN(IHUNDRED,MAX(NBY8,IONE))
        NBZ = MIN(IHUNDRED,MAX(NBZ8,IONE))
      END IF  

C--------------------------------
C     Initializations case of IDDLEVEL=0 or INACTI/=5 and INACTI/=-1
C--------------------------------
      ALLOCATE(NPARTNS(NSN+1),IELEM(NRTM),EDGE_L2(NSN))

      NPARTNS(1:NSN+1) = 0
      IELEM(1:NRTM)    = 0
      EDGE_L2(1:NSN)   = ZERO 
      LEDGMAX          = ZERO
C--------------------------------

 

       NB_LARGE_NODES = 0
       ALLOCATE(LARGE_NODE(NSN))
       ALLOCATE(IS_LARGE_NODE(NSN))
       ALLOCATE(TAGNOD(NUMNOD))
       IS_LARGE_NODE(1:NSN) = 0
       LARGE_NODE(1:NSN) = 0
       NB_LARGE_NODES = 0
       TAGNOD(1:NUMNOD) = 0

      IF(IDDLEVEL==1)THEN
        ! Looking for initial penetration (vs solids) if and only if IDDLEVEL=1
C            
        DO IE=1,NRTM
           ! looks for solid element supporting the segment (cf initial penetrations vs solids)
           IF(IELEM_M(1,IE)<=NUMELS) THEN
              IELEM(IE)= IELEM_M(1,IE)
           ELSE
             CALL INSOL25(IRECT    ,IXS     ,IXS10,IXS16,IXS20,
     .                  KNOD2ELS ,NOD2ELS ,NELS ,IE   )
             IELEM(IE)=NELS
           ENDIF
        END DO
C
        IF(INACTI==5.OR.INACTI==-1)THEN 

          ALLOCATE(EDGE_L2_TMP(NUMNOD))
          EDGE_L2_TMP(1:NUMNOD)=ZERO
          N_SOL = 0
          DO IE=1,NRTM
            IF(STF(IE)> ZERO)THEN ! Solids only
              DO IL=1,4
                N1=IRECT(IL,IE)
                N2=IRECT(MOD(IL,4)+1,IE)

                AAA = (X(1,N2)-X(1,N1))*(X(1,N2)-X(1,N1))
     .              + (X(2,N2)-X(2,N1))*(X(2,N2)-X(2,N1))
     .              + (X(3,N2)-X(3,N1))*(X(3,N2)-X(3,N1)) 
                EDGE_L2_TMP(N1) = MAX(EDGE_L2_TMP(N1), AAA )
                EDGE_L2_TMP(N2) = MAX(EDGE_L2_TMP(N2), AAA )
                IF (MSEGTYP(IE)==0.OR.MSEGTYP(IE)>NRTMT) THEN ! Solids only
                   IF(TAGNOD(N1)==0) THEN
                      N_SOL= N_SOL + 1
                      TAGNOD(N1) = 1
                   ENDIF
                   IF(TAGNOD(N2)==0) THEN
                      N_SOL= N_SOL + 1
                      TAGNOD(N2) = 1
                   ENDIF
                ENDIF
              END DO
            ENDIF

          END DO
C
          
          DO I=1,NSN
            N=NSV(I)
            IF(STFN(I)/=ZERO) THEN
              EDGE_L2(I) = HALF*SQRT(EDGE_L2_TMP(N))
              IF(TAGNOD(N)==1) LEDGMAX=LEDGMAX+EDGE_L2(I)!MAX(LEDGMAX,EDGE_L2(I))
            END IF
          END DO 

          IF(N_SOL > 0) LEDGMAX=HALF*LEDGMAX/N_SOL

 
          DEALLOCATE(EDGE_L2_TMP)

        ENDIF

c        IF(INACTI==5.OR.INACTI==-1)THEN ! Do it for all Inacti ( warnings initial penetrations)
          TAGP(1:NPART)   =0
C
          DO I=1,NSN
            N=NSV(I)
            DO IAD=KNOD2ELS(N)+1,KNOD2ELS(N+1)
              IE=NOD2ELS(IAD)
              IP=IPARTS(IE)
              IF(TAGP(IP)==0)THEN
                NPARTNS(I)=NPARTNS(I)+1
                TAGP(IP)  =1
              END IF
            END DO
            DO IAD=KNOD2ELS(N)+1,KNOD2ELS(N+1)
              IE=NOD2ELS(IAD)
              IP=IPARTS(IE)
              TAGP(IP)  =0 ! Reset
            END DO
          END DO
C
          DO I=1,NSN
            NPARTNS(I+1) = NPARTNS(I+1) + NPARTNS(I)
          END DO
          DO I=NSN,1,-1
            NPARTNS(I+1) = NPARTNS(I)
          END DO
          NPARTNS(1)=0
C
          ALLOCATE(LPARTNS(NPARTNS(NSN+1)))
C
          DO I=1,NSN
            N=NSV(I)
            DO IAD=KNOD2ELS(N)+1,KNOD2ELS(N+1)
              IE=NOD2ELS(IAD)
              IP=IPARTS(IE)
              IF(TAGP(IP)==0)THEN
                NPARTNS(I)=NPARTNS(I)+1
                LPARTNS(NPARTNS(I))=IP
                TAGP(IP)  =1
              END IF
            END DO
            DO IAD=KNOD2ELS(N)+1,KNOD2ELS(N+1)
              IE=NOD2ELS(IAD)
              IP=IPARTS(IE)
              TAGP(IP)  =0 ! Reset
            END DO
          END DO
C
          DO I=NSN,1,-1
            NPARTNS(I+1) = NPARTNS(I)
          END DO
          NPARTNS(1)=0
C
c        ELSE
c          ALLOCATE(LPARTNS(0))
c        END IF
      ELSE
        ALLOCATE(LPARTNS(0))
      END IF
C

C     initialisation complete de VOXEL
      DO I=INIVOXEL,(NBX+2)*(NBY+2)*(NBZ+2)
        VOXEL1(I)=0
      ENDDO
      INIVOXEL = MAX(INIVOXEL,(NBX+2)*(NBY+2)*(NBZ+2)+1)
C
      ESHIFT=0
C
      I_STOK = 0

      ALLOCATE(LOCAL_NEXT_NOD(NSN))
      ALLOCATE(IIX(NSN))
      ALLOCATE(IIY(NSN))
      ALLOCATE(IIZ(NSN))
C
!$OMP PARALLEL
      CALL I25TRIVOX1(
     1   NSN     ,IRECT   ,X       ,
     2   STFN    ,XYZM    ,NSV     ,I_STOK  ,
     3   ESHIFT  ,BGAPSMX ,
     4   VOXEL1  ,NBX     ,NBY     ,NBZ     ,NRTM    ,
     5   GAP_S   ,GAP_M   ,MARGE_ST,
     6   NBINFLG ,MBINFLG ,ILEV    ,MSEGTYP ,
     7   IGAP    ,GAP_S_L ,GAP_M_L ,EDGE_L2 ,LEDGMAX ,
     8   KREMNODE,REMNODE,
     9   IPARTS  ,NPARTNS ,LPARTNS ,IELEM   ,ICODE   ,
     A   ISKEW   ,DRAD, IS_LARGE_NODE, LARGE_NODE, NB_LARGE_NODES,
     B   DGAPLOAD,NRTMT,FLAG_REMOVED_NODE,
     C   IELEM_M,LOCAL_NEXT_NOD,IIX,IIY,IIZ,
     D   intbuf_tab,ipari,nin)
!$OMP END PARALLEL
      DEALLOCATE(LOCAL_NEXT_NOD)
      DEALLOCATE(IIX)
      DEALLOCATE(IIY)
      DEALLOCATE(IIZ)

      DEALLOCATE(EDGE_L2,NPARTNS,IELEM,LPARTNS)
      DEALLOCATE(IS_LARGE_NODE,LARGE_NODE,TAGNOD)
C
      IF(NSN/=0)THEN
       WRITE(IOUT,*)' POSSIBLE IMPACT NUMBER, NSN:',I_STOK,NSN
C
      ELSE
       CALL ANCMSG(MSGID=552,
     .             MSGTYPE=MSGWARNING,
     .             ANMODE=ANINFO_BLIND_2,
     .             I1=ID,
     .             C1=TITR)
      ENDIF
C
      RETURN
      END

      !||====================================================================
      !||    insol25       ../starter/source/interfaces/inter3d1/i25buc_vox1.F
      !||--- called by ------------------------------------------------------
      !||    i25buc_vox1   ../starter/source/interfaces/inter3d1/i25buc_vox1.F
      !||====================================================================
      SUBROUTINE INSOL25(IRECT   ,IXS    ,IXS10,IXS16,IXS20,
     .                   KNOD2ELS,NOD2ELS,NEL  ,I    )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NEL, I
      INTEGER IRECT(4,*), IXS(NIXS,*), KNOD2ELS(*), NOD2ELS(*), 
     .        IXS10(6,*), IXS16(8,*), IXS20(12,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER N, JJ, II, K, IC, IAD,
     .        NUSER, NUSERM
C-----------------------------------------------
C   E x t e r n a l   F u n c t i o n s
C-----------------------------------------------
C
      NEL=0

      IF(NUMELS==0) RETURN

      IF(IRECT(1,I)>NUMNOD) RETURN

      IC=0
      NUSERM = -1
      DO 230 IAD=KNOD2ELS(IRECT(1,I))+1,KNOD2ELS(IRECT(1,I)+1)
       N = NOD2ELS(IAD)
       IF(N <= NUMELS8)THEN
         DO 210 JJ=1,4
           II=IRECT(JJ,I)
           DO K=1,8
             IF(IXS(K+1,N)==II) GOTO 210
           ENDDO
           GOTO 230
  210    CONTINUE
         IC=IC+1
         NUSER = IXS(11,N)
         IF (NUSER>NUSERM) THEN
           NEL = N
           NUSERM = NUSER
         ENDIF
       ELSEIF(N <= NUMELS8+NUMELS10)THEN
         DO 220 JJ=1,4
           II=IRECT(JJ,I)
           DO K=1,8
             IF(IXS(K+1,N)==II) GOTO 220
           ENDDO
           DO K=1,6
             IF(IXS10(K,N-NUMELS8)==II) GOTO 220
           ENDDO
           GOTO 230
  220    CONTINUE
         IC=IC+1
         NUSER = IXS(11,N)
         IF (NUSER>NUSERM) THEN
           NEL = N
           NUSERM = NUSER
         ENDIF
       ELSEIF(N <= NUMELS8+NUMELS10+NUMELS20)THEN
         DO 222 JJ=1,4
           II=IRECT(JJ,I)
           DO K=1,8
             IF(IXS(K+1,N)==II) GOTO 222
           ENDDO
           DO K=1,12
             IF(IXS20(K,N-NUMELS8-NUMELS10)==II) GOTO 222
           ENDDO
           GOTO 230
  222    CONTINUE
         IC=IC+1
         NUSER = IXS(11,N)
         IF (NUSER>NUSERM) THEN
           NEL = N
           NUSERM = NUSER
         ENDIF
       ELSEIF(N <= NUMELS8+NUMELS10+NUMELS20+NUMELS16)THEN
         DO 224 JJ=1,4
           II=IRECT(JJ,I)
           DO K=1,8
             IF(IXS(K+1,N)==II) GOTO 224
           ENDDO
           DO K=1,8
             IF(IXS16(K,N-NUMELS8-NUMELS10-NUMELS20)==II) GOTO 224
           ENDDO
           GOTO 230
  224    CONTINUE
         IC=IC+1
         NUSER = IXS(11,N)
         IF (NUSER>NUSERM) THEN
           NEL = N
           NUSERM = NUSER
         ENDIF
       ELSE
         GOTO 230
       END IF
  230 CONTINUE
      RETURN
      END
