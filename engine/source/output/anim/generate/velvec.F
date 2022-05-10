Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2022 Altair Engineering Inc.
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
Chd|  VELVEC                        source/output/anim/generate/velvec.F
Chd|-- called by -----------
Chd|        GENANI                        source/output/anim/generate/genani.F
Chd|-- calls ---------------
Chd|        SPMD_ANIM_PLY_VELVEC          source/mpi/anim/spmd_anim_ply_velvec.F
Chd|        SPMD_VGATH                    source/mpi/anim/spmd_vgath.F  
Chd|        WRITE_R_C                     source/output/tools/sortie_c.c
Chd|        PLYXFEM_MOD                   share/modules/plyxfem_mod.F   
Chd|====================================================================
      SUBROUTINE VELVEC(V,V_TEMP,IVOIS,AL,NODCUT,NNWL,NNSRG,
     .                  NODGLOB,WEIGHT,NFVNOD,IFUNC,
     .                  NFNOD_PXFEM,NOD,INDX,NFNOD_CRKXFEMG,ITAB)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE PLYXFEM_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "task_c.inc"
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "scr05_c.inc"
#include      "scr14_c.inc"
#include      "spmd_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NODGLOB(*),WEIGHT(*),NOD(*),INDX(*)
      INTEGER,INTENT(IN) :: ITAB(NUMNOD)
      my_real
     .   V(3,NUMNOD),AL(*),V_TEMP(3,*)
      my_real
     .   S3000,S
      REAL R4
      REAL, DIMENSION(:,:), ALLOCATABLE :: PLYVELVEC
      INTEGER I,IVOIS(2,*),NODCUT,NNWL,K,P,BUF
      INTEGER NNSRG,NFVNOD,IFUNC,NFNOD_PXFEM,ND,EMPL,NFNOD_CRKXFEMG
C-----------------------------------------------
      INTEGER N,IPLY,JJ,EMPSIZPL
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------
      S3000 = 3000.
      S = ZERO
C
      IF (NSPMD == 1) THEN
       DO I=1,NUMNOD
         R4 = V(1,I)
         CALL WRITE_R_C(R4,1)
         R4 = V(2,I)
         CALL WRITE_R_C(R4,1)
         R4 = V(3,I)
         CALL WRITE_R_C(R4,1)
       ENDDO
       IF (NUMELIG3D /= 0)THEN
        DO I=1,64*NUMELIG3D
         R4 = V_TEMP(1,I)
         CALL WRITE_R_C(R4,1)
         R4 = V_TEMP(2,I)
         CALL WRITE_R_C(R4,1)
         R4 = V_TEMP(3,I)
         CALL WRITE_R_C(R4,1)
        ENDDO
       ENDIF
      ELSE
        IF (ISPMD.EQ.0) THEN
           BUF = NUMNODG
        ELSE
           BUF = 1
         ENDIF
         CALL SPMD_VGATH(V,NODGLOB,WEIGHT,BUF)     
      ENDIF
      IF(NODCUT.GT.0)THEN
        IF (NSPMD > 1) THEN
          IF (ISPMD.EQ.0) THEN
            PRINT *, '** NODCUT NON PARALLELIZED OPTION!'
          END IF
          GO TO 211
        END IF
        DO I=1,NODCUT
          R4 =AL(I)*V(1,IVOIS(2,I))+(UN-AL(I))*V(1,IVOIS(1,I))
          CALL WRITE_R_C(R4,1)
          R4 =AL(I)*V(2,IVOIS(2,I))+(UN -AL(I))*V(2,IVOIS(1,I))
          CALL WRITE_R_C(R4,1)
          R4 =AL(I)*V(3,IVOIS(2,I))+(UN-AL(I))*V(3,IVOIS(1,I))
          CALL WRITE_R_C(R4,1)
        ENDDO !next I
 211    CONTINUE
      ENDIF
C
      IF (ISPMD==0) THEN
        R4 = 0.
        DO I=1,NSECT+NRWALL+NNWL+NNSRG
         CALL WRITE_R_C(R4,1)
         CALL WRITE_R_C(R4,1)
         CALL WRITE_R_C(R4,1)
        ENDDO
      ENDIF
C
C  node ply xfem
C
      IF(ANIM_PLY > 0 ) THEN
       IF (NSPMD == 1) THEN 
         IF (NFNOD_PXFEM.GT.0) THEN
            IF(IFUNC == 1) THEN    
              ALLOCATE(PLYVELVEC(3,NFNOD_PXFEM))
              DO JJ =1,NPLYPART
               IPLY = INDX(JJ) 
               DO ND=1,PLYNOD(IPLY)%PLYNUMNODS
                 I = PLYNOD(IPLY)%NODES(ND)
                 EMPL = PLYNOD(IPLY)%PLYNODID(ND)
                 N = NOD(I)
                 PLYVELVEC(1,EMPL) =  PLY(IPLY)%V(1,N)
                 PLYVELVEC(2,EMPL) =  PLY(IPLY)%V(2,N)
                 PLYVELVEC(3,EMPL) =  PLY(IPLY)%V(3,N)
               ENDDO  
              ENDDO  
              CALL WRITE_R_C(PLYVELVEC,3*NFNOD_PXFEM)
              DEALLOCATE(PLYVELVEC)
C
            ELSEIF(IFUNC == 2)THEN
              ALLOCATE(PLYVELVEC(3,NFNOD_PXFEM))
              DO JJ =1,NPLYPART
                 IPLY = INDX(JJ)              
                 DO ND=1,PLYNOD(IPLY)%PLYNUMNODS
                   I = PLYNOD(IPLY)%NODES(ND)
                   EMPL = PLYNOD(IPLY)%PLYNODID(ND)
                   N = NOD(I)                   
                   PLYVELVEC(1,EMPL) = PLY(IPLY)%U(1,N)
                   PLYVELVEC(2,EMPL) = PLY(IPLY)%U(2,N)
                   PLYVELVEC(3,EMPL) = PLY(IPLY)%U(3,N)
                 ENDDO  
               ENDDO  
               CALL WRITE_R_C(PLYVELVEC,3*NFNOD_PXFEM)
               DEALLOCATE(PLYVELVEC)

            ELSEIF(IFUNC == 3) THEN
                ALLOCATE(PLYVELVEC(3,NFNOD_PXFEM))
                DO JJ =1,NPLYPART
                  IPLY = INDX(JJ) 
                  DO ND=1,PLYNOD(IPLY)%PLYNUMNODS
                     I = PLYNOD(IPLY)%NODES(ND)
                     EMPL = PLYNOD(IPLY)%PLYNODID(ND)
                     N = NOD(I)
                     PLYVELVEC(1,EMPL) = PLY(IPLY)%A(1,N)
                     PLYVELVEC(2,EMPL) = PLY(IPLY)%A(2,N)
                     PLYVELVEC(3,EMPL) = PLY(IPLY)%A(3,N)
                 ENDDO  
               ENDDO 
               CALL WRITE_R_C(PLYVELVEC,3*NFNOD_PXFEM)
               DEALLOCATE(PLYVELVEC)
            ELSE 
               R4 = ZERO
               DO I=1,NFNOD_PXFEM
                 CALL WRITE_R_C(R4,1)
                 CALL WRITE_R_C(R4,1)
                 CALL WRITE_R_C(R4,1)
                ENDDO
            ENDIF   
         ENDIF 
        ELSE
C
         EMPSIZPL = 0
         DO JJ =1,NPLYPART
            IPLY = INDX(JJ)
            CALL SPMD_ANIM_PLY_VELVEC( NODGLOB,IPLY,
     *                                 NOD, IFUNC,EMPSIZPL )
         ENDDO
       ENDIF       
      ENDIF
c----------------------------
C     nodes crk xfem
c----------------------------
      IF (ANIM_CRK > 0 ) THEN           
        IF (ISPMD == 0) THEN            
          R4 = ZERO                     
          DO I=1,NFNOD_CRKXFEMG        
            CALL WRITE_R_C(R4,1)        
            CALL WRITE_R_C(R4,1)        
            CALL WRITE_R_C(R4,1)        
          ENDDO                         
        ENDIF                           
      ENDIF                             
c----------------------------    
      IF (ISPMD==0)THEN          
        IF (NFVNOD.GT.0) THEN      
           R4 = 0.                 
           DO I=1,NFVNOD+3         
              CALL WRITE_R_C(R4,1) 
              CALL WRITE_R_C(R4,1) 
              CALL WRITE_R_C(R4,1) 
           ENDDO                   
        ENDIF                      
      ENDIF
C-------------
 300  CONTINUE
      RETURN
      END

Chd|====================================================================
Chd|  VELVEC2                       source/output/anim/generate/velvec.F
Chd|-- called by -----------
Chd|        GENANI                        source/output/anim/generate/genani.F
Chd|-- calls ---------------
Chd|        SPMD_GLOB_DSUM9               source/mpi/interfaces/spmd_th.F
Chd|        SPMD_VELVEC2                  source/mpi/anim/spmd_velvec2.F
Chd|        WRITE_R_C                     source/output/tools/sortie_c.c
Chd|====================================================================
      SUBROUTINE VELVEC2(IVOIS,V_TEMP,AL   ,NODCUT,FOPT,
     .                   NPBY,NNWL ,NNSRG,IXUR,NODGLOB,WEIGHT,FR_SEC,
     .                   NFVNOD,NFNOD_PXFEM,NFNOD_PXFEMG,NFNOD_CRKXFEMG)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
#include      "task_c.inc"
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "scr05_c.inc"
#include      "scr14_c.inc"
#include      "rnur_c.inc"
#include      "spmd_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
C     REAL ou REAL*8
      my_real
     .  AL(*),FOPT(6,*),V_TEMP(3,*)

      REAL R4
      INTEGER IVOIS(2,*),NPBY(NNPBY,*),IXUR(NIXUR,*),NODCUT,NNWL
      INTEGER NNSRG,NFNOD_PXFEM,NFNOD_PXFEMG
      INTEGER I,N,WEIGHT(*),FR_SEC(NSPMD+1,*)

      INTEGER NODGLOB(*),K,P,RBUF,NNG
      INTEGER NFVNOD,NFNOD_CRKXFEMG
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C----------------------------------------------
      my_real
     .   RWA(3,NSECT),RWAL(3,NRWALL)    ,V(3,NUMNOD)
      INTEGER LOC_PROC,PMASTER
C=======================================================================
      LOC_PROC = ISPMD + 1
C
      DO I=1,NUMNOD
       V(1,I) = ZERO
       V(2,I) = ZERO
       V(3,I) = ZERO        
      ENDDO
C
      IF (NSPMD.EQ.1) THEN
        DO N=1,NRBODY
         I = NPBY(1,N)
         V(1,I) = FOPT(1,NSECT+N)
         V(2,I) = FOPT(2,NSECT+N)
         V(3,I) = FOPT(3,NSECT+N)
        ENDDO
C
        DO N=1,NUMELUR
         I = IXUR(3,N)
         V(1,I) = V(1,I) + FOPT(1,NSECT+NRBODY+NRWALL+N)
         V(2,I) = V(2,I) + FOPT(2,NSECT+NRBODY+NRWALL+N)
         V(3,I) = V(3,I) + FOPT(3,NSECT+NRBODY+NRWALL+N)
        ENDDO
      ELSE
         DO N=1,NRBODY
           I = NPBY(1,N)
           IF (I.GT.0) THEN
             IF (WEIGHT(I).EQ.1) THEN
                V(1,I) = FOPT(1,NSECT+N)
                V(2,I) = FOPT(2,NSECT+N)
                V(3,I) = FOPT(3,NSECT+N)
             ENDIF
           ENDIF
         ENDDO

         DO N=1,NUMELUR
           I = IXUR(3,N)
           IF (WEIGHT(I).EQ.1) THEN
             V(1,I) = V(1,I) + FOPT(1,NSECT+NRBODY+NRWALL+N)
             V(2,I) = V(2,I) + FOPT(2,NSECT+NRBODY+NRWALL+N)
             V(3,I) = V(3,I) + FOPT(3,NSECT+NRBODY+NRWALL+N)
           ENDIF
         ENDDO
      ENDIF
C
      IF (NSPMD == 1) THEN
        DO I=1,NUMNOD
          R4 = V(1,I)
          CALL WRITE_R_C(R4,1)
          R4 = V(2,I)
          CALL WRITE_R_C(R4,1)
          R4 = V(3,I)
          CALL WRITE_R_C(R4,1)
        ENDDO
        IF (NUMELIG3D /= 0)THEN
         DO I=1,64*NUMELIG3D
          R4 = V_TEMP(1,I)
          CALL WRITE_R_C(R4,1)
          R4 = V_TEMP(2,I)
          CALL WRITE_R_C(R4,1)
          R4 = V_TEMP(3,I)
          CALL WRITE_R_C(R4,1)
         ENDDO
        ENDIF
      ELSE
        IF (ISPMD.EQ.0) THEN
          RBUF = NUMNODM
          NNG = NUMNODG
        ELSE
          RBUF = 1
          NNG = 1
        ENDIF
          CALL SPMD_VELVEC2(V,NODGLOB,RBUF,NNG)
      ENDIF

C
C nodcut non parallelises
      IF(NODCUT.GT.0)THEN
        IF (NSPMD > 1) THEN
          IF (ISPMD.EQ.0)THEN
            PRINT *, '** NODCUT NON PARALLELIZED OPTION'
          ENDIF
          GOTO 211
        END IF
        DO 210 I=1,NODCUT
         R4 =AL(I)*V(1,IVOIS(2,I))+(UN -AL(I))*V(1,IVOIS(1,I))
         CALL WRITE_R_C(R4,1)
         R4 =AL(I)*V(2,IVOIS(2,I))+(UN -AL(I))*V(2,IVOIS(1,I))
         CALL WRITE_R_C(R4,1)
         R4 =AL(I)*V(3,IVOIS(2,I))+(UN -AL(I))*V(3,IVOIS(1,I))
         CALL WRITE_R_C(R4,1)
 210    CONTINUE
 211    CONTINUE
      ENDIF
C
      IF (NSPMD.EQ.1) THEN
        DO I=1,NSECT
         R4 = FOPT(1,I)
         CALL WRITE_R_C(R4,1)
         R4 = FOPT(2,I)
         CALL WRITE_R_C(R4,1)
         R4 = FOPT(3,I)
         CALL WRITE_R_C(R4,1)
        ENDDO
      ELSE
        DO I=1,NSECT
         PMASTER = FR_SEC(NSPMD+1,I)
          IF (PMASTER .EQ.LOC_PROC) THEN
           RWA(1,I) = FOPT(1,I)
           RWA(2,I) = FOPT(2,I)
           RWA(3,I) = FOPT(3,I)
          ELSE
           RWA(1,I) = ZERO
           RWA(2,I) = ZERO
           RWA(3,I) = ZERO
          ENDIF
        ENDDO
        IF(NSECT.GT.0)
     .    CALL SPMD_GLOB_DSUM9(RWA,3*NSECT)
        IF (ISPMD.EQ.0) THEN
          DO I=1,NSECT
            R4 = RWA(1,I)
            CALL WRITE_R_C(R4,1)
            R4 = RWA(2,I)
            CALL WRITE_R_C(R4,1)
            R4 = RWA(3,I)
            CALL WRITE_R_C(R4,1)
          ENDDO
        ENDIF
      ENDIF
C
      IF (NSPMD.EQ.1) THEN
        DO I=1,NRWALL
          R4 = FOPT(1,NSECT+NRBODY+I)
          CALL WRITE_R_C(R4,1)
          R4 = FOPT(2,NSECT+NRBODY+I)
          CALL WRITE_R_C(R4,1)
          R4 = FOPT(3,NSECT+NRBODY+I)
          CALL WRITE_R_C(R4,1)
        ENDDO
      ELSE
        DO I=1,NRWALL
          RWAL(1,I) = FOPT(1,NSECT+NRBODY+I)
          RWAL(2,I) = FOPT(2,NSECT+NRBODY+I)
          RWAL(3,I) = FOPT(3,NSECT+NRBODY+I)
        ENDDO
        IF (NRWALL.GT.0)
     .    CALL SPMD_GLOB_DSUM9(RWAL,3*NRWALL)
        IF (ISPMD.EQ.0) THEN 
          DO I=1,NRWALL
            R4 = RWAL(1,I)
            CALL WRITE_R_C(R4,1)
            R4 = RWAL(2,I)
            CALL WRITE_R_C(R4,1)
            R4 = RWAL(3,I)
            CALL WRITE_R_C(R4,1)
          ENDDO
        ENDIF
      ENDIF
      IF (ISPMD.NE.0) GO TO 300
      R4 = 0.
      DO I=1,NNWL
       CALL WRITE_R_C(R4,1)
       CALL WRITE_R_C(R4,1)
       CALL WRITE_R_C(R4,1)
      ENDDO
        R4=ZERO
        DO I=1,NNSRG
          CALL WRITE_R_C(R4,1)
          CALL WRITE_R_C(R4,1)
          CALL WRITE_R_C(R4,1)
        ENDDO
C
      IF(ANIM_PLY > 0 ) THEN
        IF(ISPMD==0 .AND. NFNOD_PXFEMG.GT.0) THEN
           R4 = ZERO
           DO I=1,NFNOD_PXFEMG
             CALL WRITE_R_C(R4,1)
             CALL WRITE_R_C(R4,1)
             CALL WRITE_R_C(R4,1)
           ENDDO
       ENDIF
      ENDIF
c----------------------------
C     nodes crk xfem
c----------------------------
      IF (ANIM_CRK > 0 ) THEN           
        IF (ISPMD == 0) THEN            
          R4 = ZERO                     
          DO I=1,NFNOD_CRKXFEMG        
            CALL WRITE_R_C(R4,1)        
            CALL WRITE_R_C(R4,1)        
            CALL WRITE_R_C(R4,1)        
          ENDDO                         
        ENDIF                           
      ENDIF                             
c----------------------------    
      IF (NFVNOD.GT.0) THEN
         R4=ZERO
         DO I=1,NFVNOD+3
            CALL WRITE_R_C(R4,1)
            CALL WRITE_R_C(R4,1)
            CALL WRITE_R_C(R4,1)
         ENDDO
      ENDIF
 300  CONTINUE
C-----------
      RETURN
      END

Chd|====================================================================
Chd|  VELVECC                       source/output/anim/generate/velvec.F
Chd|-- called by -----------
Chd|        GENANI                        source/output/anim/generate/genani.F
Chd|-- calls ---------------
Chd|        SPMD_GLOB_FSUM9               source/mpi/interfaces/spmd_th.F
Chd|        WRITE_R_C                     source/output/tools/sortie_c.c
Chd|====================================================================
      SUBROUTINE VELVECC(V,V_TEMP,IVOIS,AL,NODCUT,NNWL,NNSRG,
     .                  NODGLOB,WEIGHT,NFVNOD,NFNOD_PXFEM,
     .                  NFNOD_PXFEMG,NFNOD_CRKXFEMG)
C-----------------------------s------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "task_c.inc"
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "scr05_c.inc"
#include      "scr14_c.inc"
#include      "spmd_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NODGLOB(*),WEIGHT(*)
C     REAL ou REAL*8
      my_real
     .   V(3,*),AL(*),V_TEMP(3,*)
C     REAL ou REAL*8
      my_real
     .   S3000,S
      REAL R4, VG(3,NUMNODG)
      INTEGER I,IVOIS(2,*),NODCUT,NNWL,K,P,BUF
      INTEGER NNSRG,NFVNOD,NFNOD_PXFEM,NFNOD_PXFEMG,NFNOD_CRKXFEMG
C-----------------------------------------------
      S3000 = 3000.
      S     = ZERO

      IF (NSPMD == 1) THEN
       DO I=1,NUMNOD
         R4 = V(1,I)
         CALL WRITE_R_C(R4,1)
         R4 = V(2,I)
         CALL WRITE_R_C(R4,1)
         R4 = V(3,I)
         CALL WRITE_R_C(R4,1)
       ENDDO
       IF (NUMELIG3D /= 0)THEN
        DO I=1,64*NUMELIG3D
         R4 = V_TEMP(1,I)
         CALL WRITE_R_C(R4,1)
         R4 = V_TEMP(2,I)
         CALL WRITE_R_C(R4,1)
         R4 = V_TEMP(3,I)
         CALL WRITE_R_C(R4,1)
        ENDDO
       ENDIF
      ELSE
        DO I=1,NUMNODG
           VG(1,I)=ZERO
           VG(2,I)=ZERO
           VG(3,I)=ZERO
        ENDDO
        DO K=1,NUMNOD
           I=NODGLOB(K)
           VG(1,I)=V(1,K)
           VG(2,I)=V(2,K)
           VG(3,I)=V(3,K)
        ENDDO

        CALL SPMD_GLOB_FSUM9(VG,3*NUMNODG)

        IF (ISPMD.EQ.0) THEN
         DO I=1,NUMNODG
           CALL WRITE_R_C(VG(1,I),1)
           CALL WRITE_R_C(VG(2,I),1)
           CALL WRITE_R_C(VG(3,I),1)
         ENDDO
        ENDIF        
      ENDIF

C
C option non parallelisee !
      IF(NODCUT.GT.0)THEN
        IF (NSPMD > 1) THEN
          IF (ISPMD.EQ.0) THEN
            PRINT *, '** NODCUT NON PARALLELIZED OPTION'
          END IF
          GO TO 211
        END IF
        DO 210 I=1,NODCUT
         R4 =AL(I)*V(1,IVOIS(2,I))+(UN-AL(I))*V(1,IVOIS(1,I))
         CALL WRITE_R_C(R4,1)
         R4 =AL(I)*V(2,IVOIS(2,I))+(UN -AL(I))*V(2,IVOIS(1,I))
         CALL WRITE_R_C(R4,1)
         R4 =AL(I)*V(3,IVOIS(2,I))+(UN-AL(I))*V(3,IVOIS(1,I))
         CALL WRITE_R_C(R4,1)
 210    CONTINUE
 211    CONTINUE
      ENDIF
C
      IF(ANIM_PLY > 0 ) THEN

       IF(ISPMD==0 .AND. NFNOD_PXFEMG.GT.0) THEN
         R4 = ZERO
         DO I=1,NFNOD_PXFEMG
            CALL WRITE_R_C(R4,1)
            CALL WRITE_R_C(R4,1)
            CALL WRITE_R_C(R4,1)
          ENDDO
       ENDIF
      ENDIF
c----------------------------
C     nodes crk xfem
c----------------------------
      IF (ANIM_CRK > 0 ) THEN           
        IF (ISPMD == 0) THEN            
          R4 = ZERO                     
          DO I=1,NFNOD_CRKXFEMG        
            CALL WRITE_R_C(R4,1)        
            CALL WRITE_R_C(R4,1)        
            CALL WRITE_R_C(R4,1)        
          ENDDO                         
        ENDIF                           
      ENDIF                             
c----------------------------    
      IF (ISPMD.EQ.0) THEN
        R4 = 0.
        DO I=1,NSECT+NRWALL+NNWL+NNSRG
         CALL WRITE_R_C(R4,1)
         CALL WRITE_R_C(R4,1)
         CALL WRITE_R_C(R4,1)
        ENDDO
        IF (NFVNOD.GT.0) THEN
           R4 = 0.
           DO I=1,NFVNOD+3
              CALL WRITE_R_C(R4,1)
              CALL WRITE_R_C(R4,1)
              CALL WRITE_R_C(R4,1)
           ENDDO
        ENDIF
      ENDIF!(ISPMD.EQ.0) 
c----------------------------  
 300  CONTINUE
      RETURN
      END

Chd|====================================================================
Chd|  VELVECC21                     source/output/anim/generate/velvec.F
Chd|-- called by -----------
Chd|        GENANI                        source/output/anim/generate/genani.F
Chd|-- calls ---------------
Chd|        SPMD_GLOB_DSUM9               source/mpi/interfaces/spmd_th.F
Chd|        WRITE_R_C                     source/output/tools/sortie_c.c
Chd|====================================================================
      SUBROUTINE VELVECC21(V,V_TEMP,IVOIS,AL,NODCUT,NNWL,
     .                  NNSRG,NODGLOB,WEIGHT,NFVNOD,NFNOD_PXFEM,
     .                  NFNOD_PXFEMG,VG21,NFNOD_CRKXFEMG)
C-----------------------------s------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "task_c.inc"
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "intstamp_c.inc"
#include      "scr05_c.inc"
#include      "scr14_c.inc"
#include      "spmd_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NODGLOB(*),WEIGHT(*)
C     REAL ou REAL*8
      my_real
     .   V(3,*),AL(*),VG21(3,*),V_TEMP(3,*)
C     REAL ou REAL*8
      my_real
     .   S3000,S
      REAL R4
      INTEGER I,IVOIS(2,*),NODCUT,NNWL,K,P,BUF
      INTEGER NNSRG,NFVNOD,NFNOD_PXFEM,NFNOD_PXFEMG,NFNOD_CRKXFEMG
       my_real
     .       , DIMENSION(:,:), ALLOCATABLE ::  V_TMP
C-----------------------------------------------
      S3000 = 3000.
      S     = ZERO
C
      IF (NSPMD == 1) THEN
       DO K=1,NUMNOD
         I=NODGLOB(K)
         R4 = VG21(1,I)+V(1,K)
         CALL WRITE_R_C(R4,1)
         R4 = VG21(2,I)+V(2,K)
         CALL WRITE_R_C(R4,1)
         R4 = VG21(3,I)+V(3,K)
         CALL WRITE_R_C(R4,1)
       ENDDO
       IF (NUMELIG3D /= 0)THEN
        DO I=1,64*NUMELIG3D
         R4 = V_TEMP(1,I)
         CALL WRITE_R_C(R4,1)
         R4 = V_TEMP(2,I)
         CALL WRITE_R_C(R4,1)
         R4 = V_TEMP(3,I)
         CALL WRITE_R_C(R4,1)
        ENDDO
       ENDIF
      ELSE
        ALLOCATE(V_TMP(3,NUMNODG))
        DO I=1,NUMNODG
           V_TMP(1,I) =VG21(1,I)
           V_TMP(2,I) =VG21(2,I)
           V_TMP(3,I) =VG21(3,I)
        ENDDO

        DO K=1,NUMNOD
           I=NODGLOB(K)
           V_TMP(1,I)=V_TMP(1,I)+V(1,K)
           V_TMP(2,I)=V_TMP(2,I)+V(2,K)
           V_TMP(3,I)=V_TMP(3,I)+V(3,K)
        ENDDO

        CALL SPMD_GLOB_DSUM9(V_TMP,3*NUMNODG)

        IF(ISPMD==0)THEN
          DO I=1,NUMNODG
             R4 = V_TMP(1,I)
             CALL WRITE_R_C(R4,1)
             R4 = V_TMP(2,I)
             CALL WRITE_R_C(R4,1)
             R4 = V_TMP(3,I)
             CALL WRITE_R_C(R4,1)
          ENDDO
        ENDIF
        DEALLOCATE(V_TMP)
      ENDIF
C
C option non parallelisee !
      IF(NODCUT.GT.0)THEN
        IF (NSPMD > 1) THEN
          IF (ISPMD.EQ.0) THEN
            PRINT *, '** NODCUT NON PARALLELIZED OPTION'
          END IF
          GO TO 211
        END IF
        DO 210 I=1,NODCUT
         R4 =AL(I)*V(1,IVOIS(2,I))+(UN-AL(I))*V(1,IVOIS(1,I))
         CALL WRITE_R_C(R4,1)
         R4 =AL(I)*V(2,IVOIS(2,I))+(UN -AL(I))*V(2,IVOIS(1,I))
         CALL WRITE_R_C(R4,1)
         R4 =AL(I)*V(3,IVOIS(2,I))+(UN-AL(I))*V(3,IVOIS(1,I))
         CALL WRITE_R_C(R4,1)
 210    CONTINUE
 211    CONTINUE
      ENDIF
C
      IF(ANIM_PLY > 0 ) THEN

       IF(ISPMD==0 .AND. NFNOD_PXFEMG.GT.0) THEN
         R4 = ZERO
         DO I=1,NFNOD_PXFEMG
            CALL WRITE_R_C(R4,1)
            CALL WRITE_R_C(R4,1)
            CALL WRITE_R_C(R4,1)
          ENDDO
       ENDIF
      ENDIF
c----------------------------
C     nodes crk xfem
c----------------------------
      IF (ANIM_CRK > 0 ) THEN           
        IF (ISPMD == 0) THEN            
          R4 = ZERO                     
          DO I=1,NFNOD_CRKXFEMG        
            CALL WRITE_R_C(R4,1)        
            CALL WRITE_R_C(R4,1)        
            CALL WRITE_R_C(R4,1)        
          ENDDO                         
        ENDIF                           
      ENDIF                             
c----------------------------    
      IF (ISPMD.NE.0) GOTO 300
      R4 = 0.
      DO I=1,NSECT+NRWALL+NNWL+NNSRG
       CALL WRITE_R_C(R4,1)
       CALL WRITE_R_C(R4,1)
       CALL WRITE_R_C(R4,1)
      ENDDO
C
      IF (NFVNOD.GT.0) THEN
         R4 = 0.
         DO I=1,NFVNOD+3
            CALL WRITE_R_C(R4,1)
            CALL WRITE_R_C(R4,1)
            CALL WRITE_R_C(R4,1)
         ENDDO
      ENDIF
C
 300  CONTINUE
      RETURN
      END

Chd|====================================================================
Chd|  VELVECC_MAX                   source/output/anim/generate/velvec.F
Chd|-- called by -----------
Chd|        GENANI                        source/output/anim/generate/genani.F
Chd|-- calls ---------------
Chd|        WRITE_R_C                     source/output/tools/sortie_c.c
Chd|====================================================================
      SUBROUTINE VELVECC_MAX(VMAX,NODCUT,NNWL,NNSRG,NFVNOD,
     .                       NFNOD_PXFEMG,NFNOD_CRKXFEMG)
C-----------------------------s------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "task_c.inc"
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "scr05_c.inc"
#include      "scr14_c.inc"
#include      "spmd_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      my_real
     .   VMAX(3,*)
      REAL R4  
      INTEGER I,K,NODCUT,NNWL
      INTEGER NNSRG,NFVNOD,NFNOD_PXFEM,NFNOD_PXFEMG,NFNOD_CRKXFEMG
C-----------------------------------------------

      IF (NSPMD == 1) THEN
        DO I=1,NUMNOD
          R4 = VMAX(1,I)
          CALL WRITE_R_C(R4,1)
          R4 = VMAX(2,I)
          CALL WRITE_R_C(R4,1)
          R4 = VMAX(3,I)
          CALL WRITE_R_C(R4,1)
        ENDDO
      ELSE

        IF (ISPMD.EQ.0) THEN

           DO I=1,NUMNODG
              R4 = VMAX(1,I)
              CALL WRITE_R_C(R4,1)
              R4 = VMAX(2,I)
              CALL WRITE_R_C(R4,1)
              R4 = VMAX(3,I)
              CALL WRITE_R_C(R4,1)
           ENDDO
        ENDIF        
      ENDIF

C
C option non parallelisee !
      IF(NODCUT.GT.0)THEN
        IF (NSPMD > 1) THEN
          IF (ISPMD.EQ.0) THEN
            PRINT *, '** NODCUT NON PARALLELIZED OPTION'
          END IF
          GO TO 211
        END IF
        DO 210 I=1,NODCUT ! put to zero for the moment option not supported
         R4 =0.
         CALL WRITE_R_C(R4,1)
         R4 =0.
         CALL WRITE_R_C(R4,1)
         R4 =0.
         CALL WRITE_R_C(R4,1)
 210    CONTINUE
 211    CONTINUE
      ENDIF
C
      IF(ANIM_PLY > 0 ) THEN

       IF(ISPMD==0 .AND. NFNOD_PXFEMG.GT.0) THEN
         R4 = ZERO
         DO I=1,NFNOD_PXFEMG
            CALL WRITE_R_C(R4,1)
            CALL WRITE_R_C(R4,1)
            CALL WRITE_R_C(R4,1)
          ENDDO
       ENDIF
      ENDIF
c----------------------------
C     nodes crk xfem
c----------------------------
      IF (ANIM_CRK > 0 ) THEN           
        IF (ISPMD == 0) THEN            
          R4 = ZERO                     
          DO I=1,NFNOD_CRKXFEMG        
            CALL WRITE_R_C(R4,1)        
            CALL WRITE_R_C(R4,1)        
            CALL WRITE_R_C(R4,1)        
          ENDDO                         
        ENDIF                           
      ENDIF                             
c----------------------------    
      IF (ISPMD.NE.0) GOTO 300
      R4 = 0.
      DO I=1,NSECT+NRWALL+NNWL+NNSRG
       CALL WRITE_R_C(R4,1)
       CALL WRITE_R_C(R4,1)
       CALL WRITE_R_C(R4,1)
      ENDDO
C
      IF (NFVNOD.GT.0) THEN
         R4 = 0.
         DO I=1,NFVNOD+3
            CALL WRITE_R_C(R4,1)
            CALL WRITE_R_C(R4,1)
            CALL WRITE_R_C(R4,1)
         ENDDO
      ENDIF
C
 300  CONTINUE

c----------------------------  
      RETURN
      END

Chd|====================================================================
Chd|  VELVEC3                       source/output/anim/generate/velvec.F
Chd|-- called by -----------
Chd|        GENANI                        source/output/anim/generate/genani.F
Chd|-- calls ---------------
Chd|        SPMD_VGATH                    source/mpi/anim/spmd_vgath.F  
Chd|        WRITE_R_C                     source/output/tools/sortie_c.c
Chd|====================================================================
      SUBROUTINE VELVEC3(V,V_TEMP,VALE,IVOIS,AL,NODCUT,NNWL,NNSRG,
     .                  NODGLOB,WEIGHT,NFVNOD,NFNOD_PXFEM,NFNOD_PXFEMG,
     .                  NFNOD_CRKXFEMG)
C-----------------------------s------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "task_c.inc"
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "scr05_c.inc"
#include      "spmd_c.inc"
#include      "scr14_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NODGLOB(*),WEIGHT(*)
C     REAL ou REAL*8
      my_real
     .   V(3,*),VALE(3,*),AL(*),V_TEMP(3,*)
C     REAL ou REAL*8
      my_real
     .   S3000,S
      REAL R4
      INTEGER I,IVOIS(2,*),NODCUT,NNWL,K,P,BUF
      INTEGER NNSRG,NFVNOD,NFNOD_PXFEM,NFNOD_PXFEMG,NFNOD_CRKXFEMG
      my_real
     .    VGLOBAL(3,NUMNOD)
C-----------------------------------------------
      S3000 = 3000.
      S = 0.
C
      IF (ISPMD.EQ.0) THEN
         BUF = NUMNODG
      ELSE
         BUF = 1
      ENDIF

      IF (NSPMD == 1)THEN
       DO I=1,NUMNOD
         R4 = V(1,I)+VALE(1,I)
         CALL WRITE_R_C(R4,1)
         R4 = V(2,I)+VALE(2,I)
         CALL WRITE_R_C(R4,1)
         R4 = V(3,I)+VALE(3,I)
         CALL WRITE_R_C(R4,1)
       ENDDO
       IF (NUMELIG3D /= 0)THEN
        DO I=1,64*NUMELIG3D
         R4 = V_TEMP(1,I)
         CALL WRITE_R_C(R4,1)
         R4 = V_TEMP(2,I)
         CALL WRITE_R_C(R4,1)
         R4 = V_TEMP(3,I)
         CALL WRITE_R_C(R4,1)
        ENDDO
       ENDIF
      ELSE
       DO I=1,NUMNOD
         VGLOBAL(1,I)=V(1,I)+VALE(1,I)
         VGLOBAL(2,I)=V(2,I)+VALE(2,I)
         VGLOBAL(3,I)=V(3,I)+VALE(3,I)
       ENDDO
        CALL SPMD_VGATH(VGLOBAL,NODGLOB,WEIGHT,BUF)
      ENDIF
C
C option non parallelisee !
      IF(NODCUT.GT.0)THEN
        IF (ISPMD.EQ.0) THEN
          PRINT *, '** NODCUT NON PARALLELIZED OPTION'
        END IF
      ENDIF
C
      IF (ISPMD.NE.0) GOTO 300
      R4 = 0.
      DO I=1,NSECT+NRWALL+NNWL+NNSRG
       CALL WRITE_R_C(R4,1)
       CALL WRITE_R_C(R4,1)
       CALL WRITE_R_C(R4,1)
      ENDDO
C      
      IF(ANIM_PLY > 0 ) THEN
       IF(ISPMD.EQ.0 .AND. NFNOD_PXFEMG.GT.0) THEN
        R4 = ZERO
        DO I=1,NFNOD_PXFEMG
           CALL WRITE_R_C(R4,1)
           CALL WRITE_R_C(R4,1)
           CALL WRITE_R_C(R4,1)
         ENDDO
       ENDIF
      ENDIF
c----------------------------
C     nodes crk xfem
c----------------------------
      IF (ANIM_CRK > 0 ) THEN           
        IF (ISPMD == 0) THEN            
          R4 = ZERO                     
          DO I=1,NFNOD_CRKXFEMG        
            CALL WRITE_R_C(R4,1)        
            CALL WRITE_R_C(R4,1)        
            CALL WRITE_R_C(R4,1)        
          ENDDO                         
        ENDIF                           
      ENDIF                             
c----------------------------    
      IF (ISPMD.EQ.0.AND.NFVNOD.GT.0) THEN
         R4 = 0.
         DO I=1,NFVNOD+3
            CALL WRITE_R_C(R4,1)
            CALL WRITE_R_C(R4,1)
            CALL WRITE_R_C(R4,1)
         ENDDO
      ENDIF
C-------------
 300  CONTINUE
      RETURN
      END



Chd|====================================================================
Chd|  VELVECC22                     source/output/anim/generate/velvec.F
Chd|-- called by -----------
Chd|        GENANI                        source/output/anim/generate/genani.F
Chd|-- calls ---------------
Chd|        WRITE_R_C                     source/output/tools/sortie_c.c
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|        I22BUFBRIC_MOD                ../common_source/modules/cut-cell-search_mod.F
Chd|        I22TRI_MOD                    ../common_source/modules/cut-cell-search_mod.F
Chd|        INITBUF_MOD                   share/resol/initbuf.F         
Chd|====================================================================
      SUBROUTINE VELVECC22(ELBUF_TAB,IPARG,IFLG,IXS,IXQ,ITAB)
C-----------------------------------------------
C   D e s c r i p t i o n
C-----------------------------------------------
C This subroutines writes at polyedra centroids :
C     velocities       (IFLG=1),
C     momentum density (IFLG=2)
C     internal forces  (IFLG=3),
C for coupling interface 22. Free nodes are used 
C as marker to plot centroid vectors 
C(see input card for grnod_id)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE INITBUF_MOD
      USE ELBUFDEF_MOD  
      USE I22BUFBRIC_MOD   
      USE I22EDGE_MOD    
      USE I22TRI_MOD         
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "task_c.inc"
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "scr05_c.inc"
#include      "scr14_c.inc"
#include      "spmd_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER, INTENT(IN) :: IPARG(NPARG,*), IFLG,IXS(NIXS,NUMELS),IXQ(NIXQ,NUMELQ),ITAB(NUMNOD)
      REAL R4
      TYPE (ELBUF_STRUCT_), DIMENSION(NGROUP), TARGET :: ELBUF_TAB      
      TYPE(BUF_MAT_),POINTER                          :: MBUF   
      TYPE(G_BUFEL_),POINTER                          :: GBUF     
C-----------------------------------------------
C   L o c a l   A r g u m e n t s
C----------------------------------------------- 
      INTEGER :: NGM, IDLOCM, IBM,ICELLM,MLW,NCELL,NELm,NBF,NBL,ICELL,NIN,NODE_ID,IB,NG,I
      my_real :: rho_cell, RHO(4), VFRAC(4)
      REAL,DIMENSION(:,:),ALLOCATABLE :: BUFFER
C-----------------------------------------------

       !---------------------------------------------------------!                 
       NBF = 1                                                                                                                                       
       NBL = NB                                                                                                                                      
       NIN = 1              
       !---------------------------------------------------------!  
       ALLOCATE(BUFFER(3,NUMNOD))
       BUFFER(:,:)           = ZERO  
             
       DO IB=NBF,NBL    
         ICELL               =  0                                                                
         NCELL               = BRICK_LIST(NIN,IB)%NBCUT                                                                                                          
         DO WHILE (ICELL.LE.NCELL) ! loop on polyhedron {1:NCELL} U {9}                                                                            
           ICELL             = ICELL +1                                                                                                                        
           IF (ICELL>NCELL .AND. NCELL/=0)ICELL=9   
           IBM               = BRICK_LIST(NIN,IB)%POLY(ICELL)%WhereIsMaster(4)
           ICELLM            = BRICK_LIST(NIN,IBM)%masterID
           IF(IBM==0)THEN
             IBM             = IB                                                         
             ICELLM          = 1                                                          
           ENDIF
           NGM               = BRICK_LIST(NIN,IBM)%NG                                     
           IDLOCM            = BRICK_LIST(NIN,IBM)%IDLOC                                  
           GBUF              =>ELBUF_TAB(NGM)%GBUF   
           MBUF              =>ELBUF_TAB(NGM)%BUFLY(1)%MAT(1,1,1)                                                                                                            
           NELm              = IPARG(2,NGM)   
           MLW               = IPARG(1,NGM)                                             
           IF(MLW==37)THEN
              !UVAR(I,1) : massic percentage of liquid * global density  (rho1*V1/V : it needs to give liquid mass multipling by element volume in aleconve.F)
              !UVAR(I,2) : density of gas
              !UVAR(I,3) : density of liquid
              !UVAR(I,4) : volumetric fraction of liquid
              !UVAR(I,5) : volumetric fraction of gas               
              RHO(1)         = MBUF%VAR((3-1)*NELm+IDLOCM)
              RHO(2)         = MBUF%VAR((2-1)*NELm+IDLOCM)  
              VFRAC(1)       = MBUF%VAR((4-1)*NELm+IDLOCM)
              VFRAC(2)       = MBUF%VAR((5-1)*NELm+IDLOCM)            
              rho_cell       = RHO(1)*VFRAC(1) + RHO(2)*VFRAC(2)
           ELSEIF(MLW==51)THEN
              RHO(1)         = ZERO
              RHO(2)         = ZERO                 
              rho_cell       = ZERO              
           ELSE 
              rho_cell       = GBUF%RHO(IDLOCM)
           ENDIF
           NODE_ID           = BRICK_LIST(NIN,IB)%POLY(ICELL)%ID_FREE_NODE  
           IF(NODE_ID.LE.0)CYCLE ! not enough nodes in the group or SMP disabling
           IF(IFLG==1)THEN       
             !velocity vector : U                                                          
             BUFFER(1,NODE_ID) = GBUF%MOM(NELm*(1-1) + IDLOCm) / rho_cell                                                                               
             BUFFER(2,NODE_ID) = GBUF%MOM(NELm*(2-1) + IDLOCm) / rho_cell 
             BUFFER(3,NODE_ID) = GBUF%MOM(NELm*(3-1) + IDLOCm) / rho_cell                      
           ELSEIF(IFLG==2)THEN
             !momentum density vector : rho.U
             BUFFER(1,NODE_ID) = GBUF%MOM(NELm*(1-1) + IDLOCm) 
             BUFFER(2,NODE_ID) = GBUF%MOM(NELm*(2-1) + IDLOCm) 
             BUFFER(3,NODE_ID) = GBUF%MOM(NELm*(3-1) + IDLOCm)
           ELSEIF(IFLG==3)THEN   
             !internal force at centroid = sum(integral(P.dS))
             BUFFER(1,NODE_ID) = BRICK_LIST(NIN,IBM)%FCELL(1)
             BUFFER(2,NODE_ID) = BRICK_LIST(NIN,IBM)%FCELL(2)
             BUFFER(3,NODE_ID) = BRICK_LIST(NIN,IBM)%FCELL(3)
           ELSE
             BUFFER(1,NODE_ID) = ZERO
             BUFFER(2,NODE_ID) = ZERO
             BUFFER(3,NODE_ID) = ZERO                          
           ENDIF
         ENDDO !next ICELL          
       ENDDO!next IB
        
       DO I=1,NUMNOD      
         R4 = BUFFER(1,I)
         CALL WRITE_R_C(R4,1)
         R4 = BUFFER(2,I)
         CALL WRITE_R_C(R4,1)
         R4 = BUFFER(3,I)
         CALL WRITE_R_C(R4,1)
       ENDDO!next I
          
       DEALLOCATE(BUFFER)   
      !---------------------------------------------------------! 

      RETURN
      END
