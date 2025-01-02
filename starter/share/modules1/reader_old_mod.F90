!Copyright>        OpenRadioss
!Copyright>        Copyright (C) 1986-2025 Altair Engineering Inc.
!Copyright>
!Copyright>        This program is free software: you can redistribute it and/or modify
!Copyright>        it under the terms of the GNU Affero General Public License as published by
!Copyright>        the Free Software Foundation, either version 3 of the License, or
!Copyright>        (at your option) any later version.
!Copyright>
!Copyright>        This program is distributed in the hope that it will be useful,
!Copyright>        but WITHOUT ANY WARRANTY; without even the implied warranty of
!Copyright>        MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!Copyright>        GNU Affero General Public License for more details.
!Copyright>
!Copyright>        You should have received a copy of the GNU Affero General Public License
!Copyright>        along with this program.  If not, see <https://www.gnu.org/licenses/>.
!Copyright>
!Copyright>
!Copyright>        Commercial Alternative: Altair Radioss Software
!Copyright>
!Copyright>        As an alternative to this open-source version, Altair also offers Altair Radioss
!Copyright>        software under a commercial license.  Contact Altair to discuss further if the
!Copyright>        commercial version may interest you: https://www.altair.com/radioss/.
 !! \brief Direct Reader (old reader) parameters
!! \details current Reader is based on library. Direct Reader (Fortran READ statement) is only related to limited specific cases
      !||====================================================================
      !||    reader_old_mod             ../starter/share/modules1/reader_old_mod.F90
      !||--- called by ------------------------------------------------------
      !||    admlcnt                    ../starter/source/model/remesh/admlist.F
      !||    admlist                    ../starter/source/model/remesh/admlist.F
      !||    bigbox                     ../starter/source/model/box/bigbox.F
      !||    bigbox2                    ../starter/source/model/box/bigbox.F
      !||    bigsbox                    ../starter/source/model/box/bigbox.F
      !||    fredec0                    ../starter/source/starter/freform.F
      !||    fredec4                    ../starter/source/starter/freform.F
      !||    fredec5                    ../starter/source/starter/freform.F
      !||    fredec6                    ../starter/source/starter/freform.F
      !||    fredec_2key_4id            ../starter/source/starter/freform.F
      !||    fredec_2key_4id_t          ../starter/source/starter/freform.F
      !||    fredec_2key_id_or_key_id   ../starter/source/starter/freform.F
      !||    fredec_key_3id_t           ../starter/source/starter/freform.F
      !||    freerr                     ../starter/source/starter/freform.F
      !||    hm_prelce16s               ../starter/source/elements/reader/hm_read_solid.F
      !||    hm_preread_node            ../starter/source/elements/reader/hm_preread_node.F
      !||    hm_read_beam               ../starter/source/elements/reader/hm_read_beam.F
      !||    hm_read_cluster            ../starter/source/output/cluster/hm_read_cluster.F
      !||    hm_read_friction_models    ../starter/source/interfaces/friction/reader/hm_read_friction_models.F
      !||    hm_read_grav               ../starter/source/loads/general/grav/hm_read_grav.F
      !||    hm_read_inter_type24       ../starter/source/interfaces/int24/hm_read_inter_type24.F
      !||    hm_read_lines              ../starter/source/groups/hm_read_lines.F
      !||    hm_read_mat                ../starter/source/materials/mat/hm_read_mat.F90
      !||    hm_read_mat10              ../starter/source/materials/mat/mat010/hm_read_mat10.F
      !||    hm_read_mat29_31           ../starter/source/materials/mat/matuser/hm_read_mat_user29_31.F
      !||    hm_read_mat_99             ../starter/source/materials/mat/matuser/hm_read_mat_user_99.F
      !||    hm_read_prop_generic       ../starter/source/properties/hm_read_prop_generic.F
      !||    hm_read_quad               ../starter/source/elements/reader/hm_read_quad.F
      !||    hm_read_rivet              ../starter/source/elements/reader/hm_read_rivet.F
      !||    hm_read_sh3n               ../starter/source/elements/reader/hm_read_sh3n.F
      !||    hm_read_shell              ../starter/source/elements/reader/hm_read_shell.F
      !||    hm_read_sphcel             ../starter/source/elements/reader/hm_read_sphcel.F
      !||    hm_read_spring             ../starter/source/elements/reader/hm_read_spring.F
      !||    hm_read_surfsurf           ../starter/source/groups/hm_read_surfsurf.F
      !||    hm_read_table1             ../starter/source/tools/curve/hm_read_table.F
      !||    hm_read_tria               ../starter/source/elements/reader/hm_read_tria.F
      !||    hm_read_truss              ../starter/source/elements/reader/hm_read_truss.F
      !||    lec_ddw                    ../starter/source/spmd/domain_decomposition/grid2mat.F
      !||    lec_ddw_poin               ../starter/source/spmd/domain_decomposition/grid2mat.F
      !||    lec_inistate_yfile         ../starter/source/initial_conditions/inista/lec_inistate_yfile.F
      !||    lecextlnk                  ../starter/source/coupling/rad2rad/lecextlnk.F
      !||    lecig3d                    ../starter/source/elements/ige3d/lecig3d.F
      !||    lecrefsta                  ../starter/source/loads/reference_state/refsta/lecrefsta.F
      !||    nbadigemesh                ../starter/source/elements/ige3d/nbadigemesh.F
      !||    nbadmesh                   ../starter/source/model/remesh/nbadmesh.F
      !||    nextsla                    ../starter/source/starter/freform.F
      !||    prelec_ddw                 ../starter/source/spmd/domain_decomposition/grid2mat.F
      !||    prelec_ddw_poin            ../starter/source/spmd/domain_decomposition/grid2mat.F
      !||    prelecig3d                 ../starter/source/elements/ige3d/prelecig3d.F
      !||    r2r_exist                  ../starter/source/coupling/rad2rad/routines_r2r.F
      !||    r2r_group                  ../starter/source/coupling/rad2rad/r2r_group.F
      !||    r2r_input                  ../starter/source/coupling/rad2rad/r2r_input.F
      !||    r2r_listcnt                ../starter/source/coupling/rad2rad/routines_r2r.F
      !||    r2r_prelec                 ../starter/source/coupling/rad2rad/r2r_prelec.F
      !||    read_sensor_user           ../starter/source/tools/sensor/read_sensor_user.F
      !||    sz_r2r                     ../starter/source/coupling/rad2rad/routines_r2r.F
      !||    user_output                ../starter/source/user_interface/user_output.F
      !||    yctrl                      ../starter/source/initial_conditions/inista/yctrl.F
      !||--- uses       -----------------------------------------------------
      !||====================================================================
      module reader_old_mod

      use names_and_titles_mod , only : ncharline

      !old Reader parameters
      INTEGER, PARAMETER :: MAXNOPT = 220 !< maximum number of option

      INTEGER,PARAMETER :: KPART  = 26   !< cursor parameter for /PART option
      INTEGER,PARAMETER :: KPROP  = 45   !< cursor parameter for /PROP option
      INTEGER,PARAMETER :: KFUNCT = 46   !< cursor parameter for /FUNCT option
      INTEGER,PARAMETER :: KR2R   = 52   !< cursor parameter for /EXTERN (R2R) option
      INTEGER,PARAMETER :: KINTER = 61   !< cursor parameter for /INTER option
      INTEGER,PARAMETER :: KSPHOPT = 96   !< cursor parameter for /SPH option
      INTEGER,PARAMETER :: KCNODE = 118   !< cursor parameter for /CNODE option
      INTEGER,PARAMETER :: KIGE3D = 148   !< cursor parameter for /IGE3D option

      INTEGER,PARAMETER :: zeroArray(MAXNOPT) = 0
      INTEGER :: KOPTAD(MAXNOPT+2) = (/1,1,zeroArray/)

      INTEGER :: NLINE(MAXNOPT) = 0  !<
      INTEGER :: NSLASH(MAXNOPT) = 0 !< number of options
      INTEGER :: KCUR !< initial cursor position for a given option (KPART, KPROP, ...)
      INTEGER :: IREC !< current line number

      CHARACTER(LEN=NCHARLINE) :: KLINE !line backup
      CHARACTER(LEN=NCHARLINE) :: LINE  !current line

      CHARACTER(LEN=10), PARAMETER :: KEY0(MAXNOPT) = (/ &
        'TITLE     '  ,& ! 001
        'KMEMOR    '  ,& ! 002
        'ARCH      '  ,& ! 003
        'SPMD      '  ,& ! 004
        'CAA       '  ,& ! 005
        'IOFLA     '  ,& ! 006
        'REFSTA    '  ,& ! 007
        'XREF      '  ,& ! 008
        'EREF      '  ,& ! 009
        'ANALY     '  ,& ! 010
        'DEF_SOLI  '  ,& ! 011
        'DEF_SHEL  '  ,& ! 012
        'ALE       '  ,& ! 013
        'UPWIND    '  ,& ! 014
        'EULER     '  ,& ! 015
        'EBCS      '  ,& ! 016
        'HEAT      '  ,& ! 017
        'THERM_ST  '  ,& ! 018
        'DFS       '  ,& ! 019
        'RANDOM    '  ,& ! 020
        'LAGMUL    '  ,& ! 021
        'MAT       '  ,& ! 022
        'NODE      '  ,& ! 023
        'BCS       '  ,& ! 024
        'SKEW      '  ,& ! 025
        'PART      '  ,& ! 026
        'THPART    '  ,& ! 027
        'BRICK     '  ,& ! 028
        'TETRA4    '  ,& ! 029
        'TETRA1    '  ,& ! 030
        'BRIC20    '  ,& ! 031
        'SHEL16    '  ,& ! 032
        'QUAD      '  ,& ! 033
        'SHELL     '  ,& ! 034
        'SH3N      '  ,& ! 035
        'SHFRA     '  ,& ! 036
        'TRUSS     '  ,& ! 037
        'BEAM      '  ,& ! 038
        'SPRING    '  ,& ! 039
        'LIAISO    '  ,& ! 040
        'GRNOD     '  ,& ! 041
        'SUBSET    '  ,& ! 042
        'SURF      '  ,& ! 043
        'LINE      '  ,& ! 044
        'PROP      '  ,& ! 045
        'FUNCT     '  ,& ! 046
        'FUNC_2D   '  ,& ! 047
        'MOVE_FUN  '  ,& ! 048
        'CLOAD     '  ,& ! 049
        'PLOAD     '  ,& ! 050
        'GRAV      '  ,& ! 051
        'EXTERN    '  ,& ! 052
        'SUBDOMAIN '  ,& ! 053
        'INIVEL    '  ,& ! 054
        'ACCEL     '  ,& ! 055
        'SENSOR    '  ,& ! 056
        'IBVEL     '  ,& ! 057
        'ACTIV     '  ,& ! 058
        'GJOINT    '  ,& ! 059
        'MPC       '  ,& ! 060
        'INTER     '  ,& ! 061
        'FRICTION  '  ,& ! 062
        'FRIC_ORIEN'  ,& ! 063
        'RWALL     '  ,& ! 064
        'RBODY     '  ,& ! 065
        'FXBODY    '  ,& ! 066
        'RLINK     '  ,& ! 067
        'MADYMO    '  ,& ! 068
        'ADMAS     '  ,& ! 069
        'IMPVEL    '  ,& ! 070
        'FIXVEL    '  ,& ! 071
        'FIXDIS    '  ,& ! 072
        'IMPDIS    '  ,& ! 073
        'IMPACC    '  ,& ! 074
        'RIVET     '  ,& ! 075
        'SECT      '  ,& ! 076
        'CYL_JO    '  ,& ! 077
        'MONVOL    '  ,& ! 078
        'TH        '  ,& ! 079
        'INISTA    '  ,& ! 080
        'INIMAP1D  '  ,& ! 081
        'INIMAP2D  '  ,& ! 082
        'XELEM     '  ,& ! 083
        'USERW     '  ,& ! 084
        'DAMP      '  ,& ! 085
        'FRAME     '  ,& ! 086
        'SPHCEL    '  ,& ! 087
        'SPHGLO    '  ,& ! 088
        'SPHBCS    '  ,& ! 089
        'INISHE    '  ,& ! 090
        'INISH3    '  ,& ! 091
        'INIBRI    '  ,& ! 092
        'INIQUA    '  ,& ! 093
        'INIBEA    '  ,& ! 094
        'INITRU    '  ,& ! 095
        'SPH       '  ,& ! 096
        'ATH       '  ,& ! 097
        'BTH       '  ,& ! 098
        'CTH       '  ,& ! 099
        'DTH       '  ,& ! 100
        'ETH       '  ,& ! 101
        'FTH       '  ,& ! 102
        'GTH       '  ,& ! 103
        'HTH       '  ,& ! 104
        'ITH       '  ,& ! 105
        'PENTA6    '  ,& ! 106
        'EIG       '  ,& ! 107
        'UNIT      '  ,& ! 108
        'FAIL      '  ,& ! 109
        'KEY       '  ,& ! 110
        'ENCRYPT   '  ,& ! 111
        'IMPLICIT  '  ,& ! 112
        'IMPL      '  ,& ! 113
        'BEM       '  ,& ! 114
        'ANIM      '  ,& ! 115
        'BEGIN     '  ,& ! 116
        'PREPRO    '  ,& ! 117
        'CNODE     '  ,& ! 118
        'TRANSF    '  ,& ! 119
        '/SUBMOD   '  ,& ! 120
        '/ENDSU    '  ,& ! 121
        'LEVSET    '  ,& ! 122
        'ADMESH    '  ,& ! 123
        'INITEM    '  ,& ! 124
        'IMPTEM    '  ,& ! 125
        'IMPFLUX   '  ,& ! 126
        'CONVEC    '  ,& ! 127
        'STAMPING  '  ,& ! 128
        'RBE2      '  ,& ! 129
        'RBE3      '  ,& ! 130
        'INTTHICK  '  ,& ! 131
        'PLYXFEM   '  ,& ! 132
        'AMS       '  ,& ! 133
        'VISC      '  ,& ! 134
        'RADIATION '  ,& ! 135
        'TABLE     '  ,& ! 136
        'BOX       '  ,& ! 137
        'STATE     '  ,& ! 138
        'INIVOL    '  ,& ! 139
        'GAUGE     '  ,& ! 140
        'EOS       '  ,& ! 141
        'LOAD      '  ,& ! 142
        'LEAK      '  ,& ! 143
        'CRASH     '  ,& ! 144
        'INICRACK  '  ,& ! 145
        'CLUSTER   '  ,& ! 146
        'DEFAULT   '  ,& ! 147
        'IGE3D     '  ,& ! 148
        'PRIVATE   '  ,& ! 149
        'PERTURB   '  ,& ! 150
        'STACK     '  ,& ! 151
        'PLY       '  ,& ! 152
        'DRAPE     '  ,& ! 153
        'INT2      '  ,& ! 154
        'INISPRI   '  ,& ! 155
        'SH_3NFRA  '  ,& ! 156
        'S17_OLD   '  ,& ! 157
        'NBCS      '  ,& ! 158
        'PRELOAD   '  ,& ! 159
        'INIGRAV   '  ,& ! 160
        'TRIA      '  ,& ! 161
        'MERGE     '  ,& ! 162
        'SET       '  ,& ! 163
        'CHECK     '  ,& ! 164
        'END       '  ,& ! 165
        '          '  ,& ! 166
        '          '  ,& ! 167
        '          '  ,& ! 168
        '          '  ,& ! 169
        '          '  ,& ! 170
        '          '  ,& ! 171
        '          '  ,& ! 172
        '          '  ,& ! 173
        '          '  ,& ! 174
        '          '  ,& ! 175
        '          '  ,& ! 176
        '          '  ,& ! 177
        '          '  ,& ! 178
        '          '  ,& ! 179
        '          '  ,& ! 180
        '          '  ,& ! 181
        '          '  ,& ! 182
        '          '  ,& ! 183
        '          '  ,& ! 184
        '          '  ,& ! 185
        '          '  ,& ! 186
        '          '  ,& ! 187
        '          '  ,& ! 188
        '          '  ,& ! 189
        '          '  ,& ! 190
        '          '  ,& ! 191
        '          '  ,& ! 192
        '          '  ,& ! 193
        '          '  ,& ! 194
        '          '  ,& ! 195
        '          '  ,& ! 196
        '          '  ,& ! 197
        '          '  ,& ! 198
        '          '  ,& ! 199
        '          '  ,& ! 200
        '          '  ,& ! 201
        '          '  ,& ! 202
        '          '  ,& ! 203
        '          '  ,& ! 204
        '          '  ,& ! 205
        '          '  ,& ! 206
        '          '  ,& ! 207
        '          '  ,& ! 208
        '          '  ,& ! 209
        '          '  ,& ! 210
        '          '  ,& ! 211
        '          '  ,& ! 212
        '          '  ,& ! 213
        '          '  ,& ! 214
        '          '  ,& ! 215
        '          '  ,& ! 216
        '          '  ,& ! 217
        '          '  ,& ! 218
        '          '  ,& ! 219
        '          '   & ! 220
        /)

       INTEGER, PARAMETER :: LKEY0(MAXNOPT) = (/ &
         5            ,& ! 001
         5            ,& ! 002
         4            ,& ! 003
         4            ,& ! 004
         3            ,& ! 005
         5            ,& ! 006
         6            ,& ! 007
         4            ,& ! 008
         4            ,& ! 009
         5            ,& ! 010
         6            ,& ! 011
         6            ,& ! 012
         3            ,& ! 013
         6            ,& ! 014
         5            ,& ! 015
         4            ,& ! 016
         4            ,& ! 017
         8            ,& ! 018
         3            ,& ! 019
         6            ,& ! 020
         6            ,& ! 021
         3            ,& ! 022
         4            ,& ! 023
         3            ,& ! 024
         4            ,& ! 025
         4            ,& ! 026
         6            ,& ! 027
         5            ,& ! 028
         6            ,& ! 029
         6            ,& ! 030
         6            ,& ! 031
         6            ,& ! 032
         4            ,& ! 033
         5            ,& ! 034
         4            ,& ! 035
         5            ,& ! 036
         5            ,& ! 037
         4            ,& ! 038
         6            ,& ! 039
         6            ,& ! 040
         5            ,& ! 041
         6            ,& ! 042
         4            ,& ! 043
         4            ,& ! 044
         4            ,& ! 045
         5            ,& ! 046
         7            ,& ! 047
         8            ,& ! 048
         5            ,& ! 049
         5            ,& ! 050
         4            ,& ! 051
         6            ,& ! 052
         9            ,& ! 053
         6            ,& ! 054
         5            ,& ! 055
         6            ,& ! 056
         5            ,& ! 057
         5            ,& ! 058
         6            ,& ! 059
         3            ,& ! 060
         5            ,& ! 061
         8            ,& ! 062
         10           ,& ! 063
         5            ,& ! 064
         5            ,& ! 065
         6            ,& ! 066
         5            ,& ! 067
         6            ,& ! 068
         5            ,& ! 069
         6            ,& ! 070
         6            ,& ! 071
         6            ,& ! 072
         6            ,& ! 073
         6            ,& ! 074
         5            ,& ! 075
         4            ,& ! 076
         6            ,& ! 077
         6            ,& ! 078
         2            ,& ! 079
         6            ,& ! 080
         8            ,& ! 081
         8            ,& ! 082
         5            ,& ! 083
         5            ,& ! 084
         4            ,& ! 085
         5            ,& ! 086
         6            ,& ! 087
         6            ,& ! 088
         6            ,& ! 089
         6            ,& ! 090
         6            ,& ! 091
         6            ,& ! 092
         6            ,& ! 093
         6            ,& ! 094
         6            ,& ! 095
         3            ,& ! 096
         3            ,& ! 097
         3            ,& ! 098
         3            ,& ! 099
         3            ,& ! 100
         3            ,& ! 101
         3            ,& ! 102
         3            ,& ! 103
         3            ,& ! 104
         3            ,& ! 105
         6            ,& ! 106
         3            ,& ! 107
         4            ,& ! 108
         4            ,& ! 109
         3            ,& ! 110
         7            ,& ! 111
         8            ,& ! 112
         4            ,& ! 113
         3            ,& ! 114
         4            ,& ! 115
         5            ,& ! 116
         6            ,& ! 117
         5            ,& ! 118
         6            ,& ! 119
         6            ,& ! 120
         6            ,& ! 121
         6            ,& ! 122
         6            ,& ! 123
         6            ,& ! 124
         6            ,& ! 125
         7            ,& ! 126
         6            ,& ! 127
         8            ,& ! 128
         4            ,& ! 129
         4            ,& ! 130
         8            ,& ! 131
         7            ,& ! 132
         3            ,& ! 133
         4            ,& ! 134
         9            ,& ! 135
         5            ,& ! 136
         3            ,& ! 137
         5            ,& ! 138
         6            ,& ! 139
         5            ,& ! 140
         3            ,& ! 141
         4            ,& ! 142
         4            ,& ! 143
         5            ,& ! 144
         8            ,& ! 145
         7            ,& ! 146
         7            ,& ! 147
         5            ,& ! 148
         7            ,& ! 149
         7            ,& ! 150
         5            ,& ! 151
         3            ,& ! 152
         5            ,& ! 153
         4            ,& ! 154
         7            ,& ! 155
         8            ,& ! 156
         7            ,& ! 157
         3            ,& ! 158
         7            ,& ! 159
         7            ,& ! 160
         4            ,& ! 161
         5            ,& ! 162
         3            ,& ! 163
         5            ,& ! 164
         3            ,& ! 165
         0            ,& ! 166
         0            ,& ! 167
         0            ,& ! 168
         0            ,& ! 169
         0            ,& ! 170
         0            ,& ! 171
         0            ,& ! 172
         0            ,& ! 173
         0            ,& ! 174
         0            ,& ! 175
         0            ,& ! 176
         0            ,& ! 177
         0            ,& ! 178
         0            ,& ! 179
         0            ,& ! 180
         0            ,& ! 181
         0            ,& ! 182
         0            ,& ! 183
         0            ,& ! 184
         0            ,& ! 185
         0            ,& ! 186
         0            ,& ! 187
         0            ,& ! 188
         0            ,& ! 189
         0            ,& ! 190
         0            ,& ! 191
         0            ,& ! 192
         0            ,& ! 193
         0            ,& ! 194
         0            ,& ! 195
         0            ,& ! 196
         0            ,& ! 197
         0            ,& ! 198
         0            ,& ! 199
         0            ,& ! 200
         0            ,& ! 201
         0            ,& ! 202
         0            ,& ! 203
         0            ,& ! 204
         0            ,& ! 205
         0            ,& ! 206
         0            ,& ! 207
         0            ,& ! 208
         0            ,& ! 209
         0            ,& ! 210
         0            ,& ! 211
         0            ,& ! 212
         0            ,& ! 213
         0            ,& ! 214
         0            ,& ! 215
         0            ,& ! 216
         0            ,& ! 217
         0            ,& ! 218
         0            ,& ! 219
         0             & ! 220
        /)

      end module reader_old_mod
