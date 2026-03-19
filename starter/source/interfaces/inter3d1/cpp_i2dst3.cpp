// =============================================================================
// cpp_i2dst3.cpp - C++ translation of i2dst3.F
// =============================================================================
// Exact same algorithm, same argument list conventions.
// Fortran 1-based first/last are converted to 0-based C indices at entry.
// =============================================================================

#include <cmath>
#include <algorithm>

#ifdef MYREAL8
using my_real = double;
#else
using my_real = float;
#endif

// --- Constants matching OpenRadioss CONSTANT_MOD ---
static constexpr my_real ZERO   = 0.0;
static constexpr my_real ONE    = 1.0;
static constexpr my_real TWO    = 2.0;
static constexpr my_real FOURTH = 0.25;
static constexpr my_real ONEP5  = 1.5;
static constexpr my_real EM10   = 1.0e-10;
static constexpr my_real EM20   = 1.0e-20;


static constexpr int MVSIZ = 512;

// =============================================================================
// I2BAR3 - Barycentric coordinate projection for one triangle edge
// =============================================================================
// Arrays are 0-based C convention. first/last are 0-based loop bounds.
static void i2bar3(
    const int first, const int last,
    my_real* XI, my_real* YI, my_real* ZI,
    my_real* XA, my_real* YA, my_real* ZA,
    my_real* XB, my_real* YB, my_real* ZB,
    my_real* XC, my_real* YC, my_real* ZC,
    my_real* NX, my_real* NY, my_real* NZ,
    my_real* LB, my_real* LC, my_real* P,
    my_real* GAPV,
    int*     TFLAG)
{
    // --- Loop 1: cross product, normalize, projection, barycentric coords ---
    for (int i = first; i <= last; ++i) {
        my_real XAB = XB[i] - XA[i];
        my_real YAB = YB[i] - YA[i];
        my_real ZAB = ZB[i] - ZA[i];

        my_real XAC = XC[i] - XA[i];
        my_real YAC = YC[i] - YA[i];
        my_real ZAC = ZC[i] - ZA[i];

        NX[i] = YAB*ZAC - ZAB*YAC;
        NY[i] = ZAB*XAC - XAB*ZAC;
        NZ[i] = XAB*YAC - YAB*XAC;

        my_real S2 = ONE / std::max(EM20, std::sqrt(NX[i]*NX[i] + NY[i]*NY[i] + NZ[i]*NZ[i]));
        NX[i] *= S2;
        NY[i] *= S2;
        NZ[i] *= S2;

        P[i] = NX[i] * (XI[i] - XA[i])
             + NY[i] * (YI[i] - YA[i])
             + NZ[i] * (ZI[i] - ZA[i]);

        my_real XP = XI[i] - NX[i] * P[i];
        my_real YP = YI[i] - NY[i] * P[i];
        my_real ZP = ZI[i] - NZ[i] * P[i];

        my_real XPA = XA[i] - XP;
        my_real YPA = YA[i] - YP;
        my_real ZPA = ZA[i] - ZP;

        my_real XPB = XB[i] - XP;
        my_real YPB = YB[i] - YP;
        my_real ZPB = ZB[i] - ZP;

        my_real XPC = XC[i] - XP;
        my_real YPC = YC[i] - YP;
        my_real ZPC = ZC[i] - ZP;

        my_real SX = YPC*ZPA - ZPC*YPA;
        my_real SY = ZPC*XPA - XPC*ZPA;
        my_real SZ = XPC*YPA - YPC*XPA;

        LB[i] = (NX[i]*SX + NY[i]*SY + NZ[i]*SZ) * S2;

        SX = YPA*ZPB - ZPA*YPB;
        SY = ZPA*XPB - XPA*ZPB;
        SZ = XPA*YPB - YPA*XPB;

        LC[i] = (NX[i]*SX + NY[i]*SY + NZ[i]*SZ) * S2;
    }

    // --- Loop 2: edge projection corrections ---
    for (int i = first; i <= last; ++i) {
        if (ONE - LB[i] - LC[i] < ZERO) {
            // inlined I7LIN3: project onto edge BC
            my_real L_XAB = XC[i] - XB[i];
            my_real L_YAB = YC[i] - YB[i];
            my_real L_ZAB = ZC[i] - ZB[i];
            my_real ALP = (XI[i]-XB[i])*L_XAB
                        + (YI[i]-YB[i])*L_YAB
                        + (ZI[i]-ZB[i])*L_ZAB;
            my_real L_NORM = ONE / std::max(EM20, L_XAB*L_XAB + L_YAB*L_YAB + L_ZAB*L_ZAB);
            ALP = std::min(std::max(ALP * L_NORM, ZERO), ONE);
            my_real XP = XB[i] + ALP*L_XAB;
            my_real YP = YB[i] + ALP*L_YAB;
            my_real ZP = ZB[i] + ALP*L_ZAB;
            NX[i] = XI[i] - XP;
            NY[i] = YI[i] - YP;
            NZ[i] = ZI[i] - ZP;
            P[i] = std::sqrt(NX[i]*NX[i] + NY[i]*NY[i] + NZ[i]*NZ[i]);
            L_NORM = ONE / std::max(EM20, P[i]);
            NX[i] *= L_NORM;
            NY[i] *= L_NORM;
            NZ[i] *= L_NORM;

        } else if (LB[i] < ZERO) {
            // inlined I7LIN3: project onto edge CA
            my_real L_XAB = XA[i] - XC[i];
            my_real L_YAB = YA[i] - YC[i];
            my_real L_ZAB = ZA[i] - ZC[i];
            my_real ALP = (XI[i]-XC[i])*L_XAB
                        + (YI[i]-YC[i])*L_YAB
                        + (ZI[i]-ZC[i])*L_ZAB;
            my_real L_NORM = ONE / std::max(EM20, L_XAB*L_XAB + L_YAB*L_YAB + L_ZAB*L_ZAB);
            ALP = std::min(std::max(ALP * L_NORM, ZERO), ONE);
            my_real XP = XC[i] + ALP*L_XAB;
            my_real YP = YC[i] + ALP*L_YAB;
            my_real ZP = ZC[i] + ALP*L_ZAB;
            NX[i] = XI[i] - XP;
            NY[i] = YI[i] - YP;
            NZ[i] = ZI[i] - ZP;
            P[i] = std::sqrt(NX[i]*NX[i] + NY[i]*NY[i] + NZ[i]*NZ[i]);
            L_NORM = ONE / std::max(EM20, P[i]);
            NX[i] *= L_NORM;
            NY[i] *= L_NORM;
            NZ[i] *= L_NORM;
            if (TFLAG[i] == 0) {  // only necessary for warped 4 node segments
                LC[i] = ONE - ALP;
                LB[i] = ZERO;
            }

        } else if (LC[i] < ZERO) {
            // inlined I7LIN3: project onto edge AB
            my_real L_XAB = XB[i] - XA[i];
            my_real L_YAB = YB[i] - YA[i];
            my_real L_ZAB = ZB[i] - ZA[i];
            my_real ALP = (XI[i]-XA[i])*L_XAB
                        + (YI[i]-YA[i])*L_YAB
                        + (ZI[i]-ZA[i])*L_ZAB;
            my_real L_NORM = ONE / std::max(EM20, L_XAB*L_XAB + L_YAB*L_YAB + L_ZAB*L_ZAB);
            ALP = std::min(std::max(ALP * L_NORM, ZERO), ONE);
            my_real XP = XA[i] + ALP*L_XAB;
            my_real YP = YA[i] + ALP*L_YAB;
            my_real ZP = ZA[i] + ALP*L_ZAB;
            NX[i] = XI[i] - XP;
            NY[i] = YI[i] - YP;
            NZ[i] = ZI[i] - ZP;
            P[i] = std::sqrt(NX[i]*NX[i] + NY[i]*NY[i] + NZ[i]*NZ[i]);
            L_NORM = ONE / std::max(EM20, P[i]);
            NX[i] *= L_NORM;
            NY[i] *= L_NORM;
            NZ[i] *= L_NORM;
            if (TFLAG[i] == 0) {  // only necessary for warped 4 node segments
                LB[i] = ALP;
                LC[i] = ZERO;
            }

        } else if (P[i] < ZERO) {
            NX[i] = -NX[i];
            NY[i] = -NY[i];
            NZ[i] = -NZ[i];
            P[i]  = -P[i];
        }
    }

    // --- Loop 3: gap clamp ---
    for (int i = first; i <= last; ++i) {
        P[i] = std::max(ZERO, GAPV[i] - P[i]);
    }
}

// =============================================================================
// I2DST3 - Distance computation for interface type 2 (3D surface contact)
// =============================================================================
// Fortran-compatible interface: all arrays are 0-based C pointers.
// IRTL, ST, DMIN are indirect-indexed via CAND_N (1-based Fortran indices).
extern "C"
void cpp_i2dst3_(
    const int& first, const int& last,
    my_real* GAPV,
    int*     CAND_E,
    int*     CAND_N,
    const my_real& TZINF,
    int*     IRTL,
    my_real* ST,       // ST(2,*) in Fortran => column-major: ST[2*(j-1)+0], ST[2*(j-1)+1]
    my_real* DMIN,
    const int& IGNORE,
    const int* IX3,
    const int* IX4,
    my_real* X1, my_real* X2,
    my_real* X3, my_real* X4,
    my_real* Y1, my_real* Y2,
    my_real* Y3, my_real* Y4,
    my_real* Z1, my_real* Z2,
    my_real* Z3, my_real* Z4,
    my_real* XI, my_real* YI, my_real* ZI,
    my_real* X0, my_real* Y0, my_real* Z0,
    my_real* NX1, my_real* NY1, my_real* NZ1,
    my_real* NX2, my_real* NY2, my_real* NZ2,
    my_real* NX3, my_real* NY3, my_real* NZ3,
    my_real* NX4, my_real* NY4, my_real* NZ4,
    my_real* P1,  my_real* P2,
    my_real* P3,  my_real* P4,
    my_real* LB1, my_real* LB2,
    my_real* LB3, my_real* LB4,
    my_real* LC1, my_real* LC2,
    my_real* LC3, my_real* LC4,
    my_real* S,   my_real* T)
{
    int    TFLAG[MVSIZ];
    my_real PENE[MVSIZ];

    // --- Convert Fortran 1-based loop bounds to C 0-based ---
    // Fortran passes the address of the first element, so arr[0] = Fortran arr(1).
    const int c_first = first - 1;
    const int c_last  = last  - 1;

    // --- Compute quad centroid ---
    for (int i = c_first; i <= c_last; ++i) {
        X0[i] = FOURTH * (X1[i] + X2[i] + X3[i] + X4[i]);
        Y0[i] = FOURTH * (Y1[i] + Y2[i] + Y3[i] + Y4[i]);
        Z0[i] = FOURTH * (Z1[i] + Z2[i] + Z3[i] + Z4[i]);
    }

    // --- Degenerate quad (triangle): use node 3 as centroid ---
    for (int i = c_first; i <= c_last; ++i) {
        if (IX3[i] == IX4[i]) {
            X0[i] = X3[i];
            Y0[i] = Y3[i];
            Z0[i] = Z3[i];
            TFLAG[i] = 1;
        } else {
            TFLAG[i] = 0;
        }
    }

    // --- 4 calls to I2BAR3 for each edge of the quad ---
    i2bar3(c_first, c_last, XI, YI, ZI, X0, Y0, Z0, X1, Y1, Z1, X2, Y2, Z2,
           NX1, NY1, NZ1, LB1, LC1, P1, GAPV, TFLAG);

    i2bar3(c_first, c_last, XI, YI, ZI, X0, Y0, Z0, X2, Y2, Z2, X3, Y3, Z3,
           NX2, NY2, NZ2, LB2, LC2, P2, GAPV, TFLAG);

    i2bar3(c_first, c_last, XI, YI, ZI, X0, Y0, Z0, X3, Y3, Z3, X4, Y4, Z4,
           NX3, NY3, NZ3, LB3, LC3, P3, GAPV, TFLAG);

    i2bar3(c_first, c_last, XI, YI, ZI, X0, Y0, Z0, X4, Y4, Z4, X1, Y1, Z1,
           NX4, NY4, NZ4, LB4, LC4, P4, GAPV, TFLAG);

    // --- Select penetration face with max P (independent IFs, P1 highest priority) ---
    for (int i = c_first; i <= c_last; ++i) {
        PENE[i] = std::max({P1[i], P2[i], P3[i], P4[i]});
        // Reverse priority: last match wins => P1 has highest priority
        S[i] = ZERO;
        T[i] = ZERO;
        if (P4[i] == PENE[i]) {
            S[i] = -LB4[i] - LC4[i];
            T[i] =  LB4[i] - LC4[i];
        }
        if (P3[i] == PENE[i]) {
            S[i] =  LB3[i] - LC3[i];
            T[i] =  LB3[i] + LC3[i];
        }
        if (P2[i] == PENE[i]) {
            S[i] =  LB2[i] + LC2[i];
            T[i] = -LB2[i] + LC2[i];
        }
        if (P1[i] == PENE[i]) {
            S[i] = -LB1[i] + LC1[i];
            T[i] = -LB1[i] - LC1[i];
        }
    }

    // --- Triangle (degenerate quad) special case ---
    for (int i = c_first; i <= c_last; ++i) {
        if (TFLAG[i] == 1) {
            PENE[i] = P1[i];
            T[i] = ONE - TWO*LB1[i] - TWO*LC1[i];
            if (T[i] < ONE - EM10) {
                S[i] = (LC1[i] - LB1[i]) / (LC1[i] + LB1[i]);
            } else if (LB1[i] < -EM10) {
                S[i] = TWO;
            } else if (LC1[i] < -EM10) {
                S[i] = -TWO;
            } else {
                S[i] = ZERO;
            }
        }
    }

    // --- Scatter results into DMIN/IRTL/ST (indirect access via CAND_N) ---
    // ST is Fortran column-major ST(2,*): element (r,c) is at ST[2*(c-1)+(r-1)]
    // Using 1-based Fortran indexing: ST(1,II) => ST[2*(II-1)+0], ST(2,II) => ST[2*(II-1)+1]

    if (IGNORE == 2 || IGNORE == 3) {
        for (int i = c_first; i <= c_last; ++i) {
            if (PENE[i] > ZERO &&
                S[i] < ONEP5 && T[i] < ONEP5 &&
                S[i] > -ONEP5 && T[i] > -ONEP5) {
                int II = CAND_N[i];
                my_real gap_pene = GAPV[i] - PENE[i];
                if (gap_pene < DMIN[II-1]) {
                    DMIN[II-1] = gap_pene;
                    IRTL[II-1] = CAND_E[i];
                    ST[2*(II-1)]   = S[i];
                    ST[2*(II-1)+1] = T[i];
                } else if (gap_pene == DMIN[II-1]) {
                    if (std::max(std::abs(S[i]),       std::abs(T[i])) <
                        std::max(std::abs(ST[2*(II-1)]), std::abs(ST[2*(II-1)+1]))) {
                        IRTL[II-1] = CAND_E[i];
                        ST[2*(II-1)]   = S[i];
                        ST[2*(II-1)+1] = T[i];
                    }
                }
            }
        }
    } else if (IGNORE == 1) {
        for (int i = c_first; i <= c_last; ++i) {
            if (PENE[i] > ZERO &&
                S[i] < ONEP5 && T[i] < ONEP5 &&
                S[i] > -ONEP5 && T[i] > -ONEP5) {
                int II = CAND_N[i];
                my_real tz_pene = TZINF - PENE[i];
                if (tz_pene < DMIN[II-1]) {
                    DMIN[II-1] = tz_pene;
                    IRTL[II-1] = CAND_E[i];
                    ST[2*(II-1)]   = S[i];
                    ST[2*(II-1)+1] = T[i];
                } else if (tz_pene == DMIN[II-1]) {
                    if (std::max(std::abs(S[i]),       std::abs(T[i])) <
                        std::max(std::abs(ST[2*(II-1)]), std::abs(ST[2*(II-1)+1]))) {
                        IRTL[II-1] = CAND_E[i];
                        ST[2*(II-1)]   = S[i];
                        ST[2*(II-1)+1] = T[i];
                    }
                }
            }
        }
    } else {
        for (int i = c_first; i <= c_last; ++i) {
            if (PENE[i] > ZERO) {
                int II = CAND_N[i];
                my_real tz_pene = TZINF - PENE[i];
                if (tz_pene < DMIN[II-1]) {
                    DMIN[II-1] = tz_pene;
                    IRTL[II-1] = CAND_E[i];
                    ST[2*(II-1)]   = S[i];
                    ST[2*(II-1)+1] = T[i];
                } else if (tz_pene == DMIN[II-1]) {
                    if (std::max(std::abs(S[i]),       std::abs(T[i])) <
                        std::max(std::abs(ST[2*(II-1)]), std::abs(ST[2*(II-1)+1]))) {
                        IRTL[II-1] = CAND_E[i];
                        ST[2*(II-1)]   = S[i];
                        ST[2*(II-1)+1] = T[i];
                    }
                }
            }
        }
    }
}
