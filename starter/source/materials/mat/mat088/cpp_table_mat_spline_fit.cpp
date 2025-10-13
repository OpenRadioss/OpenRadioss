#include <iostream>
#include <fstream>
#include <vector>
#include <cmath>
#include <iomanip>
#include <limits>
#include <algorithm>

using namespace std;

// --- Isotone regression by the Pool Adjacent Violators (PAV) algorithm
void isotone_project_pav(const std::vector<double>& y,
                         const std::vector<double>& w,
                         std::vector<double>& z)
{
    const int n = (int)y.size();
    z = y;
    std::vector<double> sum_w(n), sum_y(n);
    std::vector<int>    blkL(n), blkR(n);
    for (int i=0;i<n;++i){ sum_w[i]=w[i]; sum_y[i]=w[i]*y[i]; blkL[i]=blkR[i]=i; }

    int k = 0;
    for (int i=0;i<n;++i){
        k = i;
        while (k>0){
            int l1=blkL[k-1], r1=blkR[k-1];
            int l2=blkL[k  ], r2=blkR[k  ];
            double m1 = sum_y[k-1]/sum_w[k-1];
            double m2 = sum_y[k  ]/sum_w[k  ];
            if (m1 <= m2) break;
            sum_w[k-1] += sum_w[k];
            sum_y[k-1] += sum_y[k];
            blkR[k-1]   = r2;
            for (int j=k; j<i; ++j){
                sum_w[j] = sum_w[j+1];
                sum_y[j] = sum_y[j+1];
                blkL[j]  = blkL[j+1];
                blkR[j]  = blkR[j+1];
            }
            --i; --k;
        }
    }
    int idx = 0;
    for (int b=0; idx<n; ++b){
        double m = sum_y[b]/sum_w[b];
        for (int j=blkL[b]; j<=blkR[b]; ++j) z[idx++] = m;
    }
}

// --- Isotone smoothing (one wheel mu). Fix y(0)=0 if one x equals 0.
void smooth_isotone(const std::vector<double>& x,
                    const std::vector<double>& y_in,
                    std::vector<double>& z,
                    double mu = 1e-2,
                    int maxit = 500, double tol = 1e-9)
{
    const int n = (int)x.size();
    std::vector<double> w(n,1.0);
    z = y_in;

    int i0 = -1; for (int i=0;i<n;++i) if (std::abs(x[i])<1e-15) { i0=i; break; }
    if (i0>=0) z[i0]=0.0;

    const double eta = 1.0 / (1.0 + 4.0*mu);
    std::vector<double> g(n), zprev(n);

    auto add_Lap1D = [&](const std::vector<double>& v, std::vector<double>& out){
        out[0]     += mu * ( v[0] - v[1] );
        for (int i=1;i<n-1;++i) out[i] += mu * (2*v[i] - v[i-1] - v[i+1]);
        out[n-1]   += mu * ( v[n-1] - v[n-2] );
    };

    for (int it=0; it<maxit; ++it){
        zprev = z;
        g.assign(n, 0.0);
        for (int i=0;i<n;++i) g[i] = (z[i]-y_in[i]); 
        add_Lap1D(z, g);

        for (int i=0;i<n;++i) z[i] -= eta * g[i];

        if (i0>=0) z[i0] = 0.0;

        isotone_project_pav(z, w, z);

        double diff=0.0;
        for (int i=0;i<n;++i){ double d=z[i]-zprev[i]; diff+=d*d; }
        if (diff < tol) break;
    }
}

// --- PCHIP (Fritsch–Carlson) slopes
void pchip_slopes(const std::vector<double>& x,
                  const std::vector<double>& z,
                  std::vector<double>& m)
{
    const int n = (int)x.size();
    m.assign(n, 0.0);
    if (n==1){ m[0]=0.0; return; }
    std::vector<double> h(n-1), d(n-1);
    for (int i=0;i<n-1;++i){
        h[i] = x[i+1]-x[i];
        d[i] = (z[i+1]-z[i])/h[i];
    }
    m[0] = d[0];
    m[n-1] = d[n-2];
    for (int i=1;i<n-1;++i){
        if ( (d[i-1]*d[i]) <= 0.0 ) { m[i]=0.0; }
        else {
            double w1 = 2.0*h[i] + h[i-1];
            double w2 = h[i] + 2.0*h[i-1];
            m[i] = (w1 + w2) / ( (w1/d[i-1]) + (w2/d[i]) );
        }
    }
}

// --- PCHIP evaluation
double pchip_eval(const std::vector<double>& x,
                  const std::vector<double>& z,
                  const std::vector<double>& m,
                  double xi)
{
    const int n = (int)x.size();
    if (xi <= x.front()) return z.front();
    if (xi >= x.back())  return z.back();
    int k = (int)(std::upper_bound(x.begin(), x.end(), xi) - x.begin()) - 1; // k in [0..n-2]
    double h = x[k+1]-x[k];
    double t = (xi - x[k]) / h;
    double t2 = t*t, t3 = t2*t;

    double h00 = ( 2*t3 - 3*t2 + 1);
    double h10 = (   t3 - 2*t2 + t)*h;
    double h01 = (-2*t3 + 3*t2     );
    double h11 = (   t3 -   t2    )*h;

    return h00*z[k] + h10*m[k] + h01*z[k+1] + h11*m[k+1];
}


//----------------------------------------------------------------------------------------------------------------
// C Interface to Fortran
// ---------------------------------------------------------------------------------------------------------------
extern "C" {
    void cpp_table_mat_spline_fit(int s_inp, double* x_inp,double* y_inp,int nout, double* x_out, 
                                  double* y_out, double lambda) {

        std::vector<double> x_raw(s_inp), y_raw(s_inp);
        for (int i=0;i<s_inp;++i){ x_raw[i]=x_inp[i]; y_raw[i]=y_inp[i]; }
       
        const int n = (int)x_raw.size();
        if (n==0){
            x_out[0]=0.0; y_out[0]=0.0; return;
        }
        if (n==1){
            for (int i=0;i<nout;++i){ x_out[i]=x_raw[0]; y_out[i]=y_raw[0]; }
            return;
        }
    
        std::vector<double> z;
        smooth_isotone(x_raw, y_raw, z, /*mu=*/std::max(0.0,lambda));
    
        std::vector<double> m;
        pchip_slopes(x_raw, z, m);
    
        double xmin = x_raw.front(), xmax = x_raw.back();
        if (nout<2) nout=2;

        for (int i=0;i<nout;++i){
            double xi = xmin + (xmax - xmin) * (double)i / (double)(nout-1);
            x_out[i] = xi;
            y_out[i] = pchip_eval(x_raw, z, m, xi);
        }
    
    }
}
