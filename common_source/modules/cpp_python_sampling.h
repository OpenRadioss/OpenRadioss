#include <iostream>
#include <vector>
#include <cmath>      // For isnan, isinf, fabs, exp
#include <algorithm>  // For std::sort
#include <limits>
#include <iomanip> // For setprecision
#include <set>
#include <utility> // For std::pair
#include <numeric>

// Function to generate a linearly spaced vector
std::vector<double> linear_spacing(double start, double end, size_t n, bool include_start) {
    std::vector<double> points(n);

    for (size_t i = 0; i < n; ++i) {
        double factor = include_start ? i : (i + 1);
        points[i] = start + factor * (end - start) / (n - 1);
    }
    return points;
}

// Generate sampling with 5N points and avoid duplicate boundary values
std::vector<double> initial_sampling(double x_max, size_t n) {
    // Define the ranges
    n = n / 5;
    std::vector<double> range_starts = {0, 1, 10, 100, 1000};
    std::vector<double> range_ends = {1, 10, 100, 1000, 10000};

    std::vector<double> X;
    X.reserve(5 * n); // Reserve space for efficiency
    // Generate points for each range
    for (size_t i = 0; i < range_starts.size(); ++i) {
        bool include_start = (i == 0); // Only include start for the first range
        std::vector<double> range_points = linear_spacing(
            range_starts[i], range_ends[i], n, include_start
        );
        X.insert(X.end(), range_points.begin(), range_points.end());
    }
    // scale the points to 0 xmax
    double x_min_right = 0;
//    for (size_t i = 0; i < X.size(); i++) {
//        X[i] = x_min_right + X[i] * (x_max - x_min_right) / 10000;
//    }
    X[0] = (X[0] + X[1]) / 2.0;
    return X;
}




double safe_cast_to_double(long double d) {
    // Handle NaN cases
    if (std::isnan(d)) {
        return 0.0;
    }

    // Clamp to the maximum representable double if too large
    if (d > std::numeric_limits<double>::max()) {
        return std::numeric_limits<double>::max();
    }

    // Clamp to the minimum representable double (negative) if too small
    if (d < -std::numeric_limits<double>::max()) {
        return -std::numeric_limits<double>::max();
    }

    // Otherwise, cast safely
    return static_cast<double>(d);
}

// remove non-finite values
void clean_values(std::vector<double>& X, std::vector<double>& Y) {
    // initial size
    size_t N = X.size();
    // Remove non-finite values
    std::vector<double> XX;
    std::vector<double> YY;
    for (size_t i = 0; i < X.size(); i++) 
    {
        if (std::isfinite(Y[i])) 
        {
            if(i > 0) 
            {
                if (fabs(Y[i] - Y[i-1]) > 1e-10)
                {
                    XX.push_back(X[i]);
                    YY.push_back(Y[i]);
                }
            } else {
                XX.push_back(X[i]);
                YY.push_back(Y[i]);
            }
        }
    }
    if(XX.size() == 0) {
        return;
    }
    X = XX;
    Y = YY;
    const size_t  new_size = X.size();
    double Xlast = X[new_size - 1];
    double Ylast = Y[new_size - 1];
    for(size_t i = new_size ; i < N; i++){
        X.push_back(Xlast + 1.0);
        Y.push_back(Ylast);
    }
}

// Function to compute curvature robustly
long double compute_curvature(const double &  F_im1, const double & F_i, const double &F_ip1,const double & h1, const double & h2) {
    // Cast inputs to long double for higher precision
    long double F_im1_ld = static_cast<long double>(F_im1);
    long double F_i_ld = static_cast<long double>(F_i);
    long double F_ip1_ld = static_cast<long double>(F_ip1);
    long double h1_ld = static_cast<long double>(h1);
    long double h2_ld = static_cast<long double>(h2);

    // First derivative using non-uniform spacing
    long double F_prime = (h2_ld * (F_i_ld - F_im1_ld) + h1_ld * (F_ip1_ld - F_i_ld)) / (h1_ld * h2_ld);

    // Second derivative using non-uniform spacing
    long double F_double_prime = 2 * ((F_ip1_ld - F_i_ld) / h2_ld - (F_i_ld - F_im1_ld) / h1_ld) / (h1_ld + h2_ld);

    // Curvature formula
    long double denom = std::pow(1 + F_prime * F_prime, 1.5L);
    if (denom == 0.0L) return 0.0;  // Avoid division by zero
    long double curvature = std::abs(F_double_prime) / denom;
    // Cast the result back to double
    // if NaN or Inf return 0.0
    if (std::isnan(curvature) || std::isinf(curvature)) {
        return 0.0L;
    }
    return curvature;
}

// Select n_points based on curvature
std::vector<double> select_points(const std::vector<double>& x, const std::vector<double>& F_values, size_t n_points) {
    size_t n = x.size();
    if (n_points >= n) {
        n_points = n;
        return x;
    }

    // Step 1: Compute curvature for all points
    std::vector<long double> curvature(n, 0.0);
    for (size_t i = 1; i < n - 1; ++i) {
        double h1 = x[i] - x[i - 1];
        double h2 = x[i + 1] - x[i];
        if (h1 == 0.0 || h2 == 0.0) {
            curvature[i] = 0.0;
            continue;
        }
        //if F_values[i] is not finite, curvature is 0
        if (!std::isfinite(F_values[i]) || !std::isfinite(F_values[i - 1]) || !std::isfinite(F_values[i + 1])) {
            curvature[i] = 0.0;
            continue;
        }
        curvature[i] = compute_curvature(F_values[i - 1], F_values[i], F_values[i + 1], h1, h2);
    }

    // Step 2: Normalize curvature to create a density function
    long double curvature_sum = std::accumulate(curvature.begin(), curvature.end(), 0.0);
    std::vector<long double> density(n, 0.0L);

    density[0] = sqrt(curvature[0] * (2*(x[1] - x[0])));  // Use sqrt(curvature)
    density[n-1] = sqrt(curvature[n-1]  * (2*(x[n-1] - x[n-2])));  // Use sqrt(curvature)
    for (size_t i = 0; i < n-1; ++i) {
        density[i] = std::sqrt(curvature[i] * (x[i+1] - x[i]));  // Use sqrt(curvature)
    }
    // get the maximum desinty
    long double max_density = *std::max_element(density.begin(), density.end());
    // density = density + 0.001*max_density
    for (size_t i = 0; i < n; ++i) {
        density[i] += 0.001L * max_density;
    }
    long double density_sum = std::accumulate(density.begin(), density.end(), 0.0);
    for (auto& d : density) {
        d /= density_sum;  // Normalize
    }

    // Step 3: Compute cumulative density function (CDF)
    std::vector<long double> cdf(n, 0.0L);
    std::partial_sum(density.begin(), density.end(), cdf.begin());

    // Step 4: Select n_points based on the CDF
    std::vector<double> selected_points;
    selected_points.reserve(n_points);
    double step = 1.0 / (n_points - 1);
    double target = 0.0;

    for (size_t i = 0; i < n_points; ++i) {
        auto it = std::lower_bound(cdf.begin(), cdf.end(), target);
        size_t idx = std::distance(cdf.begin(), it);
        selected_points.push_back(x[std::min(idx, n - 1)]);  // Ensure idx is within bounds
        target += step;
    }

    // Ensure boundary points are included
    if (std::find(selected_points.begin(), selected_points.end(), x.front()) == selected_points.end()) {
        selected_points[0] = x.front();
    }
    if (std::find(selected_points.begin(), selected_points.end(), x.back()) == selected_points.end()) {
        selected_points[n_points - 1] = x.back();
    }

    return selected_points;
}