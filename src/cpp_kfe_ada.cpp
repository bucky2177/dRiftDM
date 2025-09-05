#include <Rcpp.h>
#include <iostream>
#include <fstream>
using namespace Rcpp;

// this function is a C++ version of the kfe function provided by
// Richter, Ulrich, Janczyk (2023) Journal of Mathematical Psychology
// [[Rcpp::export]]
int cpp_kfe_ada(NumericVector& pdf_u,
            NumericVector& pdf_l,
            NumericVector& xx,
            const int nt,
            const int nx,
            const double dtbase,
            const double dx,
            const double sigma,
            const NumericVector& b_vals,
            const NumericVector& mu_vals,
            const NumericVector& dt_b_vals,
            const NumericVector& x_vec) {

  if (pdf_u.size() != nt+1){
    stop("pdf-upper has wrong size!");
  }
  if (pdf_l.size() != nt+1) {
    stop("pdf-lower has wrong size!");
  }
  if (b_vals.size() != nt+1) {
    stop("b_vals has wrong size!");
  }
  if (mu_vals.size() != nt+1) {
    stop("mu_vals has wrong size!");
  }
  if (dt_b_vals.size() != nt+1) {
    stop("dt_b_vals has wrong size!");
  }
  if (xx.size() != nx+1) {
    stop("xx has wrong size!");
  }
  if (x_vec.size() != nx+1) {
    stop("x_vec has wrong size!");
  }

  const double tol = 1.e-8;

  NumericVector f(nx+1, 0.);  // storing the solution
  NumericVector mu_old(nx+1, 0.);
  NumericVector mu_new(nx+1, 0.);
  NumericVector a(nx+1, 0.), b(nx+1, 0.), c(nx+1, 0.), xxnew(nx+1,0.);

  double acc = 0;
  int nsteps = 0;
  double tt = 0.0;

  for (int n=1; n<=nt; ++n) {
    // at old time step
    const double J_old = b_vals[n-1];
    const double J_new = b_vals[n];
    const double sigma_old = sigma / J_old;
    const double Lold = 1.0 / dx * sigma_old * sigma_old / 2.0;
    const double sigma_new = sigma / J_new;
    const double L_new = 1.0 / dx * sigma_new * sigma_new / 2.0;

    for (int i=0; i<nx+1; ++i)
      mu_old[i] = (mu_vals[n-1] - dt_b_vals[n-1] * x_vec[i]) / J_old;
    // at new time step
    for (int i=0; i<nx+1; ++i)
      mu_new[i] = (mu_vals[n] - dt_b_vals[n] * x_vec[i]) / J_new;

    // for time-stepping, the actual values will be interpolated between new and old
    tt = (n-1)*dtbase;
    double dt = dtbase;
    double time = 0;

    while (time < dtbase-1.e-8) // iterate until we complete the time step
    {
      f[0] = 0.0;
      f[nx] = 0.0;

      for (int i=1;i<nx;++i)
        f[i] = dx * xx[i] - dt * 0.5 *
          ( (     -Lold - .5 * mu_old[i]) * xx[i-1]
          + (+2. * Lold                 ) * xx[i]
          + (     -Lold + .5 * mu_old[i]) * xx[i+1] );

      for (int i=0; i<nx+1; ++i) {
        a[i] =      dt * .5 * (     -L_new - .5 * mu_new[i]);
        b[i] = dx + dt * .5 * (2.0 * L_new);
        c[i] =      dt * .5 * (     -L_new + .5 * mu_new[i]);
      }
      if ( (f[0]!=0) || (f[nx]!=0) ) {
        stop("rhs not zero on thresholds!");
      }
      a[0]  = 0.0; b[0]  = 1.0; c[0]  = 0.0;
      a[nx] = 0.0; b[nx] = 1.0; c[nx] = 0.0;
      // solve
      c[0] /= b[0];
      for (int i=1;i<nx; ++i)
        c[i] /= (b[i]-c[i-1]*a[i]);
      f[0] /= b[0];
      for (int i=1;i<=nx;++i)
        f[i] = (f[i]-f[i-1]*a[i])/(b[i]-c[i-1]*a[i]);

      bool stepok = true;
      xxnew[nx] = f[nx];
      for (int i=nx-1; i>=0; --i)
      {
        xxnew[i] = f[i]-c[i]*xxnew[i+1];
        if (xxnew[i] < - tol)
        {
          stepok = false;
          break;
        }
      }
      if (stepok)
      {
        for (int i=0;i<=nx;++i)
          xx[i] = xxnew[i];
//          std::ofstream OUT("x.out",std::ios::app);
//          OUT << tt << "\t" << dt << std::endl;
//          OUT.close();
          time += dt;
          tt += dt;
          dt = std::min(dtbase,1.8 * dt);    // increase timestep
          dt = std::min(dt, dtbase - time); // not larger than step
      }
      else // repeat step
        dt = 0.125 * dt;

      ++nsteps;
    }

    pdf_u[n] = 0.5 * sigma_new*sigma_new/ dx / dx *
        (3.0 * xx[nx-1] - 1.5 * xx[nx-2] + 1.0 / 3.0 * xx[nx -3]);
    pdf_l[n] = 0.5 * sigma_new*sigma_new / dx / dx *
        (3.0 * xx[1] - 1.5 * xx[2] + 1.0 / 3.0 * xx[3]);

    acc += pdf_u[n] + pdf_l[n];
    if (acc * dx * dtbase > 0.999)
      break;
  }

  return 1;
}
