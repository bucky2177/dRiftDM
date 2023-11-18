#include <Rcpp.h>
#include <iostream>
using namespace Rcpp;

// This is a simple example of exporting a C++ function to R. You can
// source this function into an R session using the Rcpp::sourceCpp 
// function (or via the Source button on the editor toolbar). Learn
// more about Rcpp at:
//
//   http://www.rcpp.org/
//   http://adv-r.had.co.nz/Rcpp.html
//   http://gallery.rcpp.org/
//

// [[Rcpp::export]]
int cpp_kfe(NumericVector& pdf_u,
             NumericVector& pdf_l,
             NumericVector& xx,
             const int nt, const int nx,
             const double dt, const double dx,
             double sigma,
             const NumericVector b_vec,
             const NumericVector mu_vec,
             const NumericVector dt_b_vec) {
  if (pdf_u.size() != nt+1)
  { std::cerr << "pdf-upper has wrong size!" << std::endl; return -1;}
  if (pdf_l.size() != nt+1)
  { std::cerr << "pdf-lower has wrong size!" << std::endl; return -1; }
  if (b_vec.size() != nt+1)
  { std::cerr << "b_vec has wrong size!" << std::endl; return -1; }
  if (mu_vec.size() != nt+1)
  { std::cerr << "mu_vec has wrong size!" << std::endl; return -1; }
  if (dt_b_vec.size() != nt+1)
  { std::cerr << "dt_b_vec has wrong size!" << std::endl; return -1; }
  if (pdf_l.size() != nt+1)
  { std::cerr << "pdf-lower has wrong size!" << std::endl; return -1; }
  if (xx.size() != nx+1)
  { std::cerr << "xx has wrong size!" << std::endl; return -1; }

  NumericVector f     (nx+1,0.);  // storing the solution
  NumericVector mu_old(nx+1,0.);
  NumericVector mu_new(nx+1,0.);
  NumericVector x_vec(nx+1,0.);

  for (int i=0;i<nx+1;++i)
    x_vec[i] = -1.0 + 2.0*i/nx;
  
  for (int n = 1; n <= nt; ++n){
// Rannacher time-marching only required for Dirac initial and not really good?
    double theta = 0.5;
    if (n <= 4) {
      theta = 1.0;
    }
  
    // at old time
    const double J_old = b_vec[n-1];
    for (int i=0;i<nx+1;++i)
    {
//      mu_old <- rep(mu_old / J_old, nx + 1) - dt_b_old / J_old * x_vec
      mu_old[i] = (mu_vec[n-1] - dt_b_vec[n-1] * x_vec[i]) / J_old;
    }
    const double sigma_old = sigma / J_old;
    const double Lold = 1.0 / dx * sigma_old * sigma_old / 2.0;
    
    // at new time step
    const double J_new = b_vec[n];
    //mu_new <- rep(mu_new / J_new, nx + 1) - dt_b_new / J_new * x_vec
    for (int i=0;i<nx+1;++i)
      mu_new[i] = (mu_vec[n] - dt_b_vec[n] * x_vec[i]) / J_new;

    const double sigma_new = sigma / J_new;
    const double L_new = 1.0 / dx * sigma_new * sigma_new / 2.0;

    //    f <- 2.0 / 3.0 * dx * x_vals
    for (int i=0;i<nx+1;++i)
      f[i] = 2.0/3.0 * dx * xx[i];
    
//    f[2:nx] <- f[2:nx] + 1.0 / 6.0 * dx * x_vals[1:(nx - 1)]
//    f[2:nx] <- f[2:nx] + 1.0 / 6.0 * dx * x_vals[3:(nx + 1)]
    for (int i=1;i<nx;++i)
      f[i] += 1.0/6.0 * dx * (xx[i-1]+xx[i+1])
              + (theta-1.0) * dt * ( (-Lold - 0.5 * mu_old[i-1]) * xx[i-1]
                                    + 2.0 * Lold * xx[i]
                                    +(-Lold + 0.5 * mu_old[i+1]) * xx[i+1] );

  //  f[2:nx] <- f[2:nx] + (theta - 1) * dt *
//      (-L_old - 0.5 * mu_old[1:(nx - 1)]) * x_vals[1:(nx - 1)]
//    f[2:nx] <- f[2:nx] + (theta - 1) * dt * (2.0 * L_old) * x_vals[2:nx]
//    f[2:nx] <- f[2:nx] + (theta - 1) * dt *
//      (-L_old + 0.5 * mu_old[3:(nx + 1)]) * x_vals[3:(nx + 1)]
    
    if ( (f[0]!=0) || (f[nx]!=0) )
    {std::cerr << "rhs not zero on thresholds!" << std::endl; return -1; }

    NumericVector a(nx+1,0.), b(nx+1,0.), c(nx+1,0.);
    for (int i=0;i<nx+1;++i)
    {
      a[i] = 1./6. * dx + dt * theta * (-L_new - 0.5 * mu_new[i]);
      b[i] = 2./3. * dx + dt * theta * (2.0 * L_new);
      c[i] = 1./6. * dx + dt * theta * (-L_new + 0.5 * mu_new[i]);
    }
    // adapt for boundary
    a[0]  = 0.0; b[0]  = 1.0; c[0]  = 0.0;
    a[nx] = 0.0; b[nx] = 1.0; c[nx] = 0.0;
    // solve 
    c[0] /= b[0];
    for (int i=1;i<nx; ++i)
      c[i] /= (b[i]-c[i-1]*a[i]);
    f[0] /= b[0];
    for (int i=1;i<=nx;++i)
      f[i] = (f[i]-f[i-1]*a[i])/(b[i]-c[i-1]*a[i]);
    xx[nx]=f[nx];
    for (int i=nx-1;i>=0; --i)
      xx[i] = f[i]-c[i]*xx[i+1];
    
    // solve tri-diagonal system with matrix 
    // (a,b,c) and rhs f[i]
    //  a = 1.0 / 6.0 * dx + dt * theta * (-L_new - 0.5 * mu_new),
    //  b = 2.0 / 3.0 * dx + dt * theta * (2.0 * L_new),
    //  c = 1.0 / 6.0 * dx + dt * theta * (-L_new + 0.5 * mu_new)
    

//      x_vals <- tridiag(
//          f,
//          1.0 / 6.0 * dx + dt * theta * (-L_new - 0.5 * mu_new),
//          2.0 / 3.0 * dx + dt * theta * (2.0 * L_new),
//          1.0 / 6.0 * dx + dt * theta * (-L_new + 0.5 * mu_new)
//      )
      
      pdf_u[n] = 0.5 * sigma_new*sigma_new/ dx / dx *
        (3.0 * xx[nx-1] - 1.5 * xx[nx-2] + 1.0 / 3.0 * xx[nx -3]);
      pdf_l[n] = 0.5 * sigma_new*sigma_new / dx / dx * 
        (3.0 * xx[1] - 1.5 * xx[2] + 1.0 / 3.0 * xx[3]);

  }
  return 1;
  
  
}


// You can include R code blocks in C++ files processed with sourceCpp
// (useful for testing and development). The R code will be automatically 
// run after the compilation.
//

