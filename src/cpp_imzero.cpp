#include <Rcpp.h>
#include <iostream>
using namespace Rcpp;


inline double ff(const double t, const double t0,
          const double b1, const double b2,
          const double mu_int1, const double mu_int2,
          const double sqrt_sigma, const double sigma_sq,
          const double dt_b, const double mu)
{
  const double tmp  = b1-b2-mu_int1+mu_int2;
  const double ttmp = t-t0;
  const double multipl = 1./sqrt_sigma;
  const double expv = exp(-tmp*tmp/(sigma_sq*ttmp));
  const double denom = (dt_b-mu-tmp/ttmp);
  return multipl * expv * 0.5 * denom;
}

inline double psi(const double b, const double t, const double dt_b,const  double mu,const  double mu_int, const double mu_int0, const double sqrt_sigma)
{
  const double n1 = 1.0/sqrt_sigma * exp( - pow(b-mu_int+mu_int0,2.0) / (sqrt_sigma*sqrt_sigma/M_PI));
  const double n2 = dt_b - mu - (b-mu_int+mu_int0)/t;
  return 0.5*n1*n2;
}

// this function is a C++ version of the kfe function provided by
// Richter, Ulrich, Janczyk (2023) Journal of Mathematica Modeling
// [[Rcpp::export]]
int cpp_imzero(NumericVector& pdf_u,
               NumericVector& pdf_l,
               const int nt,
               const double dt,
               const double sigma,
               const NumericVector& b_vals,
               const NumericVector& mu_vals,
               const NumericVector& mu_int_vals,
               const NumericVector& dt_b_vals,
               const NumericVector& t_vec) {

  if (pdf_u.size() != nt+1){
    Rcerr << "pdf-upper has wrong size!" << std::endl; return -1;
  }
  if (pdf_l.size() != nt+1) {
    Rcerr << "pdf-lower has wrong size!" << std::endl; return -1;
  }
  if (b_vals.size() != nt+1) {
    Rcerr << "b_vals has wrong size!" << std::endl; return -1;
  }
  if (mu_vals.size() != nt+1) {
    Rcerr << "mu_vals has wrong size!" << std::endl; return -1;
  }
  if (mu_int_vals.size() != nt+1) {
    Rcerr << "mu_vals has wrong size!" << std::endl; return -1;
  }
  if (dt_b_vals.size() != nt+1) {
    Rcerr << "dt_b_vals has wrong size!" << std::endl; return -1;
  }
  if (t_vec.size() != nt+1) {
    Rcerr << "x_vec has wrong size!" << std::endl; return -1;
  }
  NumericVector sqrt_sigma_vals(nt+1);
  for (int n=0;n<=nt;++n)
    sqrt_sigma_vals[n] = sqrt(2.0 * M_PI* sigma*sigma * t_vec[n]);
  for (int ti=1;ti<=nt;++ti)
  {
    pdf_u[ti] = -2.0*psi( b_vals[ti],t_vec[ti], dt_b_vals[ti],mu_vals[ti],mu_int_vals[ti],mu_int_vals[0],sqrt_sigma_vals[ti]);
    pdf_l[ti] =  2.0*psi(-b_vals[ti],t_vec[ti],-dt_b_vals[ti],mu_vals[ti],mu_int_vals[ti],mu_int_vals[0],sqrt_sigma_vals[ti]);

    for (int j=1;j<ti;++j)
    {
      const double F11 = ff(t_vec[ti], t_vec[j], b_vals[ti], b_vals[j],mu_int_vals[ti], mu_int_vals[j],sqrt_sigma_vals[ti-j],2*sigma*sigma, dt_b_vals[ti],mu_vals[ti]);
      const double F12 = ff(t_vec[ti], t_vec[j], b_vals[ti],-b_vals[j],mu_int_vals[ti], mu_int_vals[j],sqrt_sigma_vals[ti-j],2*sigma*sigma, dt_b_vals[ti],mu_vals[ti]);
      const double F21 = ff(t_vec[ti], t_vec[j],-b_vals[ti], b_vals[j],mu_int_vals[ti], mu_int_vals[j],sqrt_sigma_vals[ti-j],2*sigma*sigma,-dt_b_vals[ti],mu_vals[ti]);
      const double F22 = ff(t_vec[ti], t_vec[j],-b_vals[ti],-b_vals[j],mu_int_vals[ti], mu_int_vals[j],sqrt_sigma_vals[ti-j],2*sigma*sigma,-dt_b_vals[ti],mu_vals[ti]);

      pdf_u[ti] +=  2.0 * dt * (pdf_u[j]*F11 + pdf_l[j]*F12);
      pdf_l[ti] += -2.0 * dt * (pdf_u[j]*F21 + pdf_l[j]*F22);
    }
  }
  return 1;
}
