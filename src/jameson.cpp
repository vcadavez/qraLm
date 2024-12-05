// Copyright Timothy H. Keitt 2015
// See license for odeintr package



#include <Rcpp.h>
// [[Rcpp::plugins(cpp11)]]

// [[Rcpp::depends(BH)]]

#include "boost/numeric/odeint.hpp"
namespace odeint = boost::numeric::odeint;

;

namespace odeintr
{
  static const std::size_t N = 4;

  typedef std::vector<double> state_type;
  
  static state_type state(N);
  
  typedef odeint::runge_kutta_dopri5<state_type> stepper_type;
  
  static auto stepper = odeint::make_dense_output(1e-06, 1e-06, stepper_type());
  
  typedef std::vector<double> vec_type;
  static std::vector<vec_type> rec_x(N);
  static vec_type rec_t;
  
  double mumax1 = 0.14, mumax2 = 0.3, ymax1 = 1e+08, ymax2 = 1e+07, gamma = 1;;
  
  //#include <Rcpp/utils.h>
  
  static void
  sys(const state_type x, state_type &dxdt, const double t)
  {
    

      /*       Q1:x[0]  dQ1<-mumax1*Q1
      Q2:x[1]  dQ2<-mumax2*Q2
      y1:x[2]  dy1<-(Q1/(1+Q1))*mumax1*(1-(y1/ymax1))*(1-(gamma*y2/ymax2))*y1
      y2:x[3]  dy2<-(Q2/(1+Q2))*mumax2*(1-(y1/ymax1))*(1-(y2/ymax2))*y2
      */
      dxdt[0] = mumax1 * x[0];
      dxdt[1] = mumax2 * x[1];
      dxdt[2] = (x[0]/(1+x[0])) * mumax1 * (1-(x[2]/ymax1)) * (1-(gamma*x[3]/ymax2))*x[2];
      dxdt[3] = (x[1]/(1+x[1])) * mumax2 * (1-(x[2]/ymax1)) * (1-(x[3]/ymax2))*x[3];
;
  }

  static void
  obs(const state_type x, const double t)
  {
    for (int i = 0; i != N; ++i)
      rec_x[i].push_back(x[i]);
    rec_t.push_back(t);
  }
  
}; // namespace odeintr

static void
reserve(odeintr::vec_type::size_type n)
{
  odeintr::rec_t.reserve(n);
  for (auto &i : odeintr::rec_x) i.reserve(n);
}

// [[Rcpp::export]]
Rcpp::List expandedjamesonC_get_output()
{
  Rcpp::List out;
  out("Time") = Rcpp::wrap(odeintr::rec_t);
  for (int i = 0; i != odeintr::N; ++i)
  {
    auto cnam = std::string("X") + std::to_string(i + 1);
    out(cnam) = Rcpp::wrap(odeintr::rec_x[i]);
  }
  out.attr("class") = "data.frame";
  int rows_out = odeintr::rec_t.size();
  auto rn = Rcpp::IntegerVector::create(NA_INTEGER, -rows_out);
  out.attr("row.names") = rn;
  return out;
};

// [[Rcpp::export]]
void expandedjamesonC_set_state(Rcpp::NumericVector new_state)
{
  if (new_state.size() != odeintr::N)
    Rcpp::stop("Invalid initial state");
  std::copy(new_state.begin(),
            new_state.end(),
            odeintr::state.begin());
}

// [[Rcpp::export]]
std::vector<double>
expandedjamesonC_get_state()
{
  return odeintr::state;
}

// [[Rcpp::export]]
void expandedjamesonC_reset_observer()
{
  for (auto &i : odeintr::rec_x) i.resize(0);
  odeintr::rec_t.resize(0);  
}

// [[Rcpp::export]]
Rcpp::List expandedjamesonC_adap(Rcpp::NumericVector init,
                             double duration,
                             double step_size = 1.0,
                             double start = 0.0)
{
  expandedjamesonC_set_state(init);
  expandedjamesonC_reset_observer(); reserve(duration / step_size);
  odeint::integrate_adaptive(odeintr::stepper, odeintr::sys, odeintr::state,
                             start, start + duration, step_size,
                             odeintr::obs);
  return expandedjamesonC_get_output();
}

// [[Rcpp::export]]
Rcpp::List expandedjamesonC_at(Rcpp::NumericVector init,
                           std::vector<double> times,
                           double step_size = 1.0,
                           double start = 0.0)
{
  expandedjamesonC_set_state(init);
  expandedjamesonC_reset_observer(); reserve(times.size());
  odeint::integrate_const(odeintr::stepper, odeintr::sys, odeintr::state,
                          start, times[0], step_size);
  odeint::integrate_times(odeintr::stepper, odeintr::sys, odeintr::state,
                          times.begin(), times.end(), step_size, odeintr::obs);
  return expandedjamesonC_get_output();
}

// [[Rcpp::export]]
Rcpp::List
expandedjamesonC_continue_at(std::vector<double> times, double step_size = 1.0)
{
  double start = odeintr::rec_t.back();
  expandedjamesonC_reset_observer(); reserve(odeintr::rec_t.size() + times.size());
  odeint::integrate_const(odeintr::stepper, odeintr::sys, odeintr::state,
                          start, times[0], step_size);
  odeint::integrate_times(odeintr::stepper, odeintr::sys, odeintr::state,
                          times.begin(), times.end(), step_size, odeintr::obs);
  return expandedjamesonC_get_output();
}

//' Integrate the growth model with Jameson effect
//' 
//' This function integrate the Jameson model through ODE. See \link{sfGrowthJameson}.
//'
//' @param init initial values
//' @param duration the end of the integration
//' @param step_size the step size of the integration
//' @param start start of the integration
//' @export
// [[Rcpp::export]]
Rcpp::List expandedjamesonC(Rcpp::NumericVector init,
                       double duration,
                       double step_size = 1.0,
                       double start = 0.0)
{
  expandedjamesonC_set_state(init);
  expandedjamesonC_reset_observer(); reserve(duration / step_size);
  odeint::integrate_const(odeintr::stepper, odeintr::sys, odeintr::state,
                          start, start + duration, step_size,
                          odeintr::obs);
  return expandedjamesonC_get_output();
}

// [[Rcpp::export]]
std::vector<double>
expandedjamesonC_no_record(Rcpp::NumericVector init,
                       double duration,
                       double step_size = 1.0,
                       double start = 0.0)
{
  expandedjamesonC_set_state(init);
  odeint::integrate_adaptive(odeintr::stepper, odeintr::sys, odeintr::state,
                             start, start + duration, step_size);
  return expandedjamesonC_get_state();
}

//' Set the parameters to integrate the growth model with Jameson effect
//' 
//' This function set the parameters to integrate the Jameson model through ODE. See \link{sfGrowthJameson}.
//'
//' @param mumax1 mumax (/h) of the first population
//' @param mumax2 mumax (/h) of the second population
//' @param ymax1 MPD (CFU) of the first population
//' @param ymax2 MPD (CFU) of the second population
//' @param gamma interaction
//' @export
// [[Rcpp::export]]
void expandedjamesonC_set_params(double mumax1 = 0.14, double mumax2 = 0.3, double ymax1 = 1e+08, double ymax2 = 1e+07, double gamma = 1)
{ 
  odeintr::mumax1 = mumax1;
odeintr::mumax2 = mumax2;
odeintr::ymax1 = ymax1;
odeintr::ymax2 = ymax2;
odeintr::gamma = gamma;
}
// [[Rcpp::export]]
Rcpp::List expandedjamesonC_get_params()
{
  Rcpp::List out;
  out["mumax1"] = odeintr::mumax1;
out["mumax2"] = odeintr::mumax2;
out["ymax1"] = odeintr::ymax1;
out["ymax2"] = odeintr::ymax2;
out["gamma"] = odeintr::gamma;
  return out;
}
;



