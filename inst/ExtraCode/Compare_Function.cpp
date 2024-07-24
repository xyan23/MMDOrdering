#include <Rcpp.h>
#include <string>

using namespace Rcpp;

//[[Rcpp::export]]
bool x_coordinate(NumericVector p1, NumericVector p2) {
    bool comp_criterion = p1[0] > p2[0];
    if (comp_criterion)
        return true;
    else
        return false;
}

//[[Rcpp::export]]
bool y_coordinate(NumericVector p1, NumericVector p2) {
    bool comp_criterion = p1[1] > p2[1];
    if (comp_criterion)
        return true;
    else
        return false;
}

//[[Rcpp::export]]
void comp_fun(std::string ordering_method, NumericVector p1, NumericVector p2) {
    if (ordering_method == "x_coordinate")
        return x_coordinate(NumericVector p1, NumericVector p2);
    else if (ordering_method == "y_coordinate")
        return y_coordinate(NumericVector p1, NumericVector p2);
    else if (ordering_method == "mmd")
        return mmd(NumericVector p1, NumericVector p2);
    else 
        stop("Choose from x_coordinate, y_coordinate, or mmd.");
}
