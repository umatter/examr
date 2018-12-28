##' Grade Exams (CH Scale)
##'
##' Grades exam results based on total points and some fix parameters following the CH grade scale (1-6)
##' @usage grade_results_ch(exams_corrected)
##' @param exams_corrected character, a data frame with the total points per student (output of correct_scan_results)
##' @param c numeric, discount multiplier for grades under 4
##' @param quant numeric, quantile defining the threshold for the top grade (6)
##' @param ratio64 numeric, ratio of minimum no. of points for a 6 and minimum points for a 4
##' @param round numeric, to what grade fractions should the grades be rounded (either .1, .5, or NULL, defaults to NULL). if NULL, grades won't be rounded
##' @return a data.frame with the grade per student
##' @details grade= a + b*points, if fail : grade =  4 - c (points(4) - points)
##' @author Ulrich Matter <umatter@protonmail.com>
##' @examples
##' myfile <- system.file("inst/exdata/example_scan_results.csv", package="examr")
##' solutions <- system.file("inst/exdata/example_sample_solutions.csv", package="examr")
##' my_results <- correct_scan_results(myfile, solutions)
##' grades <- grade_results(my_results)
##'
##' @export
##' @import plyr



grade_results_ch <-
     function(exams_corrected, c=0.18, quant=0.93, ratio64=2.68, round=.5 ) {

          # internal functions
          grade_u4 <-
               function(p4, points, c) {
                    grades <-  4 - c*(p4 - points)
                    return(grades)
               }

          # select total points per student for further processing
          totals <- exams_corrected$total_points # see correct_scan_results()

          # parameters for grading (critical quantiles)
          dec_up <- quantile(totals, probs=quant)
          quantile_up <- names(dec_up)
          minp_6 <- unname(dec_up)
          p_4 <- round_any(minp_6/ratio64,.5)

          # formula parameters
          a <- ((4*minp_6) - (6*p_4))/(minp_6 - p_4)
          b <- 2/(minp_6 - p_4)

          # all grades
          grades <- a + b*totals

          # adjust grades below 4
          grades[which(grades<4)] <- grade_u4(p4 = p_4,
                                              points = totals[which(grades<4)],
                                              c=c)

          # rounding, formatting
          if (round==.1) {

               grades[grades>6] <- 6
               grades[grades<1] <- 1
               grades[grades<4 & grades>3.9] <- 3.9 # sharp threshold
               grades[grades<6 & grades>5.9] <- 5.9 # sharp threshold

               grades <- round_any(grades, .1 )

          }
          if (round==.5) {

               grades[grades>6] <- 6
               grades[grades<1] <- 1
               grades[grades<4 & grades>3.5] <- 3.5 # sharp threshold
               grades[grades<6 & grades>5.5] <- 5.5 # sharp threshold

               grades <- round_any(grades, .5)


          }


          return(list(grades=grades,
                      dec_up=dec_up,
                      top_points=(1-quant),
                      p_4=p_4,
                      ratio64=ratio64,
                      c=c)
          )
     }
