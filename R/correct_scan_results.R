##' Correct Scan Results
##'
##' Reads the scan results of running FormScan on the answer sheets, and the sample solutions, corrects the exams
##' @usage correct_scan_results(file, solutions_file, id_nr="student_number", groups=c("group001", "group002", "group003"))
##' @param file character, the path/filename of the csv file containing the scan results
##' @param solutions_file character, the path/filename of the csv file containing the sample solutions
##' @param points_file character, the path/filename of the csv file containing the points for correct responses per exam question
##' @return a data.frame
##' @details The...
##' @author Ulrich Matter <umatter@protonmail.com>
##' @examples
##' myfile <- system.file("inst/exdata/example_scan_results.csv", package="examr")
##' solutions <- system.file("inst/exdata/example_sample_solutions.csv", package="examr")
##' my_results <- correct_scan_results(myfile, solutions)
##'
##' @export
##'

correct_scan_results <-
     function(file, solutions_file, points_file){

          # read solutions and official sample solutions
          exam <-read_scan_results(file)
          solutions <- read_scan_results(solutions_file)
          points <- read_scan_results(points_file)

          # select responses, correct responses
          responses <- exam[,-1:-2] # first two columns are filename and student id

          # generate solutions frame for correction
          sol_frame <- solutions
          names(sol_frame) <- names(solutions)
          for (i in 2:nrow(responses)) {

               sol_frame <- rbind(sol_frame, solutions)

          }

          # generate points frame for correction
          points_frame <- points
          names(points_frame) <- names(points)
          for (i in 2:nrow(responses)) {

               points_frame <- rbind(points_frame, points)

          }

          # correct exam
          correction <- responses == sol_frame
          # assign points
          points_assigned <- as.data.frame(as.matrix(points_frame) * as.numeric(as.matrix(correction)))

          # add ids
          points_assigned <- cbind(exam[,1:2], points_assigned)

          return(points_assigned)

}
