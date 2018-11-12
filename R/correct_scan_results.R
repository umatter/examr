##' Correct Scan Results
##'
##' Reads the scan results of running FormScan on the answer sheets, and the sample solutions, corrects the exams
##' @usage correct_scan_results(file, solutions_file, id_nr="student_number", groups=c("group001", "group002", "group003"))
##' @param file character, the path/filename of the csv file containing the scan results
##' @param solutions_file character, the path/filename of the csv file containing the sample solutions
##' @param id_nr character, group variable indicating the file-id (i.e., students number), defaults to "student_number"
##' @param groups named list indicating the possible responses defaults to c("group001", "group002", "group003")
##' @return a data.frame
##' @details The...
##' @author Ulrich Matter <umatter@protonmail.com>
##' @examples
##' myfile <- system.file("inst/exdata/example_scan_results.csv", package="examr")
##' solutions <- system.file("inst/exdata/example_sample_solutions.csv", package="examr")
##' my_results <- correct_scan_results(myfile, solutions)
##'
##' @export

read_scan_results <-
     function(file, solutions_file, id_nr="student_number", groups=c("group001", "group002", "group003")){


     }
