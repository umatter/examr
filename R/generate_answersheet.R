##' Generate Answer Sheet
##'
##' Based on a template, generates an excel workbooks as answer sheet for FormScanner
##' @usage generate_answersheet(as_template=NULL, title, date=Sys.Date())
##' @as_template character string, path/filename of the excel workbook to be used as temlate (defaults to the one delivered in the package)
##' @param title character string, the title of the course/exam
##' @param date character string, the date of the exam (by default the system date)
##' @return the path/filename of the generated answer sheet
##' @details The...
##' @author Ulrich Matter <umatter@protonmail.com>
##' @depends XLConnect
##' @examples
##' generate_answersheet(title="Econ 101")
##' @export
##'


generate_answersheet <-
     function(as_template=NULL, title, date=Sys.Date()) {

          if (is.null(as_template)){
               fpath <- system.file("extdata", "default_answersheet.xlsx", package="examr")
          } else {
               fpath <- as_template
          }

          # load the template answersheet
          as <- XLConnect::loadWorkbook(fpath)
          # add exam info (note: the named region is predefined in the default_answersheet)
          # add the course name
          writeNamedRegion(object = as, data = title, name = "course_name")
          # add the date of the exam
          writeNamedRegion(object = as, data = date, name = "exam_date")

          # write workbook to file
          as_output_path <- paste0("exam_answersheet_", title, ".xlsx")
          saveWorkbook(as, file = as_output_path)


          return(as_output_path)


     }

