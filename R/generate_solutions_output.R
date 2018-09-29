##' Generate Solution Text
##'
##' Generates Rmd strings for a set of questions with indicated sample solutions
##' @usage generate_solution_text(x)
##' @param x list, result of parsing file with questions via read_questions/select_questions
##' @param type_headers list, indicating the headers to be used for each question type section (see example)
##' @return a list of character strings (with markdown syntax)
##' @details The...
##' @author Ulrich Matter <umatter@protonmail.com>
##' @depends
##' @examples
##' myfile <- "inst/exdata/exam_questions.xlsx"
##' my_questions <- read_questions(myfile)
##' selection <-select_questions(my_questions)
##' generate_solution_text(selection)
##' @export
##'


generate_solution_text <-
     function(x, type_headers = list(tf="## Part 1: TRUE/FALSE Questions\nFor each of the following statements, decide whether the statement is TRUE (T) or FALSE (F).\n",
                                     mc="## Part 2: Multiple Choice Type A\nFor each of the following questions/statements, decide which of the possible responses are correct. NOTE: Either ONE OR SEVERAL of the response statements can be correct. In order to get a point, all correct response statements need to be marked!\n",
                                     one_correct="## Part 3: Multiple Choice  Type B (only one correct!)\nFor each of the following questions/statements, decide which of the possible responses is correct. NOTE: For each question only ONE of the possible responses is correct. Select ONLY ONE of the response statements per question!\n")) {

          # process TRUE/FALSE questions
          # select input
          tfs <- x$tf
          tfs$question_nr <- paste0(1:nrow(tfs), ".  ")
          tfs_header <- type_headers$tf
          tfs$sol_color <- "\\textcolor{green}{"
          tfs$sol_color[tfs$solution==F] <- "\\textcolor{red}{"
          tfs$sol_color <- paste0(tfs$sol_color, tfs$solution, "}")

          # create text output
          tf_text <- paste(tfs$question_nr,
                            tfs$question, "\n", sep = " ")

          tf_text <- paste(tf_text, tfs$sol_color, collapse = "\n")

          tf_text <- paste0(tfs_header, "\n",
                            tf_text, "\n")

          # process MC questions
          mcs <- x$mc
          mcs$question_nr <- paste0(1:nrow(mcs), ".  ")
          mcs_header <- type_headers$mc

          # create text output
          # questions
          q_text<- paste0(mcs$question_nr,
                          mcs$question, "\n")
          # responses per question
          responses_text <- list()
          for (i in 1:nrow(mcs)) {

               # extract response text, add letters
               responses <- paste0(toupper(letters[1:length(mcs[i,]$choices_solutions[[1]]$choices)]),
                                   ") ",
                                   trimws(as.character(mcs[i,]$choices_solutions[[1]]$choices)))

               # color correct solutions (latex)
               solutions <- as.logical(mcs[i,]$choices_solutions[[1]]$solutions)
               responses_colored <- paste0("\\textcolor{red}{", responses, "}\n")
               responses_colored[solutions] <- paste0("\\textcolor{green}{", responses[solutions], "}\n")

               responses_text[[i]] <- paste0(responses_colored, collapse = "")

          }
          responses_text <- unlist(responses_text)

          # put it together
          mcs_text <- paste0(q_text, responses_text, collapse="" )
          mcs_text <- paste0(mcs_header, mcs_text )


          # process MC questions (only one correct)
          mcs_oc <- x$one_correct
          mcs_oc$question_nr <- paste0(1:nrow(mcs_oc), ".  ")
          mcs_oc_header <- type_headers$one_correct

          # create text output
          # questions
          q_text_oc<- paste0(mcs_oc$question_nr,
                             mcs_oc$question, "\n")

          # responses per question
          responses_text_oc <- list()
          for (i in 1:nrow(mcs_oc)) {

               # extract response text, add letters
               responses <- paste0(toupper(letters[1:length(mcs_oc[i,]$choices_solutions[[1]]$choices)]),
                                   ") ",
                                   trimws(as.character(mcs_oc[i,]$choices_solutions[[1]]$choices)))

               # color correct solutions (latex)
               solutions <- as.logical(mcs_oc[i,]$choices_solutions[[1]]$solutions)
               responses_colored <- paste0("\\textcolor{red}{", responses, "}\n")
               responses_colored[solutions] <- paste0("\\textcolor{green}{", responses[solutions], "}\n")

               responses_text_oc[[i]] <- paste0(responses_colored, collapse = "")

          }
          responses_text_oc <- unlist(responses_text_oc)

          # put it together
          mcs_oc_text <- paste0(q_text_oc, responses_text_oc, collapse="" )
          mcs_oc_text <- paste0(mcs_oc_header, mcs_oc_text )



          # Put all questions in one text
          all_text <- paste0(tf_text, "\n",
                             mcs_text,"\n",
                             mcs_oc_text)


          return(all_text)

     }
