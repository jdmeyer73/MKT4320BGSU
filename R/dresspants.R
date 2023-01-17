#' Dress Pants Segmentation Data
#'
#' Data on a sample of 221 consumers that participated in research on dress
#'     pants. Specifically, they answered several questions about purchasing
#'     dress pants (i.e., what is important, what impacts their decision, etc.),
#'     as well as several questions about style, shopping, and personality.
#'     They also answered several demographic questions. \cr
#'     \cr
#'     \strong{Used in Lab Assignment 5}
#'
#' @usage data(dresspants)
#' @format
#' A data frame with 221 rows and 28 columns: \cr
#' \cr
#' \strong{Segmentation Variables Part 1} \cr
#'     (1:Strongly Disagree to 7:Strongly Agree)
#' \describe{
#'   \item{\code{fashn}}{I’m very conscious of dress pants as fashion objects}
#'   \item{\code{price}}{I am price conscious when it comes to dress pants}
#'   \item{\code{conv}}{I usually buy my clothing at the most convenient store}
#'   \item{\code{approp}}{I think about how my clothes fit situations/events/work}
#'   \item{\code{percept}}{The clothes I wear are important to the way people
#'       think of me}
#'   \item{\code{select}}{When I see a range of dress pants, I find it difficult
#'       to make a choice}
#'   \item{\code{stfknow}}{Satisfied with the level of knowledge about dress
#'       pants of the sales staff}
#'   \item{\code{brand}}{I always consider the brand name when purchasing dress
#'       pants}
#' }
#' \cr
#' \strong{Segmentation Variables Part 2} \cr
#'     (1:Not at all to 7:Very great extent)
#' \describe{
#'   \item{\code{disp}}{Extent to which store displays impacts purchase decision}
#'   \item{\code{staff}}{Extent to which advice provided by sales assistant
#'       impacts purchase decision}
#'   \item{\code{crease}}{Extent to which the crease impacts purchase decision}
#'   \item{\code{waist}}{Extent to which the style of the waist impacts purchase
#'       decision}
#'   \item{\code{mater}}{Extent to which the fabric impacts purchase decision}
#' }
#' \cr
#' \strong{Descriptor Variables Part 1} \cr
#'     (1:Strongly Disagree to 7:Strongly Agree)
#' \describe{
#'   \item{\code{stylish}}{I’m more stylish than most other people}
#'   \item{\code{notimp}}{Usually I do not pay much attention to the clothes
#'       I’m wearing}
#'   \item{\code{exclus}}{I like to shop for dress pants at exclusive shops }
#'   \item{\code{friends}}{I like to shop in the same clothing stores as my
#'       friends }
#' }
#' \cr
#' \strong{Descriptor Variables Part 2} \cr
#'     (1:Very unimportant to 7:Very important)
#' \describe{
#'   \item{\code{others}}{Other people as source of information for purchase
#'       decision}
#'   \item{\code{printad}}{Magazine or newspaper  as source of information for
#'       purchase decision}
#'   \item{\code{online}}{Internet as source of information for purchase
#'       decision}
#' }
#' \cr
#' \strong{Descriptor Variables Part 3} \cr
#'     (Semantic differential)
#' \describe{
#'   \item{\code{indulg}}{I am Thrifty (1) -- Indulgent (7)}
#'   \item{\code{mature}}{I am Youthful (1) -- Mature (7)}
#'   \item{\code{vain}}{I am Modest (1) -- Vain (7)}
#' }
#' \cr
#' \strong{Demographics} \cr
#' \describe{
#'   \item{\code{age}}{What is your age? Factor with 4 levels: \emph{A_18.24};
#'       \emph{B_25.39}; \emph{C_40.54}; \emph{D_55plus}}
#'   \item{\code{expense}}{In an average month, how much money do you spend on
#'       clothes? (Dollars) Factor with 3 levels: \emph{A_0to50};
#'       \emph{B_51to150}; \emph{C_Over150}}
#'   \item{\code{income}}{What was your household annual income in the last
#'       year? (000s Dollars) Factor with 3 levels: \emph{A_UpTo30K};
#'       \emph{B_30Kto80K}; \emph{C_Over80K}}
#'   \item{\code{edu}}{What is your education level? Factor with 3 levels:
#'       \emph{High school}; \emph{College}; \emph{Post-grad}}
#' }
#' @source Unknown
"dresspants"
