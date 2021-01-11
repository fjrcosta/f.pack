#' Comprimento em cm de 60 trutas marinhas pesacadas por uma traineira comercial
#' na baía de Delaware na costa leste dos Estados Unidos.
#'
#' @docType data
#'
#' @usage data(trutas)
#'
#' @format Um dataframe com 1 coluna e 60 linhas de dados (nome da variável: comprimento).
#' Arquivo em formato 'cvs'
#'
#' @keywords datasets
#'
#' @references John E. Freund (2007) Estatística aplicada para Economia, Administração e
#' Contabilidade (p. 26)
#'
#' @source \href{https://doku.pub/documents/estatistica-aplicada-economia-administraao-e-contabilidade-john-e-freundpdf-el9vjp31vrqy}{Arquivo}
#'
#' @examples
#' data(trutas)
#' média=mean(trutas$comprimento)
#' com_maximo=max(trutas$comprimento)
"trutas"
