#' Calcula o valor crítico para a estatística para um teste de hipóteses bilateral sob a distribuição t de Sudent para
#' amostras de reduzido tamanho extraídas de uma população reconhecidamente Normal
#' @param alfa O valor do nível de significância arbitrado
#' @param dados O vetor com os elementos amostrais extraídos da população sob teste
#' @return O quantil crítico da distribuição t de Student para o nível de significância arbitrado
#' @seealso \url{http://www.r-pŕoject.org}
#' para mais informações sobre a função \code{\link[stats]{qt}}
#' @importFrom graphics curve mtext polygon segments text
#' @importFrom stats dnorm dt qnorm qt sd
#' @export
#' @examples
#' \dontrun{
#' dados=c(10,11,12,13)
#' alfa=0.05
#' t_crit(alfa, dados)
#' }
t_crit = function(alfa, dados) {
    return(qt(p=c(alfa/2, 1 - alfa/2), df=(length(dados)-1) ))
}



cx.t = function(alfa, dados) {
    return(c( t_crit(alfa,dados)[1], seq(t_crit(alfa,dados)[1], t_crit(alfa,dados)[2],
                                      0.01), t_crit(alfa,dados)[2]) )
}


cy.t = function(alfa, dados) {
    return(c(t_crit(alfa, dados)[1], dt(seq(t_crit(alfa, dados)[1], t_crit(alfa, dados)[2],
                                         0.01),  df=(length(dados)-1)  ), -0.5))
}



#' Calcula o valor da estatística para um teste de hipóteses bilateral sob a distribuição t de Sudent
#' amostras de reduzido tamanho extraídas de uma população reconhecidamente Normal
#' @param dados O vetor com os elementos amostrais extraídos da população sob teste
#' @param H0 O valor assumido sob a hipótese nula
#' @export
#' @examples
#' \dontrun{
#' dados=c(10,11,12,13)
#' H0=11
#' t_calc(dados,H0)
#' }
t_calc = function(dados,H0) {
    return((mean(dados)-H0)/(sd(dados)/sqrt( length(dados)       )))
}



curva.t = function(alfa, dados, H0) {

    if (t_crit(alfa, dados)[1] < t_calc(dados, H0)[1] & t_calc(dados, H0)[1] < t_crit(alfa, dados)[2]) {
        cor = "white"
    } else {
        cor = "darkblue"
    }

    if (t_crit(alfa, dados)[1] < t_calc(dados, H0)[1] & t_calc(dados, H0)[1] < t_crit(alfa, dados)[2]) {
        x.limite = c(t_crit(alfa, dados)[1]-4, t_crit(alfa, dados)[2]+4)
    } else {
        x.limite = c(-t_calc(dados, H0)[1]-4 , t_calc(dados, H0)[1]+4)
    }


    x=NULL
    curve(dt(x, df=(length(dados)-1)), xlim =x.limite,    ylim = c(0, 0.5),
          main = paste("Distribuicao t de Student ", "(GL=", df=(length(dados)-1), ")",
                       "\nH0=", H0, "versus H1 (media amostral)=", round(mean(dados),2),"", sep = " "),
          xlab = "Valores de t",
          ylab = "Densidade")
}


aspec.t = function(alfa, dados, H0) {

    if (t_crit(alfa, dados)[1] < t_calc(dados, H0)[1] & t_calc(dados, H0)[1] < t_crit(alfa, dados)[2]) {
        cor = "white"
    } else {
        cor = "darkblue"
    }

    segments(x0 = t_crit(alfa, dados)[2], y0 = 0, x1 = t_crit(alfa, dados)[2], y1 = 0.1,
             col = "red", lty = c(2, 2), lwd = c(1, 1))
    segments(x0 = t_crit(alfa, dados)[1], y0 = 0, x1 = t_crit(alfa, dados)[1], y1 = 0.1,
             col = "red", lty = c(2, 2), lwd = c(1, 1))

    segments(x0 = t_calc(dados, H0)[1], y0 = 0.05, x1 = t_calc(dados, H0)[1], y1 = 0.26, col = 'darkorange', lty = c(2,
                                                                                                            2), lwd = c(1, 1))
    text(x = t_calc(dados, H0)[1], y = 0.15, bquote(atop(t.calculado == .(round(t_calc(dados, H0)[1], 2)))),
         col = 'darkorange', srt = 90)

    text(x = t_crit(alfa, dados)[1] -2, y = 0.35, "Regiao de \nrejeicao de Ho",
         col = "red", srt = 0, cex = 1.1)
    text(x = t_crit(alfa, dados)[2] +2, y = 0.35, "Regiao de \nrejeicao de Ho",
         col = "red", srt = 0, cex = 1.1)

    text(x = t_crit(alfa, dados)[1] -2, y = 0.28, bquote(atop('\U03B1'/2 == .(alfa/2))),
         col = "red")
    text(x = t_crit(alfa, dados)[2] +2, y = 0.28, bquote(atop('\U03B1'/2 == .(alfa/2))),
         col = "red")


    text(x = 0, y = 0.45, "Regiao de nao rejeicao de Ho", col = "red", srt = 0,
         cex = 1.1)
    text(x = 0, y = 0.4, bquote(atop((1 - '\U03B1') == .(1 - alfa))), col = "red")

    text(x = t_crit(alfa, dados)[2], y = 0.1, bquote(atop(t.crit == .(round(t_crit(alfa,
                                                                                dados)[2], 2)))), col = "red", srt = 0)
    text(x = t_crit(alfa, dados)[1], y = 0.1, bquote(atop(t.crit == .(round(t_crit(alfa,
                                                                                dados)[1], 2)))), col = "red", srt = 0)

}

#' Plota o distribuição t de Student com todos os elementos para o teste de hipóteses bilateral para
#' amostras de reduzido tamanho extraídas de uma população reconhecidamente Normal
#' @param alfa O nível de significância arbitrado
#' @param dados O vetor com os dados da amostra extraida da população sob teste
#' @param H0 O valor assumido sob a hipótese nula
#' @export
#' @examples
#' \dontrun{
#' dados=c(10,11,12,13)
#' alfa=0.05
#' H0=11
#' t_hip(alfa, dados, H0)
#' }
t_hip = function(alfa, dados, H0) {
    df = (length(dados) - 1)
    x.t = cx.t(alfa, dados)
    y.t = cy.t(alfa, dados)
    curva.t(alfa, dados, H0)
    polygon(x.t, y.t, col = "blue")
    aspec.t(alfa, dados, H0)
}



#' Calcula o valor crítico para a estatística para um teste de hipóteses bilateral sob a distribuição Normal padronizada
#' @param alfa O valor do nível de significância arbitrado
#' @return O quantil crítico da distribuição Normal padronizada para o nível de significância arbitrado
#' @seealso \url{http://www.r-pŕoject.org}
#' para mais informações sobre a função \code{\link[stats]{dnorm}}
#' @examples
#' alfa=0.05
#' z.crit(alfa)
#' @export
z.crit = function(alfa) {
    return(qnorm(c(alfa/2, 1 - alfa/2)))
}




cx.z = function(alfa) {
    return(c(z.crit(alfa)[1], seq(z.crit(alfa)[1], z.crit(alfa)[2], 0.01), z.crit(alfa)[2]))
}

cy.z = function(alfa) {
    return(c(z.crit(alfa)[1], dnorm(seq(z.crit(alfa)[1], z.crit(alfa)[2], 0.01)),
        -0.5))
}


#' Calcula o valor da estatística para um teste de hipóteses bilateral sob a distribuição Normal padronizada
#' @param dados O vetor com os elementos amostrais extraídos da população sob teste
#' @param H0 O valor assumido sob a hipótese nula
#' @param sigma Desvio padrao populacional conhecido ou arbitrado
#' @examples
#' dados=c(10,11,12,13)
#' H0=11
#' sigma=2
#' z.calc(dados, H0, sigma)
#' @export
z.calc = function(dados,H0,sigma) {
 return((mean(dados) - H0)/ (sigma / sqrt(length(dados))) )
}



curva.z = function(alfa, dados, H0, sigma) {

    if (z.crit(alfa)[1] < z.calc(dados, H0, sigma)[1] & z.calc(dados, H0, sigma)[1] < z.crit(alfa)[2]) {
        x.limite = c(z.crit(alfa)[1]-4, z.crit(alfa)[2]+4)
    } else {
        x.limite = c(-z.calc(dados, H0, sigma)[1]-4 , z.calc(dados, H0, sigma)[1]+4)
    }


    x=NULL
    curve(dnorm(x), xlim =  x.limite ,  ylim = c(0, 0.5),
          main = paste("Distribuicao Normal padronizada",
        "\nH0=", H0, "versus H1 (media amostral)=", round(mean(dados),2),"", sep = " "), xlab = "Valores de z", ylab = "Densidade")
}


aspec.z = function(alfa, dados, H0, sigma) {

    if (z.crit(alfa)[1] < z.calc(dados, H0, sigma)[1] & z.calc(dados, H0, sigma)[1] < z.crit(alfa)[2]) {
        cor = "white"
    } else {
        cor = "darkblue"
    }

    segments(x0 = z.crit(alfa)[2], y0 = 0, x1 = z.crit(alfa)[2], y1 = 0.1, col = "red",
        lty = c(2, 2), lwd = c(1, 1))
    segments(x0 = z.crit(alfa)[1], y0 = 0, x1 = z.crit(alfa)[1], y1 = 0.1, col = "red",
        lty = c(2, 2), lwd = c(1, 1))

    segments(x0 = z.calc(dados, H0, sigma)[1], y0 = 0.05, x1 = z.calc(dados, H0, sigma)[1], y1 = 0.26, col = 'darkorange', lty = c(2,
        2), lwd = c(1, 1))
    text(x = z.calc(dados, H0, sigma)[1], y = 0.25, bquote(atop(z.calculado == .(round(z.calc(dados, H0, sigma)[1], 2)))),
        col = 'darkorange', srt = 90)

    text(x = z.crit(alfa)[1] - 2, y = 0.35, "Regiao de \nrejeicao de Ho", col = "red",
        srt = 0, cex = 1.1)
    text(x = z.crit(alfa)[2] + 2, y = 0.35, "Regiao de \nrejeicao de Ho", col = "red",
        srt = 0, cex = 1.1)

    text(x = z.crit(alfa)[1] - 2, y = 0.28, bquote(atop('\U03B1'/2 == .(alfa/2))),
        col = "red")
    text(x = z.crit(alfa)[2] + 2, y = 0.28, bquote(atop('\U03B1'/2 == .(alfa/2))),
        col = "red")


    text(x = 0, y = 0.45, "Regiao de nao rejeicao de Ho", col = "red", srt = 0,
        cex = 1.1)
    text(x = 0, y = 0.4, bquote(atop((1 - '\U03B1') == .(1 - alfa))), col = "red")

    text(x = z.crit(alfa)[2], y = 0.1, bquote(atop(z.crit == .(round(z.crit(alfa)[2],
        2)))), col = "red", srt = 0)
    text(x = z.crit(alfa)[1], y = 0.1, bquote(atop(z.crit == .(round(z.crit(alfa)[1],
        2)))), col = "red", srt = 0)

}

#' Recebe o vetor com os dados amostrais, o nível de significância e o valor assumido pela hipótese nula
#' @param alfa O nível de significância arbitrado
#' @param dados O vetor com os dados da amostra extraida da população sob teste
#' @param H0 o valor assumido sob a hipótese nula
#' @param sigma Desvio padrao populacional conhecido ou arbitrado
#' @examples
#' dados=c(10,11,12,13)
#' alfa=0.05
#' H0=11
#' sigma=2
#' z.hip(alfa, dados, H0, sigma)
#' @export
z.hip = function(alfa, dados, H0, sigma) {
    x.z = cx.z(alfa)
    y.z = cy.z(alfa)
    curva.z(alfa, dados, H0, sigma)
    polygon(x.z, y.z, col = "blue")
    aspec.z(alfa, dados, H0, sigma)
}



























