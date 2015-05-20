/**
 * Created by 022280451 on 18/05/2015.
 */
class Cheque(val valor: Double) {

  def humanize():String = {
    val parteInteira = valor.toInt
    val reais = if(parteInteira > 0){
      humanize(parteInteira) + " reais"
    } else {
      ""
    }
    val parteFracionaria = BigDecimal(valor).remainder(parteInteira) * 100
    val centavos = if (parteFracionaria > 0) {
      " e " + humanize(parteFracionaria.toInt) + " centavos"
    } else {
      ""
    }
    reais + centavos
  }

  private def humanize(valorEntrada: Int): String = {
    val tamanho = valorEntrada.toString.length

    if(tamanho <= 2) {
      return humanizedDezena(valorEntrada)
    }

    if(tamanho == 3) {
      return humanizedCentena(valorEntrada)
    }

    if(tamanho >= 4) {
      val parteMilhar = valorEntrada / 1000
      val parteCentena = valorEntrada % 1000
      val humanizedMilhar = parteMilhar match {
        case 1 => "mil"
        case _ => humanize(parteMilhar) + " mil"
      }
      if(parteCentena > 0){
        return humanizedMilhar + " e " + humanizedCentena(parteCentena)
      }else {
        return humanizedMilhar
      }
    }
    ""
  }

  private def humanizedCentena(valor: Double): String = {
    val parteDezena = valor % 100
    val parteCentena = valor - parteDezena
    if (valor == 100 && parteDezena == 0) {
      return "cem"
    } else {
      return Array(numbers.get(parteCentena.toInt).get," e ",humanizedDezena(parteDezena)).mkString
    }
  }

  private def humanizedDezena(valor: Double): String = {
    if(valor <= 20 || (valor % 10) == 0 ){
      numbers.get(valor.toInt).get
    }else{
      val parteDezena = valor - (valor % 10);
      val parteUnidade = valor % 10;
      val humanizedDezena = numbers.get(parteDezena.toInt).get
      val humanizedUnidade = numbers.get(parteUnidade.toInt).get
      humanizedDezena + " e " + humanizedUnidade
    }
  }

  private def numbers: Map[Int, String] = {
    Map(
      1 -> "um",
      2 -> "dois",
      3 -> "tres",
      4 -> "quatro",
      5 -> "cinco",
      6 -> "seis",
      7 -> "sete",
      8 -> "oito",
      9 -> "nove",
      10 -> "dez",
      11 -> "onze",
      12 -> "doze",
      13 -> "treze",
      14 -> "quatorze",
      15 -> "quinze",
      16 -> "dezesseis",
      17 -> "dezessete",
      18 -> "dezoito",
      19 -> "dezenove",
      20 -> "vinte",
      30 -> "trinta",
      40 -> "quarenta",
      50 -> "cinquenta",
      60 -> "sessenta",
      70 -> "setenta",
      80 -> "oitenta",
      90 -> "noventa",
      100 -> "cento",
      200 -> "duzentos",
      300 -> "trezentos",
      400 -> "quatrocentos",
      500 -> "quinhentos",
      600 -> "seicentos",
      700 -> "setecentos",
      800 -> "oitocentos",
      900 -> "novecentos"
    )
  }
}

object Cheque {
  def apply(valor: Double) = new Cheque(valor)
  implicit def doubleToCheque(valor:Double) = Cheque(valor)
  implicit def intToCheque(valor:Int) = Cheque(valor.toDouble)
}
