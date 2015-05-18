/**
 * Created by 022280451 on 18/05/2015.
 */
class Cheque(val valor: Double) {

  def humanize():String = {
    val parteInteira = valor.toInt
    val parteFracionaria = BigDecimal(valor).remainder(parteInteira)
    var centavos = ""
    if(parteFracionaria > 0) {
      var valorCentavos = parteFracionaria.toString.substring(2).toInt
      if(valorCentavos <  10){
        valorCentavos *= 10
      }
      centavos = " e " + humanize(valorCentavos.toInt) + " centavos"
    }
    humanize(parteInteira) + " reais" + centavos
  }

  private def humanize(valorEntrada: Int): String = {
    var resultado = Array[String]()

    val parteDezena = valorEntrada % 100
    if(parteDezena > 0) {
      resultado +:= humanizedDezena(parteDezena)
    }

    val parteCentena = (valorEntrada % 1000) - parteDezena
    if(parteCentena > 0) {
      if (parteCentena == 100 && resultado.length == 0) {
        resultado +:= "cem"
      } else {
        resultado +:= numbers.get(parteCentena.toInt).get
      }
    }

    if(valorEntrada >= 1000) {
      val parteMilhar = valorEntrada / 1000
      if(parteMilhar != 1) {
        resultado +:= humanize(parteMilhar) + " mil"
      }else {
        resultado +:=  "mil"
      }
    }

    resultado.mkString(" e ")
  }

  private def humanizedDezena(valor: Double): String = {
    if(valor <= 20){
      numbers.get(valor.toInt).get
    }else{
      val parteDezena = valor - (valor % 10);
      val parteUnidade = valor % 10;
      val humanizedDezena = numbers.get(parteDezena.toInt).get
      val humanizedUnidade = numbers.get(parteUnidade.toInt) match {
        case n: Some[String] => s" e ${n.get}"
        case _ => ""
      }
      humanizedDezena + humanizedUnidade
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

