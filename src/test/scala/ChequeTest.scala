import org.scalatest.{FlatSpec, Matchers}

/**
 * Created by 022280451 on 18/05/2015.
 */
class ChequeTest extends FlatSpec with Matchers{

  it should "return dez reais when pass 10" in {
    val cheque10reais = Cheque(10)
    cheque10reais.humanize should be ("dez reais")
  }

  it should "return vinte e um reais when pass 21" in {
    val cheque21reais = Cheque(21)
    cheque21reais.humanize should be ("vinte e um reais")
  }

  it should "return cento e trinta e tres reais when pass 133" in {
    val cheque133reais = Cheque(133)
    cheque133reais.humanize should be ("cento e trinta e tres reais")
  }

  it should "return cem reais when pass 100" in {
    val cheque100reais = Cheque(100)
    cheque100reais.humanize should be ("cem reais")
  }

  it should "return quinhentos e noventa e nove when pass 599" in {
    val cheque599reais = Cheque(599)
    cheque599reais.humanize should be ("quinhentos e noventa e nove reais")
  }

  it should "return mil reais when pass 1000" in {
    val cheque1000reais = Cheque(1000)
    cheque1000reais.humanize should be ("mil reais")
  }

  it should "return trinta e tres mil e quatrocentos e cinquenta reais when pass 33450" in {
    val cheque33450reais = Cheque(33450)
    cheque33450reais.humanize should be ("trinta e tres mil e quatrocentos e cinquenta reais")
  }

  it should "return dois reais e trinta e dois centavos when pass 2.32" in {
    val cheque232reais = Cheque(2.32)
    cheque232reais.humanize should be ("dois reais e trinta e dois centavos")
  }

  it should "return tres reais e dois centavos when pass 3.02" in {
    val cheque302reais = Cheque(3.02)
    cheque302reais.humanize should be ("tres reais e dois centavos")
  }

  it should "be able to convert a number straight to the extensive representation" in {
    import Cheque._
    1000.humanize should be ("mil reais")
    33.40.humanize should be ("trinta e tres reais e quarenta centavos")
  }

}
