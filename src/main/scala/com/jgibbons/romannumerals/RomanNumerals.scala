package com.jgibbons.romannumerals

/**
  * M=1000, CM=900, D= 500, CD=400, C=100, XC=90, L=50, XL=40, X=10, IX=9, V=5, IV=4, I=1
  */
object RomanNumerals {

  def convertLongImperative( value: Int) :String = {
    var ret = ""

    var remainder = value
    val numM :Int= remainder/1000
    remainder = remainder - (numM * 1000)

    for (cnt <- 0 until numM) ret += "M"

    if (remainder >= 900) {
      ret += "CM"
      remainder = remainder - 900
    } else {
      if (remainder >= 500) {
        ret += "D"
        remainder -= 500

        val numC = remainder / 100
        for (cnt <- 0 until numC) ret += "C"
        remainder -= numC * 100
      } else {
        if (remainder >= 400) {
          ret += "CD"
          remainder -= 400
        } else {
          val numC = remainder / 100
          for (cnt <- 0 until numC) ret += "C"
          remainder -= numC * 100
        }
      }
    }

    if (remainder>=90) {
      ret += "XC"
      remainder = remainder - 90
    } else {
      if (remainder >= 50) {
        ret += "L"
        remainder -= 50

        val numX = remainder / 10
        for (cnt <- 0 until numX) ret += "X"
        remainder -= numX * 10
      } else {
        if (remainder >= 40) {
          ret += "XL"
          remainder -= 40
        } else {
          val numX = remainder / 10
          for (cnt <- 0 until numX) ret += "X"
          remainder -= numX * 10
        }
      }
    }

    if (remainder==9) {
      ret += "IX"
    } else {
      if (remainder >= 5) {
        ret += "V"
        remainder -= 5

        for (cnt <- 0 until remainder) ret += "I"
      } else {
        if (remainder == 4) {
          ret += "IV"
        } else {
          for (cnt <- 0 until remainder) ret += "I"
        }
      }
    }

    ret
  }

  /**
    * This was a very quick attempt, but it is not refined and uses var's etc.
    * Folds are not the right approach
    */
  type RomanInfo = Tuple4[Int, String, Int, String]
  val romanNumeralRanges = List[RomanInfo]( (1000, "M", 900, "CM"), (500, "D", 400,"CD"),
    (100, "C", 90, "XC"), (50, "L", 40, "XL"), (10, "X", 9, "IX"), (5, "V", 4, "IV"),
    (1, "I", 1, "I"))

  def convertFoldLeft( value: Int) : String = {
    val result = romanNumeralRanges.foldLeft( ("", value) ) (( strRemainder:Tuple2[String, Int], info: RomanInfo) => {
      var returnStr = strRemainder._1
      var remainder = strRemainder._2
      if (remainder<1) strRemainder
      else {
        val num = remainder/info._1
        for (i <- 0 until num) returnStr = returnStr + info._2
        remainder = remainder - (num * info._1)
        if (remainder >= info._3) {
          remainder = remainder - info._3
          returnStr = returnStr + info._4
        }
        (returnStr, remainder)
      }
    })

    result._1
  }

  /**
    * Just do a long imperative style, but remove the mutating state
    */
  def convertStrs(value:Int) : String = {
    val valueUnderM = value % 1000
    val valueUnderC = value % 100
    val valueUnderX = value % 10

    {if (value>=1000) "MMMMMMMMMMMMMMMMMMMMMMMMMMMM".substring(0, value/1000) else ""} +
      {if (valueUnderM >=900) "CM" else ""} +
      {if (valueUnderM >=500 && valueUnderM <900) "D" else ""} +
      {if (valueUnderM >500 && valueUnderM <900) "CCC".substring(0, (valueUnderM-500)/100) else ""} +
      {if (valueUnderM >=400 && valueUnderM <500) "CD" else ""} +
      {if (valueUnderM >=100 && valueUnderM <400) "CCC".substring(0, valueUnderM/100) else ""} +
      {if (valueUnderC >=90) "XC" else ""} +
      {if (valueUnderC >=50 && valueUnderC <90) "L" else ""} +
      {if (valueUnderC >50 && valueUnderC <90) "XXX".substring(0, (valueUnderC-50)/10) else ""} +
      {if (valueUnderC >=40 && valueUnderC <50) "XL" else ""} +
      {if (valueUnderC >=10 && valueUnderC <40) "XXX".substring(0, valueUnderC/10) else ""} +
      {if (valueUnderX ==9) "IX" else ""} +
      {if (valueUnderX >=5 && valueUnderX<9) "V" else ""} +
      {if (valueUnderX >5 && valueUnderX <9) "III".substring(0, valueUnderX-5) else ""} +
      {if (valueUnderX ==4) "IV" else ""} +
      {if (valueUnderX >=1 && valueUnderX <4) "III".substring(0, valueUnderX) else ""}
  }

  def convertMixed(value: Int) : String = {
    val numeralsFor100s = List("C", "CC", "CCC", "CD", "D", "DC", "DCC", "DCCC", "CM")
    val numeralsFor10s = List("X", "XX", "XXX", "XL", "L", "LX", "LXX", "LXXX", "XC")
    val numeralsFor1s = List("I", "II", "III", "IV", "V", "VI", "VII", "VIII", "IX")
    "" + {if (value>=1000) "MMMMMMMMMMMMMMMMMMMMMMMMMMMMMMM".substring(0, value/1000) else ""} +
    {if (value%1000>99) numeralsFor100s(value%1000/100-1) else ""} +
      {if (value%100>9) numeralsFor10s(value%100/10-1) else ""} +
      {if (value%10>0) numeralsFor1s(value%10-1) else ""}
  }

  def convert(value: Int) : String = {
    (0 until value/1000).map( _ => "M").mkString +
    ("","C", "CC", "CCC", "CD", "D", "DC", "DCC", "DCCC", "CM").productElement(value%1000/100) +
    ("","X", "XX", "XXX", "XL", "L", "LX", "LXX", "LXXX", "XC").productElement(value%100/10) +
    ("","I", "II", "III", "IV", "V", "VI", "VII", "VIII", "IX").productElement(value%10)
  }


}
