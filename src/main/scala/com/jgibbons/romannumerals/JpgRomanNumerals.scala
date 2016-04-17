package com.jgibbons.romannumerals

object JpgRomanNumerals {
  private val ROMAN100 = List("", "C", "CC", "CCC", "CD", "D", "DC", "DCC", "DCCC", "CM")
  private val ROMAN10 = List("", "X", "XX", "XXX", "XL", "L", "LX", "LXX", "LXXX", "XC")
  private val ROMAN1 = List("", "I", "II", "III", "IV", "V", "VI", "VII", "VIII", "IX")

  def convert(v: Int): String = {
    "M" * (v / 1000) + ROMAN100(v % 1000 / 100) + ROMAN10(v % 100 / 10) + ROMAN1(v % 10)
  }
}
