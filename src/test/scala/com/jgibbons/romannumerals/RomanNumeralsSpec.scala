package com.jgibbons.romannumerals

import org.scalatest.FlatSpec
import scala.collection.immutable.HashMap

class RomanNumeralsSpec extends FlatSpec {
  behavior of "RomanNumerals"

  it should "Convert Properly" in {
    val mapped = HashMap[Int, String](
      1->"I", 2->"II", 3->"III", 4->"IV", 5->"V", 6->"VI", 7->"VII", 8->"VIII", 9->"IX", 10->"X",
      11->"XI", 12->"XII", 13->"XIII", 14->"XIV", 15->"XV", 16->"XVI", 17->"XVII", 18->"XVIII", 19->"XIX", 20->"XX",
      41->"XLI", 42->"XLII", 43->"XLIII", 44->"XLIV", 45->"XLV", 46->"XLVI", 47->"XLVII", 48->"XLVIII", 49->"XLIX",
      50->"L", 51->"LI", 60->"LX", 64->"LXIV", 68->"LXVIII", 69->"LXIX",
      90->"XC", 91->"XCI", 94->"XCIV", 96->"XCVI", 99->"XCIX",
      100->"C", 140->"CXL", 144->"CXLIV", 149->"CXLIX",
      189->"CLXXXIX", 190->"CXC", 194->"CXCIV", 199->"CXCIX",
      989->"CMLXXXIX", 990->"CMXC", 994->"CMXCIV", 999->"CMXCIX",
      1194->"MCXCIV", 1199->"MCXCIX",
      1989->"MCMLXXXIX", 1990->"MCMXC",
      5989->"MMMMMCMLXXXIX", 5990->"MMMMMCMXC", 5994->"MMMMMCMXCIV", 5999->"MMMMMCMXCIX"
    )

    mapped.foreach { case (k:Int, v:String) =>
        assert( RomanNumerals.convertLongImperative(k)==v, s"convertImperative failed for $k")
        assert( RomanNumerals.convertFoldLeft(k)==v, s"convertFoldLeft failed for $k")
        assert( RomanNumerals.convertStrs(k)==v, s"convertStrs failed for $k")
        assert( RomanNumerals.convertMixed(k)==v, s"convertMixed failed for $k")
        assert( RomanNumerals.convertTailRecursive(k)==v, s"convertTailRecursive failed for $k")
        assert( RomanNumerals.convert(k)==v, s"convert failed for $k")
    }
  }
}
