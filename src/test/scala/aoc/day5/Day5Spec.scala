package aoc.day5

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class Day5Spec extends AnyWordSpec with Matchers {

  private val day5 = Day5()

  private val exampleAlmanac = Almanac(
    List(79, 14, 55, 13), List(
      Map(Day5.seed, Day5.soil, List(MapEntry(50, 98, 2), MapEntry(52, 50, 48))),
      Map(Day5.soil, Day5.fertilizer, List(MapEntry(0, 15, 37), MapEntry(37, 52, 2), MapEntry(39, 0, 15))),
      Map(Day5.fertilizer, Day5.water, List(MapEntry(49, 53, 8), MapEntry(0, 11, 42), MapEntry(42, 0, 7), MapEntry(57, 7, 4))),
      Map(Day5.water, Day5.light, List(MapEntry(88, 18, 7), MapEntry(18, 25, 70))),
      Map(Day5.light, Day5.temperature, List(MapEntry(45, 77, 23), MapEntry(81, 45, 19), MapEntry(68, 64, 13))),
      Map(Day5.temperature, Day5.humidity, List(MapEntry(0, 69, 1), MapEntry(1, 0, 69))),
      Map(Day5.humidity, Day5.location, List(MapEntry(60, 56, 37), MapEntry(56, 93, 4))
      )))

  private val example = "seeds: 79 14 55 13\n\nseed-to-soil map:\n50 98 2\n52 50 48" +
    "\n\nsoil-to-fertilizer map:\n0 15 37\n37 52 2\n39 0 15" +
    "\n\nfertilizer-to-water map:\n49 53 8\n0 11 42\n42 0 7\n57 7 4" +
    "\n\nwater-to-light map:\n88 18 7\n18 25 70" +
    "\n\nlight-to-temperature map:\n45 77 23\n81 45 19\n68 64 13" +
    "\n\ntemperature-to-humidity map:\n0 69 1\n1 0 69" +
    "\n\nhumidity-to-location map:\n60 56 37\n56 93 4"

  "parseSeedLine" should {

    "store the seed numbers in a list" in {
      val input = "seeds: 79 14 55 13"

      day5.parseSeedLine(input) shouldBe List(79, 14, 55, 13)

    }

  }

  "parseMap" should {

    "parse a seed to soil map into a map object" in {

      val input = "seed-to-soil map:\n50 98 2\n52 50 48"

      day5.parseMap(input) shouldBe exampleAlmanac.maps.find(x => x.from == Day5.seed).get

    }

    "parse a fertilizer to water map into a map object" in {

      val input = "fertilizer-to-water map:\n49 53 8\n0 11 42\n42 0 7\n57 7 4"

      day5.parseMap(input) shouldBe exampleAlmanac.maps.find(x => x.from == Day5.fertilizer).get

    }

    "handle big numbers without crashing" in {
      day5.parseMap("seed-to-soil map:\n2988689842 4194451945 100515351")
    }
  }

  "parseInput" should {

    "return an almanac" in {

      day5.parseInput(example) shouldBe exampleAlmanac

    }

  }

  "getLocationForSeed" should {

    "traverse the maps to find the location for a given seed" in {
      day5.getLocationForSeed(exampleAlmanac, 79) shouldBe 82
      day5.getLocationForSeed(exampleAlmanac, 14) shouldBe 43
      day5.getLocationForSeed(exampleAlmanac, 55) shouldBe 86
      day5.getLocationForSeed(exampleAlmanac, 13) shouldBe 35

    }

  }

  "findValueFromMap" should {

    "convert a value from one type to another using the map" in {

      val map = Map(Day5.seed, Day5.soil, List(MapEntry(50, 98, 2), MapEntry(52, 50, 48)))

      day5.findValueFromMap(map, 79) shouldBe 81
      day5.findValueFromMap(map, 14) shouldBe 14
      day5.findValueFromMap(map, 55) shouldBe 57
      day5.findValueFromMap(map, 13) shouldBe 13

      val map2 = Map(Day5.fertilizer, Day5.water, List(MapEntry(49, 53, 8), MapEntry(0, 11, 42), MapEntry(42, 0, 7), MapEntry(57, 7, 4)))
      day5.findValueFromMap(map2, 53) shouldBe 49

    }

  }

  "getLowestLocation" should {

    "get lowest of the locations for the seeds" in {
      day5.getLowestLocation(exampleAlmanac) shouldBe 35
    }

  }

  "findLowestLocationFromInput" should {

    "find lowest location from the given input string" in {
      day5.findLowestLocationFromInput(example) shouldBe 35
    }

  }

}
