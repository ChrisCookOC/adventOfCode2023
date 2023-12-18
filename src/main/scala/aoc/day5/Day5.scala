package aoc.day5

import scala.io.Source

case class Day5() {
  def findNewRangeForAllMaps(map: Map, range: List[OurRange]): List[OurRange] = {
    map.entries.foldLeft(range) { (cur, mapEntry) =>
      cur.flatMap { x =>
        if (x.hasAlreadyAmended) {
          List(x)
        } else {
          findNewRangeFromMap(mapEntry, x)
        }
      }
    }
  }

  def findNewRangeFromMap(map: MapEntry, ourRange: OurRange): List[OurRange] = {

    val sourceEnd = map.sourceStart + map.range
    if (ourRange.sourceStart < map.sourceStart && ourRange.sourceEnd > sourceEnd) {
      //Encapsulates the range
      List(
        OurRange(ourRange.sourceStart, map.sourceStart - 1, false, ourRange.sourceStart, map.sourceStart - 1),
        OurRange(map.sourceStart, sourceEnd - 1, true, map.destinationStart, map.destinationStart + map.range),
        OurRange(sourceEnd, ourRange.sourceEnd, false, sourceEnd, ourRange.sourceEnd)
      )
    } else if (ourRange.sourceStart < map.sourceStart && ourRange.sourceEnd > map.sourceStart && ourRange.sourceEnd < sourceEnd) {
      //Starts below, ends up within
      List(
        OurRange(ourRange.sourceStart, map.sourceStart - 1, false, ourRange.sourceStart, map.sourceStart - 1),
        OurRange(map.sourceStart,
          ourRange.sourceEnd, true, map.destinationStart,
          map.destinationStart - map.sourceStart + ourRange.sourceEnd)
      )
    } else if (ourRange.sourceStart >= map.sourceStart && ourRange.sourceStart < sourceEnd && ourRange.sourceEnd >= sourceEnd) {
      //Starts in range ends up out of it
      List(
        OurRange(ourRange.sourceStart, map.sourceStart + map.range - 1,
          true, map.destinationStart + (ourRange.sourceStart - map.sourceStart),
          map.destinationStart + map.range),
        OurRange(sourceEnd, ourRange.sourceEnd, false, sourceEnd, ourRange.sourceEnd)
      )
    }
    else if (map.sourceStart <= ourRange.sourceStart && ourRange.sourceEnd < sourceEnd) {
      //All in range
      List(OurRange(ourRange.sourceStart,
        ourRange.sourceEnd, true,
        map.destinationStart + (ourRange.sourceStart - map.sourceStart),
        map.destinationStart - map.sourceStart + ourRange.sourceEnd))
    }
    else {
      List(ourRange.copy(destStart = ourRange.sourceStart, destEnd = ourRange.sourceEnd))
    }
  }

  def getLowestLocationForSeedRange(almanac: Almanac, entry: SeedEntry): Long = {

    val soils = findNewRangeForAllMaps(almanac.maps.find(x => x.from == Day5.seed).get, List(OurRange(entry.start, entry.start + entry.range - 1, false)))
    val fertilizer = findNewRangeForAllMaps(almanac.maps.find(x => x.from == Day5.soil).get, soils.map(x => OurRange(x.destStart, x.destEnd, false)))
    val water = findNewRangeForAllMaps(almanac.maps.find(x => x.from == Day5.fertilizer).get, fertilizer.map(x => OurRange(x.destStart, x.destEnd, false)))
    val light = findNewRangeForAllMaps(almanac.maps.find(x => x.from == Day5.water).get, water.map(x => OurRange(x.destStart, x.destEnd, false)))
    val temperature = findNewRangeForAllMaps(almanac.maps.find(x => x.from == Day5.light).get, light.map(x => OurRange(x.destStart, x.destEnd, false)))
    val humidity = findNewRangeForAllMaps(almanac.maps.find(x => x.from == Day5.temperature).get, temperature.map(x => OurRange(x.destStart, x.destEnd, false)))
    val location = findNewRangeForAllMaps(almanac.maps.find(x => x.from == Day5.humidity).get, humidity.map(x => OurRange(x.destStart, x.destEnd, false)))
    location.map { x => x.destStart }.min
  }

  def findValueFromMap(map: Map, source: Long): Long = {
    map.entries.foldLeft(source) { (cur, entry) =>
      if (source < entry.sourceStart) cur
      else if (source >= entry.sourceStart + entry.range) cur
      else entry.destinationStart + (cur - entry.sourceStart)
    }
  }

  def findLowestLocationFromInput(input: String): Long =
    getLowestLocation(parseInput(input))

  def getLowestLocation(almanac: Almanac): Long = {
    almanac.seeds.map(x => getLowestLocationForSeedRange(almanac, x)).min
  }


  def getLocationForSeed(almanac: Almanac, seed: Long): Long = {

    val soil = findValueFromMap(almanac.maps.find(x => x.from == Day5.seed).get, seed)
    val fertilizer = findValueFromMap(almanac.maps.find(x => x.from == Day5.soil).get, soil)
    val water = findValueFromMap(almanac.maps.find(x => x.from == Day5.fertilizer).get, fertilizer)
    val light = findValueFromMap(almanac.maps.find(x => x.from == Day5.water).get, water)
    val temperature = findValueFromMap(almanac.maps.find(x => x.from == Day5.light).get, light)
    val humidity = findValueFromMap(almanac.maps.find(x => x.from == Day5.temperature).get, temperature)
    findValueFromMap(almanac.maps.find(x => x.from == Day5.humidity).get, humidity)

  }

  def parseInput(input: String): Almanac = {
    val lines = input.split("\n\n")
    val seeds = parseSeedLine(lines.head)
    val maps = lines.tail.map(x => parseMap(x)).toList

    Almanac(seeds, maps)

  }

  def parseMap(input: String): Map = {

    val lines = input.split("\n")
    val title = input.split("[- ]")
    val source = title.head
    val dest = title(2)
    val map = lines.tail.map(x => {
      val entries = x.split(" ").map(_.toLong).toList
      MapEntry(entries.head, entries(1), entries.last)
    }
    ).toList


    Map(source, dest, map)

  }

  def parseSeedLine(input: String): List[SeedEntry] = {
    input.split(" ").tail.map(_.toLong).grouped(2).map { x => SeedEntry(x.head, x.last) }.toList
  }

  def run(): Unit = {

    val file = Source.fromResource("day5Input.txt")

    val input = file.getLines().mkString("\n")

    val location = findLowestLocationFromInput(input)

    println(s"Lowest location is $location")

  }

}

object Day5 {
  val seed = "seed"
  val soil = "soil"
  val fertilizer = "fertilizer"
  val water = "water"
  val light = "light"
  val temperature = "temperature"
  val humidity = "humidity"
  val location = "location"
}

case class Map(from: String, to: String, entries: List[MapEntry])

case class Almanac(seeds: List[SeedEntry], maps: List[Map])

case class MapEntry(destinationStart: Long, sourceStart: Long, range: Long)

case class SeedEntry(start: Long, range: Long)

case class OurRange(sourceStart: Long, sourceEnd: Long, hasAlreadyAmended: Boolean, destStart: Long = 0, destEnd: Long = 0)
