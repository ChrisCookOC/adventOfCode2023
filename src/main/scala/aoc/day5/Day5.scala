package aoc.day5

import scala.io.Source

case class Day5() {
  def findValueFromMap(map: Map, source: Double): Double = {
    map.entries.foldLeft(source) { (cur, entry) =>
      if (source < entry.sourceStart) cur
      else if (source >= entry.sourceStart + entry.range) cur
      else entry.destinationStart + (cur - entry.sourceStart)
    }
  }

  def findLowestLocationFromInput(input: String): Double =
    getLowestLocation(parseInput(input))

  def getLowestLocation(exampleAlmanac: Almanac): Double = {
    exampleAlmanac.seeds.map(x => getLocationForSeed(exampleAlmanac, x)).min
  }

  def getLocationForSeed(almanac: Almanac, seed: Double): Double = {
    //    val soil = almanac.maps.find(x => x.from==Day5.seed && x.to==Day5.soil).get.map(seed)
    //    val fertilizer = almanac.maps.find(x => x.from==Day5.soil && x.to==Day5.fertilizer).get.map(soil)
    //    val water = almanac.maps.find(x => x.from==Day5.fertilizer && x.to==Day5.water).get.map(fertilizer)
    //    val light = almanac.maps.find(x => x.from==Day5.water && x.to==Day5.light).get.map(water)
    //    val temperature = almanac.maps.find(x => x.from==Day5.light && x.to==Day5.temperature).get.map(light)
    //    val humidity = almanac.maps.find(x => x.from==Day5.temperature && x.to==Day5.humidity).get.map(temperature)
    //    almanac.maps.find(x => x.from==Day5.humidity && x.to==Day5.location).get.map(humidity)

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

    //    val mb = 1024*1024
    //    var runtime: Runtime = Runtime.getRuntime
    //    println("\nMemory in MB start of ParseMap")
    //    println("** Used Memory:  " + (runtime.totalMemory - runtime.freeMemory) / mb)
    //    println("** Free Memory:  " + runtime.freeMemory / mb)
    //    println("** Total Memory: " + runtime.totalMemory / mb)
    //    println("** Max Memory:   " + runtime.maxMemory / mb)
    //
    val lines = input.split("\n")
    val title = input.split("[- ]")
    val source = title.head
    val dest = title(2)
    //    runtime = Runtime.getRuntime
    //    println("\nMemory in MB after defs")
    //    println("** Used Memory:  " + (runtime.totalMemory - runtime.freeMemory) / mb)
    //    println("** Free Memory:  " + runtime.freeMemory / mb)
    //    println("** Total Memory: " + runtime.totalMemory / mb)
    //    println("** Max Memory:   " + runtime.maxMemory / mb)
    val map = lines.tail.map(x => {
      val entries = x.split(" ").map(_.toDouble).toList
      MapEntry(entries.head, entries(1), entries.last)
    }
    ).toList
    //    runtime = Runtime.getRuntime
    //    println("\nMemory in MB after getting entries")
    //    println("** Used Memory:  " + (runtime.totalMemory - runtime.freeMemory) / mb)
    //    println("** Free Memory:  " + runtime.freeMemory / mb)
    //    println("** Total Memory: " + runtime.totalMemory / mb)
    //    println("** Max Memory:   " + runtime.maxMemory / mb)
    //    val basicMap = SortedMap(Range.inclusive(0, 99).map(_.toDouble).map(x => (x, x)): _*)
    //    runtime = Runtime.getRuntime
    //    println("\nMemory in MB after defining a 0 to 99")
    //    println("** Used Memory:  " + (runtime.totalMemory - runtime.freeMemory) / mb)
    //    println("** Free Memory:  " + runtime.freeMemory / mb)
    //    println("** Total Memory: " + runtime.totalMemory / mb)
    //    println("** Max Memory:   " + runtime.maxMemory / mb)
    //    val map = entries.foldLeft(basicMap) { (map, entry) => {
    //      runtime = Runtime.getRuntime
    //      println("\nMemory in MB in foldLeft top level")
    //      println("** Used Memory:  " + (runtime.totalMemory - runtime.freeMemory) / mb)
    //      println("** Free Memory:  " + runtime.freeMemory / mb)
    //      println("** Total Memory: " + runtime.totalMemory / mb)
    //      println("** Max Memory:   " + runtime.maxMemory / mb)
    //
    //      //TODO not this it blows up memory
    //      val values = Range.inclusive(0, entry.range.toInt - 1).map(_.toDouble)
    //      runtime = Runtime.getRuntime
    //      println("\nMemory in MB after creating range for entry")
    //      println("** Used Memory:  " + (runtime.totalMemory - runtime.freeMemory) / mb)
    //      println("** Free Memory:  " + runtime.freeMemory / mb)
    //      println("** Total Memory: " + runtime.totalMemory / mb)
    //      println("** Max Memory:   " + runtime.maxMemory / mb)
    //
    //      val newList = values.map(x => (entry.sourceStart + x, entry.destinationStart + x))
    //      runtime = Runtime.getRuntime
    //      println("\nMemory in MB in nested bit")
    //      println("** Used Memory:  " + (runtime.totalMemory - runtime.freeMemory) / mb)
    //      println("** Free Memory:  " + runtime.freeMemory / mb)
    //      println("** Total Memory: " + runtime.totalMemory / mb)
    //      println("** Max Memory:   " + runtime.maxMemory / mb)
    //
    //      map ++ newList
    //
    //    }
    //    }

    Map(source, dest, map)

  }

  def parseSeedLine(input: String): List[Double] = {
    input.split(" ").tail.map(_.toDouble).toList
  }

  def run(): Unit = {

    val file = getClass.getResourceAsStream("input.txt")

    val input = Source.fromInputStream(file).mkString

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

case class Almanac(seeds: List[Double], maps: List[Map])

case class MapEntry(destinationStart: Double, sourceStart: Double, range: Double)