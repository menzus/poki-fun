package core

trait RandomGenerator {
  def generateNextByte: (Int, RandomGenerator)
}

object RandomGenerator {

  def createWithWithSeed(seed: Long) = RandomGeneratorLcg(seed, 0, 0)

  case class RandomGeneratorLcg(seed: Long, remainingRandom: Int, remainingBytes: Int) extends RandomGenerator {

    private val a = 0x5DEECE66DL
    private val c = 0xBL
    private val m = 0xFFFFFFFFFFFFL

    override def generateNextByte: (Int, RandomGenerator) = {
      if (remainingBytes <= 0) {
        buildNextGenerator.generateNextByte
      } else {
        (remainingRandom & 0xFF, RandomGeneratorLcg(seed, remainingRandom >> 8, remainingBytes - 1))
      }
    }

    private def buildNextGenerator: RandomGenerator = {
      val newSeed = (seed * a + c) & m
      val nextInt = (newSeed >>> 16).toInt
      RandomGeneratorLcg(newSeed, nextInt, 4)
    }
  }

  def randomByteStream(initialGenerator: RandomGenerator): Stream[Int] = {
    val (nextByte, nextRandomGenerator) = initialGenerator.generateNextByte
    Stream.cons(nextByte, randomByteStream(nextRandomGenerator))
  }
}