package fp_in_scala.chapter_6.RNG

trait RNG {

  def nextInt: (Int, RNG)

}
