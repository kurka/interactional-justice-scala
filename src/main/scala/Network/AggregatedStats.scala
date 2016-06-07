package Network

case class AggregatedStats(allocations: Array[Double], satisfactions: Array[Double], utilities: Array[Double]) {
  def meanAlloc = allocations.sum / allocations.length
  def meanSatisf = satisfactions.sum / satisfactions.length
  def meanUtilities = utilities.sum / utilities.length

  override def toString = List(meanAlloc, meanSatisf, meanUtilities).mkString(", ")
}