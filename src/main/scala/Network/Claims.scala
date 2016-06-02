package Network

case class Claims(f1a: Double = 0.0,
                  f1b: Double = 0.0,
                  f1c: Double = 0.0,
                  f2: Double = 0.0,
                  f3: Double = 0.0,
                  f4: Double = 0.0,
                  f5: Double = 0.0,
                  f6: Double = 0.0
                 ) {
  val allClaims = List(this.f1a, this.f1b, this.f1c, this.f2, this.f3, this.f6)

  def updateF1a(new_value: Double) = this.copy(f1a=new_value)
  def updateF1b(new_value: Double) = this.copy(f1b=new_value)
  def updateF1c(new_value: Double) = this.copy(f1c=new_value)
  def updateF2(new_value: Double) = this.copy(f2=new_value)
  def updateF3(new_value: Double) = this.copy(f3=new_value)
  def updateF4(new_value: Double) = this.copy(f4=new_value)
  def updateF5(new_value: Double) = this.copy(f5=new_value)
  def updateF6(new_value: Double) = this.copy(f6=new_value)

  def getPhi = allClaims.sum / allClaims.length
}
