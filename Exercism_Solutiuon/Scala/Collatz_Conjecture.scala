object CollatzConjecture {
	def steps(n :Int) : Option[Int] =
		n match{
			case x if x < 1 => None
			case 1 => Some(0)
			case x if x%2 == 0 => steps(n/2).map(_+1) // map(_+1) is used to add +1 into Some()
			case _ => steps(3*n+1).map(_+1)
		}
}