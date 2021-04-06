package u05lab.code

object Lists {
    def sequence[A](e: List[Option[A]]): Option[List[A]] = e match {
        case Some(v) :: t => t.foldLeft[Option[List[A]]](Some(List(v)))({
            case (Some(l), Some(n)) => Some(l.append(List(n)))
            case _ => None
        })
        case _ => None
    }
}
