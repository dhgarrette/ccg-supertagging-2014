package dhg.ccg.cat

trait TrueCcgRule extends ((Cat, Cat) => Option[Cat]) {
  override def apply(left: Cat, right: Cat): Option[Cat]
  def inferRight(parent: Cat, left: Cat): Option[Cat]
  def inferLeft(parent: Cat, right: Cat): Option[Cat]
}

/**
 *  Forward Application
 *  X/Y  Y  =>  X
 */
case object FA extends TrueCcgRule {
  override def apply(left: Cat, right: Cat) = {
    left match {
      case x / y if y u right => Some(x)
      case _ => None
    }
  }

  override def inferRight(parent: Cat, left: Cat) = {
    left match {
      case x / y if parent u x => Some(y)
      case _ => None
    }
  }

  override def inferLeft(parent: Cat, right: Cat) = {
    Some(parent / right)
  }

  override def toString = "FA"
}

/**
 *  Forward Application: N -> NP unary conversion
 *  X/Y  Y  =>  X
 */
case object FAn2np extends TrueCcgRule {
  override def apply(left: Cat, right: Cat) = {
    (left, right) match {
      case (x / (y @ AtomCat("NP", _)), AtomCat("N", f)) if y u AtomCat("NP", f) => Some(x)
      case _ => None
    }
  }
  override def inferRight(parent: Cat, left: Cat) = ???
  override def inferLeft(parent: Cat, right: Cat) = ???
  override def toString = "FAn2np"
}

/**
 *  Backward Application
 *  Y  X\Y  =>  X
 */
case object BA extends TrueCcgRule {
  override def apply(left: Cat, right: Cat) = {
    right match {
      case l \ r if r u left => Some(l)
      case _ => None
    }
  }

  override def inferRight(parent: Cat, left: Cat) = {
    Some(parent \ left)
  }

  override def inferLeft(parent: Cat, right: Cat) = {
    right match {
      case rl \ rr if parent u rl => Some(rr)
      case _ => None
    }
  }

  override def toString = "BA"
}

/**
 *  Backward Application: N -> NP unary conversion
 *  Y  X\Y  =>  X
 */
case object BAn2np extends TrueCcgRule {
  override def apply(left: Cat, right: Cat) = {
    (left, right) match {
      case (AtomCat("N", f), l \ (r @ AtomCat("NP", _))) if r u AtomCat("NP", f) => Some(l)
      case _ => None
    }
  }
  override def inferRight(parent: Cat, left: Cat) = ???
  override def inferLeft(parent: Cat, right: Cat) = ???
  override def toString = "BAn2np"
}

/**
 *  Forward Harmonic Composition
 *  X/Y  Y/Z  =>  X/Z
 */
case object FC extends TrueCcgRule {
  override def apply(left: Cat, right: Cat) = {
    (left, right) match {
      case (ll / lr, rl / rr) if lr u rl => Some(ll / rr)
      case _ => None
    }
  }

  override def inferRight(parent: Cat, left: Cat) = {
    (parent, left) match {
      case (pl / pr, ll / lr) if pl u ll => Some(lr / pr)
      case _ => None
    }
  }

  override def inferLeft(parent: Cat, right: Cat) = {
    (parent, right) match {
      case (pl / pr, rl / rr) if pr u rr => Some(pl / rl)
      case _ => None
    }
  }

  override def toString = "FC"
}

/**
 *  Backward Harmonic Composition
 *  Y\Z  X\Y  =>  X\Z
 */
case object BC extends TrueCcgRule {
  override def apply(left: Cat, right: Cat) = {
    (left, right) match {
      case (ll \ lr, rl \ rr) if ll u rr => Some(rl \ lr)
      case _ => None
    }
  }

  override def inferRight(parent: Cat, left: Cat) = {
    (parent, left) match {
      case (pl \ pr, ll \ lr) if pr u lr => Some(pl \ ll)
      case _ => None
    }
  }

  override def inferLeft(parent: Cat, right: Cat) = {
    (parent, right) match {
      case (pl \ pr, rl \ rr) if pl u rl => Some(rr \ pr)
      case _ => None
    }
  }

  override def toString = "BC"
}

/**
 *  Forward Crossed Composition
 *  X/Y  Y\Z  =>  X\Z
 */
case object FX extends TrueCcgRule {
  override def apply(left: Cat, right: Cat) = {
    (left, right) match {
      case (ll / lr, rl \ rr) if lr u rl => Some(ll \ rr)
      case _ => None
    }
  }

  override def inferRight(parent: Cat, left: Cat) = {
    (parent, left) match {
      case (pl \ pr, ll / lr) if pl u ll => Some(lr \ pr)
      case _ => None
    }
  }

  override def inferLeft(parent: Cat, right: Cat) = {
    (parent, right) match {
      case (pl \ pr, rl \ rr) if pr u rr => Some(pl / rl)
      case _ => None
    }
  }

  override def toString = "FX"
}

/**
 *  Backward Crossed Composition
 *  Y/Z  X\Y  =>  X/Z
 */
case object BX extends TrueCcgRule {
  override def apply(left: Cat, right: Cat) = {
    (left, right) match {
      case (ll / lr, rl \ rr) if ll u rr => Some(rl / lr)
      case _ => None
    }
  }

  override def inferRight(parent: Cat, left: Cat) = {
    (parent, left) match {
      case (pl / pr, ll / lr) if pr u lr => Some(pl \ ll)
      case _ => None
    }
  }

  override def inferLeft(parent: Cat, right: Cat) = {
    (parent, right) match {
      case (pl / pr, rl \ rr) if pl u rl => Some(rr / pr)
      case _ => None
    }
  }

  override def toString = "BX"
}

object TrueCcgRules {
  val standard: Vector[TrueCcgRule] = Vector(FA, BA, FC, BC, /*FX,*/ BX)
}
