import NinetyNineScalaProblems.WorkingWithLists._
import org.scalacheck.Gen.{listOf, posNum}
import org.scalacheck.Prop.{forAll, propBoolean}
import org.scalacheck.{Gen, Properties}

object WorkingWithListsCheck extends Properties("Lists") {

    property("startsWith") = forAll { (a: String, b: String) =>
      (a+b).startsWith(a)
    }

    property("concatenate") = forAll { (a: String, b: String) =>
      (a+b).length > a.length && (a+b).length > b.length
    }

    property("substring") = forAll { (a: String, b: String, c: String) =>
      (a+b+c).substring(a.length, a.length+b.length) == b
    }

  property("last element") = forAll { (xs: List[Int]) =>
    xs.nonEmpty ==> {
      lastele(xs) == xs.last
    }
  }

  property("penultimate element") = forAll { (xs: List[Int]) =>
    xs.length >=2 ==> {
      penultimate(xs) == xs(xs.length-2)
    }
  }

  property("penultimateBuiltin element") = forAll { (xs: List[Int]) =>
    xs.length >=2 ==> {
      penultimateBuiltin(xs) == xs(xs.length-2)
    }
  }

  property("nth element") = forAll (posNum[Int], listOf(Int)) { (k,xs) =>
    (xs.length >=1 && k <= xs.length-1)==> {
      nth(k, xs) == xs(k)
    }
  }

  property("myLength element") = forAll { xs: List[Int] =>
    myLength(xs) == xs.length
  }

  property("myLengthFunc element") = forAll { xs: List[Int] =>
    myLengthFunc(xs) == xs.length
  }

  property("removeAt") = forAll (listOf(Int), posNum[Int]) { (xs, k) =>
    (k <= xs.length - 1) ==> {
      myRemoveAt(k, xs).length == xs.length - 1
    }
  }

}
