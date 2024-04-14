package examples

object VoterService:

  def canVote(age: Int): Boolean =
    if age >= 18 then true else false

  def votingWard(voterId: String): String =
    voterId.head % 3 match
      case ward @ 0 => s"ward $ward"
      case ward @ 1 => s"ward $ward"
      case ward @ 2 => s"ward $ward"
