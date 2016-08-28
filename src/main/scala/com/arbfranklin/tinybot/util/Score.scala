/*
 * Copyright (c) 2012, Andrew Franklin
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *
 * 1. Redistributions of source code must retain the above copyright notice, this
 *    list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright notice,
 *    this list of conditions and the following disclaimer in the documentation
 *    and/or other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR
 * ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
 * (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
 * LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
 * ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

package com.arbfranklin.tinybot.util

import collection.GenIterable

/**a score for an item ranging from -1 to 1 */
case class Score(d: Double) extends Ordered[Score] {
  /** is this a positive score? */
  val isPositive = d>=0d

  val isVeto = d == Score.VetoScore

  /** reweight a value */
  def *(w: Double): Score = if (d <= -1d || d >= 1d) this else Score(w * d)

  def +(s: Score) = new Score(d + s.d)
  def -(s: Score) = new Score(d - s.d)
  def unary_- = new Score(-d)

  def min(s: Score) = if (d<s.d) this else s
  def max(s: Score) = if (d>s.d) this else s

  def compare(that: Score) = d.compareTo(that.d)
}

object Score {
  final val VetoScore = -1
  /** special scores */
  val Veto = new Score(VetoScore)
  val Abstain = new Score(0)
  val Mandate = new Score(1)

  /** helper scores */
  val High = new Score(0.8)
  val Low = new Score(0.1)

  def combine(scores: GenIterable[Score]): Score = {
    if (scores.isEmpty || scores.exists(_.d <= (-1))) return Veto
    if (scores.exists(_.d >= 1)) return Mandate

    // average the scores
    val sum = scores.foldLeft(0d) {
      (total, s) => total + s.d
    }
    Score(sum / scores.size)
  }
}
