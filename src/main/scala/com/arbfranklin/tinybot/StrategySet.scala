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

package com.arbfranklin.tinybot

import strategies.Strategy
import util._

import collection.GenIterable

class StrategySet private(val strategies: List[(Strategy,Double)]) {
  def add(tuple: (Strategy,Double)): StrategySet = new StrategySet((tuple._1, tuple._2) :: strategies)
  def add(s: Strategy): StrategySet = new StrategySet((s, 1d) :: strategies)

  /**evaluation handler */
  def eval(ctx: ReactContext): List[Action] = {
    // collate all the various actions that we could make
    val votes = collateVotes(ctx)

    // explosions are the first priority
    val explosions = votes.filterOnly[Explode]
    if (explosions.nonEmpty) {
      // find the biggest requested explosion
      val biggest = explosions.maxBy(_.action.size)
      return List(biggest.action, Status(biggest.reason))
    }

    // collate any state setting
    val states = votes.collect { case Vote(state : SetState, _, _) => state }.toList

    // now spawning, but no matter what we need a move location
    val fm = findMove(votes)
    val move = fm.action
    val reason = fm.reason

    // the move instructions
    val moveInstructions = if (move!=Move.Center) List(move, SetState("lastMove", move.toString)) else List()

    // spawn?
    val spawns = votes.filterOnly[Spawn]
    if (spawns.nonEmpty && fm.score.isPositive) {
      // find the biggest spawn
      val biggest = spawns.maxBy(_.action.energy)

      val spawn = biggest.action
      if (spawn.direction != Move.Center) {
        return List(spawn, Status(biggest.reason)) ::: moveInstructions ::: states
      } else {
        // where to spawn?
        val moves = Move.values.filter(m => m != move && ctx.view.atMove(m)==Tile.Empty)
        if (moves.nonEmpty) {
          return List(Spawn(randFrom(moves), spawn.energy), Status(biggest.reason)) ::: moveInstructions ::: states
        }
      }
      // unable to complete the spawn
    }

    // if we're here, it's just a regular move
    Status(reason) :: moveInstructions ::: states
  }

  /**collate all the potential actions */
  private def collateVotes(ctx: ReactContext): GenIterable[Vote[Action]] = {
    // TODO: apply parallel collections? Slower on my mac, but maybe not on a high-end server?
    val allowedMoves = Move.values.filter(m => ctx.view.atMove(m) != Tile.Wall).toSet

    // iterate all the move suggestions the strategies have made
    strategies.flatMap { case (strategy, weight) =>
      // obtain the candidates and re-weight them
      val votes = strategy.eval(ctx, allowedMoves)
      votes.map(v => Vote(v.action, v.score * weight, v.reason))
    }
  }

  /**find the best move in the action set */
  private def findMove(votes: GenIterable[Vote[Action]]): Vote[Move] = {
    // only interested in move actions at this point
    val votesByMove = votes.filterOnly[Move].groupBy(_.action)

    // collaborate across the votes for each move square
    val aggregatedMoveVotes = votesByMove.collect {
      case (move, moveVotes) =>
        val score = Score.combine(moveVotes.map(_.score))

        // veto is dominant for reason
        val reason = moveVotes.maxBy { v => if (v.score.isVeto) Double.PositiveInfinity else v.score.d }.reason

        // new collaborated vote
        Vote(move, score, reason)
    }

    if (aggregatedMoveVotes.isEmpty) {
      Vote(Move.Center, Score.Abstain, "*stuck*")
    } else {
      // TODO: the max score is the move, but not necessarily the reason
      aggregatedMoveVotes.maxBy(_.score)
    }
  }

  private def randFrom[T](list: List[T]): T = {
    val rand = scala.util.Random
    list(rand.nextInt(list.length))
  }
}

object StrategySet {
  def apply() = new StrategySet(List())
}
