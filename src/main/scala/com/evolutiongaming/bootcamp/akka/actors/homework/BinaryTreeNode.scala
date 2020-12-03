package com.evolutiongaming.bootcamp.akka.actors.homework

import akka.actor.{Actor, ActorRef, Props}

object BinaryTreeNode {
  private sealed trait Position

  private case object Left extends Position
  private case object Right extends Position

  def props(elem: Int, initiallyRemoved: Boolean): Props = Props(new BinaryTreeNode(elem, initiallyRemoved))
}

final class BinaryTreeNode(val elem: Int, initiallyRemoved: Boolean) extends Actor {
  import BinaryTreeNode._
  import BinaryTreeSet.Operation._
  import BinaryTreeSet.Operation
  import BinaryTreeSet.OperationReply._
  import BinaryTreeSet.OperationReply

  private var subtrees = Map[Position, ActorRef]()
  private var removed = initiallyRemoved

  override def receive: Receive = {
    case i: Insert   => doInsert(i)
    case c: Contains => doContains(c)
    case r: Remove   => doRemove(r)
    //maybe add a case for terminated child node handling
    case _ => ???
  }

  private def doInsert(m: Insert): Unit = {
    val Insert(requester, id, queryElem) = m
    if (queryElem == elem) {
      removed = false
      requester ! OperationFinished(id)
    } else {
      val position = if (queryElem > elem) Right else Left
      subtrees.get(position) match {
        case Some(subtree) => subtree ! m
        case None =>
          val newSubtree = context.watch(context.actorOf(props(queryElem, false)))
          subtrees = subtrees.updated(position, newSubtree)
          requester ! OperationFinished(id)
        // doInsert(m) this would need one more message being passed to the new child
      }
    }
  }

  private def doContains(m: Contains): Unit = {
    val Contains(requester, id, queryElem) = m
    if (queryElem == elem) {
      requester ! ContainsResult(id, if (removed) false else true)
    } else handleSubtree(queryElem, requester, m, ContainsResult(id, false))
  }

  private def doRemove(m: Remove): Unit = {
    val Remove(requester, id, queryElem) = m
    if (queryElem == elem) {
      removed = true
      requester ! OperationFinished(id)
    } else handleSubtree(queryElem, requester, m, OperationFinished(id))
  }

  private def handleSubtree(queryElem: Int, requester: ActorRef, m: Operation, reply: OperationReply) = {
    val position = if (queryElem > elem) Right else Left
    subtrees.get(position) match {
      case Some(subtree) => subtree ! m
      case None          => requester ! reply
    }
  }
}
