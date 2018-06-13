package lmc

import java.nio.file.Paths

import lmc.syntax.{Named => N, Parsed => P}
import lmc.types._
import lmc.common.{Loc, ScopeBuilder, ScopeEntry, Symbol}
import lmc.diagnostics._

import scala.collection.mutable.ListBuffer
import scala.ref.WeakReference

class Renamer(
  ctx: Context.Renamer
) {
}
