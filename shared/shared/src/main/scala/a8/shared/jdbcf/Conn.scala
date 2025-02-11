package a8.shared.jdbcf

import java.sql.{Connection => JdbcConnection, DriverManager => JdbcDriverManager, PreparedStatement => JdbcPreparedStatement, SQLException => JdbcSQLException, Statement => JStatement}
import a8.shared.SharedImports._
import a8.shared.app.{LoggerF, LoggingF}
import a8.shared.jdbcf.Conn.ConnInternal
import a8.shared.jdbcf.Conn.impl.withSqlCtx0
import a8.shared.jdbcf.JdbcMetadata.{JdbcColumn, JdbcTable, ResolvedJdbcTable}
import a8.shared.jdbcf.PostgresDialect.self
import a8.shared.jdbcf.SqlString.ResolvedSql
import a8.shared.jdbcf.mapper.KeyedMapper.UpsertResult
import a8.shared.jdbcf.mapper.{KeyedMapper, Mapper}
import cats.effect.kernel.Resource.ExitCase
import sttp.model.Uri
import wvlet.log.LazyLogger
import a8.shared.jdbcf.UnsafeResultSetOps._

object Conn extends LazyLogger {

  implicit def implicitLogger = logger

  def apply[F[_] : Conn] = implicitly[Conn[F]]

  object impl {

    def withSqlCtx[F[_] : Async, A](sql: ResolvedSql)(fn: =>A): F[A] =
      Async[F].blocking(withSqlCtx0(sql)(fn))

    def withSqlCtx0[A](sql: ResolvedSql)(fn: =>A): A =
      try {
        fn
      } catch {
        case e: java.sql.SQLException =>
          throw new JdbcSQLException(s"error running -- ${sql.value} -- ${e.getMessage}", e.getSQLState, e.getErrorCode, e)
      }

    def makeResource[F[_] : Async](connFn: =>java.sql.Connection): Resource[F,Conn[F]] = {
      val jdbcMetadata = JdbcMetadata[F]
      Managed.resource(connFn)
        .map(jdbcConn => toConn[F](jdbcConn, jdbcMetadata))
    }

  }

  def fromNewConnection[F[_] : Async](
    url: Uri,
    user: String,
    password: String
  ): Resource[F, Conn[F]] = {
    impl.makeResource(JdbcDriverManager.getConnection(url.toString, user, password))
  }

  def toConn[F[_] : Async](jdbcConn: JdbcConnection, jdbcMetadata: JdbcMetadata[F])(implicit monadCancel: MonadCancel[F, _]): Conn[F] = {

    def makeResourceF[A : Managed](thunk: =>A) = Managed.resource[F,A](thunk)

    ConnInternalImpl(jdbcMetadata, jdbcConn)

  }

  trait ConnInternal[F[_]] extends Conn[F] {
    val jdbcMetadata: JdbcMetadata[F]
    def resolve(sql: SqlString): ResolvedSql
    def withInternalConn[A](fn: JdbcConnection=>A): F[A]
    def statement: fs2.Stream[F, JStatement]
    def prepare(sql: SqlString): fs2.Stream[F, JdbcPreparedStatement]

    override def resolveTableName(tableLocator: TableLocator, useCache: Boolean): F[ResolvedTableName] =
      jdbcMetadata.resolveTableName(tableLocator, this, useCache)

    def tables: F[Iterable[JdbcTable]] =
      jdbcMetadata.tables(this)

    def tableMetadata(tableLocator: TableLocator, useCache: Boolean): F[ResolvedJdbcTable] =
      jdbcMetadata.tableMetadata(tableLocator, this, useCache)

  }

}

trait Conn[F[_]] {

  val jdbcUrl: Uri

  implicit lazy val dialect: Dialect = Dialect(jdbcUrl)

  def resolveTableName(tableLocator: TableLocator, useCache: Boolean = true): F[ResolvedTableName]
  def tables: F[Iterable[JdbcTable]]
  def tableMetadata(tableLocator: TableLocator, useCache: Boolean = true): F[ResolvedJdbcTable]
  def query[A : RowReader](sql: SqlString): Query[F,A]
  def streamingQuery[A : RowReader](sql: SqlString): StreamingQuery[F,A]
  def update(updateQuery: SqlString): F[Int]
  def batcher[A : RowWriter](sql: SqlString): Batcher[F,A]
  def isAutoCommit: F[Boolean]


  def insertRow[A : Mapper](row: A): F[Unit]
  def upsertRow[A, B](row: A)(implicit keyedMapper: KeyedMapper[A,B]): F[UpsertResult]
  def updateRow[A, B](row: A)(implicit keyedMapper: KeyedMapper[A,B]): F[Unit]
  def deleteRow[A, B](row: A)(implicit keyedMapper: KeyedMapper[A,B]): F[Unit]

  def selectRows[A : Mapper](whereClause: SqlString): F[Iterable[A]]
  def streamingSelectRows[A : Mapper](whereClause: SqlString): fs2.Stream[F,A]

  def fetchRow[A, B](key: B)(implicit keyedMapper: KeyedMapper[A,B]): F[A]
  def fetchRowOpt[A, B](key: B)(implicit keyedMapper: KeyedMapper[A,B]): F[Option[A]]

  def asInternal: ConnInternal[F]

  def commit: F[Unit]
  def rollback: F[Unit]

}
