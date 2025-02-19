package a8.shared.jdbcf

import java.sql.{Connection => JdbcConnection, DriverManager => JdbcDriverManager, PreparedStatement => JdbcPreparedStatement, SQLException => JdbcSQLException, Statement => JStatement}

import a8.shared.app.LoggingF
import a8.shared.jdbcf.Conn.impl.withSqlCtx0
import a8.shared.jdbcf.JdbcMetadata.JdbcTable
import a8.shared.jdbcf.SqlString.ResolvedSql
import a8.shared.jdbcf.mapper.KeyedMapper.UpsertResult
import a8.shared.jdbcf.mapper.{KeyedMapper, Mapper}
import sttp.model.Uri
import a8.shared.SharedImports._
import a8.shared.jdbcf.Conn.ConnInternal

case class ConnInternalImpl[F[_] : Async](
  jdbcMetadata: JdbcMetadata[F],
  jdbcConn: java.sql.Connection,
)
  extends LoggingF[F]
  with ConnInternal[F]
{

  val F = Async[F]

  override def asInternal: ConnInternal[F] = this

  lazy val jdbcUrl = Uri.unsafeParse(jdbcConn.getMetaData.getURL)

  override def resolve(sql: SqlString): ResolvedSql =
    SqlString.unsafe.resolve(sql)

  override def tables: F[Iterable[JdbcTable]] = {
    F.blocking {
      resultSetToVector(
        jdbcConn
          .getMetaData
          .getTables(null, null, null, null)
      )
        .map(JdbcTable.apply)
    }
  }

  def managedStream[A : Managed](thunk: =>A): fs2.Stream[F,A] =
    Managed.stream[F, A](thunk)

  override def streamingQuery[A : RowReader](sql: SqlString): StreamingQuery[F,A] =
    StreamingQuery.create[F,A](this, sql)

  override def query[A: RowReader](sqlStr: SqlString): Query[F, A] =
    Query.create[F,A](this, sqlStr)

  override def update(updateQuery: SqlString): F[Int] = {
    prepare(updateQuery)
      .evalMap { ps =>
        F.blocking {
          logger.debug(s"executing update - ${updateQuery}")
          val i = ps.executeUpdate()
          i
        }
      }
      .compile
      .lastOrError
  }

  override def statement: fs2.Stream[F, JStatement] =
    managedStream(jdbcConn.createStatement())

  override def prepare(sql: SqlString): fs2.Stream[F, JdbcPreparedStatement] = {
    val resolvedSql = resolve(sql)
    managedStream(withSqlCtx0[JdbcPreparedStatement](resolvedSql)(jdbcConn.prepareStatement(resolvedSql.value)))
  }

  override def batcher[A : RowWriter](sql: SqlString): Batcher[F,A] =
    Batcher.create[F,A](this, sql)

  override def withInternalConn[A](fn: JdbcConnection => A): F[A] = {
    F.blocking(fn(jdbcConn))
  }

  override def isAutoCommit: F[Boolean] =
    F.delay(jdbcConn.getAutoCommit)

  def runSingleRowUpdate(sql: SqlString): F[Unit] = {
    update(sql)
      .flatMap {
        case 1 =>
          F.unit
        case i =>
          F.raiseError[Unit](new RuntimeException(s"ran update and expected 1 row to be affected and ${i} rows were affected -- ${sql}"))
      }
  }

  override def insertRow[A: Mapper](row: A): F[Unit] =
    runSingleRowUpdate(implicitly[Mapper[A]].insertSql(row))

  override def upsertRow[A, B](row: A)(implicit keyedMapper: KeyedMapper[A, B]): F[UpsertResult] = {
    val mapper = implicitly[KeyedMapper[A,B]]
    fetchRowOpt(mapper.key(row))
      .flatMap {
        case Some(v) =>
          updateRow(row)
            .as(UpsertResult.Update)
        case None =>
          insertRow(row)
            .as(UpsertResult.Insert)
      }
  }

  override def updateRow[A, B](row: A)(implicit keyedMapper: KeyedMapper[A, B]): F[Unit] =
    runSingleRowUpdate(implicitly[KeyedMapper[A,B]].updateSql(row))

  override def deleteRow[A, B](row: A)(implicit keyedMapper: KeyedMapper[A, B]): F[Unit] =
    runSingleRowUpdate(implicitly[KeyedMapper[A,B]].deleteSql(row))

  override def selectRows[A: Mapper](whereClause: SqlString): F[Iterable[A]] =
    query[A](implicitly[Mapper[A]].selectSql(whereClause))
      .select

  override def streamingSelectRows[A: Mapper](whereClause: SqlString): fs2.Stream[F, A] =
    streamingQuery[A](implicitly[Mapper[A]].selectSql(whereClause))
      .run

  override def fetchRow[A, B](key: B)(implicit keyedMapper: KeyedMapper[A, B]): F[A] =
    fetchRowOpt[A,B](key)
      .flatMap {
        case Some(row) =>
          F.pure(row)
        case None =>
          F.raiseError[A](new RuntimeException(s"expected a record with key ${key} in ${implicitly[Mapper[A]].tableName}"))
      }


  override def fetchRowOpt[A, B](key: B)(implicit keyedMapper: KeyedMapper[A, B]): F[Option[A]] =
    selectRows(implicitly[KeyedMapper[A,B]].fetchWhere(key))
      .map(_.headOption)

  override def commit: F[Unit] =
    F.blocking(jdbcConn.commit())

  override def rollback: F[Unit] =
    F.blocking(jdbcConn.rollback())

}
