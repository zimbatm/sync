package a8.shared.jdbcf

import a8.shared.Meta.{CaseClassParm, Generator, Constructors}

/**

  WARNING THIS IS GENERATED CODE.  DO NOT EDIT.

  The only manually maintained code is the code between the //==== (normally where you add your imports)

*/

//====
import sttp.model.Uri
import a8.shared.jdbcf.DatabaseConfig
import a8.shared.jdbcf.DatabaseConfig.DatabaseId

//====


object MxDatabaseConfig {
  
  trait MxDatabaseConfig {
  
    implicit lazy val jsonCodec: a8.shared.json.JsonTypedCodec[DatabaseConfig,a8.shared.json.ast.JsObj] =
      a8.shared.json.JsonObjectCodecBuilder(generator)
        .addField(_.id)
        .addField(_.url)
        .addField(_.user)
        .addField(_.password)
        .addField(_.minIdle)
        .addField(_.maxPoolSize)
        .addField(_.autoCommit)
        .build
    
    implicit val catsEq: cats.Eq[DatabaseConfig] = cats.Eq.fromUniversalEquals
    
    lazy val generator: Generator[DatabaseConfig,parameters.type] =  {
      val constructors = Constructors[DatabaseConfig](7, unsafe.iterRawConstruct)
      Generator(constructors, parameters)
    }
    
    object parameters {
      lazy val id: CaseClassParm[DatabaseConfig,DatabaseId] = CaseClassParm[DatabaseConfig,DatabaseId]("id", _.id, (d,v) => d.copy(id = v), None, 0)
      lazy val url: CaseClassParm[DatabaseConfig,Uri] = CaseClassParm[DatabaseConfig,Uri]("url", _.url, (d,v) => d.copy(url = v), None, 1)
      lazy val user: CaseClassParm[DatabaseConfig,String] = CaseClassParm[DatabaseConfig,String]("user", _.user, (d,v) => d.copy(user = v), None, 2)
      lazy val password: CaseClassParm[DatabaseConfig,String] = CaseClassParm[DatabaseConfig,String]("password", _.password, (d,v) => d.copy(password = v), None, 3)
      lazy val minIdle: CaseClassParm[DatabaseConfig,Int] = CaseClassParm[DatabaseConfig,Int]("minIdle", _.minIdle, (d,v) => d.copy(minIdle = v), Some(()=> 1), 4)
      lazy val maxPoolSize: CaseClassParm[DatabaseConfig,Int] = CaseClassParm[DatabaseConfig,Int]("maxPoolSize", _.maxPoolSize, (d,v) => d.copy(maxPoolSize = v), Some(()=> 50), 5)
      lazy val autoCommit: CaseClassParm[DatabaseConfig,Boolean] = CaseClassParm[DatabaseConfig,Boolean]("autoCommit", _.autoCommit, (d,v) => d.copy(autoCommit = v), Some(()=> true), 6)
    }
    
    
    object unsafe {
    
      def rawConstruct(values: IndexedSeq[Any]): DatabaseConfig = {
        DatabaseConfig(
          id = values(0).asInstanceOf[DatabaseId],
          url = values(1).asInstanceOf[Uri],
          user = values(2).asInstanceOf[String],
          password = values(3).asInstanceOf[String],
          minIdle = values(4).asInstanceOf[Int],
          maxPoolSize = values(5).asInstanceOf[Int],
          autoCommit = values(6).asInstanceOf[Boolean],
        )
      }
      def iterRawConstruct(values: Iterator[Any]): DatabaseConfig = {
        val value =
          DatabaseConfig(
            id = values.next().asInstanceOf[DatabaseId],
            url = values.next().asInstanceOf[Uri],
            user = values.next().asInstanceOf[String],
            password = values.next().asInstanceOf[String],
            minIdle = values.next().asInstanceOf[Int],
            maxPoolSize = values.next().asInstanceOf[Int],
            autoCommit = values.next().asInstanceOf[Boolean],
          )
        if ( values.hasNext )
           sys.error("")
        value
      }
      def typedConstruct(id: DatabaseId, url: Uri, user: String, password: String, minIdle: Int, maxPoolSize: Int, autoCommit: Boolean): DatabaseConfig =
        DatabaseConfig(id, url, user, password, minIdle, maxPoolSize, autoCommit)
    
    }
    
    
    lazy val typeName = "DatabaseConfig"
  
  }
}
