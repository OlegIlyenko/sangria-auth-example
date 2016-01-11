import Data._
import sangria.execution.{MiddlewareBeforeField, MiddlewareQueryContext, Middleware, FieldTag}
import sangria.schema._

object Schema {
  val UserNameArg = Argument("userName", StringType)
  val PasswordArg = Argument("password", StringType)
  val ColorArg = Argument("color", StringType)

  object resolveBased {
    val UserType = ObjectType("User", fields[SecureContext, User](
      Field("userName", StringType, resolve = _.value.userName),
      Field("permissions", OptionType(ListType(StringType)),
        resolve = ctx ⇒ ctx.ctx.authorised("VIEW_PERMISSIONS") { _ ⇒
          ctx.value.permissions
        })
    ))

    val QueryType = ObjectType("Query", fields[SecureContext, Unit](
      Field("me", OptionType(UserType), resolve = ctx ⇒ ctx.ctx.authorised()(user ⇒ user)),
      Field("colors", OptionType(ListType(StringType)),
        resolve = ctx ⇒ ctx.ctx.authorised("VIEW_COLORS") { _ ⇒
          ctx.ctx.colorRepo.colors
        })
    ))

    val MutationType = ObjectType("Mutation", fields[SecureContext, Unit](
      Field("login", OptionType(StringType),
        arguments = UserNameArg :: PasswordArg :: Nil,
        resolve = ctx ⇒ UpdateCtx(ctx.ctx.login(ctx.arg(UserNameArg), ctx.arg(PasswordArg))) { token ⇒
          ctx.ctx.copy(token = Some(token))
        }),
      Field("addColor", OptionType(ListType(StringType)),
        arguments = ColorArg :: Nil,
        resolve = ctx ⇒ ctx.ctx.authorised("EDIT_COLORS") { _ ⇒
          ctx.ctx.colorRepo.addColor(ctx.arg(ColorArg))
          ctx.ctx.colorRepo.colors
        })
    ))

    def schema = sangria.schema.Schema(QueryType, Some(MutationType))
  }

  object middlewareBased {
    case object Authorised extends FieldTag
    case class Permission(name: String) extends FieldTag

    object SecurityEnforcer extends Middleware[SecureContext] with MiddlewareBeforeField[SecureContext] {
      type QueryVal = Unit
      type FieldVal = Unit

      def beforeQuery(context: MiddlewareQueryContext[SecureContext, _, _]) = ()
      def afterQuery(queryVal: QueryVal, context: MiddlewareQueryContext[SecureContext, _, _]) = ()

      def beforeField(queryVal: QueryVal, mctx: MiddlewareQueryContext[SecureContext, _, _], ctx: Context[SecureContext, _]) = {
        val permissions = ctx.field.tags.collect {case Permission(p) ⇒ p}
        val requireAuth = ctx.field.tags contains Authorised
        val securityCtx = ctx.ctx

        if (requireAuth)
          securityCtx.user

        if (permissions.nonEmpty)
          securityCtx.ensurePermissions(permissions)

        continue
      }
    }

    val UserType = ObjectType("User", fields[SecureContext, User](
      Field("userName", StringType, resolve = _.value.userName),
      Field("permissions", OptionType(ListType(StringType)),
        tags = Permission("VIEW_PERMISSIONS") :: Nil,
        resolve = _.value.permissions)
    ))

    val QueryType = ObjectType("Query", fields[SecureContext, Unit](
      Field("me", OptionType(UserType), tags = Authorised :: Nil,resolve = _.ctx.user),
      Field("colors", OptionType(ListType(StringType)),
        tags = Permission("VIEW_COLORS") :: Nil, resolve = _.ctx.colorRepo.colors)
    ))

    val MutationType = ObjectType("Mutation", fields[SecureContext, Unit](
      Field("login", OptionType(StringType),
        arguments = UserNameArg :: PasswordArg :: Nil,
        resolve = ctx ⇒ UpdateCtx(ctx.ctx.login(ctx.arg(UserNameArg), ctx.arg(PasswordArg))) { token ⇒
          ctx.ctx.copy(token = Some(token))
        }),
      Field("addColor", OptionType(ListType(StringType)),
        arguments = ColorArg :: Nil,
        tags = Permission("EDIT_COLORS") :: Nil,
        resolve = ctx ⇒ {
          ctx.ctx.colorRepo.addColor(ctx.arg(ColorArg))
          ctx.ctx.colorRepo.colors
        })
    ))

    def schema = sangria.schema.Schema(QueryType, Some(MutationType))
  }

}
