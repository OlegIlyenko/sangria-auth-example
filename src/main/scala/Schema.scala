import Data._
import sangria.schema._

object Schema {
  val UserType = ObjectType("User", fields[SecureContext, User](
    Field("userName", StringType, resolve = _.value.userName),
    Field("permissions", OptionType(ListType(StringType)),
      resolve = ctx => ctx.ctx.authorised("VIEW_PERMISSIONS") { _ =>
        ctx.value.permissions
      })
  ))

  val QueryType = ObjectType("Query", fields[SecureContext, Unit](
    Field("me", OptionType(UserType), resolve = ctx => ctx.ctx.authorised()(user => user)),
    Field("colors", OptionType(ListType(StringType)),
      resolve = ctx => ctx.ctx.authorised("VIEW_COLORS") { _ =>
        ctx.ctx.colorRepo.colors
      })
  ))

  val UserNameArg = Argument("userName", StringType)
  val PasswordArg = Argument("password", StringType)
  val ColorArg = Argument("color", StringType)

  val MutationType = ObjectType("Mutation", fields[SecureContext, Unit](
    Field("login", OptionType(StringType),
      arguments = UserNameArg :: PasswordArg :: Nil,
      resolve = ctx => UpdateCtx(ctx.ctx.login(ctx.arg(UserNameArg), ctx.arg(PasswordArg))) { token =>
        ctx.ctx.copy(token = Some(token))
      }),
    Field("addColor", OptionType(ListType(StringType)),
      arguments = ColorArg :: Nil,
      resolve = ctx => ctx.ctx.authorised("EDIT_COLORS") { _ =>
        ctx.ctx.colorRepo.addColor(ctx.arg(ColorArg))
        ctx.ctx.colorRepo.colors
      })
  ))

  def schema = sangria.schema.Schema(QueryType, Some(MutationType))
}
