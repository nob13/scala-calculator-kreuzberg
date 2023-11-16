import kreuzberg.*
import kreuzberg.scalatags.*
import kreuzberg.scalatags.all.*
import kreuzberg.engine.naive.Binder
import kreuzberg.Subscribeable

case class ButtonUi(buttonText: Subscribeable[String], clazz: String = "")
    extends SimpleComponentBase:

  def assemble(using c: SimpleContext): Html =
    button(
      Some(clazz).filter(_.nonEmpty).map { cls := _ },
      buttonText.subscribe(),
    )

  val onClick = jsEvent("click")

case class DisplayUi(model: Subscribeable[String])
    extends SimpleComponentBase:

  def assemble(using c: SimpleContext): Html =
    val value = subscribe(model)
    div(cls := "display", value)

object Main2 extends SimpleComponentBase:

  val calculator = Model.create(Calculator())
  val displayUi = DisplayUi(calculator.map(_.showDisplay()))

  def assemble(using c: SimpleContext): Html =
    def makeBtn(
        label: Subscribeable[String],
        clazz: String,
        op: Calculator => Calculator,
    ): ButtonUi =
      val button = ButtonUi(label, clazz)
      add(
        button.onClick.changeModelDirect(calculator)(op),
      )
      button

    def makeDigitBtn(
        digit: Char,
    ): ButtonUi =
      makeBtn(digit.toString(), "", _.enterDigit(digit))

    div(
      cls := "base",
      displayUi,
      div(
        cls := "buttons",
        makeBtn("+", "operator", _.enterOperator(Operator.Plus)),
        makeBtn("-", "operator", _.enterOperator(Operator.Minus)),
        makeBtn("×", "operator", _.enterOperator(Operator.Multiply)),
        makeBtn("÷", "operator", _.enterOperator(Operator.Divide)),
        makeBtn("=", "equals", _.enterEquals()),
        makeDigitBtn('7'),
        makeDigitBtn('8'),
        makeDigitBtn('9'),
        makeDigitBtn('4'),
        makeDigitBtn('5'),
        makeDigitBtn('6'),
        makeDigitBtn('1'),
        makeDigitBtn('2'),
        makeDigitBtn('3'),
        makeDigitBtn('0'),
        makeBtn(".", "", _.enterDecimal()),
        makeBtn(calculator.map(_.showClear()), "", _.clear()),
      ),
    )

object App2 extends App:
  given repo: ServiceRepository = ServiceRepository.extensible
  Binder.runOnLoaded(
    Main2,
    "app",
  )
